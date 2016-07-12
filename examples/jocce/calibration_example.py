# calibrate_example.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 06/29/2015
# 
# Purpose: illustrates the "path of steepest ascent" approach used by the
# AutoCalibrator class to optimize an objective function for an HSPF model.
#
# Python built-in imports

import os, pickle, datetime, csv

# path to the baseline (uncalibrated) HSPF model (the file should contain
# a pickled instance of the HSPFModel class)

filename = 'C:/HSPF_new/07080106/hspf/2001baseline'

# NWIS gage ID for the calibration

gageid = '05472500'

# start and end dates for the calibration period (1981 to 2001)

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# model warmup time

warmup = 366

# HSPF modules present (in this case air temperature, snow, and hydrology)

atemp     = True
snow      = True
hydrology = True

# For the purposes of illustration, we will look at the Nash-Sutcliffe
# Efficiency (NSE) for a variety of values of only two of the most sensitive
# parameters, the lower-zone storage parameter (LZSN) and the infiltration
# rate (INFILT). The calibrated model has different values of LZSN and INFILT
# for different land segments, but the optimum is around 120 mm for LZSN
# and 0.76 for INFILT. HSPF technically allows INFILT between 0.0025 and
# 2500 mm/hr and LZSN between 0.25 and 2500 mm. But for practical purposes
# we will constrain the search space to:
#
# 50 < LZSN < 450 and
# 0.10 < INFILT < 2.10
#
# So the script will illustrate how to explore this space for the optimum NSE.
# The script is designed such that it will save the results to a CSV file
# so that the whole set of simulation doesn't have to be repeated for small
# modifications.

LZSNmin = 50
LZSNmax = 450
LZSNint = 20

INFILTmin = 0.10
INFILTmax = 2.10
INFILTint = 0.1

# import the Postprocessor to get the observed flows

from pyhspf import Postprocessor

# run in parallel using multiprocessing

from multiprocessing import Pool

# use matplotlib to make a graph

from matplotlib import pyplot, patches

# number of processors to use

nprocessors = 5

# save the results to this CSV file

output = 'calibration_procedure.csv'

# create a function that takes the values of LZSN and INFILT as a dictionary
# as inputs that then computes and returns the NSE (this enables the
# the simulations to be run in parallel in Python)

def get_NSE(values):

    its = values['LZSN'], values['INFILT']
    print('running simulation LZSN: {:.0f}, INFILT: {:.2f}\n'.format(*its))
    
    # open the baseline HSPF model

    with open(filename, 'rb') as f: hspfmodel = pickle.load(f)

    # change the filename to prevent two simultaneous runs with the same name

    i = len(hspfmodel.filename) - 1
    while hspfmodel.filename[i] != '/': i -= 1

    path = hspfmodel.filename[:i]
    
    its = path, values['LZSN'], values['INFILT']
    hspfmodel.filename = '{}/LZSN{:.0f}INFILT{:.2f}'.format(*its)
                              
    # change the parameter values in all the land segments

    for p in hspfmodel.perlnds:

        p.LZSN   = values['LZSN']
        p.INFILT = values['INFILT']

    # build the input WDM file
        
    hspfmodel.build_wdminfile()

    # build the UCI file (only need to save the reach_outvolume, ROVOL)
        
    hspfmodel.build_uci(['reach_outvolume'], start, end, atemp = atemp,
                        snow = snow, hydrology = hydrology)

    # run the simulation

    hspfmodel.run(verbose = True)

    # calibration period

    dates = start + datetime.timedelta(days = warmup), end

    # find the common identifier for the NWIS gage

    d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
    comid = d[gageid]

    # make an instance of the postprocessor to get the efficiency
        
    p = Postprocessor(hspfmodel, dates, comid = comid)

    # the regressions between the simulated and observed flows can be
    # computed using the "regression" function that returns:
    #
    # daily r2, log daily r2, daily NSE, log daily NSE
    # montly r2, log monthly r2, monthly NSE, log monthly NSE
        
    dr2, dlr2, dNS, dlNS, mr2, mlr2, mNS, mlNS = p.get_regression(comid)

    # close the Postprocessor

    p.close()

    return dNS

# create a function to draw lines tracing uphill paths

def uphill(sub, points, xstretch = 1.4, ystretch = 1.4, lw = 0.8):
    """
    Draws arrows tracing the path laid out by the points onto a subplot.
    """

    xs, ys = zip(*points)
    sub.plot(xs,ys, color = 'black', lw = lw)

    for p1, p2 in zip(points[:-1], points[1:]):

        dx = (p2[0] - p1[0]) * xstretch
        dy = (p2[1] - p1[1]) * ystretch
        sub.annotate('', xy = (p1[0] + dx / 2, p1[1] + dy / 2),
                     xytext = p1,
                     arrowprops = dict(arrowstyle = '->',
                                       lw = lw,
                                       connectionstyle = 'arc3'))
    
# iterate through each pair of LZSN and INFILT values, open the baseline
# model, change the values of LZSN and INFILT in each land segment, run
# the simulation, get the simulated and observed flows, calculate and save
# the NSE from each run

if __name__ == '__main__':

    # make a list of the LZSN values for the search space

    n = round((LZSNmax - LZSNmin) / LZSNint)
    
    LZSNs = [round(LZSNmin + LZSNint * i) for i in range(n + 1)]

    # make a list of the INFILT values for the search space
    
    n = round((INFILTmax - INFILTmin) / INFILTint)

    INFILTs = [round(INFILTmin + INFILTint * i, 1) for i in range(n + 1)]

    # don't re-run the simulations if they have already been completed
    
    if not os.path.isfile(output):
        
        # organize simulations as one input and one output
    
        simulations = [{'LZSN': LZSN, 'INFILT': INFILT}
                       for LZSN in LZSNs
                       for INFILT in INFILTs]

        # run the simulations in parallel

        with Pool(nprocessors) as p:
            results = p.map_async(get_NSE, simulations)
            NSEs = results.get()

        # reorganize the results into a dictionary of dictionaries
    
        results = {}
        for s, NSE in zip(simulations, NSEs):
            LZSN = s['LZSN']
            INFILT = s['INFILT']
            if LZSN in results: results[LZSN][INFILT] = NSE
            else:               results[LZSN] = {INFILT:NSE}

        # reorganize into rows in a csv file
    
        rows = [[''] + [INFILT for INFILT in INFILTs]]

        for LZSN in LZSNs:
            rows.append([LZSN] + [results[LZSN][INFILT] for INFILT in INFILTs])

        # create the file
        
        with open(output, 'w', newline = '') as csvfile:
            writer = csv.writer(csvfile)
            for row in rows: writer.writerow(row)

    else:

        with open(output, 'r') as csvfile:
            reader = csv.reader(csvfile)
            rows = [r for r in reader]

        INFILTs = [float(r)      for r in rows[0][1:]]
        LZSNs   = [float(row[0]) for row in rows[1:]]

        results = {}
        for LZSN, row in zip(LZSNs, rows[1:]):
            results[LZSN] = {}
            for INFILT, value in zip(INFILTs, row[1:]):
                results[LZSN][INFILT] = float(value)

    # now make a plot in matplotlib to illustrate the calibration process

    fig = pyplot.figure(figsize = (12,6))
    sub = fig.add_subplot(111)
    
    for INFILT in INFILTs:

        for LZSN in LZSNs:

            NSE = results[LZSN][INFILT]

            sub.text(INFILT, LZSN, '{:.0f}'.format(NSE * 10000), fontsize = 9,
                     va = 'center', ha = 'center', weight = 0.1)

    # the calibration procedure used by the AutoCalibrator begins at a point
    # (set of parameter values), then runs a series of simulations with
    # slightly smaller and larger values of each parameter and then moves
    # all the variables simultaneously to the next point repeatedly until
    # all movements decrease the value of the objective function.

    starting_points = [{'INFILT': 0.2, 'LZSN':  70},
                       {'INFILT': 2.0, 'LZSN': 430},
                       {'INFILT': 0.4, 'LZSN': 410},
                       {'INFILT': 2.0, 'LZSN': 210},
                       {'INFILT': 1.0, 'LZSN':  70},
                       ]

    # store the results in an array in numpy to navigate

    import numpy as np
    
    grid = np.array([[float(v) for v in row[1:]]
                     for row in rows[1:]])

    labels = []
    for values in starting_points:

        i = LZSNs.index(values['LZSN'])
        j = INFILTs.index(values['INFILT'])
        
        its = i, j, 10000 * float(grid[i,j])

        # keep track of the last location to use to define the end point
        # (start with the minimum value in the array)

        x,y = np.unravel_index(grid.argmin(), grid.shape)

        # draw the path for the optimization search

        vertices = [(INFILTs[j], LZSNs[i])]

        # move to the next (higher) point until reaching the top

        while grid[x,y] < grid[i,j]:

            # move the last point up
            
            x = i
            y = j

            if   grid[x+1,y] > grid[x,y]: i+=1
            elif grid[x-1,y] > grid[x,y]: i-=1

            if   grid[x,y+1] > grid[x,y]: j+=1
            elif grid[x,y-1] > grid[x,y]: j-=1

            vertices.append((INFILTs[j], LZSNs[i]))

        # draw the path

        uphill(sub, vertices, lw = 0.9)

        # draw a box around the max

        x = INFILTs[j] - INFILTint / 2
        y = LZSNs[i] - LZSNint / 2
        sub.add_patch(patches.Rectangle((x,y), INFILTint, LZSNint,
                                        fill = False))

    # add a contour plot

    levels = [0.60, 0.67, 0.688, 0.703, 0.71, 0.713]
    sub.contour(INFILTs, LZSNs, grid, levels, linewidths = 0.4, colors = 'k')
        
    # to illustrate how the calibration procedure works, here are a

    sub.set_xticks(INFILTs)
    sub.set_yticks(LZSNs)

    for t in (sub.xaxis.get_ticklines() + sub.yaxis.get_ticklines()):
        t.set_visible(False)
        sub.set_xlabel('INFILT (mm/hr)')
    sub.set_ylabel('LZSN (mm)')
    sub.set_xlim((INFILTmin - INFILTint, INFILTmax + INFILTint))
    sub.set_ylim((LZSNmin - LZSNint, LZSNmax + LZSNint))

    pyplot.savefig('example')
    pyplot.show()
