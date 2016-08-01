# intro6.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 07/05/2016
# 
# Purpose: illustrates how to use a "path of steepest ascent" approach
# to optimize an objective function for calibrating an HSPF model. This
# example illustrates how such an approach can be developed but modifies
# only two calibration parameters (LZSN and INFILT).
#
# Python built-in imports

import os, pickle, datetime

# the file path to the uncalibrated model

model = 'example03'

# make sure the baseline model exists

if not os.path.isfile(model):
    print('the hspfmodel file does not exist, run example03.py')
    raise

# other info about the simulation

start = datetime.datetime(1988, 10, 1)
end   = datetime.datetime(1990, 10, 1)

# identifier for the calibration location

gagecomid = '30'          

# For the purposes of illustration, we will look at the Nash-Sutcliffe
# Efficiency (NSE) for a variety of values of only two of the most sensitive
# parameters, the lower-zone storage parameter (LZSN) and the infiltration
# rate (INFILT). A  model can have different values of LZSN and INFILT
# for different land segments, but in this case the optimum is explored
# HSPF technically allows INFILT between 0.0001 and 100 in/hr and LZSN
# between 0.01 and 100 in. But for practical purposes we will constrain
# the search space to:
#
# 8 < LZSN < 22
#
# and
#
# 0.006 < INFILT < 0.030
#
# The script illustrates how to explore this space for the optimum NSE.
# 

LZSNmin = 8
LZSNmax = 22
LZSNint = 1

INFILTmin = 0.006
INFILTmax = 0.030
INFILTint = 0.002

# import the Postprocessor to get the observed flows

from pyhspf import Postprocessor

# the simulations can be run in parallel using multiprocessing

from multiprocessing import Pool, cpu_count

# use matplotlib to make a graph

from matplotlib import pyplot, patches

# use numpy to make an array

import numpy

# create a working subdirectory to use for the 100 simulations

working = 'simulations'

# the calibration procedure explores for a maximum value starting at a point
# (set of parameter values), then runs a series of simulations with
# slightly smaller and larger values of each parameter and then moves
# all the variables simultaneously to the next point repeatedly until
# all movements decrease the value of the objective function. The maximum
# value is around INFILT = 0.018 and LZSN = 15.

starting_points = [{'INFILT': 0.008, 'LZSN': 9},
                   {'INFILT': 0.028, 'LZSN': 9},
                   {'INFILT': 0.008, 'LZSN': 20},
                   {'INFILT': 0.028, 'LZSN': 18},
                   ]

# in this case, the entire space is computed and then each of the paths
# are followed to the maximum. but in the case of a full calibration with
# many parameters, the path can be traced without exploring every set of
# parameter values (this speeds the process tremendously).

# create a function that takes the values of LZSN and INFILT as a dictionary
# of inputs that then computes and returns the NSE (this is necessary so
# that simulations can be run in parallel in Python)

def get_NSE(values):

    its = values['LZSN'], values['INFILT']
    print('running simulation LZSN: {:.1f}, INFILT: {:.4f}\n'.format(*its))
    
    # open the baseline HSPF model

    with open(model, 'rb') as f: hspfmodel = pickle.load(f)

    # change the filename to prevent two simultaneous runs with the same name

    its = working, values['LZSN'], values['INFILT']
    hspfmodel.filename = '{}/LZSN{:.1f}INFILT{:.4f}'.format(*its)

    # change the pan coefficient

    hspfmodel.evap_multiplier = 0.76
    
    # change the parameter values in all the land segments

    for p in hspfmodel.perlnds:
        p.LZSN   = values['LZSN']
        p.INFILT = values['INFILT']

    # build the input WDM file
        
    hspfmodel.build_wdminfile()

    # build the UCI file (only need to save the reach_outvolume, ROVOL)
        
    hspfmodel.build_uci(['reach_outvolume'], start, end, hydrology = True)

    # run the simulation

    hspfmodel.run(verbose = True)

    # make an instance of the postprocessor to get the efficiency
        
    p = Postprocessor(hspfmodel, (start, end), comid = gagecomid)

    # the regressions between the simulated and observed flows can be
    # computed using the "regression" function that returns:
    #
    # daily r2, log daily r2, daily NSE, log daily NSE
    # montly r2, log monthly r2, monthly NSE, log monthly NSE
        
    dr2, dlr2, dNS, dlNS, mr2, mlr2, mNS, mlNS = p.get_regression(gagecomid)

    # close the Postprocessor

    p.close()

    return dNS

# iterate through each pair of LZSN and INFILT values, open the baseline
# model, change the values of LZSN and INFILT in each land segment, run
# the simulation, get the simulated and observed flows, calculate and save
# the NSE from each run

if __name__ == '__main__':

    # create a working directory for the (many) files used in these simulations

    if not os.path.isdir(working): os.mkdir(working)
    
    # number of processors to use

    nprocessors = max(1, cpu_count() - 1)

    # make a list of the LZSN values for the search space

    n = round((LZSNmax - LZSNmin) / LZSNint)
    
    LZSNs = [round(LZSNmin + LZSNint * i, 1) for i in range(n + 1)]

    # make a list of the INFILT values for the search space
    
    n = round((INFILTmax - INFILTmin) / INFILTint)

    INFILTs = [round(INFILTmin + INFILTint * i, 4) for i in range(n + 1)]

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

    # now make a plot in matplotlib to illustrate the calibration process

    fig = pyplot.figure(figsize = (12,6))
    sub = fig.add_subplot(111)

    sub.set_title('Paths to Determine INFILT and LZSN that Maximize ' +
                  'Nash-Sutcliffe Efficiency')
    
    for INFILT in INFILTs:

        for LZSN in LZSNs:

            NSE = results[LZSN][INFILT]

            sub.text(INFILT, LZSN, '{:.4f}'.format(NSE), fontsize = 9,
                     va = 'center', ha = 'center', weight = 0.1)

    # reorganize into rows in a grid
    
    rows = [[''] + [INFILT for INFILT in INFILTs]]

    for LZSN in LZSNs:
        rows.append([LZSN] + [results[LZSN][INFILT] for INFILT in INFILTs])

    # store the results in an array in numpy to navigate
    
    grid = numpy.array([[float(v) for v in row[1:]]
                        for row in rows[1:]])

    # plot parameters
    
    xstretch = 1.4
    ystretch = 1.4
    lw = 0.8

    # trace each path
    
    for values in starting_points:

        i = LZSNs.index(values['LZSN'])
        j = INFILTs.index(values['INFILT'])

        # keep track of the last location to use to define the end point
        # (start with the minimum value in the array)

        x,y = numpy.unravel_index(grid.argmin(), grid.shape)

        # draw the path for the optimization search

        vertices = [(INFILTs[j], LZSNs[i])]

        # move to the next (higher) point until reaching the top

        while (x,y) != (i,j):

            # move the last point up
            
            x = i
            y = j

            if   grid[x+1,y] > grid[x,y]: i+=1
            elif grid[x-1,y] > grid[x,y]: i-=1

            if   grid[x,y+1] > grid[x,y]: j+=1
            elif grid[x,y-1] > grid[x,y]: j-=1

            vertices.append((INFILTs[j], LZSNs[i]))

        # draw a box around the max

        x = INFILTs[j] - INFILTint / 2
        y = LZSNs[i] - LZSNint / 2
        sub.add_patch(patches.Rectangle((x,y), INFILTint, LZSNint,
                                        fill = False))

        # make a simple plot of the path

        xs, ys = zip(*vertices)
        sub.plot(xs,ys, color = 'black', lw = lw)

        # draw the path using arrows to show the direction

        for p1, p2 in zip(vertices[:-1], vertices[1:]):

            dx = (p2[0] - p1[0]) * xstretch
            dy = (p2[1] - p1[1]) * ystretch
            sub.annotate('', xy = (p1[0] + dx / 2, p1[1] + dy / 2),
                         xytext = p1,
                         arrowprops = dict(arrowstyle = '->',
                                           lw = lw,
                                           connectionstyle = 'arc3'))

    # add contour lines

    levels = [0.25, 0.40, 0.48, 0.53, 0.56, 0.571] 
    sub.contour(INFILTs, LZSNs, grid, levels,
                linewidths = 0.4, colors = 'k')
        
    # to illustrate how the calibration procedure works, here are a

    sub.set_xticks(INFILTs)
    sub.set_yticks(LZSNs)

    for t in (sub.xaxis.get_ticklines() + sub.yaxis.get_ticklines()):
        t.set_visible(False)
        sub.set_xlabel('INFILT (in/hr)')
    sub.set_ylabel('LZSN (in)')
    sub.set_xlim((INFILTmin - INFILTint, INFILTmax + INFILTint))
    sub.set_ylim((LZSNmin - LZSNint, LZSNmax + LZSNint))

    pyplot.savefig('example06')
    #pyplot.show()
