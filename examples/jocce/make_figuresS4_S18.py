import os, pickle, datetime, csv

from pyhspf     import Postprocessor
from matplotlib import pyplot, dates, ticker

HUC8 = '07080106'
directory = 'C:/HSPF_data'
NWISgage  = '05472500'

# make a directory to save all the images

results = 'two_year_dailies'

if not os.path.isdir(results): os.mkdir(results)
    
# open up the 30-year model results

p = '{}/{}/calibration/{}'.format(directory, HUC8, NWISgage)

with open(p, 'rb') as f: hspfmodel = pickle.load(f)

# find the part of the watershed contributing to the gage
    
d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
comid = d[NWISgage]

# make an instance of the postprocessor for the thiry-year
    
start = datetime.datetime(1981, 1, 1)
end   = datetime.datetime(2011, 1, 1)

ds = start, end

postprocessor = Postprocessor(hspfmodel, ds)

# get the flows from the 30-year model

ttimes, tflow = postprocessor.get_sim_flow(comid, dates = ds)

# close the processor

postprocessor.close()

# iterate through each two-year and make the plot

for y in range(1981, 2010, 2):

    print('reading year', y, 'data')

    # path to the two-year model
    
    p = '{}/{}/two_year/{}_{}/{}'.format(directory, HUC8, y, y + 2, NWISgage)
    
    with open(p, 'rb') as f: hspfmodel = pickle.load(f)

    # dates for the calibration
    
    start = datetime.datetime(y,     1, 1)
    end   = datetime.datetime(y + 2, 1, 1)

    ds = start, end

    # use the postprocessor to get the time series
    
    postprocessor = Postprocessor(hspfmodel, ds, comid = comid)

    # get the flows and other timeseries from the two-year calibration
        
    stimes, sflow  = postprocessor.get_sim_flow(comid)
    otimes, oflow  = postprocessor.get_obs_flow()
    ptimes, precip = postprocessor.get_precipitation(comid)
    etimes, evap   = postprocessor.get_evaporation(comid)
    times,  pet    = postprocessor.get_pet(comid = comid)

    # close this postprocessor

    postprocessor.close()

    # get the flows for the thirty-year model

    i = ttimes.index(start)

    if end in ttimes:
        j = ttimes.index(end)
        thirty = tflow[i:j]
    else:
        thirty = tflow[i:]

    # plot it up
    
    fig = pyplot.figure()

    sub1 = pyplot.subplot2grid((3,1),(0,0), rowspan = 2)
    sub2 = pyplot.subplot2grid((3,1),(2,0), sharex = sub1, rowspan = 1)
    sub3 = sub1.twinx()

    # simulated and observed flows
    
    avg = sum(oflow) / len(oflow)
    label = 'Observed (Avg: {:.1f} m\u00B3/s)'.format(avg)
    sub1.plot_date(times, oflow, label = label, marker = 's',
                   markeredgewidth = 0.5, markeredgecolor = 'red',
                   markerfacecolor = 'None', markersize = 4)
    avg = sum(sflow) / len(sflow)
    label = 'Two-Year Model (Avg: {:.1f} m\u00B3/s)'.format(avg)
    sub1.plot_date(times, sflow, label = label, fmt = '-', lw = 0.6,
                   color = 'red')
    avg = sum(thirty) / len(thirty)
    label = 'Thirty-Year Model (Avg: {:.1f} m\u00B3/s)'.format(avg)
    sub1.plot_date(times, thirty, label = label, fmt = '--',
                   color = 'k')

    sub1.set_ylabel('Flow\n(m\u00B3/s)', weight = 'bold')
    sub1.set_ylim((0, 1.5 * max(max(oflow), max(sflow))))
    sub1.xaxis.set_major_locator(dates.MonthLocator((2,6,10)))
    sub1.xaxis.set_major_formatter(dates.DateFormatter('%b-%y'))

    sub1.legend(bbox_to_anchor = (0, 0.7), loc = 'center left',
                fontsize = 10, frameon = False, columnspacing = 0.6)

    for t in sub1.get_xticklabels(): t.set_visible(False)

    # evapotranspiration

    sub2.plot_date(times, pet, fmt = '-', color = 'orange',
                   label = 'Potential')
    sub2.plot_date(times, evap, fmt = '-', color = 'green',
                   label = 'Two-Year Model')
    sub2.fill_between(times, 0, evap, color = 'green', alpha = 0.5)
    sub2.set_xlabel('Date', weight = 'bold')
    sub2.set_ylabel('Evapotranspiration\n(mm)', weight = 'bold')
    sub2.set_ylim((0,10))
    sub2.set_yticks(range(0,12,2))    
    sub2.legend(bbox_to_anchor = (0, 0.99), loc = 'upper left',
                fontsize = 10, frameon = False, columnspacing = 0.6)
    
    # precipitation
    
    sub3.fill_between(times, 0, precip, color = 'blue', alpha = 0.5)
    sub3.plot_date(times, precip, color = 'blue', fmt = '-')
    sub3.invert_yaxis()
    sub3.set_ylim((4 * max(precip), 0))
    sub3.set_ylabel('Precipitation\n(mm)', weight = 'bold')
    sub3.yaxis.set_ticks([t for t in range(0,120,20)])
    sub3.yaxis.set_label_coords(1.06, 0.75)

    for t in (sub1.get_yticklabels() +
              sub2.get_yticklabels() +
              sub3.get_yticklabels() +
              sub3.get_xticklabels()):
        t.set_fontsize(11)
    
    pyplot.subplots_adjust(hspace = 0.14, right = 0.89)

    pyplot.savefig('{}/{}_{}_daily.png'.format(results, y, y + 2))
    #pyplot.show()
    
