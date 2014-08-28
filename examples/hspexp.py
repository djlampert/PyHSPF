# hspexp.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Runs the HSPF model uci file from a calibration done for the Hunting Creek
# HSPF model provided with HSPExp. The model was calibrated using HSPExp.
# This script calculates the HSPExp parameters and plots the results using 
# matplotlib. The Postprocessor can do all the things in this script if the 
# model is built with PyHSPF.
#

import os, datetime, calendar, numpy

from scipy      import stats
from matplotlib import pyplot, ticker, dates

from pyhspf import hspf, WDMUtil

# files for the simulation

directory = '{}/data/calibrated'.format(os.getcwd()) # HSPF file location
ucifile   = 'hunting_calibrated.uci'                 # calibrated HSPEXP UCI
wdmfile   = 'hunting.wdm'                            # unmodified WDM file
plotfile  = 'calibration.png'                        # matplotlib plot file

def aggregate_daily_monthly(dates, daily, option = 'average'):

    months, monthly = [dates[0]], [daily[0]]

    # iterate through the data

    for t, v in zip(dates[1:], daily[1:]):
        
        # check if it's a new month

        if months[-1].month != t.month:

            # start a new month

            months.append(t)
            monthly.append(v)

        # otherwise add the value to the monthly total

        elif v is not None and monthly[-1] is not None: monthly[-1] += v

        else: monthly[-1] = None

    # change from total to average 

    if option == 'average': 
            
        for i in range(len(months)):

            # get the number of days in the last month

            day, ndays = calendar.monthrange(months[i].year, 
                                             months[i].month)

            # and divide to get the average

            if monthly[i] is not None:
                monthly[i] = monthly[i] / ndays
            else: 
                monthly[i] = None

    return months, monthly

# run the script using the main call; some issues in the past with Fortran

if __name__ == '__main__':

    # move into the working directory

    os.chdir(directory)

    # check that the file exists

    if not os.path.isfile(ucifile):
        print('file does not exist')
        raise

    # find the path to the HSPF message file

    pyhspfdirectory = os.path.dirname(hspf.__file__)
    messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(pyhspfdirectory)

    # run the simulation

    hspf.hsppy(ucifile, messagepath)

    wdm = WDMUtil()

    wdm.open(wdmfile, 'r')

    dsns = wdm.get_datasets(wdmfile)

    # see the datasets in the WDM file

    #for n in dsns: print(n, wdm.get_attribute(wdmfile, n, 'TSTYPE'))

    # look at the UCI file to get more info on the datasets

    precip = wdm.get_data(wdmfile, 106)
    evap   = wdm.get_data(wdmfile, 426)
    pet    = wdm.get_data(wdmfile, 425)
    rovol  = wdm.get_data(wdmfile, 420) # acre-ft
    oflow  = wdm.get_data(wdmfile, 281) # cfs

    start, end = wdm.get_dates(wdmfile, 420)

    # calculate the watershed area from the SCHEMATIC block in the UCI file

    area = (32   + 
            6    + 
            1318 + 
            193  + 
            231  + 
            84   +
            3078 + 
            449  + 
            540  + 
            35   
            )

    #print('watershed area: {} acres'.format(area))

    # convert ROVOL and observed flows to m3/s

    ft3m3 = 35.314

    # conversion from uci ext targets

    c = 0.0020107

    sflow = [r * 43560 / 3600 / ft3m3 / c for r in rovol]
    oflow /= ft3m3

    # aggregate the hourly to daily flows

    dsflows = [sum(sflow[i:i+24]) / 24 for i in range(0, len(sflow) - 23, 24)]
    doflows = [sum(oflow[i:i+24]) / 24 for i in range(0, len(oflow) - 23, 24)]

    # simulation times

    times = [start + i * datetime.timedelta(days = 1) 
             for i in range((end - start).days)]

    # aggregate the daily to monthly

    ms, msflows = aggregate_daily_monthly(times, dsflows)
    ms, moflows = aggregate_daily_monthly(times, doflows)

    # daily r2

    slope, intercept, daily_r, p, std_err = stats.linregress(doflows, dsflows)

    # daily log r2

    log_o = [numpy.log(f) for f in doflows]
    log_s = [numpy.log(f) for f in dsflows]

    slope, intercept, daily_logr, p, std_err = stats.linregress(log_o, log_s)

    # Daily Nash Sutcliffe Efficiency

    dailyNS = (1 - sum((numpy.array(dsflows) - numpy.array(doflows))**2) /
               sum((numpy.array(doflows) - numpy.mean(doflows))**2))

    # Daily log Nash Sutcliffe Efficiency

    daily_logNS = (1 - sum((numpy.array(log_s) - numpy.array(log_o))**2) /
                   sum((numpy.array(log_o) - numpy.mean(log_o))**2))

    # monthly r2

    slope, intercept, monthly_r, p, std_err = stats.linregress(moflows, msflows)

    # monthly log r2

    log_o = [numpy.log(f) for f in moflows]
    log_s = [numpy.log(f) for f in msflows]

    slope, intercept, monthly_logr, p, std_err = stats.linregress(log_o, log_s)

    # Monthly Nash Sutcliffe Efficiency

    monthlyNS = (1 - sum((numpy.array(msflows) - numpy.array(moflows))**2) /
                 sum((numpy.array(moflows) - numpy.mean(moflows))**2))

    # Monthly log Nash Sutcliffe Efficiency

    monthly_logNS = (1 - sum((numpy.array(log_s) - numpy.array(log_o))**2) /
                     sum((numpy.array(log_o) - numpy.mean(log_o))**2))

    print('daily stats:')
    print(daily_r**2, daily_logr**2, dailyNS, daily_logNS)
    print('monthly stats:')
    print(monthly_r**2, monthly_logr**2, monthlyNS, monthly_logNS)

    # calculate the mean flow rate

    s_mean = sum(dsflows) / len(dsflows)
    o_mean = sum(doflows) / len(doflows)

    # calculate the recession rates

    srates = [f1 / f2 for f1, f2 in zip(dsflows[1:], dsflows[:-1])]
    srates.insert(0, 1)
    
    orates = [f1 / f2 for f1, f2 in zip(doflows[1:], doflows[:-1])]
    orates.insert(0, 1)

    # remove the recession rates where flows increased (thus not recession)

    srates = [r for r in srates if r < 1]
    orates = [r for r in orates if r < 1]

    # get the mean low flow recession rates (lowest 30%)

    percentile = 30
    cutoff = stats.scoreatpercentile(srates, 100 - percentile)
    highest = [r for r in srates if r > cutoff]

    s_low_recession = sum(highest) / len(highest)

    percentile = 30
    cutoff = stats.scoreatpercentile(orates, 100 - percentile)
    highest = [r for r in orates if r > cutoff]

    o_low_recession = sum(highest) / len(highest)

    # get the mean low flows (lowest 50%)

    cutoff = stats.scoreatpercentile(dsflows, 50)
    low_flows = [f for f in dsflows if f < cutoff]

    s_low_flow = sum(low_flows) / len(low_flows)

    cutoff = stats.scoreatpercentile(doflows, 50)
    low_flows = [f for f in doflows if f < cutoff]

    o_low_flow = sum(low_flows) / len(low_flows)

    # get the mean high flows (highest 10%)

    cutoff = stats.scoreatpercentile(dsflows, 90)
    high_flows = [f for f in dsflows if f > cutoff]

    s_high_flow = sum(high_flows) / len(high_flows)

    cutoff = stats.scoreatpercentile(doflows, 90)
    high_flows = [f for f in doflows if f > cutoff]

    o_high_flow = sum(high_flows) / len(high_flows)

    # get the mean summer (Jun 1 - Sept 1) flows

    summer_flows = [f for t, f in zip(times, dsflows) 
                    if 5 < t.month and t.month < 9]
    s_summer_flow = sum(summer_flows) / len(summer_flows)

    summer_flows = [f for t, f in zip(times, doflows) 
                    if 5 < t.month and t.month < 9]
    o_summer_flow = sum(summer_flows) / len(summer_flows)

    # get the mean winter (Dec 1 - Mar 1) flows

    winter_flows = [f for t, f in zip(times, dsflows) 
                    if t.month == 12 or t.month < 3]
    s_winter_flow = sum(winter_flows) / len(winter_flows)

    winter_flows = [f for t, f in zip(times, doflows) 
                    if t.month == 12 or t.month < 3]
    o_winter_flow = sum(winter_flows) / len(winter_flows)

    # get the storm flows -- lots provided in the HUNTING.EXS file

    storms = [[datetime.datetime(1989, 2, 20), datetime.datetime(1989, 2, 26)],
              [datetime.datetime(1989, 5,  1), datetime.datetime(1989, 5,  5)],
              [datetime.datetime(1989, 6,  6), datetime.datetime(1989, 6, 11)],
              [datetime.datetime(1989, 9, 25), datetime.datetime(1989, 9, 29)],
              [datetime.datetime(1989,10,  1), datetime.datetime(1989,10,  5)],
              [datetime.datetime(1989,12, 31), datetime.datetime(1990, 1,  5)],
              [datetime.datetime(1990, 1, 25), datetime.datetime(1990, 1, 30)],
              [datetime.datetime(1990, 5,  4), datetime.datetime(1990, 5,  8)],
              [datetime.datetime(1990, 5, 28), datetime.datetime(1990, 6,  1)],
              [datetime.datetime(1990, 6, 15), datetime.datetime(1990, 6, 18)],
              ]
    
    storm_flows = []
    for t, f in zip(times, dsflows):

        if any([(t1 <= t and t < t2) for t1, t2 in storms]): 
            storm_flows.append(f)

    s_storm_flow = sum(storm_flows) / len(storm_flows)

    storm_flows = []
    for t, f in zip(times, doflows):

        if any([(t1 <= t and t < t2) for t1, t2 in storms]): 
            storm_flows.append(f)

    o_storm_flow = sum(storm_flows) / len(storm_flows)

    # get the peak stormflows

    s_peak_flows = []
    o_peak_flows = []
    for t1, t2 in storms:

        i, j = times.index(t1), times.index(t2)
        
        s_peak_flows.append(max(dsflows[i:j]))
        o_peak_flows.append(max(doflows[i:j]))
        
    s_peak_flow = sum(s_peak_flows) / len(s_peak_flows)
    o_peak_flow = sum(o_peak_flows) / len(o_peak_flows)

    # calculate HSPExp errors

    toterror    = s_mean / o_mean - 1
    recerror    = s_low_recession / o_low_recession - 1
    lowerror    = s_low_flow / o_low_flow - 1
    higherror   = s_high_flow / o_high_flow - 1
    summererror = s_summer_flow / o_summer_flow - 1
    wintererror = s_winter_flow / o_winter_flow - 1
    stormerror  = s_storm_flow / o_storm_flow - 1
    peakerror   = s_peak_flow / o_peak_flow - 1

    cal = ('HSPExp Calibration Errors\n\n' +
           'average flow:       {:5.1%}\n'.format(toterror)    +
           'low-flow recession: {:5.1%}\n'.format(recerror)    +
           'lowest 50% flows:   {:5.1%}\n'.format(lowerror)    +
           'highest 10% flows:  {:5.1%}\n'.format(higherror)   +
           'summer flows:       {:5.1%}\n'.format(summererror) +
           'winter flows:       {:5.1%}\n'.format(wintererror) +
           'storm flows:        {:5.1%}\n'.format(stormerror)  +
           'storm peak flows:   {:5.1%}'.format(peakerror)
           )

    print(cal)

    c0 = 'HSPExp Calibration Errors'
    c1 = ('average flow:\n' +
          'low-flow recession:\n' +
          'lowest 50% flows:\n' +
          'highest 10% flows:\n' +
          'summer flows:\n' +
          'winter flows:\n' +
          'storm flows:\n' +
          'storm peak flows:'
          )
    c2 = ('{:5.1%}\n'.format(toterror)    +
          '{:5.1%}\n'.format(recerror)    +
          '{:5.1%}\n'.format(lowerror)    +
          '{:5.1%}\n'.format(higherror)   +
          '{:5.1%}\n'.format(summererror) +
          '{:5.1%}\n'.format(wintererror) +
          '{:5.1%}\n'.format(stormerror)  +
          '{:5.1%}'.format(peakerror)
          )

    # calculate the depth of runoff (mm/yr)

    totrunoff = rovol.sum() / area * 12 * 25.4 * 365 / (end - start).days / c
    totprecip = precip.sum() * 25.4 * 365 / (end - start).days
    totevap   = evap.sum() * 25.4 * 365 / (end - start).days
    totpet    = pet.sum() * 25.4 * 365 / (end - start).days

    # mass balance

    print('\nMass Balance:\n')

    mb = ('precipitation:      {:5.0f} mm\n'.format(totprecip) +
          'evapotranspiration: {:5.0f} mm\n'.format(totevap)   +
          'potentional ET:     {:5.0f} mm\n'.format(totpet)    +
          'runoff:             {:5.0f} mm'.format(totrunoff)
          )

    print(mb)

    titlesize = 12
    axissize  = 11
    ticksize  = 10

    # calculate the Nash-Sutcliffe Efficiency

    dNS = (1 - sum((numpy.array(dsflows) - numpy.array(doflows))**2) /
           sum((numpy.array(doflows) - numpy.mean(doflows))**2))

    maxflow = max(max(doflows), max(dsflows))

    # hydrograph

    trange = range(0,10,2)

    fig = pyplot.figure(figsize = (8,8))

    fig.suptitle('HSPExp Calibration for Hunting Creek', size = 13)

    s1 = pyplot.subplot2grid((2,2), (0,0), colspan = 2)

    s1.set_title('Hydrograph', size = titlesize)
    s1.plot_date(times, dsflows, fmt = 'r-', lw = 0.5)
    s1.plot_date(times, doflows, fmt = 's', markeredgecolor = 'r', 
                 markersize = 3, markerfacecolor = 'None')
    s1.xaxis.set_major_locator(dates.MonthLocator(interval = 3))
    s1.xaxis.set_major_formatter(dates.DateFormatter('%b %y'))
    s1.yaxis.set_ticks(trange)
    s1.set_ylabel('Daily Flows (m\u00B3/s)', size = titlesize)
    s1.set_xlabel('Time', size = titlesize)
    s1.text(0.01, 0.98, c0, ha = 'left', va = 'top', size = 9,
            transform = s1.transAxes)
    s1.text(0.01, 0.93, c1, ha = 'left', va = 'top', size = 8,
            transform = s1.transAxes)
    s1.text(0.21, 0.93, c2, ha = 'right', va = 'top', size = 8,
            transform = s1.transAxes)

    # line-line

    m, b, r, p, std_err = stats.linregress(doflows, dsflows)

    s2 = pyplot.subplot2grid((2,2), (1,0))

    s2.plot([0, maxflow], [0, m * maxflow + b], '--', color = 'black',
             label = 'regression')

    # add the regression info to the plot

    s2.text(0.98, 0.02,
            'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, dNS), 
            transform = s2.transAxes, ha = 'right', va = 'bottom', 
            color = 'black', size = ticksize)

    s2.plot([0, maxflow], [0, maxflow], '-', color = 'black', 
            label = 'parity line')
    s2.plot(doflows, dsflows, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')
    s2.set_title('Parity Plot', size = titlesize)
    s2.legend(loc = 'upper left', fontsize = ticksize)
    s2.set_xlabel('Observed Daily Flows (m\u00B3/s)', size = titlesize)
    s2.set_ylabel('Simulated Daily Flows (m\u00B3/s)', size = titlesize)
    s2.xaxis.set_ticks(trange)
    s2.yaxis.set_ticks(trange)

    # calculate the cdfs for observed and simulated data and transform to z

    norm = stats.norm(0,1)

    doflows.sort()
    L = len(doflows)
    obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    obs_daily_cdf.reverse()

    dsflows.sort()
    L = len(dsflows)
    sim_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    sim_daily_cdf.reverse()

    # daily flow duration curve

    s3 = pyplot.subplot2grid((2,2), (1,1))    

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.001, 0.03, 0.2, 0.5, 0.8, 0.97, 0.999]

    norm = stats.norm(0,1)
    norm_ticks = [norm.ppf(t) for t in ticks]

    s3.set_title('Flow Duration Curves', size = titlesize)
    s3.set_yscale('log')
    s3.set_ylabel('Average Daily Flow (m\u00B3/s)', size = axissize)
    s3.set_xlabel('Probability of Exceedance', size = axissize)
    s3.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
    s3.xaxis.set_ticks(norm_ticks)
    s3.set_xticklabels(ticks)

    s3.plot(obs_daily_cdf, doflows,  '-', color = 'blue',
            label = 'observed daily')
    s3.plot(sim_daily_cdf, dsflows, '-', color = 'red',
             label = 'simulated daily')

    s3.legend(loc = 'upper right', fontsize = ticksize)

    # ticks

    for t in (s1.xaxis.get_major_ticks() + s1.yaxis.get_major_ticks() +
              s2.xaxis.get_major_ticks() + s2.yaxis.get_major_ticks() +
              s3.xaxis.get_major_ticks() + s3.yaxis.get_major_ticks()
              ):

        t.label.set_fontsize(ticksize)

    pyplot.tight_layout()
    pyplot.subplots_adjust(top = 0.92) 

    pyplot.savefig(plotfile)
    pyplot.show()
