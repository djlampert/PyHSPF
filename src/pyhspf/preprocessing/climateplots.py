# HSPF Model Plot Routines
#
# David J. Lampert, PhD, PE
#
# Last updated: 11/16/2013
#
# Purpose: Lots of routines here to generate images for climate data for an HSPF
# model. Descriptions below.
#

from scipy             import stats
from matplotlib        import pyplot
from matplotlib.ticker import MaxNLocator
from matplotlib.dates  import YearLocator, MonthLocator
from calendar          import monthrange, isleap

import os, pickle, math, datetime

def aggregate_timeseries(dataset, start, end, tstep = 'hourly', 
                         function = 'sum', verbose = False): 
    """Aggregates a timeseries for plotting."""

    dataset = [(t, v) for t, v in dataset 
               if start <= t and t <= end and 0 <= v]

    if len(dataset) <= 0:
        if verbose: print('error, dataset contains no data')

        times = [start, start + datetime.timedelta(hours = 1),
                 end - datetime.timedelta(hours = 1), end]
        values = [0, None, None, 1]

        return times, values
 
    hourly, values = zip(*dataset)

    if tstep == 'hourly': times = hourly

    if tstep == 'daily' or tstep == 'monthly':

        daily   = [hourly[0] + i * datetime.timedelta(days = 1)
                   for i in range((hourly[-1] - hourly[0]).days + 1)]

        if function == 'sum':
            dvalues = [sum(values[i:i+24])
                       for i in range(0, len(hourly), 24)]

        if function == 'average':
            dvalues = [sum(values[i:i+24]) / 24 
                       for i in range(0, len(hourly), 24)]

        times, values = daily, dvalues
        
    if tstep == 'monthly':

        days  =  monthrange(daily[0].year, daily[0].month)[1]
        delta =  datetime.timedelta(days = days)
        monthly = [daily[0]]

        if function == 'sum':
            mvalues = [sum(dvalues[:delta.days])]

        if function == 'average':
            mvalues = [sum(dvalues[:delta.days]) / len(dvalues[:delta.days])]

        while monthly[-1] + delta < daily[-1]:
            i = daily.index(monthly[-1])
            j = daily.index(monthly[-1] + delta)

            monthly.append(monthly[-1] + delta)

            if function == 'sum':
                mvalues.append(sum(dvalues[i:j]))

            if function == 'average':
                mvalues.append(sum(dvalues[i:j]) / len(dvalues[i:j]))

            days  = monthrange(monthly[-1].year, monthly[-1].month)[1]
            delta = datetime.timedelta(days = days)
         
        times, values = monthly, mvalues

    return times, values

def average(l):
    if len(l) > 0: return sum(l) / len(l)
    else:          return None

def dayofyear(dates, values):
    """Returns the day of the water year average for the timeseries."""

    year = dates[0].year
    while not isleap(year): year += 1
    delta = datetime.timedelta(days = 1)
    wateryear = [datetime.date(year - 1, 10, 1) + i * delta for i in range(366)]

    watervalues = [average([v for t, v in zip(dates, values)
                            if t.month == day.month and t.day == day.day and
                            v is not None])
                   for day in wateryear]

    return watervalues

def plot_ghcnd(station, start = None, end = None, show = False, output = None,
               verbose = True):
    """Makes a plot of the data from a GHCND station."""

    # some error handling

    precipdata = [(t, p) for t, p in station.precip 
                   if p >= 0 and start <= t and t <= end]
    tmaxdata = [(t, T) for t, T in station.tmax
                if -50 < T and start <= t and t <= end]
    tmindata = [(t, T) for t, T in station.tmin
                if -50 < T and start <= t and t <= end ]
    winddata = [(t, w) for t, w in station.wind
                if 0 <= w and start <= t and t <= end]
    snowdata = [(t, s) for t, s in station.snowdepth
                if 0 <= s and start <= t and t <= end]
    evapdata = [(t, e) for t, e in station.evap
                if 0 <= e and start <= t and t <= end]

    if (len(precipdata) == 0 and len(tmaxdata) == 0 and len(tmindata) == 0 and
        len(winddata) == 0 and len(snowdata) == 0 and len(evapdata) == 0): 
        return

    if len(precipdata) > 0: times, precip = zip(*precipdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        precip = [0, None, None, 1]

    try:
        if start is None: start = station.precip[0][0]
        if end is None:   end   = station.precip[-1][0]
    except:
        print('warning: no data present')
        return

    if verbose: print('making a plot for {}'.format(station.name))

    # make the plot

    fig, subs = pyplot.subplots(5, 1, sharex = True, figsize = (8, 10))

    vars = station.name, start, end
    fig.suptitle('{} Climate Data {:%m-%d-%Y} to {:%m-%d-%Y}'.format(*vars), 
                 size = 14)

    i = 0

    subs[i].plot_date(times, precip, color = 'cyan', fmt = '-',
                      label = 'precipitation')
    subs[i].set_ylabel('Precipitation (mm)', color = 'cyan')

    i = 1

    if len(tmaxdata) > 0: times, temp = zip(*tmaxdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        temp = [0, None, None, 1]

    subs[i].plot_date(times, temp, fmt = '-', color = 'red', lw = 0.5, 
                      label = 'max temperature')

    if len(tmindata) > 0: times, temp = zip(*tmindata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        temp = [0, None, None, 1]

    subs[i].plot_date(times, temp, fmt = '-', color = 'blue', lw = 0.5, 
                      label = 'min temperature')
    subs[i].set_ylabel('Temperature (\u00B0C)', color = 'red')

    i = 2
    
    if len(winddata) > 0: times, wind = zip(*winddata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        wind = [0, None, None, 1]

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].set_ylabel('Wind Speed (m/s)', color = 'purple')

    i = 3
    
    if len(snowdata) > 0: times, snow = zip(*snowdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        snow = [0, None, None, 1]
    subs[i].plot_date(times, snow, color = 'gray', lw = 0.5, fmt = '-', 
                      label = 'snowdepth')

    subs[i].set_ylabel('Snow Depth (mm)', color = 'gray')

    i = 4

    if len(evapdata) > 0:
        times, evap = zip(*evapdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        evap = [0, None, None, 1]
    subs[i].plot_date(times, evap, label = 'evaporation', color = 'green', 
                      fmt = '-')

    subs[i].set_ylabel('Pan Evaporation (mm)', color = 'green')

    subs[-1].set_xlabel('Date', size = 12)
    subs[0].xaxis.set_major_locator(MaxNLocator(10))

    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_gsod(station, start = None, end = None, show = False, output = None,
              verbose = True):
    """Makes a plot of the data from a GSOD station."""

    try:
        if start is None: start = station.tmax[0][0]
        if end is None:   end   = station.tmax[-1][0]
    except:
        print('warning: no data present')
        return

    if verbose: print('plotting GSOD data\n')

    # some error handling

    precipdata = [(t, p) for t, p in station.precip 
                  if p >= 0 and start <= t and t <= end]
    tmaxdata = [(t, T) for t, T in station.tmax
                if -50 < T and start <= t and t <= end]
    tmindata = [(t, T) for t, T in station.tmin
                if -50 < T and start <= t and t <= end ]
    dewtdata = [(t, T) for t, T in station.dewpoint
                if -50 < T and start <= t and t <= end ]
    winddata = [(t, w) for t, w in station.wind
                if 0 <= w and start <= t and t <= end]

    if (len(precipdata) == 0 and len(tmaxdata) == 0 and len(tmindata) == 0 and
        len(winddata) == 0 and len(dewtdata) == 0): 
        print('warning: no data present')
        return


    if verbose: print('making a plot for {}'.format(station.name))

    # make the plot

    fig, subs = pyplot.subplots(3, 1, sharex = True, figsize = (8, 8))

    var = station.name, start, end
    fig.suptitle('{} Climate Data {:%m-%d-%Y} to {:%m-%d-%Y}'.format(*var), 
                 size = 14)

    i = 0

    if len(precipdata) > 0: times, precip = zip(*precipdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        precip = [0, None, None, 1]

    subs[i].plot_date(times, precip, fmt = '-', color = 'cyan', lw = 0.5, 
                      label = 'precipitation')

    subs[i].set_ylabel('Precipitation (mm)', color = 'cyan')

    i = 1

    if len(tmaxdata) > 0: times, temp = zip(*tmaxdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        temp = [0, None, None, 1]

    subs[i].plot_date(times, temp, fmt = '-', color = 'red', lw = 0.5, 
                      label = 'max temperature')

    if len(tmindata) > 0: times, temp = zip(*tmindata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        temp = [0, None, None, 1]

    subs[i].plot_date(times, temp, fmt = '-', color = 'blue', lw = 0.5, 
                      label = 'min temperature')
    subs[i].set_ylabel('Temperature (\u00B0C)', color = 'red')

    if len(dewtdata) > 0: times, temp = zip(*dewtdata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        temp = [0, None, None, 1]

    subs[i].plot_date(times, temp, fmt = '-', color = 'green', lw = 0.5, 
                      label = 'dewpoint')
    subs[i].set_ylabel('Temperature (\u00B0C)', color = 'red')

    subs[i].legend(fontsize = 10, loc = 'lower left')

    i = 2
    
    if len(winddata) > 0: times, wind = zip(*winddata)
    else:
        times = [start, start + datetime.timedelta(days = 1), 
                 end - datetime.timedelta(days = 1), end]
        wind = [0, None, None, 1]

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].set_ylabel('Wind Speed (m/s)', color = 'purple')

    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_nsrdb(station, start, end, show = False, output = None, 
               verbose = True):
    """Makes a plot of the data."""

    if verbose: print('making a plot for {}'.format(station.station))
 
    fig = pyplot.figure()
    sub = fig.add_subplot(111)

    sub.set_title('Average Daily Solar Radition, {}'.format(station.station))
    sub.set_xlabel('Time')
    sub.set_ylabel('Solar Radiation (kW hr/m\u00B2/day)')

    for dataset, l, c in zip([station.observed, station.suny, 
                              station.metstat, station.legacy],
                             ['observed', 'SUNY', 'METSTAT', 'legacy'],
                             ['red', 'orange', 'blue', 'green']):

        times, values = aggregate_timeseries(dataset, start, end,
                                             tstep = 'monthly', 
                                             function = 'average')

        # convert from W h / m2 to kW h / m2 / day

        values = [v / 1000 * 24 if v is not None else None for v in values]
        sub.plot_date(times, values, label = l, color = c, fmt = '-', lw = 0.4)

    sub.legend(loc = 'lower left')

    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_precipitation(precip_file, HUC8, year, tstep = 'hourly', n = 4, 
                       output = None, verbose = True):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if verbose: print('plotting year {} precipitation'.format(year))

    if output is None: 
        output = os.getcwd() + '/%d%sprecipitation' % (year, tstep)
    with open(precip_file, 'rb') as f: precipitation = pickle.load(f)

    # figure out how many images needed to show "n" graphs/page and how many
    # to put on each page

    if len(precipitation) < n: l = [0, len(precipitation) - 1]
    else:
        floor     = len(precipitation) // n
        remainder = len(precipitation) % n
        l = [n * i for i in range(floor + 1)]
        if remainder > 0: l += [len(precipitation)]

    # set the dates
    
    start_date = datetime.datetime(year, 1, 1)
    end_date   = datetime.datetime(year + 1, 1, 1)

    # set the cutoff limits for the number of stations

    #totals = [precipitation[p].total_precipitation() for p in precipitation]

    #totals.sort()
    #totals.reverse()
    #cutoff = totals[n - 1]

    # make the figures and subplots

    for i, j in zip(l[:-1], l[1:]):
        fig, subs = pyplot.subplots(j-i, 1, sharex = True, sharey = True,
                                    figsize = [8.5, 11])

        fig.suptitle('%d Precipitation Data, HUC %s' % (year, HUC8), size = 14)

        for p, sub in zip(list(precipitation.keys())[i:j], subs):

            ts = precipitation[p].make_timeseries(start_date, end_date, 
                                                  tstep = tstep)
            tot = precipitation[p].total_precipitation(start_date, end_date)

            if ts is None: # no data available, fill with zeros
                ts = [0, 1, None, None]
                dates = [start_date, start_date + datetime.timedelta(hours = 1),
                         end_date, end_date - datetime.timedelta(hours = 1)]

            elif tstep == 'hourly':
                dates = [start_date + i * datetime.timedelta(hours = 1) 
                         for i in range(len(ts))]
                #ts = hourly
                l = 'hr'
                
            elif tstep == 'daily':
                #daily = aggregate_hourly_daily(hourly)
                dates = [start_date + i * datetime.timedelta(hours = 24) 
                         for i in range(len(ts))]
                #ts = daily
                l = 'd'

            nones = [-0.1 if pr is None else None for pr in ts]

            sub.plot_date(x = dates, y = ts, fmt = '-', color = 'blue')
            sub.plot_date(x = dates, y = nones, fmt = '-', color = 'red')

            sub.set_ylabel('Prec (mm)')
            sub.xaxis.set_major_locator(MonthLocator())
            sub.yaxis.set_major_locator(MaxNLocator(5))
            sub.text(0.02, 0.95, '{} {}'.format(p, precipitation[p].name), 
                     transform = sub.transAxes, ha = 'left', va = 'top',
                     size = 8)
            sub.text(0.98, 0.95, 'Total = {:.1f} in'.format(tot), 
                     transform = sub.transAxes, ha = 'right', 
                     va = 'top', size = 8)

        fig.autofmt_xdate()
        subs[-1].set_xlabel('Date')
        for tick in subs[-1].xaxis.get_major_ticks():
            tick.label.set_fontsize(10)
        pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
        #pyplot.show()
        pyplot.savefig('{}_{}_{}'.format(output, i, j))
        pyplot.clf()
        pyplot.close()

def plot_3240precip(station, start = None, end = None, tstep = 'daily', 
                    show = False, output = None):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if len(station.events) == 0:
        print('warning: station contains no data')
        return
    if start is None: start = station.events[0][0]
    if end is None:   end   = station.events[-1][0]

    # make the figure and subplots

    fig = pyplot.figure()
    sub = fig.add_subplot(111)

    v = station.coop, station.desc
    f = sub.set_title('Coop Station {}: {} Precipitation Data'.format(*v), 
                      size = 14)

    pct = station.pct_missing(start = start, end = end)
    tot = station.total_precipitation(start = start, end = end)

    avg = tot / (end - start).days * 365

    precip = station.make_timeseries(start = start, end = end, tstep = 'hourly')

    if precip is None: # no data available, fill with zeros
        precip = [0. for i in range(int((end - start).days // 24))] 

    if tstep == 'hourly':
        dates = [start + i * datetime.timedelta(hours = 1) 
                 for i in range(len(precip))]
        nones  = [-1 if p is None else 0 for p in precip]
        l = 'hr'
                
    elif tstep == 'daily':
        nones  = [-1 if all([p is None for p in precip[i:i+24]]) else 0 
                   for i in range(0, len(precip), 24)]
        precip = [sum([p for p in precip[i:i+24] if p is not None]) 
                  for i in range(0, len(precip), 24)]
        dates = [start + i * datetime.timedelta(hours = 24) 
                 for i in range(len(precip))]
        l = 'd'

    else:
        print('unable to plot data')
        raise

    t = 'Average Precipitation = {:.1f} in\n{:.1%} Missing Data'.format(avg,pct)

    sub.fill_between(dates, 0, precip, color = 'blue', alpha = 0.5)
    sub.fill_between(dates, 0, nones, color = 'red', alpha = 0.5)

    sub.set_ylabel('Prec (in)')
    sub.xaxis.set_major_locator(YearLocator(3))
    sub.yaxis.set_major_locator(MaxNLocator(4))
    sub.text(0.98, 0.95, t, ha = 'right', va = 'top', size = 10, 
             transform = sub.transAxes)

    #fig.autofmt_xdate()
    sub.set_xlabel('Date')
    for tick in sub.xaxis.get_major_ticks():
        tick.label.set_fontsize(10)

    if output is not None: pyplot.savefig(output)
    if show: pyplot.show()

    pyplot.clf()
    pyplot.close()

def plot_evaporation(evap_file, HUC8, year, output = None, verbose = True):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if verbose: print('plotting {} for year {}'.format(HUC8, year))

    if output is None: 
        output = os.getcwd() + '/%devaporation' % year
    with open(evap_file, 'rb') as f: evaporation = pickle.load(f)

    # set the dates
    
    start_date = datetime.datetime(year, 1, 1, 0, 0)
    end_date   = datetime.datetime(year + 1, 1, 1, 0, 0)

    # make the figure and subplots

    fig, subs = pyplot.subplots(len(evaporation), 1, sharex = True, 
                                sharey = True, figsize = [8.5, 11])

    fig.suptitle('%d Evaporation Data, HUC %s' % (year, HUC8), size = 14)

    if len(evaporation) == 1: subs = [subs]

    i = 0
    for e in evaporation:

        evap = evaporation[e].make_timeseries(start_date, end_date)

        if evap is None:
            evap  = [0, None, None, 20]
            dates = [start_date, start_date + datetime.timedelta(days = 1),
                     end_date - datetime.timedelta(days = 1), end_date]

        if all([e is None for e in evap]):
            evap  = [0, None, None, 20]
            dates = [start_date, start_date + datetime.timedelta(days = 1),
                     end_date - datetime.timedelta(days = 1), end_date]
            
        else:
            dates = [start_date + i * datetime.timedelta(days = 1) 
                     for i in range(len(evap))]

        subs[i].plot_date(x = dates, y = evap, color = 'green', fmt = '-',
                          label = 'evaporation')

        subs[i].set_ylabel('Evaporation (mm)')
        subs[i].yaxis.set_major_locator(MaxNLocator(4))
        subs[i].text(0.02, 0.95, evaporation[e].name + ' {}'.format(e), 
                     transform = subs[i].transAxes, ha = 'left', 
                     va = 'top', size = 8)

        tot = sum([e for e in evap if e is not None])
        subs[i].text(0.98, 0.95, 'Total = %d mm' % tot, transform
                     = subs[i].transAxes, ha = 'right', va = 'top', 
                     size = 8)
        i+=1

    subs[-1].set_xlabel('Date')
    for tick in subs[-1].xaxis.get_major_ticks():
        tick.label.set_fontsize(10)
    fig.autofmt_xdate()
    pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
    #pyplot.show()
    pyplot.savefig(output)
    pyplot.clf()
    pyplot.close()

def plot_temperature(temp_file, HUC8, year, output = None, verbose = False):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if verbose: print('plotting {} for year {}'.format(HUC8, year))
    if output is None: 
        output = os.getcwd() + '/%dtemperature' % year
    with open(temp_file, 'rb') as f: temperature = pickle.load(f)

    # set the dates
    
    start_date = datetime.datetime(year, 1, 1, 0, 0)
    end_date   = datetime.datetime(year + 1, 1, 1, 0, 0)

    # make the figure and subplots

    fig, subs = pyplot.subplots(len(temperature), 1, sharex = True, 
                                sharey = True, figsize = [8.5, 11])

    fig.suptitle('%d Daily High and Low Temperatures, HUC %s' % (year, HUC8), 
                 size = 14)

    i = 0

    if len(temperature) == 1: subs = [subs]
    for t in temperature:

        tmax, tmin = temperature[t].make_timeseries(start_date, end_date)

        # no data available

        if (tmax is None or tmin is None): 
            tmax  = [0, None, None, 20]
            tmin  = tmax
            dates = [start_date, start_date + datetime.timedelta(days = 1),
                     end_date - datetime.timedelta(days = 1), end_date]

        dates = [start_date + i * datetime.timedelta(days = 1) 
                 for i in range(len(tmax))]

        if (all([tm is None for tm in tmax]) or 
            all([tm is None for tm in tmin])):
            tmax  = [0, None, None, 20]
            tmin  = tmax
            dates = [start_date, start_date + datetime.timedelta(days = 1),
                     end_date - datetime.timedelta(days = 1), end_date]

        subs[i].plot_date(x = dates, y = tmax, fmt = 'r-', label = 'High')
        subs[i].plot_date(x = dates, y = tmin, fmt = 'b-', label = 'Low')

        subs[i].set_ylabel('Temp (\u00B0C)')
        subs[i].yaxis.set_major_locator(MaxNLocator(4))
        subs[i].text(0.02, 0.95, temperature[t].name + ' {}'.format(t), 
                     transform = subs[i].transAxes, ha = 'left', va = 'top',
                     size = 8)
        i+=1

    fig.autofmt_xdate()
    for tick in subs[-1].xaxis.get_major_ticks():
        tick.label.set_fontsize(10)
            
    subs[-1].set_xlabel('Date', size = 12)
    subs[-1].legend(bbox_to_anchor = (0.98, 0.02), loc = 'lower right', 
                    borderaxespad = 0.)
    pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
    #pyplot.show()
    pyplot.savefig(output)
    pyplot.clf()
    pyplot.close()

def plot_snowdepth(snowfile, HUC8, year, output = None, verbose = False):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if verbose: print('plotting {} for year {}'.format(HUC8, year))
    if output is None: output = os.getcwd() + '/{}snowdepth'.format(year)
    with open(snowfile, 'rb') as f: snowdepth = pickle.load(f)

    # set the dates
    
    start_date = datetime.datetime(year, 1, 1, 0, 0)
    end_date   = datetime.datetime(year + 1, 1, 1, 0, 0)

    # make the figure and subplots

    fig, subs = pyplot.subplots(len(snowdepth), 1, sharex = True, 
                                sharey = True, figsize = [8.5, 11])

    fig.suptitle('{} Snowdepth Observations, HUC {}'.format(year, HUC8), 
                 size = 14)

    i = 0

    if len(snowdepth) == 1: subs = [subs]
    for s in snowdepth:

        snow = snowdepth[s].make_timeseries(start_date, end_date, 
                                            verbose = False)

        # no data available

        if snow is None: 
            snow  = [0, None, None, 20]
            dates = [start_date, start_date + datetime.timedelta(days = 1),
                     end_date - datetime.timedelta(days = 1), end_date]
            subs[i].plot_date(dates, snow, color = 'gray', label = 'snowdepth')

        elif all([s is None for s in snow]):
            snow  = [0, None, None, 20]
            dates = [start_date, start_date + datetime.timedelta(days = 1),
                     end_date - datetime.timedelta(days = 1), end_date]
            subs[i].plot_date(dates, snow, color = 'gray', label = 'snowdepth')

        else:
            dates = [start_date + i * datetime.timedelta(days = 1) 
                     for i in range(len(snow))]

            fill = [s if s is not None else 0 for s in snow]
            subs[i].fill_between(dates, 0, fill, color = 'gray', alpha = 0.5,
                                 label = 'snowdepth')

        subs[i].set_ylabel('Snowdepth (mm)')
        subs[i].yaxis.set_major_locator(MaxNLocator(4))
        subs[i].text(0.02, 0.95, snowdepth[s].name + ' {}'.format(s), 
                     transform = subs[i].transAxes, ha = 'left', va = 'top',
                     size = 8)
        i+=1

    fig.autofmt_xdate()
    for tick in subs[-1].xaxis.get_major_ticks():
        tick.label.set_fontsize(10)
            
    subs[-1].set_xlabel('Date', size = 12)
    pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
    #pyplot.show()
    pyplot.savefig(output)
    pyplot.clf()
    pyplot.close()

def plot_wind(windfile, HUC8, year, output = None, verbose = False):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if verbose: print('plotting {} for year {}'.format(HUC8, year))
    if output is None: output = os.getcwd() + '/{}wind'.format(year)
    with open(windfile, 'rb') as f: wind = pickle.load(f)

    # set the dates
    
    start_date = datetime.datetime(year, 1, 1, 0, 0)
    end_date   = datetime.datetime(year + 1, 1, 1, 0, 0)

    # make the figure and subplots

    fig, subs = pyplot.subplots(len(wind), 1, sharex = True, 
                                sharey = True, figsize = [8.5, 11])

    fig.suptitle('{} Wind Speed Observations, HUC {}'.format(year, HUC8), 
                 size = 14)

    i = 0

    if len(wind) == 1: subs = [subs]
    for w in wind:

        values = wind[w].make_timeseries(start_date, end_date, verbose = True)

        # no data available

        if values is None: 
            dates  = [start_date, start_date + datetime.timedelta(days = 1),
                      end_date - datetime.timedelta(days = 1), end_date]
            values = [0, None, None, 20]

        elif all([v is None for v in values]):
            dates  = [start_date, start_date + datetime.timedelta(days = 1),
                      end_date - datetime.timedelta(days = 1), end_date]
            values = [0, None, None, 20]
            
        else: dates = [start_date + i * datetime.timedelta(days = 1)
                       for i in range(len(values))]

        subs[i].plot_date(dates, values, color = 'purple', fmt = '-',
                          label = 'wind')
        subs[i].set_ylabel('Wind Speed (m/s)')
        subs[i].yaxis.set_major_locator(MaxNLocator(4))
        subs[i].text(0.02, 0.95, wind[w].name + ' {}'.format(w), 
                     transform = subs[i].transAxes, ha = 'left', va = 'top',
                     size = 8)
        i+=1

    fig.autofmt_xdate()
    for tick in subs[-1].xaxis.get_major_ticks():
        tick.label.set_fontsize(10)
            
    subs[-1].set_xlabel('Date', size = 12)
    subs[-1].legend(bbox_to_anchor = (0.98, 0.02), loc = 'lower right', 
                    borderaxespad = 0.)
    pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
    #pyplot.show()
    pyplot.savefig(output)
    pyplot.clf()
    pyplot.close()

def plot_dewpoint(dewfile, HUC8, year, output = None, verbose = False):
    """Generates a series of subplots of the time series of precipitation
    data for a watershed."""

    if verbose: print('plotting {} for year {}'.format(HUC8, year))
    if output is None: output = os.getcwd() + '/{}dewpoint'.format(year)
    with open(dewfile, 'rb') as f: dewpoint = pickle.load(f)

    # set the dates
    
    start_date = datetime.datetime(year, 1, 1, 0, 0)
    end_date   = datetime.datetime(year + 1, 1, 1, 0, 0)

    # make the figure and subplots

    fig, subs = pyplot.subplots(len(dewpoint), 1, sharex = True, 
                                sharey = True, figsize = [8.5, 11])

    fig.suptitle('{} Dew Point Observations, HUC {}'.format(year, HUC8), 
                 size = 14)

    i = 0

    if len(dewpoint) == 1: subs = [subs]
    for d in dewpoint:

        values = dewpoint[d].make_timeseries(start_date, end_date, verbose=True)

        # no data available

        if values is None: 
            dates  = [start_date, start_date + datetime.timedelta(days = 1),
                      end_date - datetime.timedelta(days = 1), end_date]
            values = [0, None, None, 20]

        elif all([v is None for v in values]):
            dates  = [start_date, start_date + datetime.timedelta(days = 1),
                      end_date - datetime.timedelta(days = 1), end_date]
            values = [0, None, None, 20]
            
        else: dates = [start_date + i * datetime.timedelta(days = 1)
                       for i in range(len(values))]

        subs[i].plot_date(dates, values, color = 'green', fmt = '-',
                          label = 'green')
        subs[i].set_ylabel('Dew Point (\u00B0C)')
        subs[i].yaxis.set_major_locator(MaxNLocator(4))
        subs[i].text(0.02, 0.95, dewpoint[d].name + ' {}'.format(d), 
                     transform = subs[i].transAxes, ha = 'left', va = 'top',
                     size = 8)
        i+=1

    fig.autofmt_xdate()
    for tick in subs[-1].xaxis.get_major_ticks():
        tick.label.set_fontsize(10)
            
    subs[-1].set_xlabel('Date', size = 12)
    subs[-1].legend(bbox_to_anchor = (0.98, 0.02), loc = 'lower right', 
                    borderaxespad = 0.)
    pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
    #pyplot.show()
    pyplot.savefig(output)
    pyplot.clf()
    pyplot.close()

def plot_solar(solarfile, HUC8, year, output = None, verbose = False):
    """Generates a series of subplots of the time series of METSTAT solar
    radiation data for a watershed."""

    if verbose: print('plotting {} for year {}'.format(HUC8, year))
    if output is None: output = os.getcwd() + '/{}solar'.format(year)
    with open(solarfile, 'rb') as f: solar = pickle.load(f)

    # set the dates
    
    start = datetime.datetime(year, 1, 1, 0, 0)
    end   = datetime.datetime(year + 1, 1, 1, 0, 0)

    # make the figure and subplots

    fig, subs = pyplot.subplots(len(solar), 1, sharex = True, 
                                sharey = True, figsize = [8.5, 11])

    fig.suptitle('{} METSTAT Solar Radiation, HUC {}'.format(year, HUC8), 
                 size = 14)

    i = 0

    if len(solar) == 1: subs = [subs]
    for s in solar:

        values = solar[s].make_timeseries(start, end, tstep = 'daily',
                                          verbose = True)

        # no data available

        if values is None: 
            dates  = [start, start + datetime.timedelta(days = 1),
                      end - datetime.timedelta(days = 1), end]
            values = [0, None, None, 20]

        elif all([v is None for v in values]):
            dates  = [start, start + datetime.timedelta(days = 1),
                      end - datetime.timedelta(days = 1), end]
            values = [0, None, None, 20]
            
        else: dates = [start + i * datetime.timedelta(days = 1)
                       for i in range(len(values))]

        subs[i].plot_date(dates, values, color = 'yellow', lw = 1.5, fmt = '-')
        subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day')
        subs[i].yaxis.set_major_locator(MaxNLocator(4))
        subs[i].text(0.02, 0.95, solar[s].name + ' {}'.format(s), 
                     transform = subs[i].transAxes, ha = 'left', va = 'top',
                     size = 8)
        i+=1

    fig.autofmt_xdate()
    for tick in subs[-1].xaxis.get_major_ticks():
        tick.label.set_fontsize(10)
            
    subs[-1].set_xlabel('Date', size = 12)
    pyplot.subplots_adjust(top = 0.95, bottom = 0.1)
    #pyplot.show()
    pyplot.savefig(output)
    #pyplot.clf()
    #pyplot.close()

def plot_dailyET(HUC8, start, end, ET, evaporations, tmin, tmax, dewpoint, 
                 wind, solar, axsize = 11, output = None, show = False, 
                 verbose = True):

    if verbose: print('plotting daily potential evapotranspiration for ' +
                      '{}\n'.format(HUC8))

    # make a daily time series

    times = [start + datetime.timedelta(days = 1) * i 
             for i in range((end-start).days)]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))
    
    subs =  [pyplot.subplot2grid((5,1), (0,0), rowspan = 2)]
    subs += [pyplot.subplot2grid((5,1), (i,0), sharex = subs[0]) 
             for i in (2, 3, 4)]

    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    v = HUC8, 'Daily Climate Data', start, end
    subs[0].set_title('{} {} {:%m-%d-%Y} to {:%m-%d-%Y}'.format(*v), size = 14)

    # evaporation and potential evapotranspiration

    i = 0

    colors = ['green', 'orange', 'blue', 'brown']

    for item, c in zip(evaporations.items(), colors):
        k, v = item
        evaporation = v.make_timeseries(start, end)
        subs[i].plot_date(times, evaporation, fmt = 's', markeredgecolor = c,
                          markerfacecolor = 'None', markersize = 3, label = k)
    subs[i].plot_date(times, ET, fmt = '-', color = 'green', lw = 1.,
                      label = 'potential evapotranspiration')
    subs[i].set_ylabel('Evaporation (mm)', size = axsize)
    subs[i].yaxis.set_major_locator(MaxNLocator(10))
    subs[i].legend(fontsize = 8, loc = 'lower left')

    # min, max, and dewpoint temperatures

    i = 1

    subs[i].plot_date(times, tmax, fmt = '-', color = 'red', lw = 0.5, 
                      label = 'max temperature')
    subs[i].plot_date(times, tmin, fmt = '-', color = 'blue', lw = 0.5, 
                      label = 'min temperature')
    subs[i].plot_date(times, dewpoint, fmt = '-', color = 'green', lw = 0.5, 
                      label = 'dewpoint')
    subs[i].set_ylabel('Temperature (\u00B0C)', size = axsize)
    subs[i].yaxis.set_major_locator(MaxNLocator(5))
    subs[i].legend(fontsize = 8, loc = 'lower left')

    # average daily wind speed

    i = 2

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', size = axsize,
                       multialignment = 'center')
    subs[i].yaxis.set_major_locator(MaxNLocator(5))

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                      label = 'solar')
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', color = 'orange',
                       multialignment = 'center', size = axsize)
    subs[i].yaxis.set_major_locator(MaxNLocator(5))

    pyplot.tight_layout()
    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_dayofyearET(HUC8, start, end, evaporations, ETs, tmin, tmax, dewpoint,
                     wind, solar, labels = None, fill = False, axsize = 11,
                     output = None, show = False):
    """Plots the day of the water year estimates of ET from the Penman 
    Model."""

    dates = [start + datetime.timedelta(days = 1) * i 
             for i in range((end-start).days)]

    # figure out the average conditions for each day of the water year

    solar    = dayofyear(dates, solar)
    tmin     = dayofyear(dates, tmin)
    tmax     = dayofyear(dates, tmax)
    dewpoint = dayofyear(dates, dewpoint)
    wind     = dayofyear(dates, wind)
    ETs      = [dayofyear(dates, ET) for ET in ETs]

    # day of year times (2004 is a leap year)

    times = [datetime.datetime(2003,10,1) + datetime.timedelta(days = 1) * i 
             for i in range(366)]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))
    
    subs =  [pyplot.subplot2grid((6,1), (0,0), rowspan = 3)]
    subs += [pyplot.subplot2grid((6,1), (i,0), sharex = subs[0]) 
             for i in (3, 4, 5)]

    v = HUC8, 'Evapotranspiration'
    subs[0].set_title('{} {}'.format(*v), size = 14)

    # evaporation and potential evapotranspiration

    i = 0

    colors = ['green', 'orange', 'blue', 'brown']

    # total annual evaporation

    tots = []
    for item, c in zip(evaporations.items(), colors):
        k, v = item
        evaporation = dayofyear(dates, v.make_timeseries(start, end))
        if len([e for e in evaporation if e is not None]) > 0:
            subs[i].plot_date(times, evaporation, fmt = 's', color = c, 
                              markeredgecolor = c, markerfacecolor = 'None', 
                              markersize = 3, 
                              label = k + ' evaporation')
            tot = sum([e for e in evaporation if e is not None])
            tots.append('\n{}: {:4.0f} mm'.format(k, tot))

    if labels is None: labels = ['Penman Equation' for ET in ETs]

    tots += ['\n{}: {:4.0f} mm'.format(l, sum(e)) for l, e in zip(labels, ETs)]

    # snip the first line break

    tots[0] = tots[0][1:]

    for ET, c, l in zip(ETs, colors, labels):
        subs[i].plot_date(times, ET, color = c, lw = 1., fmt = '-', label = l)
        if fill:
            subs[i].fill_between(times, 0, ET, color = c, alpha = 0.3)

    subs[i].set_ylabel('Evaporation (mm)', size = axsize)
    subs[i].legend(fontsize = 8, loc = 'upper left')
    t = ''.join(tots)
    subs[i].text(0.98, 0.99, t, ha = 'right', va = 'top', 
                 transform = subs[i].transAxes, size = 8)

    # min, max, and dewpoint temperatures

    i = 1

    subs[i].plot_date(times, tmax, fmt = '-', color = 'red', lw = 0.5, 
                      label = 'max temperature')
    subs[i].plot_date(times, tmin, fmt = '-', color = 'blue', lw = 0.5, 
                      label = 'min temperature')
    subs[i].plot_date(times, dewpoint, fmt = '-', color = 'green', lw = 0.5, 
                      label = 'dewpoint')
    subs[i].set_ylabel('Temperature (\u00B0C)', size = axsize)

    subs[i].legend(fontsize = 8, loc = 'lower right')

    # average daily wind speed

    i = 2

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', 
                       multialignment = 'center', size = axsize)

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                      label = 'solar')
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', color = 'orange',
                       multialignment = 'center', size = axsize)

    # ticks

    for s in subs[1:]: s.yaxis.set_major_locator(MaxNLocator(5))
    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels, size = axsize)

    #pyplot.tight_layout()
    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_hourlyET(HUC8, start, end, evaporations, hETs, temp, dewpoint, wind, 
                  solar, title = None, totals = True, labels = None, 
                  colors = None, axsize = 11, fill = False, 
                  output = None, show = False, verbose = True):

    if verbose: print('plotting hourly evapotranspiration\n')

    # figure out the average conditions for each day of the water year

    dates = [start + datetime.timedelta(hours = 1) * i 
             for i in range((end-start).days * 24)]

    solar = dayofyear(dates, solar)
    temp  = dayofyear(dates, temp)
    hETs  = [[24 * p for p in dayofyear(dates, hET)] for hET in hETs]

    # these are daily values

    dates = [start + datetime.timedelta(days = 1) * i 
             for i in range((end - start).days)]

    dewpoint = dayofyear(dates, dewpoint)
    wind     = dayofyear(dates, wind)

    # day of year times (2004 is a leap year)

    times = [datetime.datetime(2003, 10, 1) + datetime.timedelta(days = 1) * i 
             for i in range(366)]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))
    
    subs =  [pyplot.subplot2grid((6,1), (0,0), rowspan = 3)]
    subs += [pyplot.subplot2grid((6,1), (i,0), sharex = subs[0]) 
             for i in (3, 4, 5)]

    if title is None:
        v = HUC8, 'Evapotranspiration'
        subs[0].set_title('{} {}'.format(*v), size = 14)
    else: subs[0].set_title(title, size = 14)

    # evaporation and potential evapotranspiration

    i = 0

    # total annual evaporation

    if labels is None: labels = ['Penman Equation' for h in hETs]

    if totals:

        tots = ['\n{}: {:4.0f} mm'.format(l, sum(e)) 
                for l, e in zip(labels, hETs)]

        # snip the first break

        tots[0] = tots[0][1:]

    else: tots = ['{} Annual Total: {:4.0f} mm'.format(labels[0], sum(hETs[0]))]

    # colors

    #color_codes = {'Empty':       (255, 255, 255),  # empty
    #               'Corn':        (255, 211,   0),  # corn 
    #               'Soybeans':    ( 38, 112,   0),  # soybeans
    #               'Other grain': (255, 221, 165),  # other grain
    #               'Developed':   (155, 155, 155),  # developed
    #               'Wetland':     ( 76, 112, 163),  # water/wetland
    #               'Forest':      (147, 204, 147),  # forest
    #               'Alfalfa':     (255, 165, 226),  # hay/alfalfa
    #               'Fallow':      (191, 191, 119),  # fallow
    #               'Pasture':     (232, 255, 191),  # pasture/grass
    #               'Other':       (  0,   0,   0)   # other
    #               }
    color_codes = {'cereals':     (255, 211,   0),  # corn 
                   'legumes':     ( 38, 112,   0),  # soybeans
                   'alfalfa':     (255, 165, 226),  # hay/alfalfa
                   'wetland':     ( 76, 112, 163),  # water/wetland
                   'fallow':      (191, 191, 119),  # fallow
                   'pasture':     (232, 255, 191),  # pasture/grass
                   'others':      (  0,   0,   0)   # other
                   }

    if colors is None:
        colors = [color_codes[crop] 
                  if crop in color_codes else color_codes['others']
                  for crop in labels]
        colors = [[r / 255, g / 255, b / 255] for r, g, b in colors]

    for hET, c, l in zip(hETs, colors, labels):
        subs[i].plot_date(times, hET, color = c, lw = 1., fmt = '-', label = l)
        if fill:
            subs[i].fill_between(times, 0, hET, color = c, alpha = 0.3)

    fmts = ['s', '+', '*', 'o']

    for item, fmt, color in zip(evaporations.items(), fmts, colors):
        k, v = item
        evaporation = dayofyear(dates, v.make_timeseries(start, end))
        if len([e for e in evaporation if e is not None]) > 0:
            subs[i].plot_date(times, evaporation, fmt = fmt,
                              markerfacecolor = 'None', markersize = 4,
                              markeredgecolor = color,
                              label = k + ' evaporation')

            if totals:
                tot = sum([e for e in evaporation if e is not None])
                tots.append('\n{}: {:4.0f} mm'.format(k, tot))

            else:

                # calculate the linear regression between April 1 and October 31
                # for the reference ET versus the pan evaporation to see the 
                # goodness of fit and estimate the pan coefficient

                xs = [x for t, x in zip(times, evaporation) 
                      if 3 < t.month and t.month < 11] 
                ys = [y for t, y in zip(times, hETs[0]) 
                      if 3 < t.month and t.month < 11] 

                # get the linear regression

                m, b, r, p, std_err = stats.linregress(xs, ys)
            
                # add it to the text box

                its = k, m, b, r**2
                t = '\n{0}: y={1:.2f}x+{2:1.2f}; r\u00B2={3:.2f}'.format(*its)
                tots.append(t)

    subs[i].set_ylabel('Evaporation\n(mm)', color = 'green', size = axsize, 
                       multialignment = 'center')
    subs[i].legend(fontsize = 8, loc = 'upper left')

    t = ''.join(tots)
    subs[i].text(0.98, 0.99, t, ha = 'right', va = 'top', 
                 transform = subs[i].transAxes, size = 8)

    subs[i].tick_params(axis = 'y', colors = 'green', size = 9)

    # temperature and dewpoint

    i = 1

    subs[i].plot_date(times, temp, fmt = '-', color = 'red', lw = 0.5, 
                      label = 'temperature')
    subs[i].plot_date(times, dewpoint, fmt = '-', color = 'green', lw = 0.5, 
                      label = 'dewpoint')
    subs[i].set_ylabel('Temperature\n(\u00B0C)', size = axsize, color = 'red',
                       multialignment = 'center')
    subs[i].tick_params(axis = 'y', colors = 'red', size = 9)

    subs[i].legend(fontsize = 8, loc = 'lower right')

    # average daily wind speed

    i = 2

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].tick_params(axis = 'y', colors = 'purple', size = 9)
    subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', size = axsize, 
                       multialignment = 'center')
    subs[i].set_ylim([0, subs[i].get_ylim()[1]])

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                      label = 'solar')
    subs[i].tick_params(axis = 'y', colors = 'orange', size = 9)
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', color = 'orange',
                       multialignment = 'center', size = axsize)
    subs[i].set_ylim([0, subs[i].get_ylim()[1]])

    # ticks

    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    for s in subs[1:]: 
        s.yaxis.set_major_locator(MaxNLocator(5))
        for t in s.yaxis.get_ticklabels(): t.set_fontsize(9)
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels, size = 9)

    pyplot.tight_layout()
    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()
