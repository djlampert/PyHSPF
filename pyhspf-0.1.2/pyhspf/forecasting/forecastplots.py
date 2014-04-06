# HSPF Model Plot Routines
#
# David J. Lampert, PhD, PE
#
# Last updated: 11/16/2013
#
# Purpose: Lots of routines here to generate images for climate data for an HSPF
# model. Descriptions below.
#

from scipy             import stats, log10
from matplotlib        import pyplot, path, patches, ticker
from matplotlib.dates  import YearLocator, MonthLocator
from calendar          import monthrange, isleap
from itertools         import chain

import os, pickle, math, datetime, numpy as np

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

def stderr(l):
    if len(l) > 2: return 1.96 * np.std(l) / np.sqrt(len(l))
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

def dayofyear_range(dates, values):
    """Returns the day of the water year average for the timeseries."""

    year = dates[0].year
    while not isleap(year): year += 1
    delta = datetime.timedelta(days = 1)
    wateryear = [datetime.date(year - 1, 10, 1) + i * delta for i in range(366)]

    watervalues = [stderr([v for t, v in zip(dates, values)
                           if t.month == day.month and t.day == day.day and
                           v is not None])
                   for day in wateryear]

    return watervalues

def make_patch(points, facecolor, edgecolor = 'Black', width = 1, alpha = None,
               hatch = None, label = None):
    """Uses a list or array of points to generate a matplotlib patch."""

    vertices = [(point[0], point[1]) for point in points]
    vertices.append((points[0][0], points[0][1]))

    codes     = [path.Path.LINETO for i in range(len(points) + 1)]
    codes[0]  = path.Path.MOVETO

    patch = patches.PathPatch(path.Path(vertices, codes), facecolor = facecolor,
                              edgecolor = edgecolor, lw = width, hatch = hatch,
                              alpha = alpha, label = label)
    return patch

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
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(10))
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
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))
    subs[i].legend(fontsize = 8, loc = 'lower left')

    # average daily wind speed

    i = 2

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', size = axsize,
                       multialignment = 'center')
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                      label = 'solar')
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', color = 'orange',
                       multialignment = 'center', size = axsize)
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))

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

    fig = pyplot.figure(figsize = (6,8))
    
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

    for s in subs[1:]: s.yaxis.set_major_locator(ticker.MaxNLocator(5))
    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels, size = axsize)

    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_hourlyET(HUC8, start, end, evaporations, hETs, temp, 
                  dewpoint, wind, solar, labels = None, colors = None, 
                  axsize = 11, fill = False, output = None, show = False, 
                  verbose = True):

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

    v = HUC8, 'Evapotranspiration'
    subs[0].set_title('{} {}'.format(*v), size = 14)

    # evaporation and potential evapotranspiration

    i = 0

    # total annual evaporation

    if labels is None: labels = ['Penman Equation' for h in hETs]

    tots = ['\n{}: {:4.0f} mm'.format(l, sum(e)) for l, e in zip(labels, hETs)]

    # snip the first break

    tots[0] = tots[0][1:]

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

    for item, fmt in zip(evaporations.items(), fmts):
        k, v = item
        evaporation = dayofyear(dates, v.make_timeseries(start, end))
        if len([e for e in evaporation if e is not None]) > 0:
            subs[i].plot_date(times, evaporation, fmt = fmt,
                              markerfacecolor = 'None', markersize = 3, 
                              label = k + ' evaporation')
            tot = sum([e for e in evaporation if e is not None])
            tots.append('\n{}: {:4.0f} mm'.format(k, tot))

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

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                      label = 'solar')
    subs[i].tick_params(axis = 'y', colors = 'orange', size = 9)
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', color = 'orange',
                       multialignment = 'center', size = axsize)

    # ticks

    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    for s in subs[1:]: 
        s.yaxis.set_major_locator(ticker.MaxNLocator(5))
        for t in s.yaxis.get_ticklabels(): t.set_fontsize(9)
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels, size = 9)

    pyplot.tight_layout()
    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

def plot_calibration(HUC8, start, end,
                     obs_daily, 
                     base_daily,
                     hind_daily,
                     obs_monthly,
                     base_monthly,
                     hind_monthly,
                     titlesize = 14, axissize = 12, ticksize = 11, 
                     output = None, show = True, verbose = False):
    """Makes a figure with subplots for the daily simulated versus observed,
    monthly simulated versus observed, and the daily flow duration curve."""

    if verbose: 
        print('plotting the calibration statistics for the simulation\n')

    # make the figure canvas

    figsize = (10, 5)

    fig = pyplot.figure(figsize = figsize)

    # calculate the cdfs for observed and simulated data and transform to z

    norm = stats.norm(0,1)

    obs_daily.sort()
    L = len(obs_daily)
    obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    obs_daily_cdf.reverse()

    base_daily.sort()
    L = len(base_daily)
    base_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    base_daily_cdf.reverse()

    hind_daily.sort()
    L = len(hind_daily)
    hind_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    hind_daily_cdf.reverse()

    obs_monthly.sort()
    L = len(obs_monthly) 
    obs_monthly_cdf = [norm.ppf(i / L) for i in range(L)]
    obs_monthly_cdf.reverse()

    base_monthly.sort()
    L = len(base_monthly)
    base_monthly_cdf = [norm.ppf(i / L) for i in range(L)]
    base_monthly_cdf.reverse()

    hind_monthly.sort()
    L = len(hind_monthly)
    hind_monthly_cdf = [norm.ppf(i / L) for i in range(L)]
    hind_monthly_cdf.reverse()

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.001, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98, 0.999]

    norm_ticks = [norm.ppf(t) for t in ticks]

    # daily flow duration curve
    
    ax5 = pyplot.subplot2grid((1,2), (0,0))

    t = '{} Flow Duration Curves {}-{}\n'.format(HUC8, start.year, end.year)

    fig.suptitle(t, size = titlesize)
    ax5.set_yscale('log')
    ax5.set_ylabel('Daily Flow (m\u00B3/s)', size = axissize)
    ax5.set_xlabel('Probability of Exceedance', size = axissize)
    ax5.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
    ax5.xaxis.set_ticks(norm_ticks)
    ax5.set_xticklabels(ticks)

    ax5.plot(obs_daily_cdf, obs_daily,  '-', color = 'red',  lw = 1.5,
             label = 'observed flows')
    ax5.plot(base_daily_cdf, base_daily, '-', color = 'blue', lw = 1.,
             label = 'observed climate simulated flows')
    ax5.plot(hind_daily_cdf, hind_daily, '--', color = 'blue', lw = 1.,
             label = 'simulated climate simulated flows')

    ax5.legend(loc = 2, fontsize = ticksize)

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98]

    norm_ticks = [norm.ppf(t) for t in ticks]

    # monthly flow duration curve

    ax6 = pyplot.subplot2grid((1,2), (0,1))

    ax6.set_yscale('log')
    ax6.set_ylabel('Monthly Flow (m\u00B3/s)', size = axissize)
    ax6.set_xlabel('Probability of Exceedance', size = axissize)
    ax6.set_xlim([norm.ppf(0.005), norm.ppf(0.995)])
    ax6.xaxis.set_ticks(norm_ticks)
    ax6.set_xticklabels(ticks)

    ax6.plot(obs_monthly_cdf, obs_monthly,  '-', color = 'red', lw = 1.5,
             label = 'observed flows')
    ax6.plot(base_monthly_cdf, base_monthly, '-', color = 'blue', lw = 1.,
             label = 'observed climate simulated flows')
    ax6.plot(hind_monthly_cdf, hind_monthly, '--', color = 'blue', lw = 1.,
             label = 'simulated climate simulated flows')

    ax6.legend(loc = 2, fontsize = ticksize)

    # tick settings

    for t in (ax5.xaxis.get_ticklabels() + ax5.yaxis.get_ticklabels() + 
              ax6.xaxis.get_ticklabels() + ax6.yaxis.get_ticklabels()): 
        t.set_fontsize(ticksize)

    # tighten the borders

    pyplot.tight_layout()
    pyplot.subplots_adjust(top = 0.9)

    # save if needed

    if output is not None: pyplot.savefig(output, dpi = 400)

    # show it

    if show: pyplot.show()

    pyplot.close()

def plot_dayofyear(HUC8, 
                   oprec, 
                   sprec, 
                   bflow,
                   hflow,
                   bevap,
                   hevap,
                   oflow = None, 
                   bands = False, lw = 1.5, labelsize = 12, ticksize = 9, 
                   legsize = 8, output = None, show = False):
    """Plots the day of the water year estimates of precipitation, 
    evapotranspiration, and runoff for the model."""

    # get the start and end year

    start, end = bflow[0][0], bflow[0][-1]

    # get the standard deviation of the precipitation, evapotranspiration,flows

    bflow_range = dayofyear_range(*bflow)
    hflow_range = dayofyear_range(*hflow)
    oprec_range = dayofyear_range(*oprec)
    sprec_range = dayofyear_range(*sprec)
    bevap_range = dayofyear_range(*bevap)
    hevap_range = dayofyear_range(*hevap)

    # figure out the average conditions for each day of the water year

    oprec = dayofyear(*oprec)
    sprec = dayofyear(*sprec)
    bflow = dayofyear(*bflow)
    hflow = dayofyear(*hflow)
    bevap = dayofyear(*bevap)
    hevap = dayofyear(*hevap)

    ls     = ['Precipitation:', 
              '\nSimulated Evapotranspiration:', '\nSimulated Runoff:']
    series = oprec, bevap, bflow
            
    tots = ['\n{:<4.0f} mm'.format(sum(t)) for t in series]

    if oflow is not None:

        oflow = dayofyear(*oflow)
        ls.append('\nObserved Runoff')
        tots.append('\n{:<4.0f} mm'.format(sum(oflow)))

    # snip the first line break

    tots[0] = tots[0][1:]

    # day of year times (2004 is a leap year)

    times = [datetime.datetime(2003, 10, 1) + datetime.timedelta(days = 1) * i 
             for i in range(366)]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))

    subs = [pyplot.subplot2grid((8,1), (0,0), rowspan = 2)]

    subs += [pyplot.subplot2grid((8,1), (2,0), rowspan = 4, sharex = subs[0]),
             pyplot.subplot2grid((8,1), (6,0), rowspan = 2, sharex = subs[0])
             ]

    v = HUC8, start.year, end.year
    subs[0].set_title('{} Average Daily Hydrograph {}-{}'.format(*v), 
                      size = 14)

    # precipitation

    prec_color = 'blue'
    subs[0].invert_yaxis()
    subs[0].plot_date(times, oprec, markeredgecolor = prec_color,
                      markersize = 3, markerfacecolor = 'None', lw = lw, 
                      fmt = 's', label = 'observed precipitation')
    subs[0].plot_date(times, sprec, color = prec_color, lw = lw, 
                      fmt = '-', label = 'simulated precipitation')
    subs[0].set_ylabel('Precipitation\n(mm)', color = prec_color, 
                       size = labelsize, multialignment = 'center')

    # add an error band

    if bands:
        minprec = [s if e is None else s - e
                   for s, e in zip(oprec, oprec_range)]
        maxprec = [s if e is None else s + e 
                   for s, e in zip(oprec, oprec_range)]

        subs[0].fill_between(times, minprec, maxprec, color = prec_color, 
                             alpha = 0.2)

    hs, ls = subs[0].get_legend_handles_labels()
    leg = subs[0].legend(hs, ls, loc = 'lower left', fontsize = legsize)

    # observed and simulated flow

    flow_color = 'red'
    subs[1].plot_date(times, bflow, fmt = '-', color = flow_color, 
                      lw = lw, label = 'observed climate simulated flow')
    subs[1].plot_date(times, hflow, fmt = '-+', color = 'orange', 
                      lw = lw, label = 'simulated climate simulated flow')
    subs[1].set_ylabel('Runoff\n(mm)', color = 'red', 
                       multialignment = 'center', size = labelsize,)

    if oflow is not None:
        subs[1].plot_date(times, oflow, fmt = 's', markersize = 3,
                          markeredgecolor = flow_color, 
                          markerfacecolor = 'None', label = 'observed flow')

    # add an error band

    if bands:

        minflow = [s if e is None else s - e
                   for s, e in zip(bflow, bflow_range)]
        maxflow = [s if e is None else s + e 
                   for s, e in zip(bflow, bflow_range)]

        subs[1].fill_between(times, minflow, maxflow, color = flow_color, 
                             alpha = 0.2)


    # add the totals

    t1 = ''.join(ls)
    t2 = ''.join(tots)
    #subs[1].text(0.01, 0.97, t1, ha = 'left', va = 'top', 
    #             transform = subs[1].transAxes, size = 8)
    #subs[1].text(0.30, 0.97, t2, ha = 'left', va = 'top', 
    #             transform = subs[1].transAxes, size = 8)

    hs, ls = subs[1].get_legend_handles_labels()
    leg = subs[1].legend(hs, ls, loc = 'upper left', fontsize = legsize)

    # evaporation

    evap_color = 'green'
    subs[2].plot_date(times, bevap, fmt = 's', markeredgecolor = evap_color,
                      markerfacecolor = 'None', markersize = 3, lw = lw, 
                      label = 'observed climate simulated evapotranspiration')
    subs[2].plot_date(times, hevap, fmt = '-', color = evap_color,
                      lw = lw, 
                      label = 'simulated climate simulated evapotranspiration')
    subs[2].set_ylabel('Evapotranspiration\n(mm)', color = evap_color,
                       multialignment = 'center', size = labelsize)

    # add an error band

    if bands:

        minevap = [s if e is None else s - e
                   for s, e in zip(bevap, bevap_range)]
        maxevap = [s if e is None else s + e 
                   for s, e in zip(bevap, bevap_range)]

        subs[2].fill_between(times, minevap, maxevap, color = evap_color, 
                             alpha = 0.2)

    # axis limits

    mi, ma = subs[0].get_ylim()
    subs[0].set_ylim([mi, 0])
    mi, ma = subs[1].get_ylim()
    subs[1].set_ylim([0, ma])
    mi, ma = subs[2].get_ylim()
    subs[2].set_ylim([0, ma])

    hs, ls = subs[2].get_legend_handles_labels()
    leg = subs[2].legend(hs, ls, loc = 'upper left', fontsize = legsize)

    # ticks

    subs[0].yaxis.set_major_locator(ticker.MaxNLocator(3))
    subs[1].yaxis.set_major_locator(ticker.MaxNLocator(5))
    subs[2].yaxis.set_major_locator(ticker.MaxNLocator(5))
    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[0].tick_params(axis = 'y', size = ticksize, colors = 'blue')
    subs[1].tick_params(axis = 'y', size = ticksize, colors = 'red')
    subs[2].tick_params(axis = 'y', size = ticksize, colors = 'green')

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels)

    # finish up

    if output is not None: pyplot.savefig(output, dpi = 400)

    # show it

    if show: pyplot.show()

    pyplot.clf()
    pyplot.close()

def plot_waterbudget(HUC8, precipitation, potential_evap, simulated_evap, 
                     simulated_flow, simulated_gw, tstep = 'monthly', 
                     limits = None, labelsize = 12, ticksize = 10, 
                     output = None, show = False):
    """Plots the day of the water year estimates of precipitation, 
    evapotranspiration, and runoff for the model."""

    # get the start and end year

    start, end = simulated_flow[0][0], simulated_flow[0][-1]

    # figure out the average conditions for each day of the water year

    if tstep == 'daily':
        precipitation  = dayofyear(*precipitation)
        potential_evap = dayofyear(*potential_evap)
        simulated_flow = dayofyear(*simulated_flow)
        simulated_evap = dayofyear(*simulated_evap)
        simulated_gw   = dayofyear(*simulated_gw)
    elif tstep == 'monthly':
        precipitation  = monthofyear(*precipitation)
        potential_evap = monthofyear(*potential_evap)
        simulated_flow = monthofyear(*simulated_flow)
        simulated_evap = monthofyear(*simulated_evap)
        simulated_gw   = monthofyear(*simulated_gw)
    else: 
        print('error, unknown timestep specified')
        raise

    ls     = ['Precipitation:', '\nEvapotranspiration:', 
              '\nRunoff:', '\nAquifer Recharge:']

    series = precipitation, simulated_evap, simulated_flow, simulated_gw

    tots = ['\n{:>4.0f} mm'.format(sum(t)) for t in series]

    # snip the first line break

    tots[0] = tots[0][1:]

    # demand and pet utilization

    tot = (sum(precipitation) - sum(simulated_evap) - 
           sum(simulated_flow) - sum(simulated_gw))
    pet_util = sum(simulated_evap) / sum(potential_evap)

    # day of year times (2004 is a leap year)

    if tstep == 'daily':
        times = [datetime.datetime(2003, 10, 1) + 
                 datetime.timedelta(days = 1) * i 
                 for i in range(366)]
    elif tstep == 'monthly':
        times = ([datetime.datetime(2003, 10 + i, 1) for i in range(3)] +
                 [datetime.datetime(2004, 1 + i,  1) for i in range(9)])

        times = ([t for x in zip(times, times[1:]) for t in x] +
                 [datetime.datetime(2004, 9, 1), datetime.datetime(2004, 10,1)])

        precipitation  = np.array([p for x in zip(precipitation, precipitation) 
                          for p in x])
        simulated_evap = np.array([p for x in zip(simulated_evap,simulated_evap)
                                   for p in x])
        simulated_flow = np.array([p for x in zip(simulated_flow,simulated_flow)
                                   for p in x])
        simulated_gw   = np.array([p for x in zip(simulated_gw, simulated_gw)
                                   for p in x])

    # make the plot

    fig = pyplot.figure(figsize = (8,8))

    sub1 = pyplot.subplot2grid((2,1), (0,0))
    sub2 = pyplot.subplot2grid((2,1), (1,0))

    if   tstep == 'daily':   w = 'Daily'
    elif tstep == 'monthly': w = 'Monthly'

    if (end-start).days > 700: 
        v = HUC8, w, start.year, end.year
        t = '{} Average {} Water Budget, {}-{}'.format(*v)
    else:
        v = HUC8, w, start.year + 1
        t = '{} {} Water Budget, {} Water Year'.format(*v)

    sub1.set_title(t, size = 14)

    # precipitation

    prec_color = 'blue'
    sub1.plot_date(times, precipitation, color = prec_color, lw = 2, 
                  fmt = '-', label = 'precipitation')
    sub2.plot_date(times, precipitation, color = prec_color, lw = 1, 
                  fmt = '-', label = 'precipitation')

    # evaporation

    evap_color = 'green'
    sub1.fill_between(times, 0, simulated_evap, color = evap_color, alpha = 1.)

    # groundwater

    gw = simulated_evap + simulated_gw

    ground_color = 'gray'
    sub1.fill_between(times, simulated_evap, gw, color = ground_color, 
                     alpha = 1.)

    # observed and simulated flow

    demand = gw + simulated_flow

    flow_color = 'red'
    sub1.fill_between(times, gw, demand, color = flow_color, alpha = 1)
    sub2.plot_date(times, demand, color = 'brown', fmt = '-', lw = 1,
                   label = 'total demand')

    sub1.set_ylabel('{} Water Depth\n(mm)'.format(w), 
                   multialignment = 'center', size = labelsize)
    sub2.set_ylabel('{} Water Depth\n(mm)'.format(w), 
                   multialignment = 'center', size = labelsize)

    # add the totals

    t1 = ''.join(ls)
    t2 = ''.join(tots)
    sub1.text(0.01, 0.99, t1, ha = 'left', va = 'top', 
             transform = sub1.transAxes, size = 10)
    sub1.text(0.30, 0.99, t2, ha = 'right', va = 'top', 
              transform = sub1.transAxes, size = 10)
    
    if   tot >= 0: 
        t = ('Soil Moisture Surplus: {:3.0f} mm\n'.format(tot) +
             'Evapotranspiration Utilization: {:2.1%}'.format(pet_util))
    elif tot < 0:  
        t = ('Soil Moisture Deficit: {:3.0f} mm\n'.format(-tot) +
             'Evapotranspiration Utilization: {:2.1%}'.format(pet_util))

    sub2.text(0.01, 0.99, t, ha = 'left', va = 'top', 
             transform = sub2.transAxes, size = 10)

    # add the economics

    sub2.plot_date(times, demand, fmt = '-', color = flow_color, 
                   lw = 1., label = 'total demand')

    # drying and wetting periods

    sub2.fill_between(times, precipitation, demand, 
                      where = precipitation > demand, 
                      color = 'blue', alpha = 0.2)
    sub2.fill_between(times, precipitation, demand, 
                      where = precipitation < demand,
                      color = 'brown', alpha = 0.3)

    # axis limits

    if limits is None:
        mi, ma = sub1.get_ylim()
        sub1.set_ylim([0, ma])
        sub2.set_ylim([0, ma])

    else:
        sub1.set_ylim(limits)
        sub2.set_ylim(limits)

    # various fill attributes

    colors  = ['green', 'gray', 'red', 'blue', 'brown']
    labels  = ['evaporation', 'aquifer recharge', 'runoff', 
               'soil moisture recharge', 'soil moisture loss']
    hatches = [None, None, None, None, None]
    alphas  = [1, 1, 1, 0.5, 0.5]

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[-0.1,  -0.1], [-0.1, -0.01], [-0.01, -0.01], 
                [-0.01, -0.1], [-0.1, -0.1]]

    hs, ls = sub1.get_legend_handles_labels()

    handles = ()
    for l, c, h, a in zip(labels, colors, hatches, alphas):
        handles += (sub2.add_patch(make_patch(dummybox, facecolor=c, hatch = h,
                                             alpha = a, width = 0)),)

    # add a legend

    hs, ls = zip(*chain(zip(hs, ls), zip(handles, labels)))

    leg = sub2.legend(hs, ls, loc = 'upper center', fontsize = ticksize,
                      ncol = math.ceil(len(hs)/2),
                      bbox_to_anchor = (0.5, -0.15))

    # ticks

    for t in sub1.xaxis.get_ticklabels(): t.set_visible(False)

    sub1.yaxis.set_major_locator(ticker.MaxNLocator(8))
    sub2.yaxis.set_major_locator(ticker.MaxNLocator(8))

    sub2.set_xlabel('Water Year')
    sub2.xaxis.set_major_locator(MonthLocator())
    labels = [t.get_text() for t in sub2.get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    sub2.set_xticklabels(labels)

    # finish up
    
    pyplot.tight_layout()

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.subplots_adjust(bottom = 0.15) 
        pyplot.show()

    pyplot.clf()
    pyplot.close()

def plot_climates(htemp, stemp, hdewpoint, sdewpoint, hwind, swind, 
                  hsolar, ssolar, hRET, sRET, start, end,
                  colors = ['green', 'green'],
                  labels = ['historical climate', 'simulated climate'],
                  fmts = ['s', '-'],
                  fill = False,
                  output = 'simulated_climate',
                  verbose = True,
                  show = False,
                  ):
    
    if verbose: print('plotting hourly evapotranspiration\n')

    # figure out the average conditions for each day of the water year

    dates = [start + datetime.timedelta(hours = 1) * i 
             for i in range((end - start).days * 24)]

    hsolar = dayofyear(dates, hsolar)
    htemp  = dayofyear(dates, htemp)

    hETs  = [[24 * p for p in dayofyear(dates, hRET)]]

    ssolar = dayofyear(dates, ssolar)
    stemp  = dayofyear(dates, stemp)

    hETs.append([24 * p for p in dayofyear(dates, sRET)])

    # these are daily values

    dates = [start + datetime.timedelta(days = 1) * i 
             for i in range((end - start).days)]

    hdewpoint = dayofyear(dates, hdewpoint)
    hwind     = dayofyear(dates, hwind)

    sdewpoint = dayofyear(dates, sdewpoint)
    swind     = dayofyear(dates, swind)

    # day of year times (2004 is a leap year)

    times = [datetime.datetime(2003, 10, 1) + datetime.timedelta(days = 1) * i 
             for i in range(366)]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))
    
    subs =  [pyplot.subplot2grid((5,1), (0,0), rowspan = 2)]
    subs += [pyplot.subplot2grid((5,1), (i,0), sharex = subs[0]) 
             for i in range(2,5)]

    subs[0].set_title('Simulated Versus Observed Climate Variables', size = 14)

    # evaporation and potential evapotranspiration

    i = 0

    # total annual evaporation

    if labels is None: labels = ['Penman Equation' for h in hETs]

    tots = ['\n{}: {:4.0f} mm'.format(l, sum(e)) for l, e in zip(labels, hETs)]

    # snip the first break

    tots[0] = tots[0][1:]

    msize = 3
    for hET, c, l, fmt in zip(hETs, colors, labels, fmts):
        
        if fmt == 's':
            subs[i].plot_date(times, hET, markeredgecolor = c, 
                              markerfacecolor = 'None', markersize = msize,
                              fmt = fmt, label = l)
        else:
            subs[i].plot_date(times, hET, color = c, lw = 1., fmt = fmt, 
                              label = l)
        if fill:
            subs[i].fill_between(times, 0, hET, color = c, alpha = 0.3)

    evapfmts = ['s', '+', '*', 'o']

    #for item, fmt in zip(evaporations.items(), evapfmts):
    #    k, v = item
    #    evaporation = dayofyear(dates, v.make_timeseries(start, end))
    #    if len([e for e in evaporation if e is not None]) > 0:
    #        subs[i].plot_date(times, evaporation, fmt = fmt,
    #                          markerfacecolor = 'None', markersize = 3, 
    #                          label = k + ' evaporation')
    #        tot = sum([e for e in evaporation if e is not None])
    #        tots.append('\n{}: {:4.0f} mm'.format(k, tot))

    subs[i].set_ylabel('Evaporation\n(mm)', color = 'green', size = 10, 
                       multialignment = 'center')
    subs[i].legend(fontsize = 8, loc = 'upper left')
    t = ''.join(tots)

    add_tots = False
    if add_tots:
        subs[i].text(0.98, 0.99, t, ha = 'right', va = 'top', 
                     transform = subs[i].transAxes, size = 8)

    subs[i].tick_params(axis = 'y', colors = 'green', size = 9)

    # temperature and dewpoint

    i = 1

    subs[i].plot_date(times, htemp, fmt = 's', markeredgecolor = 'red', 
                      markerfacecolor = 'None', markersize = msize, 
                      markeredgewidth = 0.2, label = 'historical temperature')
    subs[i].plot_date(times, stemp, fmt = '-', color = 'red', lw = 1, 
                      label = 'simulated temperature')
    subs[i].plot_date(times, hdewpoint, fmt = 's', markeredgecolor = 'green', 
                      markerfacecolor = 'None', markersize = msize, 
                      markeredgewidth = 0.2, label = 'historical dewpoint')
    subs[i].plot_date(times, sdewpoint, fmt = '-', color = 'green', lw = 1, 
                      label = 'simulated dewpoint')
    subs[i].set_ylabel('Temperature\n(\u00B0C)', size = 10, color = 'red',
                       multialignment = 'center')
    subs[i].tick_params(axis = 'y', colors = 'red', size = 9)
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))

    subs[i].legend(fontsize = 8, loc = 'lower right')

    # average daily wind speed

    i = 2

    subs[i].plot_date(times, hwind, fmt = 's', markeredgecolor = 'purple', 
                      markerfacecolor = 'None', markersize = msize,
                      label = 'historical wind')
    subs[i].plot_date(times, swind, fmt = '-', color = 'purple', lw = 1, 
                      label = 'simulated wind')
    subs[i].tick_params(axis = 'y', colors = 'purple', size = 9)
    subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', size = 10, 
                       multialignment = 'center')
    subs[i].set_ylim([0, subs[i].get_ylim()[1]])
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(6))
    subs[i].legend(fontsize = 8, loc = 'lower right')

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, hsolar, fmt = 's', markeredgecolor = 'orange', 
                      markerfacecolor = 'None', markersize = msize,
                      label = 'historical solar')
    subs[i].plot_date(times, ssolar, fmt = '-', color = 'orange', lw = 1,
                      label = 'simulated solar')
    subs[i].tick_params(axis = 'y', colors = 'orange', size = 9)
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2)', color = 'orange',
                       multialignment = 'center', size = 10)
    subs[i].set_ylim([0, subs[i].get_ylim()[1]])
    subs[i].legend(fontsize = 8, loc = 'lower right')
    subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))

    # ticks

    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    for s in subs[1:]: 
        for t in s.yaxis.get_ticklabels(): t.set_fontsize(9)
    labels = [t.get_text() for  t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels, size = 9)

    pyplot.tight_layout()
    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()
