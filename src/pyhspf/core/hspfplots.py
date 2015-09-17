# HSPF Model Plot Routines
#
# David J. Lampert, PhD, PE
#
# Last updated: 10/16/2013
#
# Purpose: Lots of routines here to generate images for development of an HSPF
# model. Descriptions below.
#

from matplotlib import pyplot, gridspec, path, patches, ticker, dates
from calendar   import isleap
from scipy      import stats, log10
from itertools  import chain

import numpy as np, os, datetime, math

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

    if datetime.datetime(year, 1, 1) > dates[-1]:
        wateryear = [datetime.date(year - 2, 10, 1) + i * delta 
                     for i in range(365)]
    else:
        wateryear = [datetime.date(year - 1, 10, 1) + i * delta 
                     for i in range(366)]

    watervalues = [average([v for t, v in zip(dates, values)
                            if t.month == day.month and t.day == day.day and
                            v is not None])
                   for day in wateryear]

    return wateryear, watervalues

def monthofyear(dates, values):
    """Returns the month of the water year average for the timeseries."""

    if len(values) > 12:
        watervalues = [average([values[j] for j in range(i, len(values), 12)]) 
                       for i in range(12)]
    else:
        watervalues = values

    months = [d.month for d in dates]

    i = months.index(10)
    watervalues = (watervalues[i:] + watervalues[:i])

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

def plot_hydrograph(HUC8, precipitation, simulated_flow, simulated_evap,
                    potential_evap, observed_flow = None, tstep = 'daily',
                    flow_color = 'red', prec_color = 'blue', 
                    pet_color = 'orange', evap_color = 'green', 
                    titlesize = 16, axsize = 14, ticksize = 12, 
                    units = 'Metric',
                    output = None, show = True, verbose = False):
    """Makes a plot of precipitation and evapotranspiration."""

    if verbose: print('making a plot of the hydrology simulation')

    fig = pyplot.figure(figsize = (12,9))

    ax1 = pyplot.subplot2grid((3,1),(0,0), rowspan = 2)

    if tstep == 'daily':
        title = '%s HSPF Hydrology Simulation, Daily Values\n' % HUC8
        flow_lw = 0.3
        prec_lw = 0.5
        pet_lw  = 1.
        evap_lw = 0.3
        flow_marker = 4
        flow_marker_lw = 0.3
    elif tstep == 'monthly':
        title = '%s HSPF Hydrology Simulation, Monthly Values\n' % HUC8
        flow_lw = 0.6
        prec_lw = 0.6
        pet_lw  = 2.
        evap_lw = 0.6
        flow_marker = 8
        flow_marker_lw = 0.6
    else:
        print('unknown time step specified')
        return

    ax1.set_title(title, fontsize = titlesize)

    ax1.plot_date(x = simulated_flow[0], y = simulated_flow[1], lw = flow_lw,
                  label = 'simulated flow', fmt = '-', color = flow_color)

    if tstep == 'monthly':
        ax1.fill_between(simulated_flow[0], 0, simulated_flow[1], 
                         facecolor = flow_color, alpha = 0.3)


    if observed_flow is not None:
        ax1.plot_date(x = observed_flow[0], y = observed_flow[1], 
                      label = 'observed flow', marker = 's', 
                      markeredgewidth = flow_marker_lw, 
                      markeredgecolor = flow_color, 
                      markerfacecolor = 'None', markersize = flow_marker)

    if   units == 'Metric':  l = 'Flow (m\u00B3/s)'
    elif units == 'English': l = 'Flow (ft\u00B3/s)'

    ax1.set_ylabel(l, color = flow_color, size = axsize)

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([0, 2 * ymax])

    # add the precipitation graph

    ax2 = ax1.twinx()

    ax2.plot_date(x = precipitation[0], y = precipitation[1], lw = prec_lw,
                  label = 'precipitation', fmt = '-', color = prec_color)
    
    # fill in the precipitation if it's monthly

    #if tstep == 'monthly':
    ax2.fill_between(precipitation[0], 0, precipitation[1], 
                         facecolor = prec_color, alpha = 0.3)

    if   units == 'Metric':  l = 'Precipitation (mm)'
    elif units == 'English': l = 'Precipitation (in)'
    ax2.set_ylabel(l, color = prec_color, fontsize = axsize)
    ax2.invert_yaxis()

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([2 * ymin, 0])

    for t in (ax1.yaxis.get_ticklabels() +
              ax2.yaxis.get_ticklabels()): 
        t.set_fontsize(ticksize) 

    # customize the tick marks and gridlines

    ax1.tick_params(axis = 'x', gridOn = 'on')
    ax1.tick_params(axis = 'y', size = ticksize, colors = flow_color, 
                    gridOn = 'on')

    ax2.tick_params(axis = 'y', colors = prec_color)
    for t in (ax1.yaxis.get_ticklines() + ax2.yaxis.get_ticklines() +
              ax1.xaxis.get_ticklabels()): 
        t.set_visible(False) 

    # add the evaporation data

    times, evaps = simulated_evap

    ax3 = pyplot.subplot2grid((3,1),(2,0), sharex = ax1)

    ax3.plot_date(x = times, y = evaps, label = 'simulated evapotranspiration', 
                  fmt = '-', lw = evap_lw, color = evap_color)

    ax3.fill_between(times, 0, evaps, 
                         facecolor = evap_color, alpha = 0.5)

    # add the potential evapotranspiration data

    times, pets = potential_evap
    ax3.plot_date(x = times, y = pets, label = 'potential evapotranspiration', 
                  fmt = '-', lw = pet_lw, color = pet_color)

    ax3.set_xlabel('Time', size = axsize)

    if   units == 'Metric':  l = 'Evaporation (mm)'
    elif units == 'English': l = 'Evaporation (in)'

    ax3.set_ylabel(l, size = axsize, color = evap_color)
    xmin, xmax, ymin, ymax = ax3.axis()
    ax3.set_ylim([0, ymax])

    # set tick size and add gridlines

    ax3.tick_params(axis = 'both', gridOn = 'on')
    ax3.tick_params(axis = 'y', size = ticksize, colors = evap_color)
        
    for t in (ax3.xaxis.get_ticklabels() + ax3.yaxis.get_ticklabels()):
        t.set_fontsize(ticksize) 
    for t in ax3.yaxis.get_ticklines(): t.set_visible(False) 

    # add a legend

    hs, ls = zip(*chain(zip(*ax1.get_legend_handles_labels()), 
                        zip(*ax2.get_legend_handles_labels()), 
                        zip(*ax3.get_legend_handles_labels())))

    leg = ax3.legend(hs, ls, loc = 'upper center', ncol = math.ceil(len(hs)/2),
                     bbox_to_anchor = (0.5, -0.25))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    pyplot.tight_layout()

    # show it

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.subplots_adjust(bottom = 0.22) 
        pyplot.show()

    pyplot.close()

def plot_runoff(HUC8, precipitation, simulated_flow, baseflow, interflow, 
                runoff, inflow = None, observed_flow = None, 
                other_storms = None, summer_storms = None, flow_color = 'red', 
                prec_color = 'blue', in_color = 'blue', base_color = 'gray', 
                int_color = 'green', run_color = 'purple', 
                summer_color = 'yellow', other_color = 'pink', units = 'Metric',
                titlesize = 16, axsize = 14, ticksize = 12, 
                output = None, show = True, verbose = False):
    """Makes a plot of precipitation, runoff, and a log plot of the runoff
    components of baseflow, interflow, and surface runoff."""

    if verbose: print('making a plot of the runoff components')

    # some system specific stuff

    if os.name == 'nt': figsize = (12,9)
    else:               figsize = (10,7.5)

    # make the figure box, with the top figure half the size of the bottom

    fig = pyplot.figure(figsize = figsize)

    ax1 = pyplot.subplot2grid((7,1),(0,0), rowspan = 4)

    ax1.set_title('%s HSPF Runoff Analysis\n' % HUC8, fontsize = titlesize)

    # observed flow

    if observed_flow is not None:
        ax1.plot_date(x = observed_flow[0], y = observed_flow[1], 
                      label = 'observed flow', marker = 's', 
                      markeredgewidth = 0.5, markeredgecolor = flow_color, 
                      markerfacecolor = 'None', markersize = 4)

    # simulated flow at the gage

    times, sflow = simulated_flow
    ax1.plot_date(x = times, y = sflow, lw = 0.5,
                  label = 'simulated flow', fmt = '-', color = flow_color)

    # inflow from upstream

    if inflow is not None:
        ax1.plot_date(x = inflow[0], y = inflow[1], marker = None)
        ax1.fill_between(times, 0, inflow[1], facecolor = in_color, 
                         alpha = 0.5, label = 'upstream inflow')
        fflow = inflow[1]
        bflow = [f + b for f, b in zip(fflow, baseflow)]
        iflow = [f + b + i for f, b, i in zip(fflow, baseflow, interflow)]
        rflow = [f + b + i + r for f, b, i, r in 
                 zip(fflow, baseflow, interflow, runoff)]

    else:
        fflow = [0 for i in range(len(baseflow))]
        bflow = baseflow
        iflow = [i + b for i, b in zip(interflow, baseflow)]
        rflow = [i + b + r for i, b, r in zip(interflow, baseflow, runoff)]

    # baseflow

    ax1.plot_date(x = times, y = bflow, 
                  fmt = '-', lw = 0., color = base_color)
    ax1.fill_between(times, fflow, bflow, facecolor = base_color, 
                     alpha = 0.5, hatch = '//')

    # interflow + baseflow

    ax1.plot_date(x = times, y = iflow, 
                  fmt = '-', lw = 0., color = int_color)
    ax1.fill_between(times, bflow, iflow, facecolor = int_color,
                     alpha = 0.5)

    # interflow + baseflow + surface runoff

    ax1.plot_date(x = times, y = rflow, 
                  fmt = '-', lw = 0., color = run_color)
    ax1.fill_between(times, iflow, rflow, facecolor = run_color, alpha = 0.5)

    # axis labels

    if   units == 'Metric':  l = 'Flow (m\u00B3/s)'
    elif units == 'English': l = 'Flow (ft\u00B3/s)'

    ax1.set_ylabel(l, size = axsize, color = flow_color)

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([ymin, 1.5 * ymax])

    # add the storm dates if supplied

    if summer_storms is not None:
        for d in summer_storms:
            ax1.fill_between(d, ax1.get_ylim()[0], ax1.get_ylim()[1], 
                             facecolor = summer_color, alpha = 0.2)

    if other_storms is not None:
        for d in other_storms:
            ax1.fill_between(d, ax1.get_ylim()[0], ax1.get_ylim()[1], 
                             facecolor = other_color, alpha = 0.2)

    # add the precipitation data

    ax2 = ax1.twinx()

    ax2.plot_date(x = precipitation[0], y = precipitation[1], lw = 0.5,
                  label = 'precipitation', fmt = '-', color = prec_color)

    if   units == 'Metric':  l = 'Precipitation (mm)'
    elif units == 'English': l = 'Precipitation (in)'

    ax2.set_ylabel(l, color = prec_color, fontsize = axsize)
    ax2.invert_yaxis()

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([3 * ymin, ymax])

    # customize the tick labels and gridlines

    ax1.tick_params(axis = 'x', gridOn = 'on')
    ax1.tick_params(axis = 'y', size = ticksize, colors = flow_color,
                    gridOn = 'on')
    ax2.tick_params(axis = 'y', colors = prec_color)

    for t in (ax1.xaxis.get_ticklabels() + ax2.yaxis.get_ticklines() +
              ax1.yaxis.get_ticklines()):
        t.set_visible(False)

    for t in (ax1.yaxis.get_ticklabels() + ax2.yaxis.get_ticklabels()): 
        t.set_fontsize(ticksize) 

    # runoff plot (log scale)

    ax3 = pyplot.subplot2grid((7,1),(4,0), sharex = ax1, rowspan = 3)

    # observed flow

    if observed_flow is not None:
        ax3.plot_date(x = observed_flow[0], y = observed_flow[1], 
                      label = 'observed flow', marker = 's', 
                      markeredgewidth = 0.5, markeredgecolor = flow_color, 
                      markerfacecolor = 'None', markersize = 4)
        min_inflow = 0.5 * min(observed_flow[1])
        max_flow   = 1.2 * max(observed_flow[1])

    else:
        min_inflow = 0.01
        max_flow   = 1000

    # simulated flow at the gage

    ax3.plot_date(x = times, y = sflow, lw = 0.5,
                  label = 'simulated flow', fmt = '-', color = flow_color)

    # inflow from upstream

    i = 100
    if inflow is not None:
        ax3.plot_date(x = inflow[0], y = inflow[1], fmt = '-', lw = 0.)
        min_baseflow = fflow
        ax3.fill_between(times, min_inflow, min_baseflow, facecolor = in_color, 
                         alpha = 0.5)
    else:
        min_baseflow = min_inflow

    ax3.plot_date(x = times, y = bflow, fmt = '-',
                  lw = 0., color = base_color)
    ax3.fill_between(times, min_baseflow, bflow, facecolor = base_color, 
                     alpha = 0.5, hatch = '//')

    # interflow + baseflow

    ax3.plot_date(x = times, y = iflow,
                  fmt = '-', lw = 0., color = int_color)
    ax3.fill_between(times, bflow, iflow, facecolor = int_color,
                     alpha = 0.5)

    # interflow + baseflow + surface runoff

    ax3.plot_date(x = times, y = rflow,
                  fmt = '-', lw = 0., color = run_color)
    ax3.fill_between(times, iflow, rflow, facecolor = run_color, alpha = 0.5)

    # add the storm dates if supplied

    if summer_storms is not None:
        for d in summer_storms:
            ax3.fill_between(d, min_inflow, max_flow, 
                             facecolor = summer_color, alpha = 0.2)

    if other_storms is not None:
        for d in other_storms:
            ax3.fill_between(d, min_inflow, max_flow, 
                             facecolor = other_color, alpha = 0.2)

    # axis settings

    ax3.set_yscale('log')
    ax2.yaxis.set_major_formatter(ticker.ScalarFormatter())
    ax3.set_ylim([min_inflow, max_flow])
    ax3.set_xlabel('Time\n ', size = axsize)

    if   units == 'Metric':  l = 'Flow (m\u00B3/s)'
    elif units == 'English': l = 'Flow (ft\u00B3/s)'

    ax3.set_ylabel(l, size = axsize, color = flow_color)

    # set tick size and add gridlines

    ax3.tick_params(axis = 'both', gridOn = 'on')        
    ax3.tick_params(axis = 'x', size = ticksize, colors = 'black')
    ax3.tick_params(axis = 'y', size = ticksize, colors = flow_color)
    for t in ax3.yaxis.get_ticklines(): t.set_visible(False) 

    # various fill attributes

    colors  = [base_color, int_color, run_color, other_color, 
               summer_color]
    labels  = ['baseflow', 'interflow', 'surface runoff', 
               'non-summer storms', 'summer storms']
    hatches = ['//', None, None, None, None]
    alphas  = [1., 0.5, 0.5, 0.2, 0.2]

    if inflow is not None:

        colors.insert(0, in_color)
        labels.insert(0, 'upstream_inflow')
        hatches.insert(0, None)
        alphas.insert(0, 0.5)

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[0.00001,  0.00001], [0.00001, 0.000011], [0.000011, 0.000011], 
                [0.000011, 0.00001], [0.00001, 0.00001]]

    handles = ()
    for l, c, h, a in zip(labels, colors, hatches, alphas):
        handles += (ax3.add_patch(make_patch(dummybox, facecolor = c, hatch = h,
                                             alpha = a, width = 0)),)

    # add a legend

    hs, ls = zip(*chain(zip(*ax1.get_legend_handles_labels()), 
                        zip(*ax2.get_legend_handles_labels()), 
                        zip(handles, labels)))

    # change the order

    #hs = hs[2], hs[3], hs[1], hs[4], hs[0], hs[5] 
    #ls = ls[2], ls[3], ls[1], ls[4], ls[0], ls[5] 

    leg = ax3.legend(hs, ls, loc = 'upper center', 
                     ncol = math.ceil(len(hs)/2), bbox_to_anchor = (0.5, -0.2))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    pyplot.tight_layout()

    # show it

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.subplots_adjust(bottom = 0.18) 
        pyplot.show()

    pyplot.close()

def plot_storm(HUC8, precipitation, simulated_flow, baseflow, interflow, runoff,
               observed_flow = None, inflow = None, flow_color = 'red', 
               prec_color = 'blue', base_color = 'gray', int_color = 'orange', 
               run_color = 'green', in_color = 'blue',
               titlesize = 16, axsize = 14, ticksize = 12, markersize = 10,
               units = 'Metric', output = None, show = True, verbose = False):
    """Makes a plot of precipitation and the runoff components of baseflow, 
    interflow, and surface runoff for a storm."""

    if verbose: print('plotting storm\n')

    # some system specific stuff

    if os.name == 'nt': figsize = (12,9)
    else:               figsize = (10,7.5)

    # make the figure box, with the top figure half the size of the bottom

    fig = pyplot.figure(figsize = figsize)

    ax1 = pyplot.subplot2grid((2,1),(0,0))

    start_date, end_date = precipitation[0][0], precipitation[0][-1]

    dates = '{:%m-%d-%Y} through {:%m-%d-%Y}'.format(start_date, end_date)
    ax1.set_title('Runoff Analysis for HUC %s, Storm %s\n' % (HUC8, dates), 
                  fontsize = titlesize)

    # add the precipitation data

    ax1.plot_date(x = precipitation[0], y = precipitation[1], lw = 0.5,
                  label = 'precipitation', fmt = '-', color = prec_color)
    ax1.fill_between(precipitation[0], 0, precipitation[1],
                     facecolor = prec_color, hatch = '//', alpha = 0.5)

    if   units == 'Metric':  l = 'Precipitation (mm)'
    elif units == 'English': l = 'Precipitation (in)'
    
    ax1.set_ylabel(l, color = prec_color, fontsize = axsize)
    ax1.invert_yaxis()

    # customize the tick labels and gridlines

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([ymin, 0])

    ax1.tick_params(axis = 'y', colors = prec_color, gridOn = 'on')
    ax1.tick_params(axis = 'x', gridOn = 'on')

    for t in ax1.xaxis.get_ticklabels() + ax1.yaxis.get_ticklines():
        t.set_visible(False)

    for t in ax1.yaxis.get_ticklabels():
        t.set_fontsize(ticksize) 

    # flow plot

    ax2 = pyplot.subplot2grid((2,1),(1,0), sharex = ax1)

    min_flow = 0.

    times, sflows = simulated_flow
    ax2.plot_date(x = times, y = sflows, label = 'simulated streamflow', 
                  fmt = '--', color = 'red', lw = 2)

    if observed_flow is not None:
        ax2.plot_date(x = observed_flow[0], y = observed_flow[1], 
                      label = 'observed flow', marker = 's', 
                      markeredgewidth = 1, markeredgecolor = flow_color, 
                      markerfacecolor = 'None', markersize = markersize)

    ax2leg = zip(*ax2.get_legend_handles_labels())

    # formatting

    ax2.set_xlim([times[0], times[-1]])
    ax2.set_xlabel('Time\n', size = axsize)

    if   units == 'Metric':  l = 'Flow (m\u00B3/s)'
    elif units == 'English': l = 'Flow (ft\u00B3/s)'

    ax2.set_ylabel(l, size = axsize, color = flow_color)

    # set tick size and add gridlines

    ax2.tick_params(axis = 'both', gridOn = 'on')        
    ax2.tick_params(axis = 'x', size = ticksize, colors = 'black')
    ax2.tick_params(axis = 'y', size = ticksize, colors = flow_color)

    for t in ax2.yaxis.get_ticklines(): t.set_visible(False) 

    # runoff plot

    if inflow is not None:

        ax2.plot_date(x = times, y = inflow, label = 'upstream inflow',
                      fmt = '-', lw = 0., color = in_color)
        ax2.fill_between(times, min_flow, inflow, facecolor = in_color,
                         alpha = 0.5)

        bflow = [f + b for f, b in zip(inflow, baseflow)]
        inf   = inflow
        iflow = [f + b + i for f, b, i in zip(inflow, baseflow, interflow)]
        rflow = [f + b + i + r for f, b, i, r in 
                 zip(inflow, baseflow, interflow, runoff)]

    else: 
        inf   = 0
        bflow = baseflow
        iflow = [b + i for b, i in zip(baseflow, interflow)]
        rflow = [i + b + r for i, b, r in zip(interflow, baseflow, runoff)]

    ax2.plot_date(x = times, y = bflow, label = 'baseflow', 
                  fmt = '-', lw = 0., color = base_color)
    ax2.fill_between(times, inf, bflow, facecolor = base_color, 
                     alpha = 0.5, hatch = '//')

    # interflow + baseflow

    ax2.plot_date(x = times, y = iflow, label = 'interflow', 
                  fmt = '-', lw = 0., color = int_color)
    ax2.fill_between(times, bflow, iflow, facecolor = int_color,
                     alpha = 0.5)

    # interflow + baseflow + surface runoff

    ax2.plot_date(x = times, y = rflow, label = 'surface runoff', 
                  fmt = '-', lw = 0.5, color = 'black')
    ax2.fill_between(times, iflow, rflow, facecolor = run_color, alpha = 0.5)

    # various fill attributes

    colors  = [prec_color, base_color, int_color,   run_color]
    labels  = ['precipitation', 'baseflow', 'interflow', 'surface runoff']
    hatches = ['//',                  '//',        None,             None]
    alphas  = [0.5,                     1.,         0.5,              0.5]

    if inflow is not None:

        colors.insert(0, in_color)
        labels.insert(0, 'upstream_inflow')
        hatches.insert(0, None)
        alphas.insert(0, 0.5)

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[-0.1,  -0.1], [-0.1, -0.01], [-0.01, -0.01], 
                [-0.01, -0.1], [-0.1, -0.1]]

    handles = ()
    for l, c, h, a in zip(labels, colors, hatches, alphas):
        handles += (ax2.add_patch(make_patch(dummybox, facecolor = c, hatch = h,
                                             alpha = a, width = 0)),)

    # add a legend

    hs, ls = zip(*chain(ax2leg, zip(handles, labels)))

    leg = ax2.legend(hs, ls, loc = 'upper center', 
                     ncol = math.ceil(len(hs)/2), bbox_to_anchor = (0.5, -0.1))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    pyplot.tight_layout()

    # show it

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.subplots_adjust(bottom = 0.15) 
        pyplot.show()

    pyplot.close()

def plot_calibration(HUC8, simulated_daily, observed_daily, simulated_monthly,
                     observed_monthly, titlesize = 14, axissize = 12,
                     ticksize = 11, output = None, show = True, 
                     verbose = False):
    """
    Makes a figure with subplots for the daily simulated versus observed,
    monthly simulated versus observed, and the daily flow duration curve.
    """

    if verbose: 
        print('plotting the calibration statistics for the simulation\n')

    # make the figure canvas

    figsize = (15,10)

    fig = pyplot.figure(figsize = figsize)

    # plot the daily simulated versus the observed

    ax1 = pyplot.subplot2grid((2,3), (0,0), aspect = 'equal')

    ax1.set_title('Daily Flow Analysis\n', size = titlesize)
    ax1.set_xlabel('Observed Daily Flows (m\u00B3/s)', size = axissize)
    ax1.set_ylabel('Simulated Daily Flows (m\u00B3/s)', size = axissize)

    # get the min and max value for the limits

    minflow = min(min([o for o in observed_daily if o is not None]), 
                  min(simulated_daily))
    maxflow = max(max([o for o in observed_daily if o is not None]), 
                  max(simulated_daily))

    # add plot for the data

    ax1.plot(observed_daily, simulated_daily, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')

    # add a parity line

    ax1.plot([0, maxflow], [0, maxflow], '-', color = 'black',
             label = 'parity line')

    # get the linear regression

    m, b, r, p, std_err = stats.linregress(observed_daily, simulated_daily)

    # calculate the Nash-Sutcliffe Efficiency

    dNS = (1 - sum((np.array(simulated_daily) - np.array(observed_daily))**2) /
           sum((np.array(observed_daily) - np.mean(observed_daily))**2))

    # add a plot of the linear regression

    ax1.plot([0, maxflow], [0, m * maxflow + b], '--', color = 'black',
             label = 'regression')

    # add the regression info to the plot

    ax1.text(0.98, 0.02,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, dNS), 
             transform = ax1.transAxes,
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax1.legend(loc = 2, fontsize = ticksize)

    # make a log plot the daily simulated versus the observed

    ax2 = pyplot.subplot2grid((2,3), (1,0), aspect = 'equal')

    ax2.set_xscale('log')
    ax2.set_yscale('log')

    ax2.set_xlabel('Observed Daily Flows (m\u00B3/s)', size = axissize)
    ax2.set_ylabel('Simulated Daily Flows (m\u00B3/s)', size = axissize)

    # get the max value for the limits

    minflow = min(min(observed_daily), min(simulated_daily))
    maxflow = max(max(observed_daily), max(simulated_daily))

    # add plot for the data

    ax2.plot(observed_daily, simulated_daily, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')

    # add a parity line

    ax2.plot([minflow, maxflow], [minflow, maxflow], '-', color = 'black',
             label = 'parity line')

    # get the linear regression of the log data

    log_obs = [log10(f) for f in observed_daily]
    log_sim = [log10(f) for f in simulated_daily]

    m, b, r, p, std_err = stats.linregress(log_obs, log_sim)

    # calculate the Nash-Sutcliffe Efficiency

    logdNS = (1 - sum((np.array(log_sim) - np.array(log_obs))**2) /
              sum((np.array(log_obs) - np.mean(log_obs))**2))

    # add a plot of the linear regression of the log flows

    ax2.plot([minflow, maxflow], 
             [10**(m * log10(minflow)), 10**(m * log10(maxflow) + b)], 
             '--', color = 'black', label = 'regression')

    # add the regression info to the plot

    ax2.text(0.98, 0.02, 
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, logdNS),
             transform = ax2.transAxes,
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax2.legend(loc = 2, fontsize = ticksize)

    # plot the monthly simulated versus the observed

    ax3 = pyplot.subplot2grid((2,3), (0,1), aspect = 'equal')

    ax3.set_title('Monthly Flow Analysis\n', size = titlesize)
    ax3.set_xlabel('Observed Monthly Flows (m\u00B3/s)', size = axissize)
    ax3.set_ylabel('Simulated Monthly Flows (m\u00B3/s)', size = axissize)

    # get the max value for the limits

    maxflow = max(max(observed_monthly), max(simulated_monthly))

    # add plot for the data

    ax3.plot(observed_monthly, simulated_monthly, 's', markeredgecolor = 'blue',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'monthly data')

    # add a parity line

    ax3.plot([0, maxflow], [0, maxflow], '-', color = 'black',
             label = 'parity line')

    # get the linear regression

    m, b, r, p, std_err = stats.linregress(observed_monthly, simulated_monthly)

    # calculate the Nash-Sutcliffe Efficiency

    mNS = (1 - sum((np.array(simulated_monthly) - 
                    np.array(observed_monthly))**2) /
           sum((np.array(observed_monthly) - 
                np.mean(observed_monthly))**2))

    # add a plot of the linear regression

    ax3.plot([0, maxflow], [0, m * maxflow + b], '--', color = 'black',
             label = 'regression')

    # add the regression info to the plot

    ax3.text(0.98, 0.02,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, mNS),
             transform = ax3.transAxes,
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax3.legend(loc = 2, fontsize = ticksize)

    # make a log plot the daily simulated versus the observed

    ax4 = pyplot.subplot2grid((2,3), (1,1), aspect = 'equal')

    ax4.set_xscale('log')
    ax4.set_yscale('log')

    ax4.set_xlabel('Observed Monthly Flows (m\u00B3/s)', size = axissize)
    ax4.set_ylabel('Simulated Monthly Flows (m\u00B3/s)', size = axissize)

    # get the max value for the limits

    minflow = min(min(observed_monthly), min(simulated_monthly))
    maxflow = max(max(observed_monthly), max(simulated_monthly))

    # add plot for the data

    ax4.plot(observed_monthly, simulated_monthly, 's', markeredgecolor = 'blue',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'monthly data')

    # add a parity line

    ax4.plot([minflow, maxflow], [minflow, maxflow], '-', color = 'black',
             label = 'parity line')

    # get the linear regression of the log data

    log_obs = [log10(f) for f in observed_monthly]
    log_sim = [log10(f) for f in simulated_monthly]

    m, b, r, p, std_err = stats.linregress(log_obs, log_sim)

    # calculate the Nash-Sutcliffe Efficiency

    logmNS = (1 - sum((np.array(log_sim) - np.array(log_obs))**2) /
              sum((np.array(log_obs) - np.mean(log_obs))**2))

    # add a plot of the linear regression of the log flows

    ax4.plot([minflow, maxflow], 
             [10**(m * log10(minflow)), 10**(m * log10(maxflow) + b)], 
             '--', color = 'black', label = 'regression')

    # add the regression info to the plot

    ax4.text(0.98, 0.02,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, logmNS),
             transform = ax4.transAxes,
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax4.legend(loc = 2, fontsize = ticksize)

    # calculate the cdfs for observed and simulated data and transform to z

    norm = stats.norm(0,1)

    observed_daily.sort()
    L = len(observed_daily)
    obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    obs_daily_cdf.reverse()

    simulated_daily.sort()
    L = len(simulated_daily)
    sim_daily_cdf = [norm.ppf(i / L) for i in range(L)]
    sim_daily_cdf.reverse()

    observed_monthly.sort()
    L = len(observed_monthly) 
    obs_monthly_cdf = [norm.ppf(i / L) for i in range(L)]
    obs_monthly_cdf.reverse()

    simulated_monthly.sort()
    L = len(simulated_monthly)
    sim_monthly_cdf = [norm.ppf(i / L) for i in range(L)]
    sim_monthly_cdf.reverse()

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.001, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98, 0.999]

    norm_ticks = [norm.ppf(t) for t in ticks]

    # daily flow duration curve
    
    ax5 = pyplot.subplot2grid((2,3), (0,2))

    ax5.set_title('Flow Duration Curves\n', size = titlesize)
    ax5.set_yscale('log')
    ax5.set_ylabel('Daily Flow (m\u00B3/s)', size = axissize)
    ax5.set_xlabel('Probability of Exceedance', size = axissize)
    ax5.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
    ax5.xaxis.set_ticks(norm_ticks)
    ax5.set_xticklabels(ticks)

    ax5.plot(obs_daily_cdf, observed_daily,  '-', color = 'blue',
             label = 'observed daily')
    ax5.plot(sim_daily_cdf, simulated_daily, '-', color = 'red',
             label = 'simulated daily')

    ax5.legend(loc = 'upper right', fontsize = ticksize)

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98]

    norm_ticks = [norm.ppf(t) for t in ticks]

    # monthly flow duration curve

    ax6 = pyplot.subplot2grid((2,3), (1,2))

    ax6.set_yscale('log')
    ax6.set_ylabel('Monthly Flow (m\u00B3/s)', size = axissize)
    ax6.set_xlabel('Probability of Exceedance', size = axissize)
    ax6.set_xlim([norm.ppf(0.005), norm.ppf(0.995)])
    ax6.xaxis.set_ticks(norm_ticks)
    ax6.set_xticklabels(ticks)

    ax6.plot(obs_monthly_cdf, observed_monthly,  '-', color = 'blue',
             label = 'observed monthly')
    ax6.plot(sim_monthly_cdf, simulated_monthly, '-', color = 'red',
             label = 'simulated monthly')

    ax6.legend(loc = 'upper right', fontsize = ticksize)

    # tick settings

    for t in (ax1.xaxis.get_ticklabels() + ax1.yaxis.get_ticklabels() +
              ax2.xaxis.get_ticklabels() + ax2.yaxis.get_ticklabels() + 
              ax3.xaxis.get_ticklabels() + ax3.yaxis.get_ticklabels() + 
              ax4.xaxis.get_ticklabels() + ax4.yaxis.get_ticklabels() + 
              ax5.xaxis.get_ticklabels() + ax5.yaxis.get_ticklabels() + 
              ax6.xaxis.get_ticklabels() + ax6.yaxis.get_ticklabels()): 
        t.set_fontsize(ticksize)

    # tighten the borders

    pyplot.tight_layout()

    # save if needed

    if output is not None: pyplot.savefig(output, dpi = 400)

    # show it

    if show: pyplot.show()

    pyplot.close()

def plot_dayofyear(HUC8, precipitation, potentialET, simulated_flow, 
                   simulated_evap, observed_flow = None, prec_bars = True,
                   flow_bars = True, evap_bars = True, lw = 1, title = None,
                   labelsize = 12, ticksize = 10, output = None, show = False):
    """Plots the day of the water year estimates of precipitation, 
    evapotranspiration, and runoff for the model."""

    # get the start and end year

    start, end = simulated_flow[0][0], simulated_flow[0][-1]

    # get the standard deviation of the precipitation, evapotranspiration,flows

    flow_range = dayofyear_range(*simulated_flow)
    prec_range = dayofyear_range(*precipitation)
    evap_range = dayofyear_range(*simulated_evap)

    # figure out the average conditions for each day of the water year

    times, precipitation  = dayofyear(*precipitation)
    times, potentialET    = dayofyear(*potentialET)
    times, simulated_flow = dayofyear(*simulated_flow)
    times, simulated_evap = dayofyear(*simulated_evap)

    ls     = ['Precipitation:', '\nPotential Evapotranspiration:', 
              '\nSimulated Evapotranspiration:', '\nSimulated Runoff:']
    series = precipitation, potentialET, simulated_evap, simulated_flow
            
    tots = ['\n{:<4.0f} mm'.format(sum(t)) for t in series]

    if observed_flow is not None:

        times, observed_flow = dayofyear(*observed_flow)
        ls.append('\nObserved Runoff')
        tots.append('\n{:<4.0f} mm'.format(sum(observed_flow)))

    # snip the first line break

    tots[0] = tots[0][1:]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))

    subs = [pyplot.subplot2grid((8,1), (0,0), rowspan = 2)]

    subs += [pyplot.subplot2grid((8,1), (2,0), rowspan = 3, sharex = subs[0]),
             pyplot.subplot2grid((8,1), (5,0), rowspan = 2, sharex = subs[0])
             ]

    if title is None:
        v = HUC8, start.year, end.year
        subs[0].set_title('{} Average Daily Hydrograph {}-{}'.format(*v), 
                          size = 14)
    else: subs[0].set_title(title)

    # precipitation

    prec_color = 'blue'
    subs[0].invert_yaxis()
    subs[0].plot_date(times, precipitation, color = prec_color, lw = lw, 
                      fmt = '-', label = 'precipitation')
    subs[0].set_ylabel('Precipitation\n(mm)', color = prec_color, 
                       size = labelsize, multialignment = 'center')

    # add an error band

    if prec_bars:
        minprec = [s if e is None else s - e
                   for s, e in zip(precipitation, prec_range)]
        maxprec = [s if e is None else s + e 
                   for s, e in zip(precipitation, prec_range)]

        subs[0].fill_between(times, minprec, maxprec, color = prec_color, 
                             alpha = 0.2)

    # observed and simulated flow

    flow_color = 'red'
    subs[1].plot_date(times, simulated_flow, fmt = '-', color = flow_color, 
                      lw = lw, label = 'simulated flow')

    if observed_flow is not None:
        subs[1].plot_date(times, observed_flow, fmt = 's', markersize = 5,
                          markeredgecolor = flow_color, 
                          markerfacecolor = 'None', label = 'observed flow')

    # add an error band

    if flow_bars:
        minflow = [s if e is None else s - e
                   for s, e in zip(simulated_flow, flow_range)]
        maxflow = [s if e is None else s + e 
                   for s, e in zip(simulated_flow, flow_range)]

        subs[1].fill_between(times, minflow, maxflow, color = flow_color, 
                             alpha = 0.2)

    subs[1].set_ylabel('Runoff\n(mm)', color = 'red', multialignment = 'center',
                       size = labelsize,)

    # add the totals

    t1 = ''.join(ls)
    t2 = ''.join(tots)
    subs[1].text(0.01, 0.97, t1, ha = 'left', va = 'top', 
                 transform = subs[1].transAxes, size = 8)
    subs[1].text(0.30, 0.97, t2, ha = 'left', va = 'top', 
                 transform = subs[1].transAxes, size = 8)

    # evaporation

    evap_color = 'green'
    subs[2].plot_date(times, simulated_evap, fmt = '-', color = evap_color,
                      lw = lw, label = 'simulated evapotranspiration')
    subs[2].plot_date(times, potentialET, fmt = '-', color = 'orange', lw = lw,
                      label = 'potential evapotranspiration')
    subs[2].set_ylabel('Evapotranspiration\n(mm)', color = evap_color,
                       multialignment = 'center', size = labelsize)

    # add an error band

    if evap_bars:
        minevap = [s if e is None else s - e
                   for s, e in zip(simulated_evap, evap_range)]
        maxevap = [s if e is None else s + e 
                   for s, e in zip(simulated_evap, evap_range)]

        subs[2].fill_between(times, minevap, maxevap, color = evap_color, 
                             alpha = 0.2)

    # axis limits

    mi, ma = subs[0].get_ylim()
    subs[0].set_ylim([mi, 0])
    mi, ma = subs[1].get_ylim()
    subs[1].set_ylim([0, ma])
    mi, ma = subs[2].get_ylim()
    subs[2].set_ylim([0, ma])

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
    subs[-1].xaxis.set_major_locator(dates.MonthLocator())
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels)

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[0.00001,   -0.00001], 
                [0.00001,  -0.000011], 
                [0.000011, -0.000011], 
                [0.000011, -0.00001], 
                [0.00001,  -0.00001],
                ]

    if prec_bars:
        subs[0].add_patch(make_patch(dummybox, facecolor = prec_color, 
                                     alpha = 0.2, label = '95% CI band'))

    if flow_bars:
        subs[0].add_patch(make_patch(dummybox, facecolor = flow_color, 
                                     alpha = 0.2, label = '95% CI band'))

    if evap_bars:
        subs[0].add_patch(make_patch(dummybox, facecolor = evap_color, 
                                     alpha = 0.2, label = '95% CI band'))

    hs, ls = zip(*chain(zip(*subs[0].get_legend_handles_labels()),
                        zip(*subs[1].get_legend_handles_labels()),
                        zip(*subs[2].get_legend_handles_labels())
                        )
                  )

    leg = subs[2].legend(hs, ls, loc = 'upper center', fontsize = ticksize,
                         ncol = math.ceil(len(hs)/2),
                         bbox_to_anchor = (0.5, -0.36))


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
    sub2.xaxis.set_major_locator(dates.MonthLocator())
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

def plot_snow(HUC8, precipitation, temperature, simulated_snow, simulated_fall,
              dewpoint = None, observed_snow = None, observed_fall = None,
              titlesize = 16, axsize = 14, ticksize = 12,
              output = None, show = True, verbose = False):
    """Makes a plot of precipitation, temperature, and simulated snow depth."""

    if verbose: print('making a plot of the snow simulation')

    # make the figure frame with bottom subplot half the size of the top

    fig = pyplot.figure(figsize = (12,9))

    ax1 = pyplot.subplot2grid((5,1),(0,0), rowspan = 2)
    
    ax1.set_title('%s HSPF Snow Simulation\n' % HUC8, size = titlesize)
    ax1.plot_date(x = simulated_fall[0], y = simulated_fall[1], lw = 0.5,
                  color = 'orange', label = 'simulated snowfall', fmt = '-')

    avg_rain = sum(precipitation[1]) / (precipitation[0][-1] -
                                        precipitation[0][0]).days * 365.25
    avg_fall = sum(simulated_fall[1]) / (simulated_fall[0][-1] -
                                         simulated_fall[0][0]).days * 365.25

    textbox = ('     Average Precipitation: {:4.0f} mm/yr\n'.format(avg_rain) +
               'Average Simulated Snowfall: {:4.0f} mm/yr'.format(avg_fall))

    # set the y limits to half (to see both snowfall and precip)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([0, 1.25 * ymax])    
    ax1.set_ylabel('Snow Fall (mm)', color = 'orange', size = axsize)

    ax1.tick_params(axis = 'x', gridOn = 'on')
    ax1.tick_params(axis = 'y', size = ticksize, colors = 'orange', 
                    gridOn = 'on')

    if observed_fall is not None:

        times, fall = observed_fall
        avg = sum(fall) / (times[-1] - times[0]).days * 365.25
        textbox += '\n Average Observed Snowfall: {:4.0f} mm/yr'.format(avg)
        snowfall = [v if v >= 1 else None for v in fall]
        ax1.plot_date(x = times, y = snowfall, label = 'observed snow fall',
                      fmt = 's', markeredgecolor = 'orange', 
                      markerfacecolor = 'None', markersize = 4)

    ax1.text(0.99, 0.6, textbox,  transform = ax1.transAxes,
             ha = 'right', va = 'center', color = 'black', size = 9)

    # add precipitation

    ax2 = ax1.twinx()

    ax2.plot_date(x = precipitation[0], y = precipitation[1], lw = 0.5,
                  label = 'precipitation', fmt = 'b-')
    ax2.set_ylabel('Precipitation (mm)', color = 'b', size = axsize)
    ax2.invert_yaxis()
    ax2.tick_params(axis = 'y', colors = 'blue')

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([3 * ymin, ymax])

    # add the simulated snow

    ax3 = pyplot.subplot2grid((5,1),(2,0), rowspan = 2, sharex = ax1)

    ax3.plot_date(x = simulated_snow[0], y = simulated_snow[1], 
                  label = 'simulated snow depth', fmt = '-', color = 'gray')
    ax3.fill_between(simulated_snow[0], 0, simulated_snow[1], 
                     facecolor = 'gray', alpha = .5)
    ax3.set_ylabel('Snow Depth (mm)', size = axsize, color = 'gray')

    # add observed snow

    if observed_snow is not None:

        times, snow = observed_snow
        snowdata = [v if v >= 1 else None for v in snow]
        ax3.plot_date(x = times, y = snowdata, label = 'observed snow depth',
                      fmt = 's', markeredgecolor = 'gray', 
                      markerfacecolor = 'None', markersize = 4)

    # settings

    ax3.tick_params(axis = 'x', gridOn = 'on')
    ax3.tick_params(axis = 'y', size = ticksize, colors = 'gray', 
                    gridOn = 'on')

    # add a graph of the temperature

    times, temperatures = temperature

    ax4 = pyplot.subplot2grid((5,1),(4,0), sharex = ax1)

    ax4.plot_date(x = times, y = temperatures, label = 'temperature', 
                  fmt = '-', color = 'red')

    if dewpoint is not None:

        times, dewpoints = dewpoint

        ax4.plot_date(x = times, y = dewpoints, label = 'dewpoint',
                      fmt = '-', color = 'cyan')

    ax4.set_xlabel('Time', size = axsize)
    ax4.yaxis.set_major_locator(ticker.MaxNLocator(5))
    ax4.set_ylabel('Temperature (\u00B0C)', size = axsize, color = 'red')

    # set tick size and add gridlines

    ax4.tick_params(axis = 'both', gridOn = 'on')
    ax4.tick_params(axis = 'y', size = ticksize, colors = 'red')
        
    for t in (  ax1.yaxis.get_ticklabels()
              + ax2.yaxis.get_ticklabels()
              + ax3.yaxis.get_ticklabels()
              + ax4.xaxis.get_ticklabels() 
              + ax4.yaxis.get_ticklabels()
              ):
        t.set_fontsize(ticksize) 

    for t in (ax3.xaxis.get_ticklabels() 
              + ax3.yaxis.get_ticklines()
              + ax4.yaxis.get_ticklines()
              ): 
        t.set_visible(False) 

    # customize the tick marks and gridlines

    for t in (ax1.yaxis.get_ticklabels() + ax2.yaxis.get_ticklabels()):
        t.set_fontsize(ticksize) 
    for t in (ax1.yaxis.get_ticklines() + ax2.yaxis.get_ticklines() +
              ax1.xaxis.get_ticklabels()): 
        t.set_visible(False) 

    # add a legend

    hs, ls = zip(*chain(zip(*ax1.get_legend_handles_labels()), 
                        zip(*ax2.get_legend_handles_labels()), 
                        zip(*ax3.get_legend_handles_labels()), 
                        zip(*ax4.get_legend_handles_labels()),
                        )
                  )

    leg = ax4.legend(hs, ls, loc = 'upper center', ncol = math.ceil(len(hs)/2),
                     bbox_to_anchor = (0.5, -0.36))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    pyplot.tight_layout()

    # show it

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.subplots_adjust(bottom = 0.22) 
        pyplot.show()

    pyplot.close()

def plot_snowcalibration(HUC8, sim_depth, obs_depth, sim_fall, obs_fall, 
                         titlesize = 14, axissize = 12, ticksize = 11, 
                         output = None, show = True, verbose = False):
    """Makes a figure with subplots for the daily simulated versus observed,
    monthly simulated versus observed, and the daily flow duration curve."""

    if verbose: 
        print('plotting the calibration statistics for the simulation\n')

    # make the figure canvas

    figsize = (15,10)
    #figsize = (15,5)

    fig = pyplot.figure(figsize = figsize)

    # plot the daily simulated versus the observed

    ax1 = pyplot.subplot2grid((2,3), (0,0), aspect = 'equal')

    ax1.set_title('Snow Analysis (Linear)\n', size = titlesize)
    ax1.set_xlabel('Observed Daily Snowdepths (mm)', size = axissize)
    ax1.set_ylabel('Simulated Daily Snowdepths (mm)', size = axissize)

    # get the max value for the limits

    mindepth = min([o for o in obs_depth])
    maxdepth = max(max([o for o in obs_depth]), max(sim_depth))

    # add plot for the data

    ax1.plot(obs_depth, sim_depth, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')

    # add a parity line

    ax1.plot([0, maxdepth], [0, maxdepth], '-', color = 'black',
             label = 'parity line')

    # get the linear regression

    m, b, r, p, std_err = stats.linregress(obs_depth, sim_depth)

    # calculate the Nash-Sutcliffe Efficiency

    dNS = (1 - sum((np.array(sim_depth) - np.array(obs_depth))**2) /
           sum((np.array(obs_depth) - np.mean(obs_depth))**2))

    # add a plot of the linear regression

    ax1.plot([0, maxdepth], [0, m * maxdepth + b], '--', color = 'black',
             label = 'regression')

    # add the regression info to the plot

    ax1.text(0.98 * maxdepth, 2 * mindepth,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, dNS), 
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax1.legend(loc = 2, fontsize = ticksize)

    # make a log plot the daily simulated versus the observed

    ax2 = pyplot.subplot2grid((2,3), (0,1), aspect = 'equal')

    ax2.set_xscale('log')
    ax2.set_yscale('log')
    ax2.set_xlim(0.5 * mindepth, 2 * maxdepth)
    ax2.set_ylim(0.5 * mindepth, 2 * maxdepth)
    ax2.set_title('Snow Analysis (log)\n', size = titlesize)
    ax2.set_xlabel('Observed Snowdepths (mm)', size = axissize)
    ax2.set_ylabel('Simulated Snowdepths (mm)', size = axissize)

    # add plot for the data

    obs = [o for o, s in zip(obs_depth, sim_depth) 
           if s > 0.5 * mindepth]
    sim = [s for s in sim_depth if s > 0.5 * mindepth]

    ax2.plot(obs_depth, sim_depth, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')

    # add a parity line

    ax2.plot([mindepth, maxdepth], [mindepth, maxdepth], '-', color = 'black',
             label = 'parity line')

    # get the linear regression of the log data

    log_obs = [log10(o) for o, s in zip(obs_depth, sim_depth) 
               if s > 0.5 * mindepth]
    log_sim = [log10(s) for s in sim_depth if s > 0.5 * mindepth]

    m, b, r, p, std_err = stats.linregress(log_obs, log_sim)

    # calculate the Nash-Sutcliffe Efficiency

    logdNS = (1 - sum((np.array(log_sim) - np.array(log_obs))**2) /
              sum((np.array(log_obs) - np.mean(log_obs))**2))

    # add a plot of the linear regression of the log flows

    ax2.plot([mindepth, maxdepth], 
             [10**(m * log10(mindepth)), 10**(m * log10(maxdepth) + b)], 
             '--', color = 'black', label = 'regression')

    # add the regression info to the plot

    ax2.text(maxdepth, 1.2 * mindepth,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, logdNS),
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax2.legend(loc = 2, fontsize = ticksize)

    # calculate the cdfs for observed and simulated data and transform to z

    norm = stats.norm(0,1)

    obs_depth.sort()
    L = len(obs_depth)
    obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]

    sim_depth.sort()
    L = len(sim_depth)
    sim_daily_cdf = [norm.ppf(i / L) for i in range(L)]

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.001, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98, 0.999]

    norm_ticks = [norm.ppf(t) for t in ticks]

    # cumulative distribution curve
    
    ax3 = pyplot.subplot2grid((2,3), (0,2))

    ax3.set_title('Cumulative Snow Distributions\n', size = titlesize)
    ax3.set_ylabel('Snowdepth (mm)', size = axissize)
    ax3.set_xlabel('Probability of Exceedance', size = axissize)
    ax3.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
    ax3.xaxis.set_ticks(norm_ticks)
    ax3.set_xticklabels(ticks)

    ax3.plot(obs_daily_cdf, obs_depth,  '-', color = 'blue',
             label = 'observed')
    ax3.plot(sim_daily_cdf, sim_depth, '-', color = 'red',
             label = 'simulated')

    ax3.legend(loc = 2, fontsize = ticksize)

    # plot the daily simulated versus the observed

    ax4 = pyplot.subplot2grid((2,3), (1,0), aspect = 'equal')

    ax4.set_xlabel('Observed Daily Snowfall (mm)', size = axissize)
    ax4.set_ylabel('Simulated Daily Snowfall (mm)', size = axissize)

    # get the max value for the limits

    minfall = min([o for o in obs_fall])
    maxfall = max(max([o for o in obs_fall]), max(sim_fall))

    # add plot for the data

    ax4.plot(obs_fall, sim_fall, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')

    # add a parity line

    ax4.plot([0, maxfall], [0, maxfall], '-', color = 'black',
             label = 'parity line')

    # get the linear regression

    m, b, r, p, std_err = stats.linregress(obs_fall, sim_fall)

    # calculate the Nash-Sutcliffe Efficiency

    dNS = (1 - sum((np.array(sim_fall) - np.array(obs_fall))**2) /
           sum((np.array(obs_fall) - np.mean(obs_fall))**2))

    # add a plot of the linear regression

    ax4.plot([0, maxfall], [0, m * maxfall + b], '--', color = 'black',
             label = 'regression')

    # add the regression info to the plot

    ax4.text(0.98 * maxfall, 2 * minfall,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, dNS), 
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    # add a legend

    ax4.legend(loc = 2, fontsize = ticksize)

    # make a log plot the daily simulated versus the observed

    ax5 = pyplot.subplot2grid((2,3), (1,1), aspect = 'equal')

    ax5.set_xscale('log')
    ax5.set_yscale('log')
    ax5.set_xlim(0.5 * minfall, 2 * maxfall)
    ax5.set_ylim(0.5 * minfall, 2 * maxfall)
    ax5.set_xlabel('Observed Snowfall (mm)', size = axissize)
    ax5.set_ylabel('Simulated Snowfall (mm)', size = axissize)

    # add plot for the data

    obs = [o for o, s in zip(obs_fall, sim_fall) 
           if s > 0.5 * minfall]
    sim = [s for s in sim_fall if s > 0.5 * minfall]

    ax5.plot(obs_fall, sim_fall, 's', markeredgecolor = 'red',
             markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
             label = 'daily data')

    # add a parity line

    ax5.plot([minfall, maxfall], [minfall, maxfall], '-', color = 'black',
             label = 'parity line')

    # get the linear regression of the log data

    log_obs = [log10(o) for o, s in zip(obs_fall, sim_fall) 
               if s > 0.5 * minfall]
    log_sim = [log10(s) for s in sim_fall if s > 0.5 * minfall]

    m, b, r, p, std_err = stats.linregress(log_obs, log_sim)

    # calculate the Nash-Sutcliffe Efficiency

    logdNS = (1 - sum((np.array(log_sim) - np.array(log_obs))**2) /
              sum((np.array(log_obs) - np.mean(log_obs))**2))

    # add a plot of the linear regression of the log flows

    ax5.plot([minfall, maxfall], 
             [10**(m * log10(minfall)), 10**(m * log10(maxfall) + b)], 
             '--', color = 'black', label = 'regression')

    # add the regression info to the plot

    ax5.text(maxfall, 1.2 * minfall,
             'r\u00B2 = {0:.3f}\nNS = {1:.3f}'.format(r**2, logdNS),
             ha = 'right', va = 'bottom', color = 'black', size = ticksize)

    ax5.legend(loc = 2, fontsize = ticksize)

    # calculate the cdfs for observed and simulated data and transform to z

    norm = stats.norm(0,1)

    obs_fall.sort()
    L = len(obs_fall)
    obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]

    sim_fall.sort()
    L = len(sim_fall)
    sim_daily_cdf = [norm.ppf(i / L) for i in range(L)]

    # tick marks (had to do this hack style for matplotlib)

    ticks = [0.001, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98, 0.999]

    norm_ticks = [norm.ppf(t) for t in ticks]

    # cumulative distribution curve
    
    ax6 = pyplot.subplot2grid((2,3), (1,2))

    ax6.set_ylabel('Snowfall (mm)', size = axissize)
    ax6.set_xlabel('Probability of Exceedance', size = axissize)
    ax6.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
    ax6.xaxis.set_ticks(norm_ticks)
    ax6.set_xticklabels(ticks)

    ax6.plot(obs_daily_cdf, obs_fall,  '-', color = 'blue',
             label = 'observed')
    ax6.plot(sim_daily_cdf, sim_fall, '-', color = 'red',
             label = 'simulated')

    ax6.legend(loc = 2, fontsize = ticksize)

    # tick settings

    for t in (ax1.xaxis.get_ticklabels() + ax1.yaxis.get_ticklabels()
              #ax2.xaxis.get_ticklabels() + ax2.yaxis.get_ticklabels()
              #ax3.xaxis.get_ticklabels() + ax3.yaxis.get_ticklabels() 
              + ax4.xaxis.get_ticklabels() + ax4.yaxis.get_ticklabels() 
              + ax3.xaxis.get_ticklabels() + ax3.yaxis.get_ticklabels()
              #+ ax6.xaxis.get_ticklabels() + ax3.yaxis.get_ticklabels()
              + ax6.xaxis.get_ticklabels() + ax6.yaxis.get_ticklabels()
              ): 
        t.set_fontsize(ticksize)

    ax2.xaxis.set_major_formatter(ticker.ScalarFormatter())
    ax5.xaxis.set_major_formatter(ticker.ScalarFormatter())
    ax2.yaxis.set_major_formatter(ticker.ScalarFormatter())
    ax5.yaxis.set_major_formatter(ticker.ScalarFormatter())

    for t in (ax2.xaxis.get_ticklabels() + ax2.yaxis.get_ticklabels()
              + ax5.xaxis.get_ticklabels() + ax5.yaxis.get_ticklabels()
              ): 
        t.set_fontsize(ticksize)

    # tighten the borders

    pyplot.tight_layout()

    # save if needed

    if output is not None: pyplot.savefig(output, dpi = 400)

    # show it

    if show: pyplot.show()

    pyplot.close()

def plot_tss(HUC8, precipitation, observed_flow, simulated_flow,
             simulated_tss, observed_tss = None, output = None, 
             title = None, show = False, verbose = False):
    """Generates a plot of a hydrograph including precipitation and compares
    it with simulation results."""

    if verbose: print('making a plot of the output')

    fig = pyplot.figure(figsize = (9,7.5))

    ax1  = pyplot.subplot2grid((7,1),(0,0), rowspan = 3)

    if title is None:
        ax1.set_title('{} HSPF Sediment Transport\n'.format(HUC8))
    else: ax1.set_title(title + '\n')

    ax1.plot_date(x = simulated_flow[0], y = simulated_flow[1], lw = 0.3,
                  label = 'simulated flow', fmt = 'r-')
    ax1.plot_date(x = observed_flow[0], y = observed_flow[1], 
                  label = 'observed flow', marker = 's', markeredgewidth = 0.3,
                  markeredgecolor = 'red', markerfacecolor = 'None', 
                  markersize = 3)

    ax1.set_ylabel('Flow (m\u00B3/s)', color = 'r')

    # set tick size and add gridlines

    ax1.tick_params(axis = 'both', gridOn = 'on')
    ax1.tick_params(axis = 'y', size = 9, colors = 'red')
    for t in ax1.yaxis.get_ticklines(): t.set_visible(False) 

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([ymin, 1.4 * ymax])

    hs, ls = ax1.get_legend_handles_labels()

    ax2 = ax1.twinx()

    ax2.plot_date(x = precipitation[0], y = precipitation[1], lw = 0.5,
                  label = 'precipitation', fmt = 'b-')
    ax2.set_ylabel('Precipitation (mm)', color = 'b')
    ax2.invert_yaxis()

    # customize the tick marks

    ax2.tick_params(axis = 'y', colors = 'blue')

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([3 * ymin, ymax])

    for t in (ax1.xaxis.get_ticklabels() + 
              ax1.yaxis.get_ticklabels() +
              ax2.yaxis.get_ticklabels()): 
        t.set_fontsize(10) 

    for t in (ax1.xaxis.get_ticklabels() + 
              ax2.yaxis.get_ticklines()):
        t.set_visible(False)

    # add a legend

    h, l = ax2.get_legend_handles_labels()

    hs = hs + h
    ls = ls + l

    # add a graph of the tss

    ax4 = pyplot.subplot2grid((7,1),(3,0), rowspan = 3, sharex = ax1)

    times, concs = simulated_tss
    ax4.plot_date(x = times, y = concs, label = 'simulated TSS', 
                  fmt = '-', color = 'brown', lw = 0.5)

    # add tss if available

    if observed_tss is not None:
        times, concs = observed_tss
        ax4.plot_date(x = times, y = concs, label = 'observed TSS', 
                      marker = 's', markeredgewidth = '.6', 
                      markeredgecolor = 'brown', markerfacecolor = 'None', 
                      markersize = 3)
        ax4.set_ylim([0, 1.1 * max(concs)])

    ax4.set_xlabel('Time\n')
    ax4.set_ylabel('Solids Concentration (mg/L)', multialignment = 'center', 
                   size = 11, color = 'brown')

    # set tick size and add gridlines

    ax4.tick_params(axis = 'both', gridOn = 'on')
    ax4.tick_params(axis = 'y', size = 9, colors = 'brown')
        
    for t in (ax4.xaxis.get_ticklabels() + ax4.yaxis.get_ticklabels()):
        t.set_fontsize(10) 
    for t in ax4.yaxis.get_ticklines(): t.set_visible(False) 

    h, l = ax4.get_legend_handles_labels()

    hs = hs + h
    ls = ls + l

    leg = ax4.legend(hs, ls, loc = 'upper center', ncol = math.ceil(len(hs) /2),
                     bbox_to_anchor = (0.5, -0.2))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = 9)

    # show it

    if output is not None: pyplot.savefig(output)
    if show:               pyplot.show()
    pyplot.close()

def plot_solids(HUC8, precipitation, observed_flow, simulated_flow, sand, silt, 
                clay, shear, observed_tss = None, inflow = None, taus = None, 
                output = None, title = None, verbose = False, ticksize = 9,
                units = 'Metric', show = False):
    """Generates a plot of a hydrograph including precipitation and compares
    it with simulation results."""

    if verbose: print('making a plot of the output')

    fig = pyplot.figure(figsize = (9,7.5))

    ax1 = pyplot.subplot2grid((7,1),(0,0), rowspan = 2)

    if title is None:
        ax1.set_title('{} HSPF Sediment Transport\n'.format(HUC8))
    else: ax1.set_title(title + '\n')

    ax1.plot_date(x = simulated_flow[0], y = simulated_flow[1], lw = 0.3,
                  label = 'simulated flow', fmt = 'r-')
    ax1.plot_date(x = observed_flow[0], y = observed_flow[1], 
                  label = 'observed flow', marker = 's', markeredgewidth = 0.3,
                  markeredgecolor = 'red', markerfacecolor = 'None', 
                  markersize = 3)

    ax1.set_ylabel('Flow (m\u00B3/s)', color = 'r')

    # set tick size and add gridlines

    ax1.tick_params(axis = 'both', gridOn = 'on')
    ax1.tick_params(axis = 'y', size = 9, colors = 'red')
    for t in ax1.yaxis.get_ticklines(): t.set_visible(False) 

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([ymin, 1.4 * ymax])

    hs, ls = ax1.get_legend_handles_labels()

    ax2 = ax1.twinx()

    ax2.plot_date(x = precipitation[0], y = precipitation[1], lw = 0.5,
                  label = 'precipitation', fmt = 'b-')
    ax2.set_ylabel('Precipitation (mm)', color = 'b')
    ax2.invert_yaxis()

    # customize the tick marks

    ax2.tick_params(axis = 'y', colors = 'blue')

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([3 * ymin, ymax])

    for t in (ax1.xaxis.get_ticklabels() + 
              ax1.yaxis.get_ticklabels() +
              ax2.yaxis.get_ticklabels()): 
        t.set_fontsize(10) 

    for t in (ax1.xaxis.get_ticklabels() + 
              ax2.yaxis.get_ticklines()):
        t.set_visible(False)

    # add a legend

    h, l = ax2.get_legend_handles_labels()

    hs = hs + h
    ls = ls + l

    # shear

    if   units == 'Metric':  u = 'kg/m\u00B2'
    elif units == 'English': u = 'lb/ft\u00B2'

    ax3 = pyplot.subplot2grid((7,1),(2,0), rowspan = 2, sharex = ax1)

    ax3.plot_date(x = shear[0], y = shear[1], fmt = '-', color = 'purple',
                  lw = .7, label = 'shear stress')

    ax3.set_ylabel('Shear Stress\n({})'.format(u), multialignment = 'center', 
                   size = 11, color = 'purple')

    # add the critical shear stresses to the figure to show when scour and 
    # deposition take place

    if taus is not None:

        taumin, taumax = taus

        ax3.fill_between(shear[0], taumax, shear[1], 
                         where = shear[1] > taumax, 
                         color = 'pink', alpha = 1.0)
        ax3.fill_between(shear[0], taumin, shear[1],
                         where = shear[1] < taumin,
                         color = 'green', alpha = 0.2)

    # set tick size and add gridlines

    ax3.yaxis.set_major_locator(ticker.MaxNLocator(4))
    ax3.tick_params(axis = 'both', gridOn = 'on')
    ax3.tick_params(axis = 'y', size = 9, colors = 'purple')
        
    for t in (ax3.xaxis.get_ticklabels() + 
              ax3.yaxis.get_ticklabels()
              ):
        t.set_fontsize(10) 
    for t in (ax3.yaxis.get_ticklines() +
              ax3.xaxis.get_ticklabels()
              ): 
        t.set_visible(False) 

    # legend

    h, l = ax3.get_legend_handles_labels()

    hs = hs + h
    ls = ls + l

    # add a graph of the tss showing the solids components

    times, c1 = sand

    c2 = [sa + si for sa, si in zip(sand[1], silt[1])]
    c3 = [sa + si + cl for sa, si, cl in zip(sand[1], silt[1], clay[1])]

    ax4 = pyplot.subplot2grid((7,1), (4,0), rowspan = 2, sharex = ax1)

    ax4.fill_between(times, 0,  c1, facecolor = 'yellow', alpha = 0.7)
    ax4.fill_between(times, c1, c2, facecolor = 'black',  alpha = 0.3)
    ax4.fill_between(times, c2, c3, facecolor = 'brown',  alpha = 0.3)

    # add tss if available

    if observed_tss is not None:
        times, concs = observed_tss
        ax4.plot_date(x = times, y = concs, label = 'observed TSS', 
                      marker = 's', markeredgewidth = '.3', 
                      markeredgecolor = 'brown', markerfacecolor = 'None', 
                      markersize = 3)
        ax4.set_ylim([0, 1.1 * max(concs)])

    ax4.set_xlabel('Time')
    ax4.set_ylabel('Solids Concentration (mg/L)', multialignment = 'center', 
                   size = 11, color = 'brown')

    # set tick size and add gridlines

    ax4.tick_params(axis = 'both', gridOn = 'on')
    ax4.tick_params(axis = 'y', size = 9, colors = 'brown')
        
    for t in (ax4.xaxis.get_ticklabels() + 
              ax4.yaxis.get_ticklabels()
              ):
        t.set_fontsize(10) 
    for t in (ax4.yaxis.get_ticklines()# +
              #ax4.xaxis.get_ticklabels()
              ): 
        t.set_visible(False) 

    # legend

    h, l = ax4.get_legend_handles_labels()

    hs = hs + h
    ls = ls + l

    # various fill attributes

    colors  = ['yellow', 'black', 'brown', 'pink', 'green']
    labels  = ['sand', 'silt', 'clay', 'scour', 'deposition']
    hatches = [None, None, None, None, None]
    alphas  = [0.7, 0.3, 0.3, 1, 0.2]

    if inflow is not None:

        colors.insert(0, 'red')
        labels.insert(0, 'upstream_inflow')
        hatches.insert(0, None)
        alphas.insert(0, 0.5)

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[-0.1,  -0.1], [-0.1, -0.01], [-0.01, -0.01], 
                [-0.01, -0.1], [-0.1, -0.1]]

    handles = ()
    for l, c, h, a in zip(labels, colors, hatches, alphas):
        handles += (ax4.add_patch(make_patch(dummybox, facecolor = c, hatch = h,
                                             alpha = a, width = 0)),)

    # add a legend

    hs, ls = zip(*chain(zip(hs, ls), zip(handles, labels)))

    leg = ax4.legend(hs, ls, loc = 'upper center', 
                     ncol = math.ceil(len(hs)/2), bbox_to_anchor = (0.5, -0.2))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    # show it

    if output is not None: pyplot.savefig(output)
    if show: pyplot.show()
    pyplot.close()

def plot_erosion(HUC8, times, precipitation, storage, runoff, detach, erosion, 
                 cumulative = False, prec_color = 'blue', 
                 storage_color = 'green', run_color = 'purple', 
                 erosion_color = 'brown', detach_color = 'yellow', 
                 units = 'Metric', titlesize = 14, axsize = 11, ticksize = 10, 
                 output = None, show = True, verbose = False):
    """Makes a plot of precipitation, runoff, and a log plot of the runoff
    components of baseflow, interflow, and surface runoff."""

    if verbose: print('making a plot of the erosion components')

    # some system specific stuff

    #if os.name == 'nt': figsize = (12,9)
    #else:               figsize = (10,7.5)
    figsize = (10,7.5)

    # make the figure box, with the top figure half the size of the bottom

    fig = pyplot.figure(figsize = figsize)

    ax1 = pyplot.subplot2grid((6,1),(0,0), rowspan = 2)

    ax1.set_title('%s HSPF Land Surface Erosion Analysis\n' % HUC8, 
                  fontsize = titlesize)

    # runoff plots

    surface = [s + r for s, r in zip(storage, runoff)]

    ax1.plot_date(x = times, y = storage, fmt = '-', lw = 0., 
                  color = storage_color)
    ax1.fill_between(times, 0, storage, facecolor = storage_color, 
                     alpha = 0.5)  
    ax1.plot_date(x = times, y = surface, fmt = '-', lw = 0., color = run_color)
    ax1.fill_between(times, storage, surface, facecolor = run_color, 
                     alpha = 0.5)

    # axis labels

    if   units == 'Metric':  l = 'Surface Runoff\nand Storage (mm)'
    elif units == 'English': l = 'Surface Runoff\nand Storage (in)'

    ax1.set_ylabel(l, size = axsize, color = run_color, 
                   multialignment = 'center')

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([0, 1.5 * ymax])

    # add the precipitation data

    ax2 = ax1.twinx()

    ax2.plot_date(x = times, y = precipitation, lw = 0.5,
                  fmt = '-', color = prec_color)
    ax2.fill_between(times, 0, precipitation, facecolor = prec_color, 
                     alpha = 0.5)

    if   units == 'Metric':  l = 'Precipitation (mm)'
    elif units == 'English': l = 'Precipitation (in)'

    ax2.set_ylabel(l, color = prec_color, fontsize = axsize)
    ax2.invert_yaxis()

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([3 * ymin, 0])

    # customize the tick labels and gridlines

    ax1.tick_params(axis = 'x', gridOn = 'on')
    ax1.tick_params(axis = 'y', size = ticksize, gridOn = 'on', 
                    colors = run_color)
    ax2.tick_params(axis = 'y', colors = prec_color)

    for t in (ax1.xaxis.get_ticklabels() + 
              ax2.yaxis.get_ticklines() +
              ax1.yaxis.get_ticklines()):
        t.set_visible(False)

    for t in (ax1.yaxis.get_ticklabels() + ax2.yaxis.get_ticklabels()): 
        t.set_fontsize(ticksize) 

    # erosion plot

    if   units == 'Metric':  
        l = 'Erosion\n(metric tons/ha)'
    elif units == 'English': 
        l = 'Erosion\n(tons/acre)'

    if cumulative: l = 'Cumulative ' + l

    ax3 = pyplot.subplot2grid((6,1),(4,0), sharex = ax1, rowspan = 2)

    ax3.plot_date(x = times, y = erosion, fmt = '-', lw = 0.5, 
                  color = erosion_color)
    ax3.fill_between(times, 0, erosion, facecolor = erosion_color, alpha = 0.8,
                     hatch = '+')

    # axis settings

    #ax3.set_ylim([min_inflow, max_flow])
    ax3.set_xlabel('Time\n ', size = axsize)

    ax3.set_ylabel(l, size = axsize, color = erosion_color,
                   multialignment = 'center')

    xmin, xmax, ymin, ymax = ax3.axis()
    ax3.set_ylim([0, ymax])

    # set tick size and add gridlines

    ax3.tick_params(axis = 'both', gridOn = 'on')        
    ax3.tick_params(axis = 'x', size = ticksize, colors = 'black')
    ax3.tick_params(axis = 'y', size = ticksize, colors = erosion_color)

    for t in (ax3.yaxis.get_ticklines() +
              ax3.xaxis.get_ticklines() 
              #ax3.xaxis.get_ticklabels()
              ): 
        t.set_visible(False) 

    ax4 = pyplot.subplot2grid((6,1), (2,0), sharex = ax1, rowspan = 2)

    ax4.plot_date(x = times, y = detach, fmt = '-', lw = 0.5, 
                  color = 'black')
    ax4.fill_between(times, 0, detach, facecolor = detach_color, alpha = 0.8,
                     hatch = 'o')

    xmin, xmax, ymin, ymax = ax4.axis()
    ax4.set_ylim([0, ymax])

    ax4.tick_params(axis = 'both', gridOn = 'on')        
    ax4.tick_params(axis = 'x', size = ticksize, colors = 'black')
    ax4.tick_params(axis = 'y', size = ticksize, colors = 'black')

   # ax4.set_xlabel('Time\n ', size = axsize)
    ax4.set_ylabel('Detached Storage\n(tonnes/ha)', size = axsize,
                   multialignment = 'center', color = 'black')

    for t in (ax4.yaxis.get_ticklines() +
              ax4.xaxis.get_ticklines() +
              ax4.xaxis.get_ticklabels()
              ): 
        t.set_visible(False) 

    # various fill attributes

    colors  = [prec_color, erosion_color, run_color, storage_color, 
               detach_color]
    labels  = ['precipitation', 'erosion', 'surface runoff', 'surface storage',
               'detached sediment']
    hatches = [None, '+', None, None, 'o']
    alphas  = [0.5, 0.8, 0.5, 0.5, 0.8]

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[0.00001,  0.00001], [0.00001, 0.000011], [0.000011, 0.000011], 
                [0.000011, 0.00001], [0.00001, 0.00001]]

    handles = ()
    for l, c, h, a in zip(labels, colors, hatches, alphas):
        handles += (ax3.add_patch(make_patch(dummybox, facecolor = c, hatch = h,
                                             alpha = a, width = 0)),)

    # add a legend

    hs, ls = zip(*chain(zip(*ax1.get_legend_handles_labels()), 
                        zip(*ax2.get_legend_handles_labels()), 
                        zip(handles, labels)))

    # change the order

    #hs = hs[2], hs[3], hs[1], hs[4], hs[0], hs[5] 
    #ls = ls[2], ls[3], ls[1], ls[4], ls[0], ls[5] 

    leg = ax3.legend(hs, ls, loc = 'upper center', 
                     ncol = math.ceil(len(hs)/2), bbox_to_anchor = (0.5, -0.2))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    #pyplot.tight_layout()

    # show it

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.subplots_adjust(bottom = 0.18) 
        pyplot.show()

    pyplot.close()

def plot_sediment_loading(HUC8, times, precipitation, simulated_flows, 
                          simulated_tss, simulated_loads,
                          observed_flows = None, observed_tss = None, 
                          observed_loads = None, cumulative = False, 
                          prec_color = 'blue', flow_color = 'red', 
                          tss_color = 'brown', load_color = 'black', 
                          units = 'Metric', titlesize = 14, axsize = 11, 
                          ticksize = 10, markersize = 3,
                          output = None, show = True, verbose = False):
    """Makes a plot of precipitation, runoff, and a log plot of the runoff
    components of baseflow, interflow, and surface runoff."""

    if verbose: print('making a plot of the erosion components')

    # some system specific stuff

    figsize = (10, 7.5)
    #if os.name == 'nt': figsize = (12,9)
    #else:               figsize = (10,7.5)

    # make the figure box, with the top figure half the size of the bottom

    fig = pyplot.figure(figsize = figsize)

    ax1 = pyplot.subplot2grid((7,1),(0,0), rowspan = 2)

    ax1.set_title('%s Sediment Loading Analysis\n' % HUC8, 
                  fontsize = titlesize)

    # runoff plots

    ax1.plot_date(x = times, y = simulated_flows, fmt = '-', lw = 0.5, 
                  color = flow_color, label = 'simulated flow')

    if observed_flows is not None:
        ax1.plot_date(x = observed_flows[0], y = observed_flows[1], 
                      label = 'observed flow', marker = 's', 
                      markeredgewidth = 1, markeredgecolor = flow_color, 
                      markerfacecolor = 'None', markersize = markersize)

    # axis labels

    if   units == 'Metric':  l = 'Flow (m\u00B3/s)'
    elif units == 'English': l = 'Flow (ft\u00B3/s)'

    ax1.set_ylabel(l, size = axsize, color = flow_color, 
                   multialignment = 'center')

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax1.axis()
    ax1.set_ylim([0, 1.5 * ymax])

    # add the precipitation data

    ax2 = ax1.twinx()

    ax2.plot_date(x = times, y = precipitation, lw = 0.5,
                  fmt = '-', color = prec_color, label = 'precipitation')
    ax2.fill_between(times, 0, precipitation, facecolor = prec_color, 
                     alpha = 0.5)

    if   units == 'Metric':  l = 'Precipitation (mm)'
    elif units == 'English': l = 'Precipitation (in)'

    ax2.set_ylabel(l, color = prec_color, fontsize = axsize)
    ax2.invert_yaxis()

    # set the y limits to half (to see both precip and flow)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax2.set_ylim([3 * ymin, 0])

    # add a graph of the tss

    ax4 = pyplot.subplot2grid((7,1),(2,0), rowspan = 2, sharex = ax1)

    ax4.plot_date(x = times, y = simulated_tss, label = 'simulated TSS', 
                  fmt = '-', color = tss_color, lw = 0.5)

    # add tss if available

    if observed_tss is not None:
        ts, concs = observed_tss
        ax4.plot_date(x = ts, y = concs, label = 'observed TSS', 
                      marker = 's', markeredgewidth = '.6', 
                      markeredgecolor = tss_color, markerfacecolor = 'None', 
                      markersize = 3)
        ax4.set_ylim([0, 1.1 * max(concs)])

    ax4.set_ylabel('Solids Concentration\n(mg/L)', multialignment = 'center', 
                   size = axsize, color = tss_color)

    # load plot

    if   units == 'Metric':  
        if cumulative: l = 'Cumulative Sediment\nLoad (tonnes)'
        else:          l = 'Sediment Load\n(tonnes)'
    elif units == 'English': 
        if cumulative: l = 'Cumulative Sediment\nLoad (tons)'
        else:          l = 'Sediment Load\n(tons)'

    ax3 = pyplot.subplot2grid((7,1),(4,0), sharex = ax1, rowspan = 2)

    ax3.plot_date(x = times, y = simulated_loads, fmt = '-', lw = 0.5, 
                  color = load_color)
    ax3.fill_between(times, 0, simulated_loads, facecolor = load_color, 
                     alpha = 0.3)#, hatch = '+')

    # axis settings

    #ax3.set_ylim([min_inflow, max_flow])
    ax3.set_xlabel('Time\n ', size = axsize)

    ax3.set_ylabel(l, size = axsize, color = load_color,
                   multialignment = 'center')

    xmin, xmax, ymin, ymax = ax3.axis()
    ax3.set_ylim([0, ymax])

    # set tick size and add gridlines

    ax3.tick_params(axis = 'both', gridOn = 'on')        
    ax4.tick_params(axis = 'both', gridOn = 'on')        
    ax3.tick_params(axis = 'x', size = ticksize)
    ax3.tick_params(axis = 'y', size = ticksize, colors = load_color)

    # customize the tick labels and gridlines

    ax1.tick_params(axis = 'x', gridOn = 'on')
    ax1.tick_params(axis = 'y', gridOn = 'on', colors = flow_color)
    ax2.tick_params(axis = 'y', colors = prec_color)
    ax4.tick_params(axis = 'y', colors = tss_color)

    for t in (ax1.xaxis.get_ticklabels() + 
              ax4.xaxis.get_ticklabels() + 
              ax2.yaxis.get_ticklines() +
              ax1.yaxis.get_ticklines()):
        t.set_visible(False)

    for t in (ax1.yaxis.get_ticklabels() + 
              ax2.yaxis.get_ticklabels() +
              ax3.yaxis.get_ticklabels() +
              ax4.yaxis.get_ticklabels() +
              ax3.xaxis.get_ticklabels()
              ): 
        t.set_fontsize(ticksize) 


    if observed_loads is not None:
        ax3.plot_date(x = observed_loads[0], y = observed_loads[1], 
                      label = 'observed loading', fmt = '-',
                      lw = 2., color = load_color)

        
    # various fill attributes

    colors = [load_color]
    labels = ['simulated_loading']
    hatches = [None]
    alphas = [0.5]

    # hack way to append the legend using a dummy box to make patches

    dummybox = [[0.00001,  0.00001], [0.00001, 0.000011], [0.000011, 0.000011], 
                [0.000011, 0.00001], [0.00001, 0.00001]]

    handles = ()
    for l, c, h, a in zip(labels, colors, hatches, alphas):
        handles += (ax3.add_patch(make_patch(dummybox, facecolor = c, hatch = h,
                                             alpha = a, width = 0)),)

    # add a legend

    hs, ls = zip(*chain(zip(*ax1.get_legend_handles_labels()), 
                        zip(*ax2.get_legend_handles_labels()), 
                        zip(*ax3.get_legend_handles_labels()), 
                        zip(*ax4.get_legend_handles_labels()), 
                        zip(handles, labels)
                        ))

    # change the order

    #hs = hs[2], hs[3], hs[1], hs[4], hs[0], hs[5] 
    #ls = ls[2], ls[3], ls[1], ls[4], ls[0], ls[5] 

    leg = ax3.legend(hs, ls, loc = 'upper center', 
                     ncol = math.ceil(len(hs)/2), bbox_to_anchor = (0.5, -0.2))
    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = ticksize)

    #pyplot.tight_layout()

    # show it

    if output is not None: 
        pyplot.savefig(output, bbox_extra_artists=(leg,), bbox_inches = 'tight')

    if show:
        pyplot.show()

    pyplot.close()

