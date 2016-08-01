# make_figure5.py

import os, pickle, datetime, numpy

from pyhspf     import Postprocessor
from matplotlib import pyplot, dates
from scipy      import stats

NWISgage  = '05472500'
HUC8      = '07080106'
start     = 1980
end       = 2011
warmup    = 1
run       = False
output    = 'Figure5'

directory = 'C:/HSPF_data'

run_dates     = (datetime.datetime(start, 1, 1),
                 datetime.datetime(end, 1, 1))

process_dates = (datetime.datetime(start + warmup, 1, 1),
                 datetime.datetime(end, 1, 1))

f = '{}/{}/calibration/{}'.format(directory, HUC8, NWISgage)

with open(f, 'rb') as m: hspfmodel = pickle.load(m)

d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
comid = d[NWISgage]

if run:

    # figure out the external targets needed

    targets = ['reach_outvolume', 'evaporation'] 

    # build the input wdmfile

    hspfmodel.build_wdminfile()

    # create the UCI file and the output WDM file

    hspfmodel.build_uci(targets, run_dates[0], run_dates[1], temp = True,
                        snow = True, hydrology = True)

    hspfmodel.messagepath = None

    # run it

    hspfmodel.run(verbose = True)

# add the simulation parameters to the calibrator

postprocessor = Postprocessor(hspfmodel, process_dates, comid = comid)

# make the figure canvas

fig = pyplot.figure(figsize = (15,9))

# font sizes

titlesize = 14
axissize = 12
ticksize = 11

# get the monthly flows

otimes, m_oflow = postprocessor.get_obs_flow(tstep = 'monthly')
stimes, m_sflow = postprocessor.get_sim_flow(comid, tstep = 'monthly')

# plot the monthly simulated versus the observed

ax3 = pyplot.subplot2grid((2,3), (0,1), aspect = 'equal')

ax3.set_title('Monthly Flow Parity Plot', size = titlesize)
ax3.set_xlabel('Observed Monthly Flows (m\u00B3/s)', size = axissize)
ax3.set_ylabel('Simulated Monthly Flows (m\u00B3/s)', size = axissize)

# get the max value for the limits

maxflow = max(max(m_oflow), max(m_sflow))

# add plot for the data

ax3.plot(m_oflow, m_sflow, 's', markeredgecolor = 'blue',
         markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
         label = 'monthly flows')

# add a parity line

ax3.plot([0, maxflow], [0, maxflow], '-', color = 'black',
         label = 'parity line')

# get the linear regression

m, b, r, p, std_err = stats.linregress(m_oflow, m_sflow)

# calculate the Nash-Sutcliffe Efficiency

mNS = (1 - sum((numpy.array(m_sflow) - numpy.array(m_oflow))**2) /
       sum((numpy.array(m_oflow) - numpy.mean(m_oflow))**2))

# add a plot of the linear regression

ax3.plot([0, maxflow], [0, m * maxflow + b], '--', color = 'black',
         label = 'regression')

# add the regression info to the plot

ax3.text(0.98, 0.02,
         'r\u00B2 = {0:.3f}\nNSE = {1:.3f}'.format(r**2, mNS),
         transform = ax3.transAxes,
         ha = 'right', va = 'bottom', color = 'black', size = ticksize)

# add a legend

ax3.legend(loc = 2, fontsize = ticksize, frameon = False)

# get the daily flows

stimes, d_sflow = postprocessor.get_sim_flow(comid, tstep = 'daily')
otimes, d_oflow = postprocessor.get_obs_flow(tstep = 'daily')

# add a plot of the daily flows

ax6 = pyplot.subplot2grid((2,3), (1,0), colspan = 3)

ax6.set_yscale('log')
ax6.set_xlabel('Year', fontsize = axissize)
ax6.set_ylabel('Daily Flow (m\u00B3/s)')
ax6.set_ylim([0.08, 2000])
ax6.xaxis.set_major_formatter(dates.DateFormatter('%Y'))
ax6.xaxis.set_major_locator(dates.YearLocator(5))

ax6.plot(stimes, d_sflow, linestyle = '-', lw = 0.5, c = 'red',
         label = 'simulated')
ax6.plot(otimes, d_oflow, marker = '+', lw = 0, c = 'red',
         markersize = 4, label = 'observed')

ax6.set_title('Hydrograph', fontsize = titlesize)
ax6.legend(loc = 2, fontsize = ticksize, frameon = False)

# get runoff area (km2)

comids = postprocessor.get_upstream_comids(comid)
area   = sum(postprocessor.get_subbasin_areas(comids))

# convert to mm

conv = 1000

# start and end dates

s, e = process_dates

# get the observed flows

t, oflow = postprocessor.get_obs_flow(tstep = 'daily')

tot_obs = (sum([f * 86400 / conv / area for f in oflow]) /
           (e - s).days * 365.25)
        
# simulated runoff

times, vols = postprocessor.get_reach_timeseries('ROVOL', comid,
                                        tstep = 'hourly')

vols = postprocessor.aggregate_hourly_daily(vols)

tot_sim = (sum([v * conv / area for v in vols]) /
           (e - s).days * 365.25)

pct = (tot_sim - tot_obs) / tot_obs

t = ('total observed runoff: {:.0f} mm/yr\n'.format(tot_obs) +
     'total simulated runoff: {:.0f} mm/yr\n'.format(tot_sim) +
     'percent error: {:.1%}'.format(pct))

ax6.text(0.995, 0.02, t, transform = ax6.transAxes,
         ha = 'right', va = 'bottom', color = 'black', size = ticksize)


# plot the daily simulated versus the observed

ax1 = pyplot.subplot2grid((2,3), (0,0), aspect = 'equal')

ax1.set_title('Daily Flow Parity Plot', size = titlesize)
ax1.set_xlabel('Observed Daily Flows (m\u00B3/s)', size = axissize)
ax1.set_ylabel('Simulated Daily Flows (m\u00B3/s)', size = axissize)

# get the min and max value for the limits

minflow = min(min([o for o in d_oflow if o is not None]), 
              min(d_sflow))
maxflow = max(max([o for o in d_oflow if o is not None]), 
              max(d_sflow))

# add plot for the data

ax1.plot(d_oflow, d_sflow, 's', markeredgecolor = 'red',
         markersize = 3, markeredgewidth = 0.5, markerfacecolor = 'None',
         label = 'daily flows')

# add a parity line

ax1.plot([0, maxflow], [0, maxflow], '-', color = 'black',
         label = 'parity line')

# get the linear regression

m, b, r, p, std_err = stats.linregress(d_oflow, d_sflow)

# calculate the Nash-Sutcliffe Efficiency

dNS = (1 - sum((numpy.array(d_sflow) - numpy.array(d_oflow))**2) /
       sum((numpy.array(d_oflow) - numpy.mean(d_oflow))**2))

# add a plot of the linear regression

ax1.plot([0, maxflow], [0, m * maxflow + b], '--', color = 'black',
         label = 'regression')

# add the regression info to the plot

ax1.text(0.98, 0.02,
         'r\u00B2 = {0:.3f}\nNSE = {1:.3f}'.format(r**2, dNS), 
         transform = ax1.transAxes,
         ha = 'right', va = 'bottom', color = 'black', size = ticksize)

# add a legend

ax1.legend(loc = 2, fontsize = ticksize, frameon = False)

# calculate the cdfs for observed and simulated data and transform to z

norm = stats.norm(0,1)

d_oflow.sort()
L = len(d_oflow)
obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]
obs_daily_cdf.reverse()

d_sflow.sort()
L = len(d_sflow)
sim_daily_cdf = [norm.ppf(i / L) for i in range(L)]
sim_daily_cdf.reverse()

# tick marks (had to do this hack style for matplotlib)

ticks = [0.001, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98, 0.999]

norm_ticks = [norm.ppf(t) for t in ticks]

# daily flow duration curve
    
ax5 = pyplot.subplot2grid((2,3), (0,2))

ax5.set_title('Flow Duration Curve', size = titlesize)
ax5.set_yscale('log')
ax5.set_ylabel('Daily Flow (m\u00B3/s)', size = axissize)
ax5.set_xlabel('Probability of Exceedance', size = axissize)
ax5.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
ax5.xaxis.set_ticks(norm_ticks)
ax5.set_xticklabels(ticks)

ax5.plot(obs_daily_cdf, d_oflow,  '-', color = 'blue', lw = 2,
         label = 'observed daily')
ax5.plot(sim_daily_cdf, d_sflow, '--', color = 'red', lw = 2,
         label = 'simulated daily')

ax5.legend(loc = 'upper right', fontsize = ticksize, frameon = False)

# tick settings

for t in (ax1.xaxis.get_ticklabels() + ax1.yaxis.get_ticklabels() +
          ax3.xaxis.get_ticklabels() + ax3.yaxis.get_ticklabels() + 
          ax5.xaxis.get_ticklabels() + ax5.yaxis.get_ticklabels() + 
          ax6.xaxis.get_ticklabels() + ax6.yaxis.get_ticklabels()): 
    t.set_fontsize(ticksize)
    
# save

pyplot.subplots_adjust(hspace = 0.27, wspace = 0.25)

if output is not None: pyplot.savefig(output, dpi = 400)

# show it

#pyplot.show()


