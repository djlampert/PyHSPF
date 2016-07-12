# make_figure3.py
#
# David Lampert
#
# makes figure 3 for the journal publication
#

import os, pickle, datetime, calendar

from matplotlib import pyplot, ticker, dates

from scipy import stats

def average(values):
    """Returns the average values of the list."""

    try: 
        return sum(values) / len(values)
    except:
        return None
        
def dayofyear(dates,
              values,
              ):
    """
    Returns the day of the water year average for the timeseries.
    """

    year = dates[0].year
    while not calendar.isleap(year): year += 1
    delta = datetime.timedelta(days = 1)
    wateryear = [datetime.date(year - 1, 10, 1) + i * delta 
                 for i in range(366)]

    watervalues = [average([v for t, v in zip(dates, values)
                            if t.month == day.month and 
                            t.day == day.day and
                            v is not None])
                   for day in wateryear]

    return watervalues

# start and end dates

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# pan evaporation station data

ghcnd = 'C:/HSPF_new/07080106/climate/GHCND'

stations = {'USC00130200': 'Ames, IA', 
            'USC00134101': 'Iowa City, IA'}

# daily climate time series

p = 'C:/HSPF_new/07080106/climate/daily'

dailyRET = 'C:/HSPF_new/07080106/climate/evapotranspiration/dailyRET'

# name of the plot

output = 'Figure4'

# make a daily time series

times = [start + datetime.timedelta(days = 1) * i 
         for i in range((end-start).days)]

with open('{}/tmax'.format(p), 'rb') as f: 
    s, t, tmax = pickle.load(f)
with open('{}/tmin'.format(p), 'rb') as f: 
    s, t, tmin = pickle.load(f)
with open('{}/dewpoint'.format(p), 'rb') as f: 
    s, t, dewpoint = pickle.load(f)
with open('{}/wind'.format(p), 'rb') as f: 
    s, t, wind = pickle.load(f)
with open('{}/solar'.format(p), 'rb') as f: 
    s, t, solar = pickle.load(f)
    
with open(dailyRET, 'rb') as f: 
    s, t, RET = pickle.load(f)

# convert to day of the year

tmin     = dayofyear(times, tmin)
tmax     = dayofyear(times, tmax)
dewpoint = dayofyear(times, dewpoint)
wind     = dayofyear(times, wind)
solar    = dayofyear(times, solar)
RET      = dayofyear(times, RET)

temp = [0.5 * (mi + ma) for mi, ma in zip(tmin, tmax)]
        
# Watts/m2 to kW hr/m2/d

solar = [s * 0.024 for s in solar]

# day of year times (2004 is a leap year)

wateryear = [datetime.datetime(2003,10,1) + 
             datetime.timedelta(days = 1) * i 
             for i in range(366)]

# make the plot

fig = pyplot.figure(figsize = (8,8))
    
subs =  [pyplot.subplot2grid((6,1), (0,0), rowspan = 3)]
subs += [pyplot.subplot2grid((6,1), (i,0), sharex = subs[0]) 
         for i in (3, 4, 5)]

# evaporation and potential evapotranspiration

tRET = sum([v for v in RET if v is not None])
t = 'Penman-Monteith Model: {:4.0f} mm/yr'.format(tRET)

i = 0

markers = '+', 's'
names = 'Iowa City, IA', 'Ames, IA'

# GHCND evaporation station data

for s, m, n in zip(stations, markers, names):

    p = '{}/{}'.format(ghcnd, s)
    with open(p, 'rb') as f: station = pickle.load(f)

    # get the time series
    
    evaporation = station.make_timeseries('evaporation', start, end)

    # convert to day of the water year
        
    evaporation = dayofyear(times, evaporation)

    # plot April 1 to Oct 31
    
    evaporation = [e if t.month > 3 and t.month < 11 else None
                   for e,t in zip(evaporation, wateryear)]
                   

    subs[i].plot_date(wateryear, evaporation, fmt = m, color = 'green', 
                      markeredgecolor = 'green', markerfacecolor = 'None',
                      markersize = 4, 
                      label = n + ' evaporation')

    sim = [e for e,t in zip(RET,wateryear)
           if t.month > 3 and t.month < 11]
    obs = [e for e,t in zip(evaporation,wateryear)
           if t.month > 3 and t.month < 11]
    slope, intercept, r, p, std_err = stats.linregress(obs, sim)

    its = n, slope, intercept, 'r\u00B2', r**2
    t += '\n{}: y={:.2f}x+{:.2f}; {}={:.2f}'.format(*its)

subs[i].plot_date(wateryear, RET, color = 'green', fmt = '-',
                  label = 'Penman-Monteith')
subs[i].set_ylabel('Evaporation\n(mm)', color = 'green', size = 11)
subs[i].legend(fontsize = 8, loc = 'upper left')

subs[i].text(0.98, 0.99, t, ha = 'right', va = 'top', 
             transform = subs[i].transAxes, size = 8)

for t in subs[i].yaxis.get_ticklabels(): t.set_color('green')

# min, max, and dewpoint temperatures

i = 1

subs[i].plot_date(wateryear, temp, fmt = '-', color = 'red', lw = 0.5, 
                  label = 'Temperature')
subs[i].plot_date(wateryear, dewpoint, fmt = '--', color = 'aqua', 
                  lw = 1, label = 'Dewpoint')
subs[i].set_ylabel('Temperature\n(\u00B0C)', color = 'red', size = 11)

subs[i].legend(fontsize = 8, loc = 'lower right')

for t in subs[i].yaxis.get_ticklabels(): t.set_color('red')

# average daily wind speed

i = 2

subs[i].plot_date(wateryear, wind, fmt = '-', color = 'purple', 
                  lw = 0.5, label = 'wind')
subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', 
                   multialignment = 'center', size = 11)
mi, ma = subs[i].get_ylim()
subs[i].set_ylim((0,ma))

for t in subs[i].yaxis.get_ticklabels(): t.set_color('purple')

# daily solar radiation

i = 3

subs[i].plot_date(wateryear, solar, fmt = '-', color = 'orange', 
                  lw = 0.5, label = 'solar')
subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', 
                   color = 'orange',
                   multialignment = 'center', size = 11)
mi, ma = subs[i].get_ylim()
subs[i].set_ylim((0,ma))

for t in subs[i].yaxis.get_ticklabels(): t.set_color('orange')

# ticks

for s in subs[1:]: s.yaxis.set_major_locator(ticker.MaxNLocator(5))
for sub in subs[:-1]: 
    for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

subs[-1].set_xlabel('Water Year')
subs[-1].xaxis.set_major_locator(dates.MonthLocator())
labels = [t.get_text() for t in subs[-1].get_xticklabels()]
labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
          'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
subs[-1].set_xticklabels(labels, size = 11)

pyplot.tight_layout()
pyplot.subplots_adjust(top = 0.95)

pyplot.savefig(output)

    
