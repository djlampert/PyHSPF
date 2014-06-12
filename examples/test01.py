# example1.py
#
# David Lampert (djlampert@gmail.com)
#
# Tests that HSPF is installed correctly and runs the 1st test simulation from 
# HSPF 12.2, and then plots the results

# Import the hspf library and WDM utility modules

from pyhspf import hspf, WDMUtil

# Check that the hspf library is configured correctly by getting the date

yr, mo, da = hspf.sydatepy()

print('')
print('testing HSPF installation on {:02d}-{:02d}-{:04d}...'.format(mo, da, yr))
print('')

# We will change into the "tests" directory so that HSPF will find the files

import os

os.chdir('data/tests')

# this is the path to the message file in PyHSPF (hspfmsg.wdm)

pyhspfdirectory = os.path.dirname(hspf.__file__)
messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(pyhspfdirectory)

# before running the examples, we have to create the WDM files used by the 
# test runs, which we will do with the WDMUtil class

wdm = WDMUtil(verbose = True)

# the name of the WDM file used in the test UCIs is test.wdm
# open it up for write access

wdm.open('test.wdm', 'w')

# the first few test runs write data to the WDM files, but they assume the 
# datasets already exist so we need to create them. have a look at the test
# UCI files if you are curious

attributes = {'TCODE ': 4, 'TSSTEP': 1, 'TSTYPE': 'WTMP', 'TSFORM': 3}

# what these attributes mean:
#
# the time series type for the first dataset is "WTMP" (water temperature)
# the time code is 4 (the units of the time step for the dataset are days)
# the time step is 1 (1 "unit" of the time step = 1 day)
# the time step form is 3 (the water temperature is a "state" variable 
# that does not depend on time step)
#
# the time step form is perhaps most difficult to understand. different 
# variables have different forms when they are discretized into time steps.
# the flow for a time step could represent the average value across the
# step, the value at the beginning or end, the average value, or the total
# value (this is perhaps most important)--thus for example if I say "the flow
# between 2 and 3 pm was 10 m3 is different than saying the flow at 3 pm was
# 10 m3 per hour. Given that context, HSPF groups all variables into one of
# three categories (the examples reference heat transfer concepts):

# TSFORM = 1 -- The mean value of a state variable (such as temperature)
# TSFORM = 2 -- The total flux across a time step (such as heat flux energy)
# TSFORM = 3 -- The value at the end of the time step (such as temperature)

wdm.create_dataset('test.wdm', 134, attributes)

# change the time series type appropriately

attributes['TSTYPE'] = 'EVAP'

# change the time series form to 2 (evaporation is a "flux" variable so the
# quantity depends on the time step length)

attributes['TSFORM'] = 2

wdm.create_dataset('test.wdm',  41, attributes)

# change the time series form to 1 (wind speed and all the rest of the 
# variables are the average across the day)

attributes['TSFORM'] = 1
attributes['TSTYPE'] = 'WIND'

wdm.create_dataset('test.wdm',  42, attributes)

attributes['TSTYPE'] = 'FLOW'
wdm.create_dataset('test.wdm', 113, attributes)
wdm.create_dataset('test.wdm', 119, attributes)

attributes['TSTYPE'] = 'DEWP'
wdm.create_dataset('test.wdm', 124, attributes)
wdm.create_dataset('test.wdm', 125, attributes)
wdm.create_dataset('test.wdm', 126, attributes)

attributes['TSTYPE'] = 'SEDM'
wdm.create_dataset('test.wdm', 127, attributes)

attributes['TSTYPE'] = 'FLOW'
wdm.create_dataset('test.wdm', 136, attributes)

# then we have to manually close the WDM file

wdm.close('test.wdm')

# Run example1.uci--this simulation just inputs data from TEST01DT.91 to the
# test.wdm file we just made

hspf.hsppy('TEST01.UCI', messagepath)

# let's go back into the test.wdm and pull out the data and graph it (you may 
# get a warning about the file being open--i've worked around it but needs
# a better fix)

wdm.open('test.wdm', 'r')

# get the datasets

wtemps = wdm.get_data('test.wdm', 134)
evaps  = wdm.get_data('test.wdm', 41)
winds  = wdm.get_data('test.wdm', 42)
flow1  = wdm.get_data('test.wdm', 113)
flow2  = wdm.get_data('test.wdm', 119)
dewp1  = wdm.get_data('test.wdm', 124)
dewp2  = wdm.get_data('test.wdm', 125)
dewp3  = wdm.get_data('test.wdm', 126)
sedm   = wdm.get_data('test.wdm', 127)
flow3  = wdm.get_data('test.wdm', 136)

# start and end dates (these are all the same so I will only do this once)

start, end = wdm.get_dates('test.wdm', 134)

times = [start + i * (end - start) / len(wtemps) for i in range(len(wtemps))]

# use matplotlib to make the chart

from matplotlib import pyplot, dates

fig, subs = pyplot.subplots(nrows = 3, ncols = 2, sharex = True,
                            figsize = (12,9))

fig.suptitle('1976 Iowa River Water Quality Data', size = 14)

subs[0,0].plot_date(times, dewp1, 'r-')
subs[0,0].plot_date(times, dewp2, 'g-')
subs[0,0].plot_date(times, dewp3, 'b-')
subs[0,0].set_ylabel('Dew point\n(\u00B0F)')

subs[1,0].plot_date(times, winds, fmt = '-', c = 'purple')
subs[1,0].set_ylabel('Wind Speed\n(miles/day)')

subs[2,0].plot_date(times, evaps, 'g-')
subs[2,0].fill_between(times, 0, evaps, color = 'green', alpha = 0.3)
subs[2,0].set_ylabel('Evaporation\n(in)')
subs[2,0].set_ylim(0, subs[2,0].get_ylim()[1])

subs[0,1].plot_date(times, flow1, fmt = '-', c = 'r')
subs[0,1].plot_date(times, flow2, fmt = '-', c = 'g')
subs[0,1].plot_date(times, flow3, fmt = '-', c = 'b')
subs[0,1].set_ylabel('Stream flow\n(ft\u00B3/s)')

subs[1,1].plot_date(times, wtemps, 'r-')
subs[1,1].set_ylabel('Water Temp\n(\u00B0C)')

subs[2,1].plot_date(times, sedm, fmt = '-', c = 'brown')
subs[2,1].set_ylabel('Suspended Solids\n(mg/L)')

locator   = dates.MonthLocator(interval = 2)
formatter = dates.DateFormatter('%b')

subs[0,0].xaxis.set_major_formatter(formatter)
subs[0,0].xaxis.set_major_locator(locator)
for row in subs:
    for s in row: 
        s.xaxis.set_major_locator(locator)
        s.xaxis.set_major_formatter(formatter)
        s.set_ylim(0, s.get_ylim()[1])

pyplot.tight_layout()
pyplot.subplots_adjust(top=0.94)

pyplot.show()
