# intro02.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 09/20/2014
#
# Purpose: This example mimicks the example from the HSPF "Expert" system 
# (hspexp) for the Hunting Creek Watershed in the Patuxent River Basin. 
# This is a more "real" example of how to create an HSPF model using pyhspf. 
# Assumes the user has some familiarity with Python, hydrologic modeling, 
# and has done the first example.

# description of simulation

description = 'Hunting Creek, Patuxent River Basin'

# need to use dates and times, so use datetime module

import datetime

# start and end dates for simulation (from the input data files)

start = datetime.datetime(1988, 10, 1)
end   = datetime.datetime(1990, 10, 1)

# this example uses a daily time step (pyhspf needs time step in minutes)

tstep = 60 * 24

# import the Watershed and Subbasin classes to use to build the model

from pyhspf import Watershed, Subbasin

# make a subbasin (the hspexp daily example file has only 1 subbasin)
# need to assign a name to the subbasin, which (unfortunately) has to be
# a string variable of length of at most 15

sname = 'patuxent'

subbasin = Subbasin(sname)

# overland flow plane info

length     = 300   # ft
planeslope = 0.38  # flow plane slope
elev       = 0     # not used for hydrology
centroid   = [0,0] # not sued for hydrology

# add the flow plane info for the subbasin

subbasin.add_flowplane(length, planeslope, centroid, elev)

# subbasin reach info; as above this isn't all directly needed by HSPF but is 
# needed to build the watershed

name     = 'HUNTING CR'
maxelev  = 125        # inflow elevation, ft
minelev  = 100        # outflow elevlation, to give delth of 25 feet
slopelen = 2.6        # reach length, miles

# here the ftable is provided directly to be consistent with the example UCI 
# file for HSPExp

ftable = [[  0.0,    0.0,   0.0,      0.0],
          [ 0.22,  0.765,   0.09,    0.09],
          [0.439,   1.53,   0.36,    0.58],
          [0.659,  2.295,   0.81,     1.7],
          [0.878,  3.058,   1.44,    3.67],
          [1.098,  3.635,   2.23,     6.8],
          [1.318,  3.988,   3.16,   11.33],
          [1.537,  4.238,   4.15,   17.05],
          [1.757,  4.387,    5.2,   24.03],
          [1.976,  4.497,   6.27,   32.02],
          [2.196,  4.606,   7.37,   40.86],
          [2.415,  4.716,   8.49,   50.51],
          [2.635,  4.826,   9.63,   60.94],
          [3.771, 63.882,  86.79,  311.2],
          [4.907, 71.162, 163.46,  747.96],
          [6.043, 78.442, 248.35, 1356.32],
          [7.179, 83.771, 340.12, 2146.83],
          [8.315, 91.541, 438.68, 3060.87],
          ]

# add the reach info to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, ftable = ftable)

# subbasin land use info (to create perlnds and implnds)

landuse_names = ['FOREST', 'ROWCRP', 'GRASS', 'Developed']
areas         = [4428.9,     641.63,  776.87,      120.18]  # acres

# fraction of developed land that is impervious (assume all impervious land)

ifraction = 1.

# add the landuse

subbasin.add_landuse(1988, landuse_names, areas)

# create a dictionary of the subbasins

subbasins = {sname: subbasin}

# create an updown dictionary for the reach network (trivial with only 1)

updown = {}

# create an instance of the watershed class to store the data to build the model

watershed = Watershed(description, subbasins)

# add the network and the outlet subbasin

watershed.add_mass_linkage(updown)
watershed.add_outlet(sname)

# make the HSPFModel instance (the data for this example use the non-default
# option of English instead of metric units)

from pyhspf import HSPFModel

hspfmodel = HSPFModel(units = 'English')

# since the climate data are provided with hspexp in an export file called
# "huntobs.exp."  WDMUtil has a method to automatically import the data to a 
# WDM file.

from pyhspf import WDMUtil

wdm = WDMUtil()

# path to hspexp2.4 data files (make sure the path is correct) 
# the data from the export file (*.exp) provided with hspexp need to be 
# imported into a wdm file; the WDMUtil class has a method for this

huntday = 'data/huntday/huntobs.exp'

f = 'hunting.wdm'

# import from exp to wdm

wdm.import_exp(huntday, f)

# copy the data to the hspfmodel using WDMUtil. in general climate 
# data would need to come from some other place  (not a wdm file); 
# e.g., an NCDC file. the preprocessing modules can automate this
# for the hspexp example, only one timeseries for precip and evap 
# are provided. the file also contains the observed flow at the outlet. 
# this is set up to find the dsns, time steps etc, though if they were 
# known they could be provided directly.

# open the wdm for read access

wdm.open(f, 'r')

# find all the dsns

dsns = wdm.get_datasets(f)

# find all the time series types 
# (this is how they are identified in the exp file)

tstypes = [wdm.get_attribute(f, n, 'TSTYPE') for n in dsns]

# find the precip and evap timeseries (we could also just look at the exp files
# to figure this out, but this illustrates some of the flexibility of PyHSPF)

precip_dsn = dsns[tstypes.index('HPCP')]
evap_dsn   = dsns[tstypes.index('EVAP')]

# get the time series and start and end dates

precip = wdm.get_data(f, precip_dsn)

start, end = wdm.get_dates(f, precip_dsn)

evap = wdm.get_data(f, evap_dsn, start = start, end = end)

# the observed flow is dsn 281

oflow = wdm.get_data(f, 281, start = start, end = end)

# close up the wdm file (forgetting this WILL cause trouble)

wdm.close('hunting.wdm')

# make a list of the times in the daily time series using datetime "timedelta"

delta = datetime.timedelta(days = 1)
times = [start + i * delta for i in range(len(precip))]

# build the model (file will all be called example02)

hspfmodel.build_from_watershed(watershed, 'example02', ifraction = ifraction,
                               tstep = tstep)

# now add the time series to the model

hspfmodel.add_timeseries('precipitation', 'hunting_prec', start, precip, 
                         tstep = tstep)
hspfmodel.add_timeseries('evaporation', 'hunting_evap', start, evap, 
                         tstep = tstep)

# and assign the time series to all the operations in the watershed

hspfmodel.assign_watershed_timeseries('precipitation', 'hunting_prec')
hspfmodel.assign_watershed_timeseries('evaporation',   'hunting_evap')

# this simulation used the hydrology modules (and no others); need to create the
# operations for the model and the default values for the hydrology parameters

hspfmodel.add_hydrology()

# build the input wdm file

hspfmodel.build_wdminfile()

# tell HSPF to keep track of the outflow volume from the reach, which
# has a Fortran name "ROVOL" and a PyHSPF name "reach_outvolume"

targets = ['reach_outvolume']

# create the UCI for the simulation period to provide the targets

hspfmodel.build_uci(targets, start, end, hydrology = True)

# run it

hspfmodel.run(verbose = True)

# pull up the results; this output wdmfile is created automatically
# and has the same name as the "filename" plus "_out.wdm"

wdmoutfile = 'example02_out.wdm'

wdm.open(wdmoutfile, 'r')

# get the dataset numbers

dsns = wdm.get_datasets(wdmoutfile)

# get the constituent ids for the datasets 

idconss = [wdm.get_attribute(wdmoutfile, n, 'IDCONS') for n in dsns]

# find the id for ROVOL

rovol_dsn = dsns[idconss.index('ROVOL')]

# get the ROVOL timeseries

rovol = wdm.get_data(wdmoutfile, rovol_dsn)

# convert volumes (acre-ft per day) to flows (cfs)

sflow = rovol * 43560 / 86400

# plot it up. because of the long time step the flow peaks are severely
# damped. in the next example uses hourly flows

from matplotlib import pyplot

fig = pyplot.figure()

# axes for flow and precip

ax1 = fig.add_subplot(211)
ax2 = fig.add_subplot(212)

ax1.plot_date(times, precip, fmt = 'b-', lw = 0.3)
ax2.plot_date(times, oflow, fmt = 's', markeredgecolor = 'red', 
              markeredgewidth = 0.3, markerfacecolor = 'None')
ax2.plot_date(times, sflow, fmt = 'r-', lw = 0.3)

ax2.set_xlabel('Date', fontsize = 9)
ax1.set_ylabel('Precipitation, (in)', fontsize = 10, color = 'blue')
ax2.set_ylabel('Flow (ft\u00B3/s)', fontsize = 10, color = 'red')

pyplot.show()
