#!/usr/bin/env python3
#
# example7.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: Demonstrates how to build an instance of the HSPFModel class 
# that can be used to generate UCI files for an HSPF simulation with PyHSPF
#
# Last updated: 04/30/2014
#
# This is a repeat of example1.py, but illustrating how to add special actions.

# start and end dates (year 2001). we will use the datetime module for this.

import datetime

start = datetime.datetime(2001, 1, 1)
end   = datetime.datetime(2003, 1, 1)

# do a 4-hour timestep; time step should be specified in minutes

tstep = 240

from pyhspf import Watershed, Subbasin, HSPFModel, WDMUtil

subbasins = {}  # subbasin dictionary

# we'll call the first subbasin 100

number   = '100'            # subbasin number
subbasin = Subbasin(number) # created subbasin "100"

# subbasin attributes

# flow plane parameters for the subbasin

length     = 100       # m
planeslope = 0.02      # -
area       = 100       # km2
elev       = 100       # m
centroid   = [-90, 40] # long, lat

# add the flow plane data for subbasin 100

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# reach info

name       = 'stream'   # something descriptive
maxelev    = 110        # elevation at the top of the reach (m)
minelev    = 100        # elevation at the bottom of the reach (m)
slopelen   = 10         # the reach length (km)

# estimates of the average conditions can be used to develop FTABLES (used by
# HSPF to specify stage-discharge relationship) or specified directly

flow       = 12            # the average flow must be in cfs
velocity   = 1             # velocity must be in fps

# add the reach to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, flow = flow, 
                   velocity = velocity)

# land segments

landuse_names = ['Developed', 'Agriculture', 'Forest']
areas         = [20, 40, 40]

subbasin.add_landuse(2001, landuse_names, areas)

# add the subbasin to the dictionary of subbasins

subbasins[number] = subbasin

# make another subbasin for this example

number   = '101'
subbasin = Subbasin(number)

# same flowplane parameters

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# slightly change the reach info

maxelev  = 100 
minelev  = 90
flow     = 12

subbasin.add_reach(name, maxelev, minelev, slopelen, flow = flow, 
                   velocity = velocity)

# for simplicity just assume the same landuse types and areas

subbasin.add_landuse(2001, landuse_names, areas)

# and add the subbasin to the subbasins dictionary

subbasins[number] = subbasin

# now that we have subbasins we can go ahead and create an instance of the 
# Watershed class, which is used to build the HSPF input files.

watershed_name = 'Dave'

watershed = Watershed(watershed_name, subbasins)

# flow network dictionary

updown = {'100':'101', '101':0}

# add the info to the watershed and outlet

watershed.add_mass_linkage(updown)
watershed.add_outlet('101')

# names of the files used in the simulation (the HSPF input and output files
# are generated automatically); can also specify a directory to use elsewhere

filename   = 'example7'
wdmoutfile = filename + '_out.wdm'
outfile    = 'example7.out' 

# create an instance of the HSPFModel class

hspfmodel = HSPFModel()

# and build the model from the watershed

hspfmodel.build_from_watershed(watershed, filename, print_file = outfile, 
                               tstep = tstep)

# let's now add a special action, thawed ground on the agricultural land
# in the first subbasin on April 1 at 12 noon.

thawdate = datetime.datetime(2001, 4, 1, 12)

hspfmodel.add_special_action('thaw', '100', 'Agriculture', thawdate)

# let's add another special action, frozen ground on the agricultural land
# in the first subbasin on December 1 at midnight.

freezedate = datetime.datetime(2001, 12, 1)

hspfmodel.add_special_action('frozen', '100', 'Agriculture', freezedate)

# climate forcing time series

# let's assume the pan evapotranspiration starts at zero then increases 
# to 12 mm in a day 7/01, then decreases to zero 1/01; thus max 4-hr 
# evaporation is 2 mm.

maxET = 2
nsteps = (end-start).days * 1440 // tstep
evaporation = [maxET * (d - datetime.datetime(d.year, 1, 1)).days / 
               (datetime.datetime(d.year, 7, 1) - 
                datetime.datetime(d.year, 1, 1)).days
               if d.month < 7
               else
               maxET - maxET * (d - datetime.datetime(d.year, 7, 1)).days / 
               (datetime.datetime(d.year + 1, 1, 1) - 
                datetime.datetime(d.year, 7, 1)).days
               for d in [start + datetime.timedelta(minutes = tstep) * i
                         for i in range(nsteps)]
               ]

# specify the time series type

tstype = 'evaporation'

# give the dataset a unique identifier

identifier = 'example_evap'

# finally need the start date, a list of the data, and the time step (min)

hspfmodel.add_timeseries(tstype, identifier, start, evaporation, tstep = tstep)

# assign the time series for this model

hspfmodel.assign_watershed_timeseries(tstype, identifier)

# now let's add some random rainfall. let's assume there is a 5% chance of rain
# every 4-hour period and that the rainfall is an integer between 0 and 20.

import random

# make random numbers for each 4 hour timestep
# if the number is greater than 0.95 (5% chance), let's say it's raining and
# assign a value (this should give about a meter of rain per year)

rainfall = [random.randint(0,20) if random.random() > 0.95 else 0. 
            for i in range(nsteps)]

# assign the precipitation time series to the file

tstype     = 'precipitation'
identifier = 'example_prec'

hspfmodel.add_timeseries(tstype, identifier, start, rainfall, tstep = tstep)

# again we need to connect the time series to the whole watershed

hspfmodel.assign_watershed_timeseries(tstype, identifier)

# now we need to tell HSPF to run hydrology and assign default parameters

hspfmodel.add_hydrology()

# and now we can build the wdm input file using the timeseries

hspfmodel.build_wdminfile()

# the last piece of info we need is the output we want from the model, which
# is stored in an output WDM file (this is made automatically). PyHSPF doesn't
# have every possible external target, but there are a bunch and the list could
# be appended pretty easily if needed.  the base assumption is every time step
# for fluxes and daily for state variables.

targets = ['reach_outvolume',  # the volume that exits each reach at each step
           'evaporation',      # the evaporation volume in the land segments
           'reach_volume',     # the volume in the reach
           'runoff']           # the surface runoff

# now then, the "build_uci" function can be used to build the UCI input file.
# it also builds the output WDM file since they work together with 
# the UCI file. in this example we are just doing hydrology but you can add 
# (provided you give the data) snow atemp, and sediment.  the other modules 
# need to be developed.

hspfmodel.build_uci(targets, start, end, hydrology = True, verbose = False)

# now the input files are ready, so run it:

hspfmodel.run(verbose = True)

# assuming that went ok (look at the echo and out files), we can retrieve the
# results using WDMUtil

wdm = WDMUtil()

# open the file for read access

wdm.open(wdmoutfile, 'r')

# let's pull up the flow at the outlet and plot it along with the precipitation
# and evapotranspiration. the attributes that identify the data are "IDCONS"
# (constituent ID) and "STAID " (station ID). these were assigned by the
# build_wdminfile and build_UCI routines automatically; modify as needed.

dsns    =  wdm.get_datasets(wdmoutfile)
idconss = [wdm.get_attribute(wdmoutfile, n, 'IDCONS') for n in dsns]
staids  = [wdm.get_attribute(wdmoutfile, n, 'STAID ') for n in dsns]

# one HSPF parameter we saved is ROVOL (the postprocessor can be used to 
# simplify this, but for now let's just use WDMUtil). The following
# finds the right dsn. see if you can follow the syntax.

n = [dsn for dsn, idcons, staid in zip(dsns, idconss, staids)
     if idcons == 'ROVOL' and staid == '101'][0]

rovol = wdm.get_data(wdmoutfile, n)

# it's always a good idea to close up the fortran files.

wdm.close(wdmoutfile)

# rovol is the total volume (in Mm3) at each time step. so we need to convert
# it m3/s. we could have had HSPF do this, but it's nice to keep track of all
# the fluxes for looking at mass balance checks.

flows = [r * 10**6 / 3600 / 4 for r in rovol]

# you could retrieve the precipitation and evapotranspiration data from the
# input file if needed but we have it still from above so I'll skip that.
# let's plot it up right quick with matplotlib. we'll use the plotdate class.

from matplotlib import pyplot

# we need a list of the dates/times for the plot

times = [start + i * datetime.timedelta(hours = 4)
         for i in range(int((end - start).total_seconds() / 3600 / 4))]

# again, isn't it cool how fast you can do something like this w/python?
# i'm omitting details here, plenty of info elsewhere on matplotlib.

fig = pyplot.figure(figsize = (8, 10))

axes = [fig.add_subplot(3, 1, i + 1) for i in range(3)]

axes[0].plot_date(times, rainfall, fmt = 'b-', lw = 0.3)
axes[1].plot_date(times, evaporation, fmt = 'g-', lw = 0.3)
axes[2].plot_date(times, flows, fmt = 'r-', lw = 0.3)

axes[2].set_xlabel('Date', fontsize = 9)
axes[0].set_ylabel('Precipitation, (mm)', fontsize = 10, color = 'blue')
axes[1].set_ylabel('Evapotranspiration (mm)', fontsize = 10, color = 'green')
axes[2].set_ylabel('Flow (m\u00B3/s)', fontsize = 10, color = 'red')

for ax in axes: ax.tick_params(axis = 'both', size = 9)
fig.autofmt_xdate(rotation = 25)

# show it

pyplot.show()

