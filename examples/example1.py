#!/usr/bin/env python3
#
# example1.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: Demonstrates how to build an instance of the HSPFModel class 
# that can be used to run HSPF simulations. Minimal assumptions are made 
# of the user's background in Python. The idea is to learn how the 
# "think in HSPF."
#
# Last updated: 05/01/2014
#
# This is a very basic example of how to create an HSPF model in Python using 
# PyHSPF. No external data files are needed. Python can be used to script the
# data in as needed. Details throughout. Keep in mind this is a "walk before
# you run" approach.

# start and end dates (year 2001). we will use the datetime module for this.

import datetime

start = datetime.datetime(2001, 1, 1)
end   = datetime.datetime(2002, 1, 1)

# do a 4-hour timestep; time step should be specified in minutes. HSPF can use
# any time step from 1 minute to 1 day (1440 minutes) as long as it divides 
# evenly into 1440.

tstep = 240

# PyHSPF has a "Watershed" class to store information about the watershed 
# related to physical properties (it has nothing to do with HSPF per se).
# instances of this class are used to generate the HSPFModel class.  
# I'll illustrate how to use it here.

from pyhspf import Watershed

# first the watershed needs to be subdivided into subbasins, which are
# stored in a dictionary where the keys correspond to the names you want 
# to give them. the keys must be strings of at most 15 characters for 
# consistency with the WDM format. a subbasin consists of a flowplane to 
# store information about the landscape, the reach/reservoir that the land 
# drains to, and landuse (or other category data) to sudivide the subbasin 
# into homogeneous land segments.

from pyhspf import Subbasin

subbasins = {}  # subbasin dictionary

# we'll call the first subbasin 100

number   = '100'             # subbasin number
subbasin = Subbasin(number)  # created subbasin "100"

# subbasins are defined by many attributes, which are grouped into categories 
# including:
#
# --the flowplane (slope length, area, centroid, average elevation)
#
# --the reach (name, upstream number, upstream elevation, downstream elevation, 
# length, average slope, inflow rate, outflow rate, average velocity, and 
# traveltime)
#
# --the landuse categories to be used by the model and the corresponding areas 
# within each subbasin 
#
# --the inlets and outlets from the watershed

# flow plane parameters for the subbasin

length     = 100       # m
planeslope = 0.02      # -
area       = 100       # km2
elev       = 100       # m
centroid   = [-90, 40] # lon, lat

# add the flow plane data for subbasin 100

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# now let's provide the info about the reach

name       = 'dave stream' # something descriptive
maxelev    = 110           # elevation at the top of the reach (m)
minelev    = 100           # elevation at the bottom of the reach (m)
slopelen   = 10            # the reach length (km)

# estimates of the average conditions can be used to develop FTABLES (used by
# HSPF to specify stage-discharge relationship) or specified directly

flow       = 10            # the inflow (cfs) sorry about the engligh units
velocity   = 1             # velocity (fps) again sorry about the english units

# now let's add the reach to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, flow = flow, 
                   velocity = velocity)

# another piece of info we need is the landuse (or however we want to 
# subdivide the subbasins into land segments, e.g. soils). so here let's
# just assume 20% is developed with 50% impervious, 40% agriculture, and 40% 
# forest. The areas are in square km so we get 20 km2 impervious, etc. 
# more than one landuse can change stored (for every year), but it's 
# not necessary to change it for HSPF. If you want to have impervious land, 
# you must use "Developed" to identify it; the relative percentages of 
# developed land can be changed from the default of 50% if desired.

landuse_names = ['Developed', 'Agriculture', 'Forest']
areas         = [20, 40, 40]

subbasin.add_landuse(2001, landuse_names, areas)

# now it's done; let's add the subbasin to the dictionary of subbasins

subbasins[number] = subbasin

# let's make one more subbasin for this example (note all the parameters the
# same except these few)

number   = '101'
subbasin = Subbasin(number)

# let's just use the same flowplane parameters

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# slightly change the reach info

maxelev    = 100 
minelev    = 90
flow       = 12

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

# another key piece of information for the watershed is the flow network. 
# it should be provided as  an "updown" dictionary--that is, you supply a 
# subbasin number, and the dictionary returns the downstream number. So for 
# this example we have subbasin 100 goes into 101 and we'll say that 101 goes 
# into 0 to indicate it doesn't connect to anything (is a watershed outlet). 

updown = {'100':'101', '101':0}

# add the info to the watershed and tell HSPF where the outlet is

watershed.add_outlet('101')
watershed.add_mass_linkage(updown)

# so that's all the physically-based data needed by HSPF. we can now make an 
# instance of another class, the HSPFmodel, which can be used to build the 
# input files (the input and output WDM files and the UCI file).

from pyhspf import HSPFModel

# names of the files used in the simulation (the HSPF input and output files
# are generated automatically); can also specify a directory to use elsewhere

filename = 'example1'

# the UCI file name will be 'example1.uci'
# the input and output WDM filenames are generated automatically, and are the
# model filename + '_in.wdm' for the input WDM file and '_out.wdm' for the 
# output file (we'll need this later to retrieve results from the files)

wdmoutfile = filename + '_out.wdm'

# let's also generate an optional output file created by HSPF directly

outfile = 'example1.out' 

# create an instance of the HSPFModel class

hspfmodel = HSPFModel()

# and build the model from the watershed

hspfmodel.build_from_watershed(watershed, filename, print_file = outfile, 
                               tstep = tstep)

# the last thing we need for the simulation is to assign precipitation, 
# potential evapotranspiration, and any other time series to the subbasins.
# there are many different ways to estimate the potential evapotranspiration 
# including correlation to observed pan evaporation, Penman-Monteith, etc. 
# let's assume the potential evapotranspiration starts at zero then increases 
# to 12 mm in a day 7/01, then decreases to zero 1/01; thus max 4-hr 
# potential evapotranspiration is 2 mm. see if you can work out the logic :0)

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

# now we need to tell HSPF how to use the time series for this model. the unique
# identifier for the time series and the unique subbasin numbers are used 
# to make this connection. we will assign this time series to the whole
# watershed, although you can have a unique time series for each subbasin, 
# landuse category, or each operation if you want.

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

# the targets above each correspond to a particular Fortran variable; the idea
# is to make them more descriptive. the targets above correspond to:
#
# reach_outvolume = ROVOL
# evaporation     = TAET for PERLNDs, and IMPEV for IMPLNDs
# reach_volume    = RO
# runoff          = SURO, IFWO, and AGWO for PERLNDs, SURO for IMPLNDs

# now then, the "build_uci" function can be used to build the UCI input file.
# it also builds the output WDM file since they work together with 
# the UCI file. in this example we are just doing hydrology but you can add 
# (provided you give the data) snow atemp, and sediment.  the other modules 
# need to be developed.

hspfmodel.build_uci(targets, start, end, hydrology = True, verbose = False)

# now the input files are ready, so run it:

hspfmodel.run(verbose = True)

# let's "pickle" the hspfmodel for later. "pickling" means writing a python
# object to a file so that you can access it later. the idea (from my point of
# view) is to save the python HSPFModel, and forget about the UCI. it's always
# there is you want to see it, but changing the parameters is much easier in
# python, and can even be scripted. pickling is pretty easy. the "with"
# statement just closes the file automatically.

import pickle

with open('hspfmodel', 'wb') as f: pickle.dump(hspfmodel, f)

# assuming that went ok (look at the echo and out files), we can retrieve the
# results using WDMUtil

from pyhspf import WDMUtil

# create an instance of WDMUtil

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

# uncomment if you want to see what's here in the output file

# print(dsns, idconss, staids)

# one HSPF parameter we saved is ROVOL (the postprocessor can be used to 
# simplify this, but for now let's just use WDMUtil). The following
# finds the right dsn. see if you can follow the syntax.

n = [dsn for dsn, idcons, staid in zip(dsns, idconss, staids)
     if idcons == 'ROVOL' and staid == '101'][0]

rovol = wdm.get_data(wdmoutfile, n)

# it's always a good idea to close up the files opened by Fortran

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

# ok, remember when we pickled the hspfmodel? let's pull it back up and change
# some parameters. you may have noticed we left out a critical part of the 
# HSPF model--the hydrology parameters. the classes i have built have default
# values in the hspfmodel.py file. separate classes are made for PERLND, IMPLND,
# RCHRES that store these parameters. i'll briefly show how to pull open the
# hspf file and change some parameters. you can always change the defaults
# as needed or just modify the UCI file once it's been created. whatever works.

# open the pickled version

with open('hspfmodel', 'rb') as f: hspfmodel = pickle.load(f)

# let's change the lower zone storage number from 150 to 50 and the upper zone
# storage number from 10 to 5 for each of the perlnds and see the effects on
# the model output. HSPF parameter names are attached to the perlnd instances
# they all have 4-6 character variables in Fortran

for p in hspfmodel.perlnds: 
    p.LZSN = 50
    p.UZSN = 5

# now just repeat the run and postprocessing (i omit comments). this could
# easily be scripted into a function.

hspfmodel.build_uci(targets, start, end, hydrology = True, verbose = False)
hspfmodel.run()

wdm.open(wdmoutfile, 'r')

flows = [r * 10**6 / 3600 / 4 for r in wdm.get_data(wdmoutfile, n)]

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

pyplot.show()

# play around with the assumptions and get familiar with the idea
