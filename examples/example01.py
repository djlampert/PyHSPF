# example01.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: Demonstrates how to build an instance of the HSPFModel class 
# that can be used to run HSPF simulations. Minimal assumptions are made 
# of the user's background in Python. The idea is to learn how the 
# "think in HSPF."
#
# Last updated: 09/20/2014
#
# This is a very basic example of how to create an HSPF model in Python using 
# PyHSPF. No external data files are needed. Python can be used to script the
# data in as needed. Details throughout. Keep in mind this is a "walk before
# you run" approach.

# import the Python datetime module to work with simulation period

import datetime

# start and end dates (year 2001)

start = datetime.datetime(2001, 1, 1)
end   = datetime.datetime(2002, 1, 1)

# do a 4-hour timestep; time step should be specified in minutes. HSPF can use
# any time step from 1 minute to 1 day (1440 minutes) as long as it divides 
# evenly into 1440.

tstep = 240

# PyHSPF has a "Watershed" class as a container for information about the 
# physical hydrography of the watershed. instances of this class are used to
# generate the HSPFModel class. the following lines illustrate how to use it.

from pyhspf import Watershed

# first the watershed needs to be subdivided into subbasins, which are
# stored in a dictionary where the keys correspond to the names you want 
# to give them. the keys must be strings of at most 15 characters for 
# consistency with the WDM format. a subbasin consists of a flowplane to 
# store information about the landscape, the reach/reservoir that the land 
# drains to, and landuse (or other category data) to sudivide the subbasin 
# into homogeneous land segments. 

from pyhspf import Subbasin

# keep up with the subbasins using the "subbasin" dictionary

subbasins = {} 

# call the first subbasin "100"

number   = '100'

# create subbasin "100"

subbasin = Subbasin(number)

# subbasins are defined by many attributes, which are grouped into categories 
# in PyHSPF Subbasin class instances including:
#
# --the flowplane (length, slope, centroid, average elevation)
#
# --the reach (name, upstream elevation, downstream elevation, length, 
#              optionally average flow rate and velocity)
#
# --the landuse categories to be used by the model and the corresponding areas 
# within each subbasin 
#
# --the inlets and outlets from the watershed

# flow plane parameters for the subbasin

length     = 100       # m
planeslope = 0.02      # -
elev       = 100       # m
centroid   = [-90, 40] # lon, lat

# add the flow plane data for subbasin 100

subbasin.add_flowplane(length, planeslope, centroid, elev)

# now let's provide the info about the reach

name       = 'dave stream' # something descriptive
maxelev    = 110           # elevation at the top of the reach (m)
minelev    = 100           # elevation at the bottom of the reach (m)
slopelen   = 10            # the reach length (km)

# HSPF uses "FTABLES" to specify the stage-discharge relationship for a reach.
# PyHSPF can estimate the FTABLE using the average flow and velocity, or the
# FTABLE can be specified directly. here the FTABLE for this subbasin reach 
# is generated from average flow and velocity.

flow       = 10            # the inflow (cfs) must use these units
velocity   = 1             # velocity (fps) again must use these units

# add the reach to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, flow = flow, 
                   velocity = velocity)

# here is an alternative set of statements to supply the FTABLE directly.
# An FTABLE consists of 4 columns representing the relationships between
# depth, surface area, volume, and flow for a reach. HSPF does a linear 
# interpolation between the depths in the first column to estimate the 
# other parameters. Up to 18 rows can be used.

#ftable = [[0,0,0,0],
#          [1,1,100,1],
#          ]

#subbasin.add_reach(name, maxelev, minelev, slopelen, ftable = ftable)

# another piece of info needed for the subbasins is the land use (used to 
# subdivide the subbasins into land segments, e.g. soils). so here this subbasin
# is assumed to be 20% developed (with 50% impervious/50% pervious land), 
# 40% agriculture, and 40% forest. The areas are in square km so we get 20 km2 
# impervious, etc. 
#
# variable land use can be provided (e.g., for different years), but it's 
# not necessary to change it for HSPF. impervious land must be specified as
# "Developed" to identify it; the relative percentages of developed land can 
# be changed from the default of 50% if desired using the "ifraction" keyword.

landuse_names = ['Developed', 'Agriculture', 'Forest']
areas         = [20, 40, 40]

subbasin.add_landuse(2001, landuse_names, areas)

# now it's done; let's add the subbasin to the dictionary of subbasins

subbasins[number] = subbasin

# make one more subbasin for this example (note all the parameters the
# same except these few)

number   = '101'
subbasin = Subbasin(number)

# let's just use the same flowplane parameters

subbasin.add_flowplane(length, planeslope, centroid, elev)

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

# now that that subbasins are specified it is possible to create an instance 
# of the Watershed class that is used to build the HSPF input files.

watershed_name = 'Dave'

watershed = Watershed(watershed_name, subbasins)

# another key piece of information for the watershed is the flow network. 
# it should be provided as  an "updown" dictionary--that is, a subbasin names
# are supplied as keys, and the dictionary returns the downstream subbasin 
# names as values. So for this example the netword is just subbasin reach "100"
# goes into "101." 

updown = {'100':'101'}

# add the mass linkage dictionary to the watershed

watershed.add_mass_linkage(updown)

# need to tell HSPF that subbasin "101" is an outlet where mass leaves. this
# information is needed because PyHSPF starts at the outlet and works 
# upstream to build the HSPF model.

watershed.add_outlet('101')

# that is all the physical hydrography data needed by HSPF. the HSPFmodel class
# can be used to build the HSPF files for the simulation (the input and output 
# WDM files and the UCI file).

from pyhspf import HSPFModel

# names of the files used in the simulation (the HSPF input and output files
# are generated automatically); can also specify a directory to use elsewhere

filename = 'example01'

# the UCI file generated by PyHSPF is named 'example01.uci' -- look at that 
# file to see how the information in this script is translated to HSPF.

# the input and output WDM filenames are generated automatically, and are the
# model filename + '_in.wdm' for the input WDM file and '_out.wdm' for the 
# output file (we'll need this later to retrieve results from the files)

wdmoutfile = filename + '_out.wdm'

# let's also generate an optional output file created by HSPF directly

outfile = filename + '.out' 

# create an instance of the HSPFModel class

hspfmodel = HSPFModel()

# and build the model from the watershed

hspfmodel.build_from_watershed(watershed, filename, print_file = outfile, 
                               tstep = tstep)

# to run a simulation it is necessary to assign precipitation, potential 
# evapotranspiration, and any other time series to the subbasins.
# there are many different ways to estimate the potential evapotranspiration 
# including correlation to observed pan evaporation, Penman-Monteith, etc. 
# here the potential evapotranspiration is assumed to start at zero then 
# increase to 12 mm in a day 7/01, then decreases to zero 1/01; thus max 4-hr 
# potential evapotranspiration is 2 mm. the following statement will generate
# a time series with these assumptions.

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

# now tell HSPF how to use the time series for this model. the unique
# identifier for the time series and the unique subbasin numbers are used 
# to make this connection. we will assign this time series to the whole
# watershed, although you can have a unique time series for each subbasin, 
# landuse category, or each operation if you want.

hspfmodel.assign_watershed_timeseries(tstype, identifier)

# now add some random rainfall. here is is assumed there is a 5% chance of rain
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

# again connect the time series to all the subbasins in the watershed

hspfmodel.assign_watershed_timeseries(tstype, identifier)

# now add default parameters to the HSPF land segments and reaches to run a
# hydrology-only simulation--similar methods exist to add the parameters for
# other HSPF modules

hspfmodel.add_hydrology()

# and now build the wdm input file using the timeseries

hspfmodel.build_wdminfile()

# the last piece of info that must be specified is the output information from 
# the simulation, which is stored in an output WDM file (made automatically
# by PyHSPF). PyHSPF doesn't have every possible HSPF external target variable,
# but the list can be appended pretty easily if needed. the base assumption is 
# every time step for fluxes and daily for state variables.

targets = ['reach_outvolume',  # the volume that exits each reach at each step
           'evaporation',      # the evaporation volume in the land segments
           'reach_volume',     # the volume in the reach
           'runoff']           # the surface runoff

# the targets above each correspond to a particular Fortran variable; the idea
# is to make them more descriptive and easier to add. the targets above 
# correspond to:
#
# reach_outvolume = ROVOL
# evaporation     = TAET for PERLNDs, and IMPEV for IMPLNDs
# reach_volume    = RO
# runoff          = SURO, IFWO, and AGWO for PERLNDs, SURO for IMPLNDs

# now the "build_uci" function can be used to build the UCI input file.
# it also builds the output WDM file since it works together with the UCI 
# file. this example just does hydrology but flags exist to add air temperature,
# snow, sediment, etc to the simulation assuming the required information has
# been provided.

hspfmodel.build_uci(targets, start, end, hydrology = True, verbose = False)

# now the input files are ready, so run it

hspfmodel.run(verbose = True)

# now "pickle" the hspfmodel for later. "pickling" means writing a python
# object to a file so that it can be accessed later. the concept is to save
# the PyHSPF HSPFModel instance, and forget about the UCI. The UCI file is 
# always there if you want to see it, but changing the parameters is much 
# easier in Python, and can even be scripted. The "with" statement just closes 
# the file where the HSPModel is stored.

import pickle

with open('hspfmodel', 'wb') as f: pickle.dump(hspfmodel, f)

# assuming that went ok (look at the HSPF-generated .ech and .out files), 
# the results can be retrieved using WDMUtil

from pyhspf import WDMUtil

# create an instance of WDMUtil

wdm = WDMUtil()

# open the file for read access

wdm.open(wdmoutfile, 'r')

# pull up the flow at the outlet and plot it along with the precipitation
# and evapotranspiration. the attributes that identify the data are "IDCONS"
# (constituent ID) and "STAID " (station ID). these were assigned by the
# build_wdminfile and build_uci routines automatically; they can be modified
# as needed. the attributes always have six characters so make sure to add 
# trailing spaces.

dsns    =  wdm.get_datasets(wdmoutfile)
idconss = [wdm.get_attribute(wdmoutfile, n, 'IDCONS') for n in dsns]
staids  = [wdm.get_attribute(wdmoutfile, n, 'STAID ') for n in dsns]

# uncomment this line to see what's here in the output file

# print(dsns, idconss, staids)

# one HSPF parameter we saved is ROVOL (PyHSPF has a Postprocessor that can 
# be used to simplify this, but WDMUtil can also be used more directly). 
# The following statement finds the dataset number for the ROVOL timeseries
# for the reach for subbasin 101.

n = [dsn for dsn, idcons, staid in zip(dsns, idconss, staids)
     if idcons == 'ROVOL' and staid == '101'][0]

# get the data for the reach volume flux dataset

rovol = wdm.get_data(wdmoutfile, n)

# need to close up the files opened by Fortran

wdm.close(wdmoutfile)

# rovol is the total volume (in Mm3) at each time step. so we need to convert
# it m3/s. we could have had HSPF do this, but it's nice to keep track of all
# the fluxes for looking at mass balance checks.

flows = [r * 10**6 / 3600 / 4 for r in rovol]

# plot it up right quick with matplotlib using the plotdate method.

from matplotlib import pyplot

# need a list of the dates/times for the plot

times = [start + i * datetime.timedelta(hours = 4)
         for i in range(int((end - start).total_seconds() / 3600 / 4))]

# details omitted here, plenty of info elsewhere on matplotlib

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

# pull open the HSPFModel that was pickled and change some hydrology process
# parameters. the PyHSPF classes have default values for the PERLNDs, IMPLNDs,
# and RCHRESs in the core module. the following lines show how to pull open the
# hspfmodel file, change some parameters, and view the impact on the simulation.

with open('hspfmodel', 'rb') as f: hspfmodel = pickle.load(f)

# change the lower zone storage number from 150 to 50 and the upper zone
# storage number from 10 to 5 for each of the perlnds and see the effects on
# the model output. HSPF parameter names are attached to the perlnd instances
# they all have 4-6 character variables in Fortran

for p in hspfmodel.perlnds: 
    p.LZSN = 50
    p.UZSN = 5

# now just repeat the run and postprocessing

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
