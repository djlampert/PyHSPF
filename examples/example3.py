#!/usr/bin/env python3
#
# Example3.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 05/07/2014
#
# Purpose: Demonstrates how to build an instance of the HSPFModel class 
# that can be used to generate UCI files for an HSPF simulation. Example
# comes from the HSPF "expert" system (hspexp) for the Hunting Creek Watershed.
# This example shows how to build the HSPFModel and saves it up for later
# (calibration).  Assumes the reader has some familiarity with Python, 
# hydrology, and has done examples 1 and 2.

import os, datetime, pickle

from pyhspf import Watershed, Subbasin, HSPFModel, WDMUtil

# inputs

description = 'Hunting Creek, Patuxent Basin'
start       = datetime.datetime(1988, 10, 1)
end         = datetime.datetime(1990, 10, 1)
tstep       = 60
units       = 'English'

# 3 subbasins/reaches; the right and left branch feed the main channel; let's
# call the left and right branches 31 and 32, and main branch 30

updown = {'32':'30', 
          '31':'30', 
          '30':0
          }

# keep track of the subbasins

subbasins = {}

# provide land info for subbasin 30

subbasin = Subbasin('30')

# overland flow plane info -- these need to be modified to agree with example
# later although the difference is very small and not really based on physical
# parameters.

length     = 700   # ft
planeslope = 0.05  # flow plane slope

# needed to build an instance of the Watershed class but not used directly

area       = 0
elev       = 0
centroid   = [0,0]

# add the flow plane info for the subbasin

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# subbasin reach info (conistent with hspfexp/data/hunthour/hunting.uci file)

name     = 'Hunting Cr'
maxelev  = 125        # inflow elevation, ft (only the difference needed)
minelev  = 100        # outflow elevlation, to give delth of 25 feet
slopelen = 0.5        # reach length, miles

# the ftable is supplied in the example; so we will add it here. if not supplied
# it is estimated from the average conditions 

ftable = [[0.0, 0.0, 0.0, 0.0],
          [0.22, 0.294, 0.04, 0.11],
          [0.439, 0.588, 0.14, 0.7],
          [0.659, 0.882, 0.32, 2.04],
          [0.878, 1.176, 0.56, 4.4],
          [1.098, 1.398, 0.86, 8.16],                                        
          [1.318, 1.534, 1.22, 13.6],
          [1.537, 1.63, 1.6, 20.46],
          [1.757, 1.688, 2.0, 28.84],
          [1.976, 1.73, 2.42, 38.42],
          [2.196, 1.772, 2.84, 49.03],
          [2.415, 1.814, 3.26, 60.61],
          [2.635, 1.856, 3.7, 73.13],
          [3.771, 24.57, 33.38, 373.44],
          [4.907, 27.368, 62.86, 897.55],
          [6.043, 30.168, 95.52, 1627.58],
          [7.179, 32.218, 130.82, 2576.2],
          [8.315, 35.206, 168.72, 3673.04]]

# add the reach info to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, ftable = ftable)

# subbasin land use info (to create perlnds and implnds)

landuse_names = ['Forest', 'Pasture/grass']
areas         = [32, 6]

# fraction of developed land that is impervious

ifraction = 1. 

# add the landuse

subbasin.add_landuse(1988, landuse_names, areas)

# add the subbasin to the dictionary

subbasins['30'] = subbasin

# Subbasin for reach 31

subbasin = Subbasin('31')

# overland flow plane info -- these need to be modified to agree with example

length     = 700   # ft
planeslope = 0.05  # flow plane slope

# needed to build an instance of the Watershed class but not used directly

area       = 0
elev       = 0
centroid   = [0,0]

# add the flow plane info for the subbasin

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# subbasin reach info (conistent with hspfexp/data/hunthour/hunting.uci file)

name     = 'Hunting Cr, rt br'
maxelev  = 125        # inflow elevation, ft (only the difference needed)
minelev  = 100        # outflow elevlation, to give delth of 25 feet
slopelen = 2.6        # reach length, miles

# the ftable is supplied in the example; so we will add it here. if not supplied
# it is estimated from the average conditions 

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
          [8.315, 91.541, 438.68, 3060.87]]

# add the reach info to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, ftable = ftable)

# subbasin land use info (to create perlnds and implnds) -- names provide 
# hydrology default values

landuse_names = ['Forest', 'Agriculture', 'Pasture/grass', 'Developed']
areas         = [1318, 193, 231, 84]

# add the landuse

subbasin.add_landuse(1988, landuse_names, areas)

# add the subbasin to the dictionary

subbasins['31'] = subbasin

# Subbasin for reach 32

subbasin = Subbasin('32')

# overland flow plane info -- these need to be modified to agree with example

length     = 700   # ft
planeslope = 0.05  # flow plane slope

# needed to build an instance of the Watershed class but not used directly

area       = 0
elev       = 0
centroid   = [0,0]

# add the flow plane info for the subbasin

subbasin.add_flowplane(length, planeslope, area, centroid, elev)

# subbasin reach info (conistent with hspfexp/data/hunthour/hunting.uci file)

name     = 'Hunting Cr, lft br'
maxelev  = 125        # inflow elevation, ft (only the difference needed)
minelev  = 100        # outflow elevlation, to give delth of 25 feet
slopelen = 3.8        # reach length, miles

# the ftable is supplied in the example; so we will add it here. if not supplied
# it is estimated from the average conditions 

ftable = [[0.0, 0.0, 0.0, 0.0],
          [0.22, 2.236, 0.26, 0.07],
          [0.439, 4.472, 1.06, 0.46],
          [0.659, 6.708, 2.36, 1.36],
          [0.878, 8.938, 4.2, 2.94],
          [1.098, 10.626, 6.52, 5.44],
          [1.318, 11.656, 9.24, 9.06],
          [1.537, 12.388, 12.14, 13.64],
          [1.757, 12.824, 15.2, 19.22],
          [1.976, 13.144, 18.32, 25.62],
          [2.196, 13.464, 21.54, 32.69],
          [2.415, 13.784, 24.82, 40.41],
          [2.635, 14.106, 28.14, 48.75],
          [3.771, 186.728, 253.68, 248.96],
          [4.907, 208.006, 477.8, 598.37],
          [6.043, 229.286, 725.92, 1085.06],
          [7.179, 244.862, 994.18, 1717.46],
          [8.315, 267.574, 1282.26, 2448.7]]

# add the reach info to the subbasin

subbasin.add_reach(name, maxelev, minelev, slopelen, ftable = ftable)

# subbasin land use info (to create perlnds and implnds) -- names provide 
# hydrology default values

landuse_names = ['Forest', 'Agriculture', 'Pasture/grass', 'Developed']
areas         = [3078, 449, 540, 35]

# add the landuse

subbasin.add_landuse(1988, landuse_names, areas)

# add the subbasin to the dictionary

subbasins['32'] = subbasin

# create an instance of the watershed class from the subbasin information

watershed = Watershed(description, subbasins)

# add the network and the outlet subbasin

watershed.add_mass_linkage(updown)
watershed.add_outlet('30')

# since the climate data are provided with hspexp in an export file called
# "huntobs.exp."  WDMUtil has a method to automatically import the data to a 
# WDM file.

wdm = WDMUtil()

# path to hspexp2.4 data files (modify as needed)

directory = os.path.abspath(os.path.dirname(__file__)) + '/data'

# the data from the export file (*.exp) provided with hspexp need to be 
# imported into a wdm file. WDMUtil has a method for this.

hunthour = '{}/hunthour/huntobs.exp'.format(directory)

f = 'hunting.wdm'

# import from exp to wdm

wdm.import_exp(hunthour, f)

# copy the data to the hspfmodel using WDMUtil

# open the wdm for read access

wdm.open(f, 'r')

# the dsns are known from the exp file so just use those this time

precip = wdm.get_data(f, 106)
evap   = wdm.get_data(f, 111)
oflow  = wdm.get_data(f, 281)

start, end = wdm.get_dates(f, 106)

# close up the wdm file (forgetting this WILL cause trouble)

wdm.close('hunting.wdm')

# the evaporation data is daily so it needs to be disaggregated to hourly for
# an hourly simulation (see how easy this is with Python)
# for some reason the WDM file was short a time step--so had to add one extra 
# value to the end of the time series to get them to run

evap   = [e / 24 for e in evap for i in range(24)] + [0]
precip = [p for p in precip] + [0]
oflow  = [o for o in oflow] + [0]

# list of times

times = [start + (end-start) / len(precip) * i for i in range(len(precip))]

# make the HSPFModel instance

hspfmodel = HSPFModel(units = units)

# build the model (file will all be called example2)

hspfmodel.build_from_watershed(watershed, 'example3', ifraction = ifraction,
                               print_file = 'example3.out', tstep = tstep)

# now add the time series to the model

hspfmodel.add_timeseries('precipitation', 'hunting_prec', start, precip, 
                         tstep = 60)
hspfmodel.add_timeseries('evaporation', 'hunting_evap', start, evap, 
                         tstep = 60)
hspfmodel.add_timeseries('flowgage', 'hunting_flow', start, oflow, 
                         tstep = 60)

# and assign the watershed time series to all the operations

hspfmodel.assign_watershed_timeseries('precipitation', 'hunting_prec')
hspfmodel.assign_watershed_timeseries('evaporation',   'hunting_evap')

# assign the flowgage to the subbasin 30 (the outlet)

hspfmodel.assign_subbasin_timeseries('flowgage', '30', 'hunting_flow')

# this simulation used the hydrology modules (and no others); need to set the
# operations for the watershed and default values for the hydrology parameters

hspfmodel.add_hydrology()

# this example will stop here by pickling the hspfmodel for later--since the
# model will be run many times it just makes sense to save the work so far
# before moving on.

with open('example3', 'wb') as f: pickle.dump(hspfmodel, f)

