#!/usr/bin/env python3
#
# Example5.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 09/16/2013
#
# Purpose: Demonstrates how to builds an instance of the HSPFModel class 
# that can be used to generate UCI files for an HSPF simulation. Example
# comes from the HSPF "expert" system (hspexp) for the Hunting Creek Watershed.
# This example shows how to calibrate the HSPF model.  Assumes the reader has 
# some familiarity with Python, hydrology, and has done examples 1-4.

import datetime, pickle

from pyhspf import HSPFModel, WDMUtil, Postprocessor

# open up the pickled HSPFModel 

with open('example3', 'rb') as f: hspfmodel = pickle.load(f)

# let's change the name to example 5

hspfmodel.filename = 'example5'

# other info about the simulation

start     = datetime.datetime(1988, 10, 1)
end       = datetime.datetime(1990, 10, 1)
gagecomid = '30'          

# since we will be running this file repeatedly, we need to keep track of the
# parameters in an efficient manner. since some of them are specific to the
# land use or soil, we will define system-wide multipliers to the calibration 
# parameters relative to the default values in most cases. for example, the 
# default INFILT is 0.04 in/hr, and the INFILT_multiplier keeps track of the 
# value throughout the watershed relative to this default, so if 
# INFILT_multipler is 2, then all values of INFILT will be adjusted to 0.08 
# in/hr.  this allows any variability in space or land use to be retained 
# while still allowing for adjustment for the calibration. note large changes 
# cannot be made for many parameters since they would move values outside of 
# allowable ranges. it is also possible to establish site-wide values such as 
# AGWRC in the example below. finally, this is just one way to do this but it 
# illustrates the power of scripting vs a graphical user interface.  
# brief description of variables provided here too.

LZETP_multiplier  = 1.    # lower zone evapotranspiration parameter
LZSN_multiplier   = 1.    # lower soil zone storage capacity
UZSN_multiplier   = 1.    # upper soil zone storage capacity
INTFW_multiplier  = 1.    # interflow inflow rate
INFILT_multiplier = 1.    # infiltration rate
IRC_multiplier    = 1.    # interflow recession rate

# these two parameters are not ratios but are site wide for this example

evap_multiplier   = 0.76  # pan evaporation relative to potential ET
AGWRC             = 0.95  # groundwater recession rate (site-wide)

# now let's adjust the parameters from the defaults using the multipliers

hspfmodel.evap_multiplier = evap_multiplier

for p in hspfmodel.perlnds:

    p.LZETP  = p.LZETP * LZETP_multiplier
    p.LZSN   = p.LZSN * LZSN_multiplier
    p.UZSN   = p.UZSN * UZSN_multiplier
    p.INTFW  = p.INTFW * INTFW_multiplier
    p.INFILT = p.INFILT * INFILT_multiplier
    p.AGWRC  = AGWRC
    p.IRC    = p.IRC * IRC_multiplier

    # since this is a short simulation let's set the initial conditions too

    p.set_pwat_state(AGWS = 0.25)

# since we are looking at gage vs calibration statistics only, the external 
# target needed are the reach outflow volume and groundwater flow

targets = ['groundwater', 'reach_outvolume']

# build the input files

hspfmodel.build_wdminfile()
hspfmodel.build_uci(targets, start, end, hydrology = True)

# and run it

hspfmodel.run(verbose = True)

# open the postprocessor to get the calibration info

p = Postprocessor(hspfmodel, (start, end), comid = gagecomid) 

# calculate the errors in the calibration parameters (requires get_calibration
# to have been run). the product of the daily log-flow and daily flow Nash-
# Sutcliffe model efficiency are one possible optimization parameter for a
# calibration. the log-flow captures relative errors (low-flow conditions)
# while the flow captures absolute error (high-flow conditions).

p.get_calibration()
p.calculate_errors()

# close the open files

p.close()

# now let's change the value of some parameters, re-run the model, and see 
# the effect on the calibration statistics. we will change the default
# values by perturbing some calibration multiplier parameter by a "factor."
# since the initial run has low storm volumes, flow should be shifted from 
# interflow to surface runoff by decreasing INTFW

LZETP_multiplier  = 1.    # lower zone evapotranspiration parameter
LZSN_multiplier   = 1.    # lower soil zone storage capacity
UZSN_multiplier   = 1.    # upper soil zone storage capacity
INTFW_multiplier  = 0.8   # interflow inflow rate
INFILT_multiplier = 1.    # infiltration rate
IRC_multiplier    = 1.    # interflow recession rate
evap_multiplier   = 0.76  # pan evaporation relative to potential ET
AGWRC             = 0.95  # groundwater recession rate (site-wide)

# need different targets for a plot, uncomment if you want to visualize

#targets = ['water_state', 'reach_outvolume', 'evaporation', 'runoff',
#           'groundwater']

# need to re-open the basevalues again

with open('example3', 'rb') as f: hspfmodel = pickle.load(f)

hspfmodel.filename = 'example5'

# and now adjust all the parameters (including the new INTFW_multiplier)

hspfmodel.evap_multiplier = evap_multiplier

for p in hspfmodel.perlnds:

    p.LZETP  = p.LZETP  * LZETP_multiplier
    p.LZSN   = p.LZSN   * LZSN_multiplier
    p.UZSN   = p.UZSN   * UZSN_multiplier
    p.INTFW  = p.INTFW  * INTFW_multiplier
    p.INFILT = p.INFILT * INFILT_multiplier
    p.IRC    = p.IRC    * IRC_multiplier
    p.AGWRC  = AGWRC
    p.set_pwat_state(AGWS = 0.25)

# run the simulation and get the results

hspfmodel.build_wdminfile()
hspfmodel.build_uci(targets, start, end, hydrology = True)
hspfmodel.run(verbose = True)

p = Postprocessor(hspfmodel, (start, end), comid = gagecomid) 

p.get_calibration()
p.calculate_errors()

# lather, rinse, repeat by changing the multipliers until the calibration is 
# satisfactory. this first run should have gone from a NS for flow * NS for 
# log flow of 0.268 to 0.288, meaning for the next run we would want to start
# with INTFW = 0.9 rather than 1; the optimized values are commented out below.
# i got a daily NS of 0.77 and daily NS for log flow of 0.68 after about an 
# hour of adjusting just these parameters. use the commented out values in 
# the next lines and change the targets above whenever you want to make 
# plots of the results.

#evap_multiplier   = 0.76
#LZETP_multiplier  = 1.1
#LZSN_multiplier   = 0.85
#UZSN_multiplier   = 4.1
#INTFW_multiplier  = .22
#INFILT_multiplier = .95
#AGWRC             = 0.95
#IRC_multiplier    = .60

#p.plot_runoff(tstep = 'daily', output = 'runoff', show = False)
#p.plot_storms(tstep = 'hourly', output = 'storms', show = False)
#p.plot_calibration(verbose = True, output = 'calibration', show = False)
