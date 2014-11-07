#!/usr/bin/env python3
#
# Example4.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 09/20/2014
#
# Purpose: Demonstrates how to use the Postprocessor class to analyze the 
# results of an HSPF simulation. The example comes from the HSPF "Expert" 
# system (hspexp) for the Hunting Creek Watershed. Assumes the reader has 
# some familiarity with Python, hydrology, and has done examples 01-03.

import os, time, datetime, pickle

from pyhspf import HSPFModel, WDMUtil

model = 'example03'
if not os.path.isfile(model):
    print('missing model file; re-run example03.py')
    raise

with open(model, 'rb') as f: hspfmodel = pickle.load(f)

# change the name to example04

hspfmodel.filename = 'example04'

# HSPF traditionally utilized pan evaporation data, which are usually multiplied
# by a pan coefficient of approximately 0.7 to adjust the pan evaporation to
# potential evapotranspiration. HSPFModel has an "evap_multiplier" attribute
# for this purpose, although it may be easier to develop "final" input 
# timeseries and just leave the pan coefficient at 1. to be consistent with the 
# Hunting evap_multiplier is adjusted to 0.76 here.

hspfmodel.evap_multiplier = 0.76

# build the input wdm file

hspfmodel.build_wdminfile()

# the external targets in the hunting example are the runoff components,
# simulated evaporation, and the reach outflow.

targets = ['water_state',     # state variables for each operation (daily)
           'reach_outvolume', # outflow volume for each reach
           'evaporation',     # simulated ET for each operation
           'runoff',          # runoff components (SURO, IFWO, AGWO)
           'groundwater'      # deep groundwater recharge (IGWI)
           ]

# now we can create the UCI for the simulation period to provide the targets

start, tstep, precip = hspfmodel.precipitations['hunting_prec']

# make a list of the precipitation inputs times

ptimes = [start + datetime.timedelta(minutes = tstep) * i 
          for i in range(len(precip))]

# start and end dates for the run

run_dates = ptimes[0], ptimes[-1]

# build the uci

hspfmodel.build_uci(targets, run_dates[0], run_dates[1], hydrology = True)

# and run it

hspfmodel.run(verbose = True)

# the Postprocessor class can be used to analyze results. the following lines
# show how to use it to do some analysis and make some cool graphs.

from pyhspf import Postprocessor

# the dates of the processing period can be changed, and the postprocessor
# can be used to analyze part of the watershed rather than the whole model.
# for example, if a gage is located at a subbasin other than the last outlet.
# these are optional, the last outlet is assumed to be the gage otherwise and 
# the run dates are used as the processing dates by default.

process_dates = run_dates   # postprocessing dates
gagecomid     = '30'        # the subbasin identifier for the gage

p = Postprocessor(hspfmodel, process_dates, comid = gagecomid)

# here are some examples of things that can be done with the postprocessor.
# many of these require certain external targets to be specified when building
# the model (e.g. runoff, groundwater, snow)

# make a plot of daily or monthly flows, precipitation, and evapotranspiration

p.plot_hydrograph(tstep = 'daily')

# plot the runoff components, flows, and precipitation on linear and log scales

p.plot_runoff(tstep = 'daily')

# make a similar plot looking at the largest storm events for each year both  
# in summer and outside summer

p.plot_storms(tstep = 'hourly')

# make plots of calibration statistics including flow-duration curves and 
# parity plots for both daily and monthly flows

p.plot_calibration(verbose = True)

# get a mass balance of the components

p.get_mass_balance()

# calculate the HSP Expert parameters for the simulated and observed data

p.get_hspexp_parameters()

# calculate and show the errors in the calibration parameters. the product 
# of the daily log-flow and daily flow Nash-Sutcliffe model efficiency are 
# one possible optimization parameter for a calibration. the log-flow 
# captures relative errors (low-flow conditions) while the flow captures 
# absolute error (high-flow conditions).

p.calculate_errors()
