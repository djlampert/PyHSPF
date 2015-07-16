# stripped.py
#
# David J. Lampert
#
# builds a stripped down model for calibration simulations
#

import os, pickle, datetime

from pyhspf             import WDMUtil
from pyhspf.calibration import CalibratorModel

# simulation start and end dates

start = datetime.datetime(2001, 1, 1)
end   = datetime.datetime(2002, 1, 1)

# the file path to the existing model and the new stripped down model

existing = 'C:/HSPF_data/07080106/hspf/2001baseline'
new      = 'stripped'

# open the existing model

with open(existing, 'rb') as f: hspfmodel = pickle.load(f)

# get the comid of the gage station data

comid = [g for g in hspfmodel.subbasin_timeseries['flowgage']][0]

run = False

if run:

    # build the input files and run the simulation

    hspfmodel.build_wdminfile()
    hspfmodel.build_uci(['reach_outvolume'], start, end, atemp = True, 
                        snow = True, hydrology = True)
    hspfmodel.run(verbose = True)

    # make and run a submodel that includes only the watershed for the gage

    submodel = CalibratorModel()
    submodel.build_submodel(hspfmodel, comid)

    import hspf

    hspf.wdflclpy(12)
    
    # build the input files and run the simulation

    submodel.build_wdminfile()
    submodel.build_uci(comid, start, end, atemp = True, snow = True,
                       hydrology = True)
    submodel.run(verbose = True)

    hspf.wdflclpy(12)

# compare the output from the two simulations

wdm = WDMUtil()

f1 = '{}_out.wdm'.format(existing)

wdm.open(f1, 'r')

dsns   = wdm.get_datasets(f1)
staids = [wdm.get_attribute(f1, n, 'STAID') for n in dsns]

data = wdm.get_data(f1, dsns[staids.index(comid)])

print('total 1:', sum(data))

f2 = '{}_out.wdm'.format(new)

wdm.open(f2, 'r')

data = wdm.get_data(f2, 1)

print('total 2:', sum(data))
