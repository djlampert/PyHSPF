import os, datetime

from pyhspf.forecasting import Forecaster

def NRCMhindcast(directory, HUC8, NWISgageid, start, end, warmup = 1,
                 build = True, run = True):

    it = directory, HUC8, NWISgageid

    basemodel  = '{}/{}/calibrations/{}/hspfmodel'.format(*it)

    # simulation and processing dates

    run_dates     = (datetime.datetime(start, 1, 1),
                     datetime.datetime(end, 1, 1))

    process_dates = (datetime.datetime(start + warmup, 1, 1),
                     datetime.datetime(end, 1, 1))

    start, end = run_dates

    # open up an instance of the Hindcaster class to work with

    hindcaster = Forecaster()

    # build the hindcast model or just set the variables

    hindpath = '{}/{}/hspf/NRCMhindcast'.format(directory, HUC8)
    if build:
        hindcaster.build_NRCM_hindcast(directory, HUC8, run_dates, basemodel,
                                       overwrite = True)
    else:
        hindcaster.set_vars(directory, HUC8, run_dates, basemodel, hindpath)

    print('')

    hindcaster.set_flowgage(NWISgageid)

    # run and save

    hindcaster.save_hindcast(process_dates = process_dates, run = run)

