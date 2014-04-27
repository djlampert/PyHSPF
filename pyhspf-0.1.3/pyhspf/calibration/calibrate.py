#!/usr/bin/env python3
#
# function to do calibration actions based on Upper Mississippi River Basin
# pre-processing and calibration methodology

import os, pickle, datetime

from pyhspf.calibration import HydrologyParameters
from pyhspf.calibration import HydrologyCalibrator
from pyhspf.calibration import SedimentCalibrator

def hydrology(NWISgageid, HUC8, start_year, end_year, 
              ccfact, uzsn_adjustment, lzsn_adjustment, lzetp_adjustment, 
              infilt, intfw, agwrc, kvary, deepfr, irc, ftable,
              warmup = 1, upcomids = None, parallel = True, level = 1, 
              run = True, perturbations = None, show_figures = None,  
              save = False, mass_balance = False, get_advice = False):

    if os.name == 'nt': directory = 'C:/HSPF_data'
    else:               directory = '/home/dave/HSPF_data'

    # simulation and processing dates

    run_dates     = (datetime.datetime(start_year, 1, 1),
                     datetime.datetime(end_year, 1, 1))

    process_dates = (datetime.datetime(start_year + warmup, 1, 1),
                     datetime.datetime(end_year, 1, 1))

    # create the base model

    basemodel   = directory + '/%s/hspf/basecase' % HUC8
    base_values = HydrologyParameters()

    # adjust perturbations when the model is close

    if perturbations is not None:
        perturbations = {p: (v / level) for p, v in perturbations.items()}

    # modify base values for the HSPF parameters 

    base_values.ccfact = ccfact
    base_values.uzsn   = uzsn_adjustment
    base_values.lzsn   = lzsn_adjustment
    base_values.lzetp  = lzetp_adjustment
    base_values.infilt = infilt
    base_values.intfw  = intfw
    base_values.agwrc  = agwrc
    base_values.kvary  = kvary
    base_values.irc    = irc
    base_values.ftable = ftable
    base_values.deepfr = deepfr

    print('')

    # open up an instance of the Calibrator class to work with

    calibrator = HydrologyCalibrator()

    # add the error tolerances (use defaults)

    calibrator.add_error_criteria()

    # build the hspfmodel

    if not os.path.exists(basemodel):
        calibrator.build_hspfmodel(directory, HUC8, run_dates, overwrite = True)
    else: 
        calibrator.set_vars(directory, HUC8, run_dates)

    # set the gage

    calibrator.set_flowgage(NWISgageid, upcomids = upcomids)

    # generate models for various parameters

    if perturbations is not None:
        variables, perturbs = zip(*perturbations.items())
        calibrator.perturb(base_values, list(variables), list(perturbs), 
                           parallel = parallel, process_dates = process_dates, 
                           show = True)

    if mass_balance: 
        calibrator.check_mass_balance(base_values, run = run, 
                                      process_dates = process_dates)

    if get_advice:   
        calibrator.get_advice(base_values, run = run, 
                              process_dates = process_dates)

    if show_figures is not None:

        snow            = ('snow'            in show_figures)
        snowcalibration = ('snowcalibration' in show_figures)
        hydrograph      = ('hydrograph'      in show_figures)
        runoff          = ('runoff'          in show_figures)
        storms          = ('storms'          in show_figures)
        calibration     = ('calibration'     in show_figures)
        
        calibrator.show_figures(base_values, 
                                run = run, 
                                process_dates = process_dates, 
                                snow = snow,
                                snowcalibration = snowcalibration,
                                hydrograph = hydrograph, 
                                runoff = runoff,
                                storms = storms,
                                calibration = calibration,
                                )

    if save:
        calibrator.save_calibration(base_values, process_dates = process_dates)

def sediment(NWISgageid, HUC8, start, end, run = True, warmup = 1,
             save = False, mass_balance = False, show_figures = None):

    if os.name == 'nt': directory = 'C:/HSPF_data'
    else:               directory = '/home/dave/HSPF_data'

    hydrology_model = '{}/{}/calibrations/{}/hspfmodel'.format(directory, HUC8, 
                                                               NWISgageid)

    calibrator = SedimentCalibrator(sediment = True)

    run_dates       = [datetime.datetime(start, 1, 1),
                       datetime.datetime(end, 1, 1)]
    process_dates   = [datetime.datetime(start + warmup, 10, 1),
                       datetime.datetime(end, 10, 1)]

    calibrator.set_vars(directory, HUC8, run_dates)
    calibrator.set_flowgage(NWISgageid)

    if mass_balance:
        calibrator.check_mass_balance(hydrology_model, run = run, 
                                      process_dates = process_dates)
        run = False

    if save:
        calibrator.save(hydrology_model, run = run, 
                        process_dates = process_dates)

    if show_figures is not None:        
        tss     = ('tss' in show_figures)
        erosion = ('erosion' in show_figures)
        solids  = ('solids' in show_figures)
        loading = ('loading' in show_figures)

        calibrator.show_figures(hydrology_model, run = run, tss = tss, 
                                solids = solids,
                                erosion = erosion, loading = loading,
                                process_dates = process_dates)
