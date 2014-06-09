# validate.py
#
# Runs a calibrated model for validation

import os, pickle, datetime

from pyhspf.core import Postprocessor

def validate(directory, HUC8, NWISgage, start, end, temp = True,
             snow = True, hydrology = True, warmup = 1, upcomids = []):

    run_dates     = (datetime.datetime(start, 1, 1),
                     datetime.datetime(end, 1, 1))

    process_dates = (datetime.datetime(start + warmup, 1, 1),
                     datetime.datetime(end, 1, 1))

    f      = '{}/{}/calibrations/{}/hspfmodel'.format(directory, HUC8, NWISgage)
    folder = '{}/{}/validations/{}'.format(directory, HUC8, NWISgage)

    with open(f, 'rb') as m: hspfmodel = pickle.load(m)

    # figure out the external targets needed

    targets = ['reach_outvolume', 'groundwater', 'water_state', 
               'evaporation', 'runoff', 'snow_state', 'snowpack', 
               'snowfall']

    # build the input wdmfile

    hspfmodel.build_wdminfile()

    # create the UCI file and the output WDM file

    hspfmodel.build_uci(targets, run_dates[0], run_dates[1], temp = temp,
                        snow = snow, hydrology = hydrology)

    hspfmodel.messagepath = None

    # run it

    hspfmodel.run(verbose = True)

    # add the simulation parameters to the calibrator

    postprocessor = Postprocessor(hspfmodel, process_dates)

    # calculate the errors and make a calibration report

    postprocessor.get_hspexp_parameters(verbose = False)
    postprocessor.get_calibration(verbose = False, vverbose = False)

    # plot everything and save to file

    if not os.path.isdir('{}/{}/validations'.format(directory, HUC8)):
        os.mkdir('{}/{}/validations'.format(directory, HUC8))
    if not os.path.isdir(folder): os.mkdir(folder)

    output  = folder + '/calibration_report.csv'
    postprocessor.calculate_errors(verbose = False, output = output)

    output  = folder + '/snow'
    postprocessor.plot_snow(output = output, show = False)

    output  = folder + '/snowcalibration'
    postprocessor.plot_snowcalibration(output = output, show = False)

    output  = folder + '/monthlyhydrograph'
    postprocessor.plot_hydrograph(output = output, show = False)

    output  = folder + '/dayofyear'
    postprocessor.plot_dayofyear(output = output, show = False)

    output  = folder + '/averagewaterbudget'
    postprocessor.plot_waterbudget(output = output, show = False)

    output  = folder + '/waterbudgets'
    postprocessor.plot_allwaterbudgets(output = output, show = False)

    output  = folder + '/runoff'
    postprocessor.plot_runoff(output = output, show = False)

    output  = folder + '/storms'
    postprocessor.plot_storms(output = output, show = False)

    output  = folder + '/calibration'
    postprocessor.plot_calibration(output = output, show = False)

    postprocessor.close()
