# two_year_calibration.py
#
# David J. Lampert
#
# 06/29/2016
#
# shows how to use PyHSPF to calibrate and postprocess a model using every 
# two-year period for the calibration followed by validation over the entire
# 30-year period.
#
# import Python built-in modules

import os, pickle, datetime

# import PyHSPF classes

from pyhspf               import Postprocessor
from pyhspf.calibration   import AutoCalibrator

# paths to the HSPF/watershed data files

network     = 'D:'
destination = 'C:/HSPF_new'

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# NWIS gage id for the calibration

gageid = '05472500'

# start and end dates for the calibration simulations (1981 to 2001)

start_year = 1981
end_year   = 2011
interval = 2

# land use year to use for the model

landuseyear = 2001

# HSPF PERLND variables to use for calibration and inital values relative to 
# the PyHSPF defaults; these represent the "starting point" for the calibration
# variables that are then modified using the "path-of-steepest ascent" 
# approach to maximize the optimization parameter (these values are for the
# 30-year calibration so they make a good starting point for each 2-yearperiod.

variables = {'LZSN':   1.08,
             'UZSN':   1.16,
             'LZETP':  1.39,   
             'INFILT': 0.75,
             'INTFW':  1.16,
             'AGWRC':  0.99,
             'IRC':    0.79,
             'KVARY':  0.033,
             }

# optimization parameter 

optimization = 'Nash-Sutcliffe Efficiency'

# degrees of perturbation

perturbations = [2, 1, 0.5]
    
# parallel flag and number of processors to use (default uses them all)

parallel    = True
nprocessors = 5

# make sure the directory and file paths exist

if not os.path.isdir(network):
    print('error, directory {} does not exist (please update)'.format(network))
    raise
if not os.path.isdir(destination):
    print('error, directory ' +
          '{} does not exist (please update)'.format(destination))
    raise

# Because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below

if __name__ == '__main__': 

    # working directory for calibration simulations

    directory = '{}/{}/hspf'.format(destination, HUC8)

    # path to the baseline model

    filename = '{}/{}baseline'.format(directory, landuseyear)

    # directory for results

    results = '{}/{}/two_year'.format(destination, HUC8)
    
    if not os.path.isdir(results): os.mkdir(results)
    
    # iterate through each two-year period, calibrate the model, run across
    # the validation period, save the results
    
    year = start_year

    while year < end_year:

        # figure out the start date and end date and warmup period (1 year)
        
        start = datetime.datetime(year - 1, 1, 1)
        end   = datetime.datetime(year + interval, 1, 1)
        warmup = (datetime.datetime(year, 1, 1) - start).days

        # file path to place the calibrated model and results

        calibration = '{}/{}_{}'.format(results, year, year + 2)

        # path where the calibrated model will be saved/located
        
        calibrated = '{}/{}'.format(calibration, gageid)

        # make the directory for the calibration simulations

        if not os.path.isdir(calibration): os.mkdir(calibration)

        # make an instance of the autocalibrator to work with
        
        if not os.path.isfile(calibrated):
        
            calibrator = AutoCalibrator(filename, start, end, directory, 
                                        warmup = warmup, gageid = gageid,
                                        atemp = True, snow = True,
                                        hydrology = True)

            # calibrate the model and save it to the "calibrated" location

            calibrator.autocalibrate(calibrated,
                                     variables = variables, 
                                     optimization = optimization,
                                     perturbations = perturbations,
                                     parallel = parallel,
                                     nprocessors = nprocessors)

        # run a simulation to analyze calibration goodness of fit
        
        with open(calibrated, 'rb') as f: hspfmodel = pickle.load(f)

        # output variables

        targets = ['water_state', 
                   'reach_outvolume', 
                   'evaporation', 
                   'runoff', 
                   'groundwater',
                   'snow_state', 
                   'snowpack', 
                   'snowfall',
                   ]

        # get the comid of the gage

        d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
        comid = d[gageid]

        # build the input WDM file

        hspfmodel.build_wdminfile()

        # build the UCI and output WDM files

        hspfmodel.build_uci(targets, start, end, atemp = True, snow = True,
                            hydrology = True)

        # run it

        hspfmodel.run(verbose = True)

        # use the Postprocessor to analyze and save the results

        dates = start + datetime.timedelta(days = warmup), end

        postprocessor = Postprocessor(hspfmodel, dates, comid = comid)

        postprocessor.get_hspexp_parameters(verbose = False)
        postprocessor.plot_hydrograph(tstep = 'monthly', show = False,
                                      output = '{}/monthly'.format(calibration))
        postprocessor.plot_calibration(output = '{}/stats'.format(calibration),
                                       show = False)
        postprocessor.plot_runoff(tstep = 'daily', show = False,
                                  output = '{}/runoff'.format(calibration))
        output = '{}/calibration_report.csv'.format(calibration)
        postprocessor.calibration_report(output = output)
        postprocessor.plot_snow(output = '{}/snow'.format(calibration), 
                                show = False)
        postprocessor.plot_dayofyear(output ='{}/dayofyear'.format(calibration),
                                     show = False)
        postprocessor.plot_storms(season = 'all', show = False, 
                                  output = '{}/storms'.format(calibration))
    
        # have to close the WDM files to continue
    
        postprocessor.close()

        # run a validation simulation
        
        with open(calibrated, 'rb') as f: hspfmodel = pickle.load(f)

        # output variables

        targets = ['water_state', 
                   'reach_outvolume', 
                   'evaporation', 
                   'runoff', 
                   'groundwater',
                   ]

        # build the input WDM file

        hspfmodel.build_wdminfile()

        # build the UCI and output WDM files

        s = datetime.datetime(start_year - 1, 1, 1)
        e = datetime.datetime(end_year, 1, 1)
        w = (datetime.datetime(start_year, 1, 1) - s).days
        
        hspfmodel.build_uci(targets, s, e, atemp = True, snow = True,
                            hydrology = True)

        # run it

        hspfmodel.run(verbose = True)

        # use the Postprocessor to analyze and save the results

        dates = s + datetime.timedelta(days = w), e

        postprocessor = Postprocessor(hspfmodel, dates, comid = comid)

        postprocessor.get_hspexp_parameters(verbose = False)
        o = '{}/monthly_validation'.format(calibration)
        postprocessor.plot_hydrograph(tstep = 'monthly', show = False,
                                      output = o)
        o = '{}/stats_validation'.format(calibration)
        postprocessor.plot_calibration(output = o, show = False)
        o = '{}/validation_report.csv'.format(calibration)
        postprocessor.calibration_report(output = o)
        o = '{}/dayofyear_validation'.format(calibration)
        postprocessor.plot_dayofyear(output = o, show = False)
    
        # have to close the WDM files to continue
    
        postprocessor.close()

        year += interval
