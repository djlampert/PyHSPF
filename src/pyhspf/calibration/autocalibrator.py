# autocalibrator.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Contains the AutoCalibrator class that can be used to calibrate a model.
# The class requires and HSPFModel class, start and end dates, and an output
# location to work in while running simulations. The primary function is
# autocalibrate, and it takes a list of HSPF variables, perturbations (as a
# percentage, optimization parameter, and flag for parallelization as 
# keyword arguments. The calibration routine can be summarized as follows:
#
#   1. Set up a series of simulations with a small perturbation to the current
#      parameter values for the parameters of interest
#   2. Make copies of the input HSPFModel and adjust the parameter values
#   3. Run the simulations and get the effect of the optimization parameter
#   4. Adjust the baseline parameter values if they improve performance
#   5. Repeat until a maximum is achieved.
#

# The class should be adaptable to other optimization parameters.

import os, pickle, datetime, time, numpy

from multiprocessing  import Pool, cpu_count
from pyhspf.core      import HSPFModel, WDMUtil
from .calibratormodel import CalibratorModel

class AutoCalibrator:
    """
    A class to use to autocalibrate an HSPF model.
    """

    def __init__(self, 
                 hspfmodel, 
                 start, 
                 end, 
                 output, 
                 comid = None,
                 gageid = None,
                 atemp = False,
                 snow = False,
                 hydrology = False,
                 submodel = None,
                 warmup = 30,
                 parameter_ranges = {'IRC':    (0.5,    2),
                                     'LZETP':  (0.2,  1.5),
                                     'DEEPFR': (0,      1),
                                     'LZSN':   (0.2,    2),
                                     'UZSN':   (0.2,   10),
                                     'INFILT': (0.01,  20),
                                     'INTFW':  (0.01,  2.),
                                     'AGWRC':  (0.5,    2),
                                     'KVARY':  (0,    0.1),
                                     'CCFACT': (1,     10),
                                     'MGMELT': (0,     25),
                                     },
                 ):

        self.hspfmodel        = hspfmodel
        self.submodel         = submodel
        self.start            = start
        self.end              = end
        self.output           = output
        self.gageid           = gageid
        self.comid            = comid
        self.atemp            = atemp
        self.snow             = snow
        self.hydrology        = hydrology
        self.warmup           = warmup
        self.parameter_ranges = parameter_ranges

    def create_submodel(self, 
                        filepath, 
                        name,
                        overwrite = True,
                        verbose = True,
                        ):
        """
        Creates a submodel of the source model to enhance performance.
        """

        if not os.path.isfile(filepath) or overwrite:
 
            if verbose: print('creating a submodel\n')

            with open(self.hspfmodel, 'rb') as f: hspfmodel = pickle.load(f)

            submodel = CalibratorModel()
            submodel.build_submodel(hspfmodel, self.comid, name = name)

            with open(filepath, 'wb') as f: pickle.dump(submodel, f)

        self.submodel = filepath

    def copymodel(self,
                  name,
                  verbose = True,
                  ):
        """
        Returns a copy of the HSPFModel.
        """
        
        if self.submodel is None: m = self.hspfmodel
        else:                     m = self.submodel

        with open(m, 'rb') as f: hspfmodel = pickle.load(f)

        hspfmodel.filename = name

        return hspfmodel

    def adjust(self, model, variable, adjustment):
        """
        Adjusts the values of the given parameter for all the PERLNDs in the
        watershed by the "adjustment." The adjustments can be defined as 
        values relative to the default (products) or absolute values (sums).
        """ 

        if variable == 'LZSN':
            for p in model.perlnds: p.LZSN   *= adjustment
        if variable == 'UZSN':
            for p in model.perlnds: p.UZSN   *= adjustment
        if variable == 'LZETP':
            for p in model.perlnds: p.LZETP  *= adjustment
        if variable == 'INFILT':
            for p in model.perlnds: p.INFILT *= adjustment
        if variable == 'INTFW':
            for p in model.perlnds: p.INTFW  *= adjustment
        if variable == 'IRC':
            for p in model.perlnds: p.IRC    *= adjustment
        if variable == 'AGWRC':
            for p in model.perlnds: p.AGWRC  *= adjustment
        if variable == 'KVARY':
            for p in model.perlnds: p.KVARY  = max(0, p.KVARY + adjustment)
        if variable == 'DEEPFR':
            for p in model.perlnds: p.DEEPFR += adjustment
        if variable == 'CCFACT':
            for o in model.perlnds + model.implnds: 
                o.CCFACT = min(10, max(1, o.CCFACT + adjustment))
        if variable == 'MGMELT':
            for o in model.perlnds + model.implnds: 
                o.MGMELT = min(25, max(0, o.MGMELT + adjustment))           
    
    def run(self, 
            model,
            targets = ['reach_outvolume'],
            verbose = False,
            ):
        """
        Creates a copy of the base model, adjusts a parameter value, runs
        the simulation, calculates and returns the perturbation.
        """

        # build the input files and run

        model.build_wdminfile()

        if self.submodel is None:

            model.build_uci(targets, self.start, self.end, atemp = self.atemp,
                            snow = self.snow, hydrology = self.hydrology)

        else:

            model.build_uci(self.comid, self.start, self.end, 
                            atemp = self.atemp, snow = self.snow, 
                            hydrology = self.hydrology)

        model.run(verbose = verbose)

        # regression period

        dates = self.start + datetime.timedelta(days = self.warmup), self.end

        # use WDMUtil to get the simulated values

        wdm = WDMUtil()

        f = '{}_out.wdm'.format(model.filename)

        wdm.open(f, 'r')
        dsns   = wdm.get_datasets(f)
        staids = [wdm.get_attribute(f, n, 'STAID') for n in dsns]

        data = wdm.get_data(f, dsns[staids.index(self.comid)], 
                            start = dates[0], end = dates[1])

        wdm.close(f)

        if model.units == 'Metric': conv = 10**6
        else:                       conv = 43560

        # the submodel is daily, full model is hourly

        if self.submodel is None: 

            sflows = [sum(data[i:i+24]) * conv / 86400
                      for i in range(0, len(data) - 23, 24)]
            
        else:

            sflows = [d * conv / 86400 for d in data]

        stimes = [self.start + i * datetime.timedelta(days = 1)
                  for i in range(self.warmup, (self.end - self.start).days)]

        otimes = self.otimes
        oflows = self.oflows

        # remove points with missing data from both simulated and oberved flows

        sflows = [sflows[stimes.index(t)] 
                  for t, f in zip(otimes, oflows) 
                  if t in stimes and f is not None]
        oflows = [oflows[otimes.index(t)] 
                  for t, f in zip(otimes, oflows) 
                  if f is not None]

        # return the appropriate performance metric

        if self.optimization == 'Nash-Sutcliffe Product':

            # daily log flows

            log_o = [numpy.log(f) for f in oflows]
            log_s = [numpy.log(f) for f in sflows]

            logdNS = (1 - sum((numpy.array(log_s) - numpy.array(log_o))**2) /
                      sum((numpy.array(log_o) - numpy.mean(log_o))**2))

            # daily NS

            dNS  = (1 - sum((numpy.array(sflows) - numpy.array(oflows))**2) /
                    sum((numpy.array(oflows) - numpy.mean(oflows))**2))

            return dNS * logdNS

        elif self.optimization == 'Nash-Sutcliffe Efficiency': 

            # daily NS

            dNS  = (1 - sum((numpy.array(sflows) - numpy.array(oflows))**2) /
                    sum((numpy.array(oflows) - numpy.mean(oflows))**2))

            return dNS

        else:

            print('error: optimization parameter ' +
                  '{} not recognized'.format(self.optimization))
            raise

    def simulate(self, simulation):
        """
        Performs a simulation and returns the optimization value.
        """

        name, perturbation, adjustments = simulation

        # create a copy of the original model to modify

        filename = '{}/{}{:4.3f}'.format(self.output, name, perturbation)

        model = self.copymodel(filename)
                                         
        # adjust the values of the parameters

        for variable, adjustment in zip(self.variables, adjustments):
            self.adjust(model, variable, adjustment)

        # run and pass back the result
                  
        print('running', name, 'perturbation')
        return self.run(model)

    def perturb(self, 
                parallel,
                nprocessors,
                timeout = 300,
                verbose = True,
                ):
        """
        Performs the perturbation analysis.
        """

        if verbose:
            st = time.time()
            if parallel:
                print('perturbing the model in parallel\n')
            else:
                print('perturbing the model serially\n')

        # adjust the parameter values for each variable for each simulation

        its = range(len(self.variables)), self.variables, self.perturbations
        adjustments = []
        for i, v, p in zip(*its):
            adjustment = self.values[:]
            adjustment[i] += p
            adjustments.append(adjustment)
                                 
        # run a baseline simulation and perturbation simulations for 
        # each of calibration variables

        its = self.variables, self.perturbations, adjustments
        simulations = ([['baseline', 0, self.values]] + 
                       [[v, p, a] for v, p, a in zip(*its)])

        if parallel:

            if nprocessors is None: n = cpu_count()
            else:                   n = nprocessors

            try: 

                # create a pool of workers and try parallel

                with Pool(n, maxtasksperchild = 4 * cpu_count()) as p:
                    results = p.map_async(self.simulate, simulations)
                    optimizations = results.get(timeout = timeout)

            except:

                print('error: parallel calibration failed\n')
                print('last values of calibration variables:\n')
                for i in zip(self.variables, self.values): print(*i)
                raise RuntimeError

        else:

            # run the simulations to get the optimization parameter values

            optimizations = [self.simulate(s) for s in simulations]

        if verbose: 

            print('\ncompleted perturbation in ' +
                  '{:.1f} seconds\n'.format(time.time() - st))

        # calculate the sensitivities for the perturbations

        sensitivities = [o - optimizations[0] for o in optimizations[1:]]

        # save the current value of the optimization parameter

        self.value = optimizations[0]

        return sensitivities

    def get_default(self, variable):
        """Gets the default value of the perturbation for the variable.
        The defaults are based on experience with parameter sensitivity."""

        if   variable == 'LZSN':   return 0.05
        elif variable == 'UZSN':   return 0.10
        elif variable == 'LZETP':  return 0.02
        elif variable == 'INFILT': return 0.02
        elif variable == 'INTFW':  return 0.01
        elif variable == 'IRC':    return 0.02
        elif variable == 'AGWRC':  return 0.005
        elif variable == 'KVARY':  return 0.002
        elif variable == 'DEEPFR': return 0.01
        elif variable == 'CCFACT': return 0.2
        elif variable == 'MGMELT': return 0.2
        else:
            print('error: unknown variable specified\n')
            raise

    def check_variables(self):
        """
        User-defined check on the values of the variables to ensure 
        the calibrated values stay within the limits.
        """

        for i in range(len(self.variables)):

            variable = self.variables[i]
            value    = self.values[i]
            mi, ma   = self.parameter_ranges[variable]
            
            if value < mi:
                its = variable, value, mi
                print('warning: current value of ' +
                      '{} ({}) is below minimum ({})'.format(*its))
                self.values[i] = mi
            if value > ma:
                its = variable, value, ma
                print('warning: current value of ' +
                      '{} ({}) is above maximum ({})'.format(*its))
                self.values[i] = ma

    def optimize(self, 
                 parallel, 
                 nprocessors,
                 ):
        """
        Optimizes the objective function for the parameters.
        """

        # set the current value of the optimization parameter

        current = self.value - 1

        # iterate through positive and negative perturbations of the 
        # calibration parameters until there is no improvement

        t1 = 'increasing {:6s} {:>5.1%} increases {} {:7.4f}'
        t2 = 'decreasing {:6s} {:>5.1%} increases {} {:7.4f}'

        while current < self.value:

            # update the current value of the optimization parameter

            current = self.value

            # set the current values of the calibration parameters
            
            values = self.values[:]

            print('\ncurrent optimization value: {:4.3f}\n'.format(self.value))

            # perturb the values positively

            positives = self.perturb(parallel, nprocessors)

            # perturb the values negatively

            self.perturbations = [-p for p in self.perturbations]
            negatives = self.perturb(parallel, nprocessors)

            # reset the perturbations to positive

            self.perturbations = [-p for p in self.perturbations]

            # iterate through the calibration variables and update their
            # values positively or negatively if they increase the value
            # of the optimization parameter

            for i in range(len(self.values)):

                p = positives[i]
                n = negatives[i]
                d = self.perturbations[i]

                # see if positive change increases optimization

                if p > 0 and p > n:

                    its = self.variables[i], d, self.optimization, p
                    print(t1.format(*its))
                    self.values[i] = round(self.values[i] + d, 3)

                elif n > 0:

                    its = self.variables[i], d, self.optimization, n
                    print(t2.format(*its))
                    self.values[i] = round(self.values[i] - d, 3)

            # make sure variables are within bounds

            self.check_variables()

            # show progress

            print('\ncalibration values relative to default:\n')
            for variable, adjustment in zip(self.variables, self.values):
                print('{:6s} {:5.3f}'.format(variable, adjustment))

        # since the last iteration made the fit worse, reset the values of 
        # the calibration parameters to the previous iteration

        self.values = values[:]

    def autocalibrate(self, 
                      output,
                      variables = {'LZSN':   1.,
                                   'UZSN':   1.,
                                   'LZETP':  1.,
                                   'INFILT': 1.,
                                   'INTFW':  1.,
                                   'IRC':    1.,
                                   'AGWRC':  1.,
                                   },
                      optimization = 'Nash-Sutcliffe Efficiency',
                      perturbations = [2, 1, 0.5],
                      submodel = True,
                      parallel = True,
                      nprocessors = None,
                      ):
        """
        Autocalibrates the hydrology for the hspfmodel by modifying the 
        values of the HSPF PERLND parameters contained in the vars list.
        """

        # open up the base model

        with open(self.hspfmodel, 'rb') as f: hspfmodel = pickle.load(f)

        # find the comid of the calibration gage

        if self.comid is None and self.gageid is not None:

            print('looking up the comid for gage {}\n'.format(self.gageid))

            # make a dictionary to use to find the comid for each gage id

            d = {v:k 
                 for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
            self.comid = d[self.gageid]

        elif self.comid is None:

            print('error, no calibration gage specified')
            raise

        # get the calibration data

        s, tstep, data = hspfmodel.flowgages[self.gageid]

        # find the indices for the calibration

        i     = (self.start - s).days + self.warmup
        j     = (self.end   - s).days
        n     = (self.end - self.start).days - self.warmup
        delta = datetime.timedelta(days = 1)

        if hspfmodel.units == 'Metric': conv = 0.3048**3
        else:                           conv = 1

        self.oflows = [d * conv for d in data[i:j]]
        self.otimes = [self.start + (i + self.warmup) * delta for i in range(n)]

        # create a submodel to improve performance

        if submodel:

            filepath = '{}/submodel'.format(self.output)
            self.create_submodel(filepath, self.comid)

        # set up the current values of the variables, the amount to perturb
        # them by in each iteration, and the optimization parameter

        self.variables    = [v for v in variables]
        self.values       = [variables[v] for v in variables]
        self.optimization = optimization

        # current value of the optimization parameter

        self.value = -10 

        # perturb until reaching a maximum (start with large perturbations)

        print('attempting to calibrate {}'.format(self.hspfmodel))

        for p in perturbations:
            self.perturbations = [p * self.get_default(v) for v in variables]
            self.optimize(parallel, nprocessors)

        print('\noptimization complete, saving model\n')

        # set the submodel to None to save the full model

        self.submodel = None

        model = self.copymodel(output)
                                 
        # adjust the values of the parameters

        for variable, adjustment in zip(self.variables, self.values):
            self.adjust(model, variable, adjustment)

        # adjust the filename

        with open(output, 'wb') as f: pickle.dump(model, f)
