#!/usr/bin/env python3
#
# Errors, Parameters, and Calibrator classes for HSPF model development
#
# David J. Lampert
#
# djlampert@gmail.com
#
# last updated: 07/23/2013

import os, pickle, time, shutil, math

from multiprocessing import Process, Queue, cpu_count

from pyhspf import Postprocessor

class SedimentCalibrator:
    """A class to use for sediment calibration for an HSPF model."""

    def __init__(self, temp = True, snow = True, hydrology = True, 
                 sediment = True):
        """Instantiate the calibrator."""

        # HSPF modules (note this is not a complete list)

        self.temp      = temp
        self.snow      = snow
        self.hydrology = hydrology
        self.sediment  = sediment

        # gage NWIS id to calibrate

        self.gageid = None

    def set_flowgage(self, gageid, upcomids = None):
        """Sets the gage for a calibration."""

        # set the gage id

        self.gageid = gageid

        # open up the base model

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        d = {v: k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
        self.gagecomid = d[gageid]

        # use the updown dictionary to find the upstream gages (if desired)

        self.upcomids = []

        if upcomids == 'gages':

            outlet = self.gagecomid

            for gagecomid in hspfmodel.subbasin_timeseries['flowgage']:

                # start at the gage and go downstream until reaching the 
                # downstream gage or the end of the watershed

                comid = gagecomid
                while hspfmodel.updown[comid] in hspfmodel.updown:

                    if (hspfmodel.updown[comid] == outlet and 
                        gagecomid not in self.upcomids): 
                        self.upcomids.append(gagecomid)
                    
                    comid = hspfmodel.updown[comid]

        elif upcomids is not None: self.upcomids += upcomids

    def set_vars(self, directory, HUC8, run_dates):
        """Set the variables in the base model."""

        self.run_dates     = run_dates
        self.directory     = directory
        self.HUC8          = HUC8
        self.basemodel     = '{}/{}/hspf/{}'.format(self.directory, self.HUC8, 
                                                    'basecase')

    def add_error_criteria(self, tot_error = 0.1, rec_error = 0.03, 
                           high_error = 0.15, low_error = 0.1, 
                           storm_error = 0.2, summer_storm_error = 0.5,
                           storm_peak_error = 0.5, season_error = 0.2):
        """Adds the error tolerances for the model."""

        self.error_criteria = Errors()
        self.error_criteria.add_directly(tot_error, rec_error, high_error, 
                                         low_error, storm_error, 
                                         summer_storm_error, storm_peak_error,
                                         season_error)

    def run_sediment(self, hspfmodel, dates, targets, 
                     overwrite = False, warmup = False, save_states = True):
        """Runs a simulation."""

        # save the run dates

        start, end = dates

        # add sediment to the existing hydrology model

        hspfmodel.add_sediment()

        self.adjust_parameters(hspfmodel)

        # build the input wdmfile

        hspfmodel.build_wdminfile()

        # warm up the model if needed

        if warmup:
            print('\nwarming up the model\n')
            hspfmodel.warmup(start, temp = self.temp, snow = self.snow,
                             hydrology = self.hydrology, sediment = True)

        # create the UCI file and the output WDM file

        hspfmodel.build_uci(targets, start, end, temp = self.temp,
                            snow = self.snow, hydrology = self.hydrology,
                            sediment = True)

        # run it

        hspfmodel.run(verbose = True)

    def set_sediment(self, hspfmodel, dates, targets):
        """Shortcut routine to skip the simulation and go straight to 
        postprocessing."""

        start, end = dates

        hspfmodel.add_sediment()

        self.adjust_parameters(hspfmodel)

        hspfmodel.wdminfile  = '{}/{}_in.wdm'.format(hspfmodel.filepath,
                                                     hspfmodel.filename)
        hspfmodel.ucifile    = '{}/{}.uci'.format(hspfmodel.filepath,
                                                  hspfmodel.filename)
        hspfmodel.wdmoutfile = '{}/{}_out.wdm'.format(hspfmodel.filepath,
                                                      hspfmodel.filename)
        hspfmodel.targets = targets

    def get_postprocessor(self, hspfmodel, dates, snowdata = None, 
                          verbose = True):
        """Postprocesses the data."""

        if verbose: print('postprocessing simulation results\n')

        gagedata = self.directory + '/%s/NWIS/%s'  % (self.HUC8, self.gageid)

        postprocessor = Postprocessor(hspfmodel, self.process_dates,
                                      comid = self.gagecomid,
                                      upcomids = self.upcomids)

        return postprocessor

    def check_mass_balance(self, hydrology_model, run = True, 
                           process_dates = None):
        """Looks at the mass balance across the headwaters."""

        if process_dates is None: self.process_dates = self.run_dates
        else:                     self.process_dates = process_dates

        # open up the default hspfmodel and create a list of HSPFModel 
        # instances to be simulated
        
        with open(hydrology_model, 'rb') as f: hspfmodel = pickle.load(f)

        hspfmodel.filename += 'sediment'
        hspfmodel.print_file = '{}/{}/hspf/sediment.out'.format(self.directory,
                                                                self.HUC8)


        # figure out the external targets needed

        targets = ['runoff', 'water_state', 'reach_sediment_in', 'reach_tss', 
                   'shear','reach_outvolume', 'detached_sediment',
                   'reach_solids',
                   'reach_sediment_out', 'erosion', 'reach_total_sediment']
        #targets = ['reach_sediment_in', 'reach_sediment_out', 'erosion',
        #           'reach_total_sediment']

        if run: self.run_sediment(hspfmodel, self.run_dates, targets)
        else:   self.set_sediment(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates,
                                               verbose = False)

        mass_balance = postprocessor.get_sediment_balance(self.gagecomid,
                                                          self.upcomids)

        self.close_postprocessor(postprocessor)
    
    def get_outletflows(self, hspfmodel):
        """Returns the time series for the outlet flow volumes as a list of
        datetime instances and a list of values."""

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates, 
                                               verbose = False)
        times   = postprocessor.get_timeseries(dates = self.process_dates, 
                                               tstep = 'hourly')
        volumes = postprocessor.get_reach_outvolume('{}'.format(self.gagecomid),
                                                    dates = self.run_dates)
        self.close_postprocessor(postprocessor)

        return times, volumes

    def plot_hydrograph(self, postprocessor, tstep = 'monthly', show = True, 
                        output = None):
        """Makes a hydrograph plot."""

        postprocessor.plot_hydrograph(tstep = tstep, show = show, 
                                      output = output)

    def plot_calibration(self, postprocessor, show = True, output = None):
        """Makes a plot of the calibration statistics."""

        postprocessor.plot_calibration(show = show, output = output)

    def plot_tss(self, postprocessor, show = False, output = None):
        """Makes a tss plot."""

        postprocessor.plot_tss(show = show, output = output)

    def plot_solids(self, postprocessor, show = False, output = None):
        """Makes a tss plot."""
        
        postprocessor.plot_solids(show = show, output = output)

    def plot_loading(self, postprocessor, show = False, output = None):
        """Makes a tss plot."""

        postprocessor.plot_sediment_loading(show = show, output = output)

    def plot_erosion(self, postprocessor, monthly = False, show = False, 
                     output = None):
        """Makes an erosion plot."""

        postprocessor.plot_erosion(tstep = 'daily', show = show, output =output)
        if monthly: 
            postprocessor.plot_erosion(tstep = 'monthly', show = show,
                                       output = output)

    def save(self, basemodel, run = True, process_dates = None):
        """Shows the figures of the output."""

        if process_dates is None: self.process_dates = self.run_dates
        else:                     self.process_dates = process_dates

        # open up the default hspfmodel and create a list of HSPFModel 
        # instances to be simulated

        with open(basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        hspfmodel.print_file = '{}/{}/hspf/sediment.out'.format(self.directory,
                                                                self.HUC8)
        hspfmodel.filename = 'sediment'

        # figure out the external targets needed

        targets = ['runoff', 'water_state', 'reach_sediment_in', 'reach_tss', 
                   'shear','reach_outvolume', 'detached_sediment',
                   'reach_solids',
                   'reach_sediment_out', 'erosion', 'reach_total_sediment']

        if run: self.run_sediment(hspfmodel, self.run_dates, targets)
        else:   self.set_sediment(hspfmodel, self.run_dates, targets)

        directory = '{}/{}/sediment'.format(self.directory, self.HUC8) 
        if not os.path.isdir(directory): os.mkdir(directory)

        directory = '{}/{}'.format(directory, self.gageid)
        if not os.path.isdir(directory): os.mkdir(directory)

        with open('{}/sedimentmodel'.format(directory), 'wb') as f:
            pickle.dump(hspfmodel, f)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates,
                                               verbose = False)

        self.plot_erosion(postprocessor, 
                          output = '{}/erosion'.format(directory)) 
        self.plot_tss(postprocessor,
                      output = '{}/tss'.format(directory))
        self.plot_solids(postprocessor,
                         output = '{}/solids'.format(directory))
        self.plot_loading(postprocessor, 
                          output = '{}/loading'.format(directory))

        self.close_postprocessor(postprocessor)

    def show_figures(self, basemodel, run = True, process_dates = None,
                     tss = False, erosion = False, solids = False, 
                     loading = False, calibration = False):
        """Shows the figures of the output."""

        if process_dates is None: self.process_dates = self.run_dates
        else:                     self.process_dates = process_dates

        # open up the default hspfmodel and create a list of HSPFModel 
        # instances to be simulated

        with open(basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        hspfmodel.print_file = '{}/{}/hspf/sediment.out'.format(self.directory,
                                                                self.HUC8)
        hspfmodel.filename += 'sediment'

        # figure out the external targets needed

        targets = ['runoff', 'water_state', 'reach_sediment_in', 'reach_tss', 
                   'shear','reach_outvolume', 'detached_sediment',
                   'reach_solids',
                   'reach_sediment_out', 'erosion', 'reach_total_sediment']

        #targets = ['reach_outvolume', 'water_state']

        #if tss:
        #    targets.append('reach_tss')
        #if erosion:                     
        #    targets.append('erosion')
        #    targets.append('detached_sediment')
        #    targets.append('runoff')
        #if solids: 
        #    targets.append('reach_solids')
        #    targets.append('shear')

        if run: self.run_sediment(hspfmodel, self.run_dates, targets)
        else:   self.set_sediment(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates,
                                               verbose = False)

        if erosion:     self.plot_erosion(postprocessor, show = True)
        if tss:         self.plot_tss(postprocessor, show = True)
        if solids:      self.plot_solids(postprocessor, show = True)
        if loading:     self.plot_loading(postprocessor, show = True)
        if calibration: self.plot_calibration(postprocessor, show = True)

        self.close_postprocessor(postprocessor)

    def close_postprocessor(self, postprocessor, save_file = None):
        """Saves the end states and closes up the postprocessor."""

        if save_file is not None: 
            postprocessor.close(save_states = save_file)
        else:           
            postprocessor.close()

    def adjust_parameters(self, hspfmodel):
        """hack way to adjust stuff for now."""

        # adjust the land segment parameters for sediment loadings

        for p in hspfmodel.perlnds: 
            p.KSER = 13.8 * p.KSER # primary storage parameter
            p.KRER = 0.5 * p.KRER # primary transport parameter
            #p.AFFIX = 0.9
            #p.SMPF = 1.
            p.DETS = 2.

        for r in hspfmodel.rchreses:
            r.BEDDEP = .2
            r.BEDWID = 1.
            r.Dsilt = 0.03
            r.Dclay = 0.01
            taucs = 8.
            r.TAUCSsilt = taucs
            r.TAUCSclay = r.TAUCSsilt
            taucd = 6.
            r.TAUCDsilt = taucd
            r.TAUCDclay = r.TAUCDsilt
            m = 0.1
            r.Msilt = m
            r.Mclay = m
            r.Wsilt = 0.01
            r.Wclay = 0.001
            #r.Mclay = 0.1
            #pass        
