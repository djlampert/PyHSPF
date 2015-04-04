#!/usr/bin/env python3
#
# forecaster.py
# Forecaster class for HSPF model forecasting or hindcasting
# David J. Lampert
# djlampert@gmail.com
#
# last updated: 01/11/2015

import os, sys, shutil, pickle, datetime, time, math

from pyhspf.core                      import HSPFModel
from pyhspf.core                      import Postprocessor
from pyhspf                           import hspf
from pyhspf.forecasting.forecastplots import plot_calibration
from pyhspf.forecasting.forecastplots import plot_dayofyear
from pyhspf.forecasting.forecastplots import plot_waterbudget

from shapefile                         import Reader, Writer
from pyhspf.preprocessing.vectorutils  import merge_shapes
from pyhspf.forecasting.gisplots     import plot_gage_subbasin
from pyhspf.forecasting.gisplots     import plot_gage_segments

class Forecaster:
    """A class to perform hindcasts for HSPF models."""

    def __init__(self, temp = True, snow = True, hydrology = True):
        """Instantiate the calibrator."""

        # HSPF modules (note this is not a complete list)

        self.temp      = temp
        self.snow      = snow
        self.hydrology = hydrology

        # gage NWIS id to calibrate

        self.gageid = None

    def is_integer(self, n):

        try: 
            int(n)
            return True
        except: 
            return False

    def set_vars(self, directory, HUC8, run_dates, basemodel, hindcast):
        """Set the variables in the base model."""

        self.run_dates = run_dates
        self.directory = directory
        self.HUC8      = HUC8
        self.basemodel = basemodel
        self.hindcast  = hindcast

    def set_flowgage(self, gageid, upcomids = None):
        """Sets the gage for a calibration."""

        # set the gage id

        self.gageid = gageid

        # open up the base model

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # make a dictionary to use to find the comid for each gage id

        d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
        self.gagecomid = d[gageid]

        # use the updown dictionary to find the upstream gages (if desired)

        self.upcomids = []

        if upcomids == 'gages':

            outlet = d[self.gageid]

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

    def build_NRCM_hindcast(self, directory, HUC8, run_dates, basemodel, 
                            filename = 'hindcast', overwrite = False, 
                            verbose = True, vverbose = False):
        """Builds an instance of the HSPFModel class and the resulting HSPF UCI
        file based on a particular directory structure for each of the needed
        files."""

        if verbose: print('building climate hindcast')

        self.basemodel = basemodel
        self.run_dates = run_dates

        start, end = run_dates

        year = start.year
    
        self.directory = directory
        self.HUC8      = HUC8

        # open it up

        with open(basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # change the filepath and filename

        d = os.path.dirname(hspf.__file__)
        messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(d)

        hspfmodel.filename = filename
        hspfmodel.messagepath = messagepath

        # add the raw meteorology data to the HSPFModel instance and 
        # generate hourly timeseries across the specified period

        # add the timeseries to the model and assign to the subbasins

        for subbasin in hspfmodel.subbasins:
            v = directory, HUC8, 'subbasinprecipitation', subbasin
            with open('{}/{}/NRCM/{}/{}'.format(*v), 'rb') as f:
                s, t, precip = pickle.load(f)

            i = int((start - s) / datetime.timedelta(minutes = t))
            prec = [p / 3 for p in precip[i:] for j in range(3)]

            hspfmodel.add_timeseries('precipitation', subbasin, start, prec)
            hspfmodel.assign_subbasin_timeseries('precipitation', 
                                                 subbasin, subbasin)

        v = directory, HUC8, 'watershedtimeseries'
        with open('{}/{}/NRCM/{}/hourlyPETs'.format(*v), 'rb') as f:
            PETs = pickle.load(f)

        # add the PET timeseries to the model--the landuse names from NASS
        # are sometimes different than assigned so make a dictionary to
        # keep track

        landuse_keys = {'Corn':          'cereals',
                        'Soybeans':      'legumes',
                        'Pasture/grass': 'pasture',
                        'Other grain':   'cereals',
                        'Hay/alfalfa':   'alfalfa',
                        'Water/wetland': 'wetlands',
                        'Fallow land':   'fallow',
                        'Forest':        'others',
                        'Developed':     'others',
                        'Impervious':    'others',
                        'Other':         'others',
                        }

        for l in PETs:
            s, t, PET = PETs[l]
            i = int((start - s) / datetime.timedelta(minutes = t))
            hspfmodel.add_timeseries('evaporation', l, start, 
                                     PET[i:], tstep = t)
            
        for k, val in landuse_keys.items():

            # assign to the landuse categories

            hspfmodel.assign_landuse_timeseries('evaporation', k, val)

        # watershed-wide time series (for snow)

        with open('{}/{}/NRCM/{}/hourlytemperature'.format(*v), 'rb') as f:
            s, t, temp = pickle.load(f)
                 
        # add it to the model

        i = int((start - s) / datetime.timedelta(minutes = t))
        hspfmodel.add_timeseries('temperature', hspfmodel.description, 
                                 start, temp[i:])

        # assign to the watershed

        hspfmodel.assign_watershed_timeseries('temperature', 
                                              hspfmodel.description)

        # dewpoint

        with open('{}/{}/NRCM/{}/dewpoint'.format(*v), 'rb') as f:
            s, t, dewpoint = pickle.load(f)

        # convert to hourly

        dewpoint = [t for t in dewpoint for i in range(24)]
        t = 60
            
        # add it to the model

        i = int((start - s) / datetime.timedelta(minutes = t))
        hspfmodel.add_timeseries('dewpoint', hspfmodel.description, 
                                 start, dewpoint[i:])

        # assign to the watershed

        hspfmodel.assign_watershed_timeseries('dewpoint',hspfmodel.description)

        # wind speed

        with open('{}/{}/NRCM/{}/wind'.format(*v), 'rb') as f:
            s, t, wind = pickle.load(f)

        # convert to hourly and from m/s to km/interval (km/hr)

        factor = 60 * t / 1000 / 24

        wind = [w * factor for w in wind for i in range(24)]
        t = 60

        # add it to the model

        i = int((start - s) / datetime.timedelta(minutes = t))
        hspfmodel.add_timeseries('wind', hspfmodel.description, start,wind[i:])

        # assign to the watershed

        hspfmodel.assign_watershed_timeseries('wind', hspfmodel.description)

        # solar radiation

        with open('{}/{}/NRCM/{}/hourlysolar'.format(*v), 'rb') as f:
            s, t, solar = pickle.load(f)

        # convert from W hr/m2 to langley/interval (= langley/hr)

        factor = 0.001434
        solar = [s * factor for s in solar]

        # add it to the model

        i = int((start - s) / datetime.timedelta(minutes = t))
        hspfmodel.add_timeseries('solar', hspfmodel.description, 
                                 start, solar[i:])
    
        # assign to the watershed

        hspfmodel.assign_watershed_timeseries('solar', hspfmodel.description)

        # dump the base model

        self.hindcast = hspfmodel.filepath + hspfmodel.filename

        with open(self.hindcast, 'wb') as f: pickle.dump(hspfmodel, f)

    def run_hydrology(self, hspfmodel, dates, targets, warmup = False):
        """Runs a simulation."""

        # save the run dates

        start, end = dates

        # build the input wdmfile

        hspfmodel.build_wdminfile()

        # warm up the model if needed

        if warmup:
            print('\nwarming up the model\n')
            hspfmodel.warmup(start, hydrology = True)

        # create the UCI file and the output WDM file

        hspfmodel.build_uci(targets, start, end, temp = self.temp,
                            snow = self.snow, hydrology = self.hydrology)

        # make a copy of the message file to prevent crashes

        new_message = '{}{}_msg.wdm'.format(hspfmodel.filepath, 
                                            hspfmodel.filename)

        shutil.copy(hspfmodel.messagepath, new_message)

        hspfmodel.messagepath = new_message

        # run it

        hspfmodel.run(verbose = True)

    def run_snow(self, hspfmodel, dates, targets):
        """Runs a simulation."""

        # save the run dates

        start, end = dates

        # build the input wdmfile

        hspfmodel.build_wdminfile()

        # create the UCI file and the output WDM file

        hspfmodel.build_uci(targets, start, end, temp = self.temp,
                            snow = self.snow, hydrology = False)

        # make a copy of the message file to prevent crashes

        new_message = '{}{}_msg.wdm'.format(hspfmodel.filepath, 
                                            hspfmodel.filename)

        shutil.copy(hspfmodel.messagepath, new_message)

        hspfmodel.messagepath = new_message

        # run it

        hspfmodel.run(verbose = True)

    def set_snow(self, hspfmodel, dates, targets = 
                 ['temperature', 'snowmelt', 'snowfall', 'snowpack', 
                  'snowcover', 'rain', 'ice']):
        """Shortcut routine to skip the simulation and go straight to 
        postprocessing."""

        start, end = dates

        hspfmodel.wdminfile  = (hspfmodel.filepath + '%s_in.wdm' % 
                                hspfmodel.filename)
        hspfmodel.ucifile    = (hspfmodel.filepath + '%s.uci' % 
                                hspfmodel.filename)
        hspfmodel.wdmoutfile = (hspfmodel.filepath + '%s_out.wdm' % 
                                hspfmodel.filename)

        hspfmodel.targets = targets

    def set_hydrology(self, hspfmodel, dates, targets):
        """Shortcut routine to skip the simulation and go straight to 
        postprocessing."""

        start, end = dates

        hspfmodel.wdminfile   = (hspfmodel.filepath + '%s_in.wdm' % 
                                 hspfmodel.filename)
        hspfmodel.ucifile     = (hspfmodel.filepath + '%s.uci' % 
                                 hspfmodel.filename)
        hspfmodel.wdmoutfile  = (hspfmodel.filepath + '%s_out.wdm' % 
                                 hspfmodel.filename)
        hspfmodel.messagepath = (hspfmodel.filepath + '%s_msg.wdm' % 
                                 hspfmodel.filename)

        hspfmodel.targets = targets

    def get_postprocessor(self, hspfmodel, dates, snowdata = None, 
                          verbose = True):
        """Postprocesses the data."""

        if verbose: print('postprocessing simulation results\n')

        start, tstep, data = hspfmodel.flowgages[self.gageid]
        end = start + datetime.timedelta(minutes = tstep) * len(data)

        if self.process_dates[0] < start or end < self.process_dates[1]:
            print('warning: missing data; processing only period of ' +
                  'available gage data\n')
            process_dates = start, end
        else:
            process_dates = self.process_dates

        postprocessor = Postprocessor(hspfmodel, process_dates,
                                      comid = self.gagecomid,
                                      upcomids = self.upcomids)

        return postprocessor
    
    def get_outletflows(self, hspfmodel):
        """Returns the time series for the outlet flow volumes as a list of
        datetime instances and a list of values."""

        postprocessor = self.get_postprocessor(hspfmodel, self.run_dates, 
                                               verbose = False)
        times = postprocessor.get_timeseries(dates = self.run_dates, 
                                             tstep = 'hourly')
        volumes = postprocessor.get_reach_timeseries('ROVOL', self.gagecomid,
                                                     dates = self.run_dates)
        self.close_postprocessor(postprocessor)

        return times, volumes

    def plot_snow(self, postprocessor, show = True, output = None):
        """Makes a snow plot."""

        postprocessor.plot_snow(show = show, output = output)

    def plot_snowcalibration(self, postprocessor, show = True, output = None):
        """Makes a snow plot."""

        postprocessor.plot_snowcalibration(show = show, output = output)

    def plot_hydrograph(self, postprocessor, tstep = 'monthly', show = True, 
                        output = None):
        """Makes a hydrograph plot."""

        postprocessor.plot_hydrograph(tstep = tstep, show = show, 
                                      output = output)

    def plot_calibration(self, basemodel, hindmodel, show = True, 
                         output = None):
        """Makes a plot of the calibration statistics."""

        # get the base simulation daily data

        process_dates = self.process_dates
        postprocessor = self.get_postprocessor(basemodel, process_dates, 
                                               verbose = False)
        comid = postprocessor.comid
        btimes, bflow = postprocessor.get_sim_flow(comid, tstep = 'daily')
        dtimes, oflow = postprocessor.get_obs_flow(tstep = 'daily')
        self.close_postprocessor(postprocessor)

        # get the hindcast simulation daily data

        postprocessor = self.get_postprocessor(hindmodel, process_dates,
                                               verbose = False)
        htimes, hflow = postprocessor.get_sim_flow(comid, tstep = 'daily')
        self.close_postprocessor(postprocessor)

        # deal with missing data

        d_hflow = [hflow[htimes.index(t)] 
                   for t, f in zip(dtimes, oflow) 
                   if t in htimes and f is not None]
        d_bflow = [bflow[btimes.index(t)] 
                   for t, f in zip(dtimes, oflow) 
                   if t in btimes and f is not None]
        d_oflow = [oflow[dtimes.index(t)] 
                   for t, f in zip(dtimes, oflow) 
                   if f is not None]

        # get the base model simulation monthly data

        postprocessor = self.get_postprocessor(basemodel, process_dates,
                                               verbose = False)
        btimes, bflow = postprocessor.get_sim_flow(comid, tstep = 'monthly')
        mtimes, oflow = postprocessor.get_obs_flow(tstep = 'monthly')
        self.close_postprocessor(postprocessor)

        # get the hindcast monthly data

        postprocessor = self.get_postprocessor(hindmodel, process_dates,
                                               verbose = False)
        htimes, hflow = postprocessor.get_sim_flow(comid, tstep = 'monthly')
        self.close_postprocessor(postprocessor)

        if mtimes[0].day != 1: mtimes, oflow = mtimes[1:], mflow[1:]

        # deal with missing data

        m_hflow = [hflow[htimes.index(t)] for t, f in zip(mtimes, oflow) 
                   if t in htimes and f is not None]
        m_bflow = [bflow[btimes.index(t)] for t, f in zip(mtimes, oflow) 
                   if t in btimes and f is not None]
        m_oflow = [f for t, f in zip(mtimes, oflow) if f is not None]

        desc = basemodel.description
        start = dtimes[0]
        end = dtimes[-1]
        plot_calibration(desc, start, end, d_oflow, d_bflow, d_hflow, 
                         m_oflow, m_bflow, m_hflow,
                         output = output, show = show)

    def plot_dayofyear(self, basemodel, hindmodel, show = True, output = None):
        """Plots the annual hydrograph for the two scenarios."""

        tstep = 'daily'
        process_dates = self.process_dates

        postprocessor = self.get_postprocessor(basemodel, process_dates,
                                               verbose = False)
        comid = postprocessor.comid
        upcomids = postprocessor.upcomids

        oprec  = postprocessor.get_precipitation(comid, upcomids = upcomids,
                                                 tstep = tstep)
        bevap  = postprocessor.get_evaporation(comid, upcomids = upcomids, 
                                              tstep = tstep)

        # get runoff area (km2)

        comids = postprocessor.get_upstream_comids(comid, upcomids = upcomids)
        area   = sum(postprocessor.get_subbasin_areas(comids))

        # convert to mm or in

        if   basemodel.units == 'Metric':  conv = 1000
        elif basemodel.units == 'English': conv = 43560 / 12

        # get the observed flows

        t, oflow = postprocessor.get_obs_flow(tstep = tstep)

        oflow = t, [f * 86400 / conv / area for f in oflow]
        
        # simulated runoff

        times, vols = postprocessor.get_reach_timeseries('ROVOL', comid,
                                                         tstep ='hourly')

        vols = postprocessor.aggregate_hourly_daily(vols)

        bflow = t, [v * conv / area for v in vols]

        self.close_postprocessor(postprocessor)

        postprocessor = self.get_postprocessor(hindmodel, process_dates,
                                               verbose = False)

        sprec  = postprocessor.get_precipitation(comid, upcomids = upcomids,
                                                 tstep = tstep)
        hevap  = postprocessor.get_evaporation(comid, upcomids = upcomids, 
                                               tstep = tstep)
       
        # simulated runoff

        times, vols = postprocessor.get_reach_timeseries('ROVOL', comid,
                                                         tstep ='hourly')

        vols = postprocessor.aggregate_hourly_daily(vols)

        hflow = t, [v * conv / area for v in vols]

        self.close_postprocessor(postprocessor)

        plot_dayofyear(basemodel.description, 
                       oprec, sprec, bflow, hflow, bevap, hevap,
                       oflow = oflow, output = output, show = show)

    def plot_waterbudget(self, postprocessor, show = True, output = None):

        if comid is None: comid = self.comid

        if dates is None: dates = self.start, self.end

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        # get runoff area (km2)

        area = sum(self.get_subbasin_areas(comids))

        # get the fluxes

        prec  = self.get_precipitation(comid, upcomids = upcomids,
                                       tstep = tstep, dates = dates)
        pet   = self.get_pet(comid = comid, upcomids = upcomids, tstep = tstep,
                             dates = dates)
        evap  = self.get_evaporation(comid, upcomids = upcomids, tstep = tstep,
                                     dates = dates)
        gw    = self.get_groundwater(comids, tstep = tstep, dates = dates)

        # converion factor

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 43560 / 12

        # simulated runoff

        times, vols = self.get_reach_timeseries('ROVOL', comid, tstep='hourly',
                                                dates = dates)

        # convert to daily

        times = self.get_timeseries(tstep = 'daily', dates = dates)
        vols  = self.aggregate_hourly_daily(vols)

        if tstep == 'monthly':
            
            times, vols = self.aggregate_daily_monthly(times, vols) 

        sflow = times, [v * conv / area for v in vols]
        
        plot_waterbudget(self.hspfmodel.description, prec, pet, evap, sflow,gw,
                         limits = limits, tstep = tstep, output = output, 
                         show = show)

        postprocessor.plot_waterbudget(show = show, output = output)

    def plot_allwaterbudgets(self, postprocessor, show = True, output = None):

        postprocessor.plot_allwaterbudgets(show = show, output = output)

    def save_hindcast(self, process_dates = None, run = True):
        """Saves all the parameters, errors, statistics,a nd graphs from the 
        calibration."""

        if process_dates is None:
            self.process_dates = self.run_dates
        else:                     
            self.process_dates = process_dates

        # set up the path to the calibrations directory

        folder = '{0}/{1}/hindcasts'.format(self.directory, self.HUC8)

        if not os.path.isdir(folder): os.mkdir(folder)

        # set up the path to the calibration for this gage

        folder = folder + '/{}'.format(self.gageid)

        if not os.path.isdir(folder): os.mkdir(folder)
            
        # open the hindcast model

        with open(self.basemodel, 'rb') as f: basemodel = pickle.load(f)
        with open(self.hindcast, 'rb') as f:  hindmodel = pickle.load(f)

        # figure out the external targets needed

        targets = ['reach_outvolume', 'groundwater', 'water_state', 
                   'evaporation', 
                   #'snow_state', 'snowpack', 'snowfall',
                   ]

        if run: self.run_hydrology(hindmodel, self.run_dates, targets)
        else:   self.set_hydrology(hindmodel, self.run_dates, targets)

        self.set_hydrology(basemodel, self.run_dates, targets)

        # plot everything and save to file

        output  = folder + '/calibration'
        self.plot_calibration(basemodel, hindmodel, output = output, 
                              show = False)

        output  = folder + '/dayofyear'
        self.plot_dayofyear(basemodel, hindmodel, output = output, 
                            show = False)

        with open(folder + '/hspfmodel',  'wb') as f: pickle.dump(hindmodel, f)

    def plot_gage_subbasin(self, hspfmodel, folder):
        """Makes a plot of the subbasin area."""

        subbasinfile  = '{}/subbasins'.format(folder)
        boundaryfile  = '{}/boundary'.format(folder)
        flowfile      = '{}/flowlines'.format(folder)
        combinedfile  = '{}/combined'.format(folder)
        watershedplot = '{}/watershed.png'.format(folder)

        # make a shapefile of the subbasins for the watershed

        f = '{0}/{1}/{1}subbasins'.format(self.directory, self.HUC8)
        for out in (subbasinfile, boundaryfile, flowfile, combinedfile):
            if not os.path.isfile(out + '.prj'):
                shutil.copy(f + '.prj', out + '.prj')

        if not os.path.isfile(subbasinfile + '.shp'):

            subshapes  = []
            subrecords = []
            for subbasin in hspfmodel.subbasins:

                f = '{0}/{1}/{2}/combined'.format(self.directory, self.HUC8, 
                                                  subbasin)
                s = Reader(f, shapeType = 5)

                subshapes.append(s.shape(0).points)
                subrecords.append(s.record(0))

            w = Writer(shapeType = 5)

            for field in s.fields:    w.field(*field)
            for record in subrecords: w.record(*record)
            for shape in subshapes:   w.poly(shapeType = 5, parts = [shape])

            w.save(subbasinfile)

        if not os.path.isfile(combinedfile + '.shp'):

            fshapes    = []
            frecords   = []
            for subbasin in hspfmodel.subbasins:
                f = '{0}/{1}/{2}/combined_flowline'.format(self.directory, 
                                                           self.HUC8, 
                                                           subbasin)
                r = Reader(f, shapeType = 3)

                fshapes.append(r.shape(0).points)
                frecords.append(r.record(0))

            w = Writer(shapeType = 3)

            for field in r.fields:  w.field(*field)
            for record in frecords: w.record(*record)
            for shape in fshapes:   w.poly(shapeType = 3, parts = [shape])

            w.save(combinedfile)

        # merge the shapes into a watershed

        if not os.path.exists(boundaryfile + '.shp'):

            merge_shapes(subbasinfile, outputfile = boundaryfile)

        # make a flowline file for the subbasins for the watershed

        if not os.path.isfile(flowfile + '.shp'):

            shapes  = []
            records = []
            for subbasin in hspfmodel.subbasins:
                f = '{0}/{1}/{2}/flowlines'.format(self.directory, 
                                                   self.HUC8, subbasin)
                r = Reader(f, shapeType = 3)
                for shape  in r.shapes():  shapes.append(shape.points)
                for record in r.records(): records.append(record)

            w = Writer(shapeType = 3)

            for field in r.fields: w.field(*field)
            for record in records: w.record(*record)
            for shape in shapes:   w.poly(shapeType = 3, parts = [shape])

            w.save(flowfile)

        if not os.path.isfile(watershedplot):

            plot_gage_subbasin(folder, self.HUC8, self.gageid, hspfmodel,
                               output = watershedplot)

    def plot_gage_landuse(self, hspfmodel, folder):
        """Makes plots of the landuse for a gage."""

        if hspfmodel.landuseyear is None: 
            subbasin = hspfmodel.subbasins[self.flowgages[self.gageid]]
            year = min(subbasin.landuse.keys())
        else:
            year = hspfmodel.landuseyear

        aggregate = 'aggregate.csv'
    
        # raw

        raw = '{0}/{1}landuse'.format(folder, self.gageid)
        if not os.path.isfile(raw + '.png'):
            plot_gage_segments(self.directory, self.HUC8, self.gageid, year, 
                               aggregate, raw, datatype = 'raw', 
                               overwrite = True)

        # subbasins

        subbasins = '{0}/{1}landsegments'.format(folder, self.gageid)
        if not os.path.isfile(subbasins + '.png'):
            plot_gage_segments(self.directory, self.HUC8, self.gageid, year, 
                               aggregate, subbasins, datatype = 'subbasins', 
                               overwrite = True)

    def close_postprocessor(self, postprocessor, save_file = None):
        """Saves the end states and closes up the postprocessor."""

        if save_file is not None: 
            postprocessor.close(save_states = save_file)
        else:           
            postprocessor.close()
