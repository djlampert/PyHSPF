#!/usr/bin/env python3
#
# Errors, Parameters, and Calibrator classes for HSPF model development
#
# David J. Lampert
#
# djlampert@gmail.com
#
# last updated: 05/01/2014

import os, sys, shutil, pickle, datetime, time, math

from multiprocessing import Process, Queue, cpu_count

from pyhspf import HSPFModel
from pyhspf import Postprocessor
from pyhspf import hspf

class HydrologyErrors:
    """A class to store statistics for calibrating an HSPF model."""

    def add_directly(self, total_error, recession_error, high_error, low_error,
                     storm_vol_error, summer_storm_error, storm_peak_error,
                     season_error, dr2 = 0, logdr2 = 0, dNS = 0, logdNS = 0,
                     mr2 = 0, logmr2 = 0, mNS = 0, logmNS = 0):
        """Add the errors manually."""

        self.total_error        = total_error
        self.recession_error    = recession_error
        self.high_error         = high_error
        self.low_error          = low_error
        self.storm_vol_error    = storm_vol_error
        self.summer_storm_error = summer_storm_error
        self.storm_peak_error   = storm_peak_error
        self.season_error       = season_error
        self.dr2                = dr2
        self.logdr2             = logdr2
        self.dNS                = dNS
        self.logdNS             = logdNS
        self.mr2                = mr2
        self.logmr2             = logmr2
        self.mNS                = mNS
        self.logmNS             = logmNS
        self.calibration        = logdNS * dNS

    def add_from_processor(self, processor):
        """Get the errors from the postprocessor."""

        self.total_error        = processor.total_error
        self.recession_error    = processor.recession_error
        self.high_error         = processor.high_error
        self.low_error          = processor.low_error
        self.storm_vol_error    = processor.storm_vol_error
        self.summer_storm_error = processor.summer_storm_error
        self.storm_peak_error   = processor.storm_peak_error
        self.season_error       = processor.season_error

        dr2, logdr2, dNS, logdNS, mr2, logmr2, mNS, logmNS = \
            processor.regression

        self.dr2                = dr2
        self.logdr2             = logdr2
        self.dNS                = dNS
        self.logdNS             = logdNS
        self.mr2                = mr2
        self.logmr2             = logmr2
        self.mNS                = mNS
        self.logmNS             = logmNS
        self.calibration        = logdNS * dNS

class HydrologyParameters:
    """A class to store the hydrology calibration parameters from an model."""

    def __init__(self, ifraction = 0.5, evap = 1., ccfact = 1., lzetp = 1., 
                 lzsn = 1., uzsn = 1., intfw = 1, infilt = 1., agwrc = 0.95, 
                 kvary = 0., deepfr = 0., irc = 0.6, ftable = 1.):
        """Adds the calibration parameters for the model."""

        self.ifraction  = ifraction # developed land impervious fraction
        self.evap       = evap      # potential to pan ET
        self.ccfact     = ccfact    # convection factor (snow)
        self.lzetp      = lzetp     # lower zone evapotranspiration (ratio)
        self.lzsn       = lzsn      # lower zone storage (ratio)
        self.uzsn       = uzsn      # upper zone storage (ratio)
        self.intfw      = intfw     # interflow inflow rate (ratio)
        self.infilt     = infilt    # infiltration to groundwater (system-wide)
        self.agwrc      = agwrc     # groundwater recession const (system-wide)
        self.kvary      = kvary     # groundwater recession parm  (system-wide)
        self.deepfr     = deepfr    # deep groundwater recharge (system-wide)
        self.irc        = irc       # interflow recession constant (system-wide)
        self.ftable     = ftable    # ftable flow (ratio)

    def copy(self):
        """Creates and returns a deep copy of this instance."""

        p = HydrologyParameters()

        p.ifraction  = self.ifraction
        p.evap       = self.evap     
        p.ccfact     = self.ccfact
        p.lzetp      = self.lzetp    
        p.lzsn       = self.lzsn     
        p.uzsn       = self.uzsn     
        p.intfw      = self.intfw    
        p.infilt     = self.infilt   
        p.agwrc      = self.agwrc    
        p.kvary      = self.kvary
        p.deepfr     = self.deepfr   
        p.irc        = self.irc      
        p.ftable     = self.ftable

        return p

class HydrologyCalibrator:
    """A class to use in calibrating an HSPF model."""

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

    def set_flowgage(self, gageid, upcomids = None):
        """Sets the gage for a calibration."""

        # set the gage id

        self.gageid = gageid

        # open up the base model

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # make a dictionary to use to find the comid for each gage id

        d = {v: k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
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

    def set_vars(self, directory, HUC8, run_dates):
        """Set the variables in the base model."""

        self.run_dates     = run_dates
        self.directory     = directory
        self.HUC8          = HUC8
        self.basemodel     = '{}/{}/hspf/{}'.format(self.directory, self.HUC8, 
                                                    'basecase')

    def build_hspfmodel(self, directory, HUC8, run_dates, filename = 'basecase',
                        landyear = 2001, overwrite = False, verbose = True, 
                        vverbose = False):
        """Builds an instance of the HSPFModel class and the resulting HSPF UCI
        file based on a particular directory structure for each of the needed
        files."""

        self.run_dates = run_dates

        start, end = run_dates

        year = start.year
    
        self.directory = directory
        self.HUC8      = HUC8

        v = directory, HUC8

        watershed     = '{}/{}/watershed'.format(*v)
        hspfdirectory = '{}/{}/hspf'.format(*v)
        if not os.path.isdir(hspfdirectory): os.mkdir(hspfdirectory)

        self.basemodel = '{}/{}/hspf/{}'.format(directory, HUC8, filename)

        # build the model if necessary

        if not os.path.exists(self.basemodel) or overwrite:

            if verbose: print('building HSPF model\n')

            with open(watershed, 'rb') as f: w = pickle.load(f)

            hspfmodel = HSPFModel()
            hspfmodel.build_from_watershed(w, filename, landuseyear = landyear,
                                           directory = '{}/{}/hspf'.format(*v),
                                           verbose = vverbose)

            if self.temp: hspfmodel.add_temp()

            if self.snow:

                # add the initial conditions for snow depth using the data if
                # it is available

                snowdata = '{}/{}/snow/snow'.format(*v)

                if os.path.isfile(snowdata):

                    with open(snowdata, 'rb') as f: 
                        snowstations = pickle.load(f)
                
                        depths = [s.get_depth(run_dates[0]) 
                                  for s in snowstations.values()
                                  if s.get_depth(run_dates[0]) is not None]
                        
                    depth = sum(depths) / len(depths)

                else: depth = 0.

                hspfmodel.add_snow(depth = depth)

            if self.hydrology: hspfmodel.add_hydrology()

            # add the calibration data to the model

            w = directory, HUC8
            with open('{}/{}/snow/snowfall'.format(*w), 'rb') as f:
                times, falls = pickle.load(f)
                hspfmodel.add_timeseries('snowfall', hspfmodel.description,
                                         times[0], falls, tstep = 1440)
                hspfmodel.assign_watershed_timeseries('snowfall', 
                                                      hspfmodel.description)

            with open('{}/{}/snow/snowdepth'.format(*w),'rb') as f:
                times, depths = pickle.load(f)
                hspfmodel.add_timeseries('snowdepth', hspfmodel.description,
                                         times[0], depths, tstep = 1440)
                hspfmodel.assign_watershed_timeseries('snowfall', 
                                                      hspfmodel.description)

            # add the timeseries

            with open('{}/{}/NWIS/dailydischarges'.format(*w), 'rb') as f:
                gagestations = pickle.load(f)
                for k, v in gagestations.items():
                    ts, flows = v
                    hspfmodel.add_timeseries('flowgage', k, ts[0], flows, 
                                             tstep = 1440)

            # assign them to the correct comid

            gagefolder = '{}/{}/NWIS'.format(*w)
            for n in os.listdir(gagefolder):

                if self.is_integer(n):
                    gagedata = '{}/{}'.format(gagefolder, n)
                    with open(gagedata, 'rb') as f: g = pickle.load(f)
                    hspfmodel.assign_subbasin_timeseries('flowgage', g.comid, 
                                                         g.gageid)

            # add the water quality data

            with open('{}/{}/NWIS/waterquality'.format(*w), 'rb') as f:
                waterquality = pickle.load(f)
                for k, v in waterquality.items():
                    hspfmodel.waterquality[k] = v

            # add the raw meteorology data to the HSPFModel instance and 
            # generate hourly timeseries for the across the specified period
       
            # add the timeseries to the model and assign to the subbasins

            for subbasin in hspfmodel.subbasins:
                v = directory, HUC8, 'subbasinprecipitation', subbasin
                with open('{}/{}/{}/{}'.format(*v), 'rb') as f:
                    s, t, precip = pickle.load(f)
                i = int((start - s) / datetime.timedelta(minutes = t))
                hspfmodel.add_timeseries('precipitation', subbasin, 
                                         start, precip[i:])
                hspfmodel.assign_subbasin_timeseries('precipitation', 
                                                     subbasin, subbasin)

            v = directory, HUC8, 'watershedtimeseries'
            with open('{}/{}/{}/hourlyPETs'.format(*v), 'rb') as f:
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

            with open('{}/{}/{}/hourlytemperature'.format(*v), 'rb') as f:
                s, t, temp = pickle.load(f)
                 
            # add it to the model

            i = int((start - s) / datetime.timedelta(minutes = t))
            hspfmodel.add_timeseries('temperature', hspfmodel.description, 
                                     start, temp[i:])

            # assign to the watershed

            hspfmodel.assign_watershed_timeseries('temperature', 
                                                  hspfmodel.description)

            # dewpoint

            with open('{}/{}/{}/dewpoint'.format(*v), 'rb') as f:
                s, t, dewpoint = pickle.load(f)

            # convert to hourly

            dewpoint = [t for t in dewpoint for i in range(24)]
            t = 60
            
            # add it to the model

            i = int((start - s) / datetime.timedelta(minutes = t))
            hspfmodel.add_timeseries('dewpoint', hspfmodel.description, 
                                     start, dewpoint[i:])

            # assign to the watershed

            hspfmodel.assign_watershed_timeseries('dewpoint', 
                                                  hspfmodel.description)

            # wind speed

            with open('{}/{}/{}/wind'.format(*v), 'rb') as f:
                s, t, wind = pickle.load(f)

            # convert to hourly and from m/s to km/interval (km/hr)

            factor = 60 * t / 1000 / 24

            wind = [w * factor for w in wind for i in range(24)]
            t = 60

            # add it to the model

            i = int((start - s) / datetime.timedelta(minutes = t))
            hspfmodel.add_timeseries('wind', hspfmodel.description, 
                                     start, wind[i:])

            # assign to the watershed

            hspfmodel.assign_watershed_timeseries('wind', 
                                                  hspfmodel.description)

            # solar radiation

            with open('{}/{}/{}/hourlysolar'.format(*v), 'rb') as f:
                s, t, solar = pickle.load(f)

            # convert from W hr/m2 to langley/interval (= langley/hr)

            factor = 0.001434
            solar = [s * factor for s in solar]

            # add it to the model

            i = int((start - s) / datetime.timedelta(minutes = t))
            hspfmodel.add_timeseries('solar', hspfmodel.description, 
                                     start, solar[i:])

            # assign to the watershed

            hspfmodel.assign_watershed_timeseries('solar', 
                                                  hspfmodel.description)

            # dump the base model

            with open(self.basemodel, 'wb') as f: pickle.dump(hspfmodel, f)

    def create_submodel(self, verbose = True, vverbose = False):
        """Builds a submodel from an HSPF model for a watershed with an outlet 
        at "comid" that begins at all the elements of "upcomids."  This enables
        analysis within the watershed if data are available for comparison."""

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        comid      = self.gagecomid
        upcomids   = self.upcomids
        filename   = hspfmodel.filename + comid
        picklefile = hspfmodel.filepath + filename

        submodel = HSPFModel()
        submodel.build_from_existing(hspfmodel, filename, directory = 
                                     hspfmodel.filepath[:-1],
                                     verbose = vverbose)

        # find the subbasins between the outlet and the upstream comids and
        # store in an updown dictionary

        updown = {comid: 0}

        current = 0

        while current != len(updown):

            # see if the current length changes to check if done

            current = len(updown)

            # iterate throught the subbasins and see if any need to be added

            for up, down in hspfmodel.updown.items():

                if (up not in updown   and   # not already there
                    up not in upcomids and   # between the boundaries
                    down in updown):         # downstream is there
                    
                    updown[up] = down
        
        # overwrite the old updown dictionary

        submodel.updown = updown

        # overwrite the inlets and outlets

        submodel.inlets  = [hspfmodel.updown[c] for c in upcomids]
        submodel.outlets = [comid]

        # overwrite the old subbasin dictionary

        submodel.subbasins = {comid: subbasin for comid, subbasin in 
                              submodel.subbasins.items() if comid in updown}

        # build with the updated model subbasin info

        submodel.build()

        # add in the modules

        if self.temp: submodel.add_temp()

        if self.snow: 
            
            densities = [o.RDENPF 
                         for o in hspfmodel.perlnds + hspfmodel.implnds]
            depths    = [o.packsnow / o.RDENPF 
                         for o in hspfmodel.perlnds + hspfmodel.implnds]

            depth   = sum(depths) / len(depths)
            density = sum(densities) / len(densities)

            submodel.add_snow(depth = depth, density = density)            

        if self.hydrology: submodel.add_hydrology()
        
        # add the flowgage data to the model

        for identifier in hspfmodel.flowgages:
            if identifier == self.gageid:
                start_date, tstep, data = hspfmodel.flowgages[identifier]
                submodel.add_timeseries('flowgage', identifier, start_date, 
                                        data, tstep = tstep)

        # add the watershed time series dictionaries for the model

        timeseries = {'inflow':        hspfmodel.inflows,
                      'temperature':   hspfmodel.temperatures,
                      'dewpoint':      hspfmodel.dewpoints,
                      'wind':          hspfmodel.windspeeds,
                      'solar':         hspfmodel.solars,
                      'snowfall':      hspfmodel.snowfalls,
                      'snowdepth':     hspfmodel.snowdepths,
                      }

        for tstype, d in timeseries.items():
            for identifier in d: 
                start_date, tstep, data = d[identifier]
                submodel.add_timeseries(tstype, identifier, start_date, data, 
                                        tstep = tstep)

        # add the subbasin timeseries as needed

        for identifier in hspfmodel.precipitations:
            if identifier in submodel.subbasins.keys():
                start_date, tstep, data = hspfmodel.precipitations[identifier]
                submodel.add_timeseries('precipitation', identifier, start_date,
                                        data, tstep = tstep)

        # add the landuse timeseries as needed

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

        ltypes = [landuse_keys[i] for i in hspfmodel.landuse]

        for identifier in hspfmodel.evaporations:
            if identifier in ltypes:
                start_date, tstep, data = hspfmodel.evaporations[identifier]
                submodel.add_timeseries('evaporation', identifier, start_date,
                                        data, tstep = tstep)

        # add the influent flows as needed
             
        for upcomid in upcomids:

            # find the upstream gage number

        
            upgage  = [v for k, v in 
                       hspfmodel.subbasin_timeseries['flowgage'].items() 
                       if k == upcomid][0]
            incomid = hspfmodel.updown[upcomid]

            # find the outlet flows from the previous upstream calibration

            t = (self.directory, self.HUC8, upgage)
            flowfile = '{}/{}/calibrations/{}/outletflows'.format(*t)
            
            # get the time series and add it to the model

            if not os.path.isfile(flowfile): 
                raise RuntimeError('warning: upstream calibration of gage ' +
                                   '{} does not exist\n'.format(upgage))
            with open(flowfile, 'rb') as f: times, data = pickle.load(f)

            tstep = math.ceil((times[1] - times[0]).total_seconds() / 60)

            submodel.add_timeseries('inflow', '{}'.format(incomid), times[0], 
                                    data, tstep = tstep)

            # assign the inflows from upstream to any subbasins

            otype = 'Reach'

            submodel.assign_operation_timeseries('inflow', incomid, 'Reach', 
                                                 '{}'.format(incomid))
            
        # assign as needed

        for tstype, identifier in hspfmodel.watershed_timeseries.items():
            
            submodel.assign_watershed_timeseries(tstype, identifier)

        for tstype, d in hspfmodel.subbasin_timeseries.items():

            for subbasin, identifier in d.items():
            
                if subbasin in submodel.subbasins:

                    submodel.assign_subbasin_timeseries(tstype, subbasin,
                                                        identifier)

        for tstype, d in hspfmodel.landuse_timeseries.items():

            for landtype, identifier in d.items():
            
                if landtype in submodel.landuse:

                    submodel.assign_landuse_timeseries(tstype, landtype,
                                                       identifier)

        for tstype, d1 in hspfmodel.operation_timeseries.items():

            for subbasin, d2 in d1.items():

                for otype, identifier in d2.items():

                    if subbasin in submodel.subbasins:

                        submodel.assign_operation_timeseries(tstype, subbasin,
                                                             otype, identifier)

        with open(picklefile, 'wb') as f: pickle.dump(submodel, f)

        self.basemodel = picklefile

    def create_snowmodel(self, hspfmodel, verbose = True, vverbose = False):
        """Builds a submodel from an HSPF model for a watershed just to 
        simulate temperature and snow."""

        filename = hspfmodel.filename + '_snow'

        submodel = HSPFModel()

        submodel.build_from_existing(hspfmodel, filename, directory = 
                                     hspfmodel.filepath[:-1],
                                     verbose = vverbose)

        # simplify the landtypes to one developed which contains one implnd and 
        # one perlnd (since they are all the same)

        for subbasin in submodel.subbasins:
            year = min(submodel.subbasins[subbasin].landuse.keys())
            submodel.subbasins[subbasin].landuse = {year: {'Developed': 100}}

        submodel.build()

        # get rid of the reaches

        submodel.rchreses = []

        # add in the modules

        submodel.add_temp()
            
        densities = [o.RDENPF 
                     for o in hspfmodel.perlnds + hspfmodel.implnds]
        depths    = [o.packsnow / o.RDENPF 
                     for o in hspfmodel.perlnds + hspfmodel.implnds]

        depth   = sum(depths) / len(depths)
        density = sum(densities) / len(densities)

        submodel.add_snow(depth = depth, density = density)            
        
        # overwrite the time series dictionaries for the model

        for subbasin in hspfmodel.subbasins:
            if subbasin in submodel.subbasins:
                start, tstep, data = hspfmodel.precipitations['%d' % subbasin]
                submodel.add_timeseries('precipitation', '%d' % subbasin, 
                                        start, data)
                submodel.assign_subbasin_timeseries('precipitation', subbasin, 
                                                    '%d' % subbasin)

        start, tstep, data = hspfmodel.temperatures[hspfmodel.description]

        submodel.add_timeseries('temperature', submodel.description, 
                                start, data)

        start, tstep, data = hspfmodel.dewpoints[hspfmodel.description]

        submodel.add_timeseries('dewpoint', submodel.description, start, data)

        submodel.assign_watershed_timeseries('temperature', 
                                             submodel.description)
        submodel.assign_watershed_timeseries('dewpoint',
                                             submodel.description)

        self.snowmodel = submodel.filepath + submodel.filename

        with open(self.snowmodel, 'wb') as f: pickle.dump(submodel, f)

    def adjust_hspfmodel(self, hspfmodel, parameters, ifraction = None, 
                         evap = None, ccfact = None, lzetp = None, lzsn = None, 
                         uzsn = None, intfw = None, infilt = None, 
                         agwrc = None, kvary = None, deepfr = None, irc = None, 
                         ftable = None):
        """Utilizes the calibration adjustments to update the values of the 
        parameters in the HSPF model. Returns an instance of the hspfmodel
        with the parameter adjustments."""
        
        if ifraction is None:  ifraction         = parameters.ifraction
        if evap is None:       evap_multiplier   = parameters.evap
        if ccfact is None:     CCFACT            = parameters.ccfact
        if lzetp is None:      LZETP_multiplier  = parameters.lzetp
        if lzsn is None:       LZSN_multiplier   = parameters.lzsn
        if uzsn is None:       UZSN_multiplier   = parameters.uzsn
        if intfw is None:      INTFW_multiplier  = parameters.intfw
        if infilt is None:     INFILT_multiplier = parameters.infilt
        if agwrc is None:      AGWRC             = parameters.agwrc
        if kvary is None:      KVARY             = parameters.kvary
        if deepfr is None:     DEEPFR            = parameters.deepfr
        if irc is None:        IRC               = parameters.irc
        if ftable is None:     ftable            = parameters.ftable

        # update the parameters

        hspfmodel.ifraction       = ifraction
        hspfmodel.evap_multiplier = evap_multiplier

        # set the values for each PERLND -- Note the limits on values

        for p in hspfmodel.perlnds:
            if p.VLE == 1: p.monLZETP = [min(l * LZSN_multiplier, 0.99) 
                                         for l in p.monLZETP]
            else:          p.LZETP  = min(p.LZETP * LZETP_multiplier, 0.99)
            p.LZSN   = max(0.26, min(p.LZSN * LZSN_multiplier, 2400))
            p.UZSN   = max(0.26, min(p.UZSN * UZSN_multiplier, 240))
            p.INTFW  = p.INTFW * INTFW_multiplier
            p.INFILT = max(0.003, min(p.INFILT * INFILT_multiplier, 2400))
            p.AGWRC  = AGWRC
            p.KVARY  = KVARY
            p.DEEPFR = DEEPFR
            p.IRC    = IRC

        for o in hspfmodel.perlnds + hspfmodel.implnds:
            o.CCFACT = CCFACT

        for r in hspfmodel.rchreses:
            for i in range(len(r.ftable)): 
                r.ftable[i][3] = r.ftable[i][3] * ftable

    def add_error_criteria(self, tot_error = 0.1, rec_error = 0.03, 
                           high_error = 0.15, low_error = 0.1, 
                           storm_error = 0.2, summer_storm_error = 0.5,
                           storm_peak_error = 0.5, season_error = 0.2):
        """Adds the error tolerances for the model."""

        self.error_criteria = HydrologyErrors()
        self.error_criteria.add_directly(tot_error, rec_error, high_error, 
                                         low_error, storm_error, 
                                         summer_storm_error, storm_peak_error,
                                         season_error)

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

        hspfmodel.wdminfile  = (hspfmodel.filepath + '%s_in.wdm' % 
                                hspfmodel.filename)
        hspfmodel.ucifile    = (hspfmodel.filepath + '%s.uci' % 
                                hspfmodel.filename)
        hspfmodel.wdmoutfile = (hspfmodel.filepath + '%s_out.wdm' % 
                                hspfmodel.filename)

        hspfmodel.targets = targets

    def get_snowprocessor(self, hspfmodel, dates, snowdata = None, 
                          verbose = True):
        """Postprocesses the data."""

        if verbose: print('postprocessing simulation results\n')

        postprocessor = Postprocessor(hspfmodel, self.process_dates)

        return postprocessor

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

    def perturb(self, base_values, variables, perturbations,
                parallel = True, process_dates = None, show = True):
        """Perturbs the values of the calibration parameters, runs simulations,
        and saves the calibration statistics."""

        if process_dates is None:
            comid = self.gagecomid
            start, tstep, data = self.basemodel.flowgages[comid]
            end = start + datetime.timedelta(minutes = tstep) * len(data)
            self.process_dates = start, end
        else:                     
            self.process_dates = process_dates

        # open up the default hspfmodel and create a list of HSPFModel 
        # instances to be simulated

        subfile = self.basemodel + '{}'.format(self.gagecomid)
        
        # create a submodel for the watershed for the gage (for efficiency)

        if not os.path.isfile(subfile):
            
            print('creating a submodel for the subwatershed for gage ' +
                  '{}\n'.format(self.gageid))
            self.create_submodel()

        else:

            print('using existing submodel for gage {}\n'.format(self.gageid))
            self.basemodel = subfile

        # adjust the default values to the current base values

        names = ['basecase']

        # create a list of Parameter instances to be simulated

        parameters = [base_values]

        for variable, perturbation in zip(variables, perturbations):

            # open the base case file

            with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)
            
            params = base_values.copy()

            # adjust the parameters by the pertubation

            if   variable == 'lzsn':      params.lzsn      += perturbation
            elif variable == 'uzsn':      params.uzsn      += perturbation
            elif variable == 'infilt':    params.infilt    += perturbation
            elif variable == 'lzetp':     params.lzetp     += perturbation
            elif variable == 'agwrc':     params.agwrc     += perturbation
            elif variable == 'kvary':     params.kvary     += perturbation
            elif variable == 'intfw':     params.intfw     += perturbation
            elif variable == 'deepfr':    params.deepfr    += perturbation
            elif variable == 'irc':       params.irc       += perturbation
            elif variable == 'ifraction': params.ifraction += perturbation
            elif variable == 'ftable':    params.ftable    += perturbation

            # provide a unique filename

            names.append(variable + '{:5.3f}'.format(perturbation))

            # add to the queue

            parameters.append(params)

        variables.insert(0, 'base')
        perturbations.insert(0, '')

        # collect simulation data in a list

        self.simulations = [] 
        
        # run either parallel or serially

        sims = names, parameters, variables, perturbations

        if parallel: self.perturb_parallel(*sims)
        else:        self.perturb_serial(*sims)

        # view the results

        if show: self.view_perturbation()

        # reset the base model

        self.basemodel = '%s/%s/hspf/%s' % (self.directory, self.HUC8, 
                                            'basecase')

    def perturb_serial(self, names, parameters, variables, perturbations, 
                       verbose = True):
        """runs the simulations for the HSPFModel instances serially."""

        print('attempting serial model simulations...\n')

        start = time.time()

        for m, p, var, val in zip(names, parameters, 
                                  variables, perturbations):
                
            # add the simulation parameters to the calibrator

            self.log_simulation(self.simulate(m, p, var, val, 'calibration'))

        if verbose: print('completed serial simulation in %.1f seconds' % 
                          (time.time() - start))

    def perturb_parallel(self, names, parameters, variables, perturbations,
                         verbose = True):
        """runs the simulations for the HSPFModel instances in parallel."""

        if verbose: print('attempting parallel model simulations...\n')

        start = time.time()

        # make copies of the message file to prevent crashes

        directory = os.path.dirname(hspf.__file__)
        messagepath = '{}/hspfmsg.wdm'.format(directory)

        # group the models to use the processors one at a time

        m = len(names)
        n = cpu_count()

        names         = [names[i:i+n]         for i in range(0, m, n)]
        parameters    = [parameters[i:i+n]    for i in range(0, m, n)]
        variables     = [variables[i:i+n]     for i in range(0, m, n)]
        perturbations = [perturbations[i:i+n] for i in range(0, m, n)]

        # create queues for the i/o variables

        inqueues  = [[Queue() for i in range(len(g))] for g in names]
        outqueues = [[Queue() for i in range(len(g))] for g in names]

        # create process instances for each simulation

        processes = [[Process(target = self.simulation_worker, args = args)
                      for args in zip(inq, outq)] 
                     for inq, outq in zip(inqueues, outqueues)]

        # run the simulations in groups

        for args in zip(inqueues, outqueues, processes, names,
                        parameters, variables, perturbations):
            inqs, outqs, procs, models, params, vars, vals = args

            for p in procs: p.start()

            for q, m, p, var, val in zip(inqs, models, params, vars, vals):
                q.put((m, p, var, val, 'calibration'))

            for p in procs: p.join()

        # get the output

        for outqs in outqueues: 
            for q in outqs: self.simulations.append(q.get())

        if verbose: print('completed parallel simulation in %.1f seconds' % 
                          (time.time() - start))

    def simulation_worker(self, inqueue, outqueue):
        """Dummy routine to provide I/O to simulation."""

        # get the input arguments

        args = inqueue.get()
        
        # run the simulation

        result = self.simulate(*args)

        outqueue.put(result)

    def simulate(self, name, parameters, variables, values, output):
        """Runs a hydrology simulation, postprocesses the data, and saves the
        calibration information."""

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # adjust the parameters for the model

        self.adjust_hspfmodel(hspfmodel, parameters)
        hspfmodel.filename = name

        if output == 'calibration':
            self.run_hydrology(hspfmodel, self.run_dates, 
                               ['reach_outvolume', 'groundwater'])
        elif output == 'hspexp':
            targets = ['evaporation', 'groundwater', 'water_state', 
                       'runoff', 'reach_outvolume']
            if self.snow: targets = targets + ['snow_state', 'snowpack']
            self.run_hydrology(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates)

        # calculate errors

        errors = self.calculate_errors(postprocessor)

        # close the processor

        self.close_postprocessor(postprocessor)

        return parameters, errors, variables, values
            
    def log_simulation(self, simulation):
        """Dummy routine to collect calibration data from a simulation."""

        self.simulations.append(simulation)

    def simulate_snow(self, hspfmodel, run = True):
        """Runs a snow simulation and packages all the info needed for 
        hydrology."""

        if run:
 
            # create a copy of the model to just do snow

            self.create_snowmodel(hspfmodel)

            with open(self.snowmodel, 'rb') as f: snowmodel = pickle.load(f)

            self.run_snow(snowmodel, self.run_dates)

        else: self.set_snow(snowmodel, self.run_dates)
            
        snowprocessor = self.get_snowprocessor(snowmodel, self.run_dates)

        wyields = snowprocessor.make_wyield_dictionary()
        rains   = snowprocessor.make_rain_dictionary()
        covers  = snowprocessor.make_snowcover_dictionary()
        ices    = snowprocessor.make_ice_dictionary()

        self.close_postprocessor(snowprocessor)

        return wyields, rains, covers, ices

    def add_snow_fluxes(self, hspfmodel, snow_fluxes, start_date):
        """Adds the snow time series to the model."""

        vars = ['wateryield', 'rain', 'snowcover', 'ice']

        for dtype, var in zip(snow_fluxes, vars):

            for subbasin, dict in dtype.items():

                for l, data in dict.items():

                    identifier = '{0}_{1}'.format(subbasin, l)
                    hspfmodel.add_timeseries(var, identifier,
                                             start_date, data)
                
                    # assign to the appropriate perlnds

                    for o in hspfmodel.landtypes[subbasin]:
                        if o != 'Impervious' and o != 'Reach':
                            hspfmodel.assign_operation_timeseries(var, subbasin,
                                                                  o, identifier)

                    # assign to the implnds

                    if l == 'Impervious':
                        hspfmodel.assign_operation_timeseries(var, subbasin, l,
                                                              identifier)

        # turn on snow for the land segments

        for o in hspfmodel.perlnds + hspfmodel.implnds: 
            o.SNOW = True

        return hspfmodel

    def check_mass_balance(self, base_values, run = True, process_dates = None):
        """Looks at the mass balance across the headwaters."""

        if process_dates is None: 
            comid = self.flowgages[self.gageid]
            start, tstep, data = self.basemodel.flowgages[comid]
            end = start + datetime.timedelta(minutes = tstep) * len(data)
            self.process_dates = start, end
        else:                     self.process_dates = process_dates

        # open up the default hspfmodel and create a list of HSPFModel 
        # instances to be simulated

        # create a submodel for the watershed for the gage (for efficiency)

        self.create_submodel()

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # adjust the default values to the current base values

        self.adjust_hspfmodel(hspfmodel, base_values)

        # figure out the external targets needed

        targets = ['reach_outvolume', 'groundwater', 'water_state', 
                   'evaporation', 'supply']
        
        if run: self.run_hydrology(hspfmodel, self.run_dates, targets)
        else:   self.set_hydrology(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates)

        mass_balance = postprocessor.get_mass_balance(self.gagecomid,
                                                      self.upcomids)

        self.close_postprocessor(postprocessor)

        # reset the base model

        self.basemodel = '%s/%s/hspf/%s' % (self.directory, self.HUC8, 
                                            'basecase')
    
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

    def calculate_errors(self, postprocessor, output = None):
        """Calculates the percent error in the calibration statistics."""

        postprocessor.get_calibration(self.gagecomid, upcomids = self.upcomids,
                                      verbose = False, vverbose = False)
        postprocessor.calculate_errors(verbose = False, output = output)

        errors = HydrologyErrors()
        errors.add_from_processor(postprocessor)

        return errors

    def view_perturbation(self):
        """Prints the calibration results."""

        parameters, errors, variables, values = zip(*self.simulations)

        basemodel  = errors[variables.index('base')]
        bparams    = parameters[variables.index('base')]

        base_values = []

        for variable in variables:
            
            if   variable == 'lzsn':      base_values.append(bparams.lzsn)
            elif variable == 'uzsn':      base_values.append(bparams.uzsn)
            elif variable == 'infilt':    base_values.append(bparams.infilt)
            elif variable == 'lzetp':     base_values.append(bparams.lzetp)
            elif variable == 'agwrc':     base_values.append(bparams.agwrc)
            elif variable == 'kvary':     base_values.append(bparams.kvary)
            elif variable == 'intfw':     base_values.append(bparams.intfw)
            elif variable == 'deepfr':    base_values.append(bparams.deepfr)
            elif variable == 'irc':       base_values.append(bparams.irc)
            elif variable == 'ifraction': base_values.append(bparams.ifraction)
            elif variable == 'evap':      base_values.append(bparams.evap)
            elif variable == 'ftable':    base_values.append(bparams.ftable)

        base_errors = [basemodel.total_error, basemodel.recession_error, 
                       basemodel.low_error,   basemodel.high_error, 
                       basemodel.storm_vol_error, basemodel.storm_peak_error,
                       basemodel.season_error, basemodel.summer_storm_error]

        base_stats = [basemodel.dr2, basemodel.logdr2, basemodel.dNS,
                      basemodel.logdNS, basemodel.mr2, basemodel.logmr2, 
                      basemodel.mNS, basemodel.logmNS, basemodel.dNS * 
                      basemodel.logdNS]

        error_columns = [[e.total_error, e.recession_error, e.low_error,
                          e.high_error, e.storm_vol_error, e.storm_peak_error,
                          e.season_error, e.summer_storm_error] for e in errors]

        stats_columns = [[e.dr2, e.logdr2, e.dNS, e.logdNS, e.mr2, e.logmr2, 
                          e.mNS, e.logmNS, e.calibration] 
                         for e in errors]

        # get the error deltas

        error_deltas = [[(sim - base) for sim, base in zip(c, base_errors)]
                         for c in error_columns]

        # get the statistics deltas

        stats_deltas = [[(sim - base) for sim, base in zip(c, base_stats)]
                         for c in stats_columns]

        # pop the base case

        base_d_errors = error_deltas.pop(variables.index('base'))
        base_d_stats  = stats_deltas.pop(variables.index('base'))

        # text

        error_descrips = ['Total Runoff:          ',
                          'Baseflow Recession:    ',
                          'Low Flows:             ',
                          'High Flows:            ',
                          'Storm Volume:          ',
                          'Storm Peak Flows:      ',
                          'Seasonal Error:        ',
                          'Summer Storms:         ']

        stats_descrips = ['daily r\u00B2:              ',
                          'daily log-flow r\u00B2:     ',
                          'daily Nash-Sutcliffe:  ',
                          'daily log-flow NS:     ',
                          'monthly r\u00B2:            ',
                          'monthly log-flow r\u00B2:   ',
                          'monthly Nash-Sutcliffe:',
                          'monthly log-flow NS:   ',
                          'daily * log-daily NS:  ']

        first_row  = ''.join(['Variable:              '] +
                             ['{:>7s}'.format(v) for v in variables])
        second_row = ''.join(['Current Value:                '] +
                             ['{:>7.2f}'.format(v) for v in base_values])
        third_row  = ''.join(['Perturbation:                 '] +
                             ['{:>7.2f}'.format(v) for v in values[1:]])

        # transpose
                        
        error_rows = zip(*error_deltas)
        stats_rows = zip(*stats_deltas)

        print('')
        print('Perturbation Results:\n')
        print(first_row)
        print(second_row)
        print(third_row)

        for descrip, b, row in zip(error_descrips, base_errors, error_rows):
            print(descrip + '{:7.1%}'.format(b) + 
                  ''.join(['{:7.1%}'.format(e) for e in row]))

        print('')
        print(first_row)
        print(second_row)
        print(third_row)

        for descrip, b, row in zip(stats_descrips, base_stats, stats_rows):
            print(descrip + '{:7.3f}'.format(b) + 
                  ''.join(['{:7.3f}'.format(e) for e in row]))
        print('')

    def view_calibration(self, stats = False, calibrator = False):
        """Prints the calibration results."""

        numbers, descriptions, parameters, errors = zip(*self.simulations)

        first_row  = ''.join(['Simulation Number:         '] + 
                             ['{:11d}'.format(n) for n in numbers])

        second_row = ''.join(['Description:               '] +
                             ['{:>11s}'.format(s) for s in descriptions] + 
                             ['   Goal'])

        error_columns = [['Total Runoff:              ',
                          'Baseflow Recession:        ',
                          'Low Flows:                 ',
                          'High Flows:                ',
                          'Storm Volume:              ',
                          'Storm Peak Flows:          ',
                          'Seasonal Error:            ',
                          'Summer Storms:             ']]

        stats_columns = [['daily r\u00B2:                  ',
                          'daily log-flow r\u00B2:         ',
                          'daily Nash-Sutcliffe:      ',
                          'monthly r\u00B2:                ',
                          'monthly log-flow r\u00B2:       ',
                          'monthly Nash-Sutcliffe:    ',
                          'monthly log-flow NS:   ']]

        parm_columns = [['impervious fraction:       ',
                         'potential/pan evaporation: ',
                         'lower zone ET ratio:       ',
                         'lower zone storage ratio:  ',
                         'upper zone storage ratio:  ',
                         'interflow inflow ratio:    ',
                         'infiltration (mm/hr):      ',
                         'baseflow recession:        ',
                         'aquifer recharge:          ',
                         'interflow recession:       ',
                         'snowmelt factor (mm/d/\u00B0C): ']]

        for error in errors + (self.error_criteria,): 
            column = [error.total_error,
                      error.recession_error,
                      error.low_error,
                      error.high_error,
                      error.storm_vol_error,
                      error.storm_peak_error,
                      error.season_error,
                      error.summer_storm_error]
            error_columns.append(column)

            stats_columns.append([error.dr2, error.logdr2, error.dNS, 
                                  error.mr2, error.logmr2, error.mNS])
                             
        for parm in parameters:
            column = [parm.ifraction,
                      parm.evap,  
                      parm.lzetp, 
                      parm.lzsn,  
                      parm.uzsn,  
                      parm.intfw, 
                      parm.infilt,
                      parm.agwrc, 
                      parm.kvary,
                      parm.deepfr,
                      parm.irc,   
                      parm.ftable] 
            parm_columns.append(column)

        # transpose

        error_rows = zip(*error_columns)
        stats_rows = zip(*stats_columns)
        parm_rows = zip(*parm_columns)

        print('')
        print('HSPF calibration results:\n')
        print(first_row)
        print(second_row)

        for row in error_rows:
            print(row[0] + ''.join(['{:11.1%}'.format(e) for e in row[1:-1]] +
                                   ['{:8.1%}'.format(row[-1])]))

        if stats:
            print('\nHSPF Model Fitting Statistics:\n')            
            print(first_row)
            print(second_row[:-4])

            for row in stats_rows:
                print(row[0] + ''.join(['{:11.3f}'.format(e) 
                                        for e in row[1:-1]]))

            print('')

        if calibrator:
            print('Calibrator Parameters:\n')
            print(first_row)
            print(second_row[:-4])

            for row in parm_rows:
                print(row[0] + ''.join(['{:11.2f}'.format(e) for e in row[1:]]))

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

    def plot_runoff(self, postprocessor, show = True, output = None):
        """Makes a runoff plot."""

        postprocessor.plot_runoff(show = show, output = output)

    def plot_storms(self, postprocessor, season = 'all', show = True, 
                    output = None):
        """Makes a series of storm plots."""

        postprocessor.plot_storms(season = season, show = show, output = output)

    def plot_calibration(self, postprocessor, show = True, output = None):
        """Makes a plot of the calibration statistics."""

        postprocessor.plot_calibration(show = show, output = output)

    def plot_dayofyear(self, postprocessor, show = True, output = None):

        postprocessor.plot_dayofyear(output = output, show = show)

    def plot_waterbudget(self, postprocessor, show = True, output = None):

        postprocessor.plot_waterbudget(show = show, output = output)

    def plot_allwaterbudgets(self, postprocessor, show = True, output = None):

        postprocessor.plot_allwaterbudgets(show = show, output = output)

    def save_calibration(self, base_values, process_dates = None, 
                         run = True, destination = 'calibrations'):
        """Saves all the parameters, errors, statistics,a nd graphs from the 
        calibration."""

        if process_dates is None: 
            comid = self.flowgages[self.gageid]
            start, tstep, data = self.basemodel.flowgages[comid]
            end = start + datetime.timedelta(minutes = tstep) * len(data)
            self.process_dates = start, end
        else:                     self.process_dates = process_dates

        # set up the path to the calibrations directory

        folder = '{0}/{1}/{2}'.format(self.directory, self.HUC8, destination)

        if not os.path.isdir(folder): os.mkdir(folder)

        # set up the path to the calibration for this gage

        folder = folder + '/{}'.format(self.gageid)

        if not os.path.isdir(folder): os.mkdir(folder)
            
        # create a submodel for the watershed for the gage

        self.create_submodel()

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # adjust the default values to the current base values

        self.adjust_hspfmodel(hspfmodel, base_values)

        # make plots of the calibration subbasin

        self.plot_gage_subbasin(hspfmodel)

        # make plots of the landuse segments

        self.plot_gage_landuse(hspfmodel)

        # figure out the external targets needed

        targets = ['reach_outvolume', 'groundwater', 'water_state', 
                   'evaporation', 'runoff', 'snow_state', 'snowpack', 
                   'snowfall']

        if run: self.run_hydrology(hspfmodel, self.run_dates, targets)
        else:   self.set_hydrology(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, process_dates)

        # calculate the errors and make a calibration report

        postprocessor.get_hspexp_parameters(verbose = False)

        output  = folder + '/calibration_report.csv'
        errors  = self.calculate_errors(postprocessor, output = output)

        # plot everything and save to file

        output  = folder + '/snow'
        self.plot_snow(postprocessor, output = output, show = False)

        output  = folder + '/snowcalibration'
        self.plot_snowcalibration(postprocessor, output = output, show = False)

        output  = folder + '/monthlyhydrograph'
        self.plot_hydrograph(postprocessor, output = output, show = False)

        output  = folder + '/dayofyear'
        self.plot_dayofyear(postprocessor, output = output, show = False)

        output  = folder + '/averagewaterbudget'
        self.plot_waterbudget(postprocessor, output = output, show = False)

        output  = folder + '/waterbudgets'
        self.plot_allwaterbudgets(postprocessor, output = output, show = False)

        output  = folder + '/runoff'
        self.plot_runoff(postprocessor, output = output, show = False)

        output  = folder + '/storms'
        self.plot_storms(postprocessor, output = output, show = False)

        output  = folder + '/calibration'
        self.plot_calibration(postprocessor, output = output, show = False)

        self.close_postprocessor(postprocessor)

        # get the outlet outflow volume time series

        outlet = self.get_outletflows(hspfmodel)

        # save the hspfmodel, base values, errors, parameters, and effluent

        with open(folder + '/hspfmodel',  'wb') as f: pickle.dump(hspfmodel, f)
        with open(folder + '/errors',     'wb') as f: pickle.dump(errors, f)
        with open(folder + '/parameters', 'wb') as f: pickle.dump(base_values,f)
        with open(folder + '/outletflows', 'wb') as f: pickle.dump(outlet, f)

    def plot_gage_subbasin(self, hspfmodel):
        """Makes a plot of the subbasin area."""

        from shapefile                         import Reader, Writer
        from pyhspf.preprocessing.merge_shapes import merge_shapes
        from pyhspf.preprocessing.gisplots     import plot_gage_subbasin

        folder = (self.directory, self.HUC8, self.gageid)

        subbasinfile = '{0}/{1}/calibrations/{2}/subbasins'.format(*folder)
        boundaryfile = '{0}/{1}/calibrations/{2}/boundary'.format(*folder)
        flowfile     = '{0}/{1}/calibrations/{2}/flowlines'.format(*folder)
        combinedfile = '{0}/{1}/calibrations/{2}/combined'.format(*folder)

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

        directory = '{0}/{1}/calibrations/{2}'.format(self.directory, 
                                                      self.HUC8,
                                                      self.gageid)

        if not os.path.isfile('{}/{}watershed.png'.format(directory, 
                                                          self.gageid)):

            plot_gage_subbasin(directory, self.HUC8, self.gageid, hspfmodel)

    def plot_gage_landuse(self, hspfmodel):
        """Makes plots of the landuse for a gage."""

        from pyhspf.preprocessing.gisplots import plot_gage_segments

        if hspfmodel.landuseyear is None: 
            subbasin = hspfmodel.subbasins[self.flowgages[self.gageid]]
            year = min(subbasin.landuse.keys())
        else:
            year = hspfmodel.landuseyear

        aggregate = 'aggregate.csv'
    
        folder = '{0}/{1}/calibrations/{2}'.format(self.directory, self.HUC8, 
                                                   self.gageid)

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

    def save_figures(self, base_values, gage, run = True, process_dates = None,
                     snow = False, hydrograph = False, dayofyear = False,
                     runoff = False, storms = False, calibration = False):
        """Saves the figures to the output directory."""

        # create a directory for the gage

        folders = (self.directory, self.HUC8, self.gageid)
        if not os.path.isdir('{0}/{1}/images/{2}'.format(*folders)):
            os.mkdir('{0}/{1}/images/{2}'.format(*folders))

        if process_dates is None: 
            comid = self.flowgages[self.gageid]
            start, tstep, data = self.basemodel.flowgages[comid]
            end = start + datetime.timedelta(minutes = tstep) * len(data)
            self.process_dates = start, end
        else:                     
            self.process_dates = process_dates

        # open up the default hspfmodel and create a list of HSPFModel 
        # instances to be simulated

        # create a submodel for the watershed for the gage (for efficiency)

        self.create_submodel()

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # adjust the default values to the current base values

        self.adjust_hspfmodel(hspfmodel, base_values)

        # figure out the external targets needed

        targets = ['reach_outvolume', 'groundwater', 'water_state']
        if snow:             
            targets = targets + ['snow_state', 'snowpack', 'supply']
        if hydrograph:       targets.append('evaporation')
        if runoff or storms: targets.append('runoff')
        
        if run: self.run_hydrology(hspfmodel, self.run_dates, targets)
        else:   self.set_hydrology(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, process_dates)

        if snow: 
            folders = (self.directory, self.HUC8, self.gageid, 'snow')
            output  = '{0}/{1}/images/{2}/{3}'.format(*folders)
            self.plot_snow(postprocessor, output = output, show = False)

        if hydrograph:
            folders = (self.directory, self.HUC8, self.gageid, 'hydrograph',
                       self.gageid)
            output  = '{0}/{1}/images/{2}/{3}{4}'.format(*folders)
            self.plot_hydrograph(postprocessor, output = output, show = False)

        if dayofyear:
            folders = (self.directory, self.HUC8, self.gageid, 'dayofyear',
                       self.gageid)
            output  = '{0}/{1}/images/{2}/{3}{4}'.format(*folders)
            self.plot_dayofyear(postprocessor, output = output, show = False)

        if runoff:
            folders = (self.directory, self.HUC8, self.gageid, 'runoff',
                       self.gageid)
            output  = '{0}/{1}/images/{2}/{3}{4}'.format(*folders)
            self.plot_runoff(postprocessor, output = output, show = False)

        if storms:
            folders = (self.directory, self.HUC8, self.gageid, 'storms',
                       self.gageid)
            output  = '{0}/{1}/images/{2}/{3}{4}'.format(*folders)
            self.plot_storms(postprocessor, output = output, show = False)

        if calibration:
            folders = (self.directory, self.HUC8, self.gageid, 'calibration',
                       self.gageid)
            output  = '{0}/{1}/images/{2}/{3}{4}'.format(*folders)
            self.plot_calibration(postprocessor, output = output, show = False)

        self.close_postprocessor(postprocessor)

        # reset the base model

        self.basemodel = '%s/%s/hspf/%s' % (self.directory, self.HUC8, 
                                            'basecase')

    def show_figures(self, base_values, run = True, process_dates = None,
                     snow = False, snowcalibration = False, hydrograph = False,
                     dayofyear = False, runoff = False, storms = False, 
                     calibration = False):
        """Shows the figures of the output."""

        if process_dates is None: 
            comid = self.flowgages[self.gageid]
            start, tstep, data = self.basemodel.flowgages[comid]
            end = start + datetime.timedelta(minutes = tstep) * len(data)
            self.process_dates = start, end
        else:                     
            self.process_dates = process_dates

        # create a submodel for the watershed for the gage (for efficiency)

        self.create_submodel()

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # adjust the default values to the current base values

        self.adjust_hspfmodel(hspfmodel, base_values)

        # figure out the external targets needed

        targets = []
        if any([hydrograph, runoff, calibration, storms]):
            targets += ['reach_outvolume', 'groundwater', 'water_state']
        if snowcalibration or snow: 
            targets += ['snowpack', 'supply', 'snowfall']
        if snow:             
            targets.append('snow_state')
        if hydrograph:       
            targets.append('evaporation')
        if runoff or storms: 
            targets.append('runoff')

        if run: self.run_hydrology(hspfmodel, self.run_dates, targets)
        else:   self.set_hydrology(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        postprocessor = self.get_postprocessor(hspfmodel, self.process_dates)

        if snow:            self.plot_snow(postprocessor, show = True)
        if snowcalibration: self.plot_snowcalibration(postprocessor, show =True)
        if hydrograph:      self.plot_hydrograph(postprocessor, show = True)
        if dayofyear:       self.plot_dayofyear(postprocessor, show = True)
        if runoff:          self.plot_runoff(postprocessor, show = True)
        if storms:          self.plot_storms(postprocessor, show = True)
        if calibration:     self.plot_calibration(postprocessor, show = True)

        self.close_postprocessor(postprocessor)

        #times, outvolumes = self.get_outletflows(hspfmodel)

        # reset the base model

        self.basemodel = '%s/%s/hspf/%s' % (self.directory, self.HUC8, 
                                            'basecase')

    def get_runoff_error(self):
        """Gets the total runoff error."""

        return ((self.postprocessor.sim_total - self.postprocessor.obs_total) /
                self.postprocessor.obs_total)

    def get_high_flow_error(self):
        """Gets the error in the highest flows."""

        return ((self.postprocessor.sim_high - self.postprocessor.obs_high) / 
                self.postprocessor.obs_high)

    def get_low_flow_error(self):
        """Gets the error in the lowest flows."""

        return ((self.postprocessor.sim_low - self.postprocessor.obs_low) / 
                self.postprocessor.obs_low)

    def low_tot_sim(self, error = 0.1):
        """Check if total runoff is too low."""

        if self.get_runoff_error() < -error:
            print('Issue: The simulated total runoff volume is too low.\n')
            return True

        return False

    def high_tot_sim(self, error = 0.1):
        """Check if total runoff is too high."""

        if self.get_runoff_error() > error:
            print('Issue: The simulated total runoff volume is too high.\n')
            return True

        return False

    def high_high_flows(self, error = 0.1):
        """Check if highest flows are too high."""

        if self.get_high_flow_error() > error:
            return True

        return False

    def low_high_flows(self, error = 0.1):
        """Check if the highest flows are too low."""

        if self.get_high_flow_error() < -error: 
            return True

        return False

    def high_low_flows(self, error = 0.1):
        """Check if the lowest flows are too high."""
        
        if self.get_low_flow_error() > error:
            return True

        return False

    def low_low_flows(self, error = 0.1):
        """Check if the lowest flows are too low."""
        
        if self.get_low_flow_error() < -error:
            return True

        return False

    def available_PET(self):
        """Checks to see if the unused potential evapotranspiration is greater
        than the difference between observed and simulated runoff volume."""

        if (self.postprocessor.obs_pet - self.postprocessor.sim_evap < 
            self.postprocessor.sim_total - self.postprocessor.obs_total):
            return False

        return True

    def lake_evaporation(self, lake = 0.7):
        """Checks to see if the ratio of lake to pan evaporation "lake" is 
        less than the user-supplied ratio of potential evapotranspiration to
        pan evaporation."""

        if self.parameters.evap > lake:
            return True

        return False

    def low_LZETP(self, tol = 0.01, pet_utilization = 0.7):
        """Checks if the values of LZETP are below typical values, and checks
        if the available PET is being utilized."""
        
        if self.parameters.lzetp < (1 - tol):
            print('The watershed PERLNDs have LZETP values that are below ' + 
                  'typical values. Try increasing these parameters.\n')
            return True

        # check PET utilization

        if (self.postprocessor.sim_evap / self.postprocessor.obs_pet < 
            pet_utilization):
            print('The simulated evapotranspiration is signficantly less ' +
                  'than the potential evapotranspiration. Increasing the ' +
                  'LZETP values will increase the utilization of the '     +
                  'potential evapotranspiration.\n')
            return True
        
        return False

    def high_LZETP(self, tol = 0.01, pet_utilization = 0.9):
        """Checks the values of LZETP to see if any are below typical values,
        or if the estimated potential evapotranspiration is less than the
        lake evaporation."""
        
        if self.parameters.lzetp > (1 + tol):
            print('The watershed PERLNDs have LZETP values that are above ' + 
                  'typical values. Try decreasing these parameters.\n')
            return True

        # check PET utilization

        if (self.postprocessor.sim_evap / self.postprocessor.obs_pet > 
            pet_utilization):
            print('The simulated evapotranspiration is almost as high as ' +
                  'the potential evapotranspiration. Increasing the '      +
                  'LZETP values will increase the utilization of the '     +
                  'potential evapotranspiration.\n')
            return True
 
        return False

    def low_LZSN(self, tol = 0.5):
        """Checks if the values of LZSN are too low (outside tolerance)."""

        if self.parameters.lzsn < tol:
            print('The watershed PERLNDs have LZSN values that are below ' +
                  'typical values. Try increasing these parameters.\n')
            return True
            
        return False

    def high_LZSN(self, tol = 2):
        """Checks if the values of LZSN are too high (outside tolerance)."""

        if self.parameters.lzsn > tol:
            print('The watershed PERLNDs have LZSN values that are above ' +
                  'typical values. Try decreasing these parameters.\n')
            return True
            
        return False

    def high_DEEPFR(self):
        """Checks if the values of DEEPFR are greater than zero."""

        if any([p.DEEPFR > 0 for p in self.postprocessor.hspfmodel.perlnds]):
            print('groundwater discharge is present in the simulation.\n')
            return

    def low_INFILT(self, tol = 5):
        """Checks if the high flow - low flow distribution is too large."""

        if (self.high_high_flows() and 
            self.low_low_flows() and 
            self.parameters.infilt < tol): 
            return True

        return False

    def high_INFILT(self, tol = 0.1):
        """Checks if the high flow - low flow distribution is too small."""

        if (self.low_high_flows() and 
            self.high_low_flows() and
            self.parameters.infilt > tol): 
            return True

        return False

    def high_high_low_ratio(self, high_error = 0.15, low_error = 0.1):
        """Checks if the high flow - low flow distribution meets the
        calibration standard."""

        if ((self.high_high_flows(error = high_error) and 
            self.low_low_flows(error = 0)) or
            (self.high_high_flows(error = 0) and 
             self.low_low_flows(error = low_error))):
            print('Issue: The high flow-low flow ratio is too large.\n')
            return True

        return False

    def low_high_low_ratio(self, high_error = 0.15, low_error = 0.1):
        """Checks if the high flow - low flow distribution meets the
        calibration standard."""

        if ((self.low_high_flows(error = high_error) and 
            self.high_low_flows(error = 0)) or
            (self.low_high_flows(error = 0) and 
             self.high_low_flows(error = low_error))):
            print('Issue: The high flow-low flow distribution is too small.\n')
            return True

        return False

    def low_AGWRC(self, value = 0.88):
        """Checks if any of the PERLNDs have an AGWRC below 0.88."""

        if any([p.AGWRC < value for p in self.postprocessor.hspfmodel.perlnds]):
            print('Some of the PERLNDs have an AGWRC less than 0.88, ' +
                  ' which is very low. Consider increasing these values.\n')
            return True

        return False

    def high_recession_rate(self, error = 0.03):
        """Checks if the recession rate is above the calibration standard."""

        if (self.postprocessor.sim_total_rec - self.postprocessor.obs_total_rec
            > error and not self.low_AGWRC()):
            print('Issue: The simulated daily low flow recession rate is ' +
                  'too high.\n')
            return True

        return False

    def low_recession_rate(self, error = 0.03):
        """Checks if the recession rate is above the calibration standard."""

        if (self.postprocessor.sim_total_rec - self.postprocessor.obs_total_rec
            < -error):
            print('Issue: The simulated daily low flow recession rate is ' +
                  'too low.\n')
            return True

        return False

    def high_storm_volume(self, error = 0.2):
        """Checks if the storm volumes are too high."""

        if ((self.postprocessor.sim_storm_volume - 
             self.postprocessor.obs_storm_volume) / 
            self.postprocessor.obs_storm_volume > error):
            print('Issue: The simulated storm volumes are too high.\n')
            return True

        return False

    def low_storm_volume(self, error = 0.2):
        """Checks if the storm volumes are too high."""

        if ((self.postprocessor.sim_storm_volume - 
             self.postprocessor.obs_storm_volume) / 
            self.postprocessor.obs_storm_volume < -error):
            print('Issue: The simulated storm volumes are too low.\n')
            return True

        return False

    def high_storm_peaks(self):
        """Checks if the storm peaks are high."""

        if (self.postprocessor.sim_storm_peaks > 
            self.postprocessor.obs_storm_peaks): 
            return True

        return False

    def low_storm_peaks(self):
        """Checks if the storm peaks are high."""

        if (self.postprocessor.sim_storm_peaks < 
            self.postprocessor.obs_storm_peaks): 
            return True

        return False

    def high_interflow(self, ratio = 2.5):
        """Checks if the ratio of interflow to surface runoff is too high."""

        if (self.postprocessor.storm_interflow / 
            self.postprocessor.storm_surface_runoff > ratio):
            print('Interflow is greater than %.1f times surface runoff.\n' %
                  ratio)
            return True
        
        return False

    def low_interflow(self, ratio = 2.5):
        """Checks if the ratio of interflow to surface runoff is too low."""

        if (self.postprocessor.storm_interflow / 
            self.postprocessor.storm_surface_runoff < ratio):
            print('Interflow is less than %.1f times surface runoff.\n' % ratio)
            return True
        
        return False

    def high_IRC(self, maxIRC = 0.7):
        """Checks if the values of IRC are high (i.e., interflow
        recession should happen faster than baseflow recession."""

        if self.parameters.irc > maxIRC:
            print('The values of IRC are above recommended levels.\n')
            return True

        return False

    def low_IRC(self, minIRC = 0.3):
        """Checks if the values of IRC are below the minimum."""

        if self.parameters.irc < minIRC:
            print('The values of IRC are below recommended levels.\n')
            return True

        return False

    def high_summer(self, error = 0.1):
        """Checks if the summer volume is too high."""

        if ((self.postprocessor.sim_summer - self.postprocessor.obs_summer) / 
            self.postprocessor.obs_summer > error):
            return True

        return False

    def low_summer(self, error = 0.1):
        """Checks if the summer volume is too low."""

        if ((self.postprocessor.sim_summer - self.postprocessor.obs_summer) / 
            self.postprocessor.obs_summer < -error):
            return True

        return False

    def high_winter(self, error = 0.1):
        """Checks if the winter volume is too high."""

        if ((self.postprocessor.sim_winter - self.postprocessor.obs_winter) / 
            self.postprocessor.obs_winter > error):
            return True

        return False

    def low_winter(self, error = 0.1):
        """Checks if the winter volume is too high."""

        if ((self.postprocessor.sim_winter - self.postprocessor.obs_winter) / 
            self.postprocessor.obs_winter < -error):
            return True

        return False

    def low_winter_high_summer(self, error = 0.2):
        """Checks if the seasonal volumes are low in winter and high in 
        summer."""

        if ((self.postprocessor.sim_summer - self.postprocessor.obs_summer) / 
            self.postprocessor.obs_summer - 
            (self.postprocessor.sim_winter - self.postprocessor.obs_winter) / 
            self.postprocessor.obs_winter > error):
            print('Issue: The simulated summer volumes are high, and the ' +
                  'simulated winter volumes are low. Try increasing UZSN.\n')
            return True

        return False

    def high_winter_low_summer(self, error = 0.2):
        """Checks if the seasonal volumes are low in winter and high in 
        summer."""

        if ((self.postprocessor.sim_summer - self.postprocessor.obs_summer) / 
            self.postprocessor.obs_summer -
            (self.postprocessor.sim_winter - self.postprocessor.obs_winter) / 
            self.postprocessor.obs_winter < -error):
            print('Issue: The simulated summer volumes are low, while the ' +
                  'simulated winter volumes are high. Try decreasing UZSN.\n')
            return True

        return False

    def low_winter_recession(self, tol = 0.01):
        """Checks if the simulated winter recession rate is less than
        the observed."""

        if (self.postprocessor.winter_sim_rec - 
            self.postprocessor.winter_obs_rec < -tol): 
            return True

        return False

    def high_winter_recession(self, tol = 0.01):
        """Checks if the simulated winter recession rate is greater than
        the observed."""

        if (self.postprocessor.winter_sim_rec - 
            self.postprocessor.winter_obs_rec > tol): 
            return True

        return False

    def low_summer_recession(self, tol = 0.01):
        """Checks if the simulated winter recession rate is less than
        the observed."""

        if (self.postprocessor.summer_sim_rec - 
            self.postprocessor.summer_obs_rec < -tol): 
            return True

        return False

    def high_summer_recession(self, tol = 0.01):
        """Checks if the simulated winter recession rate is greater than
        the observed."""

        if (self.postprocessor.summer_sim_rec - 
            self.postprocessor.summer_obs_rec > tol): 
            return True

        return False

    def low_high_recession(self):
        """Checks if the seasonal recession rates are different."""

        if self.low_winter_recession() and self.high_summer_recession():
            print('Issue: The simulated recession rates are low in winter ' +
                  'and high in summer.\n')
            return True
        
        return False

    def high_low_recession(self):
        """Checks if the seasonal recession rates are different."""

        if self.high_winter_recession() and self.low_summer_recession():
            print('Issue: The simulated recession rates are high in winter ' +
                  'and low in summer.\n')
            return True
        
        return False

    def get_advice(self, base_values, run = True, process_dates = None):
        """Provides a summary of the parameters from HSPF Expert, and then
        examines the rules as set out by HSPF Expert and provides guidance
        on parameter adjustment.
        """

        if process_dates is None: 
            comid = self.flowgages[self.gageid]
            start, tstep, data = self.basemodel.flowgages[comid]
            end = start + datetime.timedelta(minutes = tstep) * len(data)
            self.process_dates = start, end
        else:                     
            self.process_dates = process_dates

        tot_error    = self.error_criteria.total_error
        rec_error    = self.error_criteria.recession_error
        high_error   = self.error_criteria.high_error
        low_error    = self.error_criteria.low_error
        storm_error  = self.error_criteria.storm_vol_error
        season_error = self.error_criteria.season_error

        # create a submodel for the watershed for the gage (for efficiency)

        self.create_submodel()

        with open(self.basemodel, 'rb') as f: hspfmodel = pickle.load(f)

        # adjust the default values to the current base values

        self.adjust_hspfmodel(hspfmodel, base_values)

        # figure out the external targets needed

        targets = ['reach_outvolume', 'groundwater', 'water_state', 
                   'evaporation', 'runoff', 'supply']

        if self.snow: targets = targets + ['snow_state', 'snowpack']
        
        if run: self.run_hydrology(hspfmodel, self.run_dates, targets)
        else:   self.set_hydrology(hspfmodel, self.run_dates, targets)

        # add the simulation parameters to the calibrator

        self.postprocessor = self.get_postprocessor(hspfmodel, 
                                                    self.process_dates)

        self.postprocessor.get_hspexp_parameters(self.gagecomid)
        
        self.parameters = base_values

        tests = True

        # check for high total volume

        if self.high_tot_sim(error = tot_error):

            tests = False

            # check available PET

            if not self.available_PET(): 
                print('The disparity between simulated total runoff volume ' +
                      'and observed runoff volume cannot be accounted for '  +  
                      'by increasing simulated evapotranspiration. Try '     +
                      'increasing the pan evapotranspiration multiplier '    +
                      'for the timeseries.\n')

            # check for LZETP values too low

            elif self.low_LZETP(): 
                print('Increasing the lower zone evapotranspiration ' +
                      'parameter provides greater opportunity for ' +
                      'evapotranspiration and thus decreases the total ' +
                      'runoff volume.\n')

            # check if the LZSN values are below typical levels

            elif self.low_LZSN(): 
                print('Increasing the lower zone storage capacity provides ' +
                      'greater opportunity for evapotranspiration and thus ' +
                      'decreases the total runoff volume.\n')
            
            # check for high-low distriubtion

            elif self.low_INFILT() or self.high_INFILT(): 
                print('The high flow-low flow distribution is also off. ' +
                      'Although INFILT does not have a strong direct ' +
                      'influence on water balance, it may be too far off ' +
                      'for the other parameters to correct this error.\n')

            # otherwise need either more groundwater recharge or ET volume 
                
            else:
                print('If groundwater recharge to aquifers with discharge ' +
                      'outside the watershed are present, increase the '    +
                      'value of DEEPFR. Otherwise, more evapotranspiration' + 
                      'depth is needed, so increase the values of LZSN '    +
                      'and/or LZETP.\n')

        # check for low total volume

        if self.low_tot_sim(error = tot_error):  

            tests = False

            # check if potential ET greater than lake ET

            if self.lake_evaporation():
                print('Potential evapotranspiration is greater than ' +
                      'estimated lake evaporation. Try decreasing the '   +
                      'potential evapotranspiration multiplier.\n')

            # check if ET is too high

            elif self.high_LZETP(): 
                print('Decreasing the lower zone evapotranspiration ' +
                      'parameter provides less opportunity for ' +
                      'evapotranspiration and thus increases the total ' +
                      'runoff volume.\n')

            # check if LZSN is too high

            elif self.high_LZSN(): 
                print('Decreasing the lower zone storage capacity provides ' +
                      'less opportunity for evapotranspiration and thus ' +
                      'increases the total runoff volume.\n')

            # check for high-low distribution

            elif self.low_INFILT() or self.high_INFILT(): 
                print('The high flow-low flow distribution is off. ' +
                      'Although INFILT does not have a strong direct ' +
                      'influence on water balance, it may be too far off ' +
                      'for the other parameters to correct this error.\n')

            # otherwise need either less groundwater recharge or ET volume 

            else:
                print('Either decrease groundwater recharge to aquifers with ' +
                      'discharge outside the watershed by decreasing the ' +
                      'values of DEEPFR, or decrease the evapotranspiration ' +
                      'depth by decreasing the values of LZSN and LZETP.\n')

        # check the low flow recession rate

        if self.high_recession_rate(error = rec_error):

            tests = False

            print('Solution: Decrease the values of AGWRC. AGWRC controls ' +
                  'the baseflow recession rate. Decreasing the value of ' +
                  'AGWRC will steepen the base flow recession.\n')

        if self.low_recession_rate(error = rec_error): 

            tests = False

            if not self.low_AGWRC(): 
                print('Increasing AGWRC will flatten the base flow ' +
                      'recession.\n')

            else:
                print('The baseflow recession constant is already low, ' +
                      'so to increase the recession rate consider either ' +
                      'increasing transpiration along the channels through ' +
                      'BASETP or groundwater recharge through DEEPFR.\n')

        # check the high-low distribution

        if self.high_high_low_ratio(high_error = high_error, 
                                    low_error = low_error):

            tests = False

            # check seasonal volumes

            if (self.low_winter(error = season_error) and 
                self.high_summer(error = season_error) and
                not self.high_LZSN()):
                print('The seasonal flow distribution is off also. Try ' +
                      'decreasing the seasonal storage by decreasing LZSN.\n')

            # otherwise change infiltration

            else:
                print('Increase the value of INFILT to correct the flow ' +
                      'distribution.\n')

        if self.low_high_low_ratio(high_error = high_error, 
                                   low_error = low_error):

            tests = False
            
            # check seasonal volumes

            if(self.high_winter(error = season_error) and 
               self.low_summer(error = season_error) and
               not self.low_LZSN()):
                print('The seasonal flow distribution is off also. Try ' +
                      'increasing seasonal storage by increasing LZSN.\n')

            # otherwise change infiltration

            else:
                print('To correct the flow distribution, trying decreasing ' +
                      'the value of INFILT.\n')

        # check the storm volumes

        if self.high_storm_volume(error = storm_error):

            tests = False

            # see if it's way too high first
            
            if self.high_storm_volume(error = 3 * storm_error): 
                print('The storm volumes are excessively high. Increase the ' +
                      'values of INFILT to increase the infiltration rate.\n')

            # now check if the peaks are also high

            elif self.high_storm_peaks():
                print('The storm volumes are high and the storm peaks are ' +
                      'high. Increase the values of INFILT to increase the ' +
                      'infiltration rate.\n')

            # check the interflow/surface runoff ratio to see if a shift is 
            # needed to bump up the peaks

            elif self.low_interflow():
                print('The storm volumes are high but the storm peaks are ' +
                      'low. Decrease the values of INTFW to shift flow from ' +
                      'interflow to surface runoff.\n')

            # decrease IRC unless the values are too low already

            elif not self.low_IRC():
                print('The storm volumes are high but the storm peaks are ' +
                      'low and most of the storm runoff is interflow. '     +
                      'Decrease the values of IRC to decrease the time to ' +
                      'drain following a storm and increase peak flows.\n')

            # finally shift flow to baseflow if nothing else seems at issue

            else:
                print('The storm volumes are high. Increase the values ' +
                      'of INFILT to shift stormflow to baseflow.\n')

        if self.low_storm_volume(error = storm_error):

            tests = False

            # see if it's way too low first
            
            if self.low_storm_volume(error = 3 * storm_error):
                print('The storm volumes are excessively low. Decrease the ' +
                      'values of INFILT to decrease the infiltration rate.\n')

            # now check if the peaks are also low

            elif self.low_storm_peaks():
                print('The storm volumes are low and the storm peaks are ' +
                      'low. Decrease the values of INFILT to decrease the ' +
                      'infiltration rate.\n')

            # check the interflow/surface runoff ratio to see if a shift is 
            # needed to bring down the peaks

            elif self.high_interflow():
                print('The storm volumes are low but the storm peaks are ' +
                      'high. Increase the values of INTFW to shift flow from ' +
                      'surface runoff to interflow.\n')

            # increase IRC unless the values are too low already

            elif not self.high_IRC():
                print('The storm volumes are low but the storm peaks are ' +
                      'high and most of the storm runoff is surface runoff. ' +
                      'Increase the values of IRC to increase the time to ' +
                      'drain following a storm and decrease peak flows.\n')

            # finally shift flow from baseflow if nothing else seems at issue

            else:
                print('The storm volumes are low. Decrease the values ' +
                      'of INFILT to shift baseflow to stormflow.\n')


        # check the seasonal recession rates

        if self.low_high_recession() or self.high_low_recession():

            tests = False

            print('To correct the seasonal base flow recession, try '  +
                  'changing the value of KVARY. KVARY has the effect of ' +
                  'shifting base-flow drainage from drier periods ' +
                  '(no recharge) to shortly after wet ' +
                  'periods (recent recharge). It accounts for greater ' +
                  'contributing area immediately following a storm period.\n')

        # check the seasonal volume distribution

        if (self.low_winter_high_summer(error = 2 * season_error) or 
            self.high_winter_low_summer(error = 2 * season_error)):

            tests = False

            print('Surface storage in depressions and the upper few inches ' +
                  'of soil or forest litter remain near capacity in winter ' +
                  'so that the value of this storage (UZSN) has minimal ' +
                  'effect in winter but a much larger influence in summer ' +
                  'where losses for UZS are at or near the potential ' +
                  'evapotranspiration.')

        if tests: print('Simulation passes all tests.\n')
        else:     print('')

        self.close_postprocessor(self.postprocessor)

        # reset the base model

        self.basemodel = '%s/%s/hspf/%s' % (self.directory, self.HUC8, 
                                            'basecase')

    def close_postprocessor(self, postprocessor, save_file = None):
        """Saves the end states and closes up the postprocessor."""

        if save_file is not None: 
            postprocessor.close(save_states = save_file)
        else:           
            postprocessor.close()
