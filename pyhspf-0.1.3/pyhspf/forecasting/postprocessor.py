#!/usr/bin/env python3
#
# File: postprocess.py
# 
# David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Purpose: This file contains many functions to extract data for postprocessing
# from an HSPF simulation.
#
# Last updated: 03/22/2014

import os, pickle, numpy, datetime, calendar, csv

from scipy import stats

from pyhspf.core.wdmutil import WDMUtil

class Postprocessor:
    """A class for post processing HSPF hindcast data."""

    def __init__(self, basemodel, hindmodel, dates, comid = None, 
                 upcomids = [], verbose = False):

        self.basemodel = basemodel
        self.hindmodel = hindmodel
        self.verbose   = verbose

        # path to the files

        self.wdminfile   = '{0}{1}_in.wdm'.format(hspfmodel.filepath, 
                                                  hspfmodel.filename)
        self.wdmoutfile  = '{0}{1}_out.wdm'.format(hspfmodel.filepath, 
                                                   hspfmodel.filename)

        # open up the files

        verbose = True
        self.wdm = WDMUtil(verbose = verbose, 
                           messagepath = hspfmodel.messagepath)
        self.wdm.open(self.basein,  'r')
        self.wdm.open(self.baseout, 'r')
        self.wdm.open(self.hindin,  'r')
        self.wdm.open(self.hindout, 'r')

        import time
        time.sleep(5)
        dsns = self.wdm.get_datasets(self.wdmoutfile)

        print(self.wdm.openfiles)
        data = self.wdm.get_data(self.wdmoutfile, dsns[0])
        dates = self.wdm.get_dates(self.wdmoutfile, dsns[0])
        print(*dates)
        time.sleep(5)
        # define the processing period

        self.start, self.end = dates

        print(self.start, self.end)
        time.sleep(5)
        # define the processing area

        if comid is None:  self.comid = self.hspfmodel.outlets[0]
        else:              self.comid = comid

        self.upcomids = upcomids

        # Get the gage flows from the HSPFModel instance

        if len(hspfmodel.flowgages) > 0:

            for comid, g in hspfmodel.subbasin_timeseries['flowgage'].items():

                if comid == self.comid:

                    start, tstep, flows = hspfmodel.flowgages[g]
                    delta = datetime.timedelta(minutes = tstep)
                    self.gagedates = [start + i * delta
                                      for i in range(len(flows))]
                    self.gageflows = flows
                    self.gagestep  = tstep

        else: self.gagedates = None

        self.wdm_parms = None

    def is_number(self, s):
        """Tests if a string is a number."""
        try: float(s) 
        except ValueError: return False
        return True

    def closest_index(self, l, x):
        """Returns the index of the closest value in list "l" to value "x"."""

        return min(range(len(l)), key = lambda i: abs(l[i] - x))

    def within_dates(self, date, start, end):
        """Tests if "date" is greater than or equal to "start" and less than
        "end." """

        return start <= date and date < end

    def roundup(self, x, base):
        """Rounds "x" down to the nearest "base" """

        return int(base * numpy.ceil(x / base))
        
    def is_winter(self, date):
        """Tests if "date" is in winter (Dec - Feb)."""

        return date.month == 12 or date.month < 3

    def is_spring(self, date):
        """Tests if "date" is in spring (Mar - May)."""

        return 3 <= date.month and date.month < 6

    def is_summer(self, date):
        """Tests if "date" is in summer (June - Aug)."""

        return 6 <= date.month and date.month < 9

    def is_fall(self, date):
        """Tests if "date" is in fall (Sep - Nov)."""

        return 9 <= date.month and date.month < 12

    def within_season(self, date, start, end, season):
        """Tests if "date" is between the start and end and in the season."""

        if   season == 'winter':
            return (self.within_dates(date, start, end) and 
                    self.is_winter(date))
        elif season == 'spring':
            return (self.within_dates(date, start, end) and 
                    self.is_spring(date))
        elif season == 'summer':
            return (self.within_dates(date, start, end) and 
                    self.is_summer(date))
        elif season == 'fall':
            return (self.within_dates(date, start, end) and 
                    self.is_fall(date))

    def set_wdm_parms(self, wdm = 'output'):
        """Sets the values of variables for the output WDM file."""

        # ignore if correct parameters set up already

        if self.wdm_parms == wdm: return

        # otherwise set the attributes

        if   wdm == 'output': f = self.wdmoutfile
        elif wdm == 'input':  f = self.wdminfile
        else:
            print('Warning: unknown wdm file specified')
            raise

        self.dsns = self.wdm.get_datasets(f)
        self.idconss      = [self.wdm.get_attribute(f, n, 'IDCONS')
                             for n in self.dsns]
        self.descriptions = [self.wdm.get_attribute(f, n, 'DESCRP') 
                             for n in self.dsns]
        self.staids       = [self.wdm.get_attribute(f, n, 'STAID ')
                             for n in self.dsns]

        self.wdm_parms = wdm

    def aggregate_hourly_daily(self, hourly):
        """Aggregates an hourly timeseries to a daily one."""

        if len(hourly) < 24:
            print('warning: insufficiently short time series')
            return

        daily = []
        for i in range(0, len(hourly) - 23, 24):
            daily.append(sum(hourly[i:i+24]))

        return daily

    def aggregate_daily_monthly(self, dates, daily, option = 'total'):
        """Aggregates a daily timeseries into a monthly one."""

        if len(dates) < 30:
            print('warning: insufficiently short time series')
            return

        if len(dates) != len(daily):
            print('warning: dates and data length must be the same')
            return

        months, monthly = [dates[0]], [daily[0]]

        # iterate through the data

        for t, v in zip(dates[1:], daily[1:]):

            # check if it's a new month

            if months[-1].month != t.month:

                # start a new month

                months.append(t)
                monthly.append(v)

            # otherwise add the value to the monthly total

            elif v is not None and monthly[-1] is not None: monthly[-1] += v

            else: monthly[-1] = None

        # change from total to average if needed

        if option == 'average': 
        
            for i in range(len(months)):

                # get the number of days in the last month

                day, ndays = calendar.monthrange(months[i].year, 
                                                 months[i].month)

                # and divide to get the average

                if monthly[i] is not None:
                    monthly[i] = monthly[i] / ndays
                else: 
                    monthly[i] = None

        elif option != 'total':

            print('Warning: unknown option specified')
            return

        return months, monthly

    def get_timeseries(self, dates = None, tstep = 'daily'):
        """Returns a list of times for the simulation."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        if tstep == 'daily': 
            n = round((end - start).total_seconds() / 86400)
            delt = datetime.timedelta(days = 1)
        elif tstep == 'hourly':
            n = round((end - start).total_seconds() / 3600)
            delt = datetime.timedelta(hours = 1)
        else: 
            print('Warning: unknown time step specified for timeseries')
            return None

        return [start + delt * i for i in range(n)]

    def get_subbasin_areas(self, comids):
        """Returns the areas of the operations (land segments) linked to each 
        comid."""

        areas = []

        operations = self.hspfmodel.perlnds + self.hspfmodel.implnds

        for comid in comids:
            areas.append(sum([o.area for o in operations if o.comid == comid]))

        return areas

    def get_upstream_comids(self, comid, upcomids = []):
        """Uses an "updown" dictionary to find the comids upstream of comid."""

        done = False
        upstreams = [comid]
        n = 0
        while len(upstreams) != n:
            n = len(upstreams)
            for c in self.hspfmodel.updown:
                if (self.hspfmodel.updown[c] in upstreams and c not in 
                    upstreams and c not in upcomids):
                    upstreams.append(c)

        return upstreams

    def get_segment_timeseries(self, vars, comid, wdm = 'output', dates = None):
        """Gets the all the timeseries for a given list of variables in the 
        output WDM file for a subbasin identified by "comid." """

        self.set_wdm_parms(wdm = wdm)

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # get areas for the different land segments and store in a dictionary

        segment_areas = {}
        for o in self.hspfmodel.perlnds + self.hspfmodel.implnds:
            if o.comid == comid:
                segment_areas[o.landtype] = o.area

        # find the dsn of the surface storage with the right station id (comid)

        var_dsns, areas = [], []
        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id in vars and staid == comid:
                areas.append(segment_areas[desc])
                var_dsns.append(dsn)

        if len(areas) == 0:
            print('error, no segment records found for {}'.format(*vars))
            return

        vals = [self.wdm.get_data(self.wdmoutfile, n, start = start, end = end)
                for n in var_dsns]

        return vals, areas

    def get_subbasin_timeseries(self, vars, comids, dates = None):
        """Returns an area-weighted average of the variables in list "vars" for 
        each comid in the list "comids" (the idea being to give two variables
        when IMPLNDs and PERLNDS are different)."""

        subbasin_timeseries = []
        for comid in comids:
            values, areas = self.get_segment_timeseries(vars, comid, 
                                                        dates = dates)
            subbasin_timeseries.append(sum([v * a for v, a in 
                                            zip(values, areas)]) / sum(areas))

        return subbasin_timeseries

    def get_reach_timeseries(self, ID, comid, tstep = 'daily', dates = None):
        """Finds the timeseries for the constituent ID
        corresponding to the comid provided in the output WDM file."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms()

        # find the dsn of the TSS with the right station id (comid)

        n = None
        for idcons, staid, dsn in zip(self.idconss, self.staids, self.dsns):
            if idcons == ID and staid == comid:
                n = dsn
                break

        if n is None:
            print('unable to locate reach {} data for {}'.format(ID, comid))
            return

        values = self.wdm.get_data(self.wdmoutfile, n, start = start, end = end)
        times  = self.get_timeseries(tstep = tstep, dates = dates)

        return times, values

    def get_precipitation(self, comid, upcomids = [], tstep = 'daily', 
                          variable = 'supply', dates = None):
        """Returns the weighted average timeseries of precipitation 
        for the comids. Currently set up for either watershed-wide or subbasin
        timeseries (if you want something else you'll have to add it). If
        snow is simulated and "variable" is set to supply, the function
        returns the estimated moisture supplied rather than the input
        precipitation."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # get the comids and areas

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        areas = self.get_subbasin_areas(comids)

        # relevant operations (note this omits reaches)

        operations = self.hspfmodel.perlnds + self.hspfmodel.implnds

        self.set_wdm_parms()

        # check if the moisture supply is available; otherwise use input
        # precipitation

        if any([d == 'SUPY' for d in self.idconss]) and variable == 'supply':

            iters = zip(self.dsns, self.idconss, self.staids, self.descriptions)

            supys = self.get_subbasin_timeseries(['SUPY'], comids, 
                                                 dates = dates)
            prec = sum([p * a for p, a in zip(supys, areas)]) / sum(areas)

        else:
            self.set_wdm_parms(wdm = 'input')
            iters = zip(self.dsns, self.idconss, self.descriptions)

            if 'precipitation' in self.hspfmodel.watershed_timeseries:

                # then there's only one time series, find it

                prec_dsn = self.dsns[self.idconss.index('PREC')]
                prec = self.wdm.get_data(self.wdminfile, prec_dsn,
                                         start = start, end = end)

            elif 'precipitation' in self.hspfmodel.subbasin_timeseries:

                # make a dictionary for precip dsns to look up time series

                prec_dsns = {c:n for n, id, c in iters if id == 'PREC'}

                # get precipitation timeseries and subbasin areas for the 
                # upstream segments

                precips = [self.wdm.get_data(self.wdminfile, prec_dsns[comid],
                                             start = start, end = end)
                           for comid in comids]

                prec = sum([p*a for p, a in zip(precips, areas)]) / sum(areas) 

        if tstep == 'hourly':
            times = self.get_timeseries(tstep = tstep, dates = dates)
        if tstep == 'daily':
            times = self.get_timeseries(tstep = tstep, dates = dates)
            prec = [p for p in self.aggregate_hourly_daily(prec)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            prec = [p for p in self.aggregate_hourly_daily(prec)]
            times, prec = self.aggregate_daily_monthly(times, prec)
        elif tstep != 'hourly':
            print('Warning: unknown time step size specified')
            raise

        return times, prec

    def get_total_precipitation(self, comid, upcomids = [], variable = 'supply',
                                dates = None):
        """Returns the total precipitation volume for the comids."""

        times, prec = self.get_precipitation(comid, upcomids = upcomids, 
                                             tstep = 'hourly', 
                                             variable = variable, dates = dates)
        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        area   = sum(self.get_subbasin_areas(comids))

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 12

        return (prec.sum() * area / conv)

    def get_temperatures(self, tstep = 'daily', dates = None):
        """Returns a timeseries of temperatures for the watershed."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms(wdm = 'input')

        # find the temperature dataset

        i = 0
        while self.idconss[i] != 'TEMP': i+=1
        temp_dsn = self.dsns[i]

        temps = self.wdm.get_data(self.wdminfile, temp_dsn,
                                  start = start, end = end)

        if tstep == 'hourly': 
            pass
        elif tstep == 'daily': 
            temps = [t / 24 for t in self.aggregate_hourly_daily(temps)]
        elif tstep == 'monthly':
            temps = [t / 24 for t in self.aggregate_hourly_daily(temps)]
            times = self.get_timeseries(dates = dates, tstep = 'daily')
            times, temps = self.aggregate_daily_monthly(times, temps)
        else:
            print('Warning: unknown time step specified')
            return None

        times = self.get_timeseries(tstep = tstep, dates = dates)

        return times, temps

    def get_surface_storage(self, comid, upcomids = [], tstep = 'daily', 
                            dates = None):
        """Returns a timeseries of surface storage for the area. Note this
        assumes a daily dataset in the WDM file for storage."""

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        surss = self.get_subbasin_timeseries(['SURS'], comids, dates = dates)
        areas = self.get_subbasin_areas(comids)

        times  = self.get_timeseries(tstep = 'daily', dates = dates)
        surs = sum([s * a for s, a in zip(surss, areas)]) / sum(areas)

        if tstep == 'monthly':
            times, surs = self.aggregate_daily_monthly(times, surs, 
                                                       option = 'average')
        elif tstep != 'daily':
            print('Warning: unknown time step specified for surface runoff')
            return

        return times, surs

    def get_detach(self, comid, upcomids = [], tstep = 'daily', 
                   dates = None):
        """Returns a timeseries of surface detached sediment for the area. 
        Note this assumes a daily dataset in the WDM file for storage."""

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        detss = self.get_subbasin_timeseries(['DETS', 'SLDS'], comids, 
                                             dates = dates)
        areas = self.get_subbasin_areas(comids)

        times  = self.get_timeseries(tstep = 'daily', dates = dates)
        dets = sum([d * a for d, a in zip(detss, areas)]) / sum(areas)

        if tstep == 'monthly':
            times, dets = self.aggregate_daily_monthly(times, dets, 
                                                       option = 'average')
        elif tstep != 'daily':
            print('Warning: unknown time step specified for surface runoff')
            raise

        return times, dets

    def get_dewpoints(self, tstep = 'daily', dates = None):
        """Returns a timeseries of dewpoints for the watershed."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms(wdm = 'input')

        # find the dewpoint dataset

        i = 0
        while self.idconss[i] != 'DTMG': i+=1
        dewt_dsn = self.dsns[i]

        dewts = self.wdm.get_data(self.wdminfile, dewt_dsn,
                                  start = start, end = end)

        if   tstep == 'hourly': 
            pass
        elif tstep == 'daily': 
            dewts = [t / 24 for t in self.aggregate_hourly_daily(dewts)]
        elif tstep == 'monthly':
            dewts = [t / 24 for t in self.aggregate_hourly_daily(dewts)]
            times = self.get_timeseries(dates = dates, tstep = 'daily')
            times, dewts = self.aggregate_daily_monthly(times, dewts)
        else:
            print('Warning: unknown time step specified')
            return None

        times = self.get_timeseries(tstep = tstep, dates = dates)

        return times, dewts

    def get_pet(self, comid = None, upcomids = [], tstep = 'daily', 
                dates = None):
        """Returns a timeseries of potential evapotranspiration. Works for
        watershed-wide or landuse category-based.  If you want something else
        you'll have to add it."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms(wdm = 'input')

        if comid is None: comid = self.comid

        if 'evaporation' in self.hspfmodel.watershed_timeseries:

            # then there's only one timeseries

            evap_dsn = self.dsns[self.idconss.index('EVAP')]

            evap = (self.hspfmodel.evap_multiplier *
                    self.wdm.get_data(self.wdminfile, evap_dsn,
                                      start = start, end = end))

        else:

            # make a dictionary of the evaporation comids and dsn

            comids = self.get_upstream_comids(comid, upcomids = upcomids)

            iters = zip(self.dsns, self.idconss, self.descriptions)

            evap_dsns = {d:n for n, id, d in iters if id == 'EVAP'}

            # get evaporation timeseries and subbasin areas for the 
            # upstream segments

            operations = self.hspfmodel.perlnds + self.hspfmodel.implnds

            areas = []
            evaps = []

            d = self.hspfmodel.landuse_timeseries['evaporation']
            for l in self.hspfmodel.landuse:
                
                area = sum([o.area for o in operations 
                            if o.landtype == l and o.comid in comids])
                evap = self.wdm.get_data(self.wdminfile, evap_dsns[d[l]],
                                         start = start, end = end)
                areas.append(area)
                evaps.append(evap)

            evap = sum([e * a for e, a in zip(evaps, areas)]) / sum(areas) 

        if tstep == 'hourly':
            times = self.get_timeseries(tstep = tstep, dates = dates)           
        if tstep == 'daily': 
            times = self.get_timeseries(tstep = tstep, dates = dates)
            evap = self.aggregate_hourly_daily(evap)
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            evap = self.aggregate_hourly_daily(evap)
            times, evap = self.aggregate_daily_monthly(times, evap)
        elif tstep != 'daily':
            print('Warning: unknown time step specified')
            return None

        return times, evap

    def get_total_pet(self, comids, dates = None):
        """Returns the total potential evapotranspiration for the comids."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms(wdm = 'input')

        operations = self.hspfmodel.perlnds + self.hspfmodel.implnds

        if 'evaporation' in self.hspfmodel.watershed_timeseries:
            evap_dsn = self.dsns[self.idconss.index('EVAP')]
            evaps = self.wdm.get_data(self.wdminfile, evap_dsn,
                                      start = start, end = end)
            evap = self.hspfmodel.evap_multiplier * evaps.sum()
        else:
            self.set_wdm_parms(wdm = 'input')
            iters = zip(self.dsns, self.idconss, self.descriptions)
            evap_dsns = {d:n for n, id, d in iters if id == 'EVAP'}

            areas = []
            evaps = []
            d = self.hspfmodel.landuse_timeseries['evaporation']
            for l in self.hspfmodel.landuse:
                
                area = sum([o.area for o in operations 
                            if o.landtype == l and o.comid in comids])
                evap = self.wdm.get_data(self.wdminfile, evap_dsns[d[l]],
                                         start = start, end = end)
                areas.append(area)
                evaps.append(evap)

            evap = (sum([evap * area for evap, area in zip(evaps, areas)]) / 
                    sum(areas))

        areas = []
        for comid in comids:

            area = sum([o.area for o in operations if o.comid == comid])
            areas.append(area)

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 12

        return (evap.sum() * sum(areas) / conv)        

    def get_obs_snowdepth(self, identifier = None, dates = None):
        """Gets the observed snowpack depth from the hspfmodel."""

        if self.verbose: print('getting snowpack depths for the watershed')

        if dates is None: start, end = self.start, self.end

        if len(self.hspfmodel.snowdepths) == 0:
            print('error, no snow pack data file provided')
            raise

        else:

            if identifier is None:
                k = list(self.hspfmodel.snowdepths.keys())[0]
                s, tstep, values = self.hspfmodel.snowdepths[k]
            elif identifier in self.hspfmodel.snowdepths:
                s, tstep, values = self.hspfmodel.snowdepths[identifier]
            else:
                print('error, specified identifier not in snowdepths')
                raise

            delta = datetime.timedelta(minutes = tstep)
            times = [s + delta * i for i in range(len(values))]

            values = [v for t, v in zip(times, values) 
                      if start <= t and t < end]
            times  = [t for t in times if start <= t and t < end]

            return times, values 

    def get_obs_snowfall(self, identifier = None, dates = None):
        """Gets the average snowpack depth from the pickled snowfile."""

        if self.verbose: print('getting snowfall for the watershed')

        if dates is None: start, end = self.start, self.end

        if len(self.hspfmodel.snowfalls) == 0:
            print('error, no snowfall data file provided')
            raise

        else:

            if identifier is None:
                k = list(self.hspfmodel.snowfalls.keys())[0]
                s, tstep, values = self.hspfmodel.snowfalls[k]
            elif identifier in self.hspfmodel.snowfalls:
                s, tstep, values = self.hspfmodel.snowfalls[identifier]
            else:
                print('error, specified identifier not in snowfalls')
                raise

            delta = datetime.timedelta(minutes = tstep)
            times = [s + delta * i for i in range(len(values))]

            values = [v for t, v in zip(times, values) 
                      if start <= t and t < end]
            times  = [t for t in times if start <= t and t < end]

            return times, values 

    def get_total_flow(self, comid, dates = None):
        """Returns the total flow volume from reach for the subbasin with the
        given comid."""

        ts, vols = self.get_reach_timeseries('ROVOL', comid, tstep = 'hourly',
                                             dates = dates)

        return vols.sum()

    def get_total_inflow(self, upcomids, dates = None):
        """Returns the total flow volume from upstream reaches with the unique
        comids provided."""

        if len(self.hspfmodel.inlets) > 0:
            times, vols = self.get_involume(dates = dates)
            return sum(vols)

        else: return None

    def get_seasonal_flow(self, comid, season, dates = None):
        """Returns the flows from the comid during the season."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # get a timeseries for the dates and the outflow volumes

        times, vols = self.get_reach_timeseries('ROVOL', comid,
                                                tstep = 'hourly', dates = dates)

        seasonal = [(t, v) for t, v in zip(times, vols) 
                    if self.within_season(t, start, end, season)]

        return zip(*seasonal)

    def get_season_total(self, comid, season, dates = None):
        """Returns the total flow volume from the comid during the season."""

        times, vols = self.get_seasonal_flow(comid, season, dates = dates)

        return sum(vols)

    def get_sim_flow(self, comid, dates = None, tstep = 'daily'):
        """Returns a timeseries of flows (m3/s) at the comid."""

        times, vols = self.get_reach_timeseries('ROVOL', comid, 
                                                tstep = 'hourly', dates = dates)

        if   self.hspfmodel.units == 'Metric':  conv = 10**6
        elif self.hspfmodel.units == 'English': conv = 43560

        if tstep == 'hourly':

            times = self.get_timeseries(tstep = tstep, dates = dates)
            flows = [v * conv / 3600 for v in vols]

        elif tstep == 'daily': 

            times = self.get_timeseries(tstep = tstep, dates = dates)
            flows = [v * conv / 86400 for v in 
                     self.aggregate_hourly_daily(vols)]

        elif tstep == 'monthly':

            # get the daily, then aggregate to monthly

            times = self.get_timeseries(tstep = 'daily', dates = dates)
            flows = [v * conv / 86400 for v in 
                     self.aggregate_hourly_daily(vols)]
            
            times, flows = self.aggregate_daily_monthly(times, flows, 
                                                        option = 'average')

        return times, flows

    def get_groundwater(self, comids, dates = None, tstep = 'daily'):
        """Returns the total groundwater recharge for the simulation across
        the provided comids."""

        subbasin_areas = self.get_subbasin_areas(comids)
        subbasin_gw    = self.get_subbasin_timeseries(['IGWI'], 
                                                      comids, dates = dates)

        if subbasin_gw is None:
            print('warning: unable to find groundwater timeseries')
            raise

        # calculate the area weighted average groundwater recharge and total 
        # recharge volume

        groundwater_flows = (sum([g * a for g, a in 
                                  zip(subbasin_gw, subbasin_areas)]) / 
                             sum(subbasin_areas))

        if tstep == 'hourly':

            times = self.get_timeseries(tstep = tstep, dates = dates)
            groundwater_flows = [v * conv / 3600 for v in vols]

        elif tstep == 'daily': 

            times = self.get_timeseries(tstep = tstep, dates = dates)
            groundwater_flows = self.aggregate_hourly_daily(groundwater_flows)

        elif tstep == 'monthly':

            # get the daily, then aggregate to monthly

            times = self.get_timeseries(tstep = 'daily', dates = dates)
            flows = self.aggregate_hourly_daily(groundwater_flows)
            
            times, groundwater_flows = self.aggregate_daily_monthly(times, 
                                                                    flows) 

        return times, groundwater_flows

    def make_segment_dictionary(self, vars, dates = None):
        """Gets the air temperature from an output WDM file for all the
        land segments as a dictionary (subbasin, landtype)."""

        self.set_wdm_parms()

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # store data in a dictionary

        values = {comid: {} for comid in self.hspfmodel.subbasins}

        # iterate through the file and add as needed

        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id in vars:
                data = self.wdm.get_data(self.wdmoutfile, dsn, start = start, 
                                         end = end)
                values[staid][desc] = data

        return values

    def make_wyield_dictionary(self, dates = None):
        """Gets the snowpack water yield from an output WDM file for all the
        land segments as a dictionary (subbasin, landtype)."""

        self.set_wdm_parms()

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # store data in a dictionary

        wyields = {comid: {} for comid in self.hspfmodel.subbasins}

        # find the wyields and add to the dictionary

        wyield_dsns = []
        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id == 'WYIE':
                data = self.wdm.get_data(self.wdmoutfile, dsn, start = start, 
                                         end = end)
                wyields[staid][desc] = data

        return wyields

    def make_rain_dictionary(self, dates = None):
        """Gets the rain (as opposed to snow) from an output WDM file for all 
        the land segments as a dictionary (subbasin, landtype)."""

        self.set_wdm_parms()

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # store data in a dictionary

        rains = {comid: {} for comid in self.hspfmodel.subbasins}

        # find the rain datasets and add to dictionary

        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id == 'RAIN':
                data = self.wdm.get_data(self.wdmoutfile, dsn, start = start, 
                                         end = end)
                rains[staid][desc] = data

        return rains

    def make_snowcover_dictionary(self, dates = None):
        """Gets the snowpack water yield from an output WDM file for all the
        land segments as a dictionary (subbasin, landtype)."""

        self.set_wdm_parms()

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # store data in a dictionary

        snowcovers = {comid: {} for comid in self.hspfmodel.subbasins}

        # if the dataset is snowcover add it to the dictionary

        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id == 'SCOV':
                data = self.wdm.get_data(self.wdmoutfile, dsn, start = start, 
                                         end = end)
                snowcovers[staid][desc] = data

        return snowcovers

    def make_ice_dictionary(self, dates = None):
        """Gets the snowpack water yield from an output WDM file for all the
        land segments as a dictionary (subbasin, landtype)."""

        self.set_wdm_parms()

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # store data in a dictionary

        ices = {comid: {} for comid in self.hspfmodel.subbasins}

        # if the dataset is snowcover add it to the dictionary

        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id == 'ICE':
                data = self.wdm.get_data(self.wdmoutfile, dsn, start = start, 
                                         end = end)
                ices[staid][desc] = data

        return ices

    def get_snowfall(self, comid, upcomids = [], dates = None, tstep = 'daily'):
        """Gets the snow fall to the subbasin identified by "comid." """
    
        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms()

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        # store snowcfs for the different land segments in a dictionary

        areas = {}
        snowcfs = {}
        rdcsns = {}
        for o in self.hspfmodel.perlnds + self.hspfmodel.implnds:
            if o.comid not in snowcfs: 
                snowcfs[o.comid] = {}
                areas[o.comid]   = {}
                rdcsns[o.comid]  = {}
            snowcfs[o.comid][o.landtype] = o.SNOWCF
            areas[o.comid][o.landtype]   = o.area
            rdcsns[o.comid][o.landtype]  = o.RDCSN

        dsns  = {}
        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id == 'SNOF':
                if staid in comids and staid not in dsns:
                    dsns[staid] = {}
                dsns[staid][desc] = dsn

        if len(dsns) == 0:
            print('warning: no snowfall simulation results present')
            return

        falls = []
        fall_areas = []
        for comid in comids:
            for landtype in areas[comid]:
                fall_areas.append(areas[comid][landtype])
                dsn    = dsns[comid][landtype]
                snowcf = snowcfs[comid][landtype]
                f  = rdcsns[comid][landtype]
                falls.append(self.wdm.get_data(self.wdmoutfile, dsn, 
                                               start = start, end = end) / f)

        fall = (sum([fall * area for fall, area in zip(falls, fall_areas)]) / 
                sum(fall_areas))

        if tstep == 'hourly':
            times = self.get_timeseries(tstep = tstep, dates = dates)
        elif tstep == 'daily':
            times = self.get_timeseries(tstep = tstep, dates = dates)
            fall = [f for f in self.aggregate_hourly_daily(fall)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            fall  = [f for f in self.aggregate_hourly_daily(fall)]
            times, fall = self.aggregate_daily_monthly(times, fall)
        elif tstep != 'hourly':
            print('Warning: unknown time step specified for evaporation')
            return

        return times, fall

    def get_snowmelt(self, comid, dates = None):
        """Gets the snow melt yield to the subbasin identified by "comid." """
    
        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms()

        operations = self.hspfmodel.perlnds + self.hspfmodel.implnds

        segment_areas = {}
        for o in operations: segment_areas[o.landtype] = o.area

        # find the dsn of the snow melt with the right station id (comid)

        melt_dsns, areas = [], []
        for id, staid, desc, dsn in zip(self.idconss, self.staids, 
                                        self.descriptions, self.dsns):
            if id == 'MELT' and staid == comid:
                areas.append(segment_areas[desc])
                melt_dsns.append(dsn)
            
        melts = [self.wdm.get_data(self.wdmoutfile, n, start = start, end = end)
                 for n in melt_dsns]

        return melts, areas

    def get_reach_recession(self, comid, dates = None):
        """Calculates a timeseries of the daily recession rates for the comid
        provided."""

        # get the simulated flows

        ts, flows = self.get_sim_flow(comid, tstep = 'daily', dates=dates)

        # compute the recession rates and throw out if the flow increased

        sim_rates = [f1 / f2 for f1, f2 in zip(flows[1:], flows[:-1])]
        sim_rates.insert(0, 1)

        return ts, sim_rates

    def get_total_evap(self, comids, dates = None):
        """Returns the total evapotranspiration for the simulation across the
        provided comids."""

        subbasin_areas = self.get_subbasin_areas(comids)
        subbasin_evaps = self.get_subbasin_timeseries(['TAET', 'IMPEV'], 
                                                      comids, dates = dates)

        # calculate area-weighted average and total volume

        evap = sum([e * a for e, a in zip(subbasin_evaps, subbasin_areas)])

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 12

        return (evap.sum() / conv)

    def get_total_groundwater(self, comids, dates = None):
        """Returns the total groundwater recharge for the simulation across
        the provided comids."""

        subbasin_areas = self.get_subbasin_areas(comids)
        subbasin_gw    = self.get_subbasin_timeseries(['IGWI'], 
                                                      comids, dates = dates)

        if subbasin_gw is None:
            print('warning: unable to find groundwater timeseries')
            raise

        # calculate the area weighted average groundwater recharge and total 
        # recharge volume

        groundwater_flows = sum([g * a for g, a in 
                                 zip(subbasin_gw, subbasin_areas)])

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 12      

        return (groundwater_flows.sum() / conv)

    def get_surface_runoff(self, comid, upcomids = [], tstep = 'daily', 
                           dates = None):
        """Returns the weighted average surface runoff for the comids."""

        # make a dictionary of the surface runoff comids and dsn

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        suros = self.get_subbasin_timeseries(['SURO'], 
                                             comids, dates = dates)
        areas = self.get_subbasin_areas(comids)

        suro = sum([s * a for s, a in zip(suros, areas)]) / sum(areas)

        if tstep == 'hourly': 
            times  = self.get_timeseries(tstep = 'hourly', dates = dates)
        if tstep == 'daily':
            times  = self.get_timeseries(tstep = tstep, dates = dates)
            suro = [s for s in self.aggregate_hourly_daily(suro)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            suro = [s for s in self.aggregate_hourly_daily(suro)]
            times, suro = self.aggregate_daily_monthly(times, suro)
        elif tstep != 'hourly':
            print('Warning: unknown time step specified for surface runoff')
            return

        return times, suro

    def get_interflow(self, comid, upcomids = [], tstep = 'daily', 
                      dates = None):
        """Returns the weighted average interflow for the comids. """

        # make a dictionary of the interflow comids and dsn

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        ifwos = self.get_subbasin_timeseries(['IFWO'], 
                                             comids, dates = dates)
        areas = self.get_subbasin_areas(comids)

        ifwo = sum([i * a for i, a in zip(ifwos, areas)]) / sum(areas)

        if tstep == 'hourly': 
            times  = self.get_timeseries(tstep = 'hourly', dates = dates)
        if tstep == 'daily':
            times  = self.get_timeseries(tstep = tstep, dates = dates)
            ifwo = [i for i in self.aggregate_hourly_daily(ifwo)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            ifwo  = [i for i in self.aggregate_hourly_daily(ifwo)]
            times, ifwo = self.aggregate_daily_monthly(times, ifwo)
        elif tstep != 'hourly':
            print('Warning: unknown time step specified for interflow')
            return

        return times, ifwo

    def get_baseflow(self, comid, upcomids = [], tstep = 'daily', 
                     dates = None):
        """Returns the weighted average baseflow for the comids. """

        # make a dictionary of the baseflow comids and dsn

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        agwos = self.get_subbasin_timeseries(['AGWO'], 
                                              comids, dates = dates)
        areas = self.get_subbasin_areas(comids)

        agwo = sum([f * a for f, a in zip(agwos, areas)]) / sum(areas)

        if tstep == 'hourly': 
            times  = self.get_timeseries(tstep = 'hourly', dates = dates)
        if tstep == 'daily':
            times  = self.get_timeseries(tstep = tstep, dates = dates)
            agwo = [i for i in self.aggregate_hourly_daily(agwo)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            agwo  = [a for a in self.aggregate_hourly_daily(agwo)]
            times, agwo = self.aggregate_daily_monthly(times, agwo)
        elif tstep != 'hourly':
            print('Warning: unknown time step specified for baseflow')
            return

        return times, agwo

    def get_erosion(self, comid = None, upcomids = [], tstep = 'daily', 
                    dates = None):
        """Returns the erosion timeseries for the subbasins in a watershed 
        between the upstream comids and a downstream comid."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        if comid is None: comid = '{}'.format(self.comid)
        else:             comid = '{}'.format(comid)

        # make a dictionary of the baseflow comids and dsn

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        soseds = self.get_subbasin_timeseries(['SOSED', 'SOSLD'], 
                                              comids, dates = dates)
        areas  = self.get_subbasin_areas(comids)
        sosed  = sum([s * a for s, a in zip(soseds, areas)]) / sum(areas)

        if tstep == 'hourly': 
            times  = self.get_timeseries(tstep = 'hourly', dates = dates)
        if tstep == 'daily':
            times = self.get_timeseries(tstep = tstep, dates = dates)
            sosed = [i for i in self.aggregate_hourly_daily(sosed)]
        elif tstep == 'monthly':
            times  = self.get_timeseries(tstep = 'daily', dates = dates)
            sosed = [e for e in self.aggregate_hourly_daily(sosed)]
            times, sosed = self.aggregate_daily_monthly(times, sosed)
        elif tstep != 'hourly':
            print('Warning: unknown time step size specified for erosion')
            return

        return times, sosed

    def get_total_erosion(self, comids, dates = None):
        """Returns the total erosion for the simulation (tonnes) across
        the provided subbasin comids."""

        subbasin_areas = self.get_subbasin_areas(comids)

        soseds = self.get_subbasin_timeseries(['SOSED', 'SOSLD'], 
                                              comids, dates = dates)

        # calculate the total erosion

        erosion = sum([e * a for e, a in zip(soseds, subbasin_areas)])

        if   self.hspfmodel.units == 'Metric':  conv = 100
        elif self.hspfmodel.units == 'English':
            print('please check conversion units')

        return (erosion.sum() * conv)

    def get_total_sediment_in(self, comid, dates = None):
        """Returns the total mass of sediment into the reach."""

        ts, ised = self.get_reach_timeseries('ISED', comid, tstep = 'hourly')
        return ised[1].sum()

    def get_total_sediment_out(self, comid, dates = None):
        """Returns the total mass of sediment out of the reach."""

        ts, rosed = self.get_reach_timeseries('ROSED', comid, tstep = 'hourly')
        return rosed[1].sum()

    def get_sediment_loading(self, comid, tstep = 'daily', dates = None):
        """Returns the time series of sediment loading."""

        times, rosed = self.get_reach_timeseries('ROSED', comid, 
                                                 tstep = 'hourly', 
                                                 dates = dates)

        if tstep == 'hourly':
            pass
        elif tstep == 'daily':
            times = self.get_timeseries(tstep = tstep, dates = dates)
            rosed = [r for r in self.aggregate_hourly_daily(rosed)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            rosed = [r for r in self.aggregate_hourly_daily(rosed)]
            times, rosed = self.aggregate_daily_monthly(times, rosed)
        elif tstep != 'hourly':
            print('Warning: unknown time step specified for sediment loading')
            raise

        return times, rosed

    def get_evaporation(self, comid, upcomids = [], tstep = 'daily', 
                        dates = None):
        """Returns the weighted average evapotranspiration for the comids. """

        # make a dictionary of the evaporation comids and dsn

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        evaps = self.get_subbasin_timeseries(['TAET', 'IMPEV'], comids,
                                             dates = dates)
        areas = self.get_subbasin_areas(comids)

        evap = sum([e * a for e, a in zip(evaps, areas)]) / sum(areas)

        if tstep == 'hourly':
            times = self.get_timeseries(tstep = tstep, dates = dates)
        elif tstep == 'daily':
            times = self.get_timeseries(tstep = tstep, dates = dates)
            evap = [e for e in self.aggregate_hourly_daily(evap)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            evap = [e for e in self.aggregate_hourly_daily(evap)]
            times, evap = self.aggregate_daily_monthly(times, evap)
        elif tstep != 'hourly':
            print('Warning: unknown time step specified for evaporation')
            return

        return times, evap

    def get_avg_snowpack(self, comid, upcomids = [], dates = None):
        """Returns a time series of average snowpack depth across the comids."""

        comids = self.get_upstream_comids(comid, upcomids = upcomids)

        subbasin_areas = self.get_subbasin_areas(comids)
        snowpacks = self.get_subbasin_timeseries(['PDEP'], comids, 
                                                 dates = dates)
        snowpack = (sum([p * a for p, a in zip(snowpacks, subbasin_areas)]) /
                    sum(subbasin_areas))

        times = self.get_timeseries(tstep = 'daily', dates = dates)

        return times, snowpack

    def get_obs_flow(self, dates = None, tstep = 'daily', season = 'all',
                      verbose = False):
        """Returns lists of of observed flows and times for a gage station."""

        if self.gagedates is None:
            print('error, no gage data provided')
            raise

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        if   self.hspfmodel.units == 'Metric':  conv = 0.3048**3 # ft3 to m3
        elif self.hspfmodel.units == 'English': conv = 1

        # get the gage values for the simulation period

        if season == 'all': 
            def test(date): return self.within_dates(date, start, end)
        else:
            def test(date): return self.within_season(date, start, end, season)

        otimes = []
        oflows = []
        for date, flow in zip(self.gagedates, self.gageflows):

            if test(date):
                otimes.append(date)
                if flow is not None:
                    oflows.append(flow * conv) # convert to m3/s or ft3/s
                else:
                    oflows.append(None)

        if verbose and any([o is None for o in oflows]): 
            print('warning: missing flow data\n')

        # check if need to aggregate

        if self.gagestep == 60 and (tstep == 'daily' or tstep == 'monthly'):
            oflows = [f / 24 for f in self.aggregate_hourly_daily(oflows)]
            otimes = [t for t in otimes if t.hour == 0]

        if tstep == 'monthly':
            otimes, oflows = self.aggregate_daily_monthly(otimes, oflows,
                                                          option = 'average')
        elif tstep != 'daily' and tstep != 'hourly':
            print('Warning: unknown time step specified gage data')
            raise

        return otimes, oflows

    def get_waterquality_data(self, parameter, dates = None):
        """Returns lists of of observed times and values for a gage station."""

        if self.hspfmodel.waterquality is None:
            print('error, no water quality data provided')
            raise

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # get the gage values for the simulation period

        codes = {'TSS': '80154',
                 }

        p = codes[parameter]
        if p in self.hspfmodel.waterquality:
            data = zip(*[(t, s) for t, s in self.hspfmodel.waterquality[p]
                         if start <= t and t <= end])
        else: data = None

        return data

    def get_obs_sediment_load(self, tstep = 'daily', dates = None):
        """Returns the total observed sediment loading at a gage station."""
        
        # get the TSS concentrations

        tsstimes, concs = self.get_waterquality_data('TSS', dates = dates)
    
        # get the flow data

        flowtimes, flows = self.get_obs_flow(dates = dates)

        # make a time series for the period

        times = self.get_timeseries(dates = dates, tstep = tstep)

        # go through the time series and integrate the flow * concentration
        # to get the total mass flux

        if self.hspfmodel.units == 'Metric':
            if   tstep == 'hourly':
                conv = 3600 / 10**6  # m3/s * mg/L * 1 hour to metric tonnes
            elif tstep == 'daily':
                conv = 86400 / 10**6  # m3/s * mg/L * 1 day to metric tonnes
            else:
                print('you need to get the unit conversion factor')
                conv = 1
        else:
            print('you need to get the unit conversion factor')
            conv = 1

        series = []
        for t in times:

            # find the closest available TSS measurement

            c = concs[self.closest_index(tsstimes, t)]

            # find the closest gage measurement

            f = flows[self.closest_index(flowtimes, t)]
            
            series.append(c * f * conv)

        return times, series

    def get_total_obs_sediment(self, dates = None):
        """Returns the total observed sediment loading at a gage station."""

        times, loads = self.get_obs_sediment_load(tstep = 'daily',
                                                       dates = dates)

        return sum([t for t, il in zip(times, loads)])

    def get_total_gage(self, dates = None):
        """Returns the total flow at the gage in either acre-ft or Mm3."""
        
        times, flows = self.get_obs_flow(dates = dates, verbose = True)

        if   self.hspfmodel.units == 'Metric':  conv = 10**6
        elif self.hspfmodel.units == 'English': conv = 43560

        return (sum(flows) * 86400 / conv)

    def get_obs_recession(self, comid, dates = None):
        """Returns a timeseries of the daily recession rates for the comid
        provided."""

        # get the observed flows

        ts, flows = self.get_obs_flow(dates, verbose = True)

        # compute the recession rates and throw out if the flow increased

        observed_rates = [f1 / f2 for f1, f2 in zip(flows[1:], flows[:-1])]
        observed_rates.insert(0, 1)

        return ts, observed_rates

    def get_regression(self, comid, dates = None):
        """Returns the r2 for the linear and log-transformed flows at the 
        gage for both a daily and monthly basis."""

        # daily r2

        stimes, sflows = self.get_sim_flow(comid, tstep = 'daily',
                                                 dates = dates)
        otimes, oflows = self.get_obs_flow(tstep = 'daily', dates = dates)

        # deal with missing data

        sflows = [sflows[stimes.index(t)] 
                  for t, f in zip(otimes, oflows) 
                  if t in stimes and f is not None]
        oflows = [oflows[otimes.index(t)] 
                  for t, f in zip(otimes, oflows) 
                  if f is not None]

        slope, intercept, daily_r, p, std_err = stats.linregress(oflows, sflows)

        # daily log r2

        log_o = [numpy.log(f) for f in oflows]
        log_s = [numpy.log(f) for f in sflows]

        slope, intercept, daily_logr, p, std_err = stats.linregress(log_o, 
                                                                    log_s)

        # Daily Nash Sutcliffe Efficiency

        dailyNS = (1 - sum((numpy.array(sflows) - numpy.array(oflows))**2) /
                   sum((numpy.array(oflows) - numpy.mean(oflows))**2))

        # Daily log Nash Sutcliffe Efficiency

        daily_logNS = (1 - sum((numpy.array(log_s) - numpy.array(log_o))**2) /
                       sum((numpy.array(log_o) - numpy.mean(log_o))**2))

        # monthly r2

        otimes, oflows = self.get_obs_flow(tstep = 'monthly', dates = dates)
        stimes, sflows = self.get_sim_flow(comid, tstep = 'monthly',
                                                 dates = dates)

        if otimes[0].day != 1: otimes, oflows = otimes[1:], oflows[1:]

        # deal with missing data

        sflows = [sflows[stimes.index(t)] for t, f in zip(otimes, oflows) 
                  if t in stimes and f is not None]
        oflows = [f for t, f in zip(otimes, oflows) if f is not None]

        slope, intercept, monthly_r, p, std_err = stats.linregress(oflows, 
                                                                   sflows)

        # monthly log r2

        log_o = [numpy.log(f) for f in oflows]
        log_s = [numpy.log(f) for f in sflows]

        slope, intercept, monthly_logr, p, std_err = stats.linregress(log_o, 
                                                                      log_s)

        # Monthly Nash Sutcliffe Efficiency

        monthlyNS = (1 - sum((numpy.array(sflows) - numpy.array(oflows))**2) /
               sum((numpy.array(oflows) - numpy.mean(oflows))**2))

        # Monthly log Nash Sutcliffe Efficiency

        monthly_logNS = (1 - sum((numpy.array(log_s) - numpy.array(log_o))**2) /
               sum((numpy.array(log_o) - numpy.mean(log_o))**2))

        return (daily_r**2, daily_logr**2, dailyNS, daily_logNS,
                monthly_r**2, monthly_logr**2, monthlyNS, monthly_logNS)

    def get_seasonal_gage(self, season, dates = None):
        """Returns the total flow at the gage during the spring."""
        
        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        if   self.hspfmodel.units == 'English': conv = 43560
        elif self.hspfmodel.units == 'Metric':  conv = 10**6

        obs_flows = [f for t, f in zip(*self.get_obs_flow(dates = dates,
                                                                verbose = True))
                          if self.within_season(t, start, end, season)]

        return (sum(obs_flows) * 86400 / conv)

    def get_volume(self, comids, states):
        """Returns the total volume in the list of subbasins given the state
        variables."""

        volumes = [self.get_subbasin_volume(comid, states)
                   for comid in comids]

        return sum(volumes)

    def get_subbasin_volume(self, comid, states):
        """Gets the total volume in a subbasin."""

        # find all the operations in the subbasin

        landtypes = self.hspfmodel.landtypes[comid]

        # conversion factor

        if   self.hspfmodel.units == 'English': conv = 12
        elif self.hspfmodel.units == 'Metric':  conv = 1000

        # add the water from each constituent

        volume = 0

        for l in landtypes:
            
            if   l == 'Reach':      
                volume += states[comid][l]['VOL']

            elif l == 'Impervious': 

                d = states[comid][l]['RETS'] + states[comid][l]['SURS']
                volume += d * landtypes[l].area / conv

            else:

                vars = ['CEPS', 'SURS', 'UZS', 'IFWS', 'LZS', 'AGWS']
                d = sum([states[comid][l][v] for v in vars])
                volume += d * landtypes[l].area / conv

        return volume

    def get_sediment_storage(self, comids, states):
        """Returns the total sediment in the list of subbasins given the state
        variables."""

        storages = [self.get_reach_sediment(comid, states)
                    for comid in comids]

        return sum(storages)

    def get_reach_sediment(self, comid, states):
        """Gets the total sediment in a reach."""

        # find all the operations in the subbasin

        landtypes = self.hspfmodel.landtypes[comid]

        # conversion factor

        if   self.hspfmodel.units == 'English': conv = 12
        elif self.hspfmodel.units == 'Metric':  conv = 1000

        # add the water from each constituent

        return sum([s for s in states[comid][l]['RSED'] for l in landtypes
                    if l == 'Reach'])

    def get_states(self, wdm = 'output', date = None):
        """Gets the values of the state variables from the output WDM file at
        time step "i" for the simulation."""

        self.set_wdm_parms(wdm = wdm)

        # this assumes state variables all have a daily time step savings

        dsns = [n for n in self.dsns
                if self.wdm.get_attribute(self.wdmoutfile, n, 'TCODE ') == 4]

        # get info from the wdm file

        if date is None or date == self.end: i = -1
        else:                                
            ts = self.get_timeseries((self.start, self.end))
            i = ts.index(date)

        data = [self.wdm.get_data(self.wdmoutfile, n, start = self.start,
                                  end = self.end)[i]
                for n in self.dsns]

        # the organization goes like this: 1) subbasin 2) land type 3) variable
        # so there is a dictionary of dictionaries to keep track of the data

        states = {}

        # zip up the station id, landtype, constituent, and values to parse

        values = [self.staids, self.descriptions, self.idconss, data]

        for comid in self.hspfmodel.updown:

            # re-package using the state values in the subbasin and make a
            # dictionary to store the value for each perlnd

            states[comid] = {}
            subbasin_values = [[d, i, s] for c, d, i, s in zip(*values) 
                               if c == comid]

            # find all the land segments in the subbasin

            operations = [o for o in (self.hspfmodel.perlnds + 
                                      self.hspfmodel.implnds +
                                      self.hspfmodel.rchreses)
                          if o.comid == comid]
            
            for o in operations:

                states[comid][o.landtype] = {i:s for d, i, s in subbasin_values
                                             if d == o.landtype}

        return states        

    def get_recession_rates(self, comid, dates = None, season = None,
                            data = 'simulated', percentile = 30):
        """Returns the values of the highest X percentile simulated and 
        measured daily low flow recession rates for the time period."""

        if data == 'simulated':

            # get the simulated recession rates
            
            ts, rates = self.get_reach_recession(comid, dates = dates)

        elif data == 'observed':

            ts, rates = self.get_obs_recession(comid, dates = dates)

        else: 
            print('Warning: unknown data option selected')
            return

        if season is None: 
            seasonal_rates = rates
        elif season == 'summer':
            seasonal_rates = [r for t, r in zip(ts, rates)
                              if 6 <= t.month and t.month <= 8]
        elif season == 'winter':
            seasonal_rates = [r for t, r in zip(ts, rates)
                              if t.month <= 2 or 12 == t.month]

        low_rates = [r for r in seasonal_rates if r < 1]

        cutoff = stats.scoreatpercentile(low_rates, 100 - percentile)

        highest = [r for r in low_rates if r > cutoff]

        return highest

    def mean_recession(self, comid, dates = None, season = None):
        """Returns the mean values of the low flow recession rate across the
        provided time period."""
        
        observed  = self.get_recession_rates(comid, dates = dates, 
                                             season = season, 
                                             data = 'observed')        
        simulated = self.get_recession_rates(comid, dates = dates,
                                             season = season)

        return sum(observed) / len(observed), sum(simulated) / len(simulated)

    def mean_flow(self, comid, low_percent, high_percent, dates = None):
        """Returns the mean daily flows across a given percentile."""

        # get the flows

        oflows, sflows = self.get_flow_percents(comid, low_percent, 
                                                high_percent, dates = dates)

        # get the mean

        obs_mean  = sum(oflows) / len(oflows)        
        sim_mean = sum(sflows) / len(sflows)

        return obs_mean, sim_mean

    def get_flow_percents(self, comid, low_percent, high_percent, dates = None):
        """Returns the mean daily flows across a given percentile."""

        ts, flows = self.get_obs_flow(dates, verbose = True)

        # compute the cutoffs

        low  = stats.scoreatpercentile(flows, low_percent)
        high = stats.scoreatpercentile(flows, high_percent)

        # find the flows in the cutoff range

        obs_flows = [f for f in flows if low < f and f < high]

        # get the simulated data

        ts, flows = self.get_sim_flow(comid, tstep = 'daily', dates=dates)

        # compute the cutoffs

        low  = stats.scoreatpercentile(flows, low_percent)
        high = stats.scoreatpercentile(flows, high_percent)
        
        sim_flows = [f for f in flows if low < f and f < high]

        return obs_flows, sim_flows

    def get_runoff_flows(self, comid, upcomids = [], tstep = 'daily',
                         dates = None):
        """Assess runoff components of HSPF model."""
        
        # get the flow timeseries

        flows = self.get_runoff_depths(comid, upcomids = upcomids, 
                                       tstep = tstep, dates = dates)

        times, runoff, interflow, baseflow = flows

        # get runoff area (km2)

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        area   = sum(self.get_subbasin_areas(comids))

        # convert to m3/s or ft3/s

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 43560 / 12

        if   tstep == 'daily':   factor = 86400 / conv
        elif tstep == 'hourly':  factor = 3600 / conv
        else: 
            print('Warning: unknown timestep specified for runoff flows')
            return

        timeseries = (times, 
                      [r * area / factor for r in runoff], 
                      [i * area / factor for i in interflow], 
                      [b * area / factor for b in baseflow])

        return timeseries

    def get_involume(self, dates = None, tstep = 'daily'):
        """Gets the volume of flow into the watershed from upstream as a 
        timeseries."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        self.set_wdm_parms(wdm = 'input')

        # find the dsns of the inflows for each comid

        dsns = []
        for idcons, desc, dsn in zip(self.idconss, self.descriptions,
                                     self.dsns):
            if (idcons == 'FLOW' and 
                desc in self.hspfmodel.inlets):
                dsns.append(dsn)

        if len(dsns) == 0:
            print('error, no inflow records found')
            return

        inflows = [self.wdm.get_data(self.wdminfile, n, start = start, 
                                     end = end) for n in dsns]

        inflow = sum(inflows)

        if tstep == 'hourly':
            times = self.get_timeseries(tstep = tstep, dates = dates)
        elif tstep == 'daily':
            times = self.get_timeseries(tstep = tstep, dates = dates)
            inflow = [f for f in self.aggregate_hourly_daily(inflow)]
        elif tstep == 'monthly':
            times = self.get_timeseries(tstep = 'daily', dates = dates)
            inflow = [f for f in self.aggregate_hourly_daily(inflow)]
            times, inflow = self.aggregate_daily_monthly(times, inflow)
        elif tstep != 'hourly':
            print('Warning: unknown time step size specified')
            return
        
        return times, inflow

    def get_inflow(self, upcomids = None, dates = None, tstep = 'daily'):
        """Returns a time series of inflow into the watershed from the
        upcomids."""

        if upcomids is None:
            times, ivols = self.get_involume(dates = dates, tstep = tstep)

        # convert to flow if m3/s or ft3/s

        if   self.hspfmodel.units == 'English': u = 43560
        elif self.hspfmodel.units == 'Metric':  u = 10**6

        if   tstep == 'daily':  t = 86400
        elif tstep == 'hourly': t = 3600

        flows = [v * u / t for v in ivols]

        return times, flows

    def get_runoff_depths(self, comid, upcomids = [], tstep = 'daily', 
                          dates = None):
        """Returns the three components of the runoff volume from the 
        HSPF model as the times, surface runoff, interflow, baseflow."""

        # surface runoff, suro (mm or in)

        times, runoff = self.get_surface_runoff(comid, tstep = tstep,
                                                dates = dates)

        # interflow, ifwo (mm or in)

        times, interflow = self.get_interflow(comid, tstep = tstep,
                                              dates = dates)

        # baseflow, agwo (mm or in)

        times, baseflow = self.get_baseflow(comid, tstep = tstep,
                                            dates = dates)

        return times, runoff, interflow, baseflow

    def get_storm_dates(self, times, flows, prec, rec, area, season = 'all',
                        ndays = 1, pmin = 1, min_rec = 0.9, verbose = False):
        """Finds the start and end date for the largest storm during the
        supplied period given the gage flows, precipitation, and recession
        rate timeseries."""

        if len(times) < 3:
            print('error, the minimum number of dates to look for storms is 3')
            raise

        # find the peak flow date for the year (to establish the storm)

        if   season == 'spring': months = [3, 4, 5]
        elif season == 'summer': months = [6, 7, 8]
        elif season == 'fall':   months = [9, 10, 11]
        elif season == 'winter': months = [1, 2, 12]
        elif season == 'all':    months = [i for i in range(1,13)]
        elif season == 'other':  months = [1, 2, 3, 4, 5, 9, 10, 11, 12]

        fs = [f for t, f in zip(times, flows) if t.month in months]

        if len(fs) == 0: return None

        max_flow = max(fs)
        max_date = times[flows.index(max_flow)]

        # convert to the flow to a daily drainage depth (mm or in)

        if   self.hspfmodel.units == 'English': conv = 43560 / 12
        elif self.hspfmodel.units == 'Metric':  conv = 1000

        depth = max_flow / area * 86400 / conv

        # parse backward through time until the achieving a dry period of
        # "ndays" and (at a minimum) the daily discharge in rainfall to 
        # establish the start of the storm

        pdepth = 0
        i = times.index(max_date)

        # hack way to stop it from looking too far back (30 days)

        imin = max(i - 30, 0)

        while (pdepth < depth or pmin < sum(prec[i - ndays:i])) and imin < i:
            pdepth = pdepth + prec[i]
            i -= 1

        storm_start = times[i - ndays + 1]

        # continue forward until achieving a dry period of "ndays" and
        # (at a minimum) a low flow recession period

        i = times.index(max_date)
        rmin = 0.85

        # hack way to limit max forward period (30 days)

        imax = min(i + 30, len(prec), len(times) - 2)

        while ((pmin < sum(prec[i - ndays:i]) or 
                rec[i] < rmin or 
                1 < rec[i] or
                rec[i+1] < rmin
                ) and 
               i < imax):
            i += 1

        storm_end = times[i+1]

        if verbose: print('storm period:', storm_start, storm_end)

        return storm_start, storm_end

    def get_storms(self, comid, upcomids = [], season = 'all', dates = None,
                   tstep = 'hourly'):
        """Returns a list of dates for the highest storm events of the year
        during the period provided."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        # get the upstream area (for conversions to mm)

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        area   = sum(self.get_subbasin_areas(comids))

        # make a list of all the years

        years = [start]
        while years[-1] < end - datetime.timedelta(days = 10): 
            years.append(datetime.datetime(years[-1].year + 1, 1, 1))

        # bring in the gage data (daily) and simulated data (hourly)

        data = self.get_obs_flow(dates = dates, verbose = True)

        # make a list of storms

        storms = []

        for start, end in zip(years[:-1], years[1:]):
            
            # get the gage data for the year

            if len([t for t in data[0] if t.year == start.year]) > 0:
                times, oflows = zip(*[(t, f) for t, f in zip(*data)
                                      if t.year == start.year])


                # get the precipitation time series for the area for the year

                times, prec = self.get_precipitation(comid, upcomids = upcomids,
                                                     tstep = 'daily',
                                                     dates = (start, end))

                # get the recession rate time series for the area for year

                times, rec = self.get_obs_recession(comid, 
                                                         dates = (start, end))

                # find the peak storm for the year
            
                storm_dates = self.get_storm_dates(times, oflows, prec, rec, 
                                                   area, season = season)

                if storm_dates is not None:

                    storms.append(self.get_stormflows(storm_dates, tstep, 
                                                      comid))

        return storms

    def get_stormflows(self, storm_dates, tstep, comid):
        """Gets the storm timeseries parameters for a supplied list of storm 
        start and end dates."""

        # get the observed flows for the storm dates

        obs = self.get_obs_flow(dates = storm_dates, tstep = tstep)
        
        # get the time series for the storm period

        storm_times = self.get_timeseries(dates = storm_dates, tstep = 'hourly')

        # get the precipitation time series

        prec = self.get_precipitation(comid,  tstep = 'hourly', 
                                      dates = storm_dates)[1]

        # get the simulated flows

        flows = self.get_sim_flow(comid, tstep = 'hourly', 
                                        dates = storm_dates)[1]

        # get the inflow (if exists)

        if len(self.hspfmodel.inlets) > 0:

            times, inflow = self.get_inflow(tstep = 'hourly', 
                                            dates = storm_dates)
        else: inflow = None

        # get the runoff components (if available)

        if 'runoff' in self.hspfmodel.targets:
            times, r, i, b = self.get_runoff_depths(comid, tstep = 'hourly',
                                                    dates = storm_dates)
        else: r, i, b = ([0 for t in storm_times] for i in range(3))

        return (storm_times, prec, flows, inflow, r, i, b, obs)

    def get_storm_volumes(self, storms, area):
        """Returns the total observed and simulated storm volume from the list
        of storms."""

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 43560 / 12

        simulated = 0
        observed  = 0

        for storm in storms:

            t, p, s, u, r, i, b, o = storm 

            # convert flows to depth per time step interval

            flows = [f / area * 3600 / conv  for f in s]
            obs   = [f / area * self.gagestep * 60 / conv for f in o[1]]

            # add to the total

            simulated += sum(flows)
            observed  += sum(obs)

        return observed, simulated

    def get_storm_peaks(self, storms):
        """Returns the peak values of the observed and simulated flow."""

        sim_peaks = []
        obs_peaks  = []

        for storm in storms:
            daily_simulated = [f / 24 for f in 
                               self.aggregate_hourly_daily(storm[2])]
            sim_peaks.append(max(daily_simulated))

            oflows = storm[7][1]

            if self.gagestep == 1440: opeak = max(oflows)
            elif self.gagestep == 60: 
                opeak = max([f / 24 for f in 
                             self.aggregate_hourly_daily(oflows)])
            obs_peaks.append(opeak)

        observed  = sum(obs_peaks) / len(obs_peaks)
        simulated = sum(sim_peaks) / len(sim_peaks)

        return observed, simulated

    def get_storm_runoff(self, storms):
        """Returns the simulated values of the interflow and surface runoff 
        volume for the storm events."""

        interflow      = 0
        surface_runoff = 0
        for storm in storms:

            t, p, s, u, r, i, b, o = storm 

            interflow      += sum(i)
            surface_runoff += sum(r)

        return interflow, surface_runoff

    def get_calibration(self, comid = None, upcomids = [], dates = None,
                        summer_storms = None, other_storms = None,
                        verbose = True, vverbose = False):
        """Computes all the statistics for a calibration at a given comid."""

        if dates is None: start, end = self.start, self.end

        if comid is None: comid = self.comid

        # get the comids of the subbasins between "comid" and the "upcomids"

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
    
        # get the area of the upstreams

        areas = self.get_subbasin_areas(comids)
        area  = sum(areas)

        # get the number of years

        years = (end - start).total_seconds() / 86400 / 365.25
        
        # conversion factor

        if   self.hspfmodel.units == 'English': conv = 12
        elif self.hspfmodel.units == 'Metric':  conv = 1000

        factor = conv / area / years

        # get the annual avg groundwater recharge depth

        self.sim_gw = (self.get_total_groundwater(comids, dates = dates) * 
                       factor)

        # get the annual avg outflow volume depth

        self.sim_total = self.get_total_flow(comid, dates = dates) * factor

        # get the annual avg observed outflow volume depth

        self.obs_total = self.get_total_gage(dates = dates) * factor 

        # get the low flow recession rates

        self.obs_total_rec, self.sim_total_rec = \
            self.mean_recession(comid, dates = dates)

        # get the summer low flow recession rates

        self.summer_sim_rec, self.summer_obs_rec = \
            self.mean_recession(comid, dates = dates, season = 'summer')

        # get the winter low flow recession rates

        self.winter_sim_rec, self.winter_obs_rec = \
            self.mean_recession(comid, dates = dates, season = 'winter')

        # conversion factor for avg flow at a percent

        if self.hspfmodel.units == 'English': 
            f = 86400 * 365.25 / area * 12 / 43560
        elif self.hspfmodel.units == 'Metric': 
            f = 86400 * 365.25 / area / 1000

        # get the low flow average

        obs_low, sim_low = self.mean_flow(comid, 0, 50, dates = dates)
       
        self.obs_low = 0.5 * obs_low * f
        self.sim_low = 0.5 * sim_low * f

        # get the high flow average

        obs_high, sim_high = self.mean_flow(comid, 90, 100, dates = dates)

        self.obs_high = 0.1 * obs_high * f
        self.sim_high = 0.1 * sim_high * f

        # get the annual avg spring volume depth

        self.obs_spring = (self.get_seasonal_gage('spring', dates = dates) * 
                           factor)
        self.sim_spring = self.get_season_total(comid, 'spring', 
                                                dates = dates) * factor

        # get the annual avg summer volume depth

        self.obs_summer = (self.get_seasonal_gage('summer', dates = dates) * 
                           factor)
        self.sim_summer = self.get_season_total(comid, 'summer', 
                                                dates = dates) * factor

        # get the annual avg fall volume depth

        self.obs_fall = self.get_seasonal_gage('fall', dates = dates) * factor
        self.sim_fall = self.get_season_total(comid, 'fall', 
                                              dates = dates) * factor

        # get the annual avg winter volume depth

        self.obs_winter = (self.get_seasonal_gage('winter', dates = dates) * 
                           factor)
        self.sim_winter = self.get_season_total(comid, 'winter', 
                                                dates = dates) * factor

        # get the monthly observed and simulated streamflow r2 values

        self.regression = self.get_regression(comid, dates = dates)

        # get the storm events

        if summer_storms is None:
            summer_storms = self.get_storms(comid, dates = dates, 
                                            season = 'summer')
        if other_storms is None:
            other_storms  = self.get_storms(comid, dates = dates, 
                                            season = 'other')

        self.obs_storm_volume, self.sim_storm_volume = \
            self.get_storm_volumes(summer_storms + other_storms, area)

        self.obs_storm_peaks, self.sim_storm_peaks = \
            self.get_storm_peaks(summer_storms + other_storms)

        # check if runoff available

        if 'runoff' in self.hspfmodel.targets:
            self.storm_interflow, self.storm_surface_runoff = \
                self.get_storm_runoff(summer_storms + other_storms)
        else: self.storm_interflow, self.storm_surface_runoff = 0, 0

        self.obs_summer_storms, self.sim_summer_storms = \
            self.get_storm_volumes(summer_storms, area)

    def calculate_errors(self, verbose = True, vverbose = False, 
                         output = None):
        """Calculates the percent error for each of the HSPF calibration 
        parameters."""

        self.total_error = (self.sim_total - self.obs_total) / self.obs_total
        self.recession_error = ((self.sim_total_rec - self.obs_total_rec) / 
                                self.obs_total_rec)
        self.low_error = (self.sim_low - self.obs_low) / self.obs_low    
        self.high_error = (self.sim_high - self.obs_high) / self.obs_high
        self.storm_vol_error = ((self.sim_storm_volume - self.obs_storm_volume)
                                / self.obs_storm_volume)
        self.storm_peak_error = ((self.sim_storm_peaks - self.obs_storm_peaks) /
                                self.obs_storm_peaks)
        self.spring_error = ((self.sim_spring - self.obs_spring) / 
                             self.obs_spring)
        self.summer_error = ((self.sim_summer - self.obs_summer) / 
                             self.obs_summer)
        self.fall_error = (self.sim_fall - self.obs_fall)   / self.obs_fall
        self.winter_error = ((self.sim_winter - self.obs_winter) / 
                             self.obs_winter)
        self.summer_storm_error = ((self.sim_summer_storms - 
                                    self.obs_summer_storms) / 
                                   self.obs_summer_storms)
        self.season_error = abs((self.sim_summer - self.obs_summer) / 
                                self.obs_summer - 
                                (self.sim_winter - self.obs_winter) / 
                                self.obs_winter)

        if verbose:

            dr2, logdr2, dNS, logdNS, mr2, logmr2, mNS, logmNS = self.regression

            print('')
            print('HSPF calibration errors:\n')
            print('Total Runoff:{:13.1%}'.format(self.total_error))
            print('Baseflow Recession:{:7.1%}'.format(self.recession_error))
            print('Low Flows:{:16.1%}'.format(self.low_error))
            print('High Flows:{:15.1%}'.format(self.high_error))
            print('Storm Volume:{:13.1%}'.format(self.storm_vol_error))
            print('Storm Peak Flows:{:9.1%}'.format(self.storm_peak_error))

            print('Spring Flows:{:13.1%}'.format(self.spring_error))
            print('Summer Flows:{:13.1%}'.format(self.summer_error))
            print('Fall Flows:  {:13.1%}'.format(self.fall_error))
            print('Winter Flows:{:13.1%}'.format(self.winter_error))
            print('Summer Storms:{:12.1%}'.format(self.summer_storm_error))

            print('\nHSPF Model Fitting Statistics:\n')            
            print('daily flow r\u00B2:                {:6.3f}'.format(dr2))
            print('daily log-flow r\u00B2:            {:6.3f}'.format(logdr2))
            print('daily Nash-Sutcliffe:         {:6.3f}'.format(dNS))
            print('daily log-flow Nash-Sutcliffe:{:6.3f}'.format(logdNS))
            print('')
            print('NS product (optimize):        {:6.3f}'.format(dNS * logdNS))
            #print('monthly flow r\u00B2:      {:6.3f}'.format(mr2))
            #print('monthly log-flow r\u00B2:  {:6.3f}'.format(logmr2))
            #print('monthly Nash-Sutcliffe:  {:6.3f}'.format(mNS))
            print('')

        if output is not None: self.calibration_report(output = output)

    def get_sediment_balance(self, comid = None, upcomids = [], dates = None, 
                             verbose = True):
        """Returns the pertinent info for a mass balance between upcomid and
        comid in a watershed."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        if comid is None: comid = self.comid

        # make a list of the upstream comids

        upstreams = self.get_upstream_comids(comid, upcomids = upcomids)

        initials = self.get_states(date = start)
        finals   = self.get_states(date = end)

        # intial and final reach state

        init  = self.get_sediment_storage(upstreams, initials)
        final = self.get_sediment_storage(upstreams, finals)

        # fluxes

        erosion   = self.get_total_erosion(upstreams, dates = dates)
        mass_in   = self.get_total_sediment_in(comid, dates = dates)
        mass_out  = self.get_total_sediment_out(comid, dates = dates)
        obs_total = self.get_total_obs_sediment(dates = dates)

        # percentages

        reach_change  = (final - init) / init 
        sim_error     = (mass_out - obs_total) / obs_total
        balance_error = (init + erosion - mass_out - final) / init

        print('Sediment mass balance on stream reaches:\n')
        print('Initial reach mass:            {:7.0f} tonnes'.format(init))
        print('Final reach mass:              {:7.0f} tonnes'.format(final))
        print('Simulated land erosion:        {:7.0f} tonnes'.format(erosion))
        print('Simulated downstream loading:  {:7.0f} tonnes'.format(mass_out))
        print('Observed downstream loading:   {:7.0f} tonnes'.format(obs_total))
        print('')
        print('Percent error in mass balance: {:.2%}'.format(balance_error))
        print('Percent change in reach mass:  {:.2%}'.format(reach_change))
        print('Percent error in simulation:   {:.1%}'.format(sim_error))

    def get_mass_balance(self, comid = None, upcomids = [], dates = None, 
                         verbose = True):
        """Returns the pertinent info for a mass balance between upcomid and
        comid in a watershed."""

        if dates is None: start, end = self.start, self.end
        else:             start, end = dates

        if comid is None: comid = self.comid

        # make a list of the upstream comids

        upstreams = self.get_upstream_comids(comid, upcomids = upcomids)

        # get the total precipitation volume, potential evapotranspiration 
        # volume, simulated evapotranspiration volume, total flow volume, 
        # total gage volume, and inactive groundwater volume for the period

        initials = self.get_states(date = start)
        finals   = self.get_states(date = end)

        init  = self.get_volume(upstreams, initials)
        final = self.get_volume(upstreams, finals)

        prec = self.get_total_precipitation(comid, dates = dates)
        
        evap = self.get_total_evap(upstreams, dates = dates)
        flow = self.get_total_flow(comid, dates = dates)
        gw   = self.get_total_groundwater(upstreams, dates = dates)
        pet  = self.get_total_pet(upstreams, dates = dates)
        gage = self.get_total_gage(dates = dates)

        infl = self.get_total_inflow(upcomids)

        if verbose: 
            
            self.print_mass_balance(init, final, prec, evap, flow, gw, pet,
                                    infl, gage)
            
        return init, final, prec, evap, flow, gw, pet, gage 

    def print_mass_balance(self, init, final, prec, evap, flow, gw, pet, infl,
                           gage):
        """Prints mass balance summary."""

        if   self.hspfmodel.units == 'Metric':  u = 'Mm3'
        elif self.hspfmodel.units == 'English': u = 'acre-ft'

        t = (self.start.month, self.start.day, self.start.year,
             self.end.month,   self.end.day,   self.end.year)

        print('Mass balance on watershed from ' +
              '{:02d}-{:02d}-{:04d} to {:02d}-{:02d}-{:04d}:\n'.format(*t))
        print('initial watershed volume           {:>6.0f} {}'.format(init, u))
        print('final watershed volume           {:>8.0f} {}\n'.format(final,u))

        if infl is not None:
            print('total upstream inflow volume   {:>10.0f} {}'.format(infl, u))
        else: infl = 0
        print('total precipitation volume         {:>6.0f} {}'.format(prec, u))
        print('total simulated evapotranspiration {:>6.0f} {}'.format(evap, u))
        print('total simulated outflow {:>17.0f} {}'.format(flow, u))
        print('total groundwater recharge {:>14.0f} {}'.format(gw, u))
        print('total potential evapotranspiration {:>6.0f} {}'.format(pet, u))
        print('total observed outflow           {:>8.0f} {}\n'.format(gage, u))
        print('percent error in mass balance {:.2%}\n'.format( 
              abs((init + prec + infl - final - evap - flow - gw) / prec)))

    def get_hspexp_parameters(self, comid = None, upcomids = [], dates = None,
                              summer_storms = None, other_storms = None,
                              verbose = True, vverbose = False):
        """Computes all the statistics for a calibration at a given comid."""

        if dates is None: start, end = self.start, self.end

        if comid is None: comid = self.comid

        # get the comids of the subbasins between "comid" and the "upcomids"

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
    
        # get the area of the upstreams

        area = sum(self.get_subbasin_areas(comids))

        # get the number of years

        years = (end - start).total_seconds() / 86400 / 365.25
        
        # conversion factor

        if   self.hspfmodel.units == 'English': conv = 12
        elif self.hspfmodel.units == 'Metric':  conv = 1000

        factor = conv / area / years

        # get the annual avg precipitation depth

        self.obs_prec = (self.get_total_precipitation(comid, 
                                                      variable = 'precip', 
                                                      dates = dates)
                         * factor)
        self.sim_prec = (self.get_total_precipitation(comid, dates = dates)
                         * factor)

        # get the annual avg inflow volume depth

        ivol = self.get_total_inflow(upcomids)
        if ivol is not None: self.inflow = ivol * factor
        else: self.inflow = None

        # get the observed and simulated ET depth

        self.sim_evap = self.get_total_evap(comids, dates = dates) * factor
        self.obs_pet  = self.get_total_pet(comids, dates = dates) * factor

        # compute the calibration statistics for the simulation

        self.get_calibration(comid = comid, upcomids = upcomids, dates = dates,
                             summer_storms = summer_storms, 
                             other_storms = other_storms,
                             verbose = False, vverbose = False)

        # look at the calibration parameters

        if   self.hspfmodel.units == 'Metric':  u = 'mm'
        elif self.hspfmodel.units == 'English': u = 'in'

        if vverbose: 

            print('Mass Balance:\n')
            print('annual average precipitation                = %4d %s' % 
                  (self.obs_prec, u))
            print('annual average simulated evapotranspiration = %4d %s' % 
                  (self.sim_evap, u))
            print('annual average potential evapotranspiration = %4d %s' % 
                  (self.obs_pet, u))
            print('annual average groundwater recharge         = %4d %s\n' %
                  (self.sim_gw, u))
            print('Runoff Volume Comparison:\n') 
            print('annual average observed outflow  = %4d %s' % 
                  (self.obs_total, u))
            print('annual average simulated outflow = %4d %s\n' % 
                  (self.sim_total, u))
            print('Recession Rate Comparison:\n')
            print('observed  mean low flow recession rate = %5.3f'  % 
                  self.obs_total_rec)
            print('simulated mean low flow recession rate = %5.3f' % 
                  self.sim_total_rec + '\n')
            print('High flow/low flow distribution:\n')
            print('observed  mean low flow  = %6.2f m3/s'   % self.obs_low)
            print('simulated mean low flow  = %6.2f m3/s'   % self.sim_low)
            print('observed  mean high flow = %6.2f m3/s'   % self.obs_high)
            print('simulated mean high flow = %6.2f m3/s\n' % self.sim_high)
            print('Seasonal Variability:\n')
            print('annual average observed  spring volume %4d mm' % 
                  self.obs_spring)
            print('annual average simulated spring volume %4d mm' % 
                  self.sim_spring)
            print('annual average observed  summer volume %4d mm' % 
                  self.obs_summer)
            print('annual average simulated summer volume %4d mm' % 
                  self.sim_summer)
            print('annual average observed  fall   volume %4d mm' % 
                  self.obs_fall)
            print('annual average simulated fall   volume %4d mm' % 
                  self.sim_fall)
            print('annual average observed  winter volume %4d mm' % 
                  self.obs_winter)
            print('annual average simulated winter volume %4d mm\n' % 
                  self.sim_winter)

        if verbose:

            # print the cumulative flow statistics

            prec   = (self.obs_prec, self.sim_prec)
            total  = (self.obs_total, self.sim_total)
            high10 = (self.obs_high, self.sim_high)
            low50  = (self.obs_low,  self.sim_low)
            
            print('HSPF calibration statistics:\n')
            print(' ' * 37 + 'Observed   Simulated')
            print('Total Precipitation ({}/yr):         '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*prec))
            print('Total runoff ({}/yr):                '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*total))
            print('Total of highest 10% flows ({}/yr):  '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*high10))
            print('Total of lowest 50% flows ({}/yr):   '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*low50))
            print('')

            # print the evapotranspiration statistics

            evap = (self.obs_pet, self.sim_evap)

            print(' ' * 36 + 'Potential   Simulated')
            print('Evapotranspiration ({}/yr):          '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*evap))
            print('')

            # print the storm statistics

            if   self.hspfmodel.units == 'English': u2, u3 = 'ft', ''
            elif self.hspfmodel.units == 'Metric':  u2, u3 = 'm',  ' '

            total = (self.obs_storm_volume, self.sim_storm_volume)
            peaks = (self.obs_storm_peaks,  self.sim_storm_peaks)
            rec   = (self.obs_total_rec,    self.sim_total_rec)

            print(' ' * 37 + 'Observed   Simulated')
            print('Total storm volume ({}):             '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*total))
            print('Average of storm peaks ({}3/s):      '.format(u2) + u3 +
                  '{:>8.1f}{:>12.1f}'.format(*peaks))
            print('Baseflow recession rate:             ' +
                  '{:>8.3f}{:>12.3f}'.format(*rec))
            print('')
            
            # print the interflow/surface runoff statistics

            print('Total simulated storm interflow ({}):      '.format(u) +
                  '{:>8.1f}'.format(self.storm_interflow))
            print('Total simulated storm surface runoff ({}): '.format(u) +
                  '{:>8.1f}'.format(self.storm_surface_runoff))
            print('')

            # print the seasonal statistics

            summer = (self.obs_summer,        self.sim_summer)
            winter = (self.obs_winter,        self.sim_winter)
            storms = (self.obs_summer_storms, self.sim_summer_storms)

            print(' ' * 37 + 'Observed   Simulated')
            print('Summer flow volume ({}/yr):          '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*summer))
            print('Winter flow volume ({}/yr):          '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*winter))
            print('Summer storm volume ({}):            '.format(u) +
                  '{:>8.1f}{:>12.1f}'.format(*storms))
            print('')

    def calibration_report(self, output = 'calibration_report.csv'):
        """Makes a report of the calibration errors in a csv file."""

        # look at the calibration parameters

        if   self.hspfmodel.units == 'Metric':  u, u2 = 'mm', 'm'
        elif self.hspfmodel.units == 'English': u, u2 = 'in', 'ft'

        rows = [['Hydrology Calibration Report and Mass Balance'], ['']]
                
        # system-wide mass balance

        rows.append(['Mass Balance ({}/yr)'.format(u)])
        rows.append(['annual average precipitation', 
                     '{:8.1f}'.format(self.obs_prec)])
        rows.append(['annual average snow-adjusted precipitation', 
                     '{:8.1f}'.format(self.sim_prec)])

        if   self.inflow is None: infl = 0
        else:                     infl = self.inflow

        rows.append(['annual average simulated inflow', 
                     '{:8.1f}'.format(infl)])
        rows.append(['annual average simulated outflow',
                     '{:8.1f}'.format(self.sim_total)])
        rows.append(['annual average groundwater recharge',
                     '{:8.1f}'.format(self.sim_gw)])
        rows.append(['annual average simulated evapotranspiration',
                     '{:8.1f}'.format(self.sim_evap)])
        rows.append(['annual average potential evapotranspiration', 
                     '{:8.1f}'.format(self.obs_pet)])
        rows.append([''])

        rows.append(['Runoff Volume Comparison ({}/yr)'.format(u)])
        rows.append(['Parameter','Observed','Simulated', 'Percent Error'])
        rows.append(['Total runoff', '{:>8.1f}'.format(self.obs_total), 
                     '{:>12.1f}'.format(self.sim_total),
                     '{:13.1%}'.format(self.total_error)])
        rows.append(['Total of highest 10% flows', 
                     '{:>8.1f}'.format(self.obs_high),
                     '{:>12.1f}'.format(self.sim_high),
                     '{:16.1%}'.format(self.high_error)])
        rows.append(['Total of lowest 50% flows',
                     '{:>8.1f}'.format(self.obs_low),
                     '{:>12.1f}'.format(self.sim_low),
                     '{:15.1%}'.format(self.low_error)])
        rows.append(['Summer flow volume',
                     '{:>8.1f}'.format(self.obs_summer),
                     '{:>12.1f}'.format(self.sim_summer),
                     '{:13.1%}'.format(self.summer_error)])
        rows.append(['Winter flow volume',
                     '{:>8.1f}'.format(self.obs_winter),
                     '{:>12.1f}'.format(self.sim_winter),
                     '{:13.1%}'.format(self.winter_error)])
        rows.append(['Baseflow recession rate',
                     '{:>8.3f}'.format(self.obs_total_rec),
                     '{:>12.3f}'.format(self.sim_total_rec)])
        rows.append([''])

        rows.append(['Storm Analysis'])
        rows.append(['Parameter','Observed','Simulated', 'Percent Error'])
        rows.append(['Total storm volume ({})'.format(u),
                     '{:>8.1f}'.format(self.obs_storm_volume), 
                     '{:>12.1f}'.format(self.sim_storm_volume),
                     '{:13.1%}'.format(self.storm_vol_error)])
        rows.append(['Summer storm volume ({})'.format(u),
                     '{:>8.1f}'.format(self.obs_summer_storms), 
                     '{:>12.1f}'.format(self.sim_summer_storms),
                     '{:12.1%}'.format(self.summer_storm_error)])
        rows.append(['Average of storm peaks ({}3/s)'.format(u2),
                     '{:>8.1f}'.format(self.obs_storm_peaks),
                     '{:>12.1f}'.format(self.sim_storm_peaks),
                     '{:9.1%}'.format(self.storm_peak_error)])
        rows.append(['Total simulated storm interflow ({})'.format(u),
                     '{:>8.1f}'.format(self.storm_interflow)])
        rows.append(['Total simulated storm surface runoff ({})'.format(u),
                     '{:>8.1f}'.format(self.storm_surface_runoff)])
        rows.append([''])

        dr2, logdr2, dNS, logdNS, mr2, logmr2, mNS, logmNS = self.regression

        rows.append(['Calibration Statistics'])
        rows.append(['daily flow r\u00B2', '{:6.3f}'.format(dr2)])
        rows.append(['daily log-flow r\u00B2', '{:6.3f}'.format(logdr2)])
        rows.append(['daily Nash-Sutcliffe', '{:6.3f}'.format(dNS)])
        rows.append(['daily log-flow Nash-Sutcliffe', '{:6.3f}'.format(logdNS)])
        rows.append(['monthly flow r\u00B2', '{:6.3f}'.format(mr2)])
        rows.append(['monthly log-flow r\u00B2', '{:6.3f}'.format(logmr2)])
        rows.append(['monthly Nash-Sutcliffe', '{:6.3f}'.format(mNS)])
        rows.append(['monthly log-flow Nash-Sutcliffe', 
                     '{:6.3f}'.format(logmNS)])

        with open(output, 'w', newline = '') as csvfile:

            writer = csv.writer(csvfile)
            for row in rows: 
                writer.writerow(row)
            
    def plot_snow(self, comid = None, upcomids = [], tstep = 'daily', 
                  output = None, show = True):
        """Makes a plot of the snow simulation."""

        from pyhspf.core.hspfplots import plot_snow

        if comid is None: comid = self.comid

        prec  = self.get_precipitation(comid, upcomids = upcomids,tstep = tstep)
        temp  = self.get_temperatures(tstep = tstep)
        dewt  = self.get_dewpoints(tstep = tstep)
        ssnow = self.get_avg_snowpack(comid)
        osnow = self.get_obs_snowdepth()
        sfall = self.get_snowfall(comid, tstep = tstep)
        ofall = self.get_obs_snowfall()

        plot_snow(self.hspfmodel.description, prec, temp, ssnow, sfall, 
                  dewpoint = dewt, observed_snow = osnow, observed_fall = ofall,
                  output = output, show = show)

    def plot_snowcalibration(self, filter = 10, comid = None, tstep = 'daily', 
                             output = None, show = True):
        """Makes a plot of the snow simulation."""

        from pyhspf.core.hspfplots import plot_snowcalibration

        if comid is None: comid = self.comid

        # get the snowdepths

        stimes, ssnow = self.get_avg_snowpack(comid)
        otimes, osnow = self.get_obs_snowdepth()

        # remove zeros

        ssnow = [s for o, s in zip(osnow, ssnow) if o > filter]
        osnow = [o for o in osnow if o > filter]

        # get the snowfall

        stimes, sfall = self.get_snowfall(comid)
        otimes, ofall = self.get_obs_snowfall()

        # remove zeros

        sfall = [s for o, s in zip(ofall, sfall) if o > filter]
        ofall = [o for o in ofall if o > filter]

        plot_snowcalibration(self.hspfmodel.description, ssnow, osnow, 
                             sfall, ofall, output = output, show = show)

    def plot_hydrograph(self, comid = None, upcomids = [], tstep = 'daily', 
                        output = None, show = True):
        """Makes a plot of the hydrograph."""

        from pyhspf.core.hspfplots import plot_hydrograph
        
        if comid is None: comid = self.comid

        prec  = self.get_precipitation(comid, upcomids = upcomids,tstep = tstep)
        sflow = self.get_sim_flow(comid, tstep = tstep)
        oflow = self.get_obs_flow(tstep = tstep)
        pet   = self.get_pet(comid = comid, upcomids = upcomids, tstep = tstep)
        evap  = self.get_evaporation(comid, upcomids = upcomids, tstep = tstep)

        plot_hydrograph(self.hspfmodel.description, prec, sflow, evap, pet, 
                        observed_flow = oflow, tstep = tstep, output = output,
                        units = self.hspfmodel.units, show = show)

    def plot_dayofyear(self, comid = None, upcomids = [], tstep = 'daily', 
                       output = None, show = True):
        """Makes a day of year plot for the hydrology."""

        from pyhspf.core.hspfplots import plot_dayofyear

        if comid is None: comid = self.comid

        prec  = self.get_precipitation(comid, upcomids = upcomids,tstep = tstep)
        pet   = self.get_pet(comid = comid, upcomids = upcomids, tstep = tstep)
        evap  = self.get_evaporation(comid, upcomids = upcomids, tstep = tstep)

        # get runoff area (km2)

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        area   = sum(self.get_subbasin_areas(comids))

        # convert to mm or in

        if   self.hspfmodel.units == 'Metric':  conv = 1000
        elif self.hspfmodel.units == 'English': conv = 43560 / 12

        # get the observed flows

        t, oflow = self.get_obs_flow(tstep = tstep)

        oflow = t, [f * 86400 / conv / area for f in oflow]
        
        # simulated runoff

        times, vols = self.get_reach_timeseries('ROVOL', comid,tstep = 'hourly')

        vols = self.aggregate_hourly_daily(vols)

        sflow = t, [v * conv / area for v in vols]

        plot_dayofyear(self.hspfmodel.description, prec, pet, sflow, evap,
                       observed_flow = oflow, output = output, show = show)

    def plot_waterbudget(self, comid = None, upcomids = [], limits = None,
                         tstep = 'monthly', dates = None, output = None, 
                         show = True):
        """Makes a day of year or monthly plot for the water budget."""

        from pyhspf.core.hspfplots import plot_waterbudget

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

        times, vols = self.get_reach_timeseries('ROVOL', comid, tstep ='hourly',
                                                dates = dates)

        # convert to daily

        times = self.get_timeseries(tstep = 'daily', dates = dates)
        vols  = self.aggregate_hourly_daily(vols)

        if tstep == 'monthly':
            
            times, vols = self.aggregate_daily_monthly(times, vols) 

        sflow = times, [v * conv / area for v in vols]
        
        plot_waterbudget(self.hspfmodel.description, prec, pet, evap, sflow, gw,
                         limits = limits, tstep = tstep, output = output, 
                         show = show)

    def plot_allwaterbudgets(self, comid = None, upcomids = [], years = None, 
                             output = None, show = True):
        """Makes annual plots for the water balance."""

        if comid is None: comid = self.comid

        if years is None:
            if self.start.month == 10 and self.start.day == 1:
                years = range(self.start.year, self.end.year)
            else:
                years = range(self.start.year + 1, self.end.year)

        if not os.path.isdir(output) and output is not None: 
            os.mkdir(output)

        # get the maximum precipitation to use for all limits

        prec  = self.get_precipitation(comid, upcomids = upcomids,
                                       tstep = 'monthly', dates = None)

        mprec = self.roundup(max(prec[1]), 50)
        limits = [0, mprec]

        for y in years:

            if output is not None: f = '{}/{}'.format(output, y)
            else:                  f = None

            dates = datetime.datetime(y - 1, 10, 1), datetime.datetime(y, 10, 1)

            try:
                self.plot_waterbudget(comid = comid, upcomids = upcomids,
                                      tstep = 'monthly', limits = limits,
                                      dates = dates, show = show, output = f)

            except: 
                print('warning, unable to plot water budget for {}'.format(y))

    def plot_tss(self, comid = None, tstep = 'daily', output = None, 
                 show = True):
        """Makes a plot of the hydrograph."""

        from pyhspf.core.hspfplots import plot_tss
        
        if comid is None: comid = self.comid

        prec  = self.get_precipitation(comid, tstep = tstep)
        oflow = self.get_obs_flow(tstep = tstep)
        otss  = self.get_waterquality_data('TSS')
        sflow = self.get_sim_flow(comid, tstep = tstep)
        stss  = self.get_reach_timeseries('TSS', comid, tstep = tstep)

        plot_tss(self.hspfmodel.description, prec, oflow, sflow,
                 stss, observed_tss = otss, output = output, title = None, 
                 verbose = False)

    def plot_solids(self, comid = None, tstep = 'daily', output = None, 
                    show = False):
        """Makes a plot of the hydrograph."""

        from pyhspf.core.hspfplots import plot_solids
        
        if comid is None: comid = self.comid

        prec  = self.get_precipitation(comid, tstep = tstep)
        sflow = self.get_sim_flow(comid, tstep = tstep)
        oflow = self.get_obs_flow(tstep = tstep)
        sand  = self.get_reach_timeseries('SAND', comid, tstep = tstep)
        silt  = self.get_reach_timeseries('SILT', comid, tstep = tstep)
        clay  = self.get_reach_timeseries('CLAY', comid, tstep = tstep)
        shear = self.get_reach_timeseries('TAU',  comid, tstep = tstep)
        otss  = self.get_waterquality_data('TSS')

        # get the min and max shear stress for the reach

        reach_comids = [r.comid for r in self.hspfmodel.rchreses]

        reach = self.hspfmodel.rchreses[reach_comids.index(comid)]

        taus = [reach.TAUCDsilt, reach.TAUCSsilt]

        plot_solids(self.hspfmodel.description, prec, oflow, sflow,
                    sand, silt, clay, shear, observed_tss = otss, taus = taus, 
                    output = output, title = None, verbose = False)

    def plot_erosion(self, comid = None, tstep = 'daily', cumulative = True,
                     output = None, show = True):
        """Makes a plot of the hydrograph. Can be either daily or monthly,
        and can show the total erosion (cumulative) or the marginal erosion."""

        from pyhspf.core.hspfplots import plot_erosion

        if comid is None: comid = self.comid

        times, precipitation = self.get_precipitation(comid, tstep = tstep)
        times, storage       = self.get_surface_storage(comid, tstep = tstep)
        times, runoff        = self.get_surface_runoff(comid, tstep = tstep)
        times, detach        = self.get_detach(comid, tstep = tstep)
        times, erosion       = self.get_erosion(comid, tstep = tstep)

        if cumulative:
            erosion = [sum(erosion[:i]) for i in range(len(erosion))]

        plot_erosion(self.hspfmodel.description, times, precipitation, 
                     storage, runoff, detach, erosion, cumulative = cumulative,
                     units = self.hspfmodel.units, output = output, show = show)

    def plot_sediment_loading(self, comid = None, tstep = 'daily',
                              cumulative = True, output = None, show = True):

        from pyhspf.core.hspfplots import plot_sediment_loading
        
        if comid is None: comid = self.comid

        times, prec  = self.get_precipitation(comid, tstep = tstep)
        times, sflow = self.get_sim_flow(comid, tstep = tstep)
        times, sload = self.get_sediment_loading(comid, tstep = tstep)
        times, oload = self.get_obs_sediment_load(tstep = tstep)
        times, stss  = self.get_reach_timeseries('TSS', comid, tstep = tstep)

        otss  = self.get_waterquality_data('TSS')
        oflow = self.get_obs_flow(tstep = tstep)

        if cumulative:
            oload = times, [sum(oload[:i]) for i in range(len(oload))]
            sload = [sum(sload[:i]) for i in range(len(sload))]

        plot_sediment_loading(self.hspfmodel.description, times, prec, sflow, 
                              stss, sload, observed_flows = oflow, 
                              observed_tss = otss, observed_loads = oload, 
                              cumulative = cumulative,
                              output = output, show = show)

    def plot_runoff(self, comid = None, tstep = 'daily', output = None, 
                    show = True):
        """Makes a plot of the runoff components."""

        from pyhspf.core.hspfplots import plot_runoff

        if comid is None: comid = self.comid

        prec  = self.get_precipitation(comid, tstep = tstep)
        sflow = self.get_sim_flow(comid, tstep = tstep)
        oflow = self.get_obs_flow(tstep = tstep)

        times, runoff, interflow, baseflow = self.get_runoff_flows(comid, tstep
                                                                   = tstep)

        if len(self.hspfmodel.inlets) > 0:
            inflow = self.get_inflow(tstep = tstep)
        else:
            inflow = None

        other_storms  = [(s[0][0], s[0][-1]) 
                         for s in self.get_storms(comid, season = 'other')]
        summer_storms = [(s[0][0], s[0][-1])
                         for s in self.get_storms(comid, season = 'summer')]

        plot_runoff(self.hspfmodel.description, prec, sflow, baseflow, 
                    interflow, runoff, inflow = inflow, observed_flow = oflow, 
                    other_storms = other_storms, summer_storms = summer_storms,
                    units = self.hspfmodel.units, output = output, show = show)

    def plot_storms(self, comid = None, upcomids = [], season = 'all', 
                    tstep = 'daily', output = None, show = True):
        """Makes plots of all the storms."""

        from pyhspf.core.hspfplots import plot_storm

        if comid is None: comid = self.comid

        if season not in ['summer', 'other', 'all']:
            print('Warning: unknown season specified.\n')
            return None

        if season == 'all':
            
            # get all the storms

            other_storms  = self.get_storms(comid, season = 'other', 
                                            tstep = tstep)
            summer_storms = self.get_storms(comid, season = 'summer', 
                                            tstep = tstep)

            # group them chronologically

            storms = []
            for other, summer in zip(other_storms, summer_storms):
                storms.append(other)
                storms.append(summer)

        else: storms = self.get_storms(comid, season = season, tstep = tstep)

        # conversion -- seconds per interval, area in km2 or acres, 
        #               depth in mm or in

        comids = self.get_upstream_comids(comid, upcomids = upcomids)
        area = sum(self.get_subbasin_areas(comids))
        
        if   self.hspfmodel.units == 'Metric':  
            conv = self.hspfmodel.tstep * 60 / 1000
        elif self.hspfmodel.units == 'English': 
            conv = self.hspfmodel.tstep * 60 * 12 / 43560

        for storm in storms:
        
            t, prec, sim, inflow, runoff, interflow, baseflow, obs = storm

            # convert the runoff components to m3/s or ft3/s

            r = [f * area / conv for f in runoff]
            i = [f * area / conv for f in interflow]
            b = [f * area / conv for f in baseflow]

            if output is not None: out = output + '{}'.format(t[0])[:10]
            else:                  out = None
            
            if tstep == 'hourly': msize = 4
            else:                 msize = 10

            plot_storm(self.hspfmodel.description, (t, prec), (t, sim), 
                       b, i, r, inflow = inflow, observed_flow = obs, 
                       show = show, units = self.hspfmodel.units, 
                       markersize = msize, output = out)

    def plot_calibration(self, comid = None, output = None, show = True, 
                         verbose = False):
        """Makes a series of subplots of the calibration statistics."""

        from pyhspf.core.hspfplots import plot_calibration

        if comid is None: comid = self.comid

        stimes, d_sflow = self.get_sim_flow(comid, tstep = 'daily')
        otimes, d_oflow = self.get_obs_flow(tstep = 'daily')

        # deal with missing data

        d_sflow = [d_sflow[stimes.index(t)] 
                   for t, f in zip(otimes, d_oflow) 
                   if t in stimes and f is not None]
        d_oflow = [d_oflow[otimes.index(t)] 
                   for t, f in zip(otimes, d_oflow) 
                   if f is not None]

        # monthly

        otimes, m_oflow = self.get_obs_flow(tstep = 'monthly')
        stimes, m_sflow = self.get_sim_flow(comid, tstep = 'monthly')

        if otimes[0].day != 1: otimes, m_oflow = otimes[1:], m_oflow[1:]

        # deal with missing data

        m_sflow = [m_sflow[stimes.index(t)] for t, f in zip(otimes, m_oflow) 
                   if t in stimes and f is not None]
        m_oflow = [f for t, f in zip(otimes, m_oflow) if f is not None]

        plot_calibration(self.hspfmodel.description, d_sflow, d_oflow, 
                         m_sflow, m_oflow, output = output, show = show)

    def close(self, save_states = None):
        """Saves the end states to a pickle file and closes the WDM files."""

        if save_states is not None: 
            self.hspfmodel.set_states(self.get_states())
            with open(save_states, 'wb') as f: pickle.dump(self.hspfmodel, f)

        self.wdm.close(self.wdminfile)
        self.wdm.close(self.wdmoutfile)
        self.wdm.close_message()

