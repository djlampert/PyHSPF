# etcalculator.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/15/2015
#
# contains the ETCalculator class that can be used to compute evapotranspiration
# time series from other time series with models including the daily and hourly
# Penman-Monteith Equation and the FAO crop coefficient model.

import os, pickle, datetime, calendar, numpy

from matplotlib import pyplot, dates, ticker
from .crop_coefficient import calculate_cropPET

class ETCalculator:
    """
    A class that can be used to compute various evapotranspiration time series
    from other time series including hourly and daily reference 
    evapotranspiration and potential evapotranspiration for a number of crops.
    """

    def __init__(self):

        # create some dictionary structures to store important timeseries

        self.daily = {
            'tmin':        None,
            'tmax':        None,
            'temperature': None,
            'dewpoint':    None,
            'humidity':    None,
            'wind':        None,
            'solar':       None,
            'RET':         None,
            }

        self.hourly = {
            'temperature': None,
            'dewpoint':    None,
            'humidity':    None,
            'wind':        None,
            'solar':       None,
            'RET':         None,
            }

        self.dayoftheyear = {
            'temperature': None,
            'dewpoint':    None,
            'humidity':    None,
            'wind':        None,
            'solar':       None,
            'RET':         None,
            }

        self.longitude = None
        self.latitude  = None
        self.elevation = None

    def add_location(self, 
                     longitude, 
                     latitude, 
                     elevation,
                     ):
        """
        Adds the longitude, latitude, and elevation 
        (these are needed for some calculations).
        """

        self.longitude = longitude
        self.latitude  = latitude
        self.elevation = elevation

    def add_timeseries(self, 
                       tstype, 
                       tstep, 
                       start, 
                       data,
                       ):
        """
        Adds a timeseries of type "tstype" (e.g., tmax) with time step
        "tstep" (daily, hourly), start date "start, and list of values "data"
        to use for ET calculations.
        """

        # make sure the time step is valid

        if   tstep == 'daily'  or tstep == 1440: d, ts = self.daily, 1440
        elif tstep == 'hourly' or tstep == 60:   d, ts = self.hourly, 60
        else:
            print('error: unknown time step {} specified'.format(tstep))
            print('valid choices are "hourly" and "daily"\n')
            raise

        # make sure the time series type is valid

        if tstype not in d:
            print('error: unknown time series type {} specified'.format(tstype))
            print('valid choices are ' + ', '.join(d) + '\n')
            raise
            
        d[tstype] = start, data

    def time_round(self, d):
        """Rounds a datetime.datetime instance to the nearest hour."""

        yr, mo, da, hr, mi = d.timetuple()[:5]

        if 30 <= d.minute:  # round to the next hour

            r = (datetime.datetime(yr, mo, da, hr) + 
                 datetime.timedelta(hours = 1))

        else:

            r = datetime.datetime(yr, mo, da, hr)

        return r

    def rad(self, d): 
        """Degrees to radians."""

        return (numpy.pi / 180 * d)

    def sun(self, 
            time,
            zenith = 90.83, 
            nearest_hour = False,
            ):
        """
        Estimates the sunrise and sunset time for a given latitude, longitude,
        and date and optionally rounds it to the nearest hour. Algorithm
        is derived from "Almanac for Computers," 1990, Nautical Almanac Office,
        United States Naval Observatory, Washington DC 20392.
        """

        # calculate day of the year

        N = time.timetuple().tm_yday

        # convert longitude to hour value and calculate approximate time

        hour = self.longitude / 15

        # rise

        t = N + (6 - hour) / 24

        # calculate mean anomaly

        M = (360 / 365.25 * t) - 3.289

        # calculate sun's true longitude

        L = (M + 1.916 * numpy.sin(self.rad(M)) + 
             (0.02 * numpy.sin(2 * self.rad(M))) + 282.634)

        # adjust into 0, 360 range

        if L < 0:   L+=360
        if L > 360: L-=360
             
        # calculate right ascension

        RA = 180 / numpy.pi * numpy.arctan(0.91764 * numpy.tan(self.rad(L)))

        # ensure RA is in the same quadrant as L

        Lquadrant  = (numpy.floor(L  / 90)) * 90
        RAquadrant = (numpy.floor(RA / 90)) * 90

        RA = RA + (Lquadrant - RAquadrant)

        # convert to hours

        RA = RA / 15

        # calculate the solar declination

        sinDec = 0.39782 * numpy.sin(self.rad(L))
        cosDec = numpy.cos(numpy.arcsin(sinDec))

        # calculate the solar local hour angle

        cosH = ((numpy.cos(self.rad(zenith)) - sinDec * 
                 numpy.sin(self.rad(self.latitude))) / 
                (cosDec * numpy.cos(self.rad(self.latitude))))

        # rise

        H = 360 - 180 / numpy.pi * numpy.arccos(cosH)

        H = H / 15

        # calculate local mean time of rising

        T = H + RA - 0.06571 * t - 6.622

        # adjust back to UTC

        UT = T - hour

        # adjust to local time zone (this is not super accurate, but for the 
        # purposes of ET estimation really doesn't warrant detailed estimation)

        offset = round(self.longitude / 15)

        # daily saving time

        if time.month < 3 or 10 < time.month: offset -= 1

        localT = UT + offset

        # adjust to [0, 24] as needed

        if localT < 0:  localT += 24
        if localT > 24: localT -= 24
            
        t = datetime.timedelta(hours = localT)

        sunrise = ':'.join(str(t).split(':')[:2])

        sunrise = datetime.datetime(*time.timetuple()[:3]) + t

        # now do sunset

        t = N + (18 - hour) / 24

        # calculate mean anomaly

        M = (360 / 365.25 * t) - 3.289

        # calculate sun's true longitude

        L = (M + 1.916 * numpy.sin(self.rad(M)) + 
             (0.02 * numpy.sin(2 * self.rad(M))) + 282.634)

        # adjust into 0, 360 range

        if L < 0:   L+=360
        if L > 360: L-=360
             
        # calculate right ascension

        RA = 180 / numpy.pi * numpy.arctan(0.91764 * numpy.tan(self.rad(L)))

        # ensure RA is in the same quadrant as L

        Lquadrant  = (numpy.floor(L  / 90)) * 90
        RAquadrant = (numpy.floor(RA / 90)) * 90

        RA = RA + (Lquadrant - RAquadrant)

        # convert to hours

        RA = RA / 15

        # calculate the solar declination

        sinDec = 0.39782 * numpy.sin(self.rad(L))
        cosDec = numpy.cos(numpy.arcsin(sinDec))

        # calculate the solar local hour angle

        cosH = ((numpy.cos(self.rad(zenith)) - sinDec * 
                 numpy.sin(self.rad(self.latitude))) / 
                (cosDec * numpy.cos(self.rad(self.latitude))))

        # set

        H = 180 / numpy.pi * numpy.arccos(cosH) / 15

        # calculate local mean time of rising

        T = H + RA - 0.06571 * t - 6.622

        # adjust back to UTC

        UT = T - hour

        # adjust to local time zone

        localT = UT + offset

        # adjust to [0, 24] as needed

        if localT < 0:  localT += 24
        if localT > 24: localT -= 24
            
        t = datetime.timedelta(hours = localT)

        sunset = ':'.join(str(t).split(':')[:2])

        sunset = datetime.datetime(*time.timetuple()[:3]) + t

        if nearest_hour: 
            
            sunrise, sunset = self.time_round(sunrise), self.time_round(sunset)

        return sunrise, sunset

    def is_daytime(self, times):
        """
        Determines whether the sun is up or not for each values in "times"
        """

        sunrise, sunset = zip(*[self.sun(t) for t in times])

        return [r <= t and t < s for t, r, s in zip(times, sunrise, sunset)]

    def interpolate_temperatures(self, start, end):
        """
        Develops an hourly time series of temperatures from the daily min 
        and max.  Assumes a sinusoidal profile from the min temp to the max 
        from 6 am to 4 pm.
        """

        if self.daily['tmin'] is None or self.daily['tmax'] is None:

            print('error: no daily temperature data available\n')
            raise

        # make a list of times to use to calculate values

        times = [start + datetime.timedelta(days = 1) * i 
                 for i in range((end-start).days)]

        try:

            s, d = self.daily['tmin']

            # find the index of the requested start and end times

            i = (start - s).days
            j = (end - s).days

            # make the time series (if the data are available)

            tmin = d[i:j]
            
            s, d = self.daily['tmax']

            # find the index of the requested start and end times

            i = (start - s).days
            j = (end - s).days

            # make the time series (if the data are available)

            tmax = d[i:j]
            
        except:

            print('\nerror: temperature data unavailable ' +
                  'for requested period {} -- {}\n'.format(start, end))
            raise
        
        #fill in the first 6 hours with tmin

        hourly = [tmin[0] for i in range(6)]

        # iterate through to the last day

        for mi1, ma, mi2 in zip(tmin[:-1], tmax[:-1], tmin[1:]):
            
            # the day "starts" at 6 am, increases to tmax at 4 pm, then
            # declines to the next day's tmin.  assumes sinuosoidal shape

            if mi1 is None or ma is None: 
                for i in range(10): hourly.append(None)
            else:
                mid = 0.5 * (ma + mi1)
                amp = 0.5 * (ma - mi1)
                for i in range(10): 
                    hourly.append(mid + amp * numpy.sin((i-5) * numpy.pi / 10))
                
            if mi2 is None or ma is None:
                for i in range(14): hourly.append(None)
            else:
                mid = 0.5 * (ma + mi2)
                amp = 0.5 * (ma - mi2)
                for i in range(14): 
                    hourly.append(mid + amp * numpy.cos(i * numpy.pi / 14))

        # add the last day

        if tmin[-1] is None or tmax[-1] is None:
            for i in range(18): hourly.append(None)
        else:
            for i in range(10):
                hourly.append(tmin[-1] + (tmax[-1] - tmin[-1]) * i / 10)
        
            # assume tmax to the end of the day

            for i in range(8): hourly.append(tmax[-1])

        return hourly

    def atmosphere_pressure(self):
        """
        Estimates the atmospheric pressure (kPa) from the elevation (m) using a 
        simplified ideal gas law.
        """

        if self.elevation is None:
            print('error: elevation not specified\n')
            raise

        return 101.3 * ((293.0 - 0.0065 * self.elevation) / 293.0)**5.26

    def vapor_pressure(self, T):
        """
        Estimates the water vapor pressure (kPa) at a given temperature (C).
        """

        return 0.6108 * numpy.exp((17.27 * T) / (T + 237.3))

    def wind_correction(self, 
                        u1, 
                        z = 10,
                        ):
        """
        Estimates the wind speed at z = 2 m from the wind speed at height z 
        (the typical AMOS weather stations have a height of 10 meters).
        """

        return u1 * 4.87 / numpy.log(67.8 * z - 5.42)

    def get_Cd(self, 
               times, 
               Cday, 
               Cnight,
               ):
        """
        Provides an array of the values of the denominator coefficient if a 
        list is provided, otherwise just provides the appropriate day or 
        night value.
        """

        Cd = numpy.array([Cday if t else Cnight 
                          for t in self.is_daytime(times)])

        return Cd

    def get_soil(self, 
                 times, 
                 Gday, 
                 Gnight,
                 ):
        """
        Provides an array of the values of the soil heat flux if a list is 
        provided, otherwise just provides the appropriate day or night value.
        """

        Gd = numpy.array([Gday if t else Gnight 
                          for t in self.is_daytime(times)])

        return Gd

    def daily_radiation(self, 
                        times, 
                        solar, 
                        Tmin, 
                        Tmax, 
                        Pv, 
                        albedo = 0.23,
                        sigma = 4.903e-9,
                        ):
        """
        Estimates the net radiation (MJ/m2/day) at the crop surface assuming 
        the crop is grass.

        lat    -- latitude
        elev   -- elevation
        dates  -- datetime array
        solar  -- measured solar radiation
        T      -- average temperature (K)
        Pv     -- vapor pressure (kPa)
        cloud  -- cloud cover fraction
        albedo -- assumed value typical for grass
        sigma  -- boltzmann constant (MJ m-2 d-1)
        """

        # make sure the location has been supplied

        location = self.longitude, self.latitude, self.elevation
        
        if any([l is None for l in location]):
            print('error: location has not been specified\n')
            raise

        # convert the dates to julian days

        julian = numpy.array([t.timetuple().tm_yday for t in times])

        # calculate the solar declination (rad)

        sd = 23.45 * self.rad(numpy.cos(2 * numpy.pi / 365 * (julian - 172)))

        # calculate the inverse relative distance Earth-Sun

        irl = 1 + 0.033 * numpy.cos(2 * numpy.pi / 365 * julian)

        # calculate the hour angle at sunset (rad)

        sha = numpy.arccos(-numpy.tan(self.rad(self.latitude)) *
                           numpy.tan(sd))

        # calculate the extraterrestrial radiation

        et_rad = 37.59 * irl * (sha * 
                                numpy.sin(self.rad(self.latitude)) 
                                * numpy.sin(sd) + 
                                numpy.cos(self.rad(self.latitude)) * 
                                numpy.cos(sd) * numpy.sin(sha))

        # calculate the clear day solar radiation

        clear = (0.00002 * self.elevation + 0.75) * et_rad

        # shortwave radiation

        Rns = solar * (1 - albedo)

        # longwave radiation

        Rnl = (sigma * 0.5 * (Tmin**4 + Tmax**4) * 
               (0.34 - 0.139 * numpy.sqrt(Pv)) * 
               (1.35 * solar / clear - 0.35))

        return Rns - Rnl

    def hourly_radiation(self,
                         times, 
                         solar, 
                         T, 
                         Pv, 
                         albedo = 0.23,
                         sigma = 2.042e-10,
                         Gsc = 4.92,
                         t1  = 1,
                         ):
        """
        Estimates the net radiation (MJ/m2/hour) at the crop surface assuming 
        the crop is grass.

        times  -- list of datetime.datetime instances
        solar  -- measured solar radiation
        T      -- average temperature (K)
        Pv     -- vapor pressure (kPa)
        albedo -- assumed value typical for grass
        sigma  -- boltzmann constant (MJ m-2 h-1)
        Gsc    -- solar constant (MJ m-2 h-1)
        t1     -- time step length (1 hour)
        """

        # convert dates to julian days and figure out if the sun is up or not

        julian  = numpy.array([t.timetuple().tm_yday for t in times])
        ts      = numpy.array([t.timetuple().tm_hour for t in times])
        daytime = numpy.array([1 if t else 0 for t in self.is_daytime(times)])

        # calculate the solar declination (rad)

        d = self.rad(0.409 * numpy.cos(2 * numpy.pi / 365 * julian - 1.39))

        # calculate the inverse relative distance factor

        dr = 1 + 0.033 * numpy.cos(2 * numpy.pi / 365 * julian)

        # calculate the seasonal correction for solar time

        b  = 2 * numpy.pi * (julian - 81) / 364
        Sc = (0.1645 * numpy.sin(2 * b) - 0.1255 * numpy.cos(b) - 
              0.025 * numpy.sin(b))

        # convert the latitude to radians

        p = self.rad(self.latitude)

        # calculate the longitude at the center of the time zone

        Lm = self.longitude
        Lz = 15 * round(self.longitude / 15)

        # calculate the sunset hour angle

        ws = numpy.arccos(-1 * numpy.tan(p) * numpy.tan(d))

        # calculate the solar time angle at the midpoint of the hour

        w = numpy.pi / 12 * ((ts + 0.5 + (Lm - Lz) / 15 + Sc) - 12)

        # calculate the solar time angles

        w1 = w - numpy.pi * t1 / 24
        w2 = w + numpy.pi * t1 / 24

        # overwrite the values if the sun is down

        w1[numpy.where(w1 < -ws)] = -ws
        w2[numpy.where(w2 < -ws)] = -ws
        w1[numpy.where(w1 >  ws)] =  ws
        w2[numpy.where(w2 >  ws)] =  ws
        w1[numpy.where(w1 >  w2)] =  w2

        # calculate the extraterrestrial radiation for the hour

        c = 12 / numpy.pi * Gsc * dr
        Ra = c * ((w2 - w1) * numpy.sin(p) * numpy.sin(d) +
                  numpy.cos(p) * numpy.cos(d) * (numpy.sin(w2) - numpy.sin(w1)))

        # calculate the clear day solar radiation

        Rso = (0.00002 * self.elevation + 0.75) * Ra

        # calculate the angle of the sun above the horizon at the midpoint
        # of the hour period

        B = numpy.arcsin(numpy.sin(p) * numpy.sin(d) + 
                         numpy.cos(p) * numpy.cos(d) * numpy.cos(w))

        # calculate ratio of the clear sky radiation to the actual radiation

        fcd = numpy.array([1.35 * (s / c) - 0.35 
                           if 0 < c and 0.3 < s / c and 17 < a 
                           else 0.05
                           for s, c, a in zip(solar, Rso, B)])

        Rns = solar * (1 - albedo)
        Rnl = sigma * 0.5 * T**4 * (0.34 - 0.139 * numpy.sqrt(Pv)) * fcd

        return Rns - Rnl

    def penman_daily(self,
                     start,
                     end,
                     albedo = 0.23, 
                     soil = 0, 
                     Cn = 900, 
                     Cd = 0.34, 
                     wheight = 10, 
                     verbose = True,
                     ):
        """
        Calculates the reference evapotransporation (RET) in mm for a given day
        using the Penman-Monteith equation. Equations from ASCE.

        start   -- datetime.datetime
        end     -- datetime.datetime
        tmin    -- minimum air temperature (C)
        tmax    -- maximum air temperature (C)
        dew     -- dew point temperature (C)
        rad     -- solar radiation (W m-2)
        wind    -- wind speed (m s-1)
        albedo  -- assumed value typical for grass
        soil    -- ground surface heat flux
        Cn      -- numerator coefficient (literature grass: 900, alfalfa: 1600)
        Cd      -- denominator coefficient (grass: 0.34, alfalfa: 0.38)
        wheight -- measurement height for the wind speed
        """

        if verbose: 

            print('calculating daily reference evapotranspiration...\n')

        # check the location has been supplied

        location = self.longitude, self.latitude, self.elevation

        if any([v is None for v in location]):

            print('error: location must be specified\n')
            raise
        
        # check that all the time series are present

        required = 'solar', 'tmin', 'tmax', 'dewpoint', 'wind'

        if any([self.daily[ts] is None for ts in required]):

            for ts in required:
                if self.daily[ts] is None:
                    print('error: {} data unavailable'.format(ts))
            raise

        # make a list of times to use to calculate values

        times = [start + datetime.timedelta(days = 1) * i 
                 for i in range((end-start).days)]

        # create a dictionary of numpy arrays for the requested period

        data = {}

        for ts in required:

            s, d = self.daily[ts]

            # find the index of the requested start and end times

            i = (start - s).days
            j = (end - s).days

            # make the time series (if the data are available)

            try:
                data[ts] = numpy.array(d[i:j])
            except:
                print('\nerror: {} data unavailable '.format(ts) +
                      'for requested period {} -- {}\n'.format(start, end))
                raise

        # convert the units of solar radiation from W/m2 to MJ/m2/day

        solar = data['solar'] * 86400 / 10**6

        # check and replace dewpoints lower than tmin (physically impossible)

        dewpoint = numpy.minimum(data['tmin'], data['dewpoint'])

        # correct the wind speed to 2 meters from 10 meters

        u2 = self.wind_correction(data['wind'], z = wheight)

        # estimate the atmospheric pressure at the given elevation

        P = self.atmosphere_pressure()

        # estimate the average temperature (C) for the day

        tavg = (data['tmin'] + data['tmax']) / 2

        # estimate the vapor pressure (kPa) from the dew point

        Pv = self.vapor_pressure(data['dewpoint'])

        # estimate the average saturation vapor pressure (kPa)

        Ps = (self.vapor_pressure(data['tmin']) + 
              self.vapor_pressure(data['tmax'])) / 2

        # estimate slope of vapor pressure curve (kPa C-1) at mean temperature

        d = 4098 * self.vapor_pressure(tavg) / (tavg + 237.3)**2 

        # convert C to K

        T    = tavg + 273.15
        Tmin = data['tmin'] + 273.15
        Tmax = data['tmax'] + 273.15
 
        # get the net radiation

        rnet = self.daily_radiation(times, solar, Tmin, Tmax, Pv)

        # estimate the psychrometric constant (kPa C-1)
        # equation is gamma = cp(T) * P * MWair / latent_heat / MWwater
        # this uses cp for T = 20 C

        g = 0.000665 * P

        # estimate the reference evapotranspiration

        RET = ((0.408 * (rnet - soil) * d  + Cn * u2 / T * (Ps - Pv) * g) / 
               (d + g * (1 + Cd * u2)))

        self.daily['RET'] = RET

        return RET

    def penman_hourly(self,
                      start,
                      end,
                      albedo = 0.23,
                      Cn = 37, 
                      Cday = 0.24, 
                      Cnight = 0.96, 
                      Gday = 0.1, 
                      Gnight = 0.5,
                      wheight = 10, 
                      verbose = True,
                      ):
        """
        Calculates the potential evapotransporation (PET) in mm for an hourly
        timeseries using the Penman-Monteith equation. Equations from:
        
        American Society of Civil Engineers (ASCE)
        Task Committee on Standardization of Reference Evapotranspiration
        Environmental and Water Resources Institute
        THE ASCE Standardized Reference Evapotranspiration Equation
        January 2005 Final Report

        http://www.kimberly.uidaho.edu/water/asceewri/ascestzdetmain2005.pdf

        start   -- datetime.datetime
        end     -- datetime.datetime
        temp    -- temperature (C)
        dew     -- dew point temperature (C)
        rad     -- solar radiation (W m-2)
        wind    -- wind speed (m s-1)
        albedo  -- estimated for grass reference crop
        Cn      -- numerator coefficient
        Cday    -- daytime denominator coefficient
        Cnight  -- nighttime denominator coefficient
        Gday    -- ground heat flux during the day
        Gnight  -- ground heat flux during the night
        """

        if verbose: 

            print('calculating hourly reference evapotranspiration...\n')

        # check the location has been supplied

        location = self.longitude, self.latitude, self.elevation

        if any([v is None for v in location]):

            print('error: location must be specified\n')
            raise
        
        # check that all the time series are present

        required = 'solar', 'temperature', 'dewpoint', 'wind'

        if any([self.hourly[ts] is None for ts in required]):

            for ts in required:
                if self.hourly[ts] is None:
                    print('error: {} data unavailable'.format(ts))
            raise

        # make a list of times to use to calculate values

        times = [start + datetime.timedelta(hours = 1) * i 
                 for i in range((end-start).days * 24)]

        # create a dictionary of numpy arrays for the requested period

        data = {}

        for ts in required:

            s, d = self.hourly[ts]

            # find the index of the requested start and end times

            i = (start - s).days * 24
            j = (end - s).days * 24

            # make the time series (if the data are available)

            try:
                data[ts] = numpy.array(d[i:j])
            except:
                print('\nerror: {} data unavailable '.format(ts) +
                      'for requested period {} -- {}\n'.format(start, end))
                raise

        # convert the units of solar radiation from W/m2 to MJ/m2/hour

        solar = data['solar'] * 3600 / 10**6

        # check and replace dewpoints lower than tmin (physically impossible)

        dewpoint = numpy.minimum(data['temperature'], data['dewpoint'])

        # correct the wind speed to 2 meters from 10

        u2 = self.wind_correction(data['wind'], z = wheight)

        # estimate the atmospheric pressure at the given elevation

        P = self.atmosphere_pressure()

        # estimate the vapor pressure (kPa) from the dew point

        Pv = self.vapor_pressure(dewpoint)

        # estimate the average saturation vapor pressure (kPa)

        Ps = self.vapor_pressure(data['temperature'])

        # estimate the vapor pressure curve slope (kPa C-1) at mean temperature

        d = (4098 * self.vapor_pressure(data['temperature']) / 
             (data['temperature'] + 237.3)**2)

        # convert C to K

        T = data['temperature'] + 273.15
 
        # get the net radiation

        rnet = self.hourly_radiation(times, solar, T, Pv)

        # get the soil heat flux

        soil = rnet * self.get_soil(times, Gday, Gnight)

        # use the appropriate Cd values depending on whether it's day or night

        Cd = self.get_Cd(times, Cday, Cnight)

        # estimate the psychrometric constant (kPa C-1)
        # equation is gamma = cp(T) * P * MWair / latent_heat / MWwater
        # this uses cp for water at T = 20 C

        g = 0.000665 * P

        # estimate the reference evapotranspiration

        RET = ((0.408 * (rnet - soil) * d  + Cn * u2 / T * (Ps - Pv) * g) / 
               (d + (g * (1 + Cd * u2))))

        self.hourly['RET'] = RET

        if verbose: 

            print('finished calculating reference evapotranspiration\n')

        return RET

    def average(self, values):
        """Returns the average values of the list."""

        try: 
            return sum(values) / len(values)
        except:
            return None

    def dayofyear(self, dates, values):
        """Returns the day of the water year average for the timeseries."""

        year = dates[0].year
        while not calendar.isleap(year): year += 1
        delta = datetime.timedelta(days = 1)
        wateryear = [datetime.date(year - 1, 10, 1) + i * delta 
                     for i in range(366)]

        watervalues = [self.average([v for t, v in zip(dates, values)
                                     if t.month == day.month and 
                                     t.day == day.day and
                                     v is not None])
                       for day in wateryear]

        return watervalues

    def plotET(self,
               start = None,
               end   = None,
               stations = None,
               title = None,
               axsize = 11, 
               output = None, 
               show = False, 
               verbose = True,
               ):

        if verbose: print('plotting daily reference evapotranspiration...\n')

        # error handling

        required = 'tmin', 'tmax', 'dewpoint', 'wind', 'solar', 'RET'
        if any([r in self.daily for r in required]):

            for r in required:

                if not r in self.daily:

                    print('error: {} timeseries missing\n'.format(r))
                    raise

        s, RET = self.daily['RET']

        # find the start and end indices

        if start is None: 
            i = 0
            start = s
        else:             
            i = (start - s).days

        if end is None: 
            j = len(RET) + 1
            end = start + datetime.timedelta(days = 1) * len(RET)
        else:   
            j = (end - s).days

        RET = RET[i:j]

        # load the other time series

        s, tmin  = self.daily['tmin']
        s, tmax  = self.daily['tmax']
        s, dewt  = self.daily['dewpoint']
        s, wind  = self.daily['wind']
        s, solar = self.daily['solar']

        tmin  = tmin[i:j]
        tmax  = tmax[i:j]
        dewt  = dewt[i:j]
        wind  = wind[i:j]
        solar = solar[i:j]

        # make a daily time series

        times = [start + datetime.timedelta(days = 1) * i 
                 for i in range((end-start).days)]

        # make the plot

        fig = pyplot.figure(figsize = (8,8))
    
        subs =  [pyplot.subplot2grid((5,1), (0,0), rowspan = 2)]
        subs += [pyplot.subplot2grid((5,1), (i,0), sharex = subs[0]) 
                 for i in (2, 3, 4)]

        for sub in subs[:-1]: 
            for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

        if title is None: t = 'Penman-Monteith Evapotranspiration'
        else:             t = title

        fig.suptitle(t)

        # pan evaporation

        i = 0

        if stations is None: stations = []

        colors = 'green', 'orange', 'blue', 'brown'

        for s, c in zip(stations, colors):

            with open(s, 'rb') as f: station = pickle.load(f)

            evaporation = station.make_timeseries('evaporation', start, end)

            subs[i].plot_date(times, evaporation, fmt = 's', 
                              markeredgecolor = c, markerfacecolor = 'None', 
                              markersize = 3, label = station.name)

        subs[i].plot_date(times, RET, fmt = '-', color = 'green', lw = 1.,
                          label = 'reference evapotranspiration')
        subs[i].set_ylabel('Evaporation\n(mm)', size = axsize)
        subs[i].yaxis.set_major_locator(ticker.MaxNLocator(10))
        subs[i].legend(fontsize = 8)

        # min, max, and dewpoint temperatures

        i = 1

        subs[i].plot_date(times, tmax, fmt = '-', color = 'red', lw = 0.5, 
                          label = 'max temperature')
        subs[i].plot_date(times, tmin, fmt = '-', color = 'blue', lw = 0.5, 
                          label = 'min temperature')
        subs[i].plot_date(times, dewt, fmt = '-', color = 'green', lw = 0.5,
                          label = 'dewpoint')
        subs[i].set_ylabel('Temperature\n(\u00B0C)', size = axsize)
        subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))
        subs[i].legend(fontsize = 8, loc = 'lower left')

        # average daily wind speed

        i = 2

        subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                          label = 'wind')
        subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', size = axsize,
                           multialignment = 'center')
        subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))

        # daily solar radiation

        i = 3

        subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                          label = 'solar')
        subs[i].set_ylabel('Solar Radiation\n(W/m\u00B2)', 
                           color = 'orange',
                           multialignment = 'center', size = axsize)
        subs[i].yaxis.set_major_locator(ticker.MaxNLocator(5))

        subs[-1].xaxis.set_major_formatter(dates.DateFormatter('%b %y'))

        pyplot.tight_layout()
        pyplot.subplots_adjust(top = 0.95)
        if output is not None: pyplot.savefig(output)

        if show: pyplot.show()

    def plotdayofyear(self, 
                      start = None, 
                      end = None, 
                      stations = None, 
                      title = None,
                      labels = None, 
                      fill = False, 
                      axsize = 11,
                      output = None, 
                      show = False,
                      verbose = True,
                      ):
        """
        Plots the day of the water year estimates of RET from the Penman Model.
        """

        if verbose: 

            print('plotting day of the year aggregated climate data...\n')

        # error handling

        required = 'tmin', 'tmax', 'dewpoint', 'wind', 'solar', 'RET'
        if any([r in self.daily for r in required]):

            for r in required:

                if not r in self.daily:

                    print('error: {} timeseries missing\n'.format(r))
                    raise

        s, RET = self.daily['RET']

        # find the start and end indices

        if start is None: 
            i = 0
            start = s
        else:             
            i = (start - s).days

        if end is None: 
            j = len(RET) + 1
            end = start + datetime.timedelta(days = 1) * len(RET)
        else:   
            j = (end - s).days

        RET = RET[i:j]

        # load the other time series

        s, tmin  = self.daily['tmin']
        s, tmax  = self.daily['tmax']
        s, dewt  = self.daily['dewpoint']
        s, wind  = self.daily['wind']
        s, solar = self.daily['solar']

        tmin  = tmin[i:j]
        tmax  = tmax[i:j]
        dewt  = dewt[i:j]
        wind  = wind[i:j]
        solar = solar[i:j]

        # make a daily time series

        times = [start + datetime.timedelta(days = 1) * i 
                 for i in range((end-start).days)]

        # figure out the average conditions for each day of the water year

        solar  = self.dayofyear(times, solar)
        tmin   = self.dayofyear(times, tmin)
        tmax   = self.dayofyear(times, tmax)
        dewt   = self.dayofyear(times, dewt)
        wind   = self.dayofyear(times, wind)
        RET    = self.dayofyear(times, RET)

        # day of year times (2004 is a leap year)

        wateryear = [datetime.datetime(2003,10,1) + 
                     datetime.timedelta(days = 1) * i 
                     for i in range(366)]

        # make the plot

        fig = pyplot.figure(figsize = (8,8))
    
        subs =  [pyplot.subplot2grid((6,1), (0,0), rowspan = 3)]
        subs += [pyplot.subplot2grid((6,1), (i,0), sharex = subs[0]) 
                 for i in (3, 4, 5)]

        if title is None: t = 'Water Year Climate Data'
        else:             t = title

        fig.suptitle(t)

        # evaporation and potential evapotranspiration

        i = 0

        colors = 'green', 'orange', 'blue', 'brown'

        # total annual evaporation

        tots = []

        if stations is None: stations = []

        for s, c in zip(stations, colors):

            with open(s, 'rb') as f: station = pickle.load(f)

            evaporation = station.make_timeseries('evaporation', start, end)

            # convert to day of the water year

            if len([e for e in evaporation if e is not None]) > 0:

                evaporation = self.dayofyear(times, evaporation)

                subs[i].plot_date(wateryear, evaporation, fmt = 's', color = c, 
                                  markeredgecolor = c, markerfacecolor = 'None',
                                  markersize = 3, 
                                  label = station.name + ' evaporation')
                tot = sum([e for e in evaporation if e is not None])
                tots.append('{}: {:4.0f} mm/yr'.format(station.name, tot))

        tots.append('Penman-Monteith Model: {:4.0f} mm/yr'.format(sum(RET)))

        subs[i].plot_date(wateryear, RET, color = 'green', fmt = '-',
                          label = 'Penman-Monteith')
        subs[i].set_ylabel('Evaporation\n(mm)', size = axsize)
        subs[i].legend(fontsize = 8, loc = 'upper left')
        t = '\n'.join(tots)
        subs[i].text(0.98, 0.99, t, ha = 'right', va = 'top', 
                     transform = subs[i].transAxes, size = 8)

        # min, max, and dewpoint temperatures

        i = 1

        subs[i].plot_date(wateryear, tmax, fmt = '-', color = 'red', lw = 0.5, 
                          label = 'max temperature')
        subs[i].plot_date(wateryear, tmin, fmt = '-', color = 'blue', lw = 0.5, 
                          label = 'min temperature')
        subs[i].plot_date(wateryear, dewt, fmt = '-', color = 'green', 
                          lw = 0.5, label = 'dewpoint')
        subs[i].set_ylabel('Temperature\n(\u00B0C)', size = axsize)

        subs[i].legend(fontsize = 8, loc = 'lower right')

        # average daily wind speed

        i = 2

        subs[i].plot_date(wateryear, wind, fmt = '-', color = 'purple', 
                          lw = 0.5, label = 'wind')
        subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', 
                           multialignment = 'center', size = axsize)
        mi, ma = subs[i].get_ylim()
        subs[i].set_ylim((0,ma))

        # daily solar radiation

        i = 3

        subs[i].plot_date(wateryear, solar, fmt = '-', color = 'orange', 
                          lw = 0.5, label = 'solar')
        subs[i].set_ylabel('Solar Radiation\n(W/m\u00B2)', 
                           color = 'orange',
                           multialignment = 'center', size = axsize)
        mi, ma = subs[i].get_ylim()
        subs[i].set_ylim((0,ma))

        # ticks

        for s in subs[1:]: s.yaxis.set_major_locator(ticker.MaxNLocator(5))
        for sub in subs[:-1]: 
            for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

        subs[-1].set_xlabel('Water Year')
        subs[-1].xaxis.set_major_locator(dates.MonthLocator())
        labels = [t.get_text() for t in subs[-1].get_xticklabels()]
        labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
                  'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
        subs[-1].set_xticklabels(labels, size = axsize)

        pyplot.tight_layout()
        pyplot.subplots_adjust(top = 0.95)

        if output is not None: pyplot.savefig(output)

        if show: pyplot.show()

def plot_hourlyET(HUC8, start, end, evaporations, hETs, temp, dewpoint, wind, 
                  solar, title = None, totals = True, labels = None, 
                  colors = None, axsize = 11, fill = False, 
                  output = None, show = False, verbose = True):

    if verbose: print('plotting hourly evapotranspiration\n')

    # figure out the average conditions for each day of the water year

    dates = [start + datetime.timedelta(hours = 1) * i 
             for i in range((end-start).days * 24)]

    solar = dayofyear(dates, solar)
    temp  = dayofyear(dates, temp)
    hETs  = [[24 * p for p in dayofyear(dates, hET)] for hET in hETs]

    # these are daily values

    dates = [start + datetime.timedelta(days = 1) * i 
             for i in range((end - start).days)]

    dewpoint = dayofyear(dates, dewpoint)
    wind     = dayofyear(dates, wind)

    # day of year times (2004 is a leap year)

    times = [datetime.datetime(2003, 10, 1) + datetime.timedelta(days = 1) * i 
             for i in range(366)]

    # make the plot

    fig = pyplot.figure(figsize = (8,8))
    
    subs =  [pyplot.subplot2grid((6,1), (0,0), rowspan = 3)]
    subs += [pyplot.subplot2grid((6,1), (i,0), sharex = subs[0]) 
             for i in (3, 4, 5)]

    if title is None:
        v = HUC8, 'Evapotranspiration'
        subs[0].set_title('{} {}'.format(*v), size = 14)
    else: subs[0].set_title(title, size = 14)

    # evaporation and potential evapotranspiration

    i = 0

    # total annual evaporation

    if labels is None: labels = ['Penman Equation' for h in hETs]

    if totals:

        tots = ['\n{}: {:4.0f} mm'.format(l, sum(e)) 
                for l, e in zip(labels, hETs)]

        # snip the first break

        tots[0] = tots[0][1:]

    else: tots = ['{} Annual Total: {:4.0f} mm'.format(labels[0], sum(hETs[0]))]

    # colors

    #color_codes = {'Empty':       (255, 255, 255),  # empty
    #               'Corn':        (255, 211,   0),  # corn 
    #               'Soybeans':    ( 38, 112,   0),  # soybeans
    #               'Other grain': (255, 221, 165),  # other grain
    #               'Developed':   (155, 155, 155),  # developed
    #               'Wetland':     ( 76, 112, 163),  # water/wetland
    #               'Forest':      (147, 204, 147),  # forest
    #               'Alfalfa':     (255, 165, 226),  # hay/alfalfa
    #               'Fallow':      (191, 191, 119),  # fallow
    #               'Pasture':     (232, 255, 191),  # pasture/grass
    #               'Other':       (  0,   0,   0)   # other
    #               }
    color_codes = {'cereals':     (255, 211,   0),  # corn 
                   'legumes':     ( 38, 112,   0),  # soybeans
                   'alfalfa':     (255, 165, 226),  # hay/alfalfa
                   'wetland':     ( 76, 112, 163),  # water/wetland
                   'fallow':      (191, 191, 119),  # fallow
                   'pasture':     (232, 255, 191),  # pasture/grass
                   'others':      (  0,   0,   0)   # other
                   }

    if colors is None:
        colors = [color_codes[crop] 
                  if crop in color_codes else color_codes['others']
                  for crop in labels]
        colors = [[r / 255, g / 255, b / 255] for r, g, b in colors]

    for hET, c, l in zip(hETs, colors, labels):
        subs[i].plot_date(times, hET, color = c, lw = 1., fmt = '-', label = l)
        if fill:
            subs[i].fill_between(times, 0, hET, color = c, alpha = 0.3)

    fmts = ['s', '+', '*', 'o']

    for item, fmt, color in zip(evaporations.items(), fmts, colors):
        k, v = item
        evaporation = dayofyear(dates, v.make_timeseries(start, end))
        if len([e for e in evaporation if e is not None]) > 0:
            subs[i].plot_date(times, evaporation, fmt = fmt,
                              markerfacecolor = 'None', markersize = 4,
                              markeredgecolor = color,
                              label = k + ' evaporation')

            if totals:
                tot = sum([e for e in evaporation if e is not None])
                tots.append('\n{}: {:4.0f} mm'.format(k, tot))

            else:

                # calculate the linear regression between April 1 and October 31
                # for the reference ET versus the pan evaporation to see the 
                # goodness of fit and estimate the pan coefficient

                xs = [x for t, x in zip(times, evaporation) 
                      if 3 < t.month and t.month < 11] 
                ys = [y for t, y in zip(times, hETs[0]) 
                      if 3 < t.month and t.month < 11] 

                # get the linear regression

                m, b, r, p, std_err = stats.linregress(xs, ys)
            
                # add it to the text box

                its = k, m, b, r**2
                t = '\n{0}: y={1:.2f}x+{2:1.2f}; r\u00B2={3:.2f}'.format(*its)
                tots.append(t)

    subs[i].set_ylabel('Evaporation\n(mm)', color = 'green', size = axsize, 
                       multialignment = 'center')
    subs[i].legend(fontsize = 8, loc = 'upper left')

    t = ''.join(tots)
    subs[i].text(0.98, 0.99, t, ha = 'right', va = 'top', 
                 transform = subs[i].transAxes, size = 8)

    subs[i].tick_params(axis = 'y', colors = 'green', size = 9)

    # temperature and dewpoint

    i = 1

    subs[i].plot_date(times, temp, fmt = '-', color = 'red', lw = 0.5, 
                      label = 'temperature')
    subs[i].plot_date(times, dewpoint, fmt = '-', color = 'green', lw = 0.5, 
                      label = 'dewpoint')
    subs[i].set_ylabel('Temperature\n(\u00B0C)', size = axsize, color = 'red',
                       multialignment = 'center')
    subs[i].tick_params(axis = 'y', colors = 'red', size = 9)

    subs[i].legend(fontsize = 8, loc = 'lower right')

    # average daily wind speed

    i = 2

    subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                      label = 'wind')
    subs[i].tick_params(axis = 'y', colors = 'purple', size = 9)
    subs[i].set_ylabel('Wind Speed\n(m/s)', color = 'purple', size = axsize, 
                       multialignment = 'center')
    subs[i].set_ylim([0, subs[i].get_ylim()[1]])

    # daily solar radiation

    i = 3

    subs[i].plot_date(times, solar, fmt = '-', color = 'orange', lw = 0.5,
                      label = 'solar')
    subs[i].tick_params(axis = 'y', colors = 'orange', size = 9)
    subs[i].set_ylabel('Solar Radiation\n(kW hr/m\u00B2/day)', color = 'orange',
                       multialignment = 'center', size = axsize)
    subs[i].set_ylim([0, subs[i].get_ylim()[1]])

    # ticks

    for sub in subs[:-1]: 
        for t in sub.xaxis.get_ticklabels(): t.set_visible(False)

    subs[-1].set_xlabel('Water Year')
    subs[-1].xaxis.set_major_locator(MonthLocator())
    for s in subs[1:]: 
        s.yaxis.set_major_locator(MaxNLocator(5))
        for t in s.yaxis.get_ticklabels(): t.set_fontsize(9)
    labels = [t.get_text() for t in subs[-1].get_xticklabels()]
    labels = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
              'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']
    subs[-1].set_xticklabels(labels, size = 9)

    pyplot.tight_layout()
    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()
