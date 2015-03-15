# etcalculator.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 03/07/2015
#
# contains the ETCalculator class that can be used to compute evapotranspiration
# time series from other time series with models including the daily and hourly
# Penman-Monteith Equation and the FAO crop coefficient model.

import os, pickle, datetime, numpy

from .crop_coefficient import calculate_cropPET
from .climateplots     import plot_dailyET
from .climateplots     import plot_dayofyearET
from .climateplots     import plot_hourlyET

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

    def add_location(self, longitude, latitude, elevation):
        """Adds the latitude (needed for some calculations)."""

        self.longitude = longitude
        self.latitude  = latitude
        self.elevation = elevation

    def add_timeseries(self, tstype, tstep, start, data):
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
            print('valid choices are ' + ','.join(d) + '\n')
            raise
            
        d[tstype] = start, data

    def is_integer(self, s):
        """Tests if string "s" is an integer."""
        try: 
            int(s) 
            return True
        except ValueError: 
            return False

    def get_distance(self, p1, p2):
        """
        Approximates the distance in kilometers between two points on the 
        Earth's surface designated in decimal degrees using an ellipsoidal 
        projection. per CFR 73.208 it is applicable for up to 475 kilometers.
        p1 and p2 are listed as (longitude, latitude).
        """

        deg_rad = numpy.pi / 180

        dphi = p1[1] - p2[1]
        phim = 0.5 * (p1[1] + p2[1])
        dlam = p1[0] - p2[0]

        k1 = (111.13209 - 0.56605 * numpy.cos(2 * phim * deg_rad) + 0.00120 * 
              numpy.cos(4 * phim * deg_rad))
        k2 = (111.41513 * numpy.cos(phim * deg_rad) - 0.09455 * 
              numpy.cos(3 *phim * deg_rad) + 
              0.0012 * numpy.cos(5 * phim * deg_rad))

        return numpy.sqrt(k1**2 * dphi**2 + k2**2 * dlam**2)

    def make_timeseries(self, output, start, end, verbose = True):
        """Makes the following timeseries:

        1. hourly temperature
        2. daily dewpoint
        3. daily windspeed
        4. hourly solar radiation
        5. daily potential evapotranspiration
        6. hourly potential evapotranspiration

        The potential evapotranspiration timeseries are calculated using
        extracted temperature, dewpoint, wind speed, and solar radiation data
        in the ASCE Penman-Montieth equation.
        """

        if verbose: print('making input timeseries for the watershed\n')

        # source data

        #evapstations  = '{}/{}/evaporations/evaporation'.format(*v)
        #tempstations  = '{}/{}/temperatures/temperature'.format(*v)
        #dewstations   = '{}/{}/dewpoint/dewpoint'.format(*v)
        #windstations  = '{}/{}/wind/wind'.format(*v)
        #solarstations = '{}/{}/solar/solarstations'.format(*v)
        #snowstations  = '{}/{}/snow/snow'.format(*v)
        #gagedirectory = '{}/{}/NWIS'.format(*v)

        # output directory

        if not os.path.isdir(output):
            print('error: directory {} does not exist'.format(output))

        # temperature and dewpoint

        #if (not os.path.isfile('{}/tmin'.format(p)) or 
        #    not os.path.isfile('{}/tmax'.format(p)) or
        #    not os.path.isfile('{}/hourlytemperature'.format(p)) or
        #    not os.path.isfile('{}/dewpoint'.format(p))):

        #with open(tempstations, 'rb') as f: tempstations = pickle.load(f)
        #with open(dewstations, 'rb') as f:  dewstations  = pickle.load(f)

        #aggregate_temperature(tempstations, dewstations, start, end, p)

        # wind speed

        #if not os.path.isfile('{}/wind'.format(p)):

        #    with open(windstations, 'rb') as f: windstations = pickle.load(f)

        #    aggregate_wind(windstations, start, end, p)

        # solar radiation

        #if (not os.path.isfile('{}/solar'.format(p)) or
        #    not os.path.isfile('{}/dailysolar'.format(p))):
        #    with open(solarstations, 'rb') as f: solarstations = pickle.load(f)
        #    aggregate_solar(solarstations, start, end, p)

        # make plot of the time series and day of year averages

        dailyRET = '{}/dailyRET'.format(p)
        if not os.path.isfile(dailyRET):

            with open('{}/tmin'.format(p), 'rb') as f: 
                s, t, tmin = pickle.load(f)
            with open('{}/tmax'.format(p), 'rb') as f: 
                s, t, tmax = pickle.load(f)
            with open('{}/dewpoint'.format(p), 'rb') as f: 
                s, t, dewpoint = pickle.load(f)
            with open('{}/wind'.format(p), 'rb') as f: 
                s, t, wind = pickle.load(f)
            with open('{}/dailysolar'.format(p), 'rb') as f: 
                s, t, dsolar = pickle.load(f)

            # Watts/m2 to MJ/day/m2

            solar    = numpy.array(dsolar) * 86400 / 10**6
            tmin     = numpy.array(tmin)
            tmax     = numpy.array(tmax)
            dewpoint = numpy.array(dewpoint)
            wind     = numpy.array(wind)

            dates = [start + datetime.timedelta(days = 1) * i 
                     for i in range((end-start).days)]

            # Calculate daily timeseries using the ASCE short ref crop equation

            RET = penman_daily(lat, elev, dates, tmin, tmax, dewpoint, solar, 
                               wind, verbose = False)

            # This is for the tall reference crop

            #RET = penman_daily(lat, elev, dates, tmin, tmax, dewpoint, solar, wind,
            #                   Cn = 1600, Cd = 0.38, verbose = False)

            # dump the timeseries into a folder for later
        
            with open(dailyRET, 'wb') as f: pickle.dump((s, t, RET), f)

        # make plot of the time series and day of year averages

        dayofyearRET = '{}/dayofyearRET'.format(p)
        if (not os.path.isfile(dailyRET + '.png') or 
            not os.path.isfile(dayofyearRET + '.png')):
            with open('{}/tmin'.format(p), 'rb') as f: 
                s, t, tmin = pickle.load(f)
            with open('{}/tmax'.format(p), 'rb') as f: 
                s, t, tmax = pickle.load(f)
            with open('{}/dewpoint'.format(p), 'rb') as f: 
                s, t, dewpoint = pickle.load(f)
            with open('{}/wind'.format(p), 'rb') as f: 
                s, t, wind = pickle.load(f)
            with open('{}/dailysolar'.format(p), 'rb') as f: 
                s, t, dsolar = pickle.load(f)
            with open(evapstations, 'rb') as f: 
                evaporations = pickle.load(f)
            with open(dailyRET, 'rb') as f: 
                s, t, RET = pickle.load(f)

            # Watts/m2 to kW hr/m2

            dsolar = [s * 0.024 for s in dsolar]

            plot_dailyET(HUC8, start, end, RET, evaporations, tmin, tmax, 
                         dewpoint, wind, dsolar, output = dailyRET)

            plot_dayofyearET(HUC8, start, end, evaporations, [RET], tmin, tmax, 
                             dewpoint, wind, dsolar, fill = True,
                             output = dayofyearRET)

        hourlyRET = '{}/hourlyRET'.format(p)
        if not os.path.isfile(hourlyRET):
            with open('{}/hourlytemperature'.format(p), 'rb') as f: 
                s, t, temp = pickle.load(f)
            with open('{}/dewpoint'.format(p), 'rb') as f: 
                s, t, dewpoint = pickle.load(f)
            with open('{}/wind'.format(p), 'rb') as f: 
                s, t, wind = pickle.load(f)
            with open('{}/hourlysolar'.format(p), 'rb') as f: 
                s, t, solar = pickle.load(f)

            # Watts/m2 to MJ/hour/m2

            solar    = numpy.array(solar) * 3600 / 10**6
            temp     = numpy.array(temp)

            # assume dewpoint and wind are same throughout the day

            dewpoint = numpy.array([t for t in dewpoint for i in range(24)])
            wind     = numpy.array([w for w in wind for i in range(24)])

            dates = [start + datetime.timedelta(hours = 1) * i 
                     for i in range((end-start).days * 24)]

            # Calculate hourly timeseries using the ASCE short ref crop equation

            RET = penman_hourly(lat, lon, elev, dates, temp, dewpoint, solar, 
                                wind, verbose = False)

            with open(hourlyRET, 'wb') as f: pickle.dump((start, 60, RET), f)

        if not os.path.isfile(hourlyRET + '.png'):
            with open('{}/hourlytemperature'.format(p), 'rb') as f: 
                s, t, temp = pickle.load(f)
            with open('{}/dewpoint'.format(p), 'rb') as f: 
                s, t, dewpoint = pickle.load(f)
            with open('{}/wind'.format(p), 'rb') as f: 
                s, t, wind = pickle.load(f)
            with open('{}/hourlysolar'.format(p), 'rb') as f: 
                s, t, solar = pickle.load(f)
            with open(hourlyRET, 'rb') as f: 
                s, t, hRET = pickle.load(f)

            # Watts/m2 to kW hr/m2
                
            solar = [s * 0.024 for s in solar]

            with open(evapstations, 'rb') as f: evaporations = pickle.load(f)

            plot_hourlyET(HUC8, start, end, evaporations, [hRET], temp,
                          dewpoint, wind, solar, fill = True, 
                          colors = ['green', 'yellow', 'orange', 'red'],
                          output = hourlyRET)

        hourlyPETs = '{}/hourlyPETs'.format(p)
        if not os.path.isfile(hourlyPETs):
            calculate_cropPET(directory, HUC8, start, end)

        # make the subbasin precipitation files from the watershed info and 
        # hourly precipitation station files

        v = directory, HUC8
        watershed  = '{}/{}/watershed'.format(*v)
        precipfile = '{}/{}/precipitations/precipitation'.format(*v)

        with open(watershed,  'rb') as f: 
            subbasins     = pickle.load(f).subbasins
        with open(precipfile, 'rb') as f: 
            precipitation = pickle.load(f)

        if not os.path.isdir('{}/{}/subbasinprecipitation'.format(*v)):
            make_precipitation(subbasins, precipitation, start, end, 
                               '{}/{}'.format(*v))

        # aggregate the observed snowfall and snowdepth data

        snowfall  = '{}/{}/snow/snowfall'.format(*v)
        snowdepth = '{}/{}/snow/snowdepth'.format(*v)
        if not os.path.isfile(snowfall) or not os.path.isfile(snowdepth):
            make_snow(directory, HUC8, start, end)

        # aggregate the daily flow gage data into a single timeseries

        if not os.path.isfile('{}/dailydischarges'.format(gagedirectory)):
            make_gagestations(directory, HUC8)

    def hourly_temperature(self, tmin, tmax):
        """
        Develops an hourly time series of temperatures from the daily min 
        and max.  Assumes a sinusoidal profile from the min temp to the max 
        from 6 am to 4 pm.
        """
        
        #fill in the first 6 hours with tmin

        hourly = [tmin[0] for i in range(6)]

        # iterate through to the last day

        for mi1, ma, mi2 in zip(tmin[:-1], tmax, tmin[1:]):
            
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
                    hourly.append(mid + amp* numpy.cos(i * numpy.pi / 14))

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
        Estimates the wind speed at 2 m from the wind speed at height z 
        (the typical AMOS weather stations have a height of 10 meters).
        """

        return u1 * 4.87 / numpy.log(67.8 * z - 5.42)

    def get_Cd(self, 
               lat, 
               lon, 
               dates, 
               Cday, 
               Cnight,
               ):
        """
        Provides an array of the values of the denominator coefficient if a 
        list is provided, otherwise just provides the appropriate day or 
        night value.
        """

        if isinstance(dates, list):
            Cd = array([Cday if day else Cnight 
                        for day in is_daytime(lat, lon, dates)])

        else:

            if is_daytime(dates): 
                Cd = Cday
            else:                 
                Cd = Cnight

        return Cd

    def get_soil(self, 
                 lat, 
                 lon, 
                 dates, 
                 Gday, 
                 Gnight,
                 ):
        """
        Provides an array of the values of the soil heat flux if a list is 
        provided, otherwise just provides the appropriate day or night value.
        """

        if isinstance(dates, list):
            Gd = array([Gday if day else Gnight 
                        for day in is_daytime(lat, lon, dates)])

        else:

            if is_daytime(dates): 
                Gd = Gday
            else:                 
                Gd = Gnight

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

        sd = (23.45 * numpy.pi / 180 * 
              numpy.cos(2 * numpy.pi / 365 * (julian - 172)))

        # calculate the inverse relative distance Earth-Sun

        irl = 1 + 0.033 * numpy.cos(2 * numpy.pi / 365 * julian)

        # calculate the hour angle at sunset (rad)

        sha = numpy.arccos(-numpy.tan(self.latitude * numpy.pi / 180) * 
                           numpy.tan(sd))

        # calculate the extraterrestrial radiation

        et_rad = 37.59 * irl * (sha * 
                                numpy.sin(self.latitude * numpy.pi / 180)
                                * numpy.sin(sd) + 
                                numpy.cos(self.latitude * numpy.pi / 180) * 
                                numpy.cos(sd) * numpy.sin(sha))

        # calculate the clear day solar radiation

        clear = (0.00002 * self.elevation + 0.75) * et_rad

        return (solar * (1 - albedo) - sigma * 0.5 * (Tmin**4 + Tmax**4) * 
                (0.34 - 0.139 * numpy.sqrt(Pv)) * (1.35 * (solar / clear) - 
                                                   0.35))

    def hourly_radiation(self,
                         lat, 
                         lon, 
                         elev, 
                         times, 
                         solar, 
                         T, 
                         Pv, 
                         albedo = 0.23,
                         sigma = 2.042e-10,
                         ):
        """
        Estimates the net radiation (MJ/m2/hour) at the crop surface assuming 
        the crop is grass.
    
        solar  -- measured solar radiation
        T      -- average temperature (K)
        Pv     -- vapor pressure (kPa)
        albedo -- assumed value typical for grass
        sigma  -- boltzmann constant (MJ m-2 h-1)
        """

        # convert dates to julian days and figure out if the sun is up or not

        if isinstance(times, list):
            julian  = array([t.timetuple().tm_yday for t in times])
            ts      = array([t.timetuple().tm_hour for t in times])
            daytime = array([1 
                             if d else 0 
                             for d in is_daytime(lat, lon, times)])

        else:
            julian  = times.timetuple().tm_yday
            ts      = times.timetuple().tm_hour
            daytime = is_daytime(lat, lon, times)

        # calculate the solar declination (rad)

        sd  = 23.45 * pi / 180 * cos(2 * pi / 365 * (julian - 172))

        # calculate the inverse relative distance Earth-Sun

        irl = 1 + 0.033 * cos(2 * pi / 365 * julian)

        # calculate the solar time angle at the middle of the hour

        b = 2 * pi * (julian - 81) / 364
        Sc = 0.1645 * sin(2 * b) - 0.1255 * cos(b) - 0.025 * sin(b)
        stime = (ts + 0.5 + (15 * round(lon / 15) - lon) / 15 + Sc) - 12
        om = pi / 12 * ((ts + 0.5 + 
                         (15 * round(lon / 15) - lon) / 15 + Sc) - 12)

        # calculate sun angle above the horizon at the middle of the hour

        beta = 180 / pi * arcsin(sin(lat * pi / 180) * sin(sd) + 
                                 cos(lat * pi / 180) * cos(sd) * cos(om))

        # calculate the hour angle at sunset (rad)

        #sha = arccos(-tan(lat * pi / 180) * tan(sd))

        # calculate the extraterrestrial radiation for the hour

        et_rad = 18.79 * irl * daytime * (pi / 12 * sin(lat * pi / 180) * 
                                          sin(sd) +
                                          cos(lat * pi / 180) * cos(sd) * 
                                          (sin(om + pi / 24) - 
                                           sin(om - pi / 24)))

        # calculate the clear day solar radiation

        clear = (0.00002 * elev + 0.75) * et_rad

        # calculate ratio of the clear sky radiation to the actual radiation

        ratio = array([1.35 * (s / c) - 0.35 
                       if 0 < c and 0.3 < s / c and 17 < a else 0.05
                       for s, c, a in zip(solar, clear, beta)])

        Rs = solar * (1-albedo)
        Rl = sigma * 0.5 * T**4 * (0.34 - 0.139 * sqrt(Pv)) * ratio

        return (solar * (1 - albedo) - sigma * 0.5 * T**4 * 
                (0.34 - 0.139 * sqrt(Pv)) * ratio)

    def penman_daily(self,
                     #lat, 
                     #elev,
                     start,
                     end,
                     #dates, 
                     #tmin, 
                     #tmax, 
                     #dew, 
                     #solar, 
                     #wind, 
                     albedo = 0.23, 
                     soil = 0, 
                     Cn = 900, 
                     Cd = 0.34, 
                     wind_height = 10, 
                     verbose = False,
                     ):
        """
        Calculates the reference evapotransporation (RET) in mm for a given day
        using the Penman-Monteith equation. Equations from ASCE.

        start  -- datetime.datetime
        dates  -- datetime.datetime
        lat    -- latitude (decimal degrees)
        elev   -- elevation (m)
        tmin   -- minimum air temperature (C)
        tmax   -- maximum air temperature (C)
        dew    -- dew point temperature (C)
        rad    -- solar radiation (MJ m-2 day-1)
        wind   -- wind speed at 10 m (m s-1)
        albedo -- assumed value typical for grass
        """

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

        # convert the units of solar radiation from watts to MJ/m2/day

        solar = data['solar'] * 86400 / 10**6

        # check and replace dewpoints lower than tmin (physically impossible)

        dewpoint = numpy.minimum(data['tmin'], data['dewpoint'])

        # correct the wind speed to 2 meters from 10 meters

        u2 = self.wind_correction(data['wind'], z = wind_height)

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

        return RET

def penman_hourly(lat, lon, elev, dates, temp, dew, solar, wind, albedo = 0.23,
                  Cn = 37, Cday = 0.24, Cnight = 0.96, Gday = 0.1, Gnight = 0.5,
                  wind_height = 10, verbose = False):
    """
    Calculates the potential evapotransporation (PET) in mm for a given hour
    using the Penman-Monteith equation. Equations from ASCE.

    lat   - latitude (decimal degrees)
    elev  - elevation (m)
    dates - datetime.datetime instance(s)
    temp  - temperature (C)
    dew   - dew point temperature (C)
    rad   - solar radiation (MJ m-2 day-1)
    wind  - wind speed (m s-1)
    """

    # correct the wind speed to 2 meters from 10

    u2 = wind_speed_correction(wind, z = wind_height)

    # estimate the atmospheric pressure at the given elevation

    P = atmosphere_pressure(elev)

    # estimate the vapor pressure (kPa) from the dew point

    Pv = vapor_pressure(dew)

    # estimate the average saturation vapor pressure (kPa)

    Ps = vapor_pressure(temp)

    # estimate the slope of vapor pressure curve (kPa C-1) at mean temperature

    d = 4098 * vapor_pressure(temp) / (temp + 237.3)**2 

    # convert C to K

    T = temp + 273.15
 
    # get the net radiation

    rnet = hourly_radiation(lat, lon, elev, dates, solar, T, Pv)

    # get the soil heat flux

    soil = rnet * get_soil(lat, lon, dates, Gday, Gnight)

    # use the appropriate Cd values depending on whether it's day or night

    Cd = get_Cd(lat, lon, dates, Cday, Cnight)

    # estimate the psychrometric constant (kPa C-1)
    # equation is gamma = cp(T) * P * MWair / latent_heat / MWwater
    # this uses T = 20 C for cp(T)

    g = 0.000665 * P

    # estimate the potential evapotranspiration

    PET = ((0.408 * (rnet - soil) * d  + Cn * u2 / T * (Ps - Pv) * g) / 
           (d + (g * (1 + Cd * u2))))

    if verbose and isinstance(dates, datetime.datetime):
        print('\nEstimated values:\n')
        print('atmospheric pressure:  {:>7.1f} kPa'.format(P))
        print('vapor pressure:        {:>7.2f} kPa'.format(Pv))
        print('saturation pressure:   {:>7.2f} kPa'.format(Ps))
        print('net solar radiation:   {:>7.1f} MJ/m\u00B2/day'.format(rnet))
        print('vapor pressure slope:  {:>7.3f} kPa/K'.format(d))
        print('psychometric constant: {:>7.1e} kPa/K'.format(g))
        print('daily potential ET:    {:>7.1f} mm'.format(PET))

    elif verbose: print('\nPET estimates:', PET)

    return PET

    def aggregate_temperature(self, tempstations, dewstations, start, end, destination):
        """Aggregates the raw temperature data into a daily min, max, and 
        dewpoint temperature series, then makes an hourly temperature series."""

        tmins, tmaxs = [], []
        for k, v in tempstations.items():
            tmax, tmin = v.make_timeseries(start, end)
            if tmin is not None and tmax is not None:
                tmins.append(tmin)
                tmaxs.append(tmax)

        tmin, tmax = [], []
        for row in zip(*tmins):
            day = []
            for v in row:
                if v is not None: 
                    day.append(v)
                elif len(tmins) == 1: day.append(tmin[-1])        
            try:
                tmin.append(sum(day) / len(day))                
            except: print(day)

        for row in zip(*tmaxs):
            day = []
            for v in row:
                if v is not None: 
                    day.append(v)
                elif len(tmaxs) == 1: day.append(tmax[-1])
            tmax.append(sum(day) / len(day))

        with open('{}/tmax'.format(destination), 'wb') as f: 
            pickle.dump((start, 1440, tmax), f)
        with open('{}/tmin'.format(destination), 'wb') as f: 
            pickle.dump((start, 1440, tmin), f)

        # make the hourly timeseries

        hourly_temp = hourly_temperature(tmin, tmax)

        with open('{}/hourlytemperature'.format(destination), 'wb') as f: 
            pickle.dump((start, 60, hourly_temp), f)

        # process the raw dewpoint into a single daily timeseries

        dewpoints = [v.make_timeseries(start, end) for k, v in dewstations.items()]
        dewpoints = [d for d in dewpoints if d is not None]

        dewpoint = []
        for row in zip(*dewpoints):
            day = [v for v in row if v is not None]
            if len(day) > 0: 
                dewpoint.append(sum(day) / len(day))
            else:            dewpoint.append(None)

        # fill in missing data with tmin

        for t, d, i in zip(tmin, dewpoint, range(len(dewpoint))):

            if d is None or d > t: dewpoint[i] = t

        with open('{}/dewpoint'.format(destination), 'wb') as f: 
            pickle.dump((start, 1440, dewpoint), f)

#    def aggregate_wind(windstations, start, end, destination):
#    """Processes the raw wind speed data into a single daily timeseries."""
#
#    winds = [v.make_timeseries(start, end) for k, v in windstations.items()]
#    winds = [w for w in winds if w is not None]
#
#    wind = []
#    for row in zip(*winds):
#        day = [v for v in row if v is not None]
#        if len(day) > 0: wind.append(sum(day) / len(day))
#        else:            wind.append(wind[-1])
#
#    with open('{}/wind'.format(destination), 'wb') as f:
#        pickle.dump((start, 1440, wind), f)
#
#def aggregate_solar(solarstations, start, end, destination):
#    """Processes the raw solar radiation data into a single daily and
#    hourly timeseries."""
#
#    solars = [v.make_timeseries(start, end) for k, v in solarstations.items()]
#    solars = [s for s in solars if s is not None]
#
#    solar = []
#    for row in zip(*solars):
#        hour = [v for v in row if v is not None]
#        if len(hour) > 0: solar.append(sum(hour) / len(hour))
#        else:             solar.append(solar[-1])
#
#    with open('{}/hourlysolar'.format(destination), 'wb') as f: 
#        pickle.dump((start, 60, solar), f)
#
#    # aggregate the hourly to daily
#
#    dsolar = [sum(solar[i:i+24]) / 24 for i in range(0, len(solar), 24)]
#
#    with open('{}/dailysolar'.format(destination), 'wb') as f: 
#        pickle.dump((start, 1440, dsolar), f)
#
#def make_precipitation(subbasins, gages, start, end, destination,
#                       plot = False, verbose = True):
#    """Takes values of precipitation for the watershed from the PrecipStation
#    instances and performs an inverse-distance weighted average for each 
#    subbasin centroid and assigns them to a WDM file."""
#
#    # make timeseries for all the gauges
#
#    hourly = [v.make_timeseries(start, end) for k, v in gages.items()]
#
#    # keep track of the subbasin timeseries in a dictionary
#
#    d = '{}/subbasinprecipitation'.format(destination)
#    if not os.path.isdir(d): os.mkdir(d)
#
#    for subbasin in subbasins: 
#
#        output = '{}/{}'.format(d, subbasin)
#
#        if not os.path.isfile(output):
#            if verbose: 
#                print('making a precipitation time series for subbasin ' +
#                      '{}'.format(subbasin))
#
#            # find the gages and the distance from the centroid
#
#            centroid  = subbasins[subbasin].flowplane.centroid
#            distances = [get_distance(centroid, [v.longitude, v.latitude])
#                         for k,v in gages.items()]
#
#            # use the inverse weighted distance average
#
#            weights = [1 / d if d > 0 else 0 for d in distances]
#
#            # iterate through the time period and fill in the hourly values
#            # and convert to mm
#
#######            precip = [25.4 * p for p in weighted_avg(hourly, weights = weights)]
#
#            # dump the results and save for later
#
#            with open(output, 'wb') as f: pickle.dump((start, 60, precip), f)
#
#    precipitation = {}
#    for subbasin in subbasins:
#
#        output = '{}/{}'.format(d, subbasin)
#        with open(output, 'rb') as f: 
#            precipitation[subbasin] = pickle.load(f)
#
#    return precipitation
#
#def make_snow(directory, HUC8, start, end, verbose = True, vverbose = False):
#    """Averages the data from the snowstations into a single timeseries."""
#
#    if verbose: print('averaging snowfall and snowdepth station data\n')
#
#    snowdata = '{}/{}/snow/snow'.format(directory, HUC8)
#    with open(snowdata, 'rb') as f: snowstations = pickle.load(f)
#
#    times = [start + datetime.timedelta(days = 1) * i 
#             for i in range((end - start).days)]
#
#    # snowfall
#
#    data = [s.make_timeseries(start, end, tstype = 'fall', verbose = vverbose)
#            for s in snowstations.values()]
#
#    # remove any timeseries with insufficient data
#
#    data = [d for d in data if d is not None]
#
#    if len(data) == 0: 
#        print('error, no snowfall data exist')
#        raise
#
#    falls = []
#    for values in zip(*data):
#        day = [0 if v is None else v for v in values]
#        if len(day) > 0: falls.append(sum(day) / len(day))
#        else:            falls.append(0)
#
#    with open('{}/{}/snow/snowfall'.format(directory, HUC8), 'wb') as f:
#        pickle.dump((times, falls), f)
#
#    # snowdepths
#
#    data = [s.make_timeseries(start, end, verbose = vverbose)
#            for s in snowstations.values()]
#
#    # remove any timeseries with insufficient data
#
#    data = [d for d in data if d is not None]
#
#    if len(data) == 0: 
#        print('error, no snowfall data exist')
#        raise
#
#    depths = []
#    for values in zip(*data):
#        day = [0 if v is None else v for v in values]
#        if len(day) > 0: depths.append(sum(day) / len(day))
#        else:            depths.append(0)
#
#    with open('{}/{}/snow/snowdepth'.format(directory, HUC8), 'wb') as f:
#        pickle.dump((times, depths), f)
#
#def make_gagestations(directory, HUC8, verbose = True):
#    """Combines all the flow gage station data into a single file consisting
#    of a dictionary with NWIS gage ids as keys and observations as values."""
#
#    NWIS = '{}/{}/NWIS'.format(directory, HUC8)
#
#    flowdata = {}
#    waterquality = {}
#    for g in os.listdir(NWIS):
#        if is_integer(g):
#            with open('{}/{}'.format(NWIS, g), 'rb') as f: 
#                station = pickle.load(f)
#            flowdata[g] = station.gagedates, station.gageflows
#            waterquality[g] = station.waterquality
#
#    with open('{}/dailydischarges'.format(NWIS), 'wb') as f: 
#        pickle.dump(flowdata, f)
#    with open('{}/waterquality'.format(NWIS), 'wb') as f:
#        pickle.dump(waterquality, f)

#dates = [datetime.datetime(2001, 1, 1), datetime.datetime(2001, 7, 1)]
#tmin  = np.array([-5, 20])
#tmax  = np.array([10, 30])
#dew   = np.array([-6, 18])
#rad   = np.array([10, 20])
#wind  = np.array([2, 3])
