# penman.py
#
# estimates the daily timeseries of potential evapotranspiration from basic
# meteorological data

import datetime

from numpy import array, exp, sin, cos, tan, arccos, arcsin, sqrt, pi, log

from .sun   import is_daytime

def atmosphere_pressure(z):
    """Estimates the atmospheric pressure (kPa) from the elevation (m) using a 
    simplified ideal gas law."""

    return 101.3 * ((293.0 - 0.0065 * z) / 293.0)**5.26

def vapor_pressure(T):
    """Estimates the water vapor pressure (kPa) at a given temperature (C)."""

    return 0.6108 * exp((17.27 * T) / (T + 237.3))

def wind_speed_correction(u1, z = 10):
    """Estimates the wind speed at 2 m from the wind speed at height z 
    (which is typically 10 meters."""

    return u1 * 4.87 / log(67.8 * z - 5.42)

def get_Cd(lat, lon, dates, Cday, Cnight):
    """Provides an array of the values of the denominator coefficient if a list 
    is provided, otherwise just provides the appropriate day or night value."""

    if isinstance(dates, list):
        Cd = array([Cday if day else Cnight 
                    for day in is_daytime(lat, lon, dates)])

    else:

        if is_daytime(dates): Cd = Cday
        else:                 Cd = Cnight

    return Cd

def get_soil(lat, lon, dates, Gday, Gnight):
    """Provides an array of the values of the soil heat flux if a list 
    is provided, otherwise just provides the appropriate day or night value."""

    if isinstance(dates, list):
        Gd = array([Gday if day else Gnight 
                    for day in is_daytime(lat, lon, dates)])

    else:

        if is_daytime(dates): Gd = Gday
        else:                 Gd = Gnight

    return Gd

def daily_radiation(lat, elev, dates, solar, Tmin, Tmax, Pv, albedo = 0.23):
    """Estimates the net radiation (MJ/m2/day) at the crop surface assuming 
    the crop is grass.
    
    solar -- measured solar radiation
    T     -- average temperature (K)
    Pv    -- vapor pressure (kPa)
    cloud -- cloud cover fraction
    """

    sigma = 4.903e-9  # boltzmann constant

    # convert the dates to julian days

    if isinstance(dates, list):
        julian = array([d.timetuple().tm_yday for d in dates])
    else:
        julian = dates.timetuple().tm_yday

    # calculate the solar declination (rad)

    sd  = 23.45 * pi / 180 * cos(2 * pi / 365 * (julian - 172))

    # calculate the inverse relative distance Earth-Sun

    irl = 1 + 0.033 * cos(2 * pi / 365 * julian)

    # calculate the hour angle at sunset (rad)

    sha = arccos(-tan(lat * pi / 180) * tan(sd))

    # calculate the extraterrestrial radiation

    et_rad = 37.59 * irl * (sha * sin(lat * pi / 180) * sin(sd) +
                            cos(lat * pi / 180) * cos(sd) * sin(sha))

    # calculate the clear day solar radiation

    clear = (0.00002 * elev + 0.75) * et_rad

    return (solar * (1 - albedo) - sigma * 0.5 * (Tmin**4 + Tmax**4) * 
            (0.34 - 0.139 * sqrt(Pv)) * (1.35 * (solar / clear) - 0.35))

def hourly_radiation(lat, lon, elev, times, solar, T, Pv, albedo = 0.23):
    """Estimates the net radiation (MJ/m2/hour) at the crop surface assuming 
    the crop is grass.
    
    solar -- measured solar radiation
    T     -- average temperature (K)
    Pv    -- vapor pressure (kPa)
    """

    # boltzmann constant (MJ m-2 h-1)

    sigma = 2.042e-10

    # convert the dates to julian days and figure out if the sun is up or not

    if isinstance(times, list):
        julian  = array([t.timetuple().tm_yday for t in times])
        ts      = array([t.timetuple().tm_hour for t in times])
        daytime = array([1 if d else 0 for d in is_daytime(lat, lon, times)])

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
    om = pi / 12 * ((ts + 0.5 + (15 * round(lon / 15) - lon) / 15 + Sc) - 12)

    # calculate the angle of the sun above the horizon at the middle of the hour

    beta = 180 / pi * arcsin(sin(lat * pi / 180) * sin(sd) + 
                             cos(lat * pi / 180) * cos(sd) * cos(om))

    # calculate the hour angle at sunset (rad)

    #sha = arccos(-tan(lat * pi / 180) * tan(sd))

    # calculate the extraterrestrial radiation for the hour

    et_rad = 18.79 * irl * daytime * (pi / 12 * sin(lat * pi / 180) * sin(sd) +
                                      cos(lat * pi / 180) * cos(sd) * 
                                      (sin(om + pi / 24) - sin(om - pi / 24)))

    # calculate the clear day solar radiation

    clear = (0.00002 * elev + 0.75) * et_rad

    # calculate the ratio of the clear sky radiation to the actual radiation

    ratio = array([1.35 * (s / c) - 0.35 
                   if 0 < c and 0.3 < s / c and 17 < a else 0.05
                   for s, c, a in zip(solar, clear, beta)])

    Rs = solar * (1-albedo)
    Rl = sigma * 0.5 * T**4 * (0.34 - 0.139 * sqrt(Pv)) * ratio

    return (solar * (1 - albedo) - sigma * 0.5 * T**4 * 
            (0.34 - 0.139 * sqrt(Pv)) * ratio)

def penman_daily(lat, elev, dates, tmin, tmax, dew, solar, wind, albedo = 0.23, 
                 soil = 0, Cn = 900, Cd = 0.34, wind_height = 10, 
                 verbose = False):
    """
    Calculates the potential evapotransporation (PET) in mm for a given day
    using the Penman-Monteith equation. Equations from ASCE.

    lat   - latitude (decimal degrees)
    elev  - elevation (m)
    dates - datetime.datetime instance(s)
    tmin  - minimum air temperature (C)
    tmax  - maximum air temperature (C)
    dew   - dew point temperature (C)
    rad   - solar radiation (MJ m-2 day-1)
    wind  - wind speed at 10 m (m s-1)
    """

    if isinstance(dates, datetime.datetime):
        if dew > tmin:
            if verbose:
                print('warning: replacing dew point with minimum temperature')
                dew = tmin
    else:
        if (dew > tmin).any():
            if verbose:
                print('warning: replacing dew point with minimum temperature')
                dew = tmin

    # correct the wind speed to 2 meters from 10

    u2 = wind_speed_correction(wind, z = wind_height)

    # estimate the atmospheric pressure at the given elevation

    P = atmosphere_pressure(elev)

    # estimate the average temperature (C) for the day

    tavg = (tmin + tmax) / 2

    # estimate the vapor pressure (kPa) from the dew point

    Pv = vapor_pressure(dew)

    # estimate the average saturation vapor pressure (kPa)

    Ps = (vapor_pressure(tmin) + vapor_pressure(tmax)) / 2

    # estimate the slope of vapor pressure curve (kPa C-1) at mean temperature

    d = 4098 * vapor_pressure(tavg) / (tavg + 237.3)**2 

    # convert C to K

    T    = tavg + 273.15
    Tmin = tmin + 273.15
    Tmax = tmax + 273.15
 
    # get the net radiation

    rnet = daily_radiation(lat, elev, dates, solar, Tmin, Tmax, Pv)

    # estimate the psychrometric constant (kPa C-1)
    # equation is gamma = cp(T) * P * MWair / latent_heat / MWwater
    # this uses T = 20 C for cp(T)

    g = 0.000665 * P

    # estimate the potential evapotranspiration

    PET = ((0.408 * (rnet - soil) * d  + Cn * u2 / T * (Ps - Pv) * g) / 
           (d + g * (1 + Cd * u2)))

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

if __name__ == '__main__':

    lat   = 30
    elev  = 1000

    # scalars

    dates = datetime.datetime(2001, 1, 1)
    tmax  = 10
    tmin  = -5
    dew   = -6
    rad   = 10
    wind  = 2

    PET = penman_daily(lat, elev, dates, tmin, tmax, dew, rad, wind, 
                       verbose = True)

    # numpy arrays

    dates = [datetime.datetime(2001, 1, 1), datetime.datetime(2001, 7, 1)]
    tmax  = array([10, 30])
    tmin  = array([-5, 20])
    dew   = array([-6, 18])
    rad   = array([10, 20])
    wind  = array([2, 3])
    
    PET = penman_daily(lat, elev, dates, tmin, tmax, dew, rad, wind, 
                       verbose = True)
