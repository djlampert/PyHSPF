# sun.py
#
# David J. Lampert (djlampert@gmail.com)
# 
# contains utility functions related to sunrise and sunset

import os, csv, pickle, math, datetime

from math import acos, asin, atan, tan, sin, cos, pi, floor, sqrt

def time_round(d):
    """Rounds a datetime.datetime instance to the nearest hour."""

    yr, mo, da, hr, mi = d.timetuple()[:5]

    if 30 <= mi:  # round to the next hour

        r = datetime.datetime(yr, mo, da, hr) + datetime.timedelta(hours = 1)

    else:

        r = datetime.datetime(yr, mo, da, hr)

    return r

def noon(latitude, day):
    """Returns the approximate hour of solar noon."""

def jdays(utc_time):
    dt = utc_time - datetime.datetime(2000, 1, 1, 12, 0)
    return (dt.days + (dt.seconds + dt.microseconds / (1000000.0)) / 
            (24 * 3600.0) + 2451545)

def rad(d): 
    """Degrees to radians."""

    return (pi / 180 * d)

def sun(lat, lon, date, zenith = 90.83, nearest_hour = False):
    """Estimates the sunrise and sunset time for a given latitude, longitude,
    and date and optionally round it to the nearest hour."""

    if 3 < date.month and date.month < 11: offset = -5
    else:                                  offset = -6

    # calculate day of the year

    N = date.timetuple().tm_yday

    # convert longitude to hour value and calculate approximate time

    hour = lon / 15

    # rise

    t = N + (6 - hour) / 24

    # calculate mean anomaly

    M = (360 / 365.25 * t) - 3.289

    # calculate sun's true longitude

    L = M + 1.916 * sin(rad(M)) + (0.02 * sin(2 * rad(M))) + 282.634

    # adjust into 0, 360 range

    if L < 0:   L+=360
    if L > 360: L-=360
             
    # calculate right ascension

    RA = 180 / pi * atan(0.91764 * tan(rad(L)))

    # ensure RA is in the same quadrant as L

    Lquadrant  = (floor(L  / 90)) * 90
    RAquadrant = (floor(RA / 90)) * 90

    RA = RA + (Lquadrant - RAquadrant)

    # convert to hours

    RA = RA / 15

    # calculate the solar declination

    sinDec = 0.39782 * sin(rad(L))
    cosDec = cos(asin(sinDec))

    # calculate the solar local hour angle

    cosH = ((cos(rad(zenith)) - sinDec * sin(rad(lat))) / 
            (cosDec * cos(rad(lat))))

    # rise

    H = 360 - 180 / pi * acos(cosH)

    H = H / 15

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

    sunrise = ':'.join(str(t).split(':')[:2])

    sunrise = datetime.datetime(*date.timetuple()[:3]) + t

    # now do sunset

    t = N + (18 - hour) / 24

    # calculate mean anomaly

    M = (360 / 365.25 * t) - 3.289

    # calculate sun's true longitude

    L = M + 1.916 * sin(rad(M)) + (0.02 * sin(2 * rad(M))) + 282.634

    # adjust into 0, 360 range

    if L < 0:   L+=360
    if L > 360: L-=360
             
    # calculate right ascension

    RA = 180 / pi * atan(0.91764 * tan(rad(L)))

    # ensure RA is in the same quadrant as L

    Lquadrant  = (floor(L  / 90)) * 90
    RAquadrant = (floor(RA / 90)) * 90

    RA = RA + (Lquadrant - RAquadrant)

    # convert to hours

    RA = RA / 15

    # calculate the solar declination

    sinDec = 0.39782 * sin(rad(L))
    cosDec = cos(asin(sinDec))

    # calculate the solar local hour angle

    cosH = ((cos(rad(zenith)) - sinDec * sin(rad(lat))) / 
            (cosDec * cos(rad(lat))))

    # set

    H = 180 / pi * acos(cosH)

    H = H / 15

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

    sunset = datetime.datetime(*date.timetuple()[:3]) + t

    if nearest_hour: sunrise, sunset = time_round(sunrise), time_round(sunset)

    return sunrise, sunset

def is_daytime(lat, lon, times):
    """Determines whether the sun is up or not for each of the values in 
    "times" at the given latitude and longitude."""

    sunrise, sunset = zip(*[sun(lat, lon, t, nearest_hour = True) 
                            for t in times])

    return [r <= t and t < s for t, r, s in zip(times, sunrise, sunset)]

#lat, lon = 41.5772, -93.7111
#date = datetime.datetime(2013, 2, 18)
#date = datetime.datetime(2013, 6, 1)
#rise, set = sun(lat, lon, date)

