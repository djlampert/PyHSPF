# make_timeseries.py
#
# David J. Lampert, PhD, PE
#
# last updated: 12/04/2013
#
# makes timeseries for the hourly and daily RET for a watershed from 
# extracted climate data

import numpy as np, datetime, os, pickle, math

from shapefile import Reader

from .penman           import penman_daily
from .penman           import penman_hourly
from .crop_coefficient import calculate_cropPET
from .climateplots     import plot_dailyET
from .climateplots     import plot_dayofyearET
from .climateplots     import plot_hourlyET

def is_integer(s):
    """Tests if string "s" is an integer."""
    try: int(s) 
    except ValueError: return False
    return True

def get_distance(p1, p2):
    """Approximates the distance in kilometers between two points on the 
    Earth's surface designated in decimal degrees using an ellipsoidal 
    projection. per CFR 73.208 it is applicable for up to 475 kilometers.
    p1 and p2 are listed as (longitude, latitude).
    """

    deg_rad = math.pi / 180

    dphi = p1[1] - p2[1]
    phim = 0.5 * (p1[1] + p2[1])
    dlam = p1[0] - p2[0]

    k1 = (111.13209 - 0.56605 * math.cos(2 * phim * deg_rad) + 0.00120 * 
          math.cos(4 * phim * deg_rad))
    k2 = (111.41513 * math.cos(phim * deg_rad) - 0.09455 * 
          math.cos(3 *phim * deg_rad) + 0.0012 * math.cos(5 * phim * deg_rad))

    return np.sqrt(k1**2 * dphi**2 + k2**2 * dlam**2)

def weighted_avg(m, weights = None):
    """Returns the weighted average of the list of lists while ignoring 
    missing values."""

    if weights is None: weights = [1 / len(m[0]) for i in range(len(m[0]))]

    array   = np.array([row for row in zip(*m)])
    mask    = np.invert(np.equal(array, None))
    weights = np.array([weights for l in array])

    avg =  np.array([0 if np.invert(m).all() 
                     else (a[m] * w[m]).sum() / w[m].sum()
                     for a, m, w in zip(array, mask, weights)])

    return avg

def hourly_temperature(tmin, tmax):
    """Develops an hourly time series of temperatures from the daily min 
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
                hourly.append(mid + amp * math.sin((i - 5) * math.pi / 10))
                
        if mi2 is None or ma is None:
            for i in range(14): hourly.append(None)
        else:
            mid = 0.5 * (ma + mi2)
            amp = 0.5 * (ma - mi2)
            for i in range(14): 
                hourly.append(mid + amp* math.cos(i * math.pi / 14))

    # add the last day

    if tmin[-1] is None or tmax[-1] is None:
        for i in range(18): hourly.append(None)
    else:
        for i in range(10):
            hourly.append(tmin[-1] + (tmax[-1] - tmin[-1]) * i / 10)
        
        # assume tmax to the end of the day

        for i in range(8): hourly.append(tmax[-1])

    return hourly

def aggregate_temperature(tempstations, dewstations, start, end, destination):
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
            if v is not None: day.append(v)
            elif len(tmins) == 1: day.append(tmin[-1])        
        try:
            tmin.append(sum(day) / len(day))                
        except: print(day)

    for row in zip(*tmaxs):
        day = []
        for v in row:
            if v is not None: day.append(v)
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
        if len(day) > 0: dewpoint.append(sum(day) / len(day))
        else:            dewpoint.append(None)

    # fill in missing data with tmin

    for t, d, i in zip(tmin, dewpoint, range(len(dewpoint))):

        if d is None or d > t: dewpoint[i] = t

    with open('{}/dewpoint'.format(destination), 'wb') as f: 
        pickle.dump((start, 1440, dewpoint), f)

def aggregate_wind(windstations, start, end, destination):
    """Processes the raw wind speed data into a single daily timeseries."""

    winds = [v.make_timeseries(start, end) for k, v in windstations.items()]
    winds = [w for w in winds if w is not None]

    wind = []
    for row in zip(*winds):
        day = [v for v in row if v is not None]
        if len(day) > 0: wind.append(sum(day) / len(day))
        else:            wind.append(wind[-1])

    with open('{}/wind'.format(destination), 'wb') as f:
        pickle.dump((start, 1440, wind), f)

def aggregate_solar(solarstations, start, end, destination):
    """Processes the raw solar radiation data into a single daily and
    hourly timeseries."""

    solars = [v.make_timeseries(start, end) for k, v in solarstations.items()]
    solars = [s for s in solars if s is not None]

    solar = []
    for row in zip(*solars):
        hour = [v for v in row if v is not None]
        if len(hour) > 0: solar.append(sum(hour) / len(hour))
        else:             solar.append(solar[-1])

    with open('{}/hourlysolar'.format(destination), 'wb') as f: 
        pickle.dump((start, 60, solar), f)

    # aggregate the hourly to daily

    dsolar = [sum(solar[i:i+24]) / 24 for i in range(0, len(solar), 24)]

    with open('{}/dailysolar'.format(destination), 'wb') as f: 
        pickle.dump((start, 1440, dsolar), f)

def make_precipitation(subbasins, gages, start, end, destination,
                       plot = False, verbose = True):
    """Takes values of precipitation for the watershed from the PrecipStation
    instances and performs an inverse-distance weighted average for each 
    subbasin centroid and assigns them to a WDM file."""

    # make timeseries for all the gauges

    hourly = [v.make_timeseries(start, end) for k, v in gages.items()]

    # keep track of the subbasin timeseries in a dictionary

    d = '{}/subbasinprecipitation'.format(destination)
    if not os.path.isdir(d): os.mkdir(d)

    for subbasin in subbasins: 

        output = '{}/{}'.format(d, subbasin)

        if not os.path.isfile(output):
            if verbose: 
                print('making a precipitation time series for subbasin ' +
                      '{}'.format(subbasin))

            # find the gages and the distance from the centroid

            centroid  = subbasins[subbasin].flowplane.centroid
            distances = [get_distance(centroid, [v.longitude, v.latitude])
                         for k,v in gages.items()]

            # use the inverse weighted distance average

            weights = [1 / d if d > 0 else 0 for d in distances]

            # iterate through the time period and fill in the hourly values
            # and convert to mm

            precip = [25.4 * p for p in weighted_avg(hourly, weights = weights)]

            # dump the results and save for later

            with open(output, 'wb') as f: pickle.dump((start, 60, precip), f)

    precipitation = {}
    for subbasin in subbasins:

        output = '{}/{}'.format(d, subbasin)
        with open(output, 'rb') as f: 
            precipitation[subbasin] = pickle.load(f)

    return precipitation

def make_snow(directory, HUC8, start, end, verbose = True, vverbose = False):
    """Averages the data from the snowstations into a single timeseries."""

    if verbose: print('averaging snowfall and snowdepth station data\n')

    snowdata = '{}/{}/snow/snow'.format(directory, HUC8)
    with open(snowdata, 'rb') as f: snowstations = pickle.load(f)

    times = [start + datetime.timedelta(days = 1) * i 
             for i in range((end - start).days)]

    # snowfall

    data = [s.make_timeseries(start, end, tstype = 'fall', verbose = vverbose)
            for s in snowstations.values()]

    # remove any timeseries with insufficient data

    data = [d for d in data if d is not None]

    if len(data) == 0: 
        print('error, no snowfall data exist')
        raise

    falls = []
    for values in zip(*data):
        day = [0 if v is None else v for v in values]
        if len(day) > 0: falls.append(sum(day) / len(day))
        else:            falls.append(0)

    with open('{}/{}/snow/snowfall'.format(directory, HUC8), 'wb') as f:
        pickle.dump((times, falls), f)

    # snowdepths

    data = [s.make_timeseries(start, end, verbose = vverbose)
            for s in snowstations.values()]

    # remove any timeseries with insufficient data

    data = [d for d in data if d is not None]

    if len(data) == 0: 
        print('error, no snowfall data exist')
        raise

    depths = []
    for values in zip(*data):
        day = [0 if v is None else v for v in values]
        if len(day) > 0: depths.append(sum(day) / len(day))
        else:            depths.append(0)

    with open('{}/{}/snow/snowdepth'.format(directory, HUC8), 'wb') as f:
        pickle.dump((times, depths), f)

def make_gagestations(directory, HUC8, verbose = True):
    """Combines all the flow gage station data into a single file consisting
    of a dictionary with NWIS gage ids as keys and observations as values."""

    NWIS = '{}/{}/NWIS'.format(directory, HUC8)

    flowdata = {}
    waterquality = {}
    for g in os.listdir(NWIS):
        if is_integer(g):
            with open('{}/{}'.format(NWIS, g), 'rb') as f: 
                station = pickle.load(f)
            flowdata[g] = station.gagedates, station.gageflows
            waterquality[g] = station.waterquality

    with open('{}/dailydischarges'.format(NWIS), 'wb') as f: 
        pickle.dump(flowdata, f)
    with open('{}/waterquality'.format(NWIS), 'wb') as f:
        pickle.dump(waterquality, f)

def make_timeseries(directory, HUC8, start, end, verbose = True):
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

    v = directory, HUC8
    evapstations  = '{}/{}/evaporations/evaporation'.format(*v)
    tempstations  = '{}/{}/temperatures/temperature'.format(*v)
    dewstations   = '{}/{}/dewpoint/dewpoint'.format(*v)
    windstations  = '{}/{}/wind/wind'.format(*v)
    solarstations = '{}/{}/solar/solarstations'.format(*v)
    snowstations  = '{}/{}/snow/snow'.format(*v)
    gagedirectory = '{}/{}/NWIS'.format(*v)

    # output directory

    p = '{}/{}/watershedtimeseries'.format(*v)
    if not os.path.isdir(p): os.mkdir(p)

    # open the bounding box and get the mean latitude, longitude, and elevation

    f = '{0}/{1}/{1}boundaries'.format(*v)
    s = Reader(f)

    record = s.record(0)
    lon, lat, elev = record[-3:]

    # temperature and dewpoint

    if (not os.path.isfile('{}/tmin'.format(p)) or 
        not os.path.isfile('{}/tmax'.format(p)) or
        not os.path.isfile('{}/hourlytemperature'.format(p)) or
        not os.path.isfile('{}/dewpoint'.format(p))):

        with open(tempstations, 'rb') as f: tempstations = pickle.load(f)
        with open(dewstations, 'rb') as f:  dewstations  = pickle.load(f)

        aggregate_temperature(tempstations, dewstations, start, end, p)

    # wind speed

    if not os.path.isfile('{}/wind'.format(p)):

        with open(windstations, 'rb') as f: windstations = pickle.load(f)

        aggregate_wind(windstations, start, end, p)

    # solar radiation

    if (not os.path.isfile('{}/solar'.format(p)) or
        not os.path.isfile('{}/dailysolar'.format(p))):

        with open(solarstations, 'rb') as f: solarstations = pickle.load(f)

        aggregate_solar(solarstations, start, end, p)

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

        solar    = np.array(dsolar) * 86400 / 10**6
        tmin     = np.array(tmin)
        tmax     = np.array(tmax)
        dewpoint = np.array(dewpoint)
        wind     = np.array(wind)

        dates = [start + datetime.timedelta(days = 1) * i 
                 for i in range((end-start).days)]

        # Calculate daily timeseries using the ASCE short ref crop equation

        RET = penman_daily(lat, elev, dates, tmin, tmax, dewpoint, solar, wind, 
                           verbose = False)

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

        solar    = np.array(solar) * 3600 / 10**6
        temp     = np.array(temp)

        # assume dewpoint and wind are same throughout the day

        dewpoint = np.array([t for t in dewpoint for i in range(24)])
        wind     = np.array([w for w in wind for i in range(24)])

        dates = [start + datetime.timedelta(hours = 1) * i 
                 for i in range((end-start).days * 24)]

        # Calculate hourly timeseries using the ASCE short ref crop equation

        RET = penman_hourly(lat, lon, elev, dates, temp, dewpoint, solar, wind, 
                            verbose = False)

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

    # make the subbasin precipitation files from the watershed info and hourly
    # precipitation station files

    v = directory, HUC8
    watershed  = '{}/{}/watershed'.format(*v)
    precipfile = '{}/{}/precipitations/precipitation'.format(*v)

    with open(watershed,  'rb') as f: subbasins     = pickle.load(f).subbasins
    with open(precipfile, 'rb') as f: precipitation = pickle.load(f)

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

#dates = [datetime.datetime(2001, 1, 1), datetime.datetime(2001, 7, 1)]
#tmin  = np.array([-5, 20])
#tmax  = np.array([10, 30])
#dew   = np.array([-6, 18])
#rad   = np.array([10, 20])
#wind  = np.array([2, 3])
