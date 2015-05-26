# make_timeseries.py
#
# David J. Lampert (djlampert@gmail.com)
# 
# extracts the grid point for a watershed from the preprocessed NRCM data

import os, pickle, datetime, numpy

from shapefile                           import Reader
from pyhspf.forecasting.penman           import penman_hourly
from pyhspf.forecasting.climateplots     import plot_hourlyET
from pyhspf.forecasting.crop_coefficient import calculate_cropPET

def average_timeseries(directory,
                       variables = ['temperature', 'wind', 'humidity', 
                                    'solar', 'evaporation', 'snowdepth']):

    averages = '{}/averages'.format(directory)

    if not os.path.isdir(averages): os.mkdir(averages)

    for v in variables:

        destination = '{}/average_{}'.format(averages, v)

        if not os.path.isfile(destination):

            print('averaging {} timeseries...\n'.format(v))

            series = []
            source = '{}/{}'.format(directory, v)
            for f in os.listdir(source):
                p = '{}/{}'.format(source, f)
                with open(p, 'rb') as d: ts, data = zip(*pickle.load(d))
                series.append(numpy.array(data))

            values = sum(series) / len(series)

            average = [(t, va) for t, va in zip(ts, values)]

            with open(destination, 'wb') as f: pickle.dump(average, f)

def extract_NRCM(source, destination, HUC8, start, end, plot = True):
    """Extracts the raw data from the regional climate model."""
    
    extract_raw(source, destination, HUC8, plot = plot)

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end,   1, 1)

    d = '{}/{}/NRCM'.format(destination, HUC8)

    if not any([os.path.isdir('{}/{}'.format(d, ts))
                for ts in ['rain', 'snowdepth', 'temperature', 'humidity', 
                           'wind', 'solar', 'evaporation']]): 
        extract_timeseries(d, s, e)

def get_distance(p1, p2):
    """Approximates the distance in kilometers between two points on the 
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
          numpy.cos(3 *phim * deg_rad) + 0.0012 * 
          numpy.cos(5 * phim * deg_rad))

    return numpy.sqrt(k1**2 * dphi**2 + k2**2 * dlam**2)

def weighted_average(series, weights = None):
    """Takes the inverse distance weighted average of the timeseries in the
    files in "directory". 
    """

    # if no weights provided just use the averages

    if weights is None: weights = [1 / len(series) for s in series]

    array   = numpy.array([row for row in zip(*series)])
    weights = numpy.array([weights for row in array])

    avg =  numpy.array([(a * w).sum() / w.sum()
                        for a, w in zip(array, weights)])

    return avg

def make_precipitation(subbasins, directory, verbose = True):

    # precipitation timeseries for all the gridpoints from rain + snow

    rain = '{}/rain'.format(directory)

    gages = []
    precips = []
    for f in os.listdir(rain):

        with open('{}/{}'.format(rain, f), 'rb') as g: 
            t, r = zip(*pickle.load(g))

        precips.append(numpy.array(r))
        
        # add the longitude and latitude to the list

        i = f.index('_')
        gages.append((float(f[:i]), float(f[i+1:])))

    # keep track of the subbasin timeseries in a directory

    d = '{}/subbasinprecipitation'.format(directory)
    if not os.path.isdir(d): os.mkdir(d)

    start = t[0]
    for comid, subbasin in subbasins.items(): 

        output = '{}/{}'.format(d, comid)

        if not os.path.isfile(output):
            if verbose: 
                print('making a precipitation time series for subbasin ' +
                      '{}'.format(comid))

            # find the gages and the distance from the centroid

            centroid  = subbasin.flowplane.centroid
            distances = [get_distance(centroid, [lon, lat])
                         for lon, lat in gages]

            # use the inverse weighted distance average

            weights = [1 / d if d > 0 else 0 for d in distances]

            # iterate through the time period and fill in the hourly values
            # and convert to mm

            precip = weighted_average(precips, weights = weights)

            # dump the results and save for later

            with open(output, 'wb') as f: pickle.dump((start, 180, precip), f)

def calculate_gamma(T, RH, b = 18.678, c = 257.14):
    """Estimates the psychometric constant from the temperature and
    relative humidity (%) using the Magnus formula."""

    return numpy.log(RH / 100) + b * T / (c + T)

def calculate_dewpoint(T, RH, a = 6.1121, b = 18.678, c = 257.14):
    """Estimates the dewpoint temperature (C) from the temperature and
    relative humidity (%) using the Magnus formula."""

    gamma = calculate_gamma(T, RH, b = b, c = c)

    return c * gamma / (b - gamma)

def make_dewpoint(directory):
    """Makes an hourly timeseries of dewpoint temperatures."""

    # open the 3-hr temperature and daily relative humidity files

    tempfile = '{}/average_temperature'.format(directory)
    hfile    = '{}/average_humidity'.format(directory)
    dewfile  = '{}/average_dewpoint'.format(directory)

    if not os.path.isfile(dewfile):

        with open(tempfile, 'rb') as f: ts, Ts  = zip(*pickle.load(f))
        with open(hfile, 'rb') as f:    ts, RHs = zip(*pickle.load(f))

        # aggregate 3-hr temperatures to daily and get the Tmin

        Tmins = [min(Ts[i:i+8]) for i in range(0, len(Ts), 8)]
        Ts    = [sum(Ts[i:i+8]) / 8 for i in range(0, len(Ts), 8)]

        # calculate the daily dewpoint

        dewpoints = calculate_dewpoint(numpy.array(Ts), numpy.array(RHs))
        dewpoints = [(t, min(Tm, T)) for t, T, Tm in zip(ts, dewpoints, Tmins)]

        with open(dewfile, 'wb') as f: pickle.dump(dewpoints, f)

def make_timeseries(directory, HUC8, start, end, evapstations = None, 
                    plot = True):
    """Makes an hourly timeseries of the reference evapotranspiration using
    the ASCE hourly Penman-Monteith Equation."""

    nrcm = '{}/{}/NRCM'.format(directory, HUC8)

    # start and end datetime instances

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end,   1, 1)

    # average the time series together from the NRCM simulation

    average_timeseries(nrcm)

    # open the watershed info to use to make subbasin precipitation

    watershedfile = '{}/{}/watershed'.format(directory, HUC8)

    with open(watershedfile, 'rb') as f: watershed = pickle.load(f)

    make_precipitation(watershed.subbasins, nrcm)

    # convert temperature and humidity to dewpoint

    make_dewpoint('{}/{}/NRCM/averages'.format(directory, HUC8))

    # open the 3-hr temperature, solar, and dewpoint, and daily wind files

    tempfile  = '{}/averages/average_temperature'.format(nrcm)
    solarfile = '{}/averages/average_solar'.format(nrcm)
    dewfile   = '{}/averages/average_dewpoint'.format(nrcm)
    windfile  = '{}/averages/average_wind'.format(nrcm)

    # watershed timeseries

    output = '{}/watershedtimeseries'.format(nrcm)

    if not os.path.isdir(output): os.mkdir(output)

    hourlytemp  = '{}/hourlytemperature'.format(output)
    hourlysolar = '{}/hourlysolar'.format(output)
    dailydew    = '{}/dewpoint'.format(output)
    dailywind   = '{}/wind'.format(output)
    hourlyRET   = '{}/hourlyRET'.format(output)
    hourlyPETs  = '{}/hourlyPETs'.format(output)
   
    if not os.path.isfile(hourlyRET):

        print('calculating an hourly time series for the reference ET...\n')

        # open the bounding box and get the mean lat, lon, and elevation

        f  = '{0}/{1}/{1}boundaries'.format(directory, HUC8)
        sh = Reader(f)

        record = sh.record(0)
        lon, lat, elev = record[-3:]

        with open(windfile,  'rb') as f: ts, Ws   = zip(*pickle.load(f))
        with open(tempfile,  'rb') as f: ts, Ts   = zip(*pickle.load(f))
        with open(solarfile, 'rb') as f: ts, Ss   = zip(*pickle.load(f))
        with open(dewfile,   'rb') as f: ts, dews = zip(*pickle.load(f))

        # dump the daily series

        with open(dailydew,  'wb') as f: 
            pickle.dump((s, 1440, list(dews)), f)
        with open(dailywind, 'wb') as f: 
            pickle.dump((s, 1440, list(Ws)), f)

        # dump all the hourly series and convert the solar radiation 
        # from Watts/m2 to MJ/hour/m2

        temp  = [T for T in Ts for i in range(3)]
        solar = [S for S in Ss for i in range(3)]

        with open(hourlysolar, 'wb') as f: pickle.dump((s, 60, solar), f)
        with open(hourlytemp,  'wb') as f: pickle.dump((s, 60, temp),  f)

        # convert to hourly numpy arrays

        temp     = numpy.array(temp)
        solar    = numpy.array(solar) * 3600 / 10**6
        wind     = numpy.array([w for w in Ws   for i in range(24)])
        dewpoint = numpy.array([T for T in dews for i in range(24)])

        # dates

        dates = [s + i * datetime.timedelta(hours = 1) 
                 for i in range(len(solar))]
 
        RET = penman_hourly(lat, lon, elev, dates, temp, dewpoint, solar, wind,
                            verbose = False)

        # dump the timeseries

        with open(hourlyRET, 'wb')   as f: pickle.dump((s, 60, RET), f)

    if not os.path.isfile(hourlyRET + '.png'):
        with open('{}/hourlytemperature'.format(output), 'rb') as f: 
            s, t, temp = pickle.load(f)
        with open('{}/dewpoint'.format(output), 'rb') as f: 
            s, t, dewpoint = pickle.load(f)
        with open('{}/wind'.format(output), 'rb') as f: 
            s, t, wind = pickle.load(f)
        with open('{}/hourlysolar'.format(output), 'rb') as f: 
            s, t, solar = pickle.load(f)
        with open(hourlyRET, 'rb') as f: 
            s, t, hRET = pickle.load(f)

        # Watts/m2 to kW hr/m2

        solar = [s * 0.024 for s in solar]

        if evapstations is not None:
            with open(evapstations, 'rb') as f: evaporations = pickle.load(f)
        else:
            evaporations = {}

        plot_hourlyET(HUC8, s, e, evaporations, [hRET], temp,
                      dewpoint, wind, solar, fill = True, 
                      colors = ['green', 'yellow', 'orange', 'red'],
                      output = hourlyRET)

    if not os.path.isfile(hourlyPETs):
        calculate_cropPET(directory, HUC8, s, e, output = output,
                          evaporations = False)
