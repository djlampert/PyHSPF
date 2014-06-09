#!/usr/bin/env python3
#
# File: extract_NCDC.py
#
# by David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Last updated: 11/16/2013
#
# Purpose: imports climate data files to Python classes

import os, pickle, datetime, shutil

from shapefile import Reader, Writer

from pyhspf.preprocessing.ncdcstations    import PrecipStation
from pyhspf.preprocessing.ncdcstations    import TempStation
from pyhspf.preprocessing.ncdcstations    import SnowStation
from pyhspf.preprocessing.ncdcstations    import EvapStation
from pyhspf.preprocessing.ncdcstations    import WindStation
from pyhspf.preprocessing.ncdcstations    import DewStation
from pyhspf.preprocessing.ncdcstations    import SolarStation
from pyhspf.preprocessing.make_timeseries import make_timeseries

def extract_precipitation(directory, HUC8, start, end, plot = True, 
                          verbose = True):
    """Extracts hourly precipitation station data meeting the criteria."""

    # get the filenames from the 3240 Stations

    path = '{}/{}/precip3240'.format(directory, HUC8)
    filenames = [f for f in os.listdir(path) 
                 if f[-3:] != 'png' and f[-3:] != 'tar' and f[-2:] != '.Z']

    # make a new directory for the precipitation stations

    if not os.path.isdir('{}/{}/precipitations'.format(directory, HUC8)):
        os.mkdir('{}/{}/precipitations'.format(directory, HUC8))
        
    # pickle location for output dictionary

    outputfile = '{}/{}/precipitations/precipitation'.format(directory, HUC8)

    if not os.path.isfile(outputfile):

        # remove all stations with no data

        lengths = []
        for n in filenames:

            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            lengths.append(len(station.events))
            station = None

        filenames = [f for f, l in zip(filenames, lengths) if l > 0]

        # remove all stations missing more than 20% data

        missing = []
        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            missing.append(station.pct_missing(start, end))
            station = None

        filenames = [f for f, m in zip(filenames, missing) if m < 0.2]

        # keep the precipitation station data in a dictionary of objects

        precipstations = {}

        # iterate through the files and keep those meeting the criteria

        for n in filenames:
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            precipstation = PrecipStation()

            precipstation.add_location(station)

            values = station.make_timeseries(start = start, end = end)
            times = [start + i * datetime.timedelta(hours = 1)
                     for i in range((end - start).days * 24)]

            precipstation.add_timeseries(zip(times, values))

            if verbose: print('adding {} precipitation'.format(station.desc) +
                              ' to the model')

            precipstations[n] = precipstation

        with open(outputfile, 'wb') as f: pickle.dump(precipstations, f)

    if plot:

        with open(outputfile, 'rb') as f: precipstations = pickle.load(f)

        from climateplots import plot_precipitation

        n = 4
        if len(precipstations) < n: l = [0, len(precipstations) - 1]
        else:
            floor     = len(precipstations) // n
            remainder = len(precipstations) % n
            l = [n * i for i in range(floor + 1)]
            if remainder > 0: l += [len(precipstations)]
            
        for year in range(start.year, end.year):
            output = '{}/{}/precipitations/{}'.format(directory, HUC8, year)
            outputs = ['{}_{}_{}'.format(output, i, j) 
                       for i, j in zip(l[:-1], l[1:])]

            if any([not os.path.isfile(o + '.png') for o in outputs]):
                plot_precipitation(outputfile, HUC8, year, output = output, 
                                   verbose = True)
    
def extract_temperature(directory, HUC8, start, end, cutoff = 4,
                        plot = True, verbose = True):
    """Extracts temperatures from the GHCND station data meeting the 
    criteria."""

    # get the filenames from the GHCND Stations

    path = '{}/{}/GHCND'.format(directory, HUC8)
    filenames = [f for f in os.listdir(path) if f[-3:] != 'png']

    # make a new directory for the temperature stations

    if not os.path.isdir('{}/{}/temperatures'.format(directory, HUC8)):
        os.mkdir('{}/{}/temperatures'.format(directory, HUC8))

    # if there are many stations, just take some using the cutoff

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            lengths.append(len([d for d, t in station.tmin 
                                if -60 < t and start <= d and d <= end]))
        
        # establish cutoff for stations; take at most "cutoff" 
        # (i.e., take stations with the most observations)

        c = [l for l in lengths]  # make a copy
        c.sort()                  # sort it
        c.reverse()               # reverse it (= highest to lowest)
        try:    v = c[cutoff - 1] # find the value of the cutoff length
        except: 
            print('warning: error in cutoff length')
            raise

        if v > 0: stations = [f for f, l in zip(filenames, lengths) if l >= v]
        else:     stations = [f for f, l in zip(filenames, lengths) if l > v]

    else: stations = filenames

    # keep the temperature station data in a dictionary of objects

    tempstations = {}

    # iterate through the files and keep those meeting the criteria

    for n in stations:

        with open('{}/{}'.format(path, n), 'rb') as f: station = pickle.load(f)

        tmindata = [(d, t) for d, t in station.tmin
                    if -50 < t and start <= d and d <= end]
        tmaxdata = [(d, t) for d, t in station.tmax
                    if -50 < t and start <= d and d <= end]
    
        if verbose:
            print('adding {} temperature to the model'.format(station.name))

        tempstation = TempStation()

        tempstation.add_location(station)
        tempstation.add_tmin(tmindata)
        tempstation.add_tmax(tmaxdata)

        tempstations[n] = tempstation

    name = '{}/{}/temperatures/temperature'.format(directory, HUC8)
    with open(name, 'wb') as f: pickle.dump(tempstations, f)

    if plot:
        from climateplots import plot_temperature

        for year in range(start.year, end.year):
            output = '{}/{}/temperatures/{}'.format(directory, HUC8, year)
            if not os.path.isfile(output + '.png'):
                plot_temperature(name, HUC8, year, output = output, 
                                 verbose = True)

def extract_snow(directory, HUC8, start, end, cutoff = 4,
                 plot = True, verbose = True):
    """Extracts snowfall and snowdepth data from the GHCND."""

    # get the filenames from the GHCND Stations

    path = '{}/{}/GHCND'.format(directory, HUC8)
    filenames = [f for f in os.listdir(path) if f[-3:] != 'png']

    # make a new directory for the snow stations

    if not os.path.isdir('{}/{}/snow'.format(directory, HUC8)):
        os.mkdir('{}/{}/snow'.format(directory, HUC8))

    # if there are many stations, just take some using the cutoff

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            lengths.append(len([d for d, s in station.snowdepth
                                if 0 <= s and start <= d and d <= end]))
        
        # establish cutoff for stations; take at most "cutoff" 
        # (i.e., take stations with the most observations)

        c = [l for l in lengths]  # make a copy
        c.sort()                  # sort it
        c.reverse()               # reverse it (= highest to lowest)
        try:    v = c[cutoff - 1] # find the value of the cutoff length
        except: 
            print('error in cutoff length')
            raise

        if v > 0: stations = [f for f, l in zip(filenames, lengths) if l >= v]
        else:     stations = [f for f, l in zip(filenames, lengths) if l > v]

    else: stations = filenames

    # keep the snow station data in a dictionary of objects

    snowstations = {}

    # iterate through the files and find those meeting the criteria

    for n in stations:

        with open('{}/{}'.format(path, n), 'rb') as f: station = pickle.load(f)

        if verbose: print('adding {} snow to the model'.format(station.name))

        # find the data over the desired period

        snowfalldata  = [(d, s) for d, s in station.snowfall
                         if 0 <= s and start <= d and d <= end]
        snowdepthdata = [(d, s) for d, s in station.snowdepth
                         if 0 <= s and start <= d and d <= end]
    
        snowstation = SnowStation()
            
        snowstation.add_location(station)

        snowstation.add_snowdepth(snowdepthdata)
        snowstation.add_snowfall(snowfalldata)

        snowstations[station.station] = snowstation

    name = '{}/{}/snow/snow'.format(directory, HUC8)
    with open(name, 'wb') as f: pickle.dump(snowstations, f)

    if plot:
        from climateplots import plot_snowdepth

        for year in range(start.year, end.year):
            output = '{}/{}/snow/{}'.format(directory, HUC8, year)
            if not os.path.isfile(output + '.png'):
                plot_snowdepth(name, HUC8, year, output = output, 
                               verbose = True)

def extract_evaporation(directory, HUC8, start, end, cutoff = 4,
                        plot = True, verbose = True):
    """Extracts evaporation data from the GHCND."""

    # get the filenames from the GHCND Stations

    path = '{}/{}/GHCND'.format(directory, HUC8)
    filenames = [f for f in os.listdir(path) if f[-3:] != 'png']

    # make a new directory for the snow stations

    if not os.path.isdir('{}/{}/evaporations'.format(directory, HUC8)):
        os.mkdir('{}/{}/evaporations'.format(directory, HUC8))

    # if there are many stations, just take some using the cutoff

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            lengths.append(len([d for d, e in station.evap
                                if 0 <= e and start <= d and d <= end]))
        
        # establish cutoff for stations; take at most "cutoff" 
        # (i.e., take stations with the most observations)

        c = [l for l in lengths]  # make a copy
        c.sort()                  # sort it
        c.reverse()               # reverse it (= highest to lowest)
        try:    v = c[cutoff - 1] # find the value of the cutoff length
        except: 
            print('warning: error in cutoff length')
            raise

        if v > 0: stations = [f for f, l in zip(filenames, lengths) if l >= v]
        else:     stations = [f for f, l in zip(filenames, lengths) if l > v]

    else: stations = filenames

    if len(stations) == 0:
        print('warning: unable to find evaporation data')
        return

    # store the stations in a dictionary

    evapstations = {}

    # iterate through the files and find those meeting the criteria

    for n in stations:

        with open('{}/{}'.format(path, n), 'rb') as f: station = pickle.load(f)

        if verbose: 
            print('adding {} evaporation to the model'.format(station.name))

        # find the data over the desired period

        evapdata  = [(d, e) for d, e in station.evap
                     if 0 <= e and start <= d and d <= end]
    
        evapstation = EvapStation()
            
        evapstation.add_location(station)
        evapstation.add_data(station.evap)

        evapstations[n] = evapstation

    name = '{}/{}/evaporations/evaporation'.format(directory, HUC8)
    with open(name, 'wb') as f: pickle.dump(evapstations, f)

    if plot:
        from climateplots import plot_evaporation

        for year in range(start.year, end.year):
            output = '{}/{}/evaporations/{}'.format(directory, HUC8, year)
            if not os.path.isfile(output + '.png'):
                plot_evaporation(name, HUC8, year, output = output, 
                                 verbose = True)

def extract_wind(directory, HUC8, start, end, database = 'GSOD', cutoff = 4,
                 plot = True, verbose = True):
    """Extracts wind speed data from the GHCND or GSOD."""

    if database not in ['GSOD', 'GHCND']:
        print('warning: unknown database specified')
        return

    # get the filenames from the GHCND Stations

    path = '{}/{}/{}'.format(directory, HUC8, database)

    filenames = [f for f in os.listdir(path) if len(f) < 7]
                 
    # make a new directory for the wind stations

    if not os.path.isdir('{}/{}/wind'.format(directory, HUC8)):
        os.mkdir('{}/{}/wind'.format(directory, HUC8))

    # add together the stations into a unique file (USAF)

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

    # if there are many stations, just take some using the cutoff

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            lengths.append(len([d for d, w in station.wind
                                if 0 <= w and start <= d and d <= end]))
        
        # establish cutoff for stations; take at most "cutoff" 
        # (i.e., take stations with the most observations)

        c = [l for l in lengths]  # make a copy
        c.sort()                  # sort it
        c.reverse()               # reverse it (= highest to lowest)
        try:    v = c[cutoff - 1] # find the value of the cutoff length
        except: 
            print('warning: error in cutoff length')
            raise

        if v > 0: stations = [f for f, l in zip(filenames, lengths) if l >= v]
        else:     stations = [f for f, l in zip(filenames, lengths) if l > v]

    else: stations = filenames

    if len(stations) == 0:
        print('warning: unable to find wind data')
        return

    # keep the wind station data in a dictionary of objects

    windstations = {}

    # iterate through the files and find those meeting the criteria

    for n in stations:

        with open('{}/{}'.format(path, n), 'rb') as f: station = pickle.load(f)

        # find the data over the desired period

        winddata  = [(d, w) for d, w in station.wind
                     if 0 <= w and start <= d and d <= end]

        if len(winddata) > 0:
            if verbose: 
                print('adding {} wind to the model'.format(station.name))

            windstation = WindStation()
            windstation.add_location(station)
            windstation.add_data(winddata)
            windstations[n] = windstation

    name = '{}/{}/wind/wind'.format(directory, HUC8)
    with open(name, 'wb') as f: pickle.dump(windstations, f)

    if plot:
        from climateplots import plot_wind

        for year in range(start.year, end.year):
            output = '{}/{}/wind/{}'.format(directory, HUC8, year)
            if not os.path.isfile(output + '.png'):
                plot_wind(name, HUC8, year, output = output, verbose = True)

def extract_dewpoint(directory, HUC8, start, end, database = 'GSOD', cutoff = 4,
                     plot = True, verbose = True):
    """Extracts wind speed data from the GHCND or GSOD."""

    if database not in ['GSOD', 'GHCND']:
        print('warning: unknown database specified')
        return

    # get the filenames from the GHCND Stations

    path = '{}/{}/{}'.format(directory, HUC8, database)

    filenames = [f for f in os.listdir(path) if len(f) < 7]
                 
    # make a new directory for the dewpoint stations

    if not os.path.isdir('{}/{}/dewpoint'.format(directory, HUC8)):
        os.mkdir('{}/{}/dewpoint'.format(directory, HUC8))

    # add together the stations into a unique file (USAF)

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

    # if there are many stations, just take some using the cutoff

    if len(filenames) > cutoff:

        # figure out the number of observations at each station

        lengths = []

        for n in filenames:
            
            with open('{}/{}'.format(path, n), 'rb') as f: 
                station = pickle.load(f)

            lengths.append(len([d for d, t in station.dewpoint
                                if 0 <= t and start <= d and d <= end]))
        
        # establish cutoff for stations; take at most "cutoff" 
        # (i.e., take stations with the most observations)

        c = [l for l in lengths]  # make a copy
        c.sort()                  # sort it
        c.reverse()               # reverse it (= highest to lowest)
        try:    v = c[cutoff - 1] # find the value of the cutoff length
        except: 
            print('warning: error in cutoff length')
            raise

        if v > 0: stations = [f for f, l in zip(filenames, lengths) if l >= v]
        else:     stations = [f for f, l in zip(filenames, lengths) if l > v]

    else: stations = filenames

    if len(stations) == 0:
        print('warning: unable to find dewpoint data')
        return

    # keep the dewpoint station data in a dictionary of objects

    dewstations = {}

    # iterate through the files and find those meeting the criteria

    for n in stations:

        with open('{}/{}'.format(path, n), 'rb') as f: station = pickle.load(f)

        # find the data over the desired period

        dewdata  = [(d, t) for d, t in station.dewpoint
                    if -60 < t and start <= d and d <= end]

        if len(dewdata) > 0:
            if verbose: 
                print('adding {} dewpoint to the model'.format(station.name))

            dewstation = DewStation()
            dewstation.add_location(station)
            dewstation.add_data(dewdata)
            dewstations[n] = dewstation

    name = '{}/{}/dewpoint/dewpoint'.format(directory, HUC8)
    with open(name, 'wb') as f: pickle.dump(dewstations, f)

    if plot:
        from climateplots import plot_dewpoint

        for year in range(start.year, end.year):
            output = '{}/{}/dewpoint/{}'.format(directory, HUC8, year)
            if not os.path.isfile(output + '.png'):
                plot_dewpoint(name, HUC8, year, output = output, verbose = True)

def extract_solar(directory, HUC8, start, end, cutoff = 4, plot = True,
                  verbose = True):

    d = '{0}/{1}/NSRDB'.format(directory, HUC8)
    filenames = [f for f in os.listdir(d) if f[-3:] != 'png']

    if not os.path.isdir('{}/{}/solar'.format(directory, HUC8)):
        os.mkdir('{}/{}/solar'.format(directory, HUC8))

    # pull all the stations into a list

    stations = []

    for n in filenames:

        # open the station data

        with open('{}/{}'.format(d, n), 'rb') as f: 
            stations.append(pickle.load(f))

    # figure out which stations to use

    solarstations = []

    # start by taking any w/legacy data or class 1 stations (sparse)

    class1s = [s for s in stations if s.sclass == 1 or len(s.legacy) > 0]

    while len(solarstations) < cutoff and len(solarstations) < len(class1s):

        others = [s for s in class1s if s not in solarstations]
        other_lengths = [len(s.metstat) for s in others]

        longest = others[other_lengths.index(max(other_lengths))]
        solarstations.append(longest)

    # if less than the cutoff, take any class 2 stations

    class2s = [s for s in stations if s.sclass == 2]

    while (len(solarstations) < cutoff and 
           len(solarstations) < (len(class1s) + len(class2s))):

        others = [s for s in class2s if s not in solarstations]
        other_lengths = [len(s.metstat) for s in others]

        longest = others[other_lengths.index(max(other_lengths))]
        solarstations.append(longest)

    # if less than the cutoff, take stations based on record length until
    # reaching the cutoff or all are included

    while len(solarstations) < cutoff and len(solarstations) < len(stations):

        others = [s for s in stations if s not in solarstations]
        other_lengths = [len(s.metstat) for s in others]

        longest = others[other_lengths.index(max(other_lengths))]
        solarstations.append(longest)

    solarstations = {s.usaf: SolarStation(s) for s in solarstations}
        
    name = '{}/{}/solar/solarstations'.format(directory, HUC8)
    with open(name, 'wb') as f: pickle.dump(solarstations, f)

    if plot:

        from climateplots import plot_solar

        for year in range(start.year, end.year):
            output = '{}/{}/solar/{}'.format(directory, HUC8, year)
            if not os.path.isfile(output + '.png'):
                plot_solar(name, HUC8, year, output = output, 
                           verbose = True)

def extract_climate(directory, HUC8, start, end, plot = False):
    """Extracts select data from the raw climate data for processing."""

    # extract temperature stations from GHCND meeting the criteria

    if not os.path.isdir('{}/{}/temperatures'.format(directory, HUC8)):
        extract_temperature(directory, HUC8, start, end, plot = plot)

    # extract snow stations from GHCND meeting the criteria

    if not os.path.isdir('{}/{}/snow'.format(directory, HUC8)):
        extract_snow(directory, HUC8, start, end, plot = plot)

    # extract evaporation stations from GHCND meeting the criteria

    if not os.path.isdir('{}/{}/evaporations'.format(directory, HUC8)):
        extract_evaporation(directory, HUC8, start, end, plot = plot)

    # extract wind stations from GSOD meeting the criteria

    if not os.path.isdir('{}/{}/wind'.format(directory, HUC8)):
        extract_wind(directory, HUC8, start, end, plot = plot)

    # extract dewpoint stations from GSOD meeting the criteria

    if not os.path.isdir('{}/{}/dewpoint'.format(directory, HUC8)):
        extract_dewpoint(directory, HUC8, start, end, plot = plot)

    # extract the solar radiation from NSRDB

    if not os.path.isdir('{}/{}/solar'.format(directory, HUC8)):
        extract_solar(directory, HUC8, start, end, plot = plot)

    # extract the hourly precipitation data from the Precip3240 dataset

    if not os.path.isdir('{}/{}/precipitation'.format(directory, HUC8)):
        extract_precipitation(directory, HUC8, start, end, plot = plot)
        print('')

    v = directory, HUC8
    if (not os.path.isdir('{}/{}/watershedtimeseries'.format(*v)) or
        not os.path.isdir('{}/{}/subbasinprecipitation'.format(*v)) or
        not os.path.isfile('{}/{}/snow/snowfall'.format(*v)) or
        not os.path.isfile('{}/{}/snow/snowdepth'.format(*v)) or
        not os.path.isfile('{}/{}/NWIS/gagestations'.format(*v))
        ):
        make_timeseries(directory, HUC8, start, end)

# Windows

#if os.name == 'nt': 
#    directory  = 'C:/HSPF_data'

# Linux/Unix

#else:               
#    directory = '/home/dave/HSPF_data'

# watershed and start and end dates

#HUC8      = '07080106'
#HUC8      = '07100008'
#HUC8      = '07130004'
#HUC8      = '07080205'
#start     = datetime.datetime(1980, 1, 1)
#end       = datetime.datetime(2010, 1, 1)

#download_climate(directory, HUC8, start, end)
#extract_climate(directory, HUC8, start, end)
