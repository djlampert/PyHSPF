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

    # make a shapefile of the stations

    if not os.path.isfile(outputfile + '.shp'):
            
        with open(outputfile, 'rb') as f: precipstations = pickle.load(f)

        # copy the projection

        boundaryfile = '{0}/{1}/{1}boundaries'.format(directory, HUC8)
        shutil.copy(boundaryfile + '.prj', outputfile + '.prj')

        # fields for the new file

        fields = [['Station',     'N', 6,  0],
                  ['Name',        'C', 40, 0],
                  ['Elev_m',      'N', 6,  1],
                  ['avg_precip',  'N', 6,  2],
                  ['missing_p',   'N', 6,  2],
                  ['start_date',  'C', 10, 0],
                  ['end_date',    'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        # iterate through and fill in the records

        for station, info in precipstations.items():

            rain    = info.total_precipitation()
            missing = round(100 * info.pct_missing(), 1)

            # dates

            s = info.precip[0][0]
            e = info.precip[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)

            # calculate the avg

            avg_rainfall = round(rain / (e-s).days * 365.25, 2)

            w.point(info.longitude, info.latitude)
            w.record(station, info.name, round(info.elevation, 2), 
                     avg_rainfall, missing, start_date, end_date)

        w.save(outputfile)

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

    # make a shapefile of the stations

    outputfile = '{0}/{1}/temperatures/{1}temperature'.format(directory, HUC8)

    if not os.path.isfile(outputfile + '.shp'):

        print('making a shapefile of the temperature stations')

        # copy the projection

        shutil.copy('{0}/{1}/{1}boundaries.prj'.format(directory, HUC8), 
                    outputfile + '.prj')

        # fields for the new file

        fields = [['Station',    'C', 11, 0],
                  ['Name',       'C', 30, 0],
                  ['Elev_m',     'N', 6,  1],
                  ['tmin_avg',   'N', 6,  1], 
                  ['tmax_avg',   'N', 6,  1],
                  ['start_date', 'C', 10, 0],
                  ['end_date',   'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        for station, info in tempstations.items():

            s = info.tmax[0][0]
            e = info.tmax[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)

            tmin = round(sum([t[1] for t in info.tmin]) / len(info.tmin), 1)
            tmax = round(sum([t[1] for t in info.tmax]) / len(info.tmax), 1)

            w.point(info.longitude, info.latitude)
            w.record(station, info.name, info.elevation, tmin, tmax, 
                     start_date, end_date)

        if len(w.records) == 0: 
            print('unable to find stations within watershed, ' +
                  'consider expanding location criteria')
            return

        w.save(outputfile)

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

    # make a shapefile of the stations

    outputfile = '{0}/{1}/snow/{1}snow'.format(directory, HUC8)

    if not os.path.isfile(outputfile + '.shp'):

        print('making a shapefile of the snow stations')

        # copy the projection

        shutil.copy('{0}/{1}/{1}boundaries.prj'.format(directory, HUC8), 
                    outputfile + '.prj')

        # fields for the new file

        fields = [['Station',    'C', 11, 0],
                  ['Name',       'C', 30, 0],
                  ['Elev_m',     'N', 6,  1],
                  ['depth_mm',   'N', 6,  0],
                  ['snow_mm',    'N', 6,  0],
                  ['snow_days',  'N', 6,  0],
                  ['start_date', 'C', 10, 0],
                  ['end_date',   'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        for station, info in snowstations.items():

            s = info.snowdepth[0][0]
            e = info.snowdepth[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)

            # add it to the point shapefile and the local NCDC dictionary

            years = ((info.snowdepth[-1][0] - 
                      info.snowdepth[0][0]).total_seconds() / 
                     86400 / 365.25)
            depth = round(sum([d[1] for d in info.snowdepth]) / 
                          len(info.snowdepth))
            snowf = round(sum([d[1] for d in info.snowfall]) / years)
            days  = round(len([d for d in info.snowdepth if d[1] > 0]) 
                          / years)

            w.point(info.longitude, info.latitude)
            w.record(station, info.name[:30], info.elevation, depth, snowf, 
                     days, start_date, end_date)

        if len(w.records) == 0: 
            print('unable to find stations within watershed, ' +
                  'consider expanding location criteria')
            return

        w.save(outputfile)

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

    # make a shapefile of the evaporation stations

    outputfile = '{0}/{1}/evaporations/{1}evaporation'.format(directory, HUC8)

    if not os.path.isfile(outputfile + '.shp'):

        print('making a shapefile of the evaporation stations')

        # copy the projection

        shutil.copy('{0}/{1}/{1}boundaries.prj'.format(directory, HUC8), 
                    outputfile + '.prj')

        # fields for the new file
        
        fields = [['Station',    'C', 11, 0],
                  ['Name',       'C', 30, 0],
                  ['Elev_m',     'N', 6,  1],
                  ['n_obs',      'N', 6,  0], 
                  ['av_evap_mm', 'N', 6,  2],
                  ['start_date', 'C', 10, 0],
                  ['end_date',   'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        evaporation = {}
        for station, info in evapstations.items():

            s = info.events[0][0]
            e = info.events[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)

            years = [start.year + i for i in range(end.year - start.year)]
            annual = []
            for year in years:
                if year <= info.events[-1][0].year:
                    annual.append(0)
                    for event in info.events:
                        if event[0].year == year: annual[-1] += event[1]

            w.point(info.longitude, info.latitude)

            w.record(station, info.name, info.elevation, len(info.events), 
                     round(sum(annual) / len(annual) / 10, 1), start_date,
                     end_date)

        if len(w.records) == 0: 
            print('warning: unable to find stations within watershed, ' +
                  'consider expanding location criteria')
            return

        w.save(outputfile)

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

    # make a shapefile of the stations

    outputfile = '{0}/{1}/wind/{1}wind'.format(directory, HUC8)

    if not os.path.isfile(outputfile + '.shp'):

        print('making a shapefile of the wind stations')

        # copy the projection

        shutil.copy('{0}/{1}/{1}boundaries.prj'.format(directory, HUC8), 
                    outputfile + '.prj')

        # fields for the new file

        fields = [['Station',    'C', 11,  0],
                  ['Name',       'C', 30, 0],
                  ['Elev_m',     'N', 6,  1],
                  ['speed_m_s',  'N', 6,  1],
                  ['start_date', 'C', 10, 0],
                  ['end_date',   'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        for station, info in windstations.items():

            s = info.wind[0][0]
            e = info.wind[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)
            
            # add it to the point shapefile and the local NCDC dictionary

            years = ((info.wind[-1][0] - info.wind[0][0]).total_seconds() / 
                     86400 / 365.25)
            speed = round(sum([d[1] for d in info.wind]) / 
                          len(info.wind))
            days  = round(len([d for d in info.wind if d[1] >= 0]) / years)

            w.point(info.longitude, info.latitude)
            w.record(station, info.name, info.elevation, speed,
                     start_date, end_date)

        if len(w.records) == 0: 
            print('unable to find stations within watershed, ' +
                  'consider expanding location criteria')
            return

        w.save(outputfile)

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

    # make a shapefile of the stations

    outputfile = '{0}/{1}/dewpoint/{1}dewpoint'.format(directory, HUC8)

    if not os.path.isfile(outputfile + '.shp'):

        print('making a shapefile of the dewpoint stations')

        # copy the projection

        shutil.copy('{0}/{1}/{1}boundaries.prj'.format(directory, HUC8), 
                    outputfile + '.prj')

        # fields for the new file

        fields = [['Station',    'C', 11,  0],
                  ['Name',       'C', 30, 0],
                  ['Elev_m',     'N', 6,  1],
                  ['average',    'N', 6,  1],
                  ['start_date', 'C', 10, 0],
                  ['end_date',   'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        for station, info in dewstations.items():

            s = info.dewpoint[0][0]
            e = info.dewpoint[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)

            # add it to the point shapefile and the local NCDC dictionary

            years = ((info.dewpoint[-1][0] - 
                      info.dewpoint[0][0]).total_seconds() / 
                     86400 / 365.25)
            avg   = round(sum([d[1] for d in info.dewpoint]) / 
                          len(info.dewpoint))
            days  = round(len([d for d in info.dewpoint if d[1] >= 0]) / years)

            w.point(info.longitude, info.latitude)
            w.record(station, info.name, info.elevation, avg,
                     start_date, end_date)

        if len(w.records) == 0: 
            print('unable to find stations within watershed, ' +
                  'consider expanding location criteria')
            return

        w.save(outputfile)

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

    # make a shapefile of the stations

    outputfile = '{0}/{1}/solar/{1}solar'.format(directory, HUC8)

    if not os.path.isfile(outputfile + '.shp'):

        print('making a shapefile of the solar radiation stations')

        # copy the projection

        shutil.copy('{0}/{1}/{1}boundaries.prj'.format(directory, HUC8), 
                    outputfile + '.prj')

        # fields for the new file

        fields = [['Station',    'C', 11, 0],
                  ['Name',       'C', 30, 0],
                  ['Elev_m',     'N', 6,  1],
                  ['solar_avg',  'N', 6,  1],
                  ['start_date', 'C', 10, 0],
                  ['end_date',   'C', 10, 0]]

        # make the shapefile

        w = Writer(shapeType = 1)

        for field in fields: w.field(*field)

        for station, info in solarstations.items():

            s = info.solar[0][0]
            e = info.solar[-1][0]

            start_date = '{:04d}-{:02d}-{:02d}'.format(s.year, s.month, s.day)
            end_date   = '{:04d}-{:02d}-{:02d}'.format(e.year, e.month, e.day)

            solar = round(sum([s[1] for s in info.solar]) * 24 / 1000 /  
                          len(info.solar), 1)

            w.point(info.longitude, info.latitude)
            w.record(station, info.name, info.elevation, solar, 
                     start_date, end_date)

        if len(w.records) == 0: 
            print('unable to find stations within watershed, ' +
                  'consider expanding location criteria')
            return

        w.save(outputfile)

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
