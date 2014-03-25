#!/usr/bin/env python3
#
# File: extract_NCDC.py
#
# by David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Last updated: 11/16/2013
#
# Purpose: imports climate data files to Python classes

import os, pickle, io, datetime, shutil, tarfile, subprocess

from urllib    import request
from shapefile import Reader, Writer

from pyhspf.preprocessing.ncdcstations import NSRDBStation
from pyhspf.preprocessing.ncdcstations import GSODStation
from pyhspf.preprocessing.ncdcstations import GHCNDStation
from pyhspf.preprocessing.ncdcstations import Precip3240Station

def is_number(s):
    """Tests if string "s" is a number."""
    try: float(s) 
    except ValueError: return False
    return True

def is_integer(s):
    """Tests if string "s" is an integer."""
    try: int(s) 
    except ValueError: return False
    return True

def inside_box(p1, p2, p3, space = 0):
    """Checks if p3 is inside a box formed by p1 and p2."""

    if p1[0] < p3[0] and p3[0] < p2[0] or p1[0] > p3[0] and p3[0] > p2[0]:

        # x value is inside

        if p1[1] < p3[1] and p3[1] < p2[1] or p1[1] > p3[1] and p3[1] > p2[1]:
            
            # y value is inside

            return True

        else: return False

def decompress7z(filename, directory,
                 path_to_7z = r'C:/Program Files/7-Zip/7z.exe'):
    """Decompresses a Unix-compressed archive on Windows using 7zip."""

    c = '{0} e {1} -y -o{2}'.format(path_to_7z, filename, directory)

    subprocess.call(c)

def decompresszcat(filename, directory):
    """Decompresses a Unix-compressed archive on Windows using 7zip."""

    with subprocess.Popen(['zcat', filename], 
                          stdout = subprocess.PIPE).stdout as s:

        with open(filename[:-2], 'wb') as f: f.write(s.read())

def get_boundaries(shapes, space = 0.1):
    """Gets the boundaries for the plot."""

    boundaries = shapes[0].bbox
    for shape in shapes[0:]:
        b = shape.bbox
        if b[0] < boundaries[0]: boundaries[0] = b[0]
        if b[1] < boundaries[1]: boundaries[1] = b[1]
        if b[2] > boundaries[2]: boundaries[2] = b[2]
        if b[3] > boundaries[3]: boundaries[3] = b[3]

    xmin = boundaries[0] - (boundaries[2] - boundaries[0]) * space
    ymin = boundaries[1] - (boundaries[3] - boundaries[1]) * space
    xmax = boundaries[2] + (boundaries[2] - boundaries[0]) * space
    ymax = boundaries[3] + (boundaries[3] - boundaries[1]) * space

    return xmin, ymin, xmax, ymax

def find_ghcnd(bbox, GHCND, dates = None, var = None, types = 'all', 
               verbose = False):
    """Finds stations meeting the requirements from the Global Historical 
    Climate Network Daily online database."""

    xmin, ymin, xmax, ymax = bbox
    
    stations = []
    if var is None: 

        if verbose: print('looking for GHCND stations\n')

        filename = 'ghcnd-stations.txt'

        req = request.Request('{}/{}'.format(GHCND, filename))

        try:
        
            # read the data from the server

            with io.StringIO(request.urlopen(req).read().decode('utf-8')) as s:

                # iterate through the rows and split into columns

                data = [[r[:11], r[12:20], r[22:30], r[31:37], r[38:40], 
                         r[41:71], r[72:75], r[76:79], r[80:85]] 
                        for r in s.read().split('\n') if len(r.strip()) > 0]
                
        except: 
            print('unable to connect to the GHCND database')
            print('make sure that you are online')
            raise

        # parse through the GSN and HCN stations and see if they're in the box

        for station, lat, lon, elev, y, name, gsn, hcn, c in data:
                    
            if inside_box([xmin, ymin], [xmax, ymax], [float(lon), float(lat)]):

                if (types == 'all' or
                    (gsn == 'GSN' and (types == 'both' or types == 'GSN')) or
                    (hcn == 'HCN' and (types == 'both' or types == 'GSN'))):

                    stations.append(GHCNDStation(station, 
                                                 name.strip(), 
                                                 float(lat),
                                                 float(lon), 
                                                 float(elev), 
                                                 dtype = types
                                                 )
                                    )

                    if verbose: print('found GSN station ' +
                                      '{}, {}'.format(station, name.strip()))

    else:

        if verbose: 
            print('looking for GHCND stations with {} data'.format(var))
            print('if your internet connection is slow this may take a while\n')

        filename = 'ghcnd-inventory.txt'

        req = request.Request('{}/{}'.format(GHCND, filename))

        try:
        
            # read the data from the server

            with io.StringIO(request.urlopen(req).read().decode('utf-8')) as s:

                # iterate through the rows and split into columns

                data = [[r[:11], r[12:20], r[22:30], r[31:35], r[36:40], 
                         r[41:45]] for r in s.read().split('\n') 
                         if len(r.strip()) > 0]

        except: 
            print('warning: unable to connect to the GHCND database')
            print('make sure that you are online')
            raise

        # parse through the GHCND stations and see if they're in the box and
        # have the data

        if dates is None: start, end = 1980, 2010
        else:             start, end = dates

        names = []
        for station, lat, lon, parm, yr1, yr2 in data:
                    
            if inside_box([xmin, ymin], [xmax, ymax], [float(lon), float(lat)]):

                if var == parm and is_integer(yr1) and is_integer(yr2):

                    if int(yr1) < end.year and start.year < int(yr2):

                        if verbose: print('found {} station {}'.format(var,
                                                                       station))
                        names.append(station)

        filename = 'ghcnd-stations.txt'

        req = request.Request('{}/{}'.format(GHCND, filename))

        try:
        
            # read the data from the server

            with io.StringIO(request.urlopen(req).read().decode('utf-8')) as s:

                # iterate through the rows and split into columns

                data = [[r[:11], r[12:20], r[22:30], r[31:37], r[38:40], 
                         r[41:71], r[72:75], r[76:79], r[80:85]] 
                        for r in s.read().split('\n') if len(r.strip()) > 0]
                
        except: 
            print('unable to connect to the GHCND database')
            print('make sure that you are online')
            raise

        # parse through the stations and find those with the needed variable

        for station, lat, lon, elev, y, name, gsn, hcn, c in data:
                    
            if station in names:
                    stations.append(GHCNDStation(station, 
                                                 name.strip(), 
                                                 float(lat),
                                                 float(lon), 
                                                 float(elev), 
                                                 dtype = types
                                                 )
                                    )

                    if verbose: print('found GHCND station ' +
                                      '{}, {}'.format(station, name.strip()))

    if len(stations) == 0:
        print('\nwarning: unable to locate stations within the region')

    return stations

def extract_ghcnd(directory, HUC8, start, end, types = 'all', space = 0.1,
                  GHCND = 'http://www1.ncdc.noaa.gov/pub/data/ghcn/daily',
                  verbose = True):
    """Extracts data from GHCND Stations."""

    # make a folder for the files

    d = '{}/{}/GHCND'.format(directory, HUC8)
    if not os.path.isdir(d): 
        os.mkdir(d)
        if verbose: print('downloading data from GHCND\n')

    # find the bounding box

    boundaryfile = '{0}/{1}/{1}boundaries'.format(directory, HUC8)

    boundaryreader = Reader(boundaryfile)
    bbox = get_boundaries(boundaryreader.shapes(), space = space)
    
    # find stations in the bounding box

    stations = find_ghcnd(bbox, GHCND, types = types, dates = (start, end))

    # iterate through the stations and download the data

    for station in stations: station.download_data(d, start = start, end = end)

    # check to see that the data needed are there

    stations = []
    for f in os.listdir(d):
        if f[-3:] != 'png':
            with open('{}/{}'.format(d, f), 'rb') as g:
                station = pickle.load(g)
            stations.append(station)

    if space < 0.5: space = 0.5
    while all([len([d for d, e in s.evap if 0 <= e and start <= d and 
                    d <= end]) == 0 for s in stations]):
        print('no evaporation records available, looking for evaporation data')
        bbox = get_boundaries(boundaryreader.shapes(), space = space)
        print('bounding box: {:.2f}, {:.2f}, {:.2f}, {:.2f}'.format(*bbox))
        extrastations = find_ghcnd(bbox, GHCND, var = 'EVAP', 
                                   dates = (start, end), verbose = verbose)

        for s in extrastations: 
            s.download_data(d, start = start, end = end)
            if len(s.evap) > 0:
                stations.append(s)

        space += 0.5

def find_gsod(bbox, GSOD, filename, dates = None, verbose = True, 
              vverbose = False):
    """finds Global Surface Observation Data Stations inside the given 
    bounding box. Optional keyword arguments can be used to find stations
    with data with the desired period of record."""

    xmin, ymin, xmax, ymax = bbox

    req = request.Request('{}/{}'.format(GSOD, filename))

    try:
        
        # read the data from the server

        with io.StringIO(request.urlopen(req).read().decode('utf-8')) as s:

            # parse it

            data = [r for r in s.read().split('\n') if len(r) > 0]

        data = [[d[:6], d[7:12], d[13:43], d[43:48], d[49:51], d[52:56], 
                 d[57:64], d[65:72], d[73:79], d[83:91], d[92:100]]
                 for d in data if is_integer(d[0][:6])]

    except:
        print('warning: unable to locate stations within the region')
        print('verify that you have internet access')
        raise Exception

    # parse through the GSOD stations and see if they're inside the box

    stations = []
    for usaf, wban, station, ctry, st, call, lat, lon, elev, s, e in data:

        if is_number(lon) and is_number(lat):
            if inside_box([xmin, ymin], [xmax, ymax], 
                          [float(lon) / 1000, float(lat) / 1000]):

                if is_integer(s):
                    d1 = datetime.datetime(int(s[:4]),
                                           int(s[4:6]),
                                           int(s[6:8]))
                else: d1 = None
                if is_integer(e):
                    d2 = datetime.datetime(int(e[:4]),
                                           int(e[4:6]),
                                           int(e[6:8]))
                else: d2 = None

                if dates is None:
                    stations.append(GSODStation(int(usaf), 
                                                int(wban), 
                                                station.strip(), 
                                                float(lat) / 1000, 
                                                float(lon) / 1000, 
                                                float(elev) / 10,
                                                d1,
                                                d2
                                                )
                                    )
                    if vverbose: 
                        print('found GSOD station ' +
                              '{}, {}, "{}"'.format(usaf,wban,station.strip())) 

                else:
                    
                    # include only stations with data over period of record

                    if d1 is not None and d2 is not None:
                        if d1 <= dates[0] and dates[1] <= d2:
                            v = usaf, wban, station.strip(), d1.year, d2.year
                            if verbose:
                                print('found GSOD station ' +
                                      '{}, {}, "{}" {} to {}'.format(*v))
                                stations.append(GSODStation(int(usaf), 
                                                            int(wban), 
                                                            station.strip(), 
                                                            float(lat) / 1000, 
                                                            float(lon) / 1000, 
                                                            float(elev) / 10,
                                                            d1,
                                                            d2
                                                            )
                                                )

    if len(stations) == 0:
        print('\nwarning: unable to locate stations within the region')

    return stations

def extract_gsod(directory, HUC8, start, end, space = 0.1,
                 GSOD = 'ftp://ftp.ncdc.noaa.gov/pub/data/inventories',
                 filename = 'ISH-HISTORY.TXT', verbose = False):

    # make a folder for the files

    d = '{}/{}/GSOD'.format(directory, HUC8)
    if not os.path.isdir(d): os.mkdir(d)

    # open up the bounding box for the watershed

    boundaryfile = '{0}/{1}/{1}boundaries'.format(directory, HUC8)

    boundaryreader = Reader(boundaryfile)
    bbox = get_boundaries(boundaryreader.shapes(), space = space)

    # find the stations in the bounding box

    stations = find_gsod(bbox, GSOD, filename)

    years = [s.start.year for s in stations if s.start is not None]

    # if more data are needed, expand the bounding box

    if space <= 0.5: space = 0.5
    while all([start.year < y for y in years]): 
        print('GSOD files in the watershed do not contain sufficient data')
        print('looking for other stations')

        bbox = get_boundaries(boundaryreader.shapes(), space = space)
        stations += find_gsod(bbox, GSOD, filename, dates = (start, end))
        years = [s.start.year for s in stations if s.start is not None]
        space += 0.2
    
    # iterate through the stations and download the data

    for station in stations: station.download_data(d, start = start, end = end)

    # combine together stations in different files (why did they do this???)

    combined = []
    for station in stations:
        var = d, station.airforce, station.wban
        source = '{0}/{1:06d}-{2:05d}'.format(*var)
        
        if station.airforce == 999999:
            var = d, station.wban
            destination = '{0}/{1:05d}'.format(*var)
        else:
            var = d, station.airforce
            destination = '{0}/{1:06d}'.format(*var)

        if os.path.isfile(destination):
            with open(destination, 'rb') as f: existing = pickle.load(f)
            with open(source, 'rb') as f:      adding   = pickle.load(f)
            existing.add_data(adding)
            with open(destination, 'wb') as f: pickle.dump(existing, f)
        elif os.path.isfile(source): 
            shutil.copy(source, destination)
            combined.append(destination)

    # print the output

    for p in combined:
        with open(p, 'rb') as f: station = pickle.load(f)
        station.plot(start, end, output = p)

def find_prec3240(bbox, NCDC = 'ftp://ftp.ncdc.noaa.gov/pub/data', 
                  metafile = 'inventories/COOP-ACT.TXT',
                  dates = None, verbose = True):
    """Finds stations meeting the requirements from the hourly precipitation
    online NCDC database."""

    # state codes

    statecodes = {
        'AL': '01',
        'AZ': '02',
        'AR': '03',
        'CA': '04',
        'CO': '05',
        'CT': '06',
        'DE': '07',
        'FL': '08',
        'GA': '09',
        'ID': '10',
        'IL': '11',
        'IN': '12',
        'IA': '13',
        'KS': '14',
        'KY': '15',
        'LA': '16',
        'ME': '17',
        'MD': '18',
        'MA': '19',
        'MI': '20',
        'MN': '21',
        'MS': '22',
        'MO': '23',
        'MT': '24',
        'NE': '25',
        'NV': '26',
        'NH': '27',
        'NJ': '28',
        'NM': '29',
        'NY': '30',
        'NC': '31',
        'ND': '32',
        'OH': '33',
        'OK': '34',
        'OR': '35',
        'PA': '36',
        'RI': '37',
        'SC': '38',
        'SD': '39',
        'TN': '40',
        'TX': '41',
        'UT': '42',
        'VT': '43',
        'VA': '44',
        'WA': '45',
        'WV': '46',
        'WI': '47',
        'WY': '48',
        'AK': '50',
        'HI': '51',
        'PR': '66',
        'VI': '67'
        }
        
    # open the bounding box for the watershed
                          
    xmin, ymin, xmax, ymax = bbox

    # make a list of all coop stations

    stations = []

    # web location

    req = request.Request('{}/{}'.format(NCDC, metafile))

    try:

        # read the data from the server into memory

        with io.StringIO(request.urlopen(req).read().decode()) as s:

            # split it row by row according the column designations

            data = [(l[:6], l[7:9], l[10:15], l[16:21], l[22:26], l[27:32], 
                     l[33:37], l[38:58], l[59:61], l[62:92], l[93:98], 
                     l[99:130], l[131:133], l[134:136], l[137:139], l[140:144],
                     l[145:147], l[148:150], l[151:156]) 
                    for l in s.read().split('\n')][1:]

    except: 

        print('unable to connect to the hourly precipitation database')
        print('make sure that you are online')
        raise

    for (coop, n, wban, wmo, faa, nws, icao, country, st, 
         county, tz, desc, la1, la2, la3, lo1, lo2, lo3, elev) in data:

        # convert to lat/lon

        if (is_number(la1) and is_number(la2) and is_number(la3) and 
            is_number(lo1) and is_number(lo2) and is_number(lo3)):

            if float(la1) > 0:
                lat = float(la1) + float(la2) / 60 + float(la3) / 3600
            else:
                lat = float(la1) - float(la2) / 60 - float(la3) / 3600

            if float(lo1) > 0:
                lon = float(lo1) + float(lo2) / 60 + float(lo3) / 3600
            else:
                lon = float(lo1) - float(lo2) / 60 - float(lo3) / 3600

            # parse through the stations and see if they're in the box

            if (inside_box([xmin, ymin], [xmax, ymax], 
                           [float(lon), float(lat)])):

                stations.append(Precip3240Station(coop,
                                                  wban,
                                                  wmo,
                                                  lat,
                                                  lon, 
                                                  0.3048 * float(elev), 
                                                  st,
                                                  statecodes[st],
                                                  desc.strip()
                                                  )
                                )

                if verbose: print('found hourly precipitation station ' +
                                  '{}'.format(desc))

    return stations

def extract_precip3240(directory, HUC8, start, end, 
                       NCDC = 'ftp://ftp.ncdc.noaa.gov/pub/data',
                       clean = False, space = 0.2, verbose = True):
    """Makes a point shapefile of the stations from a csv file of hourly 
    precipitation data from NCDC within the bounding box of the watershed."""

    if os.name == 'nt': decompress = decompress7z
    else:               decompress = decompresszcat

    d = '{}/{}/precip3240'.format(directory, HUC8)
    if not os.path.isdir(d): os.mkdir(d)

    # open up the bounding box for the watershed

    boundaryfile = '{0}/{1}/{1}boundaries'.format(directory, HUC8)

    boundaryreader = Reader(boundaryfile)

    bbox = get_boundaries(boundaryreader.shapes(), space = space)

    # find the precipitation stations in the bounding box

    stations = find_prec3240(bbox, verbose = verbose)

    if verbose: print('')

    # make a list of all the states since that's how the NCDC data are stored

    states = list(set([s.code for s in stations]))

    # download the state data for each year
    
    for state in states:

        if verbose: print('downloading hourly precipitation data for state ' +
                          '{}\n'.format(state))

        # figure out which files are on the website

        baseurl = '{0}/hourly_precip-3240/{1}'.format(NCDC, state)

        req = request.Request(baseurl)

        # read the state's web page and find all the compressed archives

        with io.StringIO(request.urlopen(req).read().decode()) as s:

            archives = [a[-17:] for a in s.read().split('.tar.Z')]
            
        archives = [a for a in archives if is_integer(a[-4:])]

        #except: 
        #else:
        #    print('unable to connect to the hourly precipitation database')
        #    print('make sure that you are online')
        #    raise
                
        # download the state's archives for all the years

        for a in archives:

            url        = '{0}/{1}.tar.Z'.format(baseurl, a)
            compressed = '{}/{}.tar.Z'.format(d, a)

            if not os.path.isfile(compressed):

                if verbose: print(url)

                req = request.Request(url)

                # read the data from the server into memory

                    # write the compressed archive into the directory

                with open(compressed, 'wb') as f: 
                    f.write(request.urlopen(req).read())

                #except:

                #    print('unable to connect to the hourly precipitation ' +
                #          'database\nmake sure that you are online')
                #    raise

            # decompress the archive

            if not os.path.isfile(compressed[:-6]): 
                decompress(compressed, d)
                if verbose: print('')
            
    # import the data

    for station in stations: station.import_data(d, start, end)

def find_nsrdb(bbox, NSRDB, metafile, dates = None, 
               NCDC = 'ftp://ftp.ncdc.noaa.gov/pub/data',
               verbose = True):
    """finds National Solar Radiation Database Stations inside the given 
    bounding box. Optional keyword arguments can be used to find stations
    with data with the desired period of record."""

    xmin, ymin, xmax, ymax = bbox

    # check to see if need the old database and the new

    if dates is None: old = False
    else:
        old = dates[0].year  < 1991
        new = dates[1].year >= 1991

    if old:

        # use this summary table that's zipped to figure out the station #s 
        # would it be so much to ask they made a f***ing table of this?

        url = '{}/old_data/nsrdb/1961-1990/hourly/compressed'.format(NSRDB)

        req = request.Request(url)
  
        try:

            # make a list of the old stations available in the database

            with io.StringIO(request.urlopen(req).read().decode()) as s:

                oldstations = [n[-5:] for n in s.read().split('.tar.gz')]
                            
        except:

            print('warning: unable to open the WBAN metadata')
            print('make sure you are online')

        # make a dictionary of the station numbers to identify the old data
        # for some brilliant reason each station has two identifiers, and 
        # pre 1991 uses one while post 1991 uses the other)

        source = '{}/inventories/WBAN-MSC.TXT'.format(NCDC)
    
        req = request.Request(source)

        try:

            # read the data from the server

            with io.StringIO(request.urlopen(req).read().decode()) as s:

                # parse it

                metadata = [[l[:6], l[8:13], l[15:]] 
                            for l in s.read().split('\n')
                            if is_integer(l[:6]) and is_integer(l[8:13])]

            wbans = {usaf:wban for usaf, wban, desc in metadata 
                     if wban in oldstations}
            
        except:

            print('warning: unable to open the WBAN metadata')
            print('make sure you are online')

    else: wbans = {}

    # find the solar metadata

    req = request.Request('{}/{}'.format(NSRDB, metafile))

    try:

        # read the data from the server

        with io.StringIO(request.urlopen(req).read().decode()) as s:

            # parse it

            metadata = [l.split(',') for l in s.read().split('\r')
                        if len(l.split(',')) == 12]

    except:
        print('warning: unable to locate stations within the region')
        print('verify that you have internet access\n')
        raise Exception

    # parse through the GSOD stations and see if they're inside the box

    stations = []
    for line in metadata:

        usaf, cl, flag, station, st, lat, lon, elev, tz, l, l, l = line

        if (is_integer(usaf) and is_integer(flag) and is_integer(cl) and
            is_number(lat) and is_number(lon)):

            if inside_box([xmin, ymin], [xmax, ymax], 
                          [float(lon), float(lat)]):

                if usaf in wbans: wban = wbans[usaf]
                else:                    wban = None

                stations.append(NSRDBStation(usaf,
                                             wban,
                                             int(cl), 
                                             int(flag), 
                                             station, 
                                             float(lat), 
                                             float(lon), 
                                             float(elev)
                                             )
                                )

                if verbose: 
                    print('found NSRDB station ' +
                          '{}, {}'.format(usaf, station.strip())) 

    if old and all([s.wban is None for s in stations]): stations = []

    if len(stations) == 0:
        print('\nwarning: unable to locate stations for the entire ' +
              'period of interest\n')

    return stations

def extract_nsrdb(directory, HUC8, start, end, space = 0.1,
                  NSRDB = 'http://rredc.nrel.gov/solar',
                  metafile = 'old_data/nsrdb/1991-2010/NSRDB_StationsMeta.csv',
                  plot = True, verbose = True, vverbose = False):
    """Makes pickled instances of the GageStation class for all the gages
    meeting the calibration criteria for an 8-digit watershed."""

    if verbose: print('\nextracting solar radiation data from NREL\n')

    # paths for the watershed shapefiles

    boundaryfile = '{0}/{1}/{1}boundaries'.format(directory, HUC8)
    solarfile    = '{0}/{1}/{1}solarstations'.format(directory, HUC8)

    # make a folder for the files

    d = '{0}/{1}/NSRDB'.format(directory, HUC8)
    if not os.path.isdir(d): os.mkdir(d)

    boundaryreader = Reader(boundaryfile)

    stations = []
    while len(stations) == 0:
        
        bbox = get_boundaries(boundaryreader.shapes(), space = space)
        stations = find_nsrdb(bbox, NSRDB, metafile, dates = (start, end))
        space += 0.2

    # download the data

    print('')
    for station in stations: 
        
        if not os.path.isfile('{}/{}'.format(d, station.usaf)):

            station.download_data(d, dates = (start, end))

    # plot it up

    from pyhspf.preprocessing.climateplots import plot_nsrdb

    for station in stations: 

        p = '{}/{}'.format(d, station.usaf)
        if not os.path.isfile(p + '.png'):

            with open(p, 'rb') as f: s = pickle.load(f) 

            try: plot_nsrdb(s, start, end, output = p)
            except: print('unable to plot', s.station)   

def download_climate(directory, HUC8, start, end):
    """Downloads select climate data from GHCND, GSOD, NSRDB, and NCDC 3240."""
    
    # download the GHCND data

    if not os.path.isdir('{}/{}/GHCND'.format(directory, HUC8)):
        extract_ghcnd(directory, HUC8, start, end)

    # download the GSOD data

    if not os.path.isdir('{}/{}/GSOD'.format(directory, HUC8)):
        extract_gsod(directory, HUC8, start, end)

    # download the hourly precipitation data from NCDC

    if not os.path.isdir('{}/{}/precip3240'.format(directory, HUC8)):
        extract_precip3240(directory, HUC8, start, end)

    # download the NSRDB data

    if not os.path.isdir('{}/{}/NSRDB'.format(directory, HUC8)):
        extract_nsrdb(directory, HUC8, start, end)

