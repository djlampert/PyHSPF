# climateutils.py
#
# David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Last updated: 06/12/2014
#
# Purpose: contains a variety of utility functions to download and import
# climate data files to Python classes 

import os, io, datetime

from urllib import request

from .ncdcstations import NSRDBStation
from .ncdcstations import GSODStation
from .ncdcstations import GHCNDStation
from .ncdcstations import Precip3240Station

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

def inside_box(p1, 
               p2, 
               p3, 
               space = 0,
               ):
    """Checks if p3 is inside a box formed by p1 and p2."""

    if p1[0] < p3[0] and p3[0] < p2[0] or p1[0] > p3[0] and p3[0] > p2[0]:

        # x value is inside

        if p1[1] < p3[1] and p3[1] < p2[1] or p1[1] > p3[1] and p3[1] > p2[1]:
            
            # y value is inside

            return True

        else: return False

def find_ghcnd(bbox, 
               GHCND = 'http://www1.ncdc.noaa.gov/pub/data/ghcn/daily', 
               dates = None, 
               var   = None, 
               types = 'all', 
               verbose = True,
               ):
    """Finds stations meeting the requirements from the Global Historical 
    Climate Network Daily online database."""

    xmin, ymin, xmax, ymax = bbox
    
    stations = []
    if var is None: 

        if verbose: 

            print('looking for GHCND stations in ' +
                  '{:.4f}, {:.4f}, {:.4f}, {:.4f}...\n'.format(*bbox))

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

            print('unable to connect to the GHCND database\n' +
                  'make sure that you are online')
            raise

        # parse through the GSN and HCN stations and see if they're in the box

        for station, lat, lon, elev, y, name, gsn, hcn, c in data:
                    
            if inside_box([xmin, ymin], [xmax, ymax], [float(lon), float(lat)]):

                if (types == 'all' or
                    (gsn == 'GSN' and (types == 'both' or types == 'GSN')) or
                    (hcn == 'HCN' and (types == 'both' or types == 'GSN'))
                    ):

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

                        if verbose: 

                            print('found {} station {}'.format(var, station))

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
                                                 dtype = types,
                                                 )
                                    )

                    if verbose: print('found GHCND station ' +
                                      '{}, {}'.format(station, name.strip()))

    if len(stations) == 0:
        print('\nwarning: unable to locate stations within the region')

    return stations

def find_gsod(bbox, 
              GSOD = 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa',
              filename = 'isd-history.txt', 
              dates = None, 
              verbose = True, 
              vverbose = False,
              ):
    """finds Global Surface Observation Data Stations inside the given 
    bounding box. Optional keyword arguments can be used to find stations
    with data with the desired period of record."""

    if verbose:
 
        i = bbox
        print('\nsearching for GSOD stations in ' +
              '{:.4f}, {:.4f}, {:.4f}, {:.4f}...'.format(*i))

    xmin, ymin, xmax, ymax = bbox

    req = request.Request('{}/{}'.format(GSOD, filename))

    try:
        
        # read the data from the server

        with io.StringIO(request.urlopen(req).read().decode('utf-8')) as s:

            # parse it

            data = [r for r in s.read().split('\n') if len(r) > 0]

        data = [[d[:6], d[7:12], d[13:43], d[43:48], d[49:51], d[51:55], 
                 d[57:64], d[65:72], d[73:81], d[82:90], d[91:99]]
                 for d in data if is_integer(d[0][:6])]

    except:

        print('warning: unable to locate stations within the region')
        print('verify that you have internet access')
        raise Exception

    # parse through the GSOD stations and see if they're inside the box

    stations = []
    for usaf, wban, station, ctry, st, call, lat, lon, elev, s, e in data:

        if is_number(lon) and is_number(lat):

            if (inside_box([xmin,ymin], [xmax,ymax], [float(lon), float(lat)])
                and int(usaf) != 999999):

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
                                                float(lat), 
                                                float(lon), 
                                                float(elev),
                                                d1,
                                                d2,
                                                )
                                    )
                    if vverbose: 
                        print('found GSOD station ' +
                              '{}, {}, "{}"'.format(usaf,wban,station.strip())) 

                elif d1 is not None and d2 is not None:
                    
                    # include only stations with data over period of record

                    if d1 <= dates[0] and dates[1] <= d2:
                        v = usaf, wban, station.strip(), d1.year, d2.year
                        if verbose:
                            print('found GSOD station ' +
                                  '{}, {}, "{}" {} to {}'.format(*v))
                            stations.append(GSODStation(int(usaf), 
                                                        int(wban), 
                                                        station.strip(), 
                                                        float(lat), 
                                                        float(lon), 
                                                        float(elev),
                                                        d1,
                                                        d2,
                                                    )
                                            )

    if verbose: print('')

    if len(stations) == 0:
        print('\nwarning: unable to locate stations within the region')

    return stations

def find_precip3240(bbox, 
                    NCDC = 'http://www.ncdc.noaa.gov/', 
                    metafile = 'homr/file/coop-stations.txt',
                    dates = None, 
                    verbose = True,
                    ):
    """Finds stations meeting the requirements from the hourly precipitation
    online NCDC database."""

    if verbose: 

        print('\nsearching for hourly precipitation stations within ' +
              '{:.4f}, {:.4f}, {:.4f}, {:.4f}...\n'.format(*bbox))

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

            data = [(l[:8], l[9:15], l[16:21], l[25:36], l[41:71],
                     l[72:92], l[93:95], l[96:146], l[147:149], 
                     l[150:190], l[191:200], l[207:216], l[223:229], 
                     l[234:240], l[243:247], l[250:253], l[254:264],
                     l[265:268], l[269], l[271])
                    for l in s.read().split('\n')[:-1]]

    except: 

        print('unable to connect to the hourly precipitation database')
        print('make sure that you are online')
        raise

    for (wban, coop, n, ghcnd, desc, country, st, county, n, region, 
         la, lo, units, elev, eunits, tzone, region, le, f1, f2) in data:

        # check if the lat/lon are given

        if (is_number(la) and is_number(lo)):

            lat = float(la)
            lon = float(lo)

            # parse through the stations and see if they're in the box

            if (inside_box([xmin, ymin], [xmax, ymax], 
                           [float(lon), float(lat)])):

                if is_number(elev): el = float(elev)
                else:               el = None

                stations.append(Precip3240Station(coop.strip(),
                                                  wban.strip(),
                                                  desc.strip(),
                                                  lat,
                                                  lon, 
                                                  el, 
                                                  st,
                                                  statecodes[st],
                                                  )
                                )

                if verbose: print('found hourly precipitation station ' +
                                  '{}, coop {}'.format(desc.strip(), coop))

    if verbose: print('')

    return stations

def find_nsrdb(bbox, 
               NSRDB    = 'http://rredc.nrel.gov/solar',
               metafile = 'old_data/nsrdb/1991-2010/NSRDB_StationsMeta.csv',
               NCDC     = 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa',
               NCDCmeta = 'isd-history.txt',
               dates = None, 
               verbose = True
               ):
    """finds National Solar Radiation Database Stations inside the given 
    bounding box. Optional keyword arguments can be used to find stations
    with data with the desired period of record."""

    if verbose: 

        i = bbox
        print('searching for NSRDB stations in ' +
              '{:.4f}, {:.4f}, {:.4f}, {:.4f}...\n'.format(*i))

    xmin, ymin, xmax, ymax = bbox

    # check to see if need the old database and the new

    if dates is None: old = False
    else:
        old = dates[0].year  < 1991
        new = dates[1].year >= 1991

    if old:

        # use this summary table that's zipped to figure out the station #s 
        # would it be so much to ask they made a table for this???

        url = '{}/old_data/nsrdb/1961-1990/hourly/compressed'.format(NSRDB)

        req = request.Request(url)
  
        try:

            # make a list of the old stations available in the database

            with io.StringIO(request.urlopen(req).read().decode()) as s:

                oldstations = [n[-5:] for n in s.read().split('.tar.gz')]
                            
        except:

            print('warning: unable to open the WBAN metadata')
            print('make sure you are online and that the pyhspf url is right')
            raise

        # make a dictionary of the station numbers to identify the old data;
        # for some brilliant reason each station has two identifiers, and 
        # pre 1991 uses one while post 1991 uses the other)

        source = '{}/{}'.format(NCDC, NCDCmeta)

        req = request.Request(source)

        try:

            # read the data from the server

            with io.StringIO(request.urlopen(req).read().decode()) as s:

                # parse it

                metadata = [(l[:6], l[7:12])
                            for l in s.read().split('\n')
                            if is_integer(l[:6]) and is_integer(l[8:13])]

            wbans = {usaf:wban for usaf, wban in metadata 
                     if wban in oldstations}

        except:

            print('warning: unable to open the WBAN metadata')
            print('make sure you are online and the pyhspf url is correct')
            raise

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

    # parse through the NSRDB stations and see if they're inside the box

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

    elif verbose: print('')

    return stations
