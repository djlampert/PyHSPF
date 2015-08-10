# climateprocessor.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 02/15/2015
#
# Purpose: contains the ClimateProcessor class to import climate data files 
# and generate time series for hydrologic modeling.

import os, pickle, shutil, datetime, numpy

from shapefile import Reader

from .ncdcstations import ClimateMetadata
from .climateutils import find_ghcnd
from .climateutils import find_gsod
from .climateutils import find_precip3240
from .climateutils import find_nsrdb

class ClimateProcessor:
    """
    A class to download climate data from a variety of publically-available
    datasets from the World Wide Web and then process the data into timeseries
    that can be used for hydrologic modeling.
    """

    def __init__(self,
                 output     = None,
                 GHCND      = None,
                 GSOD       = None,
                 precip3240 = None,
                 NSRDB      = None,
                 path_to_7z = r'C:/Program Files/7-Zip/7z.exe',
                 verbose    = True,
                 ):
        """Set up pointers to the data files."""

        self.output     = output
        self.GHCND      = GHCND
        self.GSOD       = GSOD
        self.precip3240 = precip3240
        self.NSRDB      = NSRDB
        self.verbose    = verbose

        # the path to 7zip to decompress the archives

        self.path_to_7z = path_to_7z
        
        # file metadata for all the different stations

        self.metadata = ClimateMetadata()

    def is_number(self, n):
        """tests if "n" is a number."""

        try:
            float(n)
            return True
        except:
            return False

    def get_boundaries(self, 
                       bbox = None,
                       shapefile = None, 
                       space = 0,
                       ):
        """Gets the boundaries for the plot."""

        if   bbox is not None: boundaries = [x for x in bbox]
        elif shapefile is not None: 
            r = Reader(shapefile)
            boundaries = [b for b in r.bbox]
        else:
            print('error: no information provided')
            raise

        xmin = boundaries[0] - (boundaries[2] - boundaries[0]) * space
        ymin = boundaries[1] - (boundaries[3] - boundaries[1]) * space
        xmax = boundaries[2] + (boundaries[2] - boundaries[0]) * space
        ymax = boundaries[3] + (boundaries[3] - boundaries[1]) * space

        return xmin, ymin, xmax, ymax
 
    def extract_ghcnd(self,
                      bbox,
                      start, 
                      end, 
                      types = 'both', 
                      space = 0.1,
                      GHCND = 'http://www1.ncdc.noaa.gov/pub/data/ghcn/daily',
                      plot = True,
                      verbose = True,
                      ):
        """Extracts data from GHCND Stations."""
    
        # find stations in the bounding box

        stations = find_ghcnd(bbox, types = types, dates = (start, end))

        # iterate through the stations and download the data

        for s in stations: 

            filename = '{}/{}'.format(self.GHCND, s.station)

            if not os.path.isfile(filename):

                s.download_data(self.GHCND, start = start, end = end, 
                                plot = plot)
            
                # save the metadata for the station

                self.metadata.add_ghcndstation(filename, s)

        # check to see that the data needed are there and expand the search
        # if need be

        if space < 0.5: space = 0.5
        while all([len([d for d, e in s.evap if 0 <= e and start <= d and 
                        d <= end]) == 0 for s in stations]):

            print('warning: insufficient evaporation data available in the ' +
                  'box;\nexpanding the search for evaporation data\n')

            # new search area with more space

            bbox = self.get_boundaries(bbox = bbox, space = space)
            print('bounding box: {:.2f}, {:.2f}, {:.2f}, {:.2f}'.format(*bbox))

            # find new stations

            extrastations = find_ghcnd(bbox, var = 'EVAP', dates = (start, end),
                                       verbose = verbose)

            # download the data for the extra stations found

            for s in extrastations: 

                filename = '{}/{}'.format(self.GHCND, s.station)
                
                if not os.path.isfile(filename):

                    s.download_data(self.GHCND, start = start, end = end, 
                                    plot = plot)
                    if len(s.evap) > 0: 

                        stations.append(s)

            space += 0.5

    def extract_gsod(self,
                     bbox,
                     start, 
                     end, 
                     space = 0.1,
                     plot = True,
                     verbose = False,
                     ):
        """Extracts GSOD data for the HUC8."""

        # find the stations in the bounding box

        stations = find_gsod(bbox)

        years = [s.start.year for s in stations if s.start is not None]

        # if more data are needed, expand the bounding box

        if space < 0.5: space = 0.5
        while all([start.year < y for y in years]): 
            print('GSOD files in the watershed do not contain sufficient data')
            print('looking for other stations')

            bbox = self.get_boundaries(bbox = bbox, space = space)
            stations += find_gsod(bbox, dates = (start, end))
            years = [s.start.year for s in stations if s.start is not None]
            space += 0.2
    
        # iterate through the stations and download the data

        for s in stations: 

            filename = '{0}/{1:06d}'.format(self.GSOD, s.airforce)
            if not os.path.isfile(filename):

                s.download_data(self.GSOD, start = start, end = end)

                its = self.GSOD, s.airforce
                if plot: s.plot(output = '{}/{}'.format(*its))

    def extract_precip3240(self,
                           bbox,
                           start, 
                           end, 
                           plot = True,
                           verbose = True,
                           ):
        """downloads hourly precipitation data from NCDC in the bounding box."""

        # find the precipitation stations in the bounding box

        stations = find_precip3240(bbox, dates = (start, end), 
                                   verbose = verbose)

        # download/import the data for each station

        for s in stations:

            filename = '{}/{}'.format(self.precip3240, s.coop)
            if not os.path.isfile(filename):
        
                s.download_data(self.precip3240, start, end, 
                                path_to_7z = self.path_to_7z,
                                clean = False, plot = plot)

                # save the metadata for the station if it has any data

                if len(s.events) > 0:

                    self.metadata.add_precip3240station(filename, s)

    def extract_NSRDB(self,
                      bbox,
                      start, 
                      end, 
                      space = 0.1,
                      plot = True, 
                      verbose = True, 
                      vverbose = False,
                      ):
        """Makes pickled instances of the GageStation class for all the gages
        meeting the calibration criteria for an 8-digit watershed."""

        # find the stations

        stations = find_nsrdb(bbox, dates = (start, end), verbose = verbose)

        for s in stations:

            filename = '{}/{}'.format(self.NSRDB, s.usaf)

            if not os.path.isfile(filename):

                s.download_data(self.NSRDB, dates = (start, end))
                s.plot(start = start, end = end, tstep = 'monthly',
                       output = '{}/{}'.format(self.NSRDB, s.usaf))

    def download(self,
                 bbox,
                 start, 
                 end,
                 output,
                 datasets = 'all',
                 verbose = True,
                 ):
        """Downloads select climate data from the GHCND, GSOD, NSRDB, and 
        NCDC 3240 (hourly precipitation) datasets."""

        # make sure the output directory exists

        if not os.path.isdir(output):
            print('\nerror: directory "{}" does not exist\n'.format(output))
            raise

        # download the GHCND data

        if datasets == 'all' or 'GHCND' in datasets:

            self.GHCND = '{}/GHCND'.format(output)
            if not os.path.isdir(self.GHCND):
                os.mkdir(self.GHCND)
                self.extract_ghcnd(bbox, start, end)
            elif verbose: print('GHCND data exist\n')
                
        # download the GSOD data

        if datasets == 'all' or 'GSOD' in datasets:

            self.GSOD = '{}/GSOD'.format(output)
            if not os.path.isdir(self.GSOD):
                os.mkdir(self.GSOD)
                self.extract_gsod(bbox, start, end)
            elif verbose: print('GSOD data exist\n')

        # download the hourly precipitation data

        if datasets == 'all' or 'precip3240' in datasets:

            self.precip3240 = '{}/precip3240'.format(output)
            if not os.path.isdir(self.precip3240):
                os.mkdir(self.precip3240)
                self.extract_precip3240(bbox, start, end)
            elif verbose: print('precip3240 data exist\n')

        # download the solar radiation data

        if datasets == 'all' or 'NSRDB' in datasets:

            self.NSRDB = '{}/NSRDB'.format(output)
            if not os.path.isdir(self.NSRDB):
                os.mkdir(self.NSRDB)
                self.extract_NSRDB(bbox, start, end)
            elif verbose: print('NSRDB data exist\n')

        # save the metadata for later

        self.set_metadata(output, datasets = datasets)

    def download_shapefile(self,
                           shapefile,
                           start, 
                           end,
                           output,
                           space = 0,
                           datasets = 'all',
                           verbose = True,
                           ):
        """
        Downloads select climate data from the GHCND, GSOD, NSRDB, and NCDC 3240
        (hourly precipitation) datasets. Same as above but uses a shapefile as
        the input and includes an optional space increase to the bounding box.
        """

        bbox = self.get_boundaries(shapefile = shapefile, space = space)

        self.download(bbox, start, end, output, datasets = datasets,
                      verbose = verbose)

    def set_metadata(self,
                     output,
                     datasets = 'all',
                     ):
        """
        Gets the station metadata from the existing files. This method can be
        used to get metadata after the data have already been downloaded.
        """

        # make sure the output directory exists

        if not os.path.isdir(output):
            print('\nerror: directory "{}" does not exist\n'.format(output))
            raise

        # open it up

        filename = '{}/metadata'.format(output)
        if os.path.isfile(filename):
            with open(filename, 'rb') as f: self.metadata = pickle.load(f)
            return

        # set the GHCND data

        if datasets == 'all' or 'GHCND' in datasets:

            self.GHCND = '{}/GHCND'.format(output)
            if not os.path.isdir(self.GHCND):
                print('error: GHCND data do not exist\n')
                raise
            else:
                files = ['{}/{}'.format(self.GHCND, f) 
                         for f in os.listdir(self.GHCND) 
                         if f[-3:] != 'png']

                for filename in files:
                    with open(filename, 'rb') as f: s = pickle.load(f)
                    self.metadata.add_ghcndstation(filename, s)

        # set the GSOD data

        if datasets == 'all' or 'GSOD' in datasets:
            
            self.GSOD = '{}/GSOD'.format(output)
            if not os.path.isdir(self.GSOD):
                print('error: GSOD data do not exist\n')
                raise
            else:
                files = ['{}/{}'.format(self.GSOD, f)
                         for f in os.listdir(self.GSOD) 
                         if f[-3:] != 'png']

                for filename in files:
                    with open(filename, 'rb') as f: s = pickle.load(f)
                    self.metadata.add_gsodstation(filename, s)

        # set the hourly precipitation data

        if datasets == 'all' or 'precip3240' in datasets:

            self.precip3240 = '{}/precip3240'.format(output)
            if not os.path.isdir(self.precip3240):
                print('error: precip3240 data do not exist\n')
                raise
            else:
                files = ['{}/{}'.format(self.precip3240, f)
                         for f in os.listdir(self.precip3240) 
                         if f[-3:] != 'png' and 
                         not os.path.isdir('{}/{}'.format(self.precip3240, f))]

                for filename in files:
                    with open(filename, 'rb') as f: s = pickle.load(f)
                    self.metadata.add_precip3240station(filename, s)

        # set the solar radiation data

        if datasets == 'all' or 'NSRDB' in datasets:

            self.NSRDB = '{}/NSRDB'.format(output)
            if not os.path.isdir(self.NSRDB):
                print('error: NSRDB data do not exist\n')
                raise
            else:
                files = ['{}/{}'.format(self.NSRDB, f)
                         for f in os.listdir(self.NSRDB) 
                         if f[-3:] != 'png']

                for filename in files:
                    with open(filename, 'rb') as f: s = pickle.load(f)
                    self.metadata.add_nsrdbstation(filename, s)

        # save the metadata for later

        filename = '{}/metadata'.format(output)
        if not os.path.isfile(filename):
            
            with open(filename, 'wb') as f: pickle.dump(self.metadata, f) 

    def get_distance(self, p1, p2):
        """
        Approximates the distance in kilometers between two points on the 
        Earth's surface designated in decimal degrees using an ellipsoidal 
        projection. CFR 73.208 indicates applicability up to 475 kilometers.
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

    def aggregate(self,
                  database,
                  parameter,
                  start,
                  end,
                  method = None,
                  latitude = None,
                  longitude = None,
                  elevation = None,
                  nmax = 10,
                  verbose = True,
                  vverbose = False,
                  ):
        """
        Aggregates data for "parameter" in the "database" from "start" to 
        "end" for up to "nmax" series and returns the time series output.
        Optionally can perform an inverse distance-weighted average using the
        method to 'IDWA' and providing the latitude and longitude.
        """

        # some error handling

        if   database == 'GHCND':      
            stations = self.metadata.ghcndstations
        elif database == 'GSOD':       
            stations = self.metadata.gsodstations
        elif database == 'NSRDB':      
            stations = self.metadata.nsrdbstations
        elif database == 'precip3240': 
            stations = self.metadata.precip3240stations
        else:
            print('error: database {} not recognized'.format(database))
            raise

        if len(stations) == 0:
            print('error: no {} stations present in directory'.format(database))
            raise

        # weighted average check

        if method is not None:

            if method not in ['IDWA']:
                print('error: unknown weighting scheme ' +
                      '"{}" specified'.format(weights))
                raise
                
            elif latitude is None or longitude is None:
                print('to use the distance weighted-average, the location ' +
                      '(latitude and longitude) must be specified\n')
                raise

            elif not self.is_number(latitude) or not self.is_number(longitude):
                print('latitude and longitude must be numeric')
                raise                

        # make sure the parameter exists in the database

        s = stations[[k for k in stations][0]]
        if parameter not in s:
            its = database, parameter
            print('error: database {} contains no parameter {}'.format(*its))
            print('available parameters include:', ', '.join(s.keys()))
            raise

        if start >= end:
            print('error: end date must be after start date')
            raise
            
        # find the longest "n" datasets

        datalengths = []
        for k, v in stations.items(): datalengths.append((v[parameter], k))

        datalengths.sort()

        # save the station file names and locations

        if len(datalengths) > nmax: 
            names = [k for v, k in datalengths[-nmax:]]
        else:                       
            names = [k for k in stations]
            nmax = len(stations)

        # preallocate an array to use to aggregate the data

        if database in ['GSOD', 'GHCND']:

            data = numpy.empty((nmax, (end-start).days))

        elif database in ['NSRDB', 'precip3240']:

            data = numpy.empty((nmax, (end-start).days * 24))

        # locations of the stations

        descriptions, lats, lons, elevs = [], [], [], []

        # iterate through the stations and fill the array

        for i, k in enumerate(names):

            if verbose: 
                
                its = parameter, k
                print('fetching {} data from {}'.format(*its))

            # read the data into the array

            with open(k, 'rb') as f: s = pickle.load(f)

            data[i,:] = s.make_timeseries(parameter, start = start, end = end)

            # station locations

            descriptions.append(stations[k]['name'])
            lons.append(stations[k]['longitude'])
            lats.append(stations[k]['latitude'])
            elevs.append(stations[k]['elevation'])

        # weighting factors for inverse distance-weighted average (or other)

        if method is not None:

            if method == 'IDWA': 

                p1 = float(longitude), float(latitude)

                # find the distance between the supplied point and each station

                distances = [self.get_distance(p1, (lon, lat))
                             for lon, lat in zip(lons, lats)]

                if verbose:

                    print('')
                    for i in zip(lons, lats, descriptions, distances):

                        print('distance between ' +
                              '{:.2f}, {:.2f} & '.format(*p1) 
                              + '{:.2f}, {:.2f} {:22s}: {:.1f} km'.format(*i))

                # use the inverse distance squared as the weighting factors

                weights = numpy.array([1/d**2 for d in distances])

        if verbose: print('')

        # transpose the series to switch the ordering from stations to days

        data = data.transpose()
        
        # make a mask for missing data

        mask = numpy.invert(numpy.isnan(data))

        # iterate through the data/mask and average

        if verbose: print('aggregating the data...\n')

        if method is None:

            averages = [row[m].mean() 
                        if row[m].size > 0 else None
                        for row, m in zip(data, mask)]

        elif method == 'IDWA':

            averages = [(row[m] * weights[m]).sum() / weights[m].sum() 
                        if row[m].size > 0 else None
                        for row, m in zip(data, mask)]

        # if there were no data for all time series for a day, fill with the
        # previous days

        if len([a for a in averages if a is None]) > 0:

            missing = [i for i in range(len(averages)) if averages[i] is None]
            print('warning: missing {} values;'.format(len(missing)) +
                  ' trying to fill with next day\n')
            
            for i in missing:

                if vverbose:
                    if database in ['GSOD', 'GHCND']: 
                        delta = datetime.timedelta(days = 1)
                    elif database in ['NSRDB', 'precip3240']:
                        delta = datetime.timedelta(hours = 1)
                    print('filling {}'.format(start + delta * i))
                j = 0
                while averages[j] is None and j < len(averages) - 1: j += 1
                if j == len(averages): averages[i] = 0
                else:                  averages[i] = averages[j]

        # return the timeseries

        return averages

