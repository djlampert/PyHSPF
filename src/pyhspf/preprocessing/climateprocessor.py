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
                      types = 'all', 
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
                print('fetching the {} time series data from {}'.format(*its))

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
                    for i in zip(descriptions, lons, lats, distances):

                        print('the distance between ' +
                              '{} and {:.4f}, {:.4f} is {:.1f} km'.format(*i))

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
                  ' trying to fill with next day')
            
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

    def hourly_temperature(self, tmin, tmax):
        """Develops an hourly time series of temperatures from the daily min 
        and max.  Assumes a sinusoidal profile from the min temp to the max 
        from 6 am to 4 pm.
        """

        # fill in the first 6 hours with tmin

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
                    hourly.append(mid + amp * numpy.cos(i * numpy.pi / 14))

        # add the last day

        if tmin[-1] is None or tmax[-1] is None:
            for i in range(18): hourly.append(None)
        else:
            for i in range(10):
                hourly.append(tmin[-1] + (tmax[-1] - tmin[-1]) * i / 10)
        
            # assume tmax to the end of the day

            for i in range(8): hourly.append(tmax[-1])

        return hourly

    def aggregate_temperature(self, 
                              database,
                              #tempstations, 
                              #dewstations, 
                              start, 
                              end, 
                              destination,
                              
                              tstep = 'daily',
                              ):
        """Aggregates the raw temperature data into a daily min, max, and 
        dewpoint temperature series, then makes an hourly temperature series."""

        # make a time series and figure out how many times are in the series

        if tstep == 'daily':

            delta = datetime.timedelta(days = 1)
            times = [start + delta * i for i in range((end-start).days)]
            n = len(times)

        else:

            print('error: unknown time step size {} specified'.format(tstep))
            raise

        if database == 'GHCND': 

            files = ['{}/{}'.format(self.GHCND, f) for f in self.GHCNDstations]

        elif database == 'GSOD':  

            files = ['{}/{}'.format(self.GHCND, f) for f in self.GSODstations]

        tmins = []
        for name in files:

            with open(name, 'rb') as f: s = pickle.load(f)

            tmin = numpy.array(s.make_timeseries('tmin', start = start, 
                                                 end = end))

            if tmin is not None:

                if len(tmin) > 1:
                    tmins.append(tmin)
                    print(s.name, tmin.size)

        print(len(tmins))

        #tmin, tmax = [], []
        #for row in zip(*tmins):
        #    day = []
        #    for v in row:
        #        if v is not None: day.append(v)
        #        elif len(tmins) == 1: day.append(tmin[-1])        
        #    try:
        #        tmin.append(sum(day) / len(day))                
        #    except: print(day)

        #for row in zip(*tmaxs):
        #    day = []
        #    for v in row:
        #        if v is not None: 
        #            day.append(v)
        #        elif len(tmaxs) == 1: day.append(tmax[-1])
        #    tmax.append(sum(day) / len(day))

        #with open('{}/tmax'.format(destination), 'wb') as f: 
        #    pickle.dump((start, 1440, tmax), f)
        #with open('{}/tmin'.format(destination), 'wb') as f: 
        #    pickle.dump((start, 1440, tmin), f)

        # make the hourly timeseries

        #hourly_temp = hourly_temperature(tmin, tmax)

        #with open('{}/hourlytemperature'.format(destination), 'wb') as f: 
        #    pickle.dump((start, 60, hourly_temp), f)

        # process the raw dewpoint into a single daily timeseries

        #dewpoints = [v.make_timeseries(start, end) 
        #             for k, v in dewstations.items()]
        #dewpoints = [d for d in dewpoints if d is not None]

        #dewpoint = []
        #for row in zip(*dewpoints):
        #    day = [v for v in row if v is not None]
        #    if len(day) > 0: 
        #        dewpoint.append(sum(day) / len(day))
        #    else:            dewpoint.append(None)

        # fill in missing data with tmin

        #for t, d, i in zip(tmin, dewpoint, range(len(dewpoint))):

        #    if d is None or d > t: dewpoint[i] = t

        #with open('{}/dewpoint'.format(destination), 'wb') as f: 
        #    pickle.dump((start, 1440, dewpoint), f)

    def aggregate_wind(self,
                       windstations, 
                       start, 
                       end, 
                       destination):
        """Processes the raw wind speed data into a single daily timeseries."""

        winds = [v.make_timeseries(start, end) for k, v in windstations.items()]
        winds = [w for w in winds if w is not None]

        wind = []
        for row in zip(*winds):
            day = [v for v in row if v is not None]
            if len(day) > 0: 
                wind.append(sum(day) / len(day))
            else:            wind.append(wind[-1])

        with open('{}/wind'.format(destination), 'wb') as f:
            pickle.dump((start, 1440, wind), f)

    def aggregate_solar(self, 
                        solarstations, 
                        start, 
                        end, 
                        destination):
        """Processes the raw solar radiation data into a single daily and
        hourly timeseries."""

        solars = [v.make_timeseries(start, end) 
                  for k, v in solarstations.items()]
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

    def make_precipitation(self, 
                           subbasins, 
                           gages, 
                           start, 
                           end, 
                           destination,
                           plot = False, 
                           verbose = True):
        """Takes values of precipitation for the watershed from the 
        PrecipStation instances and performs an inverse-distance weighted 
        average for each subbasin centroid and assigns them to a WDM file."""

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

                precip = [25.4 * p 
                          for p in weighted_avg(hourly, weights = weights)]

                # dump the results and save for later

                with open(output, 'wb') as f: 

                    pickle.dump((start, 60, precip), f)

        precipitation = {}
        for subbasin in subbasins:

            output = '{}/{}'.format(d, subbasin)
            with open(output, 'rb') as f: 
                precipitation[subbasin] = pickle.load(f)

        return precipitation

    def make_snow(self, 
                  directory, 
                  HUC8, 
                  start, 
                  end, 
                  verbose = True, 
                  vverbose = False):
        """Averages the data from the snowstations into a single timeseries."""

        if verbose: print('averaging snowfall and snowdepth station data\n')

        snowdata = '{}/{}/snow/snow'.format(directory, HUC8)
        with open(snowdata, 'rb') as f: snowstations = pickle.load(f)

        times = [start + datetime.timedelta(days = 1) * i 
                 for i in range((end - start).days)]

        # snowfall

        data = [s.make_timeseries(start, end, tstype = 'fall', verbose=vverbose)
                for s in snowstations.values()]

        # remove any timeseries with insufficient data

        data = [d for d in data if d is not None]

        if len(data) == 0: 
            print('error, no snowfall data exist')
            raise

        falls = []
        for values in zip(*data):
            day = [0 if v is None else v for v in values]
            if len(day) > 0: 
                falls.append(sum(day) / len(day))
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
            if len(day) > 0: 
                depths.append(sum(day) / len(day))
            else:            depths.append(0)

        with open('{}/{}/snow/snowdepth'.format(directory, HUC8), 'wb') as f:
            pickle.dump((times, depths), f)

    def make_timeseries(self, start, end, output, verbose = True):
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

        if verbose: print('aggregating existing timeseries...\n')

        # source data

        #v = directory
        #evapstations  = '{}/{}/evaporations/evaporation'.format(*v)
        #tempstations  = '{}/{}/temperatures/temperature'.format(*v)
        #dewstations   = '{}/{}/dewpoint/dewpoint'.format(*v)
        #windstations  = '{}/{}/wind/wind'.format(*v)
        #solarstations = '{}/{}/solar/solarstations'.format(*v)
        #snowstations  = '{}/{}/snow/snow'.format(*v)
        #gagedirectory = '{}/{}/NWIS'.format(*v)

        # output directory

        p = '{}/watershedtimeseries'.format(output)
        if not os.path.isdir(p): os.mkdir(p)

        return

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

