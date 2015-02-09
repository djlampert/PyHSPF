# climateprocessor.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 02/07/2015
#
# Purpose: contains the ClimateProcessor class to import climate data files 
# and generate time series for hydrologic modeling.

import os, pickle, shutil

from shapefile import Reader

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
        self.GHCND      = output
        self.GSOD       = output
        self.precip3240 = output
        self.NSRDB      = output
        self.verbose    = verbose

        # the path to 7zip to decompress the archives

        self.path_to_7z = path_to_7z

    def get_boundaries(self, 
                       bbox = None,
                       shapefile = None, 
                       space = 0.1,
                       ):
        """Gets the boundaries for the plot."""

        if bbox is not None: boundaries = [x for x in bbox]

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

        for s in stations: s.download_data(self.GHCND, start = start, end = end,
                                           plot = plot)

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

                s.download_data(self.GHCND, start = start, end = end, 
                                plot = plot)
                if len(s.evap) > 0: stations.append(s)

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

            output = '{}/{}'.format(self.precip3240, s.coop)
            if not os.path.isfile(output):
        
                s.download_data(self.precip3240, start, end, 
                                path_to_7z = self.path_to_7z,
                                clean = False, plot = plot)

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

            output = '{}/{}'.format(self.NSRDB, s.usaf)

            if not os.path.isfile(output):

                s.download_data(self.NSRDB, dates = (start, end))
                s.plot(start = start, end = end, tstep = 'monthly',
                       output = '{}/{}'.format(self.NSRDB, s.usaf))

    def download(self,
                 bbox,
                 start, 
                 end,
                 output,
                 datasets = 'all', 
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

        # download the GSOD data

        if datasets == 'all' or 'GSOD' in datasets:

            self.GSOD = '{}/GSOD'.format(output)
            if not os.path.isdir(self.GSOD):

                os.mkdir(self.GSOD)
                self.extract_gsod(bbox, start, end)

        if datasets == 'all' or 'precip3240' in datasets:

            self.precip3240 = '{}/precip3240'.format(output)
            if not os.path.isdir(self.precip3240):

                os.mkdir(self.precip3240)
                self.extract_precip3240(bbox, start, end)

        if datasets == 'all' or 'NSRDB' in datasets:

            self.NSRDB = '{}/NSRDB'.format(output)
            if not os.path.isdir(self.NSRDB):

                os.mkdir(self.NSRDB)
                self.extract_NSRDB(bbox, start, end)

