###############################################################################
#                                                                             #
# Hydrological Simulation Program in Fortran (HSPF) Preprocessor              #
#                                                                             #
# David J. Lampert                                                            #
#                                                                             #
# last updated: 08/09/2015                                                    #
#                                                                             #
# Purpose: Extracts hydrography, land use, and climate data from online       #
# databases, spatially aggregates data, constructs precipitation and          #
# potential evapotranspiration time series, and builds an instance of the     #
# HSPFModel class.                                                            #
#                                                                             #
###############################################################################

###############################################################################
#                                                                             #
# The source data directories are all available online. Note that if the      #
# online file structure is modified the paths will need to be updated.        #
#                                                                             #
# The National Hydrography Dataset Plus (NHDPlus)                             #
# http://www.horizon-systems.com/nhdplus/                                     #
#                                                                             #
# The National Elevation Dataset (NED) on a 30-m grid (supplied with NHDPlus) #
#                                                                             #
# The Cropland Data Layer (CDL) available at                                  #
# http://nassgeodata.gmu.edu/CropScape                                        #
#                                                                             #
# The National Inventory of Dams (NID) shapefile available at                 #
# http://nationalatlas.gov/metadata/dams00x020.faq.html                       #
#                                                                             #
# The National Water Information System (NWIS) available at                   #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgages.xml              #
#                                                                             #
# The National Climate Data Center (NCDC)                                     #
# ftp://ftp.ncdc.noaa.gov/pub/data/                                           #
#                                                                             #
# The National Solar Radiation Database (NSRDB)                               #
# http://rredc.nrel.gov/solar/old_data/nsrdb/                                 #
#                                                                             #
###############################################################################

import os, time, pickle, datetime, csv

from shapefile  import Reader

# local imports

from .nhdplusextractor import NHDPlusExtractor
from .nwisextractor    import NWISExtractor
from .nidextractor     import NIDExtractor
from .delineators      import HUC8Delineator
from .ftablecalculator import FtableCalculator
from .cdlextractor     import CDLExtractor
from .climateprocessor import ClimateProcessor
from .etcalculator     import ETCalculator
from  pyhspf.core      import HSPFModel, Watershed, Subbasin

class Preprocessor:
    """
    A class that integrates the various preprocessing tools together into a 
    single object to build a baseline HSPFModel for an 8-digit Hydrologic Unit
    Code (HUC8).
    """

    def __init__(self,
                 network      = None, 
                 output       = None, 
                 HUC8         = None,
                 start        = None,
                 end          = None,
                 cdlaggregate = None,
                 landuse      = None,
                 watershed    = None,
                 htemp        = None,
                 hdewpoint    = None,
                 hsolar       = None,
                 hwind        = None,
                 hPETs        = None,
                 precip       = None,
                 hspfmodel    = None,
                 ):
        """
        Constructor method; sets up pointers to directory locations.
        """

        # paths to directories and model domain

        self.network      = network
        self.output       = output
        self.HUC8         = HUC8
        self.start        = start
        self.end          = end
        self.cdlaggregate = cdlaggregate
        self.landuse      = landuse

        # paths to essential data structure files

        self.hspfmodel = hspfmodel
        self.watershed = watershed
        self.htemp     = htemp
        self.hdewpoint = hdewpoint
        self.hwind     = hwind
        self.hsolar    = hsolar
        self.hPETs     = hPETs
        self.precip    = precip

    def set_network(self,
                    network,
                    NHDPlus = None,
                    NED     = None,
                    NWIS    = None,
                    CDL     = None,
                    NID     = None,
                    ):
        """
        Sets the default source data locations on the network.
        """

        self.network = network

        # if provided, set the file paths to raw data; otherwise use defaults

        if NHDPlus is None: self.NHDPlus = '{}/NHDPlus'.format(self.network)
        else:               self.NHDPlus = NHDPlus

        if NED is None:     self.NED = '{}/NEDSnapshot'.format(self.NHDPlus)
        else:               self.NED = NED

        if NWIS is None:    self.NWIS = '{}/NWIS'.format(self.network)
        else:               self.NWIS = NWIS

        if CDL is None:     self.CDL = '{}/CDL'.format(self.network)
        else:               self.CDL = CDL

        if NID is None:     self.NID = '{}/NID'.format(self.network)
        else:               self.NID = NID

    def set_output(self, output):
        """
        Sets the location for output directories.
        """

        self.output = output

    def set_parameters(self,
                       HUC8         = None,
                       start        = None,
                       end          = None,
                       cdlaggregate = None,
                       landuse      = None,
                       ):
        """
        Sets simulation-specific parameters.
        """

        if HUC8         is not None:  self.HUC8         = HUC8
        if start        is not None:  self.start        = start
        if end          is not None:  self.end          = end
        if cdlaggregate is not None:  self.cdlaggregate = cdlaggregate
        if landuse      is not None:  self.landuse      = landuse

    def make_float(self,
                   s,
                   ):
        """
        Converts string "s" into a float.
        """

        try: 
            f = float(s)
            return f
        except:
            print('error, cannot convert string to float')
            raise

    def preprocess(self,
                   hydrography     = None,
                   gagedirectory   = None,
                   landusedata     = None,
                   climatedata     = None,
                   hspfdirectory   = None,
                   drainmax        = 400, 
                   extra_outlets   = None,
                   masslink        = True,
                   plotET          = True,
                   hydrographyplot = True,
                   preliminary     = True,
                   delineated      = True,
                   landuseplots    = True,
                   verbose         = True, 
                   vverbose        = False, 
                   parallel        = True,
                   ):
        """
        Gathers all the input data for HSPF.
        """

        # check the network is mounted on Unix-like systems

        if os.name != 'nt':

            if (not os.path.ismount(self.network) and
                not os.path.isdir(self.network)):
                print('\nerror: network {} '.format(self.network) +
                      'does not exist or is not mounted\n')
                raise

        # check that all the required information has been supplied

        required = {self.network:      'network directory location', 
                    self.output:       'output directory location', 
                    self.HUC8:         '8-digit hydrologic unit code', 
                    self.start:        'start date', 
                    self.end:          'end date', 
                    self.cdlaggregate: 'CDL aggregation file', 
                    self.landuse:      'land use parameter file',
                    }

        for info in required:

            if info is None:
                
                print('error: required information ' +
                      '"{}" has not supplied!\n'.format(info))
                raise

        # keep track of how long it takes

        go = time.time()

        # if the destination folder does not exist, make it

        if not os.path.isdir(self.output): os.mkdir(self.output)

        # if the destination folder for the HUC8 does not exist, make it

        its = self.output, self.HUC8
        directory = '{}/{}'.format(*its)
        if not os.path.isdir(directory): os.mkdir(directory)

        # make a subdirectory for hydrography data

        if hydrography is None:
            self.hydrography = '{}/{}/hydrography'.format(*its)
        else:
            self.hydrography = hydrography

        if not os.path.isdir(self.hydrography): os.mkdir(self.hydrography)

        # make a subdirectory for NWIS flow and water quality data

        if gagedirectory is None:
            self.gagedirectory = '{}/{}/gagedata'.format(*its)
        else:
            self.gagedirectory = gagedirectory

        if not os.path.isdir(self.gagedirectory): os.mkdir(self.gagedirectory)

        # make a directory for the CDL data

        if landusedata is None:
            self.landusedata = '{}/{}/landuse'.format(*its)
        else:
            self.landusedata = landusedata

        if not os.path.isdir(self.landusedata): os.mkdir(self.landusedata)

        # make a directory for the climate data

        if climatedata is None:
            self.climatedata = '{}/{}/climate'.format(self.output, self.HUC8)
        else:
            self.climatedata = climatedata

        if not os.path.isdir(self.climatedata): os.mkdir(self.climatedata)

        # make a subdirectory for HSPF calculations

        if hspfdirectory is None:
            self.hspfdirectory = '{}/{}/hspf'.format(*its)
        else:
            self.hspfdirectory = hspfdirectory

        if not os.path.isdir(self.hspfdirectory): os.mkdir(self.hspfdirectory)

        # make a list of all the years for the CDL extraction

        self.years = [self.start.year]
        
        t = self.start
        while t < self.end:
            if t.year not in self.years: self.years.append(t.year)
            t += datetime.timedelta(days = 1)

        # file paths to files used throughout preprocessing

        self.flowfile      = '{}/flowlines'.format(self.hydrography)
        self.catchmentfile = '{}/catchments'.format(self.hydrography)
        self.elevfile      = '{}/elevations'.format(self.hydrography)
        self.damfile       = '{}/dams'.format(self.hydrography)
        self.gagefile      = '{}/gagestations'.format(self.gagedirectory)
        self.boundaryfile  = '{}/boundary'.format(self.hydrography)
        self.VAAfile       = '{}/flowlineVAAs'.format(self.hydrography)
        self.outletfile    = '{}/subbasin_outlets'.format(self.hydrography)
        self.flowlinefile  = '{}/subbasin_flowlines'.format(self.hydrography)
        self.subbasinfile  = '{}/subbasin_catchments'.format(self.hydrography)

        # watershed data container for hydrography and land use data

        its = self.output, self.HUC8
        self.watershed = '{}/{}/hspf/watershed'.format(*its)

        # make directories for hourly and daily aggregated climate time series

        self.hourly = '{}/hourly'.format(self.climatedata)
        self.daily  = '{}/daily'.format(self.climatedata)

        if not os.path.isdir(self.hourly): os.mkdir(self.hourly)
        if not os.path.isdir(self.daily):  os.mkdir(self.daily)

        # make a directory for subbasin-specific precipitation time series

        self.precip = '{}/hourlyprecipitation'.format(self.climatedata)
        if not os.path.isdir(self.precip): os.mkdir(self.precip)

        # make a directory for the evapotranspiration time series

        its = self.climatedata
        self.evapotranspiration = '{}/evapotranspiration'.format(its)
        if not os.path.isdir(self.evapotranspiration): 
            os.mkdir(self.evapotranspiration)

        # file paths to pickled file containing "final" time series

        self.tmin      = '{}/tmin'.format(self.daily)
        self.tmax      = '{}/tmax'.format(self.daily)
        self.dewpoint  = '{}/dewpoint'.format(self.daily)
        self.wind      = '{}/wind'.format(self.daily)
        self.snowfall  = '{}/snowfall'.format(self.daily)
        self.snowdepth = '{}/snowdepth'.format(self.daily)
        self.hsolar    = '{}/solar'.format(self.hourly)
        self.dsolar    = '{}/solar'.format(self.daily)
        self.dRET      = '{}/dailyRET'.format(self.evapotranspiration)
        self.hRET      = '{}/hourlyRET'.format(self.evapotranspiration)
        self.htemp     = '{}/temperature'.format(self.hourly)
        self.hdewpoint = '{}/dewpoint'.format(self.hourly)        
        self.hwind     = '{}/wind'.format(self.hourly)        
        self.hPETs     = '{}/hourlyPETs'.format(self.evapotranspiration)

        # path to file containing a list of GHCND stations with evaporation data

        self.evaporation = '{}/evaporation'.format(self.evapotranspiration)

        # extract the data for the HUC8 from the sources

        self.extract_hydrography(plot = hydrographyplot)

        # delineate the subbasins and the hydrography data

        self.delineate(drainmax      = drainmax, 
                       extra_outlets = extra_outlets,
                       parallel      = parallel,
                       preliminary   = preliminary,
                       delineated    = delineated,
                       )

        # download and extract land use data

        self.extract_CDL(plots = landuseplots)

        # build the watershed object

        if not os.path.isfile(self.watershed): 
            self.build_watershed(masslink = masslink)

        # download and extract the climate data

        self.climate(plotET = plotET)

        if verbose: 

            print('completed preprocessing watershed in ' +
                  '{:.1f} seconds\n'.format((time.time() - go)))

    def build_hspfmodel(self, 
                        output = None,
                        landuseyear = 2008,
                        ifraction = 0.5,
                        atemp = False,
                        snow = False,
                        hydrology = False,
                        verbose = True,
                        ):
        """
        Builds an instance of the HSPFModel class to the output location using
        land use data for the given year after the necessary data have been 
        supplied.
        """

        # location for the output baseline HSPFModel class instance file

        if output is None:
 
            its = self.hspfdirectory, landuseyear
            self.hspfmodel = '{}/{}baseline'.format(*its)

        else:

            self.hspfmodel = output

        # make sure the file doesn't already exist

        if os.path.isfile(self.hspfmodel):

            print('HSPFModel {} exists\n'.format(self.hspfmodel))
            return
        
        elif verbose:

            print('building the HSPFModel...\n')

        # make sure the hydrography and land use data have been used to create
        # an instance of the Watershed class

        if self.watershed is None:

            print('error: watershed data stucture has not been supplied!\n')
            raise

        # make sure the climate time series data are available

        climateseries = {self.htemp:     'hourly temperature', 
                         self.hdewpoint: 'hourly dewpoint', 
                         self.hwind:     'hourly wind speed', 
                         self.hsolar:    'hourly solar radiation',
                         self.hPETs:     'potential evapotranspiration',
                         }

        for k in climateseries: 

            if not os.path.isfile(k):

                print('error: {} '.format(climateseries[k]) +
                      'time series data are not available')
                raise

        # open the watershed data structure

        with open(self.watershed, 'rb') as f: w = pickle.load(f)

        # check that land use data for the given year are available

        for subbasin in w.subbasins.values():
            if landuseyear not in subbasin.landuse:
                print('error: land use data for year ' +
                      '{} have not been supplied'.format(landuseyear))
                raise

        # make a list of all land use categories for the watershed

        lucs = []
        for subbasin in w.subbasins.values():
            for luc in subbasin.landuse[landuseyear]:
                if luc not in lucs: lucs.append(luc)

        # open up the PET time series dictionary

        with open(self.hPETs, 'rb') as f: PETs = pickle.load(f)

        # check that PET time series are available for each land use category

        for luc in lucs:
            if luc not in PETs: 
                print('error: potential evapotranspiration time series data ' +
                      'for {} are not available'.format(luc))
                raise

        # check that precipitation time series are available for each subbasin

        if self.precip is None:
            print('error: precipitation directory has not been specified\n')
            raise

        for comid in w.subbasins:
            p = '{}/{}'.format(self.precip, comid)
            if not os.path.isfile(p):
                print('error: precipitation time series ' +
                      '{} is missing\n'.format(p))
                raise

        # create and build the model

        hspfmodel = HSPFModel()

        hspfmodel.build_from_watershed(w, self.hspfmodel, 
                                       landuseyear = landuseyear,
                                       verbose = verbose)

        # turn on the air temperature correction modules for each operation

        if atemp: hspfmodel.add_atemp()

        # turn on the snow modules and use observed depth as the initial state
        # for each operation

        if snow: 

            with open(self.snowdepth, 'rb') as f: s, t, data = pickle.load(f)

            hspfmodel.add_snow(depth = data[0])

            # add the snowdepth and snowfall time series

            hspfmodel.add_timeseries('snowdepth', self.HUC8, s, data, tstep = t)

            with open(self.snowfall, 'rb') as f: s, t, data = pickle.load(f)

            hspfmodel.add_timeseries('snowfall', self.HUC8, s, data, tstep = t)

            # assign the time series to the whole watershed (the HUC8 is the 
            # name for given to the time series in this case)

            hspfmodel.assign_watershed_timeseries('snowdepth', self.HUC8)
            hspfmodel.assign_watershed_timeseries('snowfall', self.HUC8)

        # turn on the hydrology modules for each operation

        if hydrology: hspfmodel.add_hydrology()

        # add the hourly watershed-level time series to the model 
        # (temperature, dewpoint, wind, solar)
        
        # open up the temperature time series

        with open(self.htemp, 'rb') as f: s, t, data = pickle.load(f)

        # add it to the HSPFModel

        hspfmodel.add_timeseries('temperature', self.HUC8, s, data)

        # assign it to the whole watershed

        hspfmodel.assign_watershed_timeseries('temperature', self.HUC8)

        # open up and assign the dewpoint time series

        with open(self.hdewpoint, 'rb') as f: s, t, data = pickle.load(f)
        hspfmodel.add_timeseries('dewpoint', self.HUC8, s, data)
        hspfmodel.assign_watershed_timeseries('dewpoint', self.HUC8)

        # open up the wind time series

        with open(self.hwind, 'rb') as f: s, t, data = pickle.load(f)

        # have to convert the units from m/s to km/interval (km/hr)

        factor = 60 * t / 1000 / 24

        wind = [w * factor for w in data]

        hspfmodel.add_timeseries('wind', self.HUC8, s, wind)

        # assign to the whole watershed

        hspfmodel.assign_watershed_timeseries('wind', self.HUC8)

        # open up the solar radiation time series
        
        with open(self.hsolar, 'rb') as f: s, t, data = pickle.load(f)

        # convert from W/m2 to langley/interval (langley/hr)

        factor = 0.001434 * t

        solar = [s * factor for s in data]

        hspfmodel.add_timeseries('solar', self.HUC8, s, solar)

        hspfmodel.assign_watershed_timeseries('solar', self.HUC8)

        # add land use-specific PET time series to the model

        for k in PETs:

            s, t, PET = PETs[k]

            hspfmodel.add_timeseries('evaporation', k, s, PET, tstep = t)

            # assign the time series to each land use category

            hspfmodel.assign_landuse_timeseries('evaporation', k, k)

            # if the category is developed, also add it to the IMPLNDs

            if k == 'Developed':

                hspfmodel.assign_landuse_timeseries('evaporation', 'Impervious',
                                                    k)

        # add the subbasin-specific precipitation time series to the model

        for c in w.subbasins:
            
            # path to the data file

            p = '{}/{}'.format(self.precip, c)

            # load the data

            with open(p, 'rb') as f: s, t, data = pickle.load(f)

            # add it to the model
            
            hspfmodel.add_timeseries('precipitation', c, s, data, tstep = t)

            # assign the data to the subbasin

            hspfmodel.assign_subbasin_timeseries('precipitation', c, c)

        # add the flow gages using the NWIS metadata in the subbasin outlet file

        reader = Reader(self.outletfile)

        comid_index = reader.fields.index(['COMID',   'N',  9, 0]) - 1
        nwis_index  = reader.fields.index(['SITE_NO', 'C', 15, 0]) - 1

        # add the data from each station to the model

        for r in reader.records():

            comid = '{}'.format(r[comid_index])

            if isinstance(r[nwis_index], bytes): gageid = None
            else: gageid = r[nwis_index]

            if gageid is not None:

                gagefile = '{}/{}'.format(self.gagedirectory, gageid)
                with open(gagefile, 'rb') as f: s = pickle.load(f)

                # make a time series from the start to the end date

                data = s.make_timeseries(self.start, self.end)

                # add it to the model

                hspfmodel.add_timeseries('flowgage', gageid, self.start, data, 
                                         tstep = 1440)
            
                # assign it to the corresponding subbasin 

                hspfmodel.assign_subbasin_timeseries('flowgage', comid, gageid)
            
        # save the HSPFModel for later

        with open(self.hspfmodel, 'wb') as f: pickle.dump(hspfmodel, f)
            
    def extract_hydrography(self,
                            plot = True,
                            ):
        """
        Downloads and extracts NHDPlus, NWIS, and NID data for a HUC8.
        """

        # 2-digit HUC for the HUC8 or Vector Processing Unit (for NHDPlus)

        VPU = self.HUC8[:2]

        # create an instance of the NHDPlusExtractor for the VPU

        nhdplusextractor = NHDPlusExtractor(VPU, self.NHDPlus)

        # file name for the output plot file

        if plot:
            plotfile = '{}/watershed'.format(self.hydrography)
        else:
            plotfile = None

        # extract NHDPlus data from the VPU for the HUC8 to the output directory

        nhdplusextractor.extract_HUC8(self.HUC8, self.hydrography, 
                                      plotfile = plotfile)

        # create an instance of the NWISExtractor to extract flow and water 
        # quality data for the gages in the HUC8

        nwisextractor = NWISExtractor(self.NWIS)

        # extract the gage station metadata into a new shapefile for the HUC8

        nwisextractor.extract_HUC8(self.HUC8, self.gagedirectory)

        # download all the gage time series data to the gagepath directory

        nwisextractor.download_all(self.start, self.end, 
                                   output = self.gagedirectory)

        # create an instance of the NIDExtractor to extract data for the HUC8

        nidextractor = NIDExtractor(self.NID)

        # extract the dam data from the NID to a new shapefile using the 
        # bounding box for the boundary shapefile

        nidextractor.extract_shapefile(self.boundaryfile, self.damfile)

    def delineate(self,
                  drainmax      = 400, 
                  extra_outlets = None,
                  parallel      = True,
                  preliminary   = True,
                  delineated    = True,
                  verbose       = True, 
                  vverbose      = False, 
                  ):
        """
        Delineates subbasins based on NHDPlus data.
        """

        # create an instance of the HUC8Delineator to use to subdivide the 
        # HUC8 into subbasins

        delineator = HUC8Delineator(self.HUC8, 
                                    self.VAAfile, 
                                    self.flowfile, 
                                    self.catchmentfile, 
                                    self.elevfile,
                                    self.gagefile, 
                                    self.damfile,
                                    ) 

        # delineate the watershed using the NHDPlus data and delineator

        delineator.delineate(self.hydrography, 
                             parallel       = parallel,
                             drainmax       = drainmax, 
                             extra_outlets  = extra_outlets,
                             years          = (self.start.year, self.end.year),
                             watershedplots = False,
                             verbose        = verbose,
                             vverbose       = vverbose,
                             )

        # plot the preliminary data

        f = '{}/preliminary.png'.format(self.hydrography)
        if preliminary and not os.path.isfile(f):

            description = 'Catchments, Flowlines, Dams, and Gages'
            title = ('Cataloging Unit {}\n{}'.format(self.HUC8, description))
            delineator.plot_watershed(self.catchmentfile,
                                      title = title,
                                      dams = True,
                                      width = 0.06,
                                      output = f,
                                      verbose = verbose,
                                      )

        # plot the delineated data

        f = '{}/delineated.png'.format(self.hydrography)
        if delineated and not os.path.exists(f):

            description = 'Subbasins, Major Flowlines, and Calibration Gages'
            title = ('Cataloging Unit {}\n{}'.format(self.HUC8, description))
            delineator.plot_watershed(self.subbasinfile, 
                                      title = title,
                                      gages = 'calibration',
                                      width = 0.2,
                                      dams = True, 
                                      output = f, 
                                      verbose = verbose,
                                      )

    def extract_CDL(self, 
                    plots = True,
                    ):
        """
        Extract cropland data from the NASS CDL.
        """

        if all([os.path.isfile('{}/{}landuse.csv'.format(self.landusedata, y))
                for y in self.years]):
            print('CDL data for {} exist\n'.format(self.HUC8))
            return

        # make an instance of the CDLExtractor to use to get the data

        cdlextractor = CDLExtractor(self.landusedata)

        # download the CDL data for the watershed for each year

        for year in self.years:
                
            p = '{}/{}landuse.tif'.format(self.landusedata, year)
            e = '{}/NASSerror{}.html'.format(self.landusedata, year)

            # if the file has not been downloaded (or attempted), try to get it

            if os.path.isfile(e):

                print('land use data for {} are unavailable'.format(year))

            elif not os.path.isfile(p):

                try:

                    cdlextractor.download_shapefile(self.subbasinfile, year)

                except:

                    print('warning: data for {} are not available'.format(year))

            else:

                print('land use data for {} exist'.format(year))
                
        print('')

        # check to see if CDL data are available for each year

        l = self.landusedata
        self.years = [year
                      for year in self.years
                      if os.path.isfile('{}/{}landuse.tif'.format(l, year))]

        for year in self.years:

            # landuse in each subbasin for each year

            extracted = '{}/{}landuse.tif'.format(self.landusedata, year)

            # field code for the subbasin shapefile to match CDL data

            attribute = 'ComID'

            # csv file of the output

            csvfile = '{}/{}landuse.csv'.format(self.landusedata, year)

            if not os.path.isfile(csvfile):

                try:

                    cdlextractor.calculate_landuse(extracted, 
                                                   self.subbasinfile,
                                                   self.cdlaggregate, 
                                                   attribute,
                                                   csvfile = csvfile)

                    if plots:

                        # raw landuse plot

                        raw = '{}/{}raw_landuse'.format(self.landusedata, year)
                        if not os.path.isfile(raw + '.png'):
                            cdlextractor.plot_landuse(extracted, 
                                                      self.subbasinfile, 
                                                      attribute, 
                                                      self.landuse, 
                                                      output = raw, 
                                                      lw = 2.,
                                                      datatype = 'raw')

                        # aggregated land use plot

                        its = self.landusedata, year
                        results = '{}/{}aggregated_landuse'.format(*its)
                        if not os.path.isfile(results + '.png'):
                            cdlextractor.plot_landuse(extracted, 
                                                      self.subbasinfile, 
                                                      attribute, 
                                                      self.landuse,
                                                      output = results, 
                                                      datatype = 'results')

                except:

                    print('warning: unable to calculate land use for year ' +
                          '{}; check data availability'.format(year))

    def build_watershed(self, 
                        regression = True,
                        masslink = True,
                        verbose = True,
                        ):
        """
        Method to build the Watershed data structure that is the input to an
        HSPFModel using data acquired from other PyHSPF utilities. The method
        performs the following tasks:

             1. Creates instances of the Subbasin data structure from aggregated
                NHDPlus catchment data in the self.subbasinfile shapefile
             2. Adds stream reach data for the principle flowline to the 
                Subbasin data structures using aggregated NHDPlus data in the 
                self.flowline shapefile and the value-added attributes in the
                Flowline data structures located in the self.VAAfile
             3. Adds location and information on NWIS gages to the Subbasin 
                data structures in the self.gagefile shapefile 
             4. Adds location and information on NID dams to the Subbasin data
                structures in the self.damfile shapefile
             5. Adds land use data from the CDL to the Subbasin data structures
                using information from the aggregated CDL files
             6. Creates an instance of the Watershed data structure from the 
                Subbasin data structures
             7. Adds mass linkages between the individual subbasin stream 
                reaches including the locations of inlets and outlets
        """

        # create a dictionary to store subbasin data

        subbasins = {}

        # create a dictionary structure to keep track of inlets

        inlets = {}

        # create a dictionary structure to store subbasin areas

        areas = {}

        # open up the outlet file and see if the subbasin has a gage or dam

        sf = Reader(self.outletfile)

        records = sf.records()

        comid_index = sf.fields.index(['COMID',    'N',  9, 0]) - 1
        nid_index   = sf.fields.index(['NIDID',    'C',  7, 0]) - 1
        nwis_index  = sf.fields.index(['SITE_NO',  'C', 15, 0]) - 1
        flow_index  = sf.fields.index(['AVG_FLOW', 'N', 15, 3]) - 1

        # make dictionaries of subbasins with dams and gages

        nids = {'{}'.format(r[comid_index]):r[nid_index] for r in records 
                if isinstance(r[nid_index], str)}

        nwiss = {'{}'.format(r[comid_index]):r[nwis_index] 
                 for r in records 
                 if not isinstance(r[nwis_index], bytes)}

        # make a default gage with the largest flow to use for FTABLE generation

        avg_flows = {r[nwis_index]: r[flow_index]
                     for r in records
                     if not isinstance(r[nwis_index], bytes)}

        default_gage = max(avg_flows, key = avg_flows.get)

        # read in the flow plane data into an instance of the Subbasin class

        sf = Reader(self.subbasinfile, shapeType = 5)

        comid_index = sf.fields.index(['ComID',      'N',  9, 0]) - 1
        len_index   = sf.fields.index(['PlaneLenM',  'N',  8, 2]) - 1
        slope_index = sf.fields.index(['PlaneSlope', 'N',  9, 6]) - 1
        area_index  = sf.fields.index(['AreaSqKm',   'N', 10, 2]) - 1
        cx_index    = sf.fields.index(['CenX',       'N', 12, 6]) - 1
        cy_index    = sf.fields.index(['CenY',       'N', 12, 6]) - 1
        elev_index  = sf.fields.index(['AvgElevM',   'N',  8, 2]) - 1

        for record in sf.records():

            comid     = '{}'.format(record[comid_index])
            length    = record[len_index]
            slope     = record[slope_index]
            tot_area  = record[area_index]
            centroid  = [record[cx_index], record[cy_index]]
            elevation = record[elev_index]

            subbasin  = Subbasin(comid)
            subbasin.add_flowplane(length, slope, centroid, elevation)

            subbasins[comid] = subbasin
            areas[comid]     = tot_area

        # open up the flowline VAA file to use to establish mass linkages

        with open(self.VAAfile, 'rb') as f: flowlines = pickle.load(f)
            
        # create a dictionary to connect the comids to hydroseqs

        hydroseqs = {'{}'.format(flowlines[f].comid): 
                     flowlines[f].hydroseq for f in flowlines}

        # create a dictionary linking upstream and downstream reach comids

        connections = {}    
        for c in hydroseqs:
            f = flowlines[hydroseqs[c]].down
            if f in flowlines:
                connections[c] = '{}'.format(flowlines[f].comid)

        # read in the stream reach data to an instance of the Subbasin class

        sf = Reader(self.flowlinefile)

        outcomid_index   = sf.fields.index(['OutComID',   'N',  9, 0]) - 1
        gnis_index       = sf.fields.index(['GNIS_NAME',  'C', 65, 0]) - 1
        reach_index      = sf.fields.index(['REACHCODE',  'C',  8, 0]) - 1
        incomid_index    = sf.fields.index(['InletComID', 'N',  9, 0]) - 1
        maxelev_index    = sf.fields.index(['MaxElev',    'N',  9, 2]) - 1
        minelev_index    = sf.fields.index(['MinElev',    'N',  9, 2]) - 1
        slopelen_index   = sf.fields.index(['SlopeLenKM', 'N',  6, 2]) - 1
        slope_index      = sf.fields.index(['Slope',      'N',  8, 5]) - 1
        inflow_index     = sf.fields.index(['InFlowCFS',  'N',  8, 3]) - 1
        outflow_index    = sf.fields.index(['OutFlowCFS', 'N',  8, 3]) - 1
        velocity_index   = sf.fields.index(['VelFPS',     'N',  7, 4]) - 1
        traveltime_index = sf.fields.index(['TravTimeHR', 'N',  8, 2]) - 1

        for record in sf.records():

            outcomid   = '{}'.format(record[outcomid_index])
            gnis       = record[gnis_index]
            reach      = record[reach_index]
            incomid    = '{}'.format(record[incomid_index])
            maxelev    = record[maxelev_index] / 100
            minelev    = record[minelev_index] / 100
            slopelen   = record[slopelen_index]
            slope      = record[slope_index]
            inflow     = record[inflow_index]
            outflow    = record[outflow_index]
            velocity   = record[velocity_index]
            traveltime = record[traveltime_index]

            # work around for empty GNIS stream names

            if isinstance(gnis, bytes): gnis = ''

            # average in and out flow

            flow = (inflow + outflow) / 2

            subbasin = subbasins[outcomid]

            if outcomid in nwiss:

                # if the subbasin has a gage, find the measurements

                gageid = nwiss[outcomid]

                if verbose:

                    its = gageid, outcomid
                    print('using measurements from ' +
                          '{} to estimate the reach FTABLE for {}'.format(*its))

                # path to the data file

                gagefile = '{}/{}'.format(self.gagedirectory, gageid)

                # make an instance of the FtableCalculator to use to compute
                # the FTABLE for the reach

                calculator = FtableCalculator(gagefile)

                # perform a regression on the measured reach data

                calculator.calculate_regressions()

                # calculate the FTABLE use the slope length

                ftable = calculator.create_ftable(slopelen, units = 'Metric')

                # plot the results of the correlations

                its = self.gagedirectory, gageid
                regression_plot = '{}/{}regression'.format(*its)
                if regression and not os.path.isfile(regression_plot + '.png'):
                    calculator.plot_regressions(output = regression_plot)

            elif outcomid not in nids:

                if verbose:

                    print('searching for first gage downstream of ' +
                          '{}'.format(outcomid))
                
                # find the downstream gage

                comid = outcomid

                while comid not in nwiss and comid in connections:
                    comid = connections[comid]

                if comid in nwiss:
                    gageid = nwiss[comid]

                    if verbose:

                        print('found downstream gage {}'.format(nwiss[comid]))

                else:

                    gageid = default_gage
                    comid = {v:k for k,v in nwiss.items()}[gageid]

                    if verbose:

                        print('unable to find downstream gage; using default ' +
                              'gage {}'.format(gageid))

                # path to the data file

                gagefile = '{}/{}'.format(self.gagedirectory, gageid)

                # make an instance of the FtableCalculator to use to compute
                # the FTABLE for the reach

                calculator = FtableCalculator(gagefile)

                # perform a regression on the measured reach data

                calculator.calculate_regressions()

                # get the flowline VAAs for the gage comid and subbasin

                gageflow  = flowlines[hydroseqs[comid]].flow * 0.3048**3
                reachflow = outflow * 0.3048**3
            
                # extrapolate the FTABLE from the gage data

                ftable = calculator.extend_ftable(gageflow, reachflow, slopelen,
                                                  units = 'Metric')

            else:

                # it has a dam so deal with it separately

                ftable = None

            subbasin.add_reach(gnis, maxelev, minelev, slopelen, flow = flow, 
                               velocity = velocity, traveltime = traveltime,
                               ftable = ftable)
            inlets[outcomid] = incomid

        # open up the dam file and read in the information for the dams

        sf = Reader(self.damfile)

        records = sf.records()

        name_index  = sf.fields.index(['DAM_NAME',   'C', 65,   0]) - 1
        nid_index   = sf.fields.index(['NIDID',      'C', 7,    0]) - 1
        long_index  = sf.fields.index(['LONGITUDE',  'N', 19,  11]) - 1
        lat_index   = sf.fields.index(['LATITUDE',   'N', 19,  11]) - 1
        river_index = sf.fields.index(['RIVER',      'C', 65,   0]) - 1
        owner_index = sf.fields.index(['OWN_NAME',   'C', 65,   0]) - 1
        type_index  = sf.fields.index(['DAM_TYPE',   'C', 10,   0]) - 1
        purp_index  = sf.fields.index(['PURPOSES',   'C', 254,  0]) - 1
        year_index  = sf.fields.index(['YR_COMPL',   'C', 10,   0]) - 1
        high_index  = sf.fields.index(['NID_HEIGHT', 'N', 19,  11]) - 1
        mstor_index = sf.fields.index(['MAX_STOR',   'N', 19,  11]) - 1
        nstor_index = sf.fields.index(['NORMAL_STO', 'N', 19,  11]) - 1
        area_index  = sf.fields.index(['SURF_AREA',  'N', 19,  11]) - 1

        # iterate through the subbasins and see if they have a dam 

        for comid, subbasin in subbasins.items():

            if comid in nids:

                # if the subbasin has a dam, find the data in the file

                nid = nids[comid]

                r = records[[r[nid_index] for r in records].index(nid)]

                subbasin.add_dam(nid,
                                 r[name_index],
                                 r[long_index],
                                 r[lat_index],
                                 r[river_index],
                                 r[owner_index],
                                 r[type_index],
                                 r[purp_index],
                                 r[year_index],
                                 r[high_index],
                                 r[mstor_index],
                                 r[nstor_index],
                                 r[area_index],
                                 ) 

        # read in the land use data from the csv files

        for year in self.years:

            csvfile = '{}/{}landuse.csv'.format(self.landusedata, year)

            with open(csvfile, 'r') as f: 

                reader = csv.reader(f)
                rows = [r for r in reader]

            # organize the data

            comids     = [r[0] for r in rows[3:]]
            categories = rows[2][2:]
            emptys     = [r[1] for r in rows[3:]]
            data       = [r[2:] for r in rows[3:]]

            for comid, subbasin in subbasins.items():

                i = comids.index(comid)

                # use the fractions from the data file and the subbasin areas
                # to get the area for each land segment

                segment_areas = [self.make_float(s) * areas[comid] 
                                 for s in data[i]]

                # add the land segment to the model

                subbasin.add_landuse(year, 
                                     categories, 
                                     segment_areas,
                                     )

        # create an instance of the Watershed class

        watershed = Watershed(self.HUC8, subbasins)

        # establish the mass linkages using an "updown" dictionary

        updown = {}    
        for comid, subbasin in watershed.subbasins.items():

            # get the flowline instance for the outlet comid

            flowline = flowlines[hydroseqs[comid]]

            # check if the subbasin is a watershed inlet or a headwater source

            inlet = hydroseqs[inlets[comid]]

            if flowlines[inlet].up in flowlines:
                i = '{}'.format(flowlines[flowlines[inlet].up].comid)
                subbasin.add_inlet(i)
            elif flowlines[inlet].up != 0:
                watershed.add_inlet(comid)
            else: 
                watershed.add_headwater(comid)

            # check if the subbasin is a watershed outlet, and if it is not 
            # then find the downstream reach

            if flowline.down in flowlines:
                flowline = flowlines[flowline.down]
                while '{}'.format(flowline.comid) not in subbasins:
                    flowline = flowlines[flowline.down]
                updown[comid] = '{}'.format(flowline.comid)
            else: 
                updown[comid] = 0
                watershed.add_outlet('{}'.format(comid))

        # add the updown dictionary to show mass linkage in the reaches

        watershed.add_mass_linkage(updown)

        # save the Watershed data structure for later

        with open(self.watershed, 'wb') as f: pickle.dump(watershed, f)

        # make a plot of the mass linkages between subbasin reaches

        its = self.output, self.HUC8
        masslinkplot = '{}/{}/hydrography/masslink'.format(*its)
        if masslink and not os.path.isfile(masslinkplot + '.png'):
            watershed.plot_mass_flow(output = masslinkplot)

    def climate(self, 
                plotET = True, 
                verbose = True,
                ):
        """
        Method to download, aggregate, disaggregate all climate data needed
        for HSPF modeling for an 8-digit watershed. Performs the following:

             1. Downloads raw GSOD, GHCND, NSRDB, Precip3240 data
             2. Aggregates tmin, tmax, dewpoint, and wind data from GSOD
             3. Aggregates snowfall and snowdepth data from GHCND
             4. Finds GHCND stations with pan evaporation data
             5. Aggregates hourly solar radiation station data from NSRDB
             6. Aggregates the hourly solar radiation time series to daily 
             7. Calculates subbasin-specific precipitation time series using
                inverse-distance weighted averaging (IDWA)
             8. Calculates daily and hourly reference evapotranspiration
             9. Calculates hourly potential evapotranspiration for specific
                land use categories
        """

        # create an instance of the ClimateProcessor to download and aggregate
        # data for the watershed

        climateprocessor = ClimateProcessor()

        # use the subbasin shapefile to get the data

        climateprocessor.download_shapefile(self.subbasinfile, 
                                            self.start,
                                            self.end,
                                            self.climatedata,
                                            space = 0.5)

        # aggregate and save the daily GSOD tmin, tmax, dewpoint, and wind data

        if not os.path.isfile(self.tmin):
            data = climateprocessor.aggregate('GSOD', 'tmin', 
                                              self.start, self.end)
            ts = self.start, 1440, data
            with open(self.tmin, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(self.tmax):
            data = climateprocessor.aggregate('GSOD', 'tmax', 
                                              self.start, self.end)
            ts = self.start, 1440, data
            with open(self.tmax, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(self.dewpoint):
            data = climateprocessor.aggregate('GSOD', 'dewpoint', 
                                              self.start, self.end)
            ts = self.start, 1440, data
            with open(self.dewpoint, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(self.wind):
            data = climateprocessor.aggregate('GSOD', 'wind', 
                                              self.start, self.end)
            ts = self.start, 1440, data
            with open(self.wind, 'wb') as f: pickle.dump(ts, f)

        # aggregate and save the daily GHCND snowfall and snowdepth data

        if not os.path.isfile(self.snowfall):
            data = climateprocessor.aggregate('GHCND','snowfall', 
                                              self.start, self.end)
            ts = self.start, 1440, data
            with open(self.snowfall, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(self.snowdepth):
            data = climateprocessor.aggregate('GHCND','snowdepth', 
                                              self.start, self.end)
            ts = self.start, 1440, data
            with open(self.snowdepth, 'wb') as f: pickle.dump(ts, f)

        # find and save a list of stations with pan evaporation data from GHCND

        if not os.path.isfile(self.evaporation):
        
            evapstations = []
            for k, v in climateprocessor.metadata.ghcndstations.items():

                # check if the station has any evaporation data

                if v['evap'] > 0:
                
                    # open up the file and get the data

                    with open(k, 'rb') as f: station = pickle.load(f)

                    data = station.make_timeseries('evaporation', 
                                                   self.start, self.end)

                    # ignore datasets with no observations during the period

                    observations = [v for v in data if v is not None]

                    if len(observations) > 0: evapstations.append(k)

            with open(self.evaporation, 'wb') as f: pickle.dump(evapstations, f)

        else:

            with open(self.evaporation, 'rb') as f: 
                evapstations = pickle.load(f)

        # aggregate and save the hourly NSRDB metstat data

        if not os.path.isfile(self.hsolar):
            data = climateprocessor.aggregate('NSRDB', 'metstat', 
                                              self.start, self.end)
            ts = self.start, 60, data
            with open(self.hsolar, 'wb') as f: pickle.dump(ts, f)
            
        # aggregate the hourly solar to daily and save

        if not os.path.isfile(self.dsolar):

            with open(self.hsolar, 'rb') as f: t, tstep, data = pickle.load(f)

            data = [sum(data[i:i+24]) / 24 
                    for i in range(0, 24 * (self.end - self.start).days, 24)]

            ts = self.start, 1440, data
            with open(self.dsolar, 'wb') as f: pickle.dump(ts, f)

        # use the subbasin shapefile to get the location of the centroids

        sf = Reader(self.subbasinfile)

        # index of the comid, latitude, and longitude records

        comid_index = [f[0] for f in sf.fields].index('ComID') - 1
        lon_index   = [f[0] for f in sf.fields].index('CenX')  - 1
        lat_index   = [f[0] for f in sf.fields].index('CenY')  - 1
        elev_index  = [f[0] for f in sf.fields].index('AvgElevM') - 1
        area_index  = [f[0] for f in sf.fields].index('AreaSqKm') - 1

        # iterate through the shapefile records and aggregate the timeseries

        for i in range(len(sf.records())):

            record = sf.record(i)
            comid  = record[comid_index]
            lon    = record[lon_index]
            lat    = record[lat_index]

            # check if the aggregated time series exists or calculate it

            subbasinprecip = '{}/{}'.format(self.precip, comid)
            if not os.path.isfile(subbasinprecip):

                if verbose:
                    its = comid, lon, lat
                    print('aggregating timeseries for comid ' +
                          '{} at {}, {}\n'.format(*its))

                # aggregate the hourly precipitation time series for each 
                # subbasin using IDWA

                data = climateprocessor.aggregate('precip3240', 
                                                  'precip', 
                                                  self.start, 
                                                  self.end,
                                                  method = 'IDWA', 
                                                  longitude = lon,
                                                  latitude = lat)

                # convert from in to mm

                data = [d * 25.4 for d in data]

                # save for later

                ts = self.start, 60, data
                with open(subbasinprecip, 'wb') as f: pickle.dump(ts, f)

        # create an instance of the ETCalculator to calculate ET time series

        etcalculator = ETCalculator()

        # get the centroid of the watershed from the subbasin shapefile

        areas = [r[area_index] for r in sf.records()]
        xs    = [r[lon_index]  for r in sf.records()]
        ys    = [r[lat_index]  for r in sf.records()]
        zs    = [r[elev_index] for r in sf.records()]

        # get the areal-weighted averages

        lon  = sum([a * x for a, x in zip(areas, xs)]) / sum(areas)
        lat  = sum([a * y for a, y in zip(areas, ys)]) / sum(areas)
        elev = sum([a * z for a, z in zip(areas, zs)]) / sum(areas)

        # add them to the ETCalculator

        etcalculator.add_location(lon, lat, elev)

        # check if the daily RET exists; otherwise calculate it

        if not os.path.isfile(self.dRET):

            # add the daily time series to the calculator

            with open(self.tmin, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('tmin', tstep, t, data)
            
            with open(self.tmax, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('tmax', tstep, t, data)

            with open(self.dewpoint, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('dewpoint', tstep, t, data)

            with open(self.wind, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('wind', tstep, t, data)

            with open(self.dsolar, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('solar', tstep, t, data)

            # calculate the daily RET

            etcalculator.penman_daily(self.start, self.end)

            ts = self.start, 1440, etcalculator.daily['RET'][1]

            with open(self.dRET, 'wb') as f: pickle.dump(ts, f)

        # disaggregate the daily temperature time series to hourly

        if not os.path.isfile(self.htemp):
                
            if etcalculator.daily['tmin'] is None:

                with open(self.tmin, 'rb') as f: t, tstep, data = pickle.load(f)

                etcalculator.add_timeseries('tmin', tstep, t, data)

            if etcalculator.daily['tmax'] is None:

                with open(self.tmax, 'rb') as f: t, tstep, data = pickle.load(f)

                etcalculator.add_timeseries('tmax', tstep, t, data)

            data  = etcalculator.interpolate_temperatures(self.start, self.end)
            tstep = 60
            ts    = t, tstep, data

            with open(self.htemp, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('temperature', tstep, t, data)

        # disaggregate the dewpoint and wind speed time series to hourly

        if not os.path.isfile(self.hdewpoint):

            if etcalculator.daily['dewpoint'] is None:

                with open(self.dewpoint, 'rb') as f: 
                    t, tstep, data = pickle.load(f)

            else:

                t, data = etcalculator.daily['dewpoint']
                
            tstep = 60
            data  = [v for v in data for i in range(24)]
            ts    = t, tstep, data 
            
            with open(self.hdewpoint, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('dewpoint', tstep, t, data)

        if not os.path.isfile(self.hwind):

            if etcalculator.daily['wind'] is None:
                
                with open(self.wind, 'rb') as f: t, tstep, data = pickle.load(f)

            else:

                t, data = etcalculator.daily['wind']

            tstep = 60
            data  = [v for v in data for i in range(24)]
            ts    = t, tstep, data 
            
            with open(self.hwind, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('wind', tstep, t, data)

        # check if the hourly RET exists; otherwise calculate it

        if not os.path.isfile(self.hRET):

            required = 'temperature', 'solar', 'dewpoint', 'wind'

            for tstype in required:

                if etcalculator.hourly[tstype] is None:

                    name = '{}/{}'.format(self.hourly, tstype)
                    with open(name, 'rb') as f: 
                        t, tstep, data = pickle.load(f)
                    etcalculator.add_timeseries(tstype, tstep, t, data)

            # calculate and save the hourly RET

            etcalculator.penman_hourly(self.start, self.end)

            ts = self.start, 60, etcalculator.hourly['RET'][1]

            with open(self.hRET, 'wb') as f: pickle.dump(ts, f)

            # add the daily time series for the plot

            required = 'tmin', 'tmax', 'dewpoint', 'wind', 'solar'

            for tstype in required:

                if etcalculator.daily[tstype] is None:

                    name = '{}/{}'.format(self.daily, tstype)
                    with open(name, 'rb') as f: 
                        t, tstep, data = pickle.load(f)
                    etcalculator.add_timeseries(tstype, tstep, t, data)

            # aggregate the hourly to daily for plotting

            RET = etcalculator.hourly['RET'][1]

            data = [sum(RET[i:i+24]) for i in range(0, len(RET), 24)]

            etcalculator.add_timeseries('RET', 'daily', self.start, data)

            if plotET:

                name = '{}/referenceET'.format(self.evapotranspiration)
                etcalculator.plotET(stations = evapstations, output = name)
            
                name = '{}/dayofyearET'.format(self.evapotranspiration)
                etcalculator.plotdayofyear(stations = evapstations, 
                                           output = name) 

        if not os.path.isfile(self.hPETs):

            # store the PET time series in a dictionary structure

            PETs = {}

            # add the hourly RET time series if it isn't present

            if etcalculator.hourly['RET'] is None:

                with open(self.hRET, 'rb') as f: t, tstep, data = pickle.load(f)
                etcalculator.add_timeseries('RET', tstep, t, data)

            # read the land use evapotranspiration data file

            with open(self.landuse, 'r') as f:
        
                reader = csv.DictReader(f)

                for row in reader:

                    crop      = row['HSPF Category']
                    red       = int(row['Red']) / 255
                    green     = int(row['Green']) / 255
                    blue      = int(row['Blue']) / 255
                    day       = int(row['Plant Day'])
                    month     = int(row['Plant Month'])                   
                    emergence = int(row['Emergence Days'])
                    growth    = int(row['Growth Days'])
                    full      = int(row['Full Days'])
                    late      = int(row['Late Days'])
                    Ki        = float(row['Initial Crop Coefficient'])
                    Km        = float(row['Mid Season Crop Coefficient'])
                    Kl        = float(row['Late Season Crop Coefficient'])
        
                    if crop != 'Empty':

                        plant = datetime.datetime(2001, month, day)
                        color = red, green, blue

                        # add the information and calculate the PET time series

                        etcalculator.add_crop(crop, 
                                              plant, 
                                              emergence, 
                                              growth, 
                                              full, 
                                              late, 
                                              Ki,
                                              Km, 
                                              Kl,
                                              ) 

                        etcalculator.hourly_PET(crop, self.start, self.end)

                        # get the PET time series
    
                        t, PET = etcalculator.hourlyPETs[crop]
                        ts = t, 60, PET

                        PETs[crop] = ts

            # save it

            with open(self.hPETs, 'wb') as f: pickle.dump(PETs, f)
