###############################################################################
#                                                                             #
# Hydrological Simulation Program in Fortran (HSPF) Preprocessor              #
#                                                                             #
# David J. Lampert                                                            #
#                                                                             #
# last updated: 05/20/2015                                                    #
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

import os, time, pickle, datetime, csv, math

from matplotlib import pyplot, patches
from shapefile  import Reader

# local imports

from .nhdplusextractor import NHDPlusExtractor
from .nwisextractor    import NWISExtractor
from .nidextractor     import NIDExtractor
from .delineators      import HUC8Delineator
from .cdlextractor     import CDLExtractor
from .climateprocessor import ClimateProcessor
from .etcalculator     import ETCalculator
from  pyhspf.core      import HSPFModel, Watershed, Subbasin

class Preprocessor:
    """
    A class that integrates the various preprocessing tools together into a 
    single class to build a baseline HSPFModel for an 8-digit Hydrologic Unit
    Code (HUC8).
    """

    def __init__(self,
                 network      = None, 
                 output       = None, 
                 HUC8         = None,
                 state        = None,
                 start        = None,
                 end          = None,
                 cdlaggregate = None,
                 landuse      = None,
                 ):
        """
        Constructor method; sets up pointers to directory locations.
        """

        self.network      = network
        self.output       = output
        self.HUC8         = HUC8
        self.state        = state
        self.start        = start
        self.end          = end
        self.cdlaggregate = cdlaggregate
        self.landuse      = landuse

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
                       state        = None,
                       start        = None,
                       end          = None,
                       cdlaggregate = None,
                       landuse      = None,
                       ):
        """
        Sets simulation-specific parameters.
        """

        if HUC8         is not None:  self.HUC8         = HUC8
        if state        is not None:  self.state        = state
        if start        is not None:  self.start        = start
        if end          is not None:  self.end          = end
        if cdlaggregate is not None:  self.cdlaggregate = cdlaggregate
        if landuse      is not None:  self.landuse      = landuse

    def preprocess(self,
                   hydrography      = None,
                   gagedirectory    = None,
                   landusedata      = None,
                   climatedata      = None,
                   hspfdirectory    = None,
                   drainmax         = 400, 
                   extra_outlets    = None,
                   masslink         = True,
                   plotET           = True,
                   hydrographyplot  = True,
                   preliminary      = True,
                   delineated       = True,
                   landuseplots     = True,
                   verbose          = True, 
                   vverbose         = False, 
                   parallel         = True,
                   ):
        """
        Preprocesses the data for HSPF.
        """

        # check the network is mounted on Unix-like systems

        if os.name != 'nt':

            if not os.path.ismount(self.network):
                print('\nerror: network ' +
                      '{} does not seem to be mounted\n'.format(self.network))
                raise

        # check that all the required information has been supplied

        required = {self.network:      'network directory location', 
                    self.output:       'output directory location', 
                    self.HUC8:         '8-digit hydrologic unit code', 
                    self.state:        '2-character state abbreviation', 
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
        output = '{}/{}'.format(*its)
        if not os.path.isdir(output): os.mkdir(output)

        # make a subdirectory for hydrography data

        if hydrography is None:
            self.hydrography = '{}/{}/hydrography'.format(*its)
        else:
            self.hydrography = hydrography

        if not os.path.isdir(self.hydrography): os.mkdir(self.hydrography)

        # make a subdirectory for NWIS flow and water quality data

        if gagedirectory is None:
            self.gagedirectory = '{}/{}/NWIS'.format(*its)
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

    def extract_hydrography(self, plot = True):
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

    def extract_CDL(self, plots = True):
        """
        Extract cropland data from the NASS CDL.
        """

        if all([os.path.isfile('{}/{}landuse.csv'.format(self.landusedata, y))
                for y in self.years]):
            print('CDL data for {} exist\n'.format(self.HUC8))
            return

        cdlextractor = CDLExtractor(self.CDL)

        # download the data for each state for each year

        cdlextractor.download_data(self.state.upper(), self.years)
        print('')

        # extract the CDL data for the watershed using the boundary shapefile

        cdlextractor.extract_shapefile(self.subbasinfile, self.landusedata)
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

    def build_watershed(self, masslink = True):
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

        # create a dictionary to keep track of any subbasins that are inlets

        inlets = {}

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

            subbasin = subbasins[outcomid]

            flow = (inflow + outflow) / 2
            subbasin.add_reach(gnis, maxelev, minelev, slopelen, flow = flow, 
                               velocity = velocity, traveltime = traveltime)
            inlets[outcomid] = incomid

        # open up the outlet file and see if the subbasin has a gage or dam

        sf = Reader(self.outletfile)

        records = sf.records()

        comid_index = sf.fields.index(['COMID',   'N',  9, 0]) - 1
        nid_index   = sf.fields.index(['NIDID',   'C',  7, 0]) - 1
        nwis_index  = sf.fields.index(['SITE_NO', 'C', 15, 0]) - 1

        nids = {'{}'.format(r[comid_index]):r[nid_index] for r in records 
                if isinstance(r[nid_index], str)}

        nwiss = {'{}'.format(r[comid_index]):r[nwis_index] 
                 for r in records 
                 if r[nwis_index] is not None}

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

                # if the subbasin has a dam, find the data info in the file

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

                subbasin.add_landuse(year, categories, data[i])

        # create an instance of the Watershed class

        watershed = Watershed(self.HUC8, subbasins)

        # open up the flowline VAA file to use to establish mass linkages

        with open(self.VAAfile, 'rb') as f: flowlines = pickle.load(f)
            
        # create a dictionary to connect the comids to hydroseqs

        hydroseqs = {'{}'.format(flowlines[f].comid): 
                     flowlines[f].hydroseq for f in flowlines}

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
            self.plot_mass_flow(watershed, masslinkplot)

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

    def plot_mass_flow(self,
                       watershed, 
                       output, 
                       title = 'Subbasin Reach Mass Flow Diagram',
                       fontsize = 6, 
                       theight = 0.2, 
                       l = 8.5, 
                       w = 11, 
                       verbose = True, 
                       ):
        """
        Makes a schematic of the mass linkages between the various subbasins
        in a watershed.
        """

        if os.path.exists(output):
            if verbose: print('mass link plot {} exists'.format(output))
            return
        elif verbose: print('generating a mass linkage plot\n')

        fontheight = fontsize / 72.
        rheight = 3 * fontheight
        rwidth  = 12 * fontheight
        xgap = fontheight
        ygap = rheight
        awidth = rheight / 4
        aheight = rheight / 3

        # set up a sheet to write the image

        fig = pyplot.figure(figsize = (w, l))

        ax  = fig.add_subplot(111, aspect = 'equal')
        ax.get_xaxis().set_visible(False)
        ax.get_yaxis().set_visible(False)
        t = ax.set_title(title)

        # divide the subbasins into rows and put them on the chart
        # start at the bottom to organize the linkages better

        rows = [watershed.outlets, ['outlet']]

        top = False
        while not top:
            row = []
            for next in rows[0]:
                for subbasin in watershed.updown:
                    if watershed.updown[subbasin] == next: row.append(subbasin)
            if len(row) > 0: 
                rows.insert(0, row)
            else: 
                top = True

        # add an inlet box in the row above each inlet

        for inlet in watershed.inlets: 

            i = 0
            while i < len(rows) - 1:

                for subbasin in rows[i]:

                    if subbasin == inlet:
                    
                        # find the position of the subbasin in the chart

                        j = rows[i].index(inlet)

                        if i > 0:

                            # figure out where the subbasins point
                        
                            updowns = [watershed.updown[s] for s in rows[i-1]]
                            
                            # if first or last, add it there in the row above

                            if   j == 0:                
                                rows[i-1].insert(0, 'inlet')
                            elif j == len(rows[i]) - 1: 
                                rows[i-1].append('inlet')
                            else:

                                # find the place to add in the preceeding row 

                                n = updowns.index(rows[i][j-1]) + 1
                                rows[i-1].insert(n, 'inlet')

                i += 1

        # write the subbasin boxes to the chart

        middle = math.ceil(w // (rwidth + xgap)) // 2
        last = 0

        # keep track of the bounding box of the plot

        xmin, ymin, xmax, ymax = middle, 0, middle, 0

        for i in range(len(rows)):

            row = rows[i]
        
            y = (ygap + rheight) * i + theight

            # figure out which cell to put in the main column

            if i == 0:
                main = row[(len(row) - 1) // 2]
            elif i < len(rows) - 1:
                main = watershed.updown[rows[i-1][last]]
            else: main = 'outlet'

            start = middle - row.index(main)

            if i < len(rows) - 1: next_row = rows[i + 1]

            for subbasin in row:
                x = (rwidth + xgap) * (start + row.index(subbasin))
                r = patches.Rectangle((x, y), rwidth, rheight, fill = False)

                # adjust the bounding box

                if x           < xmin: xmin = x
                if x + rwidth  > xmax: xmax = x + rwidth
                if y           < ymin: ymin = y
                if y + rheight > ymax: ymax = y + rheight

                if subbasin != 'outlet': ax.add_patch(r)

                b = ax.text(x + rwidth / 2, y + rheight / 2, subbasin,
                            horizontalalignment = 'center',
                            verticalalignment   = 'center')

                # draw the arrow

                if i < len(rows) - 1:

                    x1 = x + rwidth / 2

                    if i < len(rows) - 2 and subbasin != 'inlet':
                        next = watershed.updown[subbasin]
                        next_start = (middle - 
                                      next_row.index(watershed.updown[main]))
                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(next))
                              + rwidth / 2)

                    elif subbasin == 'inlet':
                        next = watershed.inlets[0]
                        next_start = (middle - 
                                      next_row.index(watershed.updown[main]))

                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(next))
                              + rwidth / 2)

                    else:
                        next_start = middle
                        x2 = ((rwidth + xgap) * (middle) + rwidth / 2)

                    a = pyplot.arrow(x1, y + rheight, x2 - x1, ygap, 
                                     head_width = awidth, head_length = aheight,
                                     fc = 'k', ec = 'k', 
                                     length_includes_head = True)
                    ax.add_patch(a)

            last = row.index(main)
            i += 1
        
        pad = 0.02

        xmin = xmin - (xmax - xmin) * pad
        xmax = xmax + (xmax - xmin) * pad
        ymin = ymin - (ymax - ymin) * pad
        ymax = ymax + (ymax - ymin) * pad

        ax.set_xlim(xmin, xmax)
        ax.set_ylim(ymax, ymin)
        pyplot.axis('off')
        pyplot.savefig(output, dpi = 200)

        pyplot.clf()
        pyplot.close()
