###############################################################################
#                                                                             #
# Python Upper Mississippi River Basin HSPF Preprocessor                      #
#                                                                             #
# David J. Lampert, PhD, PE                                                   #
#                                                                             #
# last updated: 10/09/2014                                                    #
#                                                                             #
# Purpose: Extracts GIS data from sources and builds the input file for HSPF  #
# for a given set of assumptions for a USGS 8-digit Watershed. Details below. #
#                                                                             #
###############################################################################
#
###############################################################################
#                                                                             #
# Paths to source data directories are all available online. Note that these  #
# can be large so you may have organize as I did below.                       #
#                                                                             #
# National Hydrography Dataset Plus (NHDPlus); should use directory structure #
# specified in the source at http://www.horizon-systems.com/nhdplus/          #
#                                                                             #
# The National Elevation Dataset (NED) on a 30-m grid (supplied with NHDPlus) #
#                                                                             #
# The land use geotiff file(s); for this case the National Agricultural       #
# Statistics Service (NASS).  Can be function of time.                        #
#                                                                             #
# The National Inventory of Dams (NID) shapefile available at                 #
# http://nationalatlas.gov/metadata/dams00x020.faq.html                       #
#                                                                             #
# The USGS NWIS gage station shapefile available at                           #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgages.xml              #
#                                                                             #
# The time series data for the climate are downloaded automatically from the  #
# National Climate Data Center (NCDC) and the National Solar Radiation        #
# Database (NSRDB)                                                            #
#                                                                             #
###############################################################################

import os, time, pickle, datetime

from shapefile import Reader

# local imports

from .nhdplusextractor import NHDPlusExtractor
from .nwisextractor    import NWISExtractor
from .nidextractor     import NIDExtractor
from .delineators      import HUC8Delineator
from .cdlextractor     import CDLExtractor
from .build_watershed  import build_watershed
from .climateprocessor import ClimateProcessor
from .etcalculator     import ETCalculator   

def preprocess(network, 
               output, 
               HUC8, 
               state, 
               start, 
               end,
               landcodes = 'aggregate.csv', 
               drainmin = 0, 
               drainmax = 400, 
               extra_outlets  = None,
               overwrite      = False, 
               verbose        = True, 
               vverbose       = False, 
               parallel       = True, 
               extract        = True, 
               delineate      = True,
               landuse        = True, 
               landstats      = True, 
               build          = True, 
               climate        = True, 
               gagedata       = True,
               subbasinplots  = False, 
               watershedplots = True, 
               landplots      = True,
               landpercents   = False, 
               flowplots      = True, 
               metstatplots   = True,
               metgageplots   = True,
               ):
    """Preprocess the data for HSPF."""

    go = time.time()

    # source data locations on the network

    NHDPlus  = '{}/NHDPlus'.format(network)
    NED      = '{}/NEDSnapshot'.format(NHDPlus)
    NWIS     = '{}/NWIS'.format(network)
    CDL      = '{}/CDL'.format(network)
    NID      = '{}/NID'.format(network)

    # file paths

    gagepath     = '{0}/{1}/NWIS'.format(output, HUC8)
    subbasinfile = '{0}/{1}/subbasin_catchments'.format(output, HUC8)
    inletfile    = '{0}/{1}/subbasin_inlets'.format(output, HUC8)
    outletfile   = '{0}/{1}/subbasin_outlets'.format(output, HUC8)
    gagefile     = '{0}/{1}/gagestations'.format(output, HUC8)
    damfile      = '{0}/{1}/dams'.format(output, HUC8)
    landusedata  = '{0}/{1}/landuse'.format(output, HUC8)
    landfile     = '{0}/{1}/subbasinlanduse'.format(output, HUC8)
    VAAfile      = '{0}/{1}/flowlineVAAs'.format(output, HUC8)
    bfile        = '{0}/{1}/boundary'.format(output, HUC8)
    cfile        = '{0}/{1}/catchments'.format(output, HUC8)
    flowfile     = '{0}/{1}/flowlines'.format(output, HUC8)
    elevfile     = '{0}/{1}/elevations'.format(output, HUC8)

    years = [year for year in range(start, end)]

    # extract the data for the HUC8 from the sources

    if extract:

        # create an instance of the extractor

        nhdplusextractor = NHDPlusExtractor(HUC8[:2], NHDPlus)

        # extract the HUC8 data for the Patuxent watershed

        nhdplusextractor.extract_HUC8(HUC8, output)

        # extract the gage data from NWIS

        nwisextractor = NWISExtractor(NWIS)

        # extract the gage stations into a new shapefile for the HUC8 and place
        # them into the newly generated directory for the HUC8

        nwisextractor.extract_HUC8(HUC8, '{}/{}'.format(output, HUC8))

        # download all the gage data to the gagepath directory

        nwisextractor.download_all(datetime.datetime(start, 1, 1),
                                   datetime.datetime(end, 1, 1), 
                                   output = gagepath)

        # extract the dam data from the NID

        nidextractor = NIDExtractor(NID)

        # extract dams in the HUC8 using the shapefile for the boundary

        nidextractor.extract_shapefile(bfile, damfile)

    if delineate:

        # create an instance of the UMRBDelineator to delineate the watershed

        delineator = HUC8Delineator(HUC8, 
                                    VAAfile, 
                                    flowfile, 
                                    cfile, 
                                    elevfile,
                                    gagefile, 
                                    damfile
                                    )

        # delineate the watershed using the NHDPlus data and delineator

        delineator.delineate(output, 
                             parallel      = parallel,
                             drainmax      = drainmax, 
                             extra_outlets = extra_outlets,
                             verbose       = verbose,
                             vverbose      = vverbose,
                             )

    if landuse:

        # extract cropland data from NASS

        cdlextractor = CDLExtractor(CDL)

        # download the data for each state for each year

        cdlextractor.download_data(state.upper(), years)

        # make a directory for the CDL files

        if not os.path.isdir(landusedata): os.mkdir(landusedata)

        # extract the data for the watershed using the boundary shapefile

        if not any([os.path.isfile('{}/{}landuse.tif'.format(landusedata, year))
                    for year in years]):
                                   
            cdlextractor.extract_shapefile(bfile, landusedata)

        # calculate the landuse in each subbasin for each year

        for year in years:

            attribute = 'ComID'
            extracted = '{}/{}landuse.tif'.format(landusedata, year)

            # csv file of the output

            csvfile = '{}/{}landuse.csv'.format(landusedata, year)
            if not os.path.isfile(csvfile):
                cdlextractor.calculate_landuse(extracted, subbasinfile, 
                                               landcodes, attribute,
                                               csvfile = csvfile)

            # raw landuse plot

            raw = '{}/{}raw'.format(landusedata, year)
            if not os.path.isfile(raw + '.png'):
                cdlextractor.plot_landuse(extracted, subbasinfile, attribute, 
                                          output = raw, lw = 2.,
                                          datatype = 'raw')

            # aggregated land use plot

            results = '{}/{}results'.format(landusedata, year)
            if not os.path.isfile(results + '.png'):
                cdlextractor.plot_landuse(extracted, subbasinfile, attribute, 
                                          output = results, 
                                          datatype = 'results')

        print('')

    # build the watershed object

    if build:

        if not os.path.isfile('{}/{}/watershed'.format(output, HUC8)):
            flowlinefile = '{}/{}/subbasin_flowlines'.format(output, HUC8)
            VAAfile      = '{}/{}/flowlineVAAs'.format(output, HUC8)
            build_watershed(subbasinfile, flowlinefile, outletfile, damfile, 
                            gagefile, landusedata, VAAfile, 
                            years, HUC8, output, plots = flowplots)

    # extract gage station data

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end, 1, 1)

    # download and extract the climate data

    if climate:

        climatedata = '{}/{}/climate'.format(output, HUC8)

        if not os.path.isdir(climatedata): os.mkdir(climatedata)
        climateprocessor = ClimateProcessor()
        climateprocessor.download_shapefile(subbasinfile, s, e, climatedata,
                                            space = 0.5)

        # make directories for hourly and daily aggregated timeseries

        hourly = '{}/hourly'.format(climatedata)
        daily  = '{}/daily'.format(climatedata)

        if not os.path.isdir(hourly): os.mkdir(hourly)
        if not os.path.isdir(daily):  os.mkdir(daily)

        # aggregate the daily GSOD tmin, tmax, dewpoint, and wind data

        tmin = '{}/tmin'.format(daily)
        tmax = '{}/tmax'.format(daily)
        dewt = '{}/dewpoint'.format(daily)
        wind = '{}/wind'.format(daily)

        if not os.path.isfile(tmin):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'tmin', s, e)
            with open(tmin, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(tmax):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'tmax', s, e)
            with open(tmax, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(dewt):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'dewpoint', s, e)
            with open(dewt, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(wind):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'wind', s, e)
            with open(wind, 'wb') as f: pickle.dump(ts, f)

        # aggregate the daily GHCND snowfall and snowdepth data

        snowfall  = '{}/snowfall'.format(daily)
        snowdepth = '{}/snowdepth'.format(daily)

        if not os.path.isfile(snowfall):
            ts = s, 1440, climateprocessor.aggregate('GHCND', 'snowfall', s, e)
            with open(snowfall, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(snowdepth):
            ts = s, 1440, climateprocessor.aggregate('GHCND', 'snowdepth', s, e)
            with open(snowdepth, 'wb') as f: pickle.dump(ts, f)

        # find stations with pan evaporation data from GHCND

        evapstations = []
        for k, v in climateprocessor.metadata.ghcndstations.items():

            # check if the station has any evaporation data

            if v['evap'] > 0:
                
                # open up the file and get the data

                with open(k, 'rb') as f: station = pickle.load(f)

                data = station.make_timeseries('evaporation', s, e)

                # ignore datasets with no observations during the period

                observations = [v for v in data if v is not None]

                if len(observations) > 0: evapstations.append(k)

        # aggregate the hourly NSRDB metstat data

        hsolar = '{}/solar'.format(hourly)
        if not os.path.isfile(hsolar):
            ts = s, 60, climateprocessor.aggregate('NSRDB', 'metstat', s, e)
            with open(hsolar, 'wb') as f: pickle.dump(ts, f)
            
        # aggregate the hourly solar to daily

        dsolar = '{}/solar'.format(daily)
        if not os.path.isfile(dsolar):

            with open(hsolar, 'rb') as f: t, tstep, data = pickle.load(f)
            ts = s, 1440, [sum(data[i:i+24]) / 24 
                           for i in range(0, 24 * (e-s).days, 24)]

            with open(dsolar, 'wb') as f: pickle.dump(ts, f)

        # aggregate the hourly precipitation for each subbasin using IDWA

        precip = '{}/hourlyprecipitation'.format(climatedata)
        if not os.path.isdir(precip): os.mkdir(precip)

        # use the subbasin shapefile to get the location of the centroids

        sf = Reader(subbasinfile)

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

            # check if the aggregated time series exists and then calculate it

            subbasinprecip = '{}/{}'.format(precip, comid)
            if not os.path.isfile(subbasinprecip):

                if verbose:
                    i = comid, lon, lat
                    print('aggregating timeseries for comid ' +
                          '{} at {}, {}\n'.format(*i))

                p = climateprocessor.aggregate('precip3240', 'precip', s, e,
                                               method = 'IDWA', longitude = lon,
                                               latitude = lat)

                ts = s, 60, p
                with open(subbasinprecip, 'wb') as f: pickle.dump(ts, f)

        # make a directory for the evapotranspiration time series

        evapotranspiration = '{}/evapotranspiration'.format(climatedata)
        if not os.path.isdir(evapotranspiration): os.mkdir(evapotranspiration)

        # use the ETCalculator to calculate the ET time series

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

        dRET = '{}/dailyRET'.format(evapotranspiration)
        if not os.path.isfile(dRET):

            # add the daily time series to the calculator

            with open(tmin, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('tmin', tstep, t, data)

            with open(tmax, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('tmax', tstep, t, data)

            with open(dewt, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('dewpoint', tstep, t, data)

            with open(wind, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('wind', tstep, t, data)

            with open(dsolar, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('solar', tstep, t, data)

            # calculate the daily RET

            etcalculator.penman_daily(s, e)

            ts = s, 1440, etcalculator.daily['RET'][1]

            with open(dRET, 'wb') as f: pickle.dump(ts, f)

        # disaggregate the daily temperature time series to hourly

        hourlytemp = '{}/temperature'.format(hourly)
        if not os.path.isfile(hourlytemp):
                
            if etcalculator.daily['tmin'] is None:

                with open(tmin, 'rb') as f: t, tstep, data = pickle.load(f)

                etcalculator.add_timeseries('tmin', tstep, t, data)

            if etcalculator.daily['tmax'] is None:

                with open(tmax, 'rb') as f: t, tstep, data = pickle.load(f)

                etcalculator.add_timeseries('tmax', tstep, t, data)

            data  = etcalculator.interpolate_temperatures(s, e)
            tstep = 60
            ts    = t, tstep, data

            with open(hourlytemp, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('temperature', tstep, t, data)

        # disaggregate the dewpoint and wind speed time series to hourly

        hourlydewt = '{}/dewpoint'.format(hourly)        
        if not os.path.isfile(hourlydewt):

            if etcalculator.daily['dewpoint'] is None:

                with open(dewt, 'rb') as f: t, tstep, data = pickle.load(f)

            else:

                t, data = etcalculator.daily['dewpoint']
                
            tstep = 60
            data  = [v for v in data for i in range(24)]
            ts    = t, tstep, data 
            
            with open(hourlydewt, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('dewpoint', tstep, t, data)

        hourlywind = '{}/wind'.format(hourly)        
        if not os.path.isfile(hourlywind):

            if etcalculator.daily['wind'] is None:

                with open(wind, 'rb') as f: t, tstep, data = pickle.load(f)

            else:

                t, data = etcalculator.daily['wind']

            tstep = 60
            data  = [v for v in data for i in range(24)]
            ts    = t, tstep, data 
            
            with open(hourlywind, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('wind', tstep, t, data)

        # check if the hourly RET exists; otherwise calculate it

        hRET = '{}/hourlyRET'.format(evapotranspiration)
        if not os.path.isfile(hRET):

            required = 'temperature', 'solar', 'dewpoint', 'wind'

            for tstype in required:

                if etcalculator.hourly[tstype] is None:

                    name = '{}/{}'.format(hourly, tstype)
                    with open(name, 'rb') as f: t, tstep, data = pickle.load(f)
                    etcalculator.add_timeseries(tstype, tstep, t, data)

            # calculate and save the hourly RET

            etcalculator.penman_hourly(s, e)

            ts = s, 60, etcalculator.hourly['RET'][1]

            with open(hRET, 'wb') as f: pickle.dump(ts, f)

            # add the daily time series for the plot

            required = 'tmin', 'tmax', 'dewpoint', 'wind', 'solar'

            for tstype in required:

                if etcalculator.daily[tstype] is None:

                    name = '{}/{}'.format(daily, tstype)
                    with open(name, 'rb') as f: t, tstep, data = pickle.load(f)
                    etcalculator.add_timeseries(tstype, tstep, t, data)

            # aggregate the hourly to daily for plotting

            hRET = etcalculator.hourly['RET'][1]

            dRET = [sum(hRET[i:i+24]) for i in range(0, len(hRET), 24)]

            etcalculator.add_timeseries('RET', 'daily', s, dRET)

            name = '{}/referenceET'.format(evapotranspiration)
            etcalculator.plotET(stations = evapstations, output = name, 
                                show = False)
            
            name = '{}/dayofyearET'.format(evapotranspiration)

            etcalculator.plotdayofyear(stations = evapstations, output = name, 
                                       show = False)

        # calculate hourly PET for different land use categories

        lucs = ('corn', 
                'soybeans', 
                'grains', 
                'alfalfa', 
                'fallow',
                'pasture', 
                'wetlands', 
                'others',
                )

        colors  = ('yellow',  
                   'green',        
                   'brown',    
                   'lime',   
                   'gray', 
                   'orange',      
                   'blue', 
                   'black',
                   )

        pdates = (datetime.datetime(2000, 4, 15),
                  datetime.datetime(2000, 5, 15),
                  datetime.datetime(2000, 4, 15),
                  datetime.datetime(2000, 5, 15),
                  datetime.datetime(2000, 3,  1),
                  datetime.datetime(2000, 3,  1),
                  datetime.datetime(2000, 3,  1),
                  datetime.datetime(2000, 3,  1),
                  )

        ems = (30,
               20,
               20,
               10,
               10,
               10,
               10,
               10,
               )

        gs = (50,
              30,
              30,
              10,
              10,
              10,
              10,
              10,
              )

        fs = (60,
              60,
              60,
              120,
              240,
              240,
              240,
              240,
              )

        ls = (40,
              30,
              40,
              10,
              10,
              10,
              10,
              10,
              )

        Kis = (0.30,
               0.40,
               0.30,
               0.30,
               0.30,
               0.30,
               1.00,
               1.00,
               )

        Kms = (1.15,
               1.15,
               1.15,
               0.95,
               0.30,
               0.85,
               1.20,
               1.00,
               )

        Kls = (0.40,
               0.55,
               0.40,
               0.90,
               0.30,
               0.30,
               1.00,
               1.00,
               )

        # add the hourly RET time series if it isn't present

        if etcalculator.hourly['RET'] is None:

            with open(hRET, 'rb') as f: t, tstep, data = pickle.load(f)
            etcalculator.add_timeseries('RET', tstep, t, data)

        # iterate through the land use categories and calculate PET time series

        for i in zip(lucs, colors, pdates, ems, gs, fs, ls, Kis, Kms, Kls):

            crop, color, plant, emergence, growth, full, late, Ki, Km, Kl = i

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

            etcalculator.hourly_PET(crop, s, e)

            # get the PET time series
    
            t, PET = etcalculator.hourlyPETs[crop]
            ts = t, 60, PET

            # save it

            name = '{}/{}'.format(evapotranspiration, crop)
            with open(name, 'wb') as f: pickle.dump(ts, f)

    # make a directory for HSPF calculations

    hspfdirectory = '{}/{}/hspf'.format(output, HUC8)
    if not os.path.isdir(hspfdirectory): os.mkdir(hspfdirectory)

    if verbose: 
        print('completed preprocessing watershed in %.1f seconds\n' % 
              (time.time() - go))

