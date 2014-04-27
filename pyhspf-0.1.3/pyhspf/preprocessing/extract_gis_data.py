# GIS data extractor                                                   
#                                                                             
# David J. Lampert, PhD, PE                                                   
#                                                                             
# last updated: 12/03/2012                                                    
#                                                                              
# Purpose: Extracts GIS data from sources and builds the input file for HSPF  
# for a given set of assumptions for a USGS 8-digit Watershed. Details below. 
#                                                                             
# Paths to source data directories are all available online. Note that these  
# can be large so you may have organize as I did below.                       
#                                                                             
# National Hydrography Dataset Plus (NHDPlus); should use directory structure 
# specified in the source at http://www.horizon-systems.com/nhdplus/          
#                                                                             
# The National Elevation Dataset (NED) on a 30-m grid (supplied with NHDPlus) 
#                                                                             
# The USGS NWIS gage station shapefile available at                           
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgages.xml              
#                                                                             
# The land use geotiff file(s); for this case the National Agricultural       
# Statistics Service (NASS).  Can be function of time.                        
                                                                             
import os, time

from multiprocessing import Process

from pyhspf.preprocessing.extract_NHDPlus       import extract_NHDPlus
from pyhspf.preprocessing.extract_elevations    import extract_elevations
from pyhspf.preprocessing.extract_cropland      import extract_cropland
from pyhspf.preprocessing.extract_gage_stations import extract_gage_stations
from pyhspf.preprocessing.extract_aquifers      import extract_aquifers
from pyhspf.preprocessing.extract_dams          import extract_dams

def extract_gis_data(NHDPlus, NED, NWIS, NASS, NID, AQUI, RPU, HUC8, state,
                     years, outputpath = None, parallel = False, 
                     overwrite = False, verbose = True, vverbose = True):
    """Extracts data from the NHDPlus database and the USGS NWIS gage file
    for an 8-digit watershed from the source files.  

    NHDPlus -- The path to the National Hydrography Dataset Plus, the data are
               available online at http://www.horizon-systems.com/nhdplus/
    NED     -- The path to the National Elevation Dataset, included with NHDPlus
    NWIS    -- The path to USGS National Water Information System shapefile,
               the data are available online at 
               http://maps.waterdata.usgs.gov/mapper/index.html
    NASS    -- The path to the National Agricultural Statistics Service dataset,
               available online at http://www.nass.usda.gov/
    RPU     -- The 
    HUC8    -- The 8-digit watershed hydrologic unit code
    """

    start = time.time()

    if verbose: print('\nextracting GIS data for HUC %s\n' % HUC8)

    if outputpath is None: output = os.getcwd()
    else:                  output = outputpath

    # set up the pointers to the files

    if   RPU   == '7a': elevfile, utm = NED + '/Ned07a/elev_cm', 16
    elif RPU   == '7b': elevfile, utm = NED + '/Ned07b/elev_cm', 15

    flowlines    = output + '/%s/%sflowlines'      % (HUC8, HUC8)
    catchments   = output + '/%s/%scatchments'     % (HUC8, HUC8)
    flowlineVAAs = output + '/%s/flowlineVAAs'     %  HUC8
    gagestations = output + '/%s/%sgagestations'   % (HUC8, HUC8)
    dem          = output + '/%s/%selevations.tif' % (HUC8, HUC8)
    dams         = output + '/%s/%sdams'           % (HUC8, HUC8)
    aquifers     = output + '/%s/%saquifers'       % (HUC8, HUC8)

    # create a directory for the output

    if not os.path.isdir(output + '/%s' % HUC8): os.mkdir(output + '/%s' % HUC8)

    # extract the flowline and catchment shapefiles from NHDPlus (skips this if
    # files already exist); otherwise read the comids from the shapefiles

    if (not os.path.isfile(flowlines + '.shp') or
        not os.path.isfile(catchments + '.shp') or
        not os.path.isfile(flowlineVAAs)):

        if verbose: print('creating flowline and catchment shapefiles\n')
        extract_NHDPlus(NHDPlus, HUC8, flowlines, catchments, flowlineVAAs,
                        verbose = verbose, vverbose = vverbose)

    elif verbose: print('flow and catchment files exist\n')
         
    # extract the gage stations for the watershed from the USGS shapefile

    if not os.path.isfile(gagestations + '.shp'):

        if verbose: print('creating gage station shapefile\n')
        extract_gage_stations(NHDPlus, NWIS, HUC8, gagestations, 
                              verbose = vverbose)

    elif verbose: print('gage file exists\n')

    # extract the dam locations for the watershed from the NID shapefile

    if not os.path.isfile(dams + '.shp'):

        if verbose: print('creating dam shapefile\n')
        extract_dams(output, NID, HUC8, dams, verbose = vverbose)

    elif verbose: print('dam file exists\n')

    # extract the aquifers from the source

    if not os.path.isfile(aquifers + '.shp'):

        if verbose: print('aquifer shapefile needs to be created\n')
        extract_aquifers(output, HUC8, AQUI, verbose = vverbose)

    elif verbose: print('aquifer shapefile exists\n')

   # extract the elevations from the NED

    if not os.path.isfile(dem):

        if verbose: print('elevation raster needs to be created\n')
        extract_elevations(elevfile, HUC8, flowlines, dem, verbose = vverbose)

    elif verbose: print('elevation raster exists\n')

    # extract the cropland data from NASS

    if parallel:
        processes = []
        for year in years:
            if 2000 <= year:
                if year in [2000, 2001, 2002, 2003, 2004, 2005, 2010, 2011]: 
                    landuse = (NASS + '/nass_%s/cdl_30m_r_%s_%d_utm%d.tif' %
                               (state, state, year, utm)) 
                elif year in [2006, 2007, 2008, 2009]:
                    landuse = (NASS + '/nass_%s/cdl_56m_r_%s_%d_utm%d.tif' %
                               (state, state, year, utm)) 

                crop = output + '/%s/%scropland%d.tif' % (HUC8, HUC8, year)

                if not os.path.isfile(crop):

                    if verbose: 
                        print('cropland raster for ' +
                              '{} needs to be created'.format(year))

                    processes.append(Process(target = extract_cropland,
                                             args = (landuse, flowlines,
                                                     HUC8, year, crop),
                                             kwargs = {'verbose': vverbose}))

                elif verbose: print('cropland raster exists')

            elif verbose: print('year {} unavailable from NASS'.format(year))
        if verbose: print('')

        for p in processes: p.start()
        for p in processes: p.join()
        processes = None

    else:
        for year in years:
            if year in [2000, 2001, 2002, 2003, 2004, 2005, 2010, 2011]: 
                landuse = (NASS + '/nass_%s/cdl_30m_r_%s_%d_utm%d.tif' %
                           (state, state, year, utm))

            elif year in [2006, 2007, 2008, 2009]:
                landuse = (NASS + '/nass_%s/cdl_56m_r_%s_%d_utm%d.tif' %
                           (state, state, year, utm)) 
            elif verbose: 
                print('year %d unavailable from NASS, using 2000\n' % year)

            crop = output + '/%s/%scropland%d.tif' % (HUC8, HUC8, year)

            if not os.path.isfile(crop):

                if verbose: 
                    print('cropland raster for %d needs to be created\n' % year)

                extract_cropland(landuse, flowlines, HUC8, year, crop, 
                                 verbose = vverbose)

            elif verbose: print('cropland raster exists\n')
