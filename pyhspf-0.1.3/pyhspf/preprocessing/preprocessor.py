#!/usr/bin/env python3
#
###############################################################################
#                                                                             #
# Python HSPF Pre-processor                                                   #
#                                                                             #
# David J. Lampert, PhD, PE                                                   #
#                                                                             #
# last updated: 08/16/2013                                                    #
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
# The Prinicipal Aquifers of the United States shapefile                      #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/aquifers_us.xml              #
#                                                                             #
# The USGS NWIS gage station shapefile available at                           #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgages.xml              #
#                                                                             #
# The time series data for the climate are downloaded automatically from the  #
# National Climate Data Center (NCDC) and the National Solar Radiation        #
# Database (NSRDB)                                                            #
#                                                                             #
###############################################################################

def preprocessor(network, output, HUC8, state, RPU, start, end,
                 landcodes = 'aggregate.csv', drainmin = 0, drainmax = 400, 
                 extra_outlets = None,
                 overwrite = False, verbose = True, vverbose = False, 
                 parallel = True, extract = True, subdivide = True, 
                 subbasins = True, landuse = True, landstats = True, 
                 build = True, climate = True, gagedata = True,
                 subbasinplots = False, watershedplots = True, landplots = True,
                 landpercents = False, flowplots = True, metstatplots = True,
                 metgageplots = True):
    """Preprocess the data for HSPF."""

    import os, time, datetime

    from pyhspf.preprocessing.extract_gis_data    import extract_gis_data
    from pyhspf.preprocessing.subdivide_watershed import subdivide_watershed
    from pyhspf.preprocessing.make_subbasins      import make_subbasins
    from pyhspf.preprocessing.calculate_landuse   import calculate_landuse
    from pyhspf.preprocessing.landuse_stats       import landuse_stats
    from pyhspf.preprocessing.build_watershed     import build_watershed
    from pyhspf.preprocessing.make_gagestations   import make_gagestations
    from pyhspf.preprocessing.download_climate    import download_climate
    from pyhspf.preprocessing.extract_climate     import extract_climate

    go = time.time()

    # source data locations on the network

    NHDPlus = network + '/NHDPlus/NHDPlusMS/NHDPlus07'
    NED     = NHDPlus + '/NEDSnapshot'
    NWIS    = network + '/NWIS/USGS_Streamgages-NHD_Locations'
    NASS    = network + '/NASS'
    NID     = network + '/NID/dams00x020'
    AQUI    = network + '/Aquifers/aquifrp025'

    # file paths

    subbasinfile = output + '/%s/%ssubbasins'        % (HUC8, HUC8)
    flowfile     = output + '/%s/%sflowlines'        % (HUC8, HUC8)
    outletfile   = output + '/%s/%ssubbasin_outlets' % (HUC8, HUC8)
    gagefile     = output + '/%s/%sgagestations'     % (HUC8, HUC8)
    damfile      = output + '/%s/%sdams'             % (HUC8, HUC8)
    landfile     = output + '/%s/subbasinlanduse'    %  HUC8

    years = [year for year in range(start, end)]

    # extract the data for the HUC8 from the sources

    if extract:
        extract_gis_data(NHDPlus, NED, NWIS, NASS, NID, AQUI, RPU, HUC8, state, 
                         years, outputpath = output, parallel = parallel, 
                         overwrite = overwrite, verbose = verbose,
                         vverbose = vverbose)

    # subdivide the watershed to meet the subbasin criteria

    if subdivide:
        subdivide_watershed(HUC8, flowfile, gagefile, damfile, 
                            drainmin = drainmin, 
                            drainmax = drainmax, extra_outlets = extra_outlets,
                            outputpath = output, verbose = verbose, 
                            vverbose = vverbose) 

    # make the subbasins from the outlets (time-consuming)

    if subbasins:
        make_subbasins(HUC8, output, subbasin_plots = subbasinplots, 
                       watershed_plots = watershedplots, parallel = 
                       parallel, verbose = verbose, vverbose = vverbose)

    # calculate the landuse statistics for each year and each subbasin

    if landuse:
        calculate_landuse(output, HUC8, years, landcodes, overwrite = 
                          overwrite, parallel = parallel, verbose = verbose)

    # tabulate the land use statistics

    if landpercents:
        landusepercent = output + '/%s/%slandpercent.csv' % (HUC8, HUC8)
        landuse_stats(output, HUC8, landcodes, subbasinfile, landusepercent,
                      units = 'percent', overwrite = overwrite,
                      verbose = verbose, vverbose = vverbose)

    if landstats:
        landusekm = output + '/%s/%slanduse.csv'     % (HUC8, HUC8)
        landuse_stats(output, HUC8, landcodes, subbasinfile, landusekm, 
                      picklefile = landfile, units = 'sqkm', 
                      overwrite = overwrite, plots = landplots, 
                      verbose = verbose, vverbose = vverbose) 

    # build the watershed object

    if build:
        flowlinefile = output + '/%s/%ssubbasin_flowlines' % (HUC8, HUC8)
        VAAfile  = output + '/%s/flowlineVAAs'    % HUC8
        build_watershed(subbasinfile, flowlinefile, outletfile, damfile, 
                        gagefile, landfile, landcodes, VAAfile, 
                        years, HUC8, output, plots = flowplots)

    # extract gage station data

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end, 1, 1)

    if gagedata:
        make_gagestations(output, HUC8, state, s, e, verbose = verbose)

    # extract climate data

    if climate:
        download_climate(output, HUC8, s, e)
        extract_climate(output, HUC8, s, e)

    # make a directory for HSPF calculations

    hspfdirectory = '{}/{}/hspf'.format(output, HUC8)
    if not os.path.isdir(hspfdirectory): os.mkdir(hspfdirectory)

    if verbose: 
        print('\ncompleted preprocessing watershed in %.1f seconds\n' % 
              (time.time() - go))

