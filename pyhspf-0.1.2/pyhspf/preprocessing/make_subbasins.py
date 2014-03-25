# Make Subbasins
#                                                                             
# David J. Lampert, PhD, PE                                                   
#                                                                             
# last updated: 12/03/2012                                                    
#                                                                              
# Purpose: Combines catchments from NHDPlus together into subbasins and 
# performs GIS calculations for parameters needed by HSPF.
                                                                             
import os, time, pickle

from multiprocessing import Process

from pyhspf.preprocessing.extract_catchments import extract_catchments
from pyhspf.preprocessing.combine_catchments import combine_catchments
from pyhspf.preprocessing.combine_subbasins  import combine_subbasins
from pyhspf.preprocessing.merge_shapes       import merge_shapes
from pyhspf.preprocessing.gisplots           import plot_watershed
from pyhspf.preprocessing.gisplots           import plot_subbasin
from pyhspf.preprocessing.gisplots           import plot_aquifers

def make_subbasins(HUC8, directory, subbasin_plots = True, watershed_plots = 
                   True, aquifer_plots = True, parallel = False, 
                   verbose = True, vverbose = False, form = 'png'):
    """Extracts catchments from the watershed catchment file for each subbasin
    and then combines the catchments together and calculates slope parameters.
    """

    start = time.time()

    # paths to input/output files

    flowfile      = directory + '/%s/%sflowlines'      % (HUC8, HUC8)
    catchmentfile = directory + '/%s/%scatchments'     % (HUC8, HUC8)
    dem           = directory + '/%s/%selevations.tif' % (HUC8, HUC8)
    subbasinfile  = directory + '/%s/%ssubbasins'      % (HUC8, HUC8)
    boundaries    = directory + '/%s/%sboundaries'     % (HUC8, HUC8)
    comidfile     = directory + '/%s/subbasincomids'   %  HUC8

    # image paths

    imagepath     = directory + '/%s/images'        %  HUC8
    preliminary   = imagepath + '/%spreliminary.%s' % (HUC8, form)
    delineated    = imagepath + '/%sdelineated.%s'  % (HUC8, form)
    aquifers      = imagepath + '/%saquifers.%s'    % (HUC8, form)

    # open up the subbasin dictionaries

    with open(comidfile, 'rb') as f: subbasins = pickle.load(f)

    # set up an output folder for images

    if not os.path.isdir(imagepath): os.mkdir(imagepath)

    # divide the catchment shapefile into subbasin catchment shapefiles 

    if parallel:
        processes = []
        for subbasin in subbasins:
            catchments = directory + '/%s/%d/catchments' % (HUC8, subbasin)
            names = subbasins[subbasin]
            if not os.path.isfile(catchments + '.shp'):
                processes.append(Process(target = extract_catchments,
                                         args = (catchmentfile, names),
                                         kwargs = {'output':  catchments,
                                                   'verbose': vverbose}))
        for p in processes: p.start()
        for p in processes: p.join()
        processes = None

    else:
        for subbasin in subbasins:
            catchments = directory + '/%s/%d/catchments' % (HUC8, subbasin)
            if not os.path.isfile(catchments + '.shp'):
                extract_catchments(catchmentfile, subbasins[subbasin], 
                                   output = catchments, verbose =vverbose)

    # combine the catchments in each subbasin into a single shapefile

    if parallel and not os.path.isfile(subbasinfile + '.shp'):

        if verbose: print('attempting to combine subbasin catchments ' +
                          'in parallel, this may take a while...\n')

        processes = []
        for subbasin in subbasins:
            catchments = directory + '/%s/%d/catchments' % (HUC8, subbasin)
            flowlines  = directory + '/%s/%d/flowlines'  % (HUC8, subbasin)
            combined   = directory + '/%s/%d/combined'   % (HUC8, subbasin)
            if not os.path.isfile(combined + '.shp'):
                processes.append(Process(target = combine_catchments, 
                                         args = (catchments, flowlines, dem, 
                                                 subbasin),
                                         kwargs = {'output': combined,
                                                   'verbose': vverbose}))
        for p in processes: p.start()
        for p in processes: p.join()
        processes = None

        if verbose: print('successfully combined catchments in parallel in ' +
                          '%.1f seconds \n' % (time.time() - start))

    elif not os.path.isfile(subbasinfile + '.shp'):
        for n, subbasin in zip(range(len(subbasins)), subbasins):
            catchments = directory + '/%s/%d/catchments' % (HUC8, subbasin)
            flowlines  = directory + '/%s/%d/flowlines'  % (HUC8, subbasin)
            combined   = directory + '/%s/%d/combined'   % (HUC8, subbasin)
            if not os.path.isfile(combined + '.shp'):
                try:
                    combine_catchments(catchments, flowlines, dem, subbasin, 
                                       output = combined, verbose = vverbose)
                    if verbose: 
                        print('successfully combined catchments in ' +
                              'subbasin %d\n' % subbasin)
                except:
                    if verbose: 
                        print('warning: unable to combine catchments ' + 
                              'in %s\n' % path)

    for subbasin in subbasins:
        prelim = directory + '/%s/%d/preliminary.tif' % (HUC8, subbasin)
        post   = directory + '/%s/%d/combined.tif'    % (HUC8, subbasin)
        if subbasin_plots:
            if not os.path.isfile(prelim):
                try: plot_subbasin(directory, HUC8, subbasin, raster = 
                                   'elevation', catchments = True, flowlines = 
                                   True, outlets = False, output = prelim,
                                   verbose = vverbose)
                except: print('warning: failed to generate preliminary plot\n')

            if not os.path.isfile(post):
                try: plot_subbasin(directory, HUC8, subbasin, raster = 
                                   'elevation', catchments = False, flowlines =
                                   False, outlets = True, output = post,
                                   verbose = vverbose)
                except: print('warning: failed to generate combined plot\n')

    # put together the combined subbasins into a single file

    if not os.path.isfile(subbasinfile + '.shp'):
        combine_subbasins(directory, HUC8, subbasins, verbose = verbose)

    if not os.path.isfile(boundaries + '.shp'):
        merge_shapes(subbasinfile, outputfile = boundaries, verbose = verbose,
                     vverbose = vverbose)

    if watershed_plots:
        if not os.path.isfile(preliminary):
            plot_watershed(directory, HUC8, raster = 'elevation', dams = True,
                           output = preliminary,
                           verbose = verbose)
            #except: print('unable to make preliminary plot for %s\n' % HUC8)
        if not os.path.exists(delineated):
            plot_watershed(directory, HUC8, raster = 'elevation', 
                           gages = 'calibration', catchments = False, 
                           flowlines = False, dams = True, output = delineated, 
                           verbose = verbose)
            #except: print('unable to make delineated plot for %s\n' % HUC8)

    if aquifer_plots:
        if not os.path.isfile(aquifers):
            plot_aquifers(directory, HUC8, subbasins = True, 
                          output = aquifers, verbose = verbose)
            #except: print('unable to plot aquifer locations for %s\n' % HUC8)

    elapsed = time.time() - start
    if verbose: 
        print('completed subbasin GIS calculations in %.1f seconds\n' % elapsed)
