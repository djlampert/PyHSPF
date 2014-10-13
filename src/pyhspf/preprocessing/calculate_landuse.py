# Landuse calculator
#
# David J. Lampert, PhD, PE
#
# last updated: 12/04/2012
#
# Calculates the land use data from a raster file within each of the shapes
# in a shapefile.

import os, csv, pickle

from shapefile       import Reader
from numpy           import array, unique, argwhere
from multiprocessing import Process

from .rasterutils import get_raster_in_poly

def is_number(s):
    try: float(s) 
    except ValueError: return False
    return True

def yearly_landuse(rasterfile, shapefile, filepath, overwrite = False, 
                   verbose = True):
    """Parses through the rasterfile and determines the number of pixels in 
    each shape in the shapefile, and then creates a dictionary linking the
    "comid" attribute to the unique pixel values and their corresponding areas 
    for each shape in the shapefile.
    """

    if os.path.isfile(filepath) and not overwrite:
        if verbose: print('file {} exists'.format(filepath))
        return

    if verbose: print('calculating land use statistics')

    # open the subbasin shapefiles

    sf = Reader(shapefile, shapeType = 5)

    comid_index = sf.fields.index(['ComID',      'N',  9, 0]) - 1
    area_index  = sf.fields.index(['AreaSqKm',   'N', 10, 2]) - 1

    subbasins = {}
    for i in range(len(sf.records())):

        # get the record info and geometry for the subbasin

        points = array(sf.shape(i).points)
        record = sf.record(i)

        comid    = '{}'.format(record[comid_index])
        tot_area = record[area_index]

        if verbose: print('calculating land use data for {}'.format(comid))
 
        # read all the pixels in the area

        values, origin = get_raster_in_poly(rasterfile, points, verbose = False)
        values = values.flatten()
        values = values[values.nonzero()]

        tot_pixels = len(values)

        # count the number of pixels of each land use type

        landcodes = []
        areas     = []
        for v in unique(values):
            pixels = len(values[argwhere(values == v)])
            area   = tot_area * pixels / tot_pixels

            landcodes.append(v)
            areas.append(area)

        subbasins['{}'.format(comid)] = landcodes, areas

    # pickle the data into a file for later

    with open(filepath, 'wb') as f: pickle.dump(subbasins, f)

def make_landuse_csv(codes, areas, landtypes, heading, output, method = 
                     'append', verbose = False):
    """Writes the land use data from a year to a csv file."""

    # check if the file exists and whether to append

    if not os.path.isfile(output): values = []

    elif method == 'append':

        # if it's an existing file and don't want to replace, then need to 
        # read the data

        with open(output, 'r') as f: 
            rows = [row for row in csv.reader(f)]
            values = [row[2:] for row in rows[2:]]
        
    elif method == 'overwrite':
        if verbose: print('warning: %s exists, overwriting' % output)
        values = []

    else: 
        if verbose: print('unknown method "%s" specified' % method)
        return

    # set up the csv file by columns and then transpose it

    landcodes = list(landtypes.keys())
    landcodes.sort()
    
    columns = [['Number'] + landcodes]

    columns.append(['Land Use Type'])
    for code in landcodes:
        columns[-1].append(landtypes[code])

    # add any existing values

    for column in zip(*values): columns.append(list(column))

    # use the new landtypes and areas to fill out the last column, leaving 
    # blank strings as needed

    new = ['%s' % heading]
    for code in landcodes:
        if code in codes:
            new.append('%.4f' % areas[codes.index(code)])
        else: new.append('')

    columns.append(new)

        # sort the columns and get rid of any repetition

    for column in columns:
        while columns.count(column) > 1: columns.remove(column)
    columns[2:].sort()

    # write the csv file

    with open(output, 'w', newline = '') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Subbasin Land Use Areas (km\u00B2)'])
        writer.writerow([''])

        for row in zip(*columns): writer.writerow(row)

def calculate_landuse(directory, HUC8, years, codefile, overwrite = False, 
                      parallel = False, verbose = True, vverbose = False):
    """Calculates the land use statstics in each subbbasin."""

    if verbose: print('calculating land use data for %s\n' % HUC8)

    # open up the subbasin list

    subbasinfile = directory  + '/%s/%ssubbasins' % (HUC8, HUC8)

    sf = Reader(subbasinfile, shapeType = 5)

    comid_index = sf.fields.index(['ComID', 'N',  9, 0]) - 1

    comids = ['{}'.format(r[comid_index]) for r in sf.records()]

    # set up a dictionary to map the raster values to land use types based 
    # on the code file

    with open(codefile, 'r') as f: codes = [row for row in csv.reader(f)]

    landtypes = {int(row[0]): row[1] for row in codes[1:]}

    # make a new directory to store the land use pickle files

    if not os.path.isdir(directory + '/%s/landuse' % HUC8):
        os.mkdir(directory + '/%s/landuse' % HUC8)

    # iterate through each year and calculate the land use statistics, which
    # are dumped into binary files for later analysis

    if parallel:
        processes = []
        for year in years:
            landfile = directory + '/%s/%scropland%d.tif' % (HUC8, HUC8, year)
            outfile  = directory + '/%s/landuse/%dlanduse' % (HUC8, year)

            if (os.path.isfile(landfile) and 
                (overwrite or not os.path.isfile(outfile))):

                if verbose: print('calculating land use data for %d' % year)

                processes.append(Process(target = yearly_landuse,
                                         args   = (landfile, subbasinfile, 
                                                   outfile),
                                         kwargs = {'overwrite': overwrite,
                                                   'verbose':   vverbose}))

        for p in processes: p.start()
        for p in processes: p.join()

    else:
        for year in years:
            landfile = directory + '/%s/%scropland%d.tif'  % (HUC8, HUC8, year)
            outfile  = directory + '/%s/landuse/%dlanduse' % (HUC8, year)

            if (os.path.isfile(landfile) and 
                (overwrite or not os.path.isfile(outfile))):

                if verbose: print('calculating land use data for %d' % year)

                yearly_landuse(landfile, subbasinfile, outfile,
                               overwrite = overwrite, verbose = vverbose)

    # go through the landuse for each year and puts the data into csv
    # files in each subbasin directory, starting by overwriting the old file

    d = '{}/{}/landuse'.format(directory, HUC8)
    year = min(years)
    while not os.path.isfile('{}/{}landuse'.format(d, year)): year += 1

    for comid in comids:

        landfile = '{}/{}landuse'.format(d, year)

        with open(landfile, 'rb') as f: subbasin_data = pickle.load(f)

        output = '{}/{}/{}/landuse.csv'.format(directory, HUC8, comid)

        # get the unique codes and respective areas for the subbasin

        codes, areas = subbasin_data[comid]

        if os.path.isfile(output) and not overwrite:
            if vverbose: print('%s exists, returning' % output)
        else:
            if verbose: 
                print('aggregating land use data for subbasin {}'.format(comid))

            make_landuse_csv(codes, areas, landtypes, year, output, 
                             method = 'overwrite', verbose = vverbose)

            for y in years:

                landfile = '{}/{}landuse'.format(d, y)

                if os.path.isfile(landfile) and y > year:
                    with open(landfile, 'rb') as f: 
                        subbasin_data = pickle.load(f)

                    codes, areas = subbasin_data[comid]

                    make_landuse_csv(codes, areas, landtypes, y, output,
                                     verbose = vverbose)

    if verbose: print('')
