import os, shutil

from shapefile import Reader, Writer

def combine_subbasins(directory, HUC8, comids, verbose = True):
    """Combines outlet subbasins for an 8-digit hydrologic unit into a single 
    shapefile.  Assumes directory structure of:

    directory\HUC8\comids\combined.shp 
    
    where comids are all the elements in a list of the subbasin outlets from 
    the NHDPlus dataset.
    """

    output = directory + '/%s/%ssubbasins' % (HUC8, HUC8)

    if verbose: 
        print('trying to combine subbasin shapefiles into a single file\n')

    w = Writer(shapeType = 5)

    projection = None
    fields     = None

    for comid in comids:
        filename = directory + '/%s/%d/combined' % (HUC8, comid)
        if os.path.isfile(filename + '.shp'):

            # start by copying the projection files

            if projection is None:
                projection = (directory + '/%s/%ssubbasins.prj' % 
                                        (HUC8, HUC8))
                shutil.copy(filename + '.prj', projection)

            # read the new file
  
            r = Reader(filename, shapeType=5)

            if fields is None:
                fields = r.fields
                for field in fields: w.field(*field)

            shape = r.shape(0)

            # write the shape and record to the new file

            w.poly(shapeType = 5, parts = [shape.points])
            record = r.record(0)
            w.record(*record)

        elif verbose: print('unable to locate %s' % filename)

    if fields is not None: 
        w.save(output)
        if verbose: print('successfully combined subbasin shapefiles\n')
    elif verbose: print('unable to combine subbasins\n')

