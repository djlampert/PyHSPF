import os, shutil

from shapefile import Reader, Writer

def extract_catchments(catchmentfile, comids, output = None, verbose = True):
    """Iterates through a catchment shapefile for a basin and makes a new
    shapefile containing the catchments with the comids from the "comids" list.
    """

    if output is None: output = os.getcwd() + '/catchments'

    # start by copying the projection files

    shutil.copy(catchmentfile + '.prj', output + '.prj')
  
    shapefile = Reader(catchmentfile, shapeType = 5)
    records   = shapefile.records()

    # figure out which field code is the comid

    feature_index = shapefile.fields.index(['FEATUREID', 'N', 9,  0]) - 1

    # go through the reach indices, add add them to the list of flowlines if
    # they are in the watershed, and make a list of the corresponding comids

    if verbose: print('searching for catchments\n')

    indices = []
   
    i = 0
    for record in records:
        if record[feature_index] in comids:
            indices.append(i)
        i+=1

    # write the data from the HUC8 to a new shapefile

    w = Writer(shapeType = 5)

    for field in shapefile.fields:  w.field(*field)

    for i in indices:
        shape  = shapefile.shape(i)
        w.poly(shapeType = 5, parts = [shape.points])
        w.record(*records[i])

    w.save(output)

    if verbose: print('successfully extracted catchments\n')
