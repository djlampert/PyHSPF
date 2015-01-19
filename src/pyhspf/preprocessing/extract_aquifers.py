import shutil, os, time

from shapefile import Reader, Writer
from .dbfutils import read_dbf

def get_boundaries(shapes, space = 0.1):
    """Gets the boundaries for the plot."""

    boundaries = shapes[0].bbox
    for shape in shapes[0:]:
        b = shape.bbox
        if b[0] < boundaries[0]: boundaries[0] = b[0]
        if b[1] < boundaries[1]: boundaries[1] = b[1]
        if b[2] > boundaries[2]: boundaries[2] = b[2]
        if b[3] > boundaries[3]: boundaries[3] = b[3]

    xmin = boundaries[0] - (boundaries[2] - boundaries[0]) * space
    ymin = boundaries[1] - (boundaries[3] - boundaries[1]) * space
    xmax = boundaries[2] + (boundaries[2] - boundaries[0]) * space
    ymax = boundaries[3] + (boundaries[3] - boundaries[1]) * space

    return xmin, ymin, xmax, ymax

def inside_box(p1, p2, p3, space = 0):
    """Checks if p3 is inside a box formed by p1 and p2."""

    if p1[0] < p3[0] and p3[0] < p2[0] or p1[0] > p3[0] and p3[0] > p2[0]:

        # x value is inside

        if p1[1] < p3[1] and p3[1] < p2[1] or p1[1] > p3[1] and p3[1] > p2[1]:
            
            # y value is inside

            return True

        else: return False

    else: return False

def extract_aquifers(directory, HUC8, aquifers, pad = 0.2, verbose = True):
    """Extracts aquifers from the source datafile to the destination using
    the HUC8 boundaries for the query."""

    start = time.time()

    # open up the HUC8 boundary shapefile and use it to get the bounding box

    shapefile = Reader(directory + '/%s/%scatchments' % (HUC8, HUC8))

    xmin, ymin, xmax, ymax = get_boundaries(shapefile.shapes())

    # convert to bounding corners for testing

    p1 = [xmin - pad * (xmax - xmin), ymin - pad * (ymax - ymin)]
    p2 = [xmax + pad * (xmax - xmin), ymax + pad * (ymax - ymin)]

    shapefile = None

    # start by copying the projection files

    if verbose: print('\ncopying the projections\n')

    shutil.copy(directory + '/%s/%scatchments.prj' % (HUC8, HUC8), 
                directory + '/%s/%saquifers.prj' % (HUC8, HUC8))

    # open the flowline file
    
    if verbose: print('reading the aquifer file\n')
    
    shapefile = Reader(aquifers, shapeType = 5)

    # work around for issues with pyshp

    records   = []
    for i in range(len(shapefile.shapes())):
        try: records.append(shapefile.record(i))
        except: records.append('')
     
    # use the bounding boxes to see if the shapes are within the watershed area

    if verbose: print('searching for aquifers in the watershed\n')

    bboxes = [shapefile.shape(i).bbox for i in range(len(records))]

    corners = [[[b[0], b[1]], [b[0], b[3]], [b[2], b[1]], [b[2], b[3]]]
               for b in bboxes]

    indices = [i for i, c in zip(range(len(corners)), corners) if 
               any([inside_box(p1, p2, p) for p in c]) or 
               all([inside_box(c[0], c[3], p1), inside_box(c[0], c[3], p2)])]

    # remove any non aquifers

    indices = [i for i in indices if shapefile.record(i)[4] != 999]

    # find a record for the non aquifer

    i = 0
    while shapefile.record(i)[4] != 999: i+=1

    nonrecord = shapefile.record(i)
    nonrecord[1] = nonrecord[1].decode('utf-8')
    nonrecord[5] = 0
    nonrecord[6] = 0

    if len(indices) == 0:
        if verbose: print('query returned no values, returning\n')
        return
    
    # write the data from the HUC8 to a new shapefile
    
    w = Writer(shapeType = 5)
    
    for field in shapefile.fields:  w.field(*field)
    
    for i in indices:
        shape = shapefile.shape(i)

        # check for multiple parts

        if len(shape.parts) > 1:
            parts = [shape.points[i:j] 
                     for i, j in zip(shape.parts[:-1], shape.parts[1:])]
        else: parts = [shape.points]

        record = records[i]
    
        # little work around for blank binary values
    
        if isinstance(record[1], bytes):
            record[1] = record[1].decode('utf-8')

        w.poly(shapeType = 5, parts = parts)
        w.record(*record)

    # add a shape for the bounding box showing no aquifer locations

    part = [p1, [p1[0], p2[1]], p2, [p2[0], p1[1]]]

    w.poly(shapeType = 5, parts = [part])
    w.record(*nonrecord)
    
    w.save(directory + '/%s/%saquifers' % (HUC8, HUC8))
    
    end = time.time()
    
    if verbose: 
        print('successfully queried data in %.2f seconds\n' % (end - start))


#directory = r'C:\HSPF_data'
#HUC8      = '07080106'
#aquifers  = r'Z:\Aquifers\aquifrp025'
#extract_aquifer(directory, HUC8, aquifers, pad = 0.2, verbose = True)

#from plot_routines import plot_aquifers

#plot_aquifers(directory, HUC8, subbasins = True)
