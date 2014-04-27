import os, shutil, numpy, time, math

from shapefile import Reader, Writer

from pyhspf.preprocessing.raster       import get_raster, get_raster_on_poly
from pyhspf.preprocessing.raster       import get_raster_in_poly
from pyhspf.preprocessing.merge_shapes import format_shape, combine_shapes

def get_distance(p1, p2):
    """Approximates the distance in kilometers between two points on the 
    Earth's surface designated in decimal degrees using an ellipsoidal 
    projection. per CFR 73.208 it is applicable for up to 475 kilometers.
    p1 and p2 are listed as (longitude, latitude).
    """

    deg_rad = math.pi / 180

    dphi = p1[1] - p2[1]
    phim = 0.5 * (p1[1] + p2[1])
    dlam = p1[0] - p2[0]

    k1 = (111.13209 - 0.56605 * math.cos(2 * phim * deg_rad) + 0.00120 * 
          math.cos(4 * phim * deg_rad))
    k2 = (111.41513 * math.cos(phim * deg_rad) - 0.09455 * 
          math.cos(3 *phim * deg_rad) + 0.0012 * math.cos(5 * phim * deg_rad))

    return numpy.sqrt(k1**2 * dphi**2 + k2**2 * dlam**2)

def get_distance_vector(catchpoints, closest):
    """Vectorized version of get_distance method (for computational efficiency).
    """

    deg_rad = math.pi / 180
          
    dphis = catchpoints[:, 1] - closest[:, 1]
    phims = 0.5 * (catchpoints[:, 1] + closest[:, 1])
    dlams = catchpoints[:,0] - closest[:,0]

    k1s = (111.13209 - 0.56605 * numpy.cos(2 * phims * deg_rad) + 
           0.00120 * numpy.cos(4 * phims * deg_rad))
    k2s = (111.41513 * numpy.cos(phims * deg_rad) - 0.09455 * 
           numpy.cos(3 * phims * deg_rad) + 0.0012 * 
           numpy.cos(5 * phims * deg_rad))
    
    return numpy.sqrt(k1s**2 * dphis**2 + k2s**2 * dlams**2)

def get_overland(p1, p2, tolerance = 0.1, min_slope = 0.00001):
    """Returns the slope of the z-coordinate in the x-y plane between points
    p1 and p2.  Returns the min_slope if the points are too close together
    as specified by the tolerance (km).  Also return half the average length
    from the catchment boundary to the flowline (since the average length 
    across each line is half the total length)."""

    L = get_distance(p1, p2)

    if L > tolerance: return L / 2., (p1[2] - p2[2]) / L / 100000
    else:             return tolerance, min_slope

def get_overland_vector(catchpoints, closest, tol = 0.1, min_slope = 0.00001):
    """Vectorized version of the get_overland function (for computational
    efficiency)."""

    length = get_distance_vector(catchpoints, closest)
    slope  = (catchpoints[:,2] - closest[:,2]) / length / 100000

    for l, s in zip(length, slope):
        if l < tol: l, s = tol, min_slope

    return length / 2., slope

def get_centroid(points):
    """Calculates the centroid of a polygon with paired values of xs and ys."""

    xs, ys = points[:, 0], points[:, 1]

    a = xs[:-1] * ys[1:]
    b = ys[:-1] * xs[1:]

    A = numpy.sum(a - b) / 2.

    cx = xs[:-1] + xs[1:]
    cy = ys[:-1] + ys[1:]

    Cx = numpy.sum(cx * (a - b)) / (6. * A)
    Cy = numpy.sum(cy * (a - b)) / (6. * A)

    return Cx, Cy

def combine_catchments(catchmentfile, flowfile, elevationfile, comid, 
                       output = None, overwrite = False, verbose = True):
    """Combines together all the catchments in a basin catchment shapefile.
    Creates a new shapefile called "combined" in the same directory as the 
    original file.  Uses the elevation data from the raster file and the flow
    data file to estimate the length and average slope of the overland flow 
    plane.
    """

    t0 = time.time()
    numpy.seterr(all = 'raise')

    if output is None: output = os.getcwd() + r'\combined'

    if os.path.isfile(output + '.shp') and not overwrite:
        if verbose: print('combined catchment shapefile %s exists' % output)
        return
   
    if verbose: print('combining catchments from %s\n' % catchmentfile)

    # start by copying the projection files

    shutil.copy(catchmentfile + '.prj', output + '.prj')

    # load the catchment and flowline shapefiles

    c = Reader(catchmentfile, shapeType = 5)
    f = Reader(flowfile,      shapeType = 3)

    # make lists of the comids and featureids

    featureid_index = c.fields.index(['FEATUREID', 'N', 9, 0]) - 1
    comid_index     = f.fields.index(['COMID', 'N', 9,  0])    - 1

    featureids = [r[featureid_index] for r in c.records()]
    comids     = [r[comid_index]     for r in f.records()]

    # check that shapes are traceable--don't have multiple points and start
    # and end at the same place--then make an appropriate list of shapes
    # and records--note it's more memory efficient to read one at a time

    n = len(c.records())
    shapes  = []
    records = [] 
    bboxes  = []

    try: 
        for i in range(n):
            catchment = c.shape(i)
            record = c.record(i)

            shape_list = format_shape(catchment.points)
            for s in shape_list:
                shapes.append(s)
                records.append(record)
                bboxes.append(catchment.bbox)

        try:    combined = combine_shapes(shapes, bboxes, verbose = verbose)
        except: combined = combine_shapes(shapes, bboxes, skip = True, 
                                          verbose = verbose)

    except: 
        shapes  = []
        records = [] 
        bboxes  = []
        for i in range(n):
            catchment = c.shape(i)
            record = c.record(i)

            shape_list = format_shape(catchment.points, omit = True)
            for s in shape_list:
                shapes.append(s)
                records.append(record)
                bboxes.append(catchment.bbox)

        try:    combined = combine_shapes(shapes, bboxes, verbose = verbose)
        except: combined = combine_shapes(shapes, bboxes, skip = True,
                                          verbose = verbose)

    # iterate through the catchments and get the elevation data from NED
    # then estimate the value of the overland flow plane length and slope

    lengths = numpy.empty((n), dtype = 'float')
    slopes  = numpy.empty((n), dtype = 'float')

    for i in range(n):
        catchment = c.shape(i)
        flowline  = f.shape(comids.index(featureids[i]))

        catchpoints = get_raster_on_poly(elevationfile, catchment.points,
                                         verbose = verbose)
        catchpoints = numpy.array([p for p in catchpoints])

        zs = get_raster(elevationfile, flowline.points)

        flowpoints = numpy.array([[p[0], p[1], z] 
                                  for p, z in zip(flowline.points, zs)])

        # iterate through the raster values and find the closest flow point

        closest = numpy.empty((len(catchpoints), 3), dtype = 'float')

        for point, j in zip(catchpoints, range(len(catchpoints))):
            closest[j] = flowpoints[numpy.dot(flowpoints[:, :2], 
                                              point[:2]).argmin()]

        # estimate the slope and overland flow plane length

        length, slope = get_overland_vector(catchpoints, closest)

        if verbose: print('avg slope and length =', slope.mean(), length.mean())

        lengths[i], slopes[i] = length.mean(), slope.mean()

    if verbose: print('\nfinished overland flow plane calculations\n')

    # get area of the subbasin from the catchment metadata

    areasq_index = c.fields.index(['AreaSqKM', 'N', 19, 6]) - 1
    areas        = numpy.array([r[areasq_index] for r in c.records()])

    # take the area weighted average of the slopes and flow lengths

    tot_area   = round(areas.sum(), 2)
    avg_length = round(1000 * numpy.sum(areas * lengths) / tot_area, 1)
    avg_slope  = round(numpy.sum(areas * slopes) / tot_area, 4)

    # get the centroid and the average elevation

    combined = [[float(x), float(y)] for x, y in combined]
    centroid = get_centroid(numpy.array(combined))

    Cx, Cy = round(centroid[0], 4), round(centroid[1], 4)

    elev_matrix, origin = get_raster_in_poly(elevationfile, combined, 
                                             verbose = verbose)

    elev_matrix = elev_matrix.flatten()
    elev_matrix = elev_matrix[elev_matrix.nonzero()]
    
    avg_elev = round(elev_matrix.mean() / 100., 2)

    # write the data to the shapefile

    w = Writer(shapeType = 5)

    fields = [['ComID',      'N',  9, 0],
              ['PlaneLenM',  'N',  8, 2],
              ['PlaneSlope', 'N',  9, 6],
              ['AreaSqKm',   'N', 10, 2],
              ['CenX',       'N', 12, 6],
              ['CenY',       'N', 12, 6],
              ['AvgElevM',   'N',  8, 2]]

    record = [comid, avg_length, avg_slope, tot_area, Cx, Cy, avg_elev]

    for field in fields:  w.field(*field)
    
    w.record(*record)
    
    w.poly(shapeType = 5, parts = [combined])

    w.save(output)

    if verbose: print('\ncompleted catchment combination in %.1f seconds\n' % 
                      (time.time() - t0))

