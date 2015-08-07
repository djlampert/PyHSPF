# This file contains a series of functions were reading and working with raster
# data.
#
# last modified: 11/01/2012
#
# David Lampert

import gdal, osr, ogr, gdalnumeric, time

from gdalconst       import GA_ReadOnly
from itertools       import chain
from numpy           import empty, array, where
from PIL             import Image, ImageDraw
from matplotlib.path import Path

def edge(i, j, v):
    """Determines if a point at i, j in a boolean grid v is on the boundary."""

    try:
        if v[i-1,j] and v[i+1,j] and v[i,j-1] and v[i,j+1]: return False
        else:                                               return True
    except: return True

def inside_box(p1, p2, p3, space = 0):
    """Checks if p3 is inside a box formed by p1 and p2."""

    if p1[0] < p3[0] and p3[0] < p2[0] or p1[0] > p3[0] and p3[0] > p2[0]:

        # x value is inside

        if p1[1] < p3[1] and p3[1] < p2[1] or p1[1] > p3[1] and p3[1] > p2[1]:
            
            # y value is inside

            return True

        else: return False

    else: return False

def get_NAD1983_transform(dataset):
    """
    Gets a GDAL transform to convert coordinate northings and eastings 
    associated with the NAD 1983 projection to latitudes and longitudes.
    """

    # get the old coordinate system

    old = osr.SpatialReference()
    old.ImportFromWkt(dataset.GetProjection())

    # create the new coordinate system

    nad83_wkt = (
        """
        GEOGCS["NAD83",
        DATUM["North_American_Datum_1983",
        SPHEROID["GRS 1980",6378137,298.257222101,
        AUTHORITY["EPSG","7019"]],
        AUTHORITY["EPSG","6269"]],
        PRIMEM["Greenwich",0,
        AUTHORITY["EPSG","8901"]],
        UNIT["degree",0.0174532925199433,
        AUTHORITY["EPSG","9108"]],
        AUTHORITY["EPSG","4269"]]
        """
        )

    new = osr.SpatialReference()
    new.ImportFromWkt(nad83_wkt)

    # create a transform object to convert between coordinate systems

    transform = osr.CoordinateTransformation(new, old) 

    return transform

def get_degree_transform(dataset):
    """
    Gets a GDAL transform to convert coordinate latitudes and longitudes
    to northings and eastings associated with the NAD 1983 projection.
    """

    # get the old coordinate system

    old = osr.SpatialReference()
    old.ImportFromWkt(dataset.GetProjection())

    # create the new coordinate system

    nad83_wkt = (
        """
        GEOGCS["NAD83",
        DATUM["North_American_Datum_1983",
        SPHEROID["GRS 1980",6378137,298.257222101,
        AUTHORITY["EPSG","7019"]],
        AUTHORITY["EPSG","6269"]],
        PRIMEM["Greenwich",0,
        AUTHORITY["EPSG","8901"]],
        UNIT["degree",0.0174532925199433,
        AUTHORITY["EPSG","9108"]],
        AUTHORITY["EPSG","4269"]]
        """
        )

    new = osr.SpatialReference()
    new.ImportFromWkt(nad83_wkt)

    # create a transform object to convert between coordinate systems

    transform = osr.CoordinateTransformation(old, new) 

    return transform

def get_pixel(x, x0, width):
    """returns the pixel number for a coordinate value."""

    return int((x - x0) // width)

def get_remainder(x, x0, width):
    """
    returns the remainder for the pixel.
    """

    return (x - x0) % width

def get_cdl_meters(extent):
    """
    returns the bounding box coordinates in m for the Cropland Data Layer
    for a given extent in longitude/latitude.
    """

    nad1983 = (
        """
        PROJCS["unnamed",
        GEOGCS["NAD83",
        DATUM["North_American_Datum_1983",
        SPHEROID["GRS 1980",6378137,298.2572221010002,
        AUTHORITY["EPSG","7019"]],
        TOWGS84[0,0,0,0,0,0,0],
        AUTHORITY["EPSG","6269"]],
        PRIMEM["Greenwich",0],
        UNIT["degree",0.0174532925199433],
        AUTHORITY["EPSG","4269"]],
        PROJECTION["Albers_Conic_Equal_Area"],
        PARAMETER["standard_parallel_1",29.5],
        PARAMETER["standard_parallel_2",45.5],
        PARAMETER["latitude_of_center",23],
        PARAMETER["longitude_of_center",-96],
        PARAMETER["false_easting",0],
        PARAMETER["false_northing",0],
        UNIT["metre",1,AUTHORITY["EPSG","9001"]]]
        """
        ) 

    # new coordinate system

    degrees = (
        """
        GEOGCS["NAD83",
        DATUM["North_American_Datum_1983",
        SPHEROID["GRS 1980",6378137,298.257222101,
        AUTHORITY["EPSG","7019"]],
        AUTHORITY["EPSG","6269"]],
        PRIMEM["Greenwich",0,
        AUTHORITY["EPSG","8901"]],
        UNIT["degree",0.0174532925199433,
        AUTHORITY["EPSG","9108"]],
        AUTHORITY["EPSG","4269"]]
        """
        )

    # create a transform object to convert between the coordinate systems

    old = osr.SpatialReference()
    old.ImportFromWkt(degrees)

    new = osr.SpatialReference()
    new.ImportFromWkt(nad1983)

    transform = osr.CoordinateTransformation(old, new) 

    # transform the extent into the new coordinates

    lonmin, latmin, lonmax, latmax = extent

    points = zip([lonmin] * 2 + [lonmax] * 2, [latmin, latmax] * 2)

    xs, ys = [], []

    for p in points:

        x, y, z = transform.TransformPoint(*p)

        xs.append(x)
        ys.append(y)

    transformed = [min(xs), min(ys), max(xs), max(ys)]

    return transformed
  
def get_raster(filename, points, quiet = False):
    """
    Reads the value of attributes in a raster file at a list of points.
    """

    if quiet: gdal.PushErrorHandler('CPLQuietErrorHandler') 

    # register all of the drivers

    gdal.AllRegister()

    # open the image

    dataset = gdal.Open(filename, GA_ReadOnly)

    # get the coordinate transformation

    transform = get_NAD1983_transform(dataset)

    # get image size

    rows  = dataset.RasterYSize
    cols  = dataset.RasterXSize
    bands = dataset.RasterCount

    # get georeference info

    x0, width, x_rotation, y0, y_rotation, height = dataset.GetGeoTransform()

    # loop through the points and get the raster values

    values = []
    for point in points:

        # get x,y

        x, y, z = transform.TransformPoint(point[0], point[1]) 

        # transform the easting and northing to pixel space

        pixel_x = int((x - x0) / width)
        pixel_y = int((y - y0) / height)

        # loop through the bands and find the values

        for i in range(1, bands + 1):

            band = dataset.GetRasterBand(i)

            # read data and add the value to the string

            value = band.ReadRaster(pixel_x, pixel_y, 1, 1)
            if value is None: value = -1
            else: value = int.from_bytes(value, byteorder = 'little')

        values.append(value)

    return values

def get_raster_table(filename, 
                     extent, 
                     dtype, 
                     locations = False, 
                     quiet = False,
                     ):
    """
    Gets the values of a DEM raster over a rectangular plot with corners 
    located at longmin, latmin, longmin, and latmax as specified by extents.
    Returns a matrix of values and the corresponding latitude and longitude.
    """

    start = time.time()

    longmin, latmin, longmax, latmax = extent

    if quiet: gdal.PushErrorHandler('CPLQuietErrorHandler') 

    # register all of the drivers

    gdal.AllRegister()

    # open the image

    dataset = gdal.Open(filename, GA_ReadOnly)

    # get image size

    rows  = dataset.RasterYSize
    cols  = dataset.RasterXSize
    bands = dataset.RasterCount

    # get georeference info

    x0, w, x_rotation, y0, y_rotation, h = dataset.GetGeoTransform()

    # transform the to/from NAD 1983 and latitudes/longitudes

    NAD1983_transform = get_NAD1983_transform(dataset)
    degree_transform  = get_degree_transform(dataset)

    # transform the corner points to NAD 1983

    points = zip([longmin] * 2 + [longmax] * 2, [latmin, latmax] * 2)

    xs, ys, zs = zip(*[NAD1983_transform.TransformPoint(*point) 
                       for point in points])
   
    # get the pixel values of the min longitudes and latitudes and the number
    # of pixels in each direction
 
    pxmin  = min([get_pixel(x, x0, w) for x in xs])
    pymin  = min([get_pixel(y, y0, h) for y in ys])
    width  = max([get_pixel(x, x0, w) for x in xs]) - pxmin
    height = max([get_pixel(y, y0, h) for y in ys]) - pymin

    # find the location of the origin (pixels are integers, degrees are reals)

    rx = get_remainder(min(xs), x0, w)
    ry = get_remainder(min(ys), y0, h)

    origin = [min(xs), min(ys)]

    # pre-allocate some space to store the lat/long of each pixel and the value

    latitudes  = empty((height, width), dtype = 'float')
    longitudes = empty((height, width), dtype = 'float')

    # read the band
        
    band = dataset.GetRasterBand(1)

    # iterate through the file, noting that pixels start at top left and move
    # down and right, and the y values move up

    values = empty((height, width), dtype = dtype)

    if locations:

        # need to return the latitudes and longitudes, which takes time

        for row in range(height):
            values[height - row - 1] = band.ReadAsArray(pxmin, pymin + row, 
                                                        width, 1) 
            for column in range(width):
                x, y, z = degree_transform.TransformPoint(origin[0] + w *column,
                                                          origin[1] + h * row)
                latitudes[height - row - 1,  column] = y
                longitudes[height - row - 1, column] = x

        return longitudes, latitudes, values

    else:

        # just return the location of the origin

        try: 

            for row in range(height):
                b = band.ReadAsArray(pxmin, pymin + row, width, 1) 
                values[height - row - 1] = b
                    
        except: 

            if not quiet: print('warning: unable to read data\n')
            values = None

        return values, [origin[0] - rx, origin[1] - ry]

def get_raster_in_poly(rasterfile, poly, dtype = 'uint16', locations = False,
                       verbose = True):
    """Parses through the raster values with corresponding latitudes and
    longitudes and returns a list of the values inside the polygon."""

    start = time.time()

    gdal.AllRegister()

    # open the image

    dataset = gdal.Open(rasterfile, GA_ReadOnly)

    # get the transform info

    x0, w, x_rotation, y0, y_rotation, h = dataset.GetGeoTransform()

    NAD1983_transform = get_NAD1983_transform(dataset)

    # transform the corner points to NAD 1983

    xs, ys, zs = zip(*[NAD1983_transform.TransformPoint(*point) 
                       for point in poly])

    # get the pixel values of the min xs and ys and the number of pixels 
    # in each direction
 
    pxmin  = min([get_pixel(x, x0, w) for x in xs])
    pymin  = min([get_pixel(y, y0, h) for y in ys])
    width  = max([get_pixel(x, x0, w) for x in xs]) - pxmin
    height = max([get_pixel(y, y0, h) for y in ys]) - pymin

    # get the location of the origin

    rx = get_remainder(min(xs), x0, w)
    ry = get_remainder(min(ys), y0, h)

    origin = [min(xs) - rx, min(ys) - ry]

    # convert points of the polygon to pixels

    pixel_xs = [get_pixel(x, x0, w) - pxmin for x in xs]
    pixel_ys = [get_pixel(y, y0, h) - pymin for y in ys]

    pixel_polygon = [(x, y) for x, y in zip(pixel_xs, pixel_ys)]

    # make a PIL image with the appropriate dimensions to use as a mask 

    rasterpoly = Image.new('L', (width, height), 1)
    rasterize  = ImageDraw.Draw(rasterpoly)

    # rasterize the polygon

    rasterize.polygon(pixel_polygon, 0)

    # convert the PIL array to numpy boolean to use as a mask

    mask = 1 - array(rasterpoly)

    # read the band
        
    band = dataset.GetRasterBand(1)

    # iterate through the file, noting that pixels start at top left and move
    # down and right, and the y values move up

    values = empty((height, width), dtype = dtype)

    for row in range(height):
        values[row] = band.ReadAsArray(pxmin, pymin + row, width, 1) 

    values = values * mask

    if verbose: print('found %d points in polygon in %.1f seconds\n' % 
                      (mask.sum(), time.time() - start))

    # determine if need to calculate point coordinates (which takes time, and
    # is primarily for plotting)

    if locations: 

        # need to return the latitudes and longitudes, which takes time

        if verbose: print('transforming pixels to coordinates...\n')

        degree_transform = get_degree_transform(dataset)

        xs = empty((height, width), dtype = 'float')
        ys = empty((height, width), dtype = 'float')

        for i in range(len(values)):
            for j in range(len(values[i])):

                if values[i, j] != 0:
                    x, y, z = degree_transform.TransformPoint(origin[0] + w * j,
                                                              origin[1] + h * i)
                    xs[i, j] = x
                    ys[i, j] = y

        return xs.flatten(), ys.flatten(), values.flatten()

    else: return values, origin

def get_raster_on_poly(rasterfile, poly, dtype = 'uint16', verbose = True):
    """Parses through an array of raster values with corresponding latitudes and
    longitudes and returns a list of the values on the shape boundary. """

    # get the extent and the raster values in the bounding box

    xmin = min([x for x, y in poly])
    xmax = max([x for x, y in poly])
    ymin = min([y for x, y in poly])
    ymax = max([y for x, y in poly])

    extent = xmin, ymin, xmax, ymax
    xs, ys, zs = get_raster_table(rasterfile, extent, dtype, locations = True)

    # create a matplotlib path for the polygon for point testing

    path = Path(poly)

    # set up a list for the points on the boundary

    points = []

    # find the bottom row

    n = len(xs[0]) - 1  # index of last column (used a lot)

    bottom = False
    row = 0
    while not bottom:
        row = row - 1
        x_row, y_row = xs[row], ys[row]
        bottom = any([path.contains_point([x,y]) for x, y in zip(x_row, y_row)])

    # start at the left and go right until a point is inside

    j = 0
    while j < n and not path.contains_point([x_row[j], y_row[j]]): j+=1

    # start at the right and go left until a point is inside

    k = n
    while k > 0 and not path.contains_point([x_row[k], y_row[k]]): k = k - 1

    for p in zip(xs[row, j:k+1], ys[row, j:k+1], zs[row, j:k+1]): 
        points.append(p)

    # keep track of the bottom row

    bottom, bleft, bright = row + len(xs), j, k
    
    # find the top row

    top = False
    row = -1
    while not top:
        row+=1
        x_row, y_row = xs[row], ys[row]
        top = any([path.contains_point([x, y]) for x, y in zip(x_row, y_row)])

    # start at the left and go right until a point is inside

    j = 0
    while j < n and not path.contains_point([x_row[j], y_row[j]]): j+=1

    # start at the right and go left until a point is inside

    k = n
    while k > 0 and not path.contains_point([x_row[k], y_row[k]]): k = k - 1

    for p in zip(xs[row, j:k+1], ys[row, j:k+1], zs[row, j:k+1]):
        points.append(p)

    # keep track of the left and right sides of the row above

    top, left, right = row + 1, j, k

    # parse through the rows and look for the first values inside; keep track
    # of the edges from the previous row (left and right)

    for x_row, y_row, z_row in zip(xs[top:bottom - 1], ys[top:bottom - 1], 
                                   zs[top:bottom - 1]):

        # start at the left and go right until a point is inside

        j = 0
        while j < n and not path.contains_point([x_row[j], y_row[j]]): j+=1

        # start at the right and go left until a point is inside

        k = n
        while k > 0 and not path.contains_point([x_row[k], y_row[k]]): k = k - 1

        # add the points from left to last left and right to the last right

        if   j == right: l = range(0)
        elif j <   left: l = range(j, left)
        else:            l = range(left, j + 1)

        if   k ==  left: r = range(0)
        elif k  > right: r = range(right + 1, k + 1)
        else:            r = range(k, right + 1)

        for i in chain(l, r): points.append((x_row[i], y_row[i], z_row[i]))

        if j != right: left  = j
        if k != left:  right = k

    # connect to the last row

    x_row, y_row, z_row = xs[bottom - 1], ys[bottom - 1], zs[bottom - 1]

    l, r = range(left, bleft + 1), range(bright, right + 1)

    for i in chain(l, r): points.append((x_row[i], y_row[i], z_row[i]))

    return points

def get_raster_on_line(longitudes, latitudes, values, line):
    """Parses through an array of raster values with corresponding latitudes and
    longitudes and returns a list of the values on the line within d (km)."""

    d = sqrt(norm([longitudes[0,0], latitudes[0,0]],
                  [longitudes[1,0], latitudes[1,0]])) * 2

    print(len(line), d)

    line_values = []
    lats        = []
    longs       = []

    # now look at the interior points and keep only those within the distance

    # follow the flow line and find any points within the tolerance

    for current, next in zip(line[:-1], line[1:]):

        # parse through the values in the matrix and see if they if in the 
        # bounding box of the line segment, then check the distance tolerance

        for i in range(len(values)):
            for j in range(len(values[i])):

                point = [longitudes[i,j], latitudes[i,j]]

                p1, p2 = [0,0], [0,0]
                
                tol = 0.2
                if current[0] < next[0]:
                    p1[0] = current[0] - tol * (next[0] - current[0])
                    p2[0] = next[0]    + tol * (next[0] - current[0])
                else: 
                    p1[0] = current[0] + tol * (next[0] - current[0])
                    p2[0] = next[0]    - tol * (next[0] - current[0])

                if current[1] < next[1]:
                    p1[1] = current[1] - tol * (next[1] - current[1])
                    p2[1] = next[1]    + tol * (next[1] - current[1])
                else: 
                    p1[1] = current[1] + tol * (next[1] - current[1])
                    p2[1] = next[1]    - tol * (next[1] - current[1])

                if inside_box(p1, p2, point):
 
                    if point_line_distance(current, next, point) < d:

                        line_values.append(values[i, j])
                        lats.append(latitudes[i, j])
                        longs.append(longitudes[i, j]) 

    line_values = array(line_values, dtype = 'float32')
    lats        = array(lats,        dtype = 'float32')
    longs       = array(longs,       dtype = 'float32')

    return longs, lats, line_values
