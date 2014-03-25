import os, time, gdal, numpy

from gdalconst  import GA_ReadOnly
from PIL        import Image, ImageDraw

rfile = 'C:/Users/dlampert/Desktop/HWSD_RASTER/hwsd.bil'

# coordinates for the bounding box

lonmin, latmin, lonmax, latmax = -100, 35, -90, 45

# extract the values of the raster and the origin from the raster file

dtype = 'uint16'

# utility functions

def get_closest(l, x):
    """Returns the index of the closest value in list "l" to "x". """

    closest = min(l, key=lambda y:abs(y - x))

    return l.index(closest)

def get_pixel(x, x0, width):
    """returns the pixel number for a coordinate value."""

    return int((x - x0) // width)

def get_remainder(x, x0, width):
    """returns the remainder for the pixel."""

    return (x - x0) % width

def get_values(rfile):
    """Gets the pertinent info from the raster file."""

    # register all of the drivers

    gdal.AllRegister()

    # open the image

    dataset = gdal.Open(rfile, GA_ReadOnly)

    # get image size

    rows  = dataset.RasterYSize
    cols  = dataset.RasterXSize
    bands = dataset.RasterCount

    # get georeference info

    x0, w, x_rotation, y0, y_rotation, h = dataset.GetGeoTransform()

    # get the offset from the origin

    xoffset = w / 2
    yoffset = h / 2

    xs = [x0 + xoffset + w * i for i in range(cols)]
    ys = [y0 + yoffset + h * i for i in range(rows)]

    # read the band
        
    band = dataset.GetRasterBand(1)

    # read the values into an array

    values = band.ReadAsArray(0, 0, cols, rows)

    # close the files

    dataset = None

    return (x0, y0), xs, ys, values

def get_mask(xs, ys, points):
    """Returns a mask array for the points (paired values of x and y) for a 
    uniform grid with x values "xs" and y values "ys" """

    # transform the points into pixel space

    pixels = [(get_closest(xs, p[0]), get_closest(ys, p[1])) for p in points]

    # make a PIL image with the appropriate dimensions to use as a mask 

    rasterpoly = Image.new('L', (len(xs), len(ys)), 1)
    rasterize  = ImageDraw.Draw(rasterpoly)

    # rasterize the polygon

    rasterize.polygon(pixels, 0)

    # convert the PIL array to numpy boolean to use as a mask

    return 1 - numpy.array(rasterpoly)

start = time.time()

origin, xs, ys, values = get_values(rfile)

# make a list of all the vertices in the polygon

vertices = [(lonmin, latmin),
            (lonmin, latmax),
            (lonmax, latmax),
            (lonmax, latmin),
            ]

# get a mask for the vertices

mask = get_mask(xs, ys, vertices)

# use the mask to zero all the points outside the polygon

masked = mask * values

# cut away the zeros

inside = masked[numpy.nonzero(masked)]

# total number of grid points

tot = len(inside)

print('found {} values inside the box'.format(tot))

unique = numpy.unique(inside)

number = {}
for u in unique: 
    number[u] = len(inside[numpy.argwhere(inside == u)])

for u in number: print(u, number[u])

print('successfully calculated raster data')
