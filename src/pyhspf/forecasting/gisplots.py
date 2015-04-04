# HSPF Model Plot Routines
#
# David J. Lampert, PhD, PE
#
# Last updated: 04/16/2013
#
# Purpose: Lots of routines here to generate images for development of an HSPF
# model. Descriptions below.
#

import numpy as np, os, pickle, math, csv, datetime

from matplotlib              import pyplot, path,patches,colors, cm, gridspec
from matplotlib.ticker       import MaxNLocator, MultipleLocator
from mpl_toolkits.axes_grid1 import make_axes_locatable
from scipy                   import stats, log10
from itertools               import chain
from shapefile               import Reader, Writer
from PIL                     import Image, ImageDraw

from pyhspf.preprocessing.vectorutils        import format_shape
from pyhspf.preprocessing.rasterutils        import get_pixel, get_raster
from pyhspf.preprocessing.rasterutils        import get_raster_in_poly
from pyhspf.preprocessing.rasterutils        import get_raster_on_poly

def is_number(s):
    try: float(s) 
    except ValueError: return False
    return True

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

    return np.sqrt(k1**2 * dphi**2 + k2**2 * dlam**2)

def get_aggregate_map(aggregatefile):
    """Creates a dictionary that maps landuse codes to HSPF codes."""

    with open(aggregatefile, 'r') as csvfile:
        reader = csv.reader(csvfile)
        rows = [row for row in reader][1:]

    columns = [column for column in zip(*rows)]

    # set up the groups for the unique values in the aggregate file, a 
    # dictionary to map to the HSPF ID types, and the cumulative areas

    codes      = [int(code) for code in columns[0]]
    groups     = [int(code) for code in columns[2]]
    categories = [category for category in columns[3]]

    m         = {code: group for code, group in zip(codes, groups)}
    landtypes = {group: category for group, category in zip(groups, categories)}
    groups = list(set(groups))
    groups.sort()

    return m, landtypes, groups

def shape_to_mask(shape, width = 1000):

    # separate the x and y values for the shape

    xs, ys = zip(*shape)

    x0 = min(xs)
    y0 = min(ys)

    # calculate the pixel height

    height = math.ceil((max(ys) - min(ys)) / (max(xs) - min(xs)) * width)

    # calculate the width and height of a pixel

    w = (max(xs) - min(xs)) / width
    h = (max(ys) - min(ys)) / height

    # convert points of the polygon to pixels

    pixel_polygon = [(get_pixel(x, x0, w), get_pixel(y, y0, h))
                     for x, y in zip(xs, ys)]

    # make a PIL image with the appropriate dimensions to use as a mask 

    rasterpoly = Image.new('L', (width, height), 1)
    rasterize  = ImageDraw.Draw(rasterpoly)

    # rasterize the polygon

    rasterize.polygon(pixel_polygon, 0)

    # convert the PIL array to numpy boolean to use as a mask

    mask = 1 - np.array(rasterpoly)

    return mask

def poly_to_cdf(poly, n = 1000, dim = 'x'):
    """Determines the cumulative distribution function of the area of the 
    polygon (assumed to be made up of a list of points) in the chosen dimension
    for "n" values."""

    # convert the points to xs and ys

    xs, ys = zip(*poly)

    if dim == 'x':

        # convert the points to a mask array of ones (in) and zeros (out)
        
        mask = shape_to_mask(poly, width = n)

        # get the total number of pixels in the shape

        tot = mask.sum()

        # iterate from left to right or top to bottom and get the fraction
        # inside as a function of x or y

        x0 = min(xs)
        delx = (max(xs) - min(xs)) / n
        xrange = [x0 + delx * i for i in range(n + 1)]
        fractions = [column.sum() / tot for column in mask.transpose()]

        cdf = [0]
        for f in fractions: cdf.append(cdf[-1] + f)

        return xrange, cdf

    elif dim == 'y':
        mask = shape_to_mask(poly, width = 1)

    else:
        print('unknown coordinate dimension, please specify x or y.')
        return

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

def aggregate_hourly_daily(hourly):
    """Aggregates an hourly timeseries to a daily one."""

    if len(hourly) < 24:
        print('insufficiently short time series')
        return

    daily = []
    for i in range(0, len(hourly) - 23, 24):
        daily.append(sum(hourly[i:i+24]))

    return daily

def make_patch(points, facecolor, edgecolor = 'Black', width = 1, alpha = None,
               hatch = None, label = None):
    """Uses a list or array of points to generate a matplotlib patch."""

    vertices = [(point[0], point[1]) for point in points]
    vertices.append((points[0][0], points[0][1]))

    codes     = [path.Path.LINETO for i in range(len(points) + 1)]
    codes[0]  = path.Path.MOVETO

    patch = patches.PathPatch(path.Path(vertices, codes), facecolor = facecolor,
                              edgecolor = edgecolor, lw = width, hatch = hatch,
                              alpha = alpha, label = label)
    return patch

def add_raster(fig, filename, resolution, extent, colormap, intensity, 
               scale = 1):
    """adds a rectangular raster image with corners located at the extents
    to a plot.
    """

    xmin, ymin, xmax, ymax = extent

    xs = np.array([xmin + (xmax - xmin) / resolution * i 
                   for i in range(resolution + 1)])
    ys = np.array([ymax  - (ymax  - ymin)  / resolution * i 
                   for i in range(resolution + 1)])

    zs = np.zeros((resolution + 1, resolution + 1))

    for i in range(len(ys)):
        zs[i, :] = get_raster(filename, zip(xs, [ys[i]] * (resolution + 1)),
                              quiet = True)

    if intensity is not None:
        indices = zs > intensity[0]
        if zs[indices].min() > intensity[1]:
            zs = zs / scale
        norm = colors.Normalize(vmin = intensity[0], vmax = intensity[1])
    elif scale is not None:
        zs = zs / scale
        space = 0.2
        min, max = zs.min(), zs.max()
        min, max = min - space * (max - min), max + space * (max - min)
        norm = colors.Normalize(vmin = min, vmax = max)
    else: norm = None

    # plot the grid

    im = fig.imshow(zs, extent = [xmin, xmax, ymin, ymax], norm = norm, 
                    cmap = colormap)

    return im

def plot_climate(HUC8, sfile, bfile, pfile = None, efile = None, tfile = None, 
                 snowfile = None, centroids = True, radius = None, 
                 patchcolor = None, solarfile = None, windfile = None,
                 output = None, show = False, verbose = True):
    """Makes a plot of all the hourly precipitation stations of a watershed
    defined by "bfile" with subbasin defined by "sfile" from the source 
    precipitation shapefile "pfile"."""


    if verbose: 
        print('generating plot of watershed %s NCDC stations\n' % HUC8)

    fig = pyplot.figure()
    subplot = fig.add_subplot(111, aspect = 'equal')
    subplot.tick_params(axis = 'both', which = 'major', labelsize = 10)

    # add the title

    description = 'Climate Data Stations'
    title = 'Cataloging Unit %s\n%s' % (HUC8, description)
    subplot.set_title(title, fontsize = 14)

    # open up and show the catchments

    if patchcolor is None: facecolor = (1,0,0,0.)
    else:                  facecolor = patchcolor

    b = Reader(bfile, shapeType = 5)

    points = np.array(b.shape(0).points)
    subplot.add_patch(make_patch(points, facecolor = facecolor, width = 1.))

    extent = get_boundaries(b.shapes(), space = 0.02)

    xmin, ymin, xmax, ymax = extent

    # add the subbasin file

    s = Reader(sfile, shapeType = 5)

    # make patches of the subbasins

    for i in range(len(s.records())):
        shape = s.shape(i)
        points = np.array(shape.points)
        subplot.add_patch(make_patch(points, facecolor, width = 0.15))

    plots = [] # keep track of the scatterplots
    names = [] # keep track of names for the legend
        
    # add the subbasin centroids

    if centroids:

        cx_index = s.fields.index(['CenX', 'N', 12, 6]) - 1
        cy_index = s.fields.index(['CenY', 'N', 12, 6]) - 1

        centroids = [[r[cx_index], r[cy_index]] for r in s.records()]
        xs, ys = zip(*centroids)
        cplot = subplot.scatter(xs, ys, marker = '+', c = 'pink', s = 15)
        plots.append(cplot)
        names.append('Centroids')

    # add a circle showing around subbasin "radius" showing the gages within
    # the radius for a given subbasin

    if radius is not None:

        comid_index = s.fields.index(['ComID',    'N',  9, 0]) - 1
        cx_index    = s.fields.index(['CenX',     'N', 12, 6]) - 1
        cy_index    = s.fields.index(['CenY',     'N', 12, 6]) - 1
        area_index  = s.fields.index(['AreaSqKm', 'N', 10, 2]) - 1

        comids = ['{}'.format(r[comid_index]) for r in s.records()]
        cxs    = [r[cx_index]    for r in s.records()]
        cys    = [r[cy_index]    for r in s.records()]
        areas  = [r[area_index]  for r in s.records()]

        try: i = comids.index(radius)
        except: i = 0
        
        c = [cxs[i], cys[i]]

        radii = [math.sqrt(a / math.pi) for a in areas]

        # scale kms to degrees

        km  = get_distance([xmin, ymin], [xmax, ymax])
        deg = math.sqrt((xmin - xmax)**2 + (ymax - ymin)**2)

        r = sum(radii) / len(radii) * deg / km * 5

        circle = pyplot.Circle(c, radius = r, edgecolor = 'black', 
                               facecolor = 'yellow', alpha = 0.5) 
        subplot.add_patch(circle)
        subplot.scatter(c[0], c[1], marker = '+', c = 'black')

    # add the precipitation gage points

    if pfile is not None:

        with open(pfile, 'rb') as f: precips = pickle.load(f)

        gage_points = [(p.longitude, p.latitude) for p in precips.values()]
        x1, y1 = zip(*gage_points)
        plots.append(subplot.scatter(x1, y1, marker = 'o', c = 'b'))
        names.append('Precipitation') 

    # add the pan evaporation points

    if efile is not None:

        with open(efile, 'rb') as f: evaps = pickle.load(f)

        gage_points = [(e.longitude, e.latitude) for e in evaps.values()]

        x2, y2 = zip(*gage_points)
        eplot = subplot.scatter(x2, y2, s = evaps, marker = 'o', c = 'g')
        plots.append(eplot)
        names.append('Pan Evaporation')

    # add the temperature station points

    if tfile is not None:

        with open(tfile, 'rb') as f: temps = pickle.load(f)

        gage_points = [(t.longitude, t.latitude) for t in temps.values()]
        x2, y2 = zip(*gage_points)
        plots.append(subplot.scatter(x2, y2, marker = 's', c = 'red'))
        names.append('Temperature')

    # add the snowdepth station points

    if snowfile is not None:

        with open(snowfile, 'rb') as f: snows = pickle.load(f)

        snow_points = [(s.longitude, s.latitude) for s in snows.values()]
        x2, y2 = zip(*snow_points)
        plots.append(subplot.scatter(x2, y2, marker = 'o', c = 'gray', 
                                   alpha = 0.5))
        names.append('Snow')

    # add the solar radiation files

    if solarfile is not None:

        with open(solarfile, 'rb') as f: solar = pickle.load(f)

        points = [(s.longitude, s.latitude) for s in solar.values()]
        x2, y2 = zip(*points)
        plots.append(subplot.scatter(x2, y2, marker = 'o', c = 'orange'))
        names.append('Solar')

    # add the wind files

    if windfile is not None:

        with open(windfile, 'rb') as f: wind = pickle.load(f)

        points = [(w.longitude, w.latitude) for w in wind.values()]
        x2, y2 = zip(*points)
        plots.append(subplot.scatter(x2, y2, marker = 'o', c = 'pink'))
        names.append('Wind')

    # add a legend

    leg = subplot.legend(plots, names, loc = 'upper center', 
                         ncol = 3, bbox_to_anchor = (0.5, -0.15))

    legtext = leg.get_texts()
    pyplot.setp(legtext, fontsize = 10)

    #subplot.set_position([0.125, 0.1, 0.6, 0.8])

    # set the labels

    subplot.set_xlabel('Longitude, Decimal Degrees', size = 13)
    subplot.set_ylabel('Latitude, Decimal Degrees',  size = 13)

    # show it

    if output is not None: pyplot.savefig(output)
    if show: pyplot.show()

    pyplot.clf()
    pyplot.close()

