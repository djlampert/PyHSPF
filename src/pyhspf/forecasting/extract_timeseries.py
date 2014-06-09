#!/usr/bin/env python
#
# extract_NRCM.py
# David J. Lampert
# 
# extracts the grid point for a watershed from the preprocessed NRCM data

import os, shutil, pickle, datetime, numpy

from multiprocessing import Pool, cpu_count
from matplotlib      import pyplot, path, patches
from shapefile       import Reader

def inside_box(p1, p2, p3, space = 0):
    """Checks if p3 is inside a box formed by p1 and p2."""

    if p1[0] < p3[0] and p3[0] < p2[0] or p1[0] > p3[0] and p3[0] > p2[0]:

        # x value is inside

        if p1[1] < p3[1] and p3[1] < p2[1] or p1[1] > p3[1] and p3[1] > p2[1]:
            
            # y value is inside

            return True

        else: return False

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

def plot_NRCM(lons, lats, bfile = None, sfile = None, space = 0.05,
              show = False, output = None):

    fig = pyplot.figure()

    sub = fig.add_subplot(111, aspect = 'equal')
    sub.set_title('Nested Regional Climate Model Grid Points')
    sub.scatter(lons, lats, marker = '+', c = 'r', s = 40)

    if bfile is not None:
        
        sf = Reader(bfile)
        boundary = sf.shape(0).points
        sub.add_patch(make_patch(boundary, (1, 0, 0, 0), width = 1.2))

    if sfile is not None:
        
        sf = Reader(sfile)

        for s in sf.shapes():
            boundary = s.points
            sub.add_patch(make_patch(boundary, (1, 0, 0, 0), width = 0.2))

    sub.set_xlabel('Longitude, Decimal Degrees', size = 13)
    sub.set_ylabel('Latitude, Decimal Degrees', size = 13)

    xmin, ymin, xmax, ymax = get_boundaries(sf.shapes(), space = space)

    pyplot.xlim([xmin, xmax])
    pyplot.ylim([ymin, ymax])

    if output is not None: pyplot.savefig(output)

    if show: pyplot.show()

    pyplot.clf()
    pyplot.close()

def extract_raw(source, destination, HUC8, plot = True, save = True, 
                verbose = True):
    """Extracts the grid data for the HUC8."""

    # make a new directory for the HUC8

    d = '{}/{}/NRCM'.format(destination, HUC8)

    if not os.path.isdir(d): os.mkdir(d)

    # make a "raw directory" for the unaltered info

    raw = '{}/raw'.format(d)

    if not os.path.isdir(raw): 
        os.mkdir(raw)
        if verbose: print('extracting NRCM predictions...\n')

    # use the boundary file to find the bounding box for the grid points

    boundaryfile = '{0}/{1}/{1}boundaries'.format(destination, HUC8)
    subbasinfile = '{0}/{1}/{1}subbasins'.format(destination, HUC8)
    space        = 0.1

    sf = Reader(boundaryfile)

    bbox = get_boundaries(sf.shapes(), space = space)

    xmin, ymin, xmax, ymax = bbox

    if verbose and not os.path.isdir(raw): 
        print('bounding box =', xmin, ymin, xmax, ymax, '\n')

    lats, lons = [], []
    for f in os.listdir(source):
        i = f.index('_')
        lon = float(f[:i])
        lat = float(f[i+1:])

        if inside_box([xmin, ymin], [xmax, ymax], [lon, lat]):
            lats.append(lat)
            lons.append(lon)

            if not os.path.isfile('{}/{}'.format(raw, f)):
                shutil.copy('{}/{}'.format(source, f), '{}/{}'.format(raw, f))
            
    if plot: 
        if save: output = '{}/gridpoints'.format(d)
        else:    output = None
        if not os.path.isfile(output):
            plot_NRCM(lons, lats, bfile = boundaryfile, sfile = subbasinfile,
                      output = output, show = False)

def extract_timeseries(directory, start, end,
                       series = ['rain', 'temperature', 'wind', 'humidity', 
                                 'solar', 'snowdepth', 'evaporation']):

    source = '{}/raw'.format(directory)

    gridfiles = [os.path.join(source, f) for f in os.listdir(source)]

    for ts in series:

        destination = '{}/{}'.format(directory, ts)
        if not os.path.isdir(destination): 
            os.mkdir(destination)

    # iterate through the grid point data files

    for f in gridfiles:

        with open(f, 'rb') as p: g = pickle.load(p)

        # dump each time series and get rid of time zone info

        for ts in series:

            data = [(datetime.datetime(t.year, t.month, t.day, t.hour), v) 
                     for t, v in g.data[ts]]
            data = [(t, v) for t, v in data if start <= t and t < end]
            
            it = directory, ts, g.lon, g.lat

            destination = '{}/{}/{:8.4f}_{:7.4f}'.format(*it)

            if not os.path.isfile(destination):
                with open(destination, 'wb') as f: pickle.dump(data, f)

def average_timeseries(directory,
                       variables = ['temperature', 'wind', 'humidity', 
                                    'solar', 'evaporation', 'snowdepth']):

    averages = '{}/averages'.format(directory)

    if not os.path.isdir(averages): os.mkdir(averages)

    for v in variables:

        destination = '{}/average_{}'.format(averages, v)

        if not os.path.isfile(destination):

            print('averaging {} timeseries...\n'.format(v))

            series = []
            source = '{}/{}'.format(directory, v)
            for f in os.listdir(source):
                p = '{}/{}'.format(source, f)
                with open(p, 'rb') as d: ts, data = zip(*pickle.load(d))
                series.append(numpy.array(data))

            values = sum(series) / len(series)

            average = [(t, va) for t, va in zip(ts, values)]

            with open(destination, 'wb') as f: pickle.dump(average, f)

def extract_NRCM(source, destination, HUC8, start, end, plot = True):
    """Extracts the raw data from the regional climate model."""
    
    extract_raw(source, destination, HUC8, plot = plot)

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end,   1, 1)

    d = '{}/{}/NRCM'.format(destination, HUC8)

    if not any([os.path.isdir('{}/{}'.format(d, ts))
                for ts in ['rain', 'snowdepth', 'temperature', 'humidity', 
                           'wind', 'solar', 'evaporation']]): 
        extract_timeseries(d, s, e)

