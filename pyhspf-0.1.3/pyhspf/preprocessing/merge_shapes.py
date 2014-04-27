# Merge shapes in a shapefile
#
# David J. Lampert, PhD, PE
#
# last updated Nov 20, 2012
#
# scroll to the bottom to use. note requires the python shapefile library (from
# Joel Lawhead) and numpy.  set up for python 3.2
#
# basically just enter the name of the shapefile and the output file and run
# the script.

import os, shutil, numpy, time

from shapefile import Reader, Writer

def array_to_list(a, tol = 6):
    """Converts a 2-d array with two columns to a list of lists."""
    
    return [[row[0].round(decimals = tol), row[1].round(decimals = tol)] 
            for row in a]

def format_shape(points, omit = False, tol = 6):
    """Formats a list of points into list of polygons arranged into arrays."""

    # check if there are multiple shapes, this is a major problem with NHDPlus

    parts = []
    i     = 0
    while i < len(points) - 1:
        current = i
        while points[i + 1] != points[current] and i < len(points) - 2: i+=1
        parts.append([current, i + 1])
        i+=2

    formatted = []
    for start, end in parts:

        # create an array to store the points in efficiently

        a = numpy.empty((len(points[start:end + 1]), 2), dtype = 'float')

        # format the points and add them to the array

        for p, i in zip(points[start:end + 1], range(end - start + 1)):

            a[i] = round(p[0], tol), round(p[1], tol)


        if len(a) > 5: formatted.append(a)
        elif not omit: formatted.append(a)

    return formatted

def next_index(l, i):
    """returns the index "i" of the next point in the array "a".  Work around 
    for reaching the end to go to the start.
    """

    if i == len(l) - 1: i  = 1
    else:               i += 1
    if l[i-1] == l[i]:  i += 1

    return i

def find_neighbor_indices(shape, bboxes):
    """Returns the indices of the list "bboxes" that overlap with the bounding
    box of "shape."
    """

    xmin = min([x for x, y in shape])
    ymin = min([y for x, y in shape])
    xmax = max([x for x, y in shape])
    ymax = max([y for x, y in shape])

    neighbors = []
    for b, i in zip(bboxes, range(len(bboxes))):
        
        if xmin < b[2] and xmax < b[0]: x = False
        else: x = True
        if ymin < b[3] and ymax < b[1]: y = False
        else: y = True

        if x and y: neighbors.append(i)

    return neighbors

def get_all(shape, shapes):
    """Makes a list of all the points in a list of shapes common to shape. """

    neighbor_points = []
    for s in shapes:
        for p in s:
            if p not in neighbor_points and p in shape: 
                neighbor_points.append(p)

    return neighbor_points

def combine_shapes(shapes, bboxes, skip = False, verbose = True):
    """Combines a list of shapes into a single shape.  Assumes the shapes are 
    simply connected with one common boundary.  Also assumes the points are in 
    clockwise order.  Starts at the smallest "x" value (longitude) and traces
    the first shape until reaching a junction, then figures out which direction
    to go next until reaching the first shape again.
    """

    start = time.time()

    # in the event of a single shape just return it

    if len(shapes) == 1: return [(x, y) for x, y in shapes[0]]

    # find the smallest value of x (which is an outside point) and start there
    # also use the shape with the max x as a check the trace made it around

    xmin        = min([shape[:, 0].min() for shape in shapes])
    xmax        = max([shape[:, 0].max() for shape in shapes])

    opposite_index = [shape[:, 0].max() for shape in shapes].index(xmax)
    shape_index = [shape[:, 0].min() for shape in shapes].index(xmin)
    start_index = shape_index

    # get the total number of points in the shape list to use to prevent crash
    
    total_points = sum([len(s) for s in shapes])

    # get the index of the starting point in the first shape

    i = numpy.where(shapes[shape_index] == xmin)[0][0]

    # "current" is current shape being traced (as list); "points" are the trace
 
    if verbose: print('tracing shape %d' % shape_index)
    shapes = [array_to_list(s) for s in shapes]
    current = shapes[shape_index]
    points = [current[i]]

    # go to the second point. note if the index reaches the end of the list
    # a mechanism is needed to go back to the start of the list.

    i = next_index(current, i)
          
    # find the indices of all shapes with an overlapping bounding box

    neighbor_indices = find_neighbor_indices(current, bboxes)
    neighbor_indices.remove(shape_index)

    # make a list of the neighbors and their points

    neighbors = [shapes[j] for j in neighbor_indices]
    if len(neighbors) == 0:
        print('error, no neighbors detected')
        raise
    neighboring_points = get_all(current, neighbors)

    # trace the current shape until reaching a point common to the neighbors
 
    while current[i] not in neighboring_points:
        points.append(current[i])
        i = next_index(current, i)
    points.append(current[i])

    for neighbor, j in zip(neighbors, neighbor_indices):
        if points[-1] in neighbor:
            shape_index = j

    current = shapes[shape_index]

    i = current.index(points[-1])
    i = next_index(current, i)

    # repeat the process until reaching the first shape again, also check it
    # made it to the opposite side

    opposite = False
    while shape_index != start_index or current[i] == points[0]:

        if shape_index == opposite_index: opposite = True

        if current[i] in points and skip:
            i = next_index(current, i)
            print('warning: repeated point, ignoring')
        elif current[i] in points:
            print('trace error occurred')
            raise
        if verbose: print('tracing shape', shape_index)

        # find the neighbors

        neighbor_indices = find_neighbor_indices(current, bboxes)
        neighbor_indices.remove(shape_index)
        neighbors = [shapes[j] for j in neighbor_indices]
        neighboring_points = get_all(current, neighbors)

        # trace the current shape until reaching a point common to the neighbors
    
        while current[i] not in neighboring_points:
            points.append(current[i])
            i = next_index(current, i)
        points.append(current[i])

        for neighbor, j in zip(neighbors, neighbor_indices):
            if points[-1] in neighbor:
                shape_index = j

        current = shapes[shape_index]
        i = current.index(points[-1])
        i = next_index(current, i)

    # add the last points from the first shape

    if verbose: print('tracing shape %d' % shape_index)

    while current[i] != points[0]:
        points.append(current[i])
        i = next_index(current, i)
    points.append(points[0])

    if verbose: print('\nfinished tracing catchments in %.1f seconds\n' 
                      % (time.time() - start))

    if opposite: return points
    else: 
        if verbose: 
            print('\ntrace failed to reach opposite side, ' +
                  'trying alternate method')
        raise

def merge_shapes(inputfile, outputfile = None, overwrite = False, 
                 verbose = True, vverbose = False):
    """Merges all the shapes in a shapefile into a single shape."""

    if outputfile is None: output = os.getcwd() + r'\merged'

    if os.path.isfile(outputfile) and not overwrite:
        if verbose: print('combined watershed shapefile %s exists' % outputfile)
        return
   
    if verbose: print('combining shapes from %s\n' % inputfile)

    # start by copying the projection files

    shutil.copy(inputfile + '.prj', outputfile + '.prj')

    # load the catchment and flowline shapefiles

    r = Reader(inputfile, shapeType = 5)
    n = len(r.records())

    try: 
        shapes  = []
        records = [] 
        bboxes  = []

        for i in range(n):
            shape = r.shape(i)
            record = r.record(i)

            shape_list = format_shape(shape.points)

            for sh in shape_list:
                shapes.append(sh)
                records.append(record)
                bboxes.append(shape.bbox)

                try: combined = combine_shapes(shapes, bboxes, 
                                                  verbose = vverbose)
                except: combined = combine_shapes(shapes, bboxes, skip = True, 
                                                  verbose = vverbose)

    except:
        shapes  = []
        records = [] 
        bboxes  = []
        for i in range(n):
            shape = r.shape(i)
            record = r.record(i)

            shape_list = format_shape(shape.points, omit = True)

            for sh in shape_list:
                shapes.append(sh)
                records.append(record)
                bboxes.append(shape.bbox)

        try:    combined = combine_shapes(shapes, bboxes, verbose = vverbose)
        except: combined = combine_shapes(shapes, bboxes, skip = True,
                                          verbose = vverbose)

    # create the new file with the merged shapes

    w = Writer(shapeType = 5)

    w.poly(shapeType = 5, parts = [combined])

    # copy the fields from the original and then the first record; note this
    # can be adapted as needed

    for field in r.fields: w.field(*field)
    w.record(*r.record(0))

    w.save(outputfile)

    if verbose: 
        print('successfully combined shapes from %s to %s\n' % 
              (inputfile, outputfile))

# pretty simple to use--point the inputfile to the shapefile with the shapes
# you want to merge (do NOT include the extension .shp, .prj, etc.) and the 
# name you want for the output file (if unspecified it will be called "merged"
# and placed in the current directory).

#inputfile = r'C:\HSPF_data\07100008\22248777\catchments'
#outputfile = r'C:\HSPF_data\07100008\22248777\merged'

#merge_shapes(inputfile, outputfile = outputfile)
