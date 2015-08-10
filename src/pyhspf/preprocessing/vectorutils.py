# various utility functions for manipulating vector GIS data
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated 08/07/2015
#
# contains the merge_shapes function that takes the name of an input polygon 
# shapefile and then merges the shapes together using the combine_shapes method
# using a "trace" of the outside of the shapes.

import os, shutil, time, collections, numpy

from shapefile import Reader, Writer

def array_to_list(a, 
                  tol = 6,
                  ):
    """
    Converts a 2-d array with two columns to a list of lists.
    """
    
    return [[row[0].round(decimals = tol), row[1].round(decimals = tol)] 
            for row in a]

def format_shape(points, 
                 omit = False, 
                 tol = 6,
                 ):
    """
    Formats a list of points into list of polygons arranged into arrays.
    """

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

        # create an array to store the points efficiently

        a = numpy.empty((len(points[start:end + 1]), 2), dtype = 'float')

        # format the points and add them to the array

        for p, i in zip(points[start:end + 1], range(end - start + 1)):

            a[i] = round(p[0], tol), round(p[1], tol)

        if len(a) > 5: formatted.append(a)
        elif not omit: formatted.append(a)

    return formatted

def next_old(l, 
               i,
               ):
    """
    Returns the index "i" of the next point in the array "a".  Work around 
    for reaching the end to go to the start.
    """

    if i == len(l) - 1: i  = 1
    else:               i += 1
    if l[i-1] == l[i]:  i += 1

    return i

def find_neighbor_indices(shape, 
                          bboxes,
                          ):
    """
    Returns the indices of the list "bboxes" that overlap with the bounding
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

def get_all(shape, 
            shapes,
            ):
    """
    Makes a list of all the points in a list of shapes common to shape.
    """

    neighbor_points = []
    for s in shapes:
        for p in s:
            if p not in neighbor_points and p in shape: 
                neighbor_points.append(p)

    return neighbor_points

def combine_shapes2(shapes, 
                    bboxes, 
                    skip = False, 
                    verbose = True,
                    ):
    """
    Combines a list of shapes into a single shape.  Assumes the shapes are 
    simply connected with one common boundary.  Also assumes the points are in 
    clockwise order.  Starts at the smallest "x" value (longitude) and traces
    the first shape until reaching a junction, then figures out which direction
    to go next until reaching the first shape again.
    """

    start = time.time()

    # in the event of a single shape just return it

    if len(shapes) == 1: return [(x, y) for x, y in shapes[0]]

    # find all the edge points (only appear once)

    all_points = [(x, y) for shape in shapes for x, y in shape]

    # find all points that aren't duplicated (interior points are duplicated)

    edges = [p for p, count in collections.Counter(all_points).items()
             if count == 1]

    xs, ys = zip(*edges)

    # find the smallest value of x (which is an outside point) and start there
    # also use the shape with the max x as a check the trace made it around

    xmin = min(xs)
    xmax = max(xs)
    ymin = min(ys)
    ymax = max(ys)
    #xmin = min([shape[:, 0].min() for shape in shapes])
    #xmax = max([shape[:, 0].max() for shape in shapes])

    opposite_index = [shape[:, 0].max() for shape in shapes].index(xmax)
    shape_index = [shape[:, 0].min() for shape in shapes].index(xmin)
    start_index = shape_index

    # get the total number of points in the shape list to use to prevent crash
    
    total_points = len(all_points)
    #total_points = sum([len(s) for s in shapes])

    # get the index of the starting point in the first shape

    i = numpy.where(shapes[shape_index] == xmin)[0][0]

    # "current" is current shape being traced (as list); "points" are the trace
 
    if verbose: print('tracing shape {}'.format(shape_index))

    shapes = [array_to_list(s) for s in shapes]
    current = shapes[shape_index]
    points = [current[i]]

    # go to the second point. note if the index reaches the end of the list
    # a mechanism is needed to go back to the start of the list.

    i = next_old(current, i)
          
    # find the indices of all shapes with an overlapping bounding box

    neighbor_indices = find_neighbor_indices(current, bboxes)
    neighbor_indices.remove(shape_index)

    # make a list of the neighbors and their points

    neighbors = [shapes[j] for j in neighbor_indices]
    if len(neighbors) == 0:
        print('warning, no neighbors detected')
        raise
    neighboring_points = get_all(current, neighbors)

    if len(neighboring_points) == 0:
        print('warning, no neighbors detected')
        raise

    # trace the current shape until reaching a point common to the neighbors

    #print(edges[:5])
    #print(current[i] in edges)
    #while current[i] in edges:
    while current[i] not in neighboring_points:
        points.append(current[i])
        i = next_old(current, i)
    points.append(current[i])
    
    for neighbor, j in zip(neighbors, neighbor_indices):
        if points[-1] in neighbor:
        #if current[i] in neighbor:
            shape_index = j

    #i = shapes[shape_index].index(current[i])
    current = shapes[shape_index]

    i = current.index(points[-1])
    i = next_old(current, i)

    # repeat the process until reaching the first shape again, also check it
    # made it to the opposite side

    opposite = start_index == opposite_index

    while shape_index != start_index or current[i] == points[0]:

        if shape_index == opposite_index: opposite = True

        if current[i] in points and skip:
            i = next_old(current, i)
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
            i = next_old(current, i)
        points.append(current[i])

        for neighbor, j in zip(neighbors, neighbor_indices):
            if points[-1] in neighbor:
                shape_index = j

        current = shapes[shape_index]
        i = current.index(points[-1])
        i = next_old(current, i)

    # add the last points from the first shape

    if verbose: print('tracing shape {}'.format(shape_index))

    while current[i] != points[0]:
        points.append(current[i])
        i = next_old(current, i)
    points.append(points[0])

    if verbose: 
        v = time.time() - start
        print('\nfinished tracing catchments in {:.1f} seconds\n'.format(v))

    if opposite: return points
    else: 
        if verbose: 
            print('\ntrace failed to reach opposite side, ' +
                  'trying alternate method')
        raise

def get_distance(p1, p2):
    """
    Returns the distance between p1 and p2.
    """

    x1, y1 = p1
    x2, y2 = p2

    return (x2 - x1)**2 + (y2 - y1)**2

def find_closest(p, points):
    """
    Returns the closest point in the list "points" to the point "p."
    """

    distances = [get_distance(p, point) for point in points]

    return points[distances.index(min(distances))]

def get_centroid(points):
    """
    Calculates the centroid of a polygon with paired x-y values.
    """

    xs = numpy.array([x for x, y in points] + [points[0][0]])
    ys = numpy.array([y for x, y in points] + [points[0][1]])

    a = xs[:-1] * ys[1:]
    b = ys[:-1] * xs[1:]

    A = numpy.sum(a - b) / 2.

    cx = xs[:-1] + xs[1:]
    cy = ys[:-1] + ys[1:]

    Cx = numpy.sum(cx * (a - b)) / (6. * A)
    Cy = numpy.sum(cy * (a - b)) / (6. * A)

    return Cx, Cy

def next_index(l, 
               i,
               ):
    """
    Returns the index "i" of the next point in the list "l."  Work around 
    for reaching the end to go to the start.
    """

    if i == len(l) - 1: i  = 0
    else:               i += 1

    return i

def find_neighbors(j,
                   shapes,
                   bboxes,
                   ):
    """
    Returns the neighbors of the list "shapes" with corresponding "bboxes" 
    that overlap with the bounding box "bbox."
    """

    xmin, ymin, xmax, ymax = bboxes[j]

    indices = []
    for i, b in enumerate(bboxes):
        
        if xmin < b[2] and xmax < b[0]: x = False
        else: x = True
        if ymin < b[3] and ymax < b[1]: y = False
        else: y = True

        if x and y and b != bboxes[j]: indices.append(i)

    return [shapes[i] for i in indices]

def combine_shapes(shapes, 
                   verbose = True,
                   ):
    """
    Combines a list of shapes into a single shape.  Assumes the shapes are 
    simply connected with one common boundary.  Also assumes the points are in 
    clockwise order.  Starts at the smallest "x" value (longitude) and traces
    the first shape until reaching a junction, then figures out which direction
    to go next until reaching the first shape again.
    """

    st = time.time()

    # in the event of a single shape just return it

    if len(shapes) == 1: 

        return [(round(x, 6), round(y, 6)) for x, y in shapes[0].points]

    # find all the edge points (only appear once)

    all_points = [(round(x, 6), round(y, 6)) 
                  for shape in shapes for x, y in shape.points]

    if verbose: print('found', len(all_points), 'points')

    # find all points that aren't duplicated (interior points are duplicated)

    edges = [p for p, count in collections.Counter(all_points).items()
             if count == 1]

    # round everything to enable checking and keep track of the bounding boxes
    # get rid of first and last points

    bboxes    = []
    formatted = []
    for shape in shapes:

        # deal with shapes with multiple parts

        points = [(round(x, 6), round(y, 6)) for x,y in shape.points]
        duplicates = [p for p, count in collections.Counter(points).items()
                      if count > 1]

        points = [p for p in points if p not in duplicates]

        formatted.append(points)
        bboxes.append(shape.bbox)

    # keep the formatted shapes and bounding boxes

    bboxes = [b for s, b in zip(formatted, bboxes)]
    shapes = formatted

    # get rid of shapes that have no edge points

    if verbose: print('removing shapes with no edge points')

    keepers = [i for i,s in enumerate(shapes) if any([p in edges for p in s])]

    if verbose:

        its = len(shapes), len(keepers)
        print('intially {} shapes; finally {} shapes'.format(*its))

    bboxes  = [bboxes[i] for i in keepers]
    shapes  = [shapes[i] for i in keepers]

    if verbose: print('found', len(edges), 'points')

    # find the smallest value of x (which is an outside point) and start there
    # also use the shape with the max x as a check the trace made it around

    xs, ys = zip(*edges)

    xmin = min(xs)
    xmax = max(xs)
    ymin = min(ys)
    ymax = max(ys)

    start    = edges[xs.index(xmin)]
    opposite = edges[xs.index(xmax)]

    # "current" is current shape being traced (as list), "points" are the trace

    current = shapes[[min(shape)[0] for shape in shapes].index(xmin)]
    i = current.index(start)

    # keep the results in a list

    points = []

    # iterate through using the existence of the opposite side point as
    # the criteria that the trace went around

    while opposite not in points or current[i] != start:

        # index of the current shape

        j = shapes.index(current)

        if verbose: 
            
            its = j, current[i][0], current[i][1]
            print('started  tracing shape ' +
                  '{:3d} starting at {:8.4f}, {:7.4f}'.format(*its))

        # find the neighbors

        neighbors = find_neighbors(j, shapes, bboxes)

        # trace the current shape until reaching a point not on the edge
    
        while current[i] in edges:
            points.append(current[i])
            edges.remove(current[i])
            i = next_index(current, i)

        if verbose:
            its = j, current[i][0], current[i][1]
            print('finished tracing shape ' +
                  '{:3d} starting at {:8.4f}, {:7.4f}'.format(*its))

        # make a list of the candidates for the next shape to trace

        candidates = [neighbor for neighbor in neighbors 
                      if current[i] in neighbor]

        if current[i] == start: pass
        elif len(candidates) == 0:

            # couldn't find anything

            if verbose:

                print('unable to find neighboring shape at point ' +
                      '{:8.4f}, {:7.4f} '.format(*current[i]) + 
                      'in shape {};\n'.format(j) +
                      'searching for closest point'.format(j))

            # find the closest edge point

            closest = find_closest(current[i], edges)

            if verbose:

                print('found close point {:8.4f}, {:7.4f}'.format(*closest))

            # find the shape

            n = 0
            while closest not in shapes[n] and n < len(shapes) - 1: n += 1

            i = shapes[n].index(closest)
            current = shapes[n]

            # remove the last point, since it's in a weird place

            l = points.pop(-1)

        elif len(candidates) == 1:

            # only one choice, find the index of the starting point

            points.append(current[i])
            if current[i] in edges: edges.remove(current[i])    

            i = candidates[0].index(current[i])
            current = candidates[0]

        else:

            if verbose: print('multiple candidates detected')

            # find the candidate with an edge point

            candidate_edges = [[p for p in candidate if p in edges]
                               for candidate in candidates]
            winner = candidates[candidate_edges.index(max(candidate_edges))]
            i = winner.index(current[i])

            # remove the last point, since it's in a weird place

            l = points.pop(-1)

            current = winner

        # work around for straight intersections

        if current[i] != start and current[next_index(current, i)] not in edges:
           
            if verbose: print('went the wrong way, finding the closest edge')

            l = points.pop(-1)

            # find the closest edge point

            closest = find_closest(points[-1], edges)

            if verbose:
                
                print('found close point {:8.4f}, {:7.4f}'.format(*closest))

            # find the shape

            n = 0
            while closest not in shapes[n] and n < len(shapes) - 1: n += 1

            i = shapes[n].index(closest)
            current = shapes[n]

        # add the point

        if current[i] not in points: points.append(current[i])
        if current[i] in edges: edges.remove(current[i])

        if current[i] != start: i = next_index(current, i)

    if verbose: 
        v = time.time() - st
        print('\nfinished tracing catchments in {:.1f} seconds\n'.format(v))

    if opposite in points: return points
    else: 
        print('\ntrace failed to reach opposite side, ' +
              'trying alternate method')
        raise

def merge_shapes(inputfile, 
                 outputfile = None, 
                 overwrite = False, 
                 verbose = True, 
                 vverbose = False,
                 ):
    """
    Merges all the shapes in a shapefile into a single shape.
    """

    if outputfile is None: output = '{}/merged'.format(os.getcwd())

    if os.path.isfile(outputfile + '.shp') and not overwrite:
        if verbose: 
            print('combined watershed shapefile {} exists'.format(outputfile))
        return
   
    if verbose: print('combining shapes from {}\n'.format(inputfile) + 
                      'this may take a while...\n')

    # start by copying the projection files

    shutil.copy(inputfile + '.prj', outputfile + '.prj')

    # load the catchment and flowline shapefiles

    r = Reader(inputfile, shapeType = 5)

    try: 

        combined = combine_shapes(r.shapes(), verbose = vverbose)

    except:

        print('error: unable to combine shapes')
        raise

    # create the new file with the merged shapes

    w = Writer(shapeType = 5)

    w.poly(shapeType = 5, parts = [combined])

    # copy the fields from the original and then the first record; note this
    # can be adapted as needed

    for field in r.fields: w.field(*field)
    w.record(*r.record(0))

    w.save(outputfile)

    if verbose: 

        its = inputfile, outputfile
        print('successfully combined shapes from {} to {}\n'.format(*its))

# pretty simple to use--point the inputfile to the shapefile with the shapes
# you want to merge (do NOT include the extension .shp, .prj, etc.) and the 
# name you want for the output file (if unspecified it will be called "merged"
# and placed in the current directory).

#inputfile = r'C:\HSPF_data\07100008\22248777\catchments'
#outputfile = r'C:\HSPF_data\07100008\22248777\merged'

#merge_shapes(inputfile, outputfile = outputfile)
