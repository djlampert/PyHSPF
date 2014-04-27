from shapefile import Reader

def get_distance(p1, p2):
    """Determines the two-dimensional distance between two points p1 and p2.
    """

    return (p1[0] - p2[0])**2 + (p1[1] - p2[1])**2

def closest_index(point, shapes, warning = False):
    """Determines the index of the shape in the shapefile that is closest to
    the point.
    """

    x, y = point[0], point[1]

    # find all flowlines that have a bounding box around the point

    matches = []

    i = 0
    for shape in shapes:
        bbox = shape.bbox
        xmin, ymin, xmax, ymax = bbox[0], bbox[1], bbox[2], bbox[3]

        if xmin < x and x < xmax and ymin < y and y < ymax: matches.append(i)
        i+=1

    if len(matches) == 0:

        if warning: print('unable to find a flowline with appropriate ' +
                          'bounding box, increasing tolerance\n')

        i = 0
        for shape in shapes:

            bbox = shape.bbox

            xmin = bbox[0] - (bbox[2] - bbox[0])
            xmax = bbox[2] + (bbox[2] - bbox[0])
            ymin = bbox[1] - (bbox[3] - bbox[1])
            ymax = bbox[3] + (bbox[3] - bbox[1])

            if xmin < x and x < xmax and ymin < y and y < ymax: 
                matches.append(i)
            i+=1

    if len(matches) > 1:

        # if more than one bounding box contains the outlet, then find the
        # line with the point closest to the outlet

        if warning:
            print('multiple possible matches found, determining best match\n')

        distances = []
        for i in matches:
            shape   = shapes[i]
            bbox   = shape.bbox
            points = shape.points

            distance = max(bbox[2] - bbox[0], bbox[3] - bbox[1])
            for p in points:
                distance = min(distance, get_distance(p, [x, y]))
            distances.append(distance)

        matches = [matches[distances.index(min(distances))]]

    if len(matches) != 1: 
        if warning: print('warning: unable to determine closest flowline')
        return None
    else: return matches[0]

def find_flowlines(gagefile, flowfile):
    """Determines the COMIDS of the flowlines in the flowline shapefile that 
    correspond to the USGS gages from the gage shapefile.
    """

    flowlines = Reader(flowfile, shapeType = 3)
    outlets   = Reader(gagefile, shapeType = 1)

    points  = [outlet.points[0] for outlet in outlets.shapes()]
    records = outlets.records()

    lines  = flowlines.shapes()

    # find the indices of closest flowline for each point

    indices = [closest_index(point, lines) for point in points]

    # make a dictionary linking the outlet site index numbers to the 
    # corresponding flowline comids

    comid_index = flowlines.fields.index(['COMID', 'N', 9, 0])  - 1

    comids =[]
    for i in indices:
        if i is not None: comids.append(flowlines.record(i)[comid_index])
        else:             comids.append(None)

    return comids
