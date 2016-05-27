# ftable.py
#
# Estimates FTABLES from flow and velocity for HSPF
#
# last updated: 05/19/2016
# 
# David J. Lampert
#

import math

def get_h(f, 
          v,
          ):
    """
    Returns the bottom width (m) of a trapezoidal channel with flowrate "f" 
    (cfs) and velocity "v" (fps) assuming a 1:1 side slope.
    """

    return math.sqrt(f / v / 2 * 0.3048**2)

def get_A(x, 
          h,
          ):
    """
    Returns the area of a trapezoid with bottom width "h" and height "x".
    """

    return (x * h + x**2)

def get_P(x, 
          h, 
          slope,
          ):
    """
    Returns the wetted perimeter of a trapezoid with bottom width "h" and 
    height "x" and side slope "slope".
    """

    factor = 1 / math.sin(math.atan(slope))

    return (h + 2 * factor * x)

def manning_flow(A, 
                 P, 
                 S, 
                 n = 0.05,
                 ):
    """
    Estimates the flow (m3/s) using Manning's Equation for a given area 
    "A" (m2), perimeter "P" (m), slope "S", and roughness coefficient "n".
    """

    return (1 / n * A**(5/3) / P**(2/3) * S**0.5)

def manning_avg(h, 
                S, 
                n = 0.05,
                ):
    """
    Estimates the flow of a trapezoidal channel using manning's equation
    assuming bottom width h, with 1:1 side slopes of height h.
    """

    A = get_A(h, h)
    P = get_P(h, h, 1)

    return manning_flow(A, P, S, n = n)
    
def make_ftable(f, 
                v, 
                L, 
                s, 
                s0 = 1, 
                s1 = 0.5, 
                s2 = 0.5,
                ):
    """
    Makes an ftable for a reach based on some assumptions about the 
    geometry of the flood plain detailed below.

    f  -- average flow (cfs)
    v  -- average velocity (fps)
    L  -- reach length (km)
    s  -- reach average slope
    s0 -- main channel side slope
    s1 -- lower flood plain side slope
    s2 -- upper flood plain side slope
    """

    # estimate the depth at average flow using velocity and trap x-section

    h = get_h(f, v)

    # convert the flow to m3/s

    fm3s = f * 0.3048**3

    # use manning equation to find h for the average flow

    for i in range(5):
        q = manning_avg(h, s)
        h = (fm3s / q)**(1/3) * h

    # store the ftable in a list of lists

    ftable = [[0., 0., 0., 0.]]

    # get the info for the main channel (assumed 25% more than average flow)

    for x in [0.25 * h, 0.5 * h, 0.75 * h, h, 1.25 * h]:

        A  = get_A(x, h)
        P  = get_P(x, h, s0)
        SA = (2 * x / s0 + h) * L / 10
        V  = A * L / 1000
        q  = manning_flow(A, P, s)

        ftable.append([x, SA, V, q])
        
    # get the info for the lower flood plane (1.25 * h < x < 1.875 * h)

    x1 = 1.25 * h                  # lower flood plain start depth
    A1 = get_A(x1, h)              # cumulative area below flood plain
    P1 = get_P(x1, h, s0) + 6 * h  # cumulative perimeter below flood plain
    b1 = 9.5 * h                   # lower flood plain base width

    for x in [1.25 * x1, 1.5 * x1]:

        A  = A1 + get_A(x - x1, b1)
        P  = P1 + get_P(x - x1, b1, s1) - b1
        SA = (2 * (x - x1) / s1 + b1) * L / 10
        V  = A * L / 1000
        q  = manning_flow(A, P, s)
    
        ftable.append([x, SA, V, q])

    # get the info for the upper flood plane (1.875 * h < x < 60 * h)

    x2 = 1.5 * x1                          # upper flood plain start depth
    A2 = A1 + get_A(x2 - x1, b1)           # cumulative area below 
    P2 = P1 + get_P(x2 - x1, b1, s1) - b1  # cumulative perimeter below
    b2 = b1  + 2 * (x2 - x1) / s1          # upper flood plain base width

    for x in [2 * h, 3 * h, 5 * h, 10 * h, 20 * h, 40 * h, 60 * h]:

        A  = A2 + get_A(x - x2, b2)
        P  = P1 + get_P(x - x2, b2, s2) - b2
        SA = (2 * (x - x2) / s2 + b2) * L / 10
        V  = A * L / 1000
        q  = manning_flow(A, P, s)
    
        ftable.append([x, SA, V, q])
    
    return ftable

def spillway(qavg,
             r = 40,
             gravity = 9.81,
             ):
    """
    Estimates the critical depth (m) and spillway width from the average 
    flow (m3/s), gravitational constant, and assumed width to depth ratio r.
    """

    yc = (qavg**2 / r / gravity)**0.2
    
    return yc, r * yc

def discharge_constant(q, 
                       h,
                       ):
    """
    Calculates the discharge constant for the flow (m3/s) at a given 
    depth (m).
    """

    return (q / h**1.5)

def discharge(k, 
              h,
              ):
    """
    Calculates the flow using the discharge equation/constant (assumes units
    are consistent).
    """

    return (k * h**1.5)

def lake_ftable(qavg, 
                vavg, 
                L, 
                s, 
                dam,
                n = 10,
                ):
    """
    Makes an ftable for a lake reach based on a dam.

    qavg  -- average flow (cfs)
    vavg  -- average velocity (fps)
    L     -- reach length (km)
    s     -- reach slope
    dam   -- an instance of the Dam class
    n     -- number of segments between spillway height and max lake depth
    """

    # lake characterstics

    depth        = dam.height       # dam height (ft)
    norm_storage = dam.norm_storage # storage at dam height (acre-ft)
    max_storage  = dam.max_storage  # max storage (acre-ft)
    area         = dam.surface_area # surface area at dam height (acres)

    # convert to meters, Mm3 and hectares and check for data availability

    norm_volume = norm_storage * 0.0012335
    max_volume  = max_storage  * 0.0012335
    max_depth   = depth        * 0.3048

    if area == 0: 

        # no area available; roughly assume volume = depth * area / 3

        conv = 100 # Mm3 / m to ha
        
        area = 3 * norm_volume / depth * conv

    else:

        conv = 0.4049 # acres to ha

        area = area * conv

    # estimate the average depth of the reach assuming a cubic relationship
    # between volume and depth using the max values

    avg_depth = max_depth * (norm_volume / max_volume)**(1/3)

    # estimate the critical depth (m) and spillway width (m)
    
    yc, b = spillway(qavg * 0.3048**3)
          
    # calculate the scaling constant for the flow at average depth

    k = discharge_constant(qavg * 0.3048**3, yc)

    # calculate the spillway height from the upstream and critical depths

    height = avg_depth - yc
    
    # first row of the ftable is all zeros

    ftable = [[0,0,0,0]]

    # second row for dead storage
    # assume a quadratic increase in surface area and a cubic increase in volume

    a = (height / avg_depth)**2 * area
    v = (height / avg_depth)**3 * norm_volume
    
    ftable.append([height, a, v, 0])

    # third row for half normal spillway depth

    a = ((height + yc / 2) / avg_depth)**2 * area
    v = ((height + yc / 2) / avg_depth)**3 * norm_volume
    q = discharge(k, yc / 2)
    
    ftable.append([height + yc / 2, a, v, q])
        
    # calculate the ftable values for each depth

    for i in range(n + 1):

        # relative height of the lake above the dam
        # x = (depth - avg_depth) / (maxdepth - avg_depth)

        x = i / n
        
        # depth of flow upstream of the dam

        y = x * (max_depth - avg_depth) + avg_depth

        # depth of flow over the dam
        
        d = y - height

        # assume a quadratic increase in surface area with depth

        a = (y / avg_depth)**2 * area

        # assume a cubic increase in volume with depth

        v = (y / avg_depth)**3 * norm_volume

        # use the discharge equation to estimate flow

        q = discharge(k, d)

        # append the ftable

        ftable.append([y, a, v, q])

    # add rows to the ftable until reaching 17

    while len(ftable) < 17:
        last = ftable[-1]
        ftable.append([last[0] + 0.1 * (max_depth - avg_depth),
                       last[1] * 1.2,
                       last[2] * 1.3,
                       last[3] * 1.5])

    return ftable
