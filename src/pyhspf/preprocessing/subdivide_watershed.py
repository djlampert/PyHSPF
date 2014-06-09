# Subdivide Watershed
#                                                                             
# David J. Lampert, PhD, PE                                                   
#                                                                             
# last updated: 12/03/2012                                                    
#                                                                              
# Purpose: Analyzes a watershed and subdivides it according to the criteria
# specified below.
                                                                  
import os, shutil, time, pickle

from shapefile import Reader, Writer

from pyhspf.preprocessing.find_flowlines  import find_flowlines
from pyhspf.preprocessing.dbf             import read_dbf

#from preprocessing.make_subbasin_outlets      import make_subbasin_outlets
#from preprocessing.make_subbasin_flowlines    import make_subbasin_flowlines
#from preprocessing.combine_flowlines          import combine_flowlines
#from preprocessing.combine_subbasin_flowlines import combine_subbasin_flowlines

def get_distance(p1, p2):
    """Returns the square of the distance between two points."""

    return((p1[0] - p2[0])**2 + (p1[1] - p2[1])**2)

def make_subbasin_outlets(HUC8, attributefile, gagefile, damfile, flowfile, 
                          outletfile, inletfile, subbasinfile, drainmin = None, 
                          drainmax = None, first_year = 2001, last_year = 2009,
                          extras = None, verbose = True):
    """Goes through watershed composed of flowlines in the "all_comids" list 
    and subdivides the list into lists that flow into each of the gage stations 
    contained in the gage_comid list plus at the outlet of the watershed 
    using data from NHDPlus.  Creates a feature class of outlets containing
    all the data needed for HSPF simulations. Returns a dictionary linking 
    the flowlines comids corresponding to the gage stations and a list of the 
    comids of all the upstream flowlines. Note that flowlines that do not
    flow into the gages will not be included in this list
    """

    if verbose: print('subdividing watershed\n')

    # use criteria for subbasin delineation to make a list of inlets and outlets

    inlets = []

    if extras is None: outlets = []
    else:              outlets = extras

    # open up the flowline data in a dictionary using hydroseqs as keys and make
    # a dictionary linking the comids to hydroseqs

    with open(attributefile, 'rb') as f: flowlines = pickle.load(f)

    hydroseqs  = {flowlines[f].comid: f for f in flowlines}

    # find the dam comids

    dam_comids = find_flowlines(damfile, flowfile)

    for comid in dam_comids: 
        if comid is not None and comid in hydroseqs: outlets.append(comid)

    # read the dam file to find the outlet points

    damreader  = Reader(damfile, shapeType = 1)
    dampoints  = [s.points[0] for s in damreader.shapes()]
    damrecords = damreader.records()

    nid_index = damreader.fields.index(['NIDID', 'C', 7, 0]) - 1

    # start with the USGS NWIS gages

    gage_comids = find_flowlines(gagefile, flowfile)

    # check the gages and see if they meet the criteria for outlets

    gagereader  = Reader(gagefile, shapeType = 1)
    gagerecords = gagereader.records()

    # figure out which field codes are the HUC8, the first day, the site
    # number, the drainage area, and the average 

    day1_index  = gagereader.fields.index(['DAY1',       'N', 19, 0]) - 1
    dayn_index  = gagereader.fields.index(['DAYN',       'N', 19, 0]) - 1
    drain_index = gagereader.fields.index(['DA_SQ_MILE', 'N', 19, 2]) - 1
    HUC8_index  = gagereader.fields.index(['HUC',        'C',  8, 0]) - 1
    site_index  = gagereader.fields.index(['SITE_NO',    'C', 15, 0]) - 1
    nwis_index  = gagereader.fields.index(['NWISWEB',    'C', 75, 0]) - 1
    ave_index   = gagereader.fields.index(['AVE',        'N', 19, 3]) - 1

    min_gage = min([int(r[drain_index]) for r in gagerecords
                    if r[HUC8_index] == HUC8] + [0])
    max_gage = max([int(r[drain_index]) for r in gagerecords
                    if r[HUC8_index] == HUC8] + [0])

    gage_outlets = []
    last_HUC     = []
    next_HUC     = None
    for record, comid in zip(gagerecords, gage_comids):

        # make sure there are data, the drainage area is larger than min 
        # (note the units are different), the gage is not an inlet, and 
        # that it's in the watershed

        if comid is not None:
            if (int(str(record[day1_index])[:4]) <= last_year  and
                int(str(record[dayn_index])[:4]) >= first_year and
                record[drain_index] > drainmin / 2.59          and  
                flowlines[hydroseqs[comid]].up in flowlines    and 
                record[HUC8_index] == HUC8):

                if comid not in outlets:
                    gage_outlets.append(comid)

                    if verbose: 
                        print('adding outlet %d for gage station %s' % 
                              (comid, record[site_index]))

        # get the next and last HUC8s 
        
        if int(record[HUC8_index]) != HUC8:
            if int(record[drain_index]) < min_gage:
                last_HUC.append(record[HUC8_index])
            elif int(record[drain_index]) > max_gage:
                next_HUC = record[HUC8_index]

    down_gages = [r for r in gagerecords if r[HUC8_index] == next_HUC]
    up_gages   = [r for r in gagerecords if r[HUC8_index] in last_HUC]

    gagereader = None

    # add the gage stations meeting the criteria as outlets

    for comid in gage_outlets:
        outlets.append(comid)
        if verbose: print('adding outlet %d for gage station' % comid)

    # find all the inlets

    for f in flowlines:
        if flowlines[f].up not in flowlines and flowlines[f].up != 0:
            inlets.append(flowlines[f].comid)

    # find the watershed outlet using the drainage area

    max_area   = max([flowlines[f].drain for f in flowlines])
    last_comid = [flowlines[f].comid for f in flowlines 
                  if flowlines[f].drain == max_area][0]

    if last_comid not in outlets: outlets.append(last_comid)\

    # check to see if there are two flowlines feeding the watershed outlet

    for f in flowlines:
        if (flowlines[f].down == flowlines[hydroseqs[last_comid]].down and
            flowlines[f].comid != last_comid):
            print('adding outlet for second watershed outlet at', 
                  flowlines[f].comid, '\n')
            outlets.append(flowlines[f].comid)

    # trace the main channels from the inlet hydroseqs

    main = []
    for inlet in inlets:
        flowline = flowlines[hydroseqs[inlet]]
        if flowline not in main: main.append(flowline)
        while flowline.down in flowlines:
            flowline = flowlines[flowline.down]
            if flowline not in main: main.append(flowline)

    # make the main channel if there is no inlet

    if len(inlets) == 0:
        flowline = flowlines[hydroseqs[last_comid]]
        main.append(flowline)
        while flowline.up != 0:
            flowline = flowlines[flowline.up]
            main.append(flowline)

    # add outlets to connect outlets to the main channel as needed

    for outlet in outlets:

        flowline = flowlines[hydroseqs[outlet]]

        # check that it isn't the watershed outlet

        if flowline.down in flowlines:

            # check if it's connected

            if flowline not in main:

                if verbose: print(flowline.comid, 'is not connected')

                # then need to add outlets to connect to the main line

                while flowlines[flowline.down] not in main:
                    main.append(flowline)
                    flowline = flowlines[flowline.down]
                    if flowline.down not in flowlines: 
                        if verbose: print('reached the watershed outlet')
                        break

                if flowline.comid not in outlets: 
                    outlets.append(flowline.comid)
                    main.append(flowline)
                    if verbose: print('adding outlet %d for connectivity' % 
                                      flowline.comid)

                # add outlets for any others streams at the junction

                others = [flowlines[f] for f in flowlines 
                          if (flowlines[f].down == flowline.down and
                              flowlines[f] != flowline)]

                for other in others:

                    if other.comid not in outlets:
                        outlets.append(other.comid)
                        if verbose: print('adding another outlet ' +
                                          '%d for connectivity' % 
                                          other.comid)
    
    # check the drainage areas to make sure subbasins are not too large
    # start at the main outlet and move upstream adding outlets as needed

    if drainmax is None: drainmax = max_area

    n = -1
    while len(outlets) != n:

        if verbose: print('checking outlet conditions\n')

        # move upstream and look at the changes in drainage area for each outlet

        n = len(outlets)

        for outlet in outlets:
            flowline = flowlines[hydroseqs[outlet]] # current flowline
            boundaries     = [0] + [hydroseqs[b] for b in inlets + outlets]
            drain_area     = flowline.divarea

            # check until reaching another outlet or the top of the watershed
            # additional checks for max basin drainage area and major tributary

            while flowline.up not in boundaries:

                # find all the tributaries

                tributaries = [f for f in flowlines 
                               if flowlines[f].down == flowline.hydroseq]

                # find the major tributary

                major = flowlines[flowline.up]

                # if any of the tributaries are an outlet or if the minor 
                # tributaries exceeds drainage max, then make them all outlets

                if (any([flowlines[f].comid in outlets for f in tributaries]) or
                    flowline.divarea - major.divarea > drainmax):

                    for f in tributaries: 
                        if flowlines[f].comid not in outlets: 
                            outlets.append(flowlines[f].comid)

                            if verbose: 
                                print('adding outlet %d for major tributary' %
                                      flowlines[f].comid)

                    break

                elif drain_area - flowline.divarea > drainmax:

                    if flowlines[flowline.down].comid not in outlets: 
                        outlets.append(flowlines[flowline.down].comid)

                        if verbose: 
                            print('adding outlet %d for drainage area' % 
                                  flowlines[flowline.down].comid)

                    break


                else: flowline = flowlines[flowline.up]

    # group flowlines into sub basins associated with each outlet

    subbasins = {outlet: [outlet] for outlet in outlets}

    # check to see if there are multiple outlets at the watershed outlet

    downhydroseq = flowlines[hydroseqs[last_comid]].down

    for comid in hydroseqs:
        if (flowlines[hydroseqs[comid]].down == downhydroseq and 
            comid not in outlets):
            subbasins[last_comid].append(comid)

    # go through each gage and make a list of all the comids in the subbasin

    for subbasin in subbasins:
        current = [hydroseqs[outlet] for outlet in subbasins[subbasin]]
        while len(current) > 0:
            last    = current[:]
            current = []

            for comid in hydroseqs:
                if (flowlines[hydroseqs[comid]].down in last and 
                    comid not in outlets):

                    subbasins[subbasin].append(comid)
                    current.append(hydroseqs[comid])

    # make a shapefile containing the outlet points

    if verbose: print('copying the projections\n')

    # start by copying the projection files

    shutil.copy(flowfile + '.prj', outletfile + '.prj')
    shutil.copy(flowfile + '.prj', inletfile  + '.prj')

    # read the flowline and gage files

    flowreader  = Reader(flowfile, shapeType = 3)
    flowrecords = flowreader.records()

    # read the gage file

    gagereader  = Reader(gagefile, shapeType = 1)
    gagepoints  = [s.points[0] for s in gagereader.shapes()]

    # figure out which field codes are the Reach code and comid in the flow file

    comid_index = flowreader.fields.index(['COMID',     'N',  9, 0]) - 1
    reach_index = flowreader.fields.index(['REACHCODE', 'C', 14, 0]) - 1
    gnis_index  = flowreader.fields.index(['GNIS_NAME', 'C', 65, 0]) - 1

    # make a list of the comids

    comids = [record[comid_index] for record in flowrecords]

    # make the inlet file

    if len(inlets) > 0:

        w = Writer(shapeType = 1)

        w.field(*['COMID',      'N',  9, 0])
        w.field(*['REACHCODE',  'C', 14, 0])
        w.field(*['SITE_NO',    'C', 15, 0])
        w.field(*['DRAIN_SQKM', 'N', 15, 3])
        w.field(*['AVG_FLOW',   'N', 15, 3])
        w.field(*['GNIS_NAME',  'C', 65, 0])
        w.field(*['NWISWEB',    'C', 75, 0])

        for inlet in inlets:
            index = comids.index(inlet)
            shape = flowreader.shape(index)
            point = shape.points[0]

            # get the parameters from the flow file

            reachcode = flowrecords[index][reach_index]
            comid     = flowrecords[index][comid_index]
            gnis      = flowrecords[index][gnis_index]

            if isinstance(gnis, bytes): gnis = gnis.decode().strip()

            # get the area from the flowline database

            area = flowlines[hydroseqs[inlet]].drain

            if inlet in gage_outlets:#gage_comids:

                distances = [get_distance(point, p) for p in gagepoints]
                closest   = distances.index(min(distances))

                site_no = gagerecords[closest][site_index]
                nwis    = gagerecords[closest][nwis_index]
                flow    = round(gagerecords[closest][ave_index], 3)

            else:

                site_no = ''
                nwis    = ''

                # estimate the flow from the nearest gage, start by going 
                # upstream until reaching a gage comid and get the drainage area

                next_gage = flowlines[hydroseqs[comid]]
                current_area = next_gage.drain

                while (next_gage.comid not in gage_comids and 
                       next_gage.down in flowlines):
                    next_gage = flowlines[next_gage.down]
                if next_gage.comid == last_comid:
                    flow = round(max([record[ave_index] 
                                      for record in gagerecords]), 3)
                else:
                    # get the flow from the gage file (note units)

                    distances = [get_distance(point, p) for p in gagepoints]
                    closest   = distances.index(min(distances))
                    next_flow = gagerecords[closest][ave_index]
                    next_area = gagerecords[closest][drain_index] * 2.59

                    flow = round(next_flow * current_area / next_area, 3)

            w.point(point[0], point[1])
            w.record(comid, reachcode, site_no, area, flow, gnis, nwis)
    
        w.save(inletfile)

    # create the outlet point file that will store the comid and reachcode

    w = Writer(shapeType = 1)

    w.field(*['COMID',      'N',  9, 0])
    w.field(*['REACHCODE',  'C', 14, 0])
    w.field(*['NIDID',      'C',  7, 0])
    w.field(*['SITE_NO',    'C', 15, 0])
    w.field(*['DRAIN_SQKM', 'N', 15, 3])
    w.field(*['AVG_FLOW',   'N', 15, 3])
    w.field(*['GNIS_NAME',  'C', 65, 0])
    w.field(*['NWISWEB',    'C', 75, 0])

    for outlet in outlets:

        # find the flowline and use the last point as the outlet

        index = comids.index(outlet)
        shape = flowreader.shape(index)
        point = shape.points[-1]

        # get the parameters from the flow file

        reachcode = flowrecords[index][reach_index]
        comid     = flowrecords[index][comid_index]
        gnis      = flowrecords[index][gnis_index]

        if isinstance(gnis, bytes): gnis = gnis.decode().strip()

        # get the area from the flowline database

        area = flowlines[hydroseqs[outlet]].divarea

        # find the nearest dam if the outlet is co-located with a dam

        if outlet in dam_comids:

            distances = [get_distance(point, p) for p in dampoints]
            closest   = distances.index(min(distances))

            dam_no = damrecords[closest][nid_index]

        else:

            dam_no = ''

        # find the nearest gage station if the outlet is co-located with a gage

        if outlet in gage_outlets:

            distances = [get_distance(point, p) for p in gagepoints]
            closest   = distances.index(min(distances))

            site_no = gagerecords[closest][site_index]
            nwis    = gagerecords[closest][nwis_index]
            flow    = round(gagerecords[closest][ave_index], 3)

        else:

            site_no = ''
            nwis    = ''

            # estimate the flow by interpolating from the nearest gage, start
            # by going downstream until reaching a gage comid and get the
            # drainage area and then repeat going upstream

            next_gage = flowlines[hydroseqs[comid]]
            while (next_gage.comid not in gage_comids and 
                   next_gage.down in flowlines):
                next_gage = flowlines[next_gage.down]

            # see if the next_gage is outside the watershed, otherwise get 
            # the flows from the gage file (note units)

            if next_gage.comid == last_comid:
                next_drains = [r[drain_index] for r in down_gages]
                next_index  = next_drains.index(max(next_drains))
                next_flow   = down_gages[next_index][ave_index]
                next_area   = down_gages[next_index][drain_index]
            else: 
                i = comids.index(next_gage.comid)
                next_point = flowreader.shape(i).points[-1]
                distances  = [get_distance(next_point, p) for p in gagepoints]
                closest    = distances.index(min(distances))
                next_flow  = gagerecords[closest][ave_index]
                next_area  = gagerecords[closest][drain_index] * 2.59

            last_gage = flowlines[hydroseqs[comid]]
            while (last_gage.comid not in gage_comids and
                   last_gage.up in flowlines):
                last_gage = flowlines[last_gage.up]

            # see whether it's at the top of the watershed or an inlet
            # otherwise get the flows from the gage file (note units)

            if last_gage.up == 0 or len(up_gages) == 0:
                last_flow = 0
                last_area = 0
            elif last_gage.up not in flowlines: 
                last_drains = [r[drain_index] for r in up_gages]
                last_index  = last_drains.index(max(last_drains))
                last_flow   = up_gages[last_index][ave_index]
                last_area   = up_gages[last_index][drain_index]
            else: 
                i = comids.index(last_gage.comid)
                last_point = flowreader.shape(i).points[-1]
                distances  = [get_distance(last_point, p) for p in gagepoints]
                closest    = distances.index(min(distances))
                last_flow  = gagerecords[closest][ave_index]
                last_area  = gagerecords[closest][drain_index] * 2.59

            if last_flow == next_flow: flow = last_flow
            else:
                flow = round(last_flow + (next_flow - last_flow) * 
                             (area - last_area) / (next_area - last_area), 3)

        w.point(point[0], point[1])
        w.record(comid, reachcode, dam_no, site_no, area, flow, gnis, nwis)
    
    w.save(outletfile)

    with open(subbasinfile, 'wb') as f: pickle.dump(subbasins, f)

    return subbasins

def make_subbasin_flowlines(flowfile, comids, output = None, verbose = True):
    """Makes a shapefile containing the major flowlines above a USGS gage
    within a HUC8.
    """

    if output is None: output = os.getcwd() + '/subbasin_flowlines'

    projection = flowfile + '.prj'

    # start by copying the projection files

    shutil.copy(projection, output + '.prj')

    # open the flowline shapefile
  
    shapefile = Reader(flowfile, shapeType = 3)
    records   = shapefile.records()

    # figure out which field code is the comid

    comid_index = shapefile.fields.index(['COMID', 'N', 9,  0]) - 1

    # go through the list of flowline comids and find the ones in the subbasin

    if verbose: print('extracting subbasin flowlines')

    indices = []
   
    i = 0
    for record in records:
        if record[comid_index] in comids: indices.append(i)
        i+=1

    # write the data to a new shapefile

    w = Writer(shapeType = 3)

    for field in shapefile.fields:  w.field(*field)

    for i in indices:
        shape  = shapefile.shape(i)

        w.poly(shapeType=3, parts = [shape.points])

        record = records[i]

        # little work around for blank GNIS_ID and GNIS_NAME values

        if isinstance(record[3], bytes):
            record[3] = record[3].decode('utf-8')
        if isinstance(record[4], bytes):
            record[4] = record[4].decode('utf-8')

        w.record(*record)

    w.save(output)

    if verbose: print('successfully extracted subbasin flowlines')

def combine_flowlines(VAAfile, flowfile, output = None, overwrite = False,
                      verbose = True):
    """Makes a shapefile containing the major flowlines above each USGS gage
    within a subbasin based on the NHDPlus dataset.
    """

    if output is None: output = '{}/combined_flowline'.format(os.getcwd())

    if os.path.isfile(output) and not overwrite:
        if verbose: print('combined flowline shapefile %s exists' % output)
        return

    projection = flowfile + '.prj'

    # start by copying the projection files

    shutil.copy(projection, output + '.prj')

    # get the flowline attributes

    with open(VAAfile, 'rb') as f: flowlines = pickle.load(f)

    # all the fields for the combined flowline feature class

    fields = [['OutComID', 'N', 9, 0], 
              ['GNIS_NAME', 'C', 65, 0],
              ['REACHCODE', 'C', 8, 0],
              ['InletComID', 'N', 9, 0],
              ['MaxElev', 'N', 9, 2],
              ['MinElev', 'N', 9, 2],
              ['SlopeLenKM', 'N', 6, 2],
              ['Slope', 'N', 8, 5],
              ['InFlowCFS', 'N', 8, 3],
              ['OutFlowCFS', 'N', 8, 3],
              ['VelFPS', 'N', 7, 4],
              ['TravTimeHR', 'N', 8, 2]]

    # go through the reach indices, add add them to the list of flowlines if
    # they are in the watershed, and make a list of the corresponding comids
  
    shapefile = Reader(flowfile, shapeType = 3)
    records   = shapefile.records()

    # figure out which field code is the comid, reachcode, and gnis name

    comid_index = shapefile.fields.index(['COMID',     'N',  9, 0]) - 1
    reach_index = shapefile.fields.index(['REACHCODE', 'C', 14, 0]) - 1
    gnis_index  = shapefile.fields.index(['GNIS_NAME', 'C', 65, 0]) - 1

    all_comids = [r[comid_index] for r in records]
    
    # make a dictionary linking the hydrologic sequence

    updown = {f: flowlines[f].down for f in flowlines 
              if flowlines[f].comid in all_comids}
    downup = {f: flowlines[f].up   for f in flowlines
              if flowlines[f].comid in all_comids}

    # pick a flowline and follow it to the end of the watershed

    current = list(updown.keys())[0]

    while updown[current] in updown: current = updown[current]

    primary = [current]
    while downup[current] in downup:
        current = downup[current]
        primary.insert(0, current)

    inlet_comid = flowlines[primary[0]].comid
    last_comid  = flowlines[primary[-1]].comid

    inlet_flow  = round(flowlines[primary[0]].flow, 3)
    outlet_flow = round(flowlines[primary[-1]].flow, 3)
    velocity    = round(flowlines[primary[-1]].velocity, 4)
    traveltime  = round(sum([flowlines[f].traveltime for f in primary]), 3)

    # use the attributes for the last flowline for the combined flowline

    top    = flowlines[primary[0]].maxelev
    bottom = flowlines[primary[-1]].minelev
    length = round(sum([flowlines[f].length for f in primary]), 2)

    estimated_slope = round((top - bottom) / 100000 / length, 6)

    if estimated_slope < 0.00001: slope = 0.00001
    else:                         slope = estimated_slope

    # write the data from the HUC8 to a new shapefile

    w = Writer(shapeType = 3)

    # the fields for the shape will be effluent comid, GNIS name, the length 
    # (km), the 8-digit reach code, the slope, the flow at the inlet and outlet,
    # the velocity in ft/s, and the travel time in hours

    for field in fields: w.field(*field)

    last_index = all_comids.index(last_comid)
    if isinstance(records[last_index][gnis_index], bytes):
        gnis = records[last_index][gnis_index].decode('utf-8')
    else: gnis = records[last_index][gnis_index]

    r = [last_comid, gnis, records[last_index][reach_index][:8], inlet_comid, 
         top, bottom, length, slope, inlet_flow, outlet_flow, velocity, 
         traveltime]

    w.record(*r)

    points = []
    for f in primary:
        shape = shapefile.shape(all_comids.index(flowlines[f].comid))

        for p in shape.points:
            if p not in points: points.append(p)

    w.poly(shapeType = 3, parts = [points])

    w.save(output)

    if verbose: 
        print('successfully combined subbasin {} flowlines'.format(last_comid))

def combine_subbasin_flowlines(directory, comids, output, overwrite = False, 
                               verbose = True):
    """Combines outlet subbasin flowlines for an 8-digit hydrologic unit into 
    a single shapefile.  Assumes directory structure of:

    path_to_HUC8\comids\combined_flowline.shp 
    
    where comids are all the elements in a list of the subbasin outlets from 
    the NHDPlus dataset.
    """

    l = Writer(shapeType = 3)
    projection = None
    fields     = None

    for comid in comids:
        filename = directory + '/%d/combined_flowline' % comid
        if os.path.isfile(filename + '.shp'):
            if verbose: print('found combined file %s\n' % filename)

            # start by copying the projection files

            if projection is None:
                projection = output + '.prj'
                shutil.copy(filename + '.prj', projection)

            # read the new file
  
            r = Reader(filename, shapeType = 3)

            if fields is None:
                fields = r.fields
                for field in fields: l.field(*field)

            shape = r.shape(0)

            # write the shape and record to the new file

            l.poly(shapeType = 3, parts = [shape.points])
            record = r.record(0)
            if isinstance(record[1], bytes): record[1] = ''
            if record[1] == 65 * ' ': record[1] = ''
            l.record(*record)

        elif verbose: print('unable to locate %s\n' % filename)

    if fields is not None:  
        l.save(output)
        if verbose: print('successfully combined flowline shapefiles')
    elif verbose: print('warning: unable to combine flowline shapefiles')

def subdivide_watershed(HUC8, flowfile, gagefile, damfile, extra_outlets = None,
                        drainmin = None, drainmax = None, outputpath = None,
                        verbose = True, vverbose = False):
    """Analyzes the GIS data, subdivides the watershed into subbasins that are
    co-located with the gages and have drainage areas no larger than the max
    specified.

    extra_outlets -- a list of longitudes and latitudes for any subbasin outlets
    drainmin      -- the minimum drainage area for a subbasin outlet
    outputpath    -- path to write output
    """

    start = time.time()

    if outputpath is None: output = os.getcwd()
    else:                  output = outputpath

    # subdivide the watershed using the USGS NWIS stations and any additional
    # subbasins

    attributefile = output + '/%s/flowlineVAAs'         %  HUC8
    subbasinfile  = output + '/%s/subbasincomids'       %  HUC8
    merged        = output + '/%s/%ssubbasin_flowlines' % (HUC8, HUC8)
    outletfile    = output + '/%s/%ssubbasin_outlets'   % (HUC8, HUC8)
    inletfile     = output + '/%s/%ssubbasin_inlets'    % (HUC8, HUC8)

    if (not os.path.isfile(subbasinfile) or 
        not os.path.isfile(outletfile + '.shp') or
        not os.path.isfile(merged + '.shp')):

        if verbose: print('delineating HSPF watershed for USGS HUC %s\n' % HUC8)

        # add any additional outlets as a list of points (or None) and 
        # divide the flowfiles into subbasins above each of the subbasin outlets
        # subbasins is a dictionary of linking the outlet flowline comid to the 
        # comids of all the tributaries up to the previous outlet

        subbasins = make_subbasin_outlets(HUC8, attributefile, gagefile, 
                                          damfile, flowfile, outletfile, 
                                          inletfile, subbasinfile, 
                                          drainmin = drainmin, 
                                          drainmax = drainmax, 
                                          extras = extra_outlets, 
                                          verbose = vverbose)

    else: 
        if verbose: print('HSPF watershed {} exists\n'.format(HUC8))
        with open(subbasinfile, 'rb') as f: subbasins = pickle.load(f)

    # divide the flowline shapefile into subbasin flowline shapefiles 

    for subbasin in subbasins:
        path = output + '/%s/%d' % (HUC8, subbasin)
        flow = path + '/flowlines'
        
        # make a directory for the output if needed

        if not os.path.isdir(path): os.mkdir(path)

        # extract the flowlines if needed

        if not os.path.isfile(flow + '.shp'):
            make_subbasin_flowlines(flowfile, subbasins[subbasin], output = 
                                    flow, verbose = vverbose)

    # combine the flowlines in each subbasin into a combined shapefile

    for subbasin in subbasins:
        flow     = output + '/%s/%d/flowlines' % (HUC8, subbasin)
        combined = output + '/%s/%d/combined_flowline' % (HUC8, subbasin)
        if not os.path.isfile(combined + '.shp'):
            combine_flowlines(attributefile, flow, output = combined, 
                              verbose = verbose)
    if verbose: print('')

    # merge the flowlines into a single file

    if not os.path.isfile(merged + '.shp'):
        combine_subbasin_flowlines(output + '/%s' % HUC8, subbasins, merged, 
                                   overwrite = True, verbose = vverbose)
        if verbose: print('')

    if verbose: print('successfully divided watershed in %.1f seconds\n' %
                      (time.time() - start))
