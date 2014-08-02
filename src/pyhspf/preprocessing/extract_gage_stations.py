import os, shutil

from shapefile import Reader, Writer

from .dbf import read_dbf

def extract_gage_stations(NHDPlus, gagefile, HUC8, output, verbose = True):
    """Extracts the USGS gage stations for a watershed from the gage station 
    shapefile and the 8-digit hydrologic unit code of interest. Removes those
    that have insufficient data or (optionally) not meeting minimum catchment
    drainage area (square miles).  Finally, goes into the next watershed and 
    adds the first gage station.
    """

    database        = NHDPlus + '/NHDPlusAttributes/PlusFlowlineVAA.dbf'
    flowline_source = NHDPlus + '/NHDSnapshot/Hydrography/NHDFlowline'

    # start by copying the projection files

    if verbose: print('copying the projections from the NWIS source\n')

    projection = gagefile + '.prj'

    shutil.copy(projection, output + '.prj')

    # get the gages within the watershed

    if verbose: print('reading the gage file\n')

    sf          = Reader(gagefile, shapeType = 1)
    gagerecords = sf.records()

    HUC8_index  = sf.fields.index(['HUC',  'C', 8, 0]) - 1
    day1_index  = sf.fields.index(['DAY1', 'N', 19, 0]) - 1

    # iterate through the field and determine which points are in the watershed

    if verbose: 
        print('extracting gage stations in the watershed into new file\n')

    gage_indices = []

    i = 0
    for record in gagerecords:
        if record[HUC8_index] == HUC8: gage_indices.append(i)
        i+=1

    if verbose: print('searching for first gage downstream of the basin\n')

    # read hydrologic sequence attribute from the database file

    attributes = ['ComID', 'Hydroseq', 'DnHydroseq', 'UpHydroseq', 'ReachCode']

    values = read_dbf(database, attributes = attributes, verbose = verbose) 

    # find all the comids in the reach

    reach_indices = []

    i = 0
    for entry in values['ReachCode']:
        if entry[:8] == HUC8: reach_indices.append(i)
        i+=1

    # make a dictionary linking the hydrologic sequence and the comids

    updown = {values['Hydroseq'][i]: values['DnHydroseq'][i] 
              for i in reach_indices}
    downup = {values['Hydroseq'][i]: values['UpHydroseq'][i] 
              for i in reach_indices}

    # use the up to down dictionary to find outlets and the reach

    last_comid = None
    for hydroseq in updown: 
        if updown[hydroseq] not in updown:
            last_index = values['Hydroseq'].index(hydroseq)

    last_comid = values['ComID'][last_index]
    exit_index = values['Hydroseq'].index(values['DnHydroseq'][last_index])
    down_reach = values['ReachCode'][exit_index][:8]
    #exit_comid = values['ComID'][exit_index]

    # use the down to up dictionary to find inlets and the reaches

    inlets    = []
    upreaches = []
    for hydroseq in downup: 
        if downup[hydroseq] not in downup and downup[hydroseq] != 0:
            i = values['Hydroseq'].index(hydroseq)
            inlets.append(values['ComID'][i])
            entrance_index = values['Hydroseq'].index(downup[hydroseq])
            upreaches.append(values['ReachCode'][entrance_index][:8])

    # now go back into the gage file and find gages in the preceding and 
    # succeeding reaches

    down_gage_indices = []
    up_gage_indices = []

    i = 0
    for record in gagerecords:
        if record[HUC8_index] == down_reach: down_gage_indices.append(i)
        if record[HUC8_index] in upreaches:  up_gage_indices.append(i)
        i+=1

    # now find all the comids in the next reach and put them into hydro sequence

    down_reach_indices = [i for i in range(len(values['ReachCode']))
                          if values['ReachCode'][i][:8] == down_reach]

    updown =      {values['Hydroseq'][i]: values['DnHydroseq'][i] 
                   for i in down_reach_indices}
    down_comids = {values['Hydroseq'][i]: values['ComID'][i]
                   for i in down_reach_indices}

    # repeat the process for the inlets

    up_reach_indices = [i for i in range(len(values['ReachCode']))
                        if values['ReachCode'][i][:8] in upreaches]

    downup =    {values['Hydroseq'][i]: values['UpHydroseq'][i] 
                 for i in up_reach_indices}
    up_comids = {values['Hydroseq'][i]: values['ComID'][i]
                 for i in up_reach_indices}

    # now use the NHDPlus source to get the bounding boxes for the flowlines
    # to find the first one downstream and the ones upstream

    shapefile = Reader(flowline_source, shapeType = 3)
    NHDPlusrecords = shapefile.records()

    reach_index = shapefile.fields.index(['REACHCODE', 'C', 14, 0]) - 1
    comid_index = shapefile.fields.index(['COMID',     'N',  9, 0]) - 1
    down_flow = {}
    up_flow   = {}

    i = 0
    for record in NHDPlusrecords:
        if record[reach_index][:8] == down_reach:
            down_flow[record[comid_index]] = i
        if record[reach_index][:8] in upreaches:
            up_flow[record[comid_index]] = i
        i+=1

    # now start at the top of the watershed and find the first gage downstream
    # that fits into a flowline bounding box

    current = values['Hydroseq'][exit_index]
    downgage = -1
    while downgage < 0 or gagerecords[i][reach_index] == HUC8:
        comid = down_comids[current]
        bbox = shapefile.shape(down_flow[comid]).bbox
        for i in down_gage_indices:
            p = sf.shape(i).points[0]
            if (bbox[0] <= p[0] and p[0] <= bbox[2] and bbox[1] <= p[1] and 
                p[1] <= bbox[3] and gagerecords[i][day1_index] > 0):
                downgage = i
        current = updown[current]

    # repeat going upstream from the inlets

    upgages = []
    for inlet in inlets:
        index = values['ComID'].index(inlet)

        current = values['UpHydroseq'][index]
        isgage = False
        while not isgage and downup[current] != 0 and downup[current] in downup:
            comid = up_comids[current]
            bbox = shapefile.shape(up_flow[comid]).bbox
            for i in up_gage_indices:
                p = sf.shape(i).points[0]
                if (bbox[0] <= p[0] and p[0] <= bbox[2] and bbox[1] <= p[1] and 
                    p[1] <= bbox[3] and gagerecords[i][day1_index] > 0):
                    upgages.append(i)
            current = downup[current]
            
    gage_indices = gage_indices + [downgage] + upgages

    # write the data from the HUC8 to a new shapefile

    w = Writer(shapeType = 1)

    for field in sf.fields:  w.field(*field)

    for i in gage_indices:
        point = sf.shape(i).points[0]
        w.point(*point)
        w.record(*gagerecords[i])

    w.save(output)

    if verbose: 
        print('successfully extracted USGS NWIS gage stations for watershed\n')
