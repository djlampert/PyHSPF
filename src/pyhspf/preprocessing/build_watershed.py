# build_watershed.py
#
# builds the watershed instance from the other data

import os, csv, pickle, math

from shapefile import Reader

from pyhspf    import Watershed, Subbasin
from .gisplots import get_aggregate_map, plot_mass_flow

def build_watershed(subbasinfile, flowfile, outletfile, damfile, gagefile,
                    landfile, aggregatefile, VAAfile, years, HUC8, output, 
                    plots = True, overwrite = False, format = 'png'):

    # create a dictionary to store subbasin data

    subbasins = {}

    # create a dictionary to keep track of subbasin inlets

    inlets = {}

    # read in the flow plane data into an instance of the FlowPlane class

    sf = Reader(subbasinfile, shapeType = 5)

    comid_index = sf.fields.index(['ComID',      'N',  9, 0]) - 1
    len_index   = sf.fields.index(['PlaneLenM',  'N',  8, 2]) - 1
    slope_index = sf.fields.index(['PlaneSlope', 'N',  9, 6]) - 1
    area_index  = sf.fields.index(['AreaSqKm',   'N', 10, 2]) - 1
    cx_index    = sf.fields.index(['CenX',       'N', 12, 6]) - 1
    cy_index    = sf.fields.index(['CenY',       'N', 12, 6]) - 1
    elev_index  = sf.fields.index(['AvgElevM',   'N',  8, 2]) - 1

    for record in sf.records():
        comid     = '{}'.format(record[comid_index])
        length    = record[len_index]
        slope     = record[slope_index]
        tot_area  = record[area_index]
        centroid  = [record[cx_index], record[cy_index]]
        elevation = record[elev_index]

        subbasin  = Subbasin(comid)
        subbasin.add_flowplane(length, slope, 0, centroid, elevation)

        subbasins[comid] = subbasin

    # read in the flowline data to an instance of the Reach class

    sf = Reader(flowfile)

    outcomid_index   = sf.fields.index(['OutComID',   'N',  9, 0]) - 1
    gnis_index       = sf.fields.index(['GNIS_NAME',  'C', 65, 0]) - 1
    reach_index      = sf.fields.index(['REACHCODE',  'C',  8, 0]) - 1
    incomid_index    = sf.fields.index(['InletComID', 'N',  9, 0]) - 1
    maxelev_index    = sf.fields.index(['MaxElev',    'N',  9, 2]) - 1
    minelev_index    = sf.fields.index(['MinElev',    'N',  9, 2]) - 1
    slopelen_index   = sf.fields.index(['SlopeLenKM', 'N',  6, 2]) - 1
    slope_index      = sf.fields.index(['Slope',      'N',  8, 5]) - 1
    inflow_index     = sf.fields.index(['InFlowCFS',  'N',  8, 3]) - 1
    outflow_index    = sf.fields.index(['OutFlowCFS', 'N',  8, 3]) - 1
    velocity_index   = sf.fields.index(['VelFPS',     'N',  7, 4]) - 1
    traveltime_index = sf.fields.index(['TravTimeHR', 'N',  8, 2]) - 1

    for record in sf.records():

        outcomid   = '{}'.format(record[outcomid_index])
        gnis       = record[gnis_index]
        reach      = record[reach_index]
        incomid    = '{}'.format(record[incomid_index])
        maxelev    = record[maxelev_index] / 100
        minelev    = record[minelev_index] / 100
        slopelen   = record[slopelen_index]
        slope      = record[slope_index]
        inflow     = record[inflow_index]
        outflow    = record[outflow_index]
        velocity   = record[velocity_index]
        traveltime = record[traveltime_index]

        if isinstance(gnis, bytes): gnis = ''

        subbasin = subbasins[outcomid]

        flow = (inflow + outflow) / 2
        subbasin.add_reach(gnis, maxelev, minelev, slopelen, flow = flow, 
                           velocity = velocity, traveltime = traveltime)
        inlets[outcomid] = incomid

    # open up the outlet file and see if the subbasin has a gage or dam

    sf = Reader(outletfile)

    records = sf.records()

    comid_index = sf.fields.index(['COMID',   'N',  9, 0]) - 1
    nid_index   = sf.fields.index(['NIDID',   'C',  7, 0]) - 1
    nwis_index  = sf.fields.index(['SITE_NO', 'C', 15, 0]) - 1

    nids = {'{}'.format(r[comid_index]):r[nid_index] for r in records 
            if isinstance(r[nid_index], str)}

    nwiss = {'{}'.format(r[comid_index]):r[nwis_index] for r in records 
             if r[nwis_index] is not None}

    # open up the dam file and read in the information for the dams

    sf = Reader(damfile)

    records = sf.records()

    name_index  = sf.fields.index(['DAM_NAME',   'C', 65,   0]) - 1
    nid_index   = sf.fields.index(['NIDID',      'C', 7,    0]) - 1
    long_index  = sf.fields.index(['LONGITUDE',  'N', 19,  11]) - 1
    lat_index   = sf.fields.index(['LATITUDE',   'N', 19,  11]) - 1
    river_index = sf.fields.index(['RIVER',      'C', 65,   0]) - 1
    owner_index = sf.fields.index(['OWN_NAME',   'C', 65,   0]) - 1
    type_index  = sf.fields.index(['DAM_TYPE',   'C', 10,   0]) - 1
    purp_index  = sf.fields.index(['PURPOSES',   'C', 254,  0]) - 1
    year_index  = sf.fields.index(['YR_COMPL',   'C', 10,   0]) - 1
    high_index  = sf.fields.index(['NID_HEIGHT', 'N', 19,  11]) - 1
    mstor_index = sf.fields.index(['MAX_STOR',   'N', 19,  11]) - 1
    nstor_index = sf.fields.index(['NORMAL_STO', 'N', 19,  11]) - 1
    area_index  = sf.fields.index(['SURF_AREA',  'N', 19,  11]) - 1

    # iterate through the subbasins and see if they have a dam

    for comid, subbasin in subbasins.items():

        if comid in nids:

            # if the subbasin has a dam, find the data info in the file

            nid = nids[comid]

            r = records[[r[nid_index] for r in records].index(nid)]

            subbasin.add_dam(nid,
                             r[name_index],
                             r[long_index],
                             r[lat_index],
                             r[river_index],
                             r[owner_index],
                             r[type_index],
                             r[purp_index],
                             r[year_index],
                             r[high_index],
                             r[mstor_index],
                             r[nstor_index],
                             r[area_index]
                             )

    # open up the aggregate file to get the landuse group map

    m, landtypes, groups = get_aggregate_map(aggregatefile)

    # convert to a list of landuse names

    names = [landtypes[group] for group in groups]

    # read the land use data for each year into the subbasins

    with open(landfile, 'rb') as f: landyears, landuse = pickle.load(f)

    for comid in subbasins:

        subbasin      = subbasins[comid]
        subbasin_data = landuse[comid]

        for year, data in zip(landyears, zip(*subbasin_data)):

            subbasin.add_landuse(year, names, data)

    # create an instance of the watershed class

    watershed = Watershed(HUC8, subbasins)

    # open up the flowline VAA file to use to establish mass linkages

    with open(VAAfile, 'rb') as f: flowlines = pickle.load(f)
            
    # create a dictionary to connect the comids to hydroseqs

    hydroseqs = {'{}'.format(flowlines[f].comid): 
                 flowlines[f].hydroseq for f in flowlines}

    # establish the mass linkages using a dictionary "updown" and a list of 
    # head water subbasins

    updown = {}
    
    for comid, subbasin in watershed.subbasins.items():

        # get the flowline instance for the outlet comid

        flowline = flowlines[hydroseqs[comid]]

        # check if the subbasin is a watershed inlet or a headwater source

        inlet = hydroseqs[inlets[comid]]

        if flowlines[inlet].up in flowlines:
            i = '{}'.format(flowlines[flowlines[inlet].up].comid)
            subbasin.add_inlet(i)
        elif flowlines[inlet].up != 0:
            watershed.add_inlet(comid)
        else: 
            watershed.add_headwater(comid)

        # check if the subbasin is a watershed outlet, and if it is not, then
        # find the downstream reach

        if flowline.down in flowlines:
            flowline = flowlines[flowline.down]
            while '{}'.format(flowline.comid) not in subbasins:
                flowline = flowlines[flowline.down]
            updown[comid] = '{}'.format(flowline.comid)
        else: 
            updown[comid] = 0
            watershed.add_outlet('{}'.format(comid))

        # open 

    watershed.add_mass_linkage(updown)

    if output is None: 
        filename = os.getcwd() + '/watershed'
        plotname = os.getcwd() + '/masslink.%s' % format
    else:              
        filename = output + '/%s/watershed' % HUC8
        plotname = output + '/%s/images/%smasslink.%s' % (HUC8, HUC8, format)

    if not os.path.isfile(filename) or overwrite:
        with open(filename, 'wb') as f: pickle.dump(watershed, f)

    if not os.path.isfile(plotname) and plots or overwrite and plots: 
        plot_mass_flow(watershed, plotname)
