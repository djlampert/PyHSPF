###############################################################################
#                                                                             #
# Hydrological Simulation Program in Fortran (HSPF) Preprocessor              #
#                                                                             #
# David J. Lampert                                                            #
#                                                                             #
# last updated: 04/18/2015                                                    #
#                                                                             #
# Purpose: Extracts hydrography, land use, and climate data from online       #
# databases, spatially aggregates data, constructs precipitation and          #
# potential evapotranspiration time series, and builds an instance of the     #
# HSPFModel class.                                                            #
#                                                                             #
###############################################################################

###############################################################################
#                                                                             #
# The source data directories are all available online. Note that if the      #
# online file structure is modified the paths will need to be updated.        #
#                                                                             #
# The National Hydrography Dataset Plus (NHDPlus)                             #
# http://www.horizon-systems.com/nhdplus/                                     #
#                                                                             #
# The National Elevation Dataset (NED) on a 30-m grid (supplied with NHDPlus) #
#                                                                             #
# The Cropland Data Layer (CDL) available at                                  #
# http://nassgeodata.gmu.edu/CropScape                                        #
#                                                                             #
# The National Inventory of Dams (NID) shapefile available at                 #
# http://nationalatlas.gov/metadata/dams00x020.faq.html                       #
#                                                                             #
# The USGS NWIS gage station shapefile available at                           #
# http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgages.xml              #
#                                                                             #
# The National Climate Data Center (NCDC)                                     #
# ftp://ftp.ncdc.noaa.gov/pub/data/                                           #
#                                                                             #
# The National Solar Radiation Database (NSRDB)                               #
# http://rredc.nrel.gov/solar/old_data/nsrdb/                                 #
#                                                                             #
###############################################################################

import os, time, pickle, datetime, csv, math

from matplotlib import pyplot, patches
from shapefile  import Reader

# local imports

from .nhdplusextractor import NHDPlusExtractor
from .nwisextractor    import NWISExtractor
from .nidextractor     import NIDExtractor
from .delineators      import HUC8Delineator
from .cdlextractor     import CDLExtractor
from .climateprocessor import ClimateProcessor
from .etcalculator     import ETCalculator
from  pyhspf.core      import Watershed, Subbasin

class Preprocessor:
    """
    A class that integrates the various preprocessing tools together into a 
    single class to build a baseline HSPFModel
    """

    def __init__(self,
                 network, 
                 output, 
                 HUC8      = None,
                 state     = None,
                 start     = None,
                 end       = None,
                 landcodes = None,
                 ):

        self.network   = network
        self.output    = output

        # source data locations on the network

        self.NHDPlus  = '{}/NHDPlus'.format(self.network)
        self.NED      = '{}/NEDSnapshot'.format(self.NHDPlus)
        self.NWIS     = '{}/NWIS'.format(self.network)
        self.CDL      = '{}/CDL'.format(self.network)
        self.NID      = '{}/NID'.format(self.network)

        self.HUC8      = HUC8
        self.state     = state
        self.start     = start
        self.end       = end
        self.landcodes = landcodes
    
    def preprocess(self,
                   HUC8, 
                   state, 
                   start, 
                   end,
                   drainmax = 400, 
                   extra_outlets  = None,
                   overwrite      = False, 
                   verbose        = True, 
                   vverbose       = False, 
                   parallel       = True, 
                   extract        = True, 
                   delineate      = True,
                   landuse        = True, 
                   landstats      = True, 
                   build          = True, 
                   climate        = True, 
                   gagedata       = True,
                   subbasinplots  = False, 
                   watershedplots = True, 
                   landplots      = True,
                   landpercents   = False, 
                   flowplots      = True, 
                   metstatplots   = True,
                   metgageplots   = True,
                   ):
        """
        Preprocesses the data for HSPF
        """

        # check the network is mounted on Unix-like systems

        if os.name != 'nt':

            if not os.path.ismount(self.network):
                print('\nerror: network ' +
                      '{} does not seem to be mounted\n'.format(self.network))
                raise

        # keep track of how long it takes

        go = time.time()

        years = [year for year in range(start, end)]

        # extract the data for the HUC8 from the sources

        start = datetime.datetime(start, 1, 1)
        end   = datetime.datetime(end, 1, 1)

        if extract: self.extract(HUC8, start, end)

        # delineate the subbasins and the hydrography data

        if delineate: self.delineate(HUC8)

        # download and extract land use data

        if landuse: self.extract_CDL(HUC8, state, years)

        # build the watershed object

        if build: self.build(HUC8, start, end, years)

        # download and extract the climate data

        if climate: self.climate(HUC8, start, end)

        # make a directory for HSPF calculations

        hspfdirectory = '{}/{}/hspf'.format(self.output, HUC8)
        if not os.path.isdir(hspfdirectory): os.mkdir(hspfdirectory)

        if verbose: 
            print('completed preprocessing watershed in %.1f seconds\n' % 
                  (time.time() - go))

    def extract(self, 
                HUC8, 
                start, 
                end,
               ):
        """
        Downloads and extracts NHDPlus, NWIS, and NID data for a HUC8.
        """

        VPU = HUC8[:2]

        # create an instance of the extractor

        nhdplusextractor = NHDPlusExtractor(VPU, self.NHDPlus)

        # extract the HUC8 data for the HUC8 to the output 

        nhdplusextractor.extract_HUC8(HUC8, self.output)

        # extract the gage data from NWIS

        nwisextractor = NWISExtractor(self.NWIS)

        # extract the gage stations into a new shapefile for the HUC8 and 
        # place them into the newly generated directory for the HUC8

        directory = '{}/{}'.format(self.output, HUC8)

        nwisextractor.extract_HUC8(HUC8, directory)

        # download all the gage data to the gagepath directory

        gagepath = '{}/{}/NWIS'.format(self.output, HUC8)

        nwisextractor.download_all(start, end, output = gagepath)

        # extract the dam data from the NID

        nidextractor = NIDExtractor(self.NID)

        # extract dams in the HUC8 using the shapefile for the boundary

        bfile   = '{0}/{1}/boundary'.format(self.output, HUC8)
        damfile = '{0}/{1}/dams'.format(self.output, HUC8)

        nidextractor.extract_shapefile(bfile, damfile)

    def delineate(self,
                  HUC8,
                  drainmax      = 400, 
                  extra_outlets = None,
                  parallel      = True, 
                  verbose       = True, 
                  vverbose      = False, 
                  ):

        # use the HUC8Delineator to delineate the watershed

        VAAfile  = '{0}/{1}/flowlineVAAs'.format(self.output, HUC8)
        flowfile = '{0}/{1}/flowlines'.format(self.output, HUC8)
        cfile    = '{0}/{1}/catchments'.format(self.output, HUC8)
        elevfile = '{0}/{1}/elevations'.format(self.output, HUC8)
        damfile  = '{0}/{1}/dams'.format(self.output, HUC8)
        gagefile = '{0}/{1}/gagestations'.format(self.output, HUC8)

        delineator = HUC8Delineator(HUC8, 
                                    VAAfile, 
                                    flowfile, 
                                    cfile, 
                                    elevfile,
                                    gagefile, 
                                    damfile,
                                    ) 

        # delineate the watershed using the NHDPlus data and delineator

        delineator.delineate(self.output, 
                             parallel      = parallel,
                             drainmax      = drainmax, 
                             extra_outlets = extra_outlets,
                             verbose       = verbose,
                             vverbose      = vverbose,
                             )

    def extract_CDL(self,
                    HUC8,
                    state,
                    years,
                    ):

        # extract cropland data from NASS

        cdlextractor = CDLExtractor(self.CDL)

        # download the data for each state for each year

        cdlextractor.download_data(state.upper(), years)

        # make a directory for the CDL files

        landusedata = '{0}/{1}/landuse'.format(self.output, HUC8)

        if not os.path.isdir(landusedata): os.mkdir(landusedata)

        # extract the data for the watershed using the boundary shapefile

        landfile     = '{0}/{1}/subbasinlanduse'.format(self.output, HUC8)
        subbasinfile = '{0}/{1}/subbasin_catchments'.format(self.output, HUC8)

        if not any([os.path.isfile('{}/{}landuse.tif'.format(landusedata, year))
                    for year in years]):
                                   
            cdlextractor.extract_shapefile(subbasinfile, landusedata)
            print('')

        # calculate the landuse in each subbasin for each year

        for year in years:

            attribute = 'ComID'
            extracted = '{}/{}landuse.tif'.format(landusedata, year)

            # csv file of the output

            csvfile = '{}/{}landuse.csv'.format(landusedata, year)
            if not os.path.isfile(csvfile):

                if self.landcodes is None:
                    print('error: no landuse code file specified\n')
                    raise
                    
                cdlextractor.calculate_landuse(extracted, subbasinfile, 
                                               self.landcodes, attribute,
                                               csvfile = csvfile)

                # raw landuse plot

                raw = '{}/{}raw'.format(landusedata, year)
                if not os.path.isfile(raw + '.png'):
                    cdlextractor.plot_landuse(extracted, subbasinfile, 
                                              attribute, 
                                              output = raw, lw = 2.,
                                              datatype = 'raw')

                # aggregated land use plot

                results = '{}/{}results'.format(landusedata, year)
                if not os.path.isfile(results + '.png'):
                    cdlextractor.plot_landuse(extracted, subbasinfile, 
                                              attribute, 
                                              output = results, 
                                              datatype = 'results')

        print('')

    def build_watershed(self,
                        subbasinfile, 
                        flowfile, 
                        outletfile, 
                        damfile, 
                        gagefile,
                        landfiles, 
                        VAAfile, 
                        years, 
                        HUC8, 
                        output, 
                        plots = True, 
                        overwrite = False,
                        ):

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
            subbasin.add_flowplane(length, slope, centroid, elevation)

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
                                 r[area_index],
                                 ) 

        # read in the landuse data from the csv files

        for year in years:

            csvfile = '{}/{}landuse.csv'.format(landfiles, year)

            with open(csvfile, 'r') as f: 

                reader = csv.reader(f)
                rows = [r for r in reader]

            # organize the data

            comids     = [r[0] for r in rows[3:]]
            categories = rows[2][2:]
            emptys     = [r[1] for r in rows[3:]]
            data       = [r[2:] for r in rows[3:]]

            if any([e == '0' for e in emptys]): 

                print('warning, empty cells detected\n')

            for comid, subbasin in subbasins.items():

                i = comids.index(comid)

                subbasin.add_landuse(year, categories, data[i])

        # create an instance of the Watershed class

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

            # check if the subbasin is a watershed outlet, and if it is not 
            # then find the downstream reach

            if flowline.down in flowlines:
                flowline = flowlines[flowline.down]
                while '{}'.format(flowline.comid) not in subbasins:
                    flowline = flowlines[flowline.down]
                updown[comid] = '{}'.format(flowline.comid)
            else: 
                updown[comid] = 0
                watershed.add_outlet('{}'.format(comid))

        # add the updown dictionary to show mass linkage in the reaches

        watershed.add_mass_linkage(updown)

        if output is None: output = os.getcwd()

        filename = '{}/{}/watershed'.format(output, HUC8)
        plotname = '{}/{}/masslink'.format(output, HUC8)

        if not os.path.isfile(filename) or overwrite:
            with open(filename, 'wb') as f: pickle.dump(watershed, f)

        if (not os.path.isfile(plotname + '.png') and plots or overwrite and 
            plots):
 
            self.plot_mass_flow(watershed, plotname)

    def plot_mass_flow(self,
                       watershed, 
                       output, 
                       title = 'Subbasin Reach Mass Flow Diagram',
                       fontsize = 6, 
                       theight = 0.2, 
                       l = 8.5, 
                       w = 11, 
                       verbose = True, 
                       overwrite = True,
                       ):
        """
        Makes a schematic of the mass linkages between the various subbasins
        in a watershed.
        """

        if os.path.exists(output) and not overwrite:
            if verbose: print('file %s exists' % output)
            return
        elif verbose: print('generating a mass linkage plot\n')

        fontheight = fontsize / 72.
        rheight = 3 * fontheight
        rwidth  = 12 * fontheight
        xgap = fontheight
        ygap = rheight
        awidth = rheight / 4
        aheight = rheight / 3

        # set up a sheet to write the image

        fig = pyplot.figure(figsize = (w, l))

        ax  = fig.add_subplot(111, aspect = 'equal')
        ax.get_xaxis().set_visible(False)
        ax.get_yaxis().set_visible(False)
        t = ax.set_title(title)

        # divide the subbasins into rows and put them on the chart
        # start at the bottom to organize the linkages better

        rows = [watershed.outlets, ['outlet']]

        top = False
        while not top:
            row = []
            for next in rows[0]:
                for subbasin in watershed.updown:
                    if watershed.updown[subbasin] == next: row.append(subbasin)
            if len(row) > 0: 
                rows.insert(0, row)
            else: 
                top = True

        # add an inlet box in the row above each inlet

        for inlet in watershed.inlets: 

            i = 0
            while i < len(rows) - 1:

                for subbasin in rows[i]:

                    if subbasin == inlet:
                    
                        # find the position of the subbasin in the chart

                        j = rows[i].index(inlet)

                        if i > 0:

                            # figure out where the subbasins point
                        
                            updowns = [watershed.updown[s] for s in rows[i-1]]
                            
                            # if first or last, add it there in the row above

                            if   j == 0:                
                                rows[i-1].insert(0, 'inlet')
                            elif j == len(rows[i]) - 1: 
                                rows[i-1].append('inlet')
                            else:

                                # find the place to add in the preceeding row 

                                n = updowns.index(rows[i][j-1]) + 1
                                rows[i-1].insert(n, 'inlet')

                i += 1

        # write the subbasin boxes to the chart

        middle = math.ceil(w // (rwidth + xgap)) // 2
        last = 0

        # keep track of the bounding box of the plot

        xmin, ymin, xmax, ymax = middle, 0, middle, 0

        for i in range(len(rows)):

            row = rows[i]
        
            y = (ygap + rheight) * i + theight

            # figure out which cell to put in the main column

            if i == 0:
                main = row[(len(row) - 1) // 2]
            elif i < len(rows) - 1:
                main = watershed.updown[rows[i-1][last]]
            else: main = 'outlet'

            start = middle - row.index(main)

            if i < len(rows) - 1: next_row = rows[i + 1]

            for subbasin in row:
                x = (rwidth + xgap) * (start + row.index(subbasin))
                r = patches.Rectangle((x, y), rwidth, rheight, fill = False)

                # adjust the bounding box

                if x           < xmin: xmin = x
                if x + rwidth  > xmax: xmax = x + rwidth
                if y           < ymin: ymin = y
                if y + rheight > ymax: ymax = y + rheight

                if subbasin != 'outlet': ax.add_patch(r)

                b = ax.text(x + rwidth / 2, y + rheight / 2, subbasin,
                            horizontalalignment = 'center',
                            verticalalignment   = 'center')

                # draw the arrow

                if i < len(rows) - 1:

                    x1 = x + rwidth / 2

                    if i < len(rows) - 2 and subbasin != 'inlet':
                        next = watershed.updown[subbasin]
                        next_start = (middle - 
                                      next_row.index(watershed.updown[main]))
                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(next))
                              + rwidth / 2)

                    elif subbasin == 'inlet':
                        next = watershed.inlets[0]
                        next_start = (middle - 
                                      next_row.index(watershed.updown[main]))

                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(next))
                              + rwidth / 2)

                    else:
                        next_start = middle
                        x2 = ((rwidth + xgap) * (middle) + rwidth / 2)

                    a = pyplot.arrow(x1, y + rheight, x2 - x1, ygap, 
                                     head_width = awidth, head_length = aheight,
                                     fc = 'k', ec = 'k', 
                                     length_includes_head = True)
                    ax.add_patch(a)

            last = row.index(main)
            i += 1
        
        pad = 0.02

        xmin = xmin - (xmax - xmin) * pad
        xmax = xmax + (xmax - xmin) * pad
        ymin = ymin - (ymax - ymin) * pad
        ymax = ymax + (ymax - ymin) * pad

        ax.set_xlim(xmin, xmax)
        ax.set_ylim(ymax, ymin)
        pyplot.axis('off')
        #pyplot.tight_layout()
        pyplot.savefig(output, dpi = 200)

        pyplot.clf()
        pyplot.close()

    def build(self,
              HUC8,
              start,
              end,
              years,
              flowplots = True,
              ):

        # file paths

        subbasinfile = '{}/{}/subbasin_catchments'.format(self.output, HUC8)
        flowlinefile = '{}/{}/subbasin_flowlines'.format(self.output, HUC8)
        outletfile   = '{}/{}/subbasin_outlets'.format(self.output, HUC8)
        damfile      = '{}/{}/dams'.format(self.output, HUC8)
        gagefile     = '{}/{}/gagestations'.format(self.output, HUC8)
        landusedata  = '{}/{}/landuse'.format(self.output, HUC8)
        VAAfile      = '{}/{}/flowlineVAAs'.format(self.output, HUC8)

        if not os.path.isfile('{}/{}/watershed'.format(self.output, HUC8)):

            self.build_watershed(subbasinfile, flowlinefile, outletfile, 
                                 damfile,
                                 gagefile, landusedata, VAAfile, 
                                 years, HUC8, self.output, plots = flowplots)
        
    def climate(self,
                HUC8,
                s,
                e,
                verbose = True,
                ):

        subbasinfile = '{}/{}/subbasin_catchments'.format(self.output, HUC8)

        climatedata = '{}/{}/climate'.format(self.output, HUC8)

        # make a directory for the climate data and time series

        if not os.path.isdir(climatedata): os.mkdir(climatedata)

        # use the Climateprocessor to get the data

        climateprocessor = ClimateProcessor()
        climateprocessor.download_shapefile(subbasinfile, s, e, climatedata,
                                            space = 0.5)

        # make directories for hourly and daily aggregated timeseries

        hourly = '{}/hourly'.format(climatedata)
        daily  = '{}/daily'.format(climatedata)

        if not os.path.isdir(hourly): os.mkdir(hourly)
        if not os.path.isdir(daily):  os.mkdir(daily)

        # aggregate the daily GSOD tmin, tmax, dewpoint, and wind data

        tmin = '{}/tmin'.format(daily)
        tmax = '{}/tmax'.format(daily)
        dewt = '{}/dewpoint'.format(daily)
        wind = '{}/wind'.format(daily)

        if not os.path.isfile(tmin):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'tmin', s, e)
            with open(tmin, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(tmax):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'tmax', s, e)
            with open(tmax, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(dewt):
            ts = s, 1440, climateprocessor.aggregate('GSOD','dewpoint', s,e)
            with open(dewt, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(wind):
            ts = s, 1440, climateprocessor.aggregate('GSOD', 'wind', s, e)
            with open(wind, 'wb') as f: pickle.dump(ts, f)

        # aggregate the daily GHCND snowfall and snowdepth data

        snowfall  = '{}/snowfall'.format(daily)
        snowdepth = '{}/snowdepth'.format(daily)

        if not os.path.isfile(snowfall):
            ts = s, 1440, climateprocessor.aggregate('GHCND','snowfall',s,e)
            with open(snowfall, 'wb') as f: pickle.dump(ts, f)
        if not os.path.isfile(snowdepth):
            ts = s, 1440,climateprocessor.aggregate('GHCND','snowdepth',s,e)
            with open(snowdepth, 'wb') as f: pickle.dump(ts, f)

        # find stations with pan evaporation data from GHCND

        evapstations = []
        for k, v in climateprocessor.metadata.ghcndstations.items():

        # check if the station has any evaporation data

            if v['evap'] > 0:
                
                # open up the file and get the data

                with open(k, 'rb') as f: station = pickle.load(f)

                data = station.make_timeseries('evaporation', s, e)

                # ignore datasets with no observations during the period

                observations = [v for v in data if v is not None]

                if len(observations) > 0: evapstations.append(k)

        # aggregate the hourly NSRDB metstat data

        hsolar = '{}/solar'.format(hourly)
        if not os.path.isfile(hsolar):
            ts = s, 60, climateprocessor.aggregate('NSRDB', 'metstat', s, e)
            with open(hsolar, 'wb') as f: pickle.dump(ts, f)
            
        # aggregate the hourly solar to daily

        dsolar = '{}/solar'.format(daily)
        if not os.path.isfile(dsolar):

            with open(hsolar, 'rb') as f: t, tstep, data = pickle.load(f)
            ts = s, 1440, [sum(data[i:i+24]) / 24 
                           for i in range(0, 24 * (e-s).days, 24)]

            with open(dsolar, 'wb') as f: pickle.dump(ts, f)

        # aggregate the hourly precipitation for each subbasin using IDWA

        precip = '{}/hourlyprecipitation'.format(climatedata)
        if not os.path.isdir(precip): os.mkdir(precip)

        # use the subbasin shapefile to get the location of the centroids

        sf = Reader(subbasinfile)

        # index of the comid, latitude, and longitude records

        comid_index = [f[0] for f in sf.fields].index('ComID') - 1
        lon_index   = [f[0] for f in sf.fields].index('CenX')  - 1
        lat_index   = [f[0] for f in sf.fields].index('CenY')  - 1
        elev_index  = [f[0] for f in sf.fields].index('AvgElevM') - 1
        area_index  = [f[0] for f in sf.fields].index('AreaSqKm') - 1

        # iterate through the shapefile records and aggregate the timeseries

        for i in range(len(sf.records())):

            record = sf.record(i)
            comid  = record[comid_index]
            lon    = record[lon_index]
            lat    = record[lat_index]

            # check if the aggregated time series exists or calculate it

            subbasinprecip = '{}/{}'.format(precip, comid)
            if not os.path.isfile(subbasinprecip):

                if verbose:
                    i = comid, lon, lat
                    print('aggregating timeseries for comid ' +
                          '{} at {}, {}\n'.format(*i))

                p = climateprocessor.aggregate('precip3240', 'precip', s, e,
                                               method = 'IDWA', 
                                               longitude = lon,
                                               latitude = lat)

                ts = s, 60, p
                with open(subbasinprecip, 'wb') as f: pickle.dump(ts, f)

        # make a directory for the evapotranspiration time series

        evapotranspiration = '{}/evapotranspiration'.format(climatedata)
        if not os.path.isdir(evapotranspiration): 
            os.mkdir(evapotranspiration)

        # use the ETCalculator to calculate the ET time series

        etcalculator = ETCalculator()

        # get the centroid of the watershed from the subbasin shapefile

        areas = [r[area_index] for r in sf.records()]
        xs    = [r[lon_index]  for r in sf.records()]
        ys    = [r[lat_index]  for r in sf.records()]
        zs    = [r[elev_index] for r in sf.records()]

        # get the areal-weighted averages

        lon  = sum([a * x for a, x in zip(areas, xs)]) / sum(areas)
        lat  = sum([a * y for a, y in zip(areas, ys)]) / sum(areas)
        elev = sum([a * z for a, z in zip(areas, zs)]) / sum(areas)

        # add them to the ETCalculator

        etcalculator.add_location(lon, lat, elev)

        # check if the daily RET exists; otherwise calculate it

        dRET = '{}/dailyRET'.format(evapotranspiration)
        if not os.path.isfile(dRET):

            # add the daily time series to the calculator

            with open(tmin, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('tmin', tstep, t, data)
            
            with open(tmax, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('tmax', tstep, t, data)

            with open(dewt, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('dewpoint', tstep, t, data)

            with open(wind, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('wind', tstep, t, data)

            with open(dsolar, 'rb') as f: t, tstep, data = pickle.load(f)

            etcalculator.add_timeseries('solar', tstep, t, data)

            # calculate the daily RET

            etcalculator.penman_daily(s, e)

            ts = s, 1440, etcalculator.daily['RET'][1]

            with open(dRET, 'wb') as f: pickle.dump(ts, f)

        # disaggregate the daily temperature time series to hourly

        hourlytemp = '{}/temperature'.format(hourly)
        if not os.path.isfile(hourlytemp):
                
            if etcalculator.daily['tmin'] is None:

                with open(tmin, 'rb') as f: t, tstep, data = pickle.load(f)

                etcalculator.add_timeseries('tmin', tstep, t, data)

            if etcalculator.daily['tmax'] is None:

                with open(tmax, 'rb') as f: t, tstep, data = pickle.load(f)

                etcalculator.add_timeseries('tmax', tstep, t, data)

            data  = etcalculator.interpolate_temperatures(s, e)
            tstep = 60
            ts    = t, tstep, data

            with open(hourlytemp, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('temperature', tstep, t, data)

        # disaggregate the dewpoint and wind speed time series to hourly

        hourlydewt = '{}/dewpoint'.format(hourly)        
        if not os.path.isfile(hourlydewt):

            if etcalculator.daily['dewpoint'] is None:

                with open(dewt, 'rb') as f: t, tstep, data = pickle.load(f)

            else:

                t, data = etcalculator.daily['dewpoint']
                
            tstep = 60
            data  = [v for v in data for i in range(24)]
            ts    = t, tstep, data 
            
            with open(hourlydewt, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('dewpoint', tstep, t, data)

        hourlywind = '{}/wind'.format(hourly)        
        if not os.path.isfile(hourlywind):

            if etcalculator.daily['wind'] is None:
                
                with open(wind, 'rb') as f: t, tstep, data = pickle.load(f)

            else:

                t, data = etcalculator.daily['wind']

            tstep = 60
            data  = [v for v in data for i in range(24)]
            ts    = t, tstep, data 
            
            with open(hourlywind, 'wb') as f: pickle.dump(ts, f)

            etcalculator.add_timeseries('wind', tstep, t, data)

        # check if the hourly RET exists; otherwise calculate it

        hRET = '{}/hourlyRET'.format(evapotranspiration)
        if not os.path.isfile(hRET):

            required = 'temperature', 'solar', 'dewpoint', 'wind'

            for tstype in required:

                if etcalculator.hourly[tstype] is None:

                    name = '{}/{}'.format(hourly, tstype)
                    with open(name, 'rb') as f: 
                        t, tstep, data = pickle.load(f)
                    etcalculator.add_timeseries(tstype, tstep, t, data)

            # calculate and save the hourly RET

            etcalculator.penman_hourly(s, e)

            ts = s, 60, etcalculator.hourly['RET'][1]

            with open(hRET, 'wb') as f: pickle.dump(ts, f)

            # add the daily time series for the plot

            required = 'tmin', 'tmax', 'dewpoint', 'wind', 'solar'

            for tstype in required:

                if etcalculator.daily[tstype] is None:

                    name = '{}/{}'.format(daily, tstype)
                    with open(name, 'rb') as f: 
                        t, tstep, data = pickle.load(f)
                    etcalculator.add_timeseries(tstype, tstep, t, data)

            # aggregate the hourly to daily for plotting

            hRET = etcalculator.hourly['RET'][1]

            dRET = [sum(hRET[i:i+24]) for i in range(0, len(hRET), 24)]

            etcalculator.add_timeseries('RET', 'daily', s, dRET)

            name = '{}/referenceET'.format(evapotranspiration)
            etcalculator.plotET(stations = evapstations, output = name, 
                                show = False)
            
            name = '{}/dayofyearET'.format(evapotranspiration)

            etcalculator.plotdayofyear(stations = evapstations, 
                                       output = name, 
                                       show = False)

        # calculate hourly PET for different land use categories

        lucs = ('corn', 
                'soybeans', 
                'grains', 
                'alfalfa', 
                'fallow',
                'pasture', 
                'wetlands', 
                'others',
                )

        colors  = ('yellow',  
                   'green',        
                   'brown',    
                   'lime',   
                   'gray', 
                   'orange',      
                   'blue', 
                   'black',
                   )

        pdates = (datetime.datetime(2000, 4, 15),
                  datetime.datetime(2000, 5, 15),
                  datetime.datetime(2000, 4, 15),
                  datetime.datetime(2000, 5, 15),
                  datetime.datetime(2000, 3,  1),
                  datetime.datetime(2000, 3,  1),
                  datetime.datetime(2000, 3,  1),
                  datetime.datetime(2000, 3,  1),
                  )

        ems = (30,
               20,
               20,
               10,
               10,
               10,
               10,
               10,
               )

        gs = (50,
              30,
              30,
              10,
              10,
              10,
              10,
              10,
              )

        fs = (60,
              60,
              60,
              120,
              240,
              240,
              240,
              240,
              )

        ls = (40,
              30,
              40,
              10,
              10,
              10,
              10,
              10,
              )

        Kis = (0.30,
               0.40,
               0.30,
               0.30,
               0.30,
               0.30,
               1.00,
               1.00,
               )

        Kms = (1.15,
               1.15,
               1.15,
               0.95,
               0.30,
               0.85,
               1.20,
               1.00,
               )

        Kls = (0.40,
               0.55,
               0.40,
               0.90,
               0.30,
               0.30,
               1.00,
               1.00,
               )

        # add the hourly RET time series if it isn't present

        if etcalculator.hourly['RET'] is None:

            with open(hRET, 'rb') as f: t, tstep, data = pickle.load(f)
            etcalculator.add_timeseries('RET', tstep, t, data)

        # iterate through the land use categories and calculate PET 

        for i in zip(lucs, colors, pdates, ems, gs, fs, ls, Kis, Kms, Kls):

            crop, c, plant, emergence, growth, full, late, Ki, Km, Kl = i

            # add the information and calculate the PET time series

            etcalculator.add_crop(crop, 
                                  plant, 
                                  emergence, 
                                  growth, 
                                  full, 
                                  late, 
                                  Ki,
                                  Km, 
                                  Kl,
                                  ) 

            etcalculator.hourly_PET(crop, s, e)

            # get the PET time series
    
            t, PET = etcalculator.hourlyPETs[crop]
            ts = t, 60, PET

            # save it

            name = '{}/{}'.format(evapotranspiration, crop)
            with open(name, 'wb') as f: pickle.dump(ts, f)
