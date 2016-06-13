# watershed.py
#
# David J. Lampert (djlampert@gmail.com)
#
# This file contains several classes used as data structures to store 
# the information needed to build a watershed model.
#

import math

from matplotlib import pyplot, patches

class FlowPlane:
    """
    A data structure that contains information about a flow plane.
    """

    def __init__(self, 
                 length, 
                 slope, 
                 centroid, 
                 elev,
                 ):

        self.length    = length
        self.slope     = slope
        self.centroid  = centroid
        self.avgelev   = elev

class Dam:
    """
    A data structure to store information about a dam from the NID database.
    """

    def __init__(self, 
                 nid, 
                 name, 
                 lon, 
                 lat, 
                 river, 
                 owner, 
                 damtype, 
                 purposes, 
                 year, 
                 height, 
                 max_storage, 
                 norm_storage, 
                 surface_area,
                 ):

        # the following are available from the National Inventory of Dams

        self.nid          = nid          # NID number
        self.name         = name         # name
        self.lon          = lon          # longitude
        self.lat          = lat          # latitude
        self.river        = river        # name of primary tributary

        self.owner        = owner        # owner type  F  = Federal
                                         #             L  = Local
                                         #             P  = Private
                                         #             S  = State
                                         #             U  = Utility

        self.damtype      = damtype      # material(s) CB = Concrete Buttress
                                         #             CN = Concrete
                                         #             ER = Rockfill
                                         #             MS = Masonry
                                         #             MV = MultiArch       
                                         #             OT = Other
                                         #             PG = Gravity
                                         #             RE = Earth
                                         #             ST = Stone
                                         #             TC = Timber Crib
                                         #             VA = Arch

        self.purposes     = purposes     #             C  = Flood control
                                         #             D  = Debris control
                                         #             F  = Fish and wildlife
                                         #             H  = Hydroelectric
                                         #             I  = Irrigation
                                         #             N  = Navigation
                                         #             O  = Other
                                         #             P  = Stock Pond
                                         #             R  = Recreation
                                         #             S  = Water supply
                                         #             T  = Tailings

        self.year         = year         # year constructed
        self.height       = height       # height (ft)
        self.max_storage  = max_storage  # max storage (acre-ft)
        self.norm_storage = norm_storage # storage below normal level (acre-ft)
        self.surface_area = surface_area # surface area (acres)

class Reach:
    """
    A data structure containing information about stream reaches and reservoirs.
    """

    def __init__(self, 
                 name, 
                 maxelev, 
                 minelev, 
                 slopelen, 
                 flow       = None, # optional average flow
                 velocity   = None, # optional average velocity
                 traveltime = None, # optional average traveltime
                 dam        = None, # optional dam instance
                 ftable     = None  # optional stage-discharge table
                 ):

        self.name       = name
        self.maxelev    = maxelev
        self.minelev    = minelev
        self.slopelen   = slopelen
        self.flow       = flow
        self.velocity   = velocity
        self.traveltime = traveltime
        self.dam        = dam
        self.ftable     = ftable

    def add_dam(self, nid, name, long, lat, river, owner, damtype, purposes, 
                year, height, max_storage, norm_storage, surface_area):

        self.dam = Dam(nid, name, long, lat, river, owner, damtype, purposes, 
                       year, height, max_storage, norm_storage, surface_area)

class Subbasin:
    """
    A data structure that contains information about an HSPF watershed subbasin.
    """

    def __init__(self, name):
        """Sets up some basic properties of a subbasin."""

        self.name    = name
        self.reach   = None
        self.inlets  = []
        self.outlets = []
        self.landuse = {}

    def add_flowplane(self, length, slope, centroid, elev):
        """Adds the flowplane info to the subbasin."""

        self.flowplane = FlowPlane(length, slope, centroid, elev)

    def add_reach(self, name, maxelev, minelev, slopelen, flow = None, 
                  velocity = None, traveltime = None, dam = None, 
                  ftable = None):
        """Adds the Reach info to the subbasin."""

        self.reach = Reach(name, maxelev, minelev, slopelen, flow = flow, 
                           velocity = velocity, traveltime = traveltime, 
                           dam = dam, ftable = ftable)

    def add_dam(self, nid, name, lon, lat, river, owner, damtype, purposes, 
                year, height, max_storage, norm_storage, surface_area):

        if self.reach is not None:

            self.reach.add_dam(nid, name, lon, lat, river, owner, damtype, 
                               purposes, year, height, max_storage, 
                               norm_storage, surface_area)
       
    def add_inlet(self, inlet):
        """Adds an inlet to the subbasin."""

        self.inlets.append(inlet)

    def add_landuse(self, year, landtypes, areas):

        self.landuse[year] = {l:a for l, a in zip(landtypes, areas)}

class Watershed:
    """
    A data structure that stores all the information about a watershed needed 
    to build an HSPF model.
    """

    def __init__(self, name, subbasins):
        """
        Constructor based on a list of instances of the Subbasin class.

        subbasins -- a dictionary with keys as subbasin names and values as
                     instances of the Subbasin class for the watershed
        outlets   -- a list of the comids of the outlets for the watershed
        inlets    -- a list of the comids of the inlets for the watershed
        updown    -- a dictionary linking the mass flows for the watershed;
                     the keys are subbasin comids and the values are the 
                     downstream subbasin comids
        """
        
        self.name       = name
        self.subbasins  = subbasins
        self.outlets    = []
        self.inlets     = []
        self.headwaters = []

    def add_inlet(self, inlet):

        self.inlets.append(inlet)

    def add_outlet(self, outlet):

        self.outlets.append(outlet)

    def add_headwater(self, comid):

        self.headwaters.append(comid)

    def add_mass_linkage(self, updown):

        self.updown = updown

    def plot_mass_flow(self,
                       title = 'Subbasin Reach Mass Flow Diagram',
                       fontsize = 6, 
                       theight = 0.2, 
                       l = 8.5, 
                       w = 11, 
                       verbose = True, 
                       show = False,
                       output = None,
                       ):
        """
        Makes a schematic of the mass linkages between the various subbasins
        in a watershed.
        """
            
        if verbose: print('generating a mass linkage plot\n')
       
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

        rows = [self.outlets, ['outlet']]

        top = False
        while not top:
            row = []
            for nxt in rows[0]:
                for subbasin in self.updown:
                    if self.updown[subbasin] == nxt: row.append(subbasin)
            if len(row) > 0: 
                rows.insert(0, row)
            else: 
                top = True

        # add an inlet box in the row above each inlet

        for inlet in self.inlets: 

            i = 0
            while i < len(rows) - 1:

                for subbasin in rows[i]:

                    if subbasin == inlet:
                    
                        # find the position of the subbasin in the chart

                        j = rows[i].index(inlet)

                        if i > 0:

                            # figure out where the subbasins point
                        
                            updowns = [self.updown[s] for s in rows[i-1]]

                            # if first or last, add it there in the row above

                            if   j == 0:                
                                rows[i-1].insert(0, 'inlet')
                            elif j == len(rows[i]) - 1: 
                                rows[i-1].append('inlet')
                            else:

                                # find the location of the first arrow
                                
                                while rows[i][j-1] not in updowns: j = j - 1

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
                if rows[i-1][last] in self.updown:
                    main = self.updown[rows[i-1][last]]    
                else:
                    main = [c for c in rows[i] if c in self.inlets][0]
            else:
                main = 'outlet'

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
                        nxt = self.updown[subbasin]
                        if main in self.updown: dwn = self.updown[main]
                        else: dwn = [c for c in next_row if c in self.inlets][0]
                        next_start = (middle - 
                                      next_row.index(dwn))
                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(nxt))
                              + rwidth / 2)

                    elif subbasin == 'inlet':
                        nxt = self.inlets[0]
                        if main in self.updown: dwn = self.updown[main]
                        else: dwn = [c for c in next_row if c in self.inlets][0]
                        next_start = (middle - 
                                      next_row.index(dwn))

                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(nxt))
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

        if output is not None: pyplot.savefig(output, dpi = 200)

        if show: pyplot.show()

        pyplot.clf()
        pyplot.close()
