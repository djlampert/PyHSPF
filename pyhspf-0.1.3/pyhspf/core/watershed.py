#
# watershed.py
#
# David J. Lampert, PhD, PE
#
# This file contains several classes useful for building a watershed.
#

class FlowPlane:
    """A class that contains information about a flow plane."""

    def __init__(self, length, slope, area, centroid, elev):

        self.length    = length
        self.slope     = slope
        self.area      = area
        self.centroid  = centroid
        self.avgelev   = elev

class Dam:
    """A class to store information about a dam."""

    def __init__(self, nid, name, long, lat, river, owner, damtype, purposes, 
                 year, height, max_storage, norm_storage, surface_area):

        # the following are available from the National Inventory of Dams

        self.nid          = nid          # NID number
        self.name         = name         # name
        self.long         = long         # longitude
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
    """A class that contains physical information about a reach."""

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
    """A class that contains information about an HSPF watershed subbasin."""

    def __init__(self, name):
        """Sets up some basic properties of a subbasin."""

        self.name    = name
        self.reach   = None
        self.inlets  = []
        self.outlets = []
        self.landuse = {}

    def add_flowplane(self, length, slope, area, centroid, elev):
        """Adds the flowplane info to the subbasin."""

        self.flowplane = FlowPlane(length, slope, area, centroid, elev)

    def add_reach(self, name, maxelev, minelev, slopelen, flow = None, 
                  velocity = None, traveltime = None, dam = None, 
                  ftable = None):
        """Adds the Reach info to the subbasin."""

        self.reach = Reach(name, maxelev, minelev, slopelen, flow = flow, 
                           velocity = velocity, traveltime = traveltime, 
                           dam = dam, ftable = ftable)

    def add_dam(self, nid, name, long, lat, river, owner, damtype, purposes, 
                year, height, max_storage, norm_storage, surface_area):

        if self.reach is not None:

            self.reach.add_dam(nid, name, long, lat, river, owner, damtype, 
                               purposes, year, height, max_storage, 
                               norm_storage, surface_area)
       
    def add_inlet(self, inlet):
        """Adds an inlet to the subbasin."""

        self.inlets.append(inlet)

    def add_landuse(self, year, landtypes, areas):

        self.landuse[year] = {l:a for l, a in zip(landtypes, areas)}

class Watershed:
    """A class that stores all the information about a watershed needed to 
    build and HSPF model."""

    def __init__(self, name, subbasins):
        """Constructor based on a list of instances of the Subbasin class.

        subbasins -- a list of the instances of the Subbasin class for the 
                     watershed
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
