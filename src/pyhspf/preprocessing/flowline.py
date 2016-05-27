# flowline.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 05/20/2016
#
# contains the Flowline class, which can be used to store data from the 
# NHDPlus V2 dataset 

class Flowline:
    """
    A data structure to store flowline attributes from the NHDPlus database.
    """

    def __init__(self, comid, hydroseq, down, up, drain, area, div, reachcode):
        """Attributes from NHDPlus PlusFlowVAA."""

        self.comid    = comid
        self.hydroseq = hydroseq
        self.down     = down
        self.up       = up
        self.drain    = drain
        self.area     = area
        self.divarea  = div
        self.reach    = reachcode

    def add_slope(self, maxelev, minelev, length):      
        """Attributes from NHDPlus ElevSlope."""

        self.maxelev  = maxelev
        self.minelev  = minelev
        self.length   = length

    def add_flow(self, flow, velocity, gageid):
        """Attributes from EROM_MA0001."""

        self.flow       = flow
        self.velocity   = velocity
        self.gageid     = gageid

    def estimate_traveltime(self):
        """Estimates the travel time in hours for a flowline."""

        ft_per_km = 3280.84
        s_per_hr  = 3600

        self.traveltime = self.length * ft_per_km / self.velocity / s_per_hr

