# nrcm.py
#
# contains a class to store the climate data at a grid point from an NRCM
# simulation.

class NRCMGrid:
    """A class to store the data from an NRCM grid point."""

    def __init__(self, lat, lon, start):

        self.lat    = lat
        self.lon    = lon
        self.start  = start
        self.data   = {}
        self.tsteps = {}

    def add_data(self, dtype, tstep, data):
        """Adds data of data type "dtype" to the model."""

        if dtype not in self.data:
            self.data[dtype] = data
            self.tsteps[dtype] = tstep
        else:
            self.data[dtype] += data

