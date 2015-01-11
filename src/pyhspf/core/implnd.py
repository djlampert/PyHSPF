# implnd.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: Contains the Implnd class that acts as a container to store
# information about an HSPF IMPLND (Impervious Land Segment).
#
# last updated: 01/10/2015

class Implnd:
    """A class for an impervious land segment for an HSPF model."""

    def __init__(self, 
                 operation, 
                 subbasin, 
                 area, 
                 length, 
                 slope, 
                 elev, 
                 lat,
                 ATMP = False, 
                 SNOW = False, 
                 IWAT = False, 
                 SLD  = False, 
                 IWG  = False,  
                 IQAL = False):

        # identification information

        self.operation  = operation    # the HSPF operation number (1-999)
        self.subbasin   = subbasin     # the subbasin number of the outlet
        self.landtype   = 'Impervious' # land type (always the same)

        # physically-based parameters

        self.area       = area         # the area of the land segment
        self.LSUR       = length       # the overland flow plane length
        self.SLSUR      = slope        # the overland flow plane slope
        self.elev       = elev         # the mean elevation of the segment (m)
        self.lat        = lat          # the mean latitude of the segment (m)

        # the rest are flags for HSPF modules

        self.ATMP       = ATMP
        self.SNOW       = SNOW
        self.IWAT       = IWAT
        self.SLD        = SLD
        self.IWG        = IWG
        self.IQAL       = IQAL

    def set_iwat_parms(self, 
                       units, 
                       VRS = 0, 
                       VNN = 0
                       ):
        """Shortcut function to set each IWAT block."""

        self.units = units # English or Metric

        self.set_iwat_parm1()
        self.set_iwat_parm2()
        self.set_iwat_parm3()
        self.set_iwat_state1()

        if self.units == 'English':
            self.PETMAX = 40
            self.PETMIN = 35
            self.RETS   = 0.01
            self.SURS   = 0.01

    def set_iwat_parm1(self, 
                       CSNO = 0, 
                       RTOP = 1, 
                       VRS  = 0, 
                       VNN  = 0, 
                       RTLI = 0
                       ):
        """Sets the values for the parameter flags."""

        self.CSNO = CSNO
        self.RTOP = RTOP
        self.VRS  = VRS
        self.VNN  = VNN
        self.RTLI = RTLI
        
    def get_iwat_parm1(self):
        """Returns the values for the IWAT-PARM1 flags."""
        
        return (self.operation, self.CSNO, self.RTOP, self.VRS, self.VNN, 
                self.RTLI)

    def set_iwat_parm2(self, 
                       NSUR = 0.1, 
                       RETSC = 0
                       ):
        """Sets the values for some parameters (IWAT2) in the IWATER module.
        Note that LSUR and SLSUR come from physical data so they are not
        set here."""

        #self.LSUR   = LSUR   # overland flow plane length (0.3 m or more)
        #self.SLSUR  = SLSUR  # overland flow slope (0.000001 to 10.)
        self.NSUR   = NSUR   # overland flow manning's n
        self.RETSC  = RETSC  # interception storage capacity (mm)

    def get_iwat_parm2(self):
        """Gets the values for IWAT-PARM2."""

        return self.operation, self.LSUR, self.SLSUR, self.NSUR, self.RETSC
        
    def set_iwat_parm3(self, 
                       PETMAX = 1., 
                       PETMIN = 0.
                       ):
        """Sets the evapotranspiration parameters for IWATER."""

        self.PETMAX = PETMAX  # air temp where input ET is reduced
        self.PETMIN = PETMIN  # air temp below which ET is zero

    def get_iwat_parm3(self):
        """Returns the IWAT-PARM3 values."""

        return self.operation, self.PETMAX, self.PETMIN

    def set_iwat_state1(self, 
                        RETS = 0.025, 
                        SURS = 0.025
                        ):
        """Sets the value of the state variables. """

        self.RETS = RETS
        self.SURS = SURS

    def get_iwat_state1(self):
        """Gets the value of the IWAT-STATE1 parameters."""

        return self.operation, self. RETS, self.SURS

    def set_atemp_dat(self, 
                      ELDAT = 0., 
                      AIRTMP = 0.
                      ):
        """Sets the data for adjusting gage temperature to land segment temp."""

        self.ELDAT  = ELDAT  # elevation difference between gage and segment (m)
        self.AIRTMP = AIRTMP # initial temperature (C)

    def get_atemp_dat(self):
        """Returns the ATEMP-DAT values."""

        return self.operation, self.ELDAT, self.AIRTMP

    def set_ice_flag(self, 
                     ICEFG = 1
                     ):
        """Sets the flag for ice simulation."""
        
        self.ICEFG = ICEFG # ice formation flag

    def get_ice_flag(self):
        """Returns the values of ICE-FLAG as a tuple."""

        return self.operation, self.ICEFG

    def set_snow_flags(self, 
                       SNOPFG = 0, 
                       VKMFG = 0
                       ):
        """Sets the flags for the snow module."""

        self.SNOPFG = SNOPFG # snow melt algorithm (0: energy, 1: temp index)
        self.VKMFG  = VKMFG  # monthly variable degree day flag 

    def get_snow_flags(self):
        """Returns the SNOW-FLAGS as a tuple."""

        return self.operation, self.SNOPFG, self.VKMFG

    def set_snow_parm1(self, 
                       SHADE = 0.15, 
                       SNOWCF = 1., 
                       COVIND = 10., 
                       KMELT = 0., 
                       TBASE = 0.
                       ):
        """Sets the snow melt parameter values. Note first two pre-defined."""

        #self.LAT    = LAT   # latitude
        #self.MELEV  = MELEV # mean elevation (m)
        self.SHADE  = SHADE  # land surface fraction shaded
        self.SNOWCF = SNOWCF # gage catch efficiency factor
        self.COVIND = COVIND # snowpack depth corresponding to full area cov.
        self.KMELT  = KMELT  # degree-day factor for SNOPFG = 1 (mm/d/C)
        self.TBASE  = TBASE  # reference temperature index (C)

    def get_snow_parm1(self):
        """Returns the SNOW-PARM1 values as a tuple."""

        return (self.operation, self.lat, self.elev, self.SHADE, self.SNOWCF,
                self.COVIND, self.KMELT, self.TBASE)

    def set_snow_parm2(self, 
                       RDCSN = 0.12, 
                       TSNOW = 1., 
                       SNOEVP = 0.1, 
                       CCFACT = 1., 
                       MWATER = 0.03, 
                       MGMELT = 0.25
                       ):
        """Sets snowpack parameter values."""

        self.RDCSN  = RDCSN  # snow density relative to water
        self.TSNOW  = TSNOW  # air temperature where snow occurs
        self.SNOEVP = SNOEVP # snow evaporation field adjustment
        self.CCFACT = CCFACT # condensation/convection field adjustment
        self.MWATER = MWATER # maximum water content of the snow pack
        self.MGMELT = MGMELT # maximum snowmelt rate (mm/day)

    def get_snow_parm2(self):
        """Returns the SNOW-PARM2 values as a tuple."""

        return (self.operation, self.RDCSN, self.TSNOW, self.SNOEVP, 
                self.CCFACT, self.MWATER, self.MGMELT)

    def set_snow_init1(self, 
                       packsnow = 0., 
                       packice = 0., 
                       packwatr = 0., 
                       RDENPF = 0.2, 
                       DULL = 400., 
                       PAKTMP = 0.
                       ):
        """Sets the initial snow pack conditions. Note all are in water-
        equivalent units."""

        self.packsnow = packsnow  # snow in pack (mm water)
        self.packice  = packice   # ice in snow pack (mm water)
        self.packwatr = packwatr  # water in snow pack (mm water)
        self.RDENPF   = RDENPF    # relative density of snow and ice to water
        self.DULL     = DULL      # dullness of pack (for albedo estimate)
        self.PAKTMP   = PAKTMP    # mean temperature of snow pack (C)

    def get_snow_init1(self):
        """Returns the values of SNOW-INIT1 as a tuple."""

        return (self.operation, self.packsnow, self.packice, self.packwatr,
                self.RDENPF, self.DULL, self.PAKTMP)

    def set_snow_init2(self, 
                       COVINX = 12., 
                       XLNMLT = 2.5, 
                       SKYCLR = 0.
                       ):
        """Sets the initial values of other snow parameters."""

        self.COVINX = COVINX # snow pack depth needed to cover segment (mm)
        self.XLNMLT = XLNMLT # increment to ice storage in the pack
        self.SKYCLR = SKYCLR # clear sky fraction

    def get_snow_init2(self):
        """Returns the values of SNOW-INIT2 as a tuple."""

        return (self.operation, self.COVINX, self.XLNMLT, self.SKYCLR)

    def set_sld_parm1(self, VASD = 0, VRSD = 0, SDOP = 0):
        """Sets the flags of the sediment transport parameters."""

        self.VASD = VASD # monthly accumulation rate of solids flag
        self.VRSD = VRSD # unit removal flag
        self.SDOP = SDOP # sediment removal algorithm flag
        
    def get_sld_parm1(self):
        """Returns the SLD-PARM1 flags."""

        return (self.operation, self.VASD, self.VRSD, self.SDOP)

    def set_sld_parm2(self, 
                      KEIM = 2., 
                      JEIM = 1.8, 
                      ACCSDP = 0.0001, 
                      REMSDP =0.
                      ):
        """Sets the value of the sediment transport parameters."""

        self.KEIM   = KEIM   # solids washoff coefficient (needs work)
        self.JEIM   = JEIM   # solids washoff exponenet (-)
        self.ACCSDP = ACCSDP # rate of deposition (tonnes/ha/day)
        self.REMSDP = REMSDP # fraction of solids storage removed daily (-)

    def get_sld_parm2(self):
        """Returns the SLD-PARM2 parameters as a tuple."""
        
        return (self.operation, self.KEIM, self.JEIM, self.ACCSDP, self.REMSDP)

    def set_sld_stor(self, 
                     SLDS = 0.
                     ):
        """Sets the initial values of the storage."""

        self.SLDS = SLDS # solids storage (tonnes)

    def get_sld_stor(self):
        """Returns the SLD-STOR values."""

        return self.operation, self.SLDS
