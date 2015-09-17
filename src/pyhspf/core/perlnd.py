# perlnd.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: Contains the Perlnd class that acts as a container for the 
# information in an HSPF PERLND (Pervious Land Segment).
#
# last updated: 01/10/2015

class Perlnd:
    """
    A data structure for a pervious land segment for an HSPF model.
    """

    def __init__(self, 
                 operation, 
                 subbasin, 
                 landtype, 
                 area, 
                 length, 
                 slope, 
                 melev,
                 belv, 
                 lat, 
                 ATMP = False, 
                 SNOW = False, 
                 PWAT = False,  
                 SED  = False, 
                 PST  = False, 
                 PWG  = False, 
                 PQAL = False, 
                 MSTL = False, 
                 PEST = False, 
                 NITR = False, 
                 PHOS = False, 
                 TRAC = False
                 ):
        """Sets up the modules and basic info for the PERLND."""

        # identification information

        self.operation  = operation  # the HSPF operation number (1-999)
        self.subbasin   = subbasin   # the subbasin number of the outlet
        self.landtype   = landtype   # the landuse category

        # physically-based parameters (units consistent with HSPF formulation)

        self.area       = area     # the area of the land segment
        self.MELEV      = melev    # the mean elevation of the segment
        self.BELV       = belv     # the bottom elevation of the segment
        self.lat        = lat      # the mean latitude of the segment
        self.LSUR       = length   # overland flow plane length (0.3 m or more)
        self.SLSUR      = slope    # overland flow slope

        # the rest are flags for HSPF modules

        self.ATMP       = ATMP
        self.SNOW       = SNOW
        self.PWAT       = PWAT
        self.SED        = SED
        self.PST        = PST
        self.PWG        = PWG
        self.PQAL       = PQAL
        self.MSTL       = MSTL
        self.PEST       = PEST
        self.NITR       = NITR
        self.PHOS       = PHOS
        self.TRAC       = TRAC

    def set_land_parms(self, monLZETP = False):
        """Set parameters related to land use categories."""

        crops = ['Corn', 
                 'Soybeans', 
                 'Other grain', 
                 'Hay/alfalfa', 
                 'Other',
                 'Agriculture'
                 ]

        # forest cover percent

        if self.landtype == 'Forest': self.FOREST = 0.75 
        else:                         self.FOREST = 0. 

        # lower zone evapotranspiration parameter. the following values are
        # defaults, and the LZETP_multiplier in the HSPFModel class is used to 
        # control these for land use categories in a given watershed.

        if   self.landtype == 'Water/wetland': self.LZETP = 0.8
        elif self.landtype == 'Forest':        self.LZETP = 0.6

        elif self.landtype in crops and monLZETP:
            self.LZETP = 0
            self.VLE   = 1
            self.set_monthly('LZETPARM',
                             values = [0.1, 0.1,  0.1,  0.2,  0.3,  0.4,
                                       0.5, 0.6,  0.6,  0.3,  0.2,  0.1])

        elif self.landtype == 'Pasture/grass': self.LZETP = 0.3
        elif self.landtype == 'Developed':     self.LZETP = 0.2
        elif self.landtype == 'Fallow land':   self.LZETP = 0.1
        else:                                  self.LZETP = 0.5

        # canopy storage

        if self.landtype == 'Corn':
            if self.units == 'Metric':
                values = [0.1, 0.1, 0.3, 0.7, 1.3, 2, 
                          3.6, 5., 5., 1.3, 0.3, 0.1]
            elif self.units == 'English':
                values = [0.02, 0.02, 0.02, 0.05, 0.08, 0.1,
                          0.14, 0.19, 0.20, 0.1, 0.03, 0.02]
                
            self.CEPSC = 0
            self.VCSFG = 1
            self.set_monthly('INTERCEP', values = values)

        if self.landtype == 'Soybeans':
            if self.units == 'Metric':
                values = [0.1, 0.1, 0.3, 0.6, 1., 1.5, 
                          2., 2., 2., 1., 0.3, 0.1]
            elif self.units == 'English':
                values = [0.02, 0.02, 0.02, 0.03, 0.06, 0.1,
                          0.1, 0.1, 0.1, 0.06, 0.03, 0.02]               
            self.CEPSC = 0
            self.VCSFG = 1
            self.set_monthly('INTERCEP', values = values)

        # nominal soil moisture capacity (related to rooting depth)

        if self.units == 'Metric':

            lzsns = {'Water/wetland': 200,
                     'Forest':        200,
                     'Agriculture':   150,
                     'Pasture/grass': 100,
                     'Developed':      50,
                     'Fallow land':    50
                     }
            lzsndefault = 100

        elif self.units == 'English':

            lzsns = {'Water/wetland': 8,
                     'Forest':        8,
                     'Agriculture':   6,
                     'Pasture/grass': 4,
                     'Developed':     2,
                     'Fallow land':   2
                     }
            lzsndefault = 4

        if   self.landtype in crops:     self.LZSN = lzsns['Agriculture']
        elif self.landtype not in lzsns: self.LZSN = lzsndefault
        else:                            self.LZSN = lzsns[self.landtype]

        # high water table (wetlands)

        self.HWT = 0
        if self.landtype == 'Water/wetland': 
            #self.HWT = 1 # wetland flag (had issues trying this--needs work)
            self.AGWETP = 0.7 # PET fraction satisfied by groundwater (0 to 1)
            self.RTOP   = 2
        else:                           
            #self.HWT = 0
            self.AGWETP = 0
            self.RTOP = 1

    def set_pwat_parms(self, 
                       units,     # English or Metric
                       VCS  = 1,  # variable monthly canopy storage flag
                       VUZ  = 0,  # variable monthly upper zone storage flag
                       VNN  = 1,  # variable monthly Manning n flag
                       VIFW = 0,  # variable monthly interflow inflow flag
                       VIRC = 0,  # variable monthly interflow recession flag
                       VLE  = 0   # variable monthly lower zone ET parm flag
                       ):
        """Shortcut function to set each PWAT block."""

        self.units = units

        self.set_pwat_parm1(VCS = VCS, VUZ = VUZ, VNN = VNN, VIFW = VIFW,
                            VIRC = VIRC, VLE = VLE)
        self.set_pwat_parm2()
        self.set_pwat_parm3()
        self.set_pwat_parm4()
        self.set_pwat_parm5()
        self.set_pwat_parm6()
        self.set_pwat_parm7()

        # sort of hack way of setting English values for parameters

        if self.units == 'English':

            self.PETMAX = 40
            self.PETMIN = 35
            self.INFILT = 0.04
            self.UZSN   = 1.

        # set monthly defaults as needed

        if self.VCS  == 1: self.set_monthly('INTERCEP')
        if self.VUZ  == 1: self.set_monthly('UZSN')
        if self.VNN  == 1: self.set_monthly('MANNING')
        if self.VIFW == 1: self.set_monthly('INTERFLW')
        if self.VIRC == 1: self.set_monthly('IRC')
        if self.VLE  == 1: self.set_monthly('LZETPARM')

        # override default values for landuse-specific files

        self.set_land_parms()

        # initial state variables

        self.set_pwat_state()

    def set_pwat_parm1(self, 
                       CSNO = 0, 
                       RTOP = 1, 
                       UZFG = 1, 
                       VCS  = 0, 
                       VUZ  = 0, 
                       VNN  = 0, 
                       VIFW = 0, 
                       VIRC = 0, 
                       VLE  = 0, 
                       IFFC = 1, 
                       HWT  = 0, 
                       IRRG = 0
                       ):
        """sets up flags (PARM1) for the PWATER module."""

        # algorithm flags

        self.CSNO = CSNO    # snow flag (0 or 1)
        self.HWT  = HWT     # high water table flag (0 or 1)
        if self.HWT == 0:   self.RTOP = RTOP  
                            # overland flow routing (0: functional description
                            #                        1: HSPX, ARM and NPS method
                            #                        2: high water table
                            #                        3: high water table)
        else:               self.RTOP = 2
        self.UZFG = UZFG    # upper zone inflow method (0 or 1 unless wetland)
        self.IFFC = IFFC    # frozen ground effect on infiltration (0, 1, 2) 
        #self.HWT  = HWT    # high water table/low gradient (wetlands, 0 or 1)
        self.IRRG = IRRG    # irrigation demand (0, 1, 2, 3)

        # parameters that can vary by month flags (0 or 1)

        self.VCS  = VCS     # interception storage capacity 
        self.VUZ  = VUZ     # upper zone nominal storage parameter
        self.VNN  = VNN     # Manning's n for the overland flow plane
        self.VIFW = VIFW    # interflow inflow parameter
        self.VIRC = VIRC    # interflow recession constant
        self.VLE  = VLE     # lower zone evapotranspiration (E-T) parameter

        # default monthly values for those parameters

        if self.units == 'Metric':
            self.CEPSCdefaults = [0.75, 0.75, 0.75, 2.5, 2.5, 2.5, 3.75, 3.75, 
                                  2.5, 1.25, 0.75, 0.75]
            self.UZSNdefaults  = [63., 61., 57., 50., 44., 39., 38., 40., 45., 
                                  51., 58., 62.]
        elif self.units == 'English':
            self.CEPSCdefaults = [0.03, 0.03, 0.03, 0.1, 0.1, 0.1, 0.15, 0.15, 
                                  0.1, 0.05, 0.03, 0.03]
            self.UZSNdefaults  = [2.5, 2.3, 2.1, 2., 1.8, 1.7, 1.8, 1.8, 1.9, 
                                  2., 2.2, 2.3]
        self.NSURdefaults  = [0.25, 0.24, 0.22, 0.2, 0.17, 0.16, 0.15, 
                              0.16, 0.18, 0.2, 0.23, 0.24]
        self.INTFWdefaults = [10.0 for i in range(12)]
        self.IRCdefaults   = [0.6  for i in range(12)]
        self.LZETPdefaults = [0.5  for i in range(12)]

    def get_pwat_parm1(self):
        """Returns the PWAT-PARM1 flags as a tuple."""

        return (self.operation,     self.CSNO, self.RTOP, self.UZFG, self.VCS, 
                self.VUZ, self.VNN, self.VIFW, self.VIRC, self.VLE, self.IFFC, 
                self.HWT, self.IRRG)
        
    def set_pwat_parm2(self, 
                       INFILT = 1., 
                       KVARY = 0., 
                       AGWRC = 0.95
                       ):
        """Sets the values for some parameters (PWAT2) in the PWATER module.
        Note that LSUR and SLSUR come from physical data so they are not
        set here."""

        #self.FOREST = FOREST # fraction covered by forest (0. to 1.), SNOW only
        #self.LZSN   = LZSN    # lower zone nominal storage (.25 to 2500. mm)
        self.INFILT = INFILT  # infiltration capacity (0.0025 to 2500. mm/hr)
        #self.LSUR   = LSUR   # overland flow plane length (0.3 m or more)
        #self.SLSUR  = SLSUR  # overland flow slope (0.000001 to 10.)
        self.KVARY  = KVARY   # groundwater recession flow (0. mm-1 or more)
        self.AGWRC  = AGWRC   # basic groundwater recession rate, only applies
                              # if KVARY = 0,= today/yest (0.001 to 0.999 day-1)

    def get_pwat_parm2(self):
        """Returns the PWAT-PARM2 parameters as a tuple."""

        return (self.operation, self.FOREST, self.LZSN, self.INFILT, self.LSUR,
                self.SLSUR, self.KVARY, self.AGWRC)

    def set_pwat_parm3(self, 
                       PETMAX = 1., 
                       PETMIN = 0., 
                       INFEXP = 2., 
                       INFILD = 2., 
                       DEEPFR = 0., 
                       BASETP = 0.):
        """Sets the values for some parameters (PWAT3) in the PWATER module."""

        self.PETMAX = PETMAX # air temp where input ET is reduced
        self.PETMIN = PETMIN # air temp below which ET is zero
        self.INFEXP = INFEXP # infiltration equation exponent (0 to 10)
        self.INFILD = INFILD # ratio of max and mean infilt capacities  (1 to 2)
        self.DEEPFR = DEEPFR # inflow to inactive groundwater fraction  (0 to 1)
        self.BASETP = BASETP # fraction of PET satisfied by baseflow    (0 to 1)
        #self.AGWETP = AGWETP # fraction of PET satisfied by groundwater(0 to 1)
        
    def get_pwat_parm3(self):
        """Returns the PWAT-PARM3 parameters as a tuple."""

        return (self.operation, self.PETMAX, self.PETMIN, self.INFEXP, 
                self.INFILD, self.DEEPFR, self.BASETP, self.AGWETP)

    def set_pwat_parm4(self, 
                       CEPSC = 0., 
                       UZSN = 20., 
                       NSUR = 0.1, 
                       INTFW = 10., 
                       IRC = 0.6,
                       ):
        """
        Sets the values for parameters that can vary by month but don't.
        """

        self.CEPSC = CEPSC # interception storage capacity
        self.UZSN  = UZSN  # upper zone nominal storage
        self.NSUR  = NSUR  # Manning's n for the overland flow plane
        self.INTFW = INTFW # interflow inflow parameter
        self.IRC   = IRC   # interflow recession parameter
        #self.LZETP = LZETP # lower zone ET parameter

    def get_pwat_parm4(self):
        """
        Returns the PWAT-PARM4 parameters as a tuple.
        """

        return (self.operation, self.CEPSC, self.UZSN, self.NSUR, self.INTFW,
                self.IRC, self.LZETP)

    def set_pwat_parm5(self, 
                       FZG = 0.0394, 
                       FZGL = 0.1,
                       ):
        """
        Set the values for the parameters relating the effects of ice to
        infiltration rate.
        """

        self.FZG  = FZG
        self.FZGL = FZGL

    def get_pwat_parm5(self):
        """
        Return the PWAT-PARM5 parameters as a tuple.
        """

        return (self.operation, self.FZG, self.FZGL)

    def set_pwat_parm6(self, 
                       GWDATM = None, 
                       PCW = 0.4, 
                       PGW = 0.4, 
                       UPGW = 0.4,
                       ):
        """
        Sets the values of the high water table parameters.
        """

        if GWDATM is None: self.GWDATM = self.BELV - 5
        else:              self.GWDATM = GWDATM

        self.PCW  = PCW   # cohesion water porosity
        self.PGW  = PGW   # gravitational water porosity
        self.UPGW = UPGW  # upper gravitational water porosity

    def get_pwat_parm6(self):
        """
        Returns the PWAT-PARM6 parameters as a tuple.
        """
        
        return (self.operation, self.MELEV, self.BELV, self.GWDATM, self.PCW,
                self.PGW, self.UPGW)

    def set_pwat_parm7(self, 
                       STABNO = 999, 
                       SRRC = 0.1, 
                       SREXP = 2., 
                       IFWSC = 20., 
                       DELTA = 0.025, 
                       UELFAC = 4., 
                       LELFAC = 2.5,
                       ):
        """
        Sets the values for the high water table parameters.
        """

        self.STABNO = STABNO  # ftable number
        self.SRRC   = SRRC    # surface runoff recession coefficient
        self.SREXP  = SREXP   # surface runoff exponent
        self.IFWSC  = IFWSC   # maximum interflow storage capacity
        self.DELTA  = DELTA   # groundwater tolernace level
        self.UELFAC = UELFAC  # multiplier for upper zone storage capacity
        self.LELFAC = LELFAC  # multiplier for lower zone storage capacity

    def get_pwat_parm7(self):
        """
        Returns the values for PWAT-PARM7 as a tuple.
        """

        return (self.operation, self.STABNO, self.SRRC, self.SREXP, self.IFWSC,
                self.DELTA, self.UELFAC, self.LELFAC)
                       
    def set_pwat_state(self, 
                       CEPS = 0., 
                       SURS = 0., 
                       UZS = None, 
                       IFWS = 0., 
                       LZS = None, 
                       AGWS = 0., 
                       GWVS = 0.,
                       ):
        """
        Sets the initial values for the state variables for PWATER.
        """

        self.CEPS = CEPS
        self.SURS = SURS
        self.IFWS = IFWS
        self.AGWS = AGWS
        self.GWVS = GWVS

        # default soil moisture values should be close to storage capacity

        if UZS is None: self.UZS = self.UZSN
        else:           self.UZS = UZS

        if LZS is None: self.LZS = self.LZSN
        else:           self.LZS = LZS

    def get_pwat_state(self):
        """
        Returns the initial values for the state variables for PWATER.
        """

        return (self.operation, self.CEPS, self.SURS, self.UZS, self.IFWS, 
                self.LZS, self.AGWS, self.GWVS)

    def set_monthly(self, 
                    name, 
                    values = None,
                    ):
        """
        Returns a list of the monthly values of the parameter "name." 
        """

        # establish monthly variable value lists

        if values is not None:

            if name == 'INTERCEP': self.monCEPSC = values
            if name == 'UZSN':     self.monUZSN  = values
            if name == 'MANNING':  self.monNSUR  = values
            if name == 'INTERFLW': self.monINTFW = values
            if name == 'IRC':      self.monIRC   = values
            if name == 'LZETPARM': self.monLZETP = values

        else:

            if name == 'INTERCEP': self.monCEPSC = self.CEPSCdefaults
            if name == 'UZSN':     self.monUZSN  = self.UZSNdefaults
            if name == 'MANNING':  self.monNSUR  = self.NSURdefaults
            if name == 'INTERFLW': self.monINTFW = self.INTFWdefaults
            if name == 'IRC':      self.monIRC   = self.IRCdefaults
            if name == 'LZETPARM': self.monLZETP = self.LZETPdefaults

    def get_monthly(self, 
                    name,
                    ):
        """
        Returns a list of the monthly values of the parameter "name." 
        """

        if name == 'INTERCEP': return self.monCEPSC
        if name == 'UZSN':     return self.monUZSN
        if name == 'MANNING':  return self.monNSUR
        if name == 'INTERFLW': return self.monINTFW
        if name == 'IRC':      return self.monIRC
        if name == 'LZETPARM': return self.monLZETP

    def set_atemp_dat(self, 
                      ELDAT = 0., 
                      AIRTMP = 0.,
                      ):
        """
        Sets the data for adjusting gage temperature to land segment temp.
        """

        self.ELDAT  = ELDAT  # elevation difference between gage and segment (m)
        self.AIRTMP = AIRTMP # initial temperature (C)

    def get_atemp_dat(self):
        """
        Returns the ATEMP-DAT values.
        """

        return (self.operation, self.ELDAT, self.AIRTMP)

    def set_ice_flag(self, 
                     ICEFG = 1,
                     ):
        """
        Sets the flag for ice simulation.
        """
        
        self.ICEFG = ICEFG # ice formation flag

    def get_ice_flag(self):
        """
        Returns the values of ICE-FLAG as a tuple.
        """

        return (self.operation, self.ICEFG)

    def set_snow_flags(self, 
                       SNOPFG = 0, 
                       VKMFG = 0,
                       ):
        """
        Sets the flags for the snow module.
        """

        self.SNOPFG = SNOPFG # snow melt algorithm (0: energy, 1: temp index)
        self.VKMFG  = VKMFG  # monthly variable degree day flag 

    def get_snow_flags(self):
        """
        Returns the SNOW-FLAGS as a tuple.
        """

        return (self.operation, self.SNOPFG, self.VKMFG)

    def set_snow_parm1(self, 
                       SHADE = 0.15, 
                       SNOWCF = 1., 
                       COVIND = 10., 
                       KMELT = 0., 
                       TBASE = 0.,
                       ):
        """
        Sets the snow melt parameter values. Note first two pre-defined.
        """

        #self.LAT    = LAT   # latitude
        #self.MELEV  = MELEV # mean elevation (m)
        self.SHADE  = SHADE  # land surface fraction shaded
        self.SNOWCF = SNOWCF # gage catch efficiency factor
        self.COVIND = COVIND # snowpack depth corresponding to full area cov.
        self.KMELT  = KMELT  # degree-day factor for SNOPFG = 1 (mm/d/C)
        self.TBASE  = TBASE  # reference temperature index (C)

    def get_snow_parm1(self):
        """
        Returns the SNOW-PARM1 values as a tuple.
        """

        return (self.operation, self.lat, self.MELEV, self.SHADE, self.SNOWCF,
                self.COVIND, self.KMELT, self.TBASE)

    def set_snow_parm2(self, 
                       RDCSN = 0.12, 
                       TSNOW = 2., 
                       SNOEVP = 0.1, 
                       CCFACT = 1., 
                       MWATER = 0.03, 
                       MGMELT = 0.25,
                       ):
        """
        Sets snowpack parameter values.
        """

        self.RDCSN  = RDCSN  # snow density relative to water
        self.TSNOW  = TSNOW  # air temperature where snow occurs
        self.SNOEVP = SNOEVP # snow evaporation field adjustment
        self.CCFACT = CCFACT # condensation/convection field adjustment
        self.MWATER = MWATER # maximum water content of the snow pack
        self.MGMELT = MGMELT # maximum snowmelt rate (mm/day)

    def get_snow_parm2(self):
        """
        Returns the SNOW-PARM2 values as a tuple.
        """

        return (self.operation, self.RDCSN, self.TSNOW, self.SNOEVP, 
                self.CCFACT, self.MWATER, self.MGMELT)

    def set_snow_init1(self, 
                       packsnow = 0., 
                       packice = 0., 
                       packwatr = 0., 
                       RDENPF = 0.2, 
                       DULL = 400., 
                       PAKTMP = 0.,
                       ):
        """
        Sets the initial snow pack conditions. Note all are in water-
        equivalent units.
        """

        self.packsnow = packsnow  # snow in pack (mm water)
        self.packice  = packice   # ice in snow pack (mm water)
        self.packwatr = packwatr  # water in snow pack (mm water)
        self.RDENPF   = RDENPF    # relative density of snow and ice to water
        self.DULL     = DULL      # dullness of pack (for albedo estimate)
        self.PAKTMP   = PAKTMP    # mean temperature of snow pack (C)

    def get_snow_init1(self):
        """
        Returns the values of SNOW-INIT1 as a tuple.
        """

        return (self.operation, self.packsnow, self.packice, self.packwatr,
                self.RDENPF, self.DULL, self.PAKTMP)

    def set_snow_init2(self, 
                       COVINX = 10., 
                       XLNMLT = 2.5, 
                       SKYCLR = 0.,
                       ):
        """
        Sets the initial values of other snow parameters.
        """

        self.COVINX = COVINX # snow pack depth needed to cover segment (mm)
        self.XLNMLT = XLNMLT # increment to ice storage in the pack
        self.SKYCLR = SKYCLR # clear sky fraction

    def get_snow_init2(self):
        """
        Returns the values of SNOW-INIT2 as a tuple.
        """

        return (self.operation, self.COVINX, self.XLNMLT, self.SKYCLR)

    def set_sed_parm1(self, 
                      CRV = 0, 
                      VSIV = 0, 
                      SDOP = 0,
                      ):
        """
        Sets the sediment transport flags for monthly variables.
        """

        self.CRV  = CRV  # erosion-related cover flag
        self.VSIV = VSIV # net vertical sediment input flag
        self.SDOP = SDOP # land surface removal algorithm flag

    def get_sed_parm1(self):
        """
        Returns the SED-PARM1 values as a tuple.
        """

        return (self.operation, self.CRV, self.VSIV, self.SDOP)

    def set_sed_parm2(self, 
                      SMPF = 1., 
                      KRER = 0.35, 
                      JRER = 2.0, 
                      AFFIX = None,
                      COVER = 0., 
                      NVSI = 1.,
                      ):
        """
        Sets some of the sediment transport parameters.
        """

        self.SMPF  = SMPF  # supporting management practice factor (-)
        self.KRER  = KRER  # soil detachment coefficient (kg mm^-JRER hr^JRER-1)
        self.JRER  = JRER  # soil detachment exponent (-)
        #self.AFFIX = AFFIX# daily detached sediment fractional decrease (-)
        self.COVER = COVER # land surface shielded fraction (-)
        self.NVSI  = NVSI  # sediment deposition (or erosion) rate (kg/ha/day)

        # land use specific parameters

        crops = ['Corn', 'Soybeans', 'Other grain', 'Hay/alfalfa', 'Other',
                 'Agriculture']

        if AFFIX is None:
            if   self.landtype == 'Water/wetland': self.AFFIX = 0.002
            elif self.landtype == 'Forest':        self.AFFIX = 0.002
            elif self.landtype in  crops:          self.AFFIX = 0.01
            elif self.landtype == 'Pasture/grass': self.AFFIX = 0.005
            elif self.landtype == 'Developed':     self.AFFIX = 0.01
            elif self.landtype == 'Fallow land':   self.AFFIX = 0.01
            else:                                 self.AFFIX = 0
        else:               
            self.AFFIX = AFFIX
                     
    def get_sed_parm2(self):
        """
        Returns the SED-PARM2 values as a tuple.
        """

        return (self.operation, self.SMPF, self.KRER, self.JRER, self.AFFIX,
                self.COVER, self.NVSI)

    def set_sed_parm3(self, 
                      KSER = None, 
                      JSER = 2.0, 
                      KGER = 0., 
                      JGER = 1.,
                      ):
        """
        Sets some of the sediment transport parameters.
        """

        self.KSER = KSER  # detached sediment washoff coeff (tonne/ha hr^JSER-1)
        self.JSER = JSER  # detached sediment washoff exponent (-)
        self.KGER = KGER  # matrix soil scour coefficient (needs work)
        self.JGER = JGER  # matrix soil scour exponent (-)

        # land use specific parameters

        crops = ['Corn', 'Soybeans', 'Other grain', 'Hay/alfalfa', 'Other',
                 'Agriculture']

        if KSER is None:
            if   self.landtype == 'Water/wetland': self.KSER = 0.2
            elif self.landtype == 'Forest':        self.KSER = 0.2
            elif self.landtype in  crops:          self.KSER = 1.0
            elif self.landtype == 'Pasture/grass': self.KSER = 0.2
            elif self.landtype == 'Developed':     self.KSER = 0.1
            elif self.landtype == 'Fallow land':   self.KSER = 0.2
            else:                                 self.KSER = 0.2
        else:               
            self.KSER = KSER

    def get_sed_parm3(self):
        """
        Returns the SED-PARM3 values as a tuple.
        """

        return (self.operation, self.KSER, self.JSER, self.KGER, self.JGER)

    def set_sed_stor(self, 
                     DETS = 0.2,
                     ):
        """
        Sets the initial value of the detached sediment.
        """

        self.DETS = DETS

    def get_sed_stor(self):
        """
        Returns the value of the sediment storage.
        """

        return (self.operation, self.DETS)


