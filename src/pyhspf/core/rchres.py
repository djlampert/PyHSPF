# rchres.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: Contains the Rchres class that acts as a container for information
# about an HSPF RCHRES (Reach/Mixed Reservoir).
#
# last updated: 01/10/2015

class Rchres:
    """A class to store and retrieve information for a reach/reservoir 
    for an HSPF model.
    """

    def __init__(self, 
                 operation, 
                 subbasin,
                 name, 
                 length, 
                 delth, 
                 nexits   = 1, 
                 lake     = False,
                 dam      = None, 
                 flow     = None, 
                 velocity = None, 
                 ftable   = None, 
                 HYFG     = False, 
                 ADFG     = False, 
                 CNFG     = False, 
                 HTFG     = False, 
                 SDFG     = False, 
                 GQFG     = False, 
                 OXFG     = False, 
                 NUFG     = False, 
                 PKFG     = False, 
                 PHFG     = False
                 ):
        """Sets the basic reach properties and the activity flags for RCHRES."""

        self.operation = operation
        self.subbasin  = subbasin
        self.landtype  = 'Reach'  # for generic HSPF operations; always "Reach"
        self.name      = name
        self.length    = length
        self.delth     = delth
        self.flow      = flow
        self.velocity  = velocity
        self.nexits    = nexits
        self.ftable    = ftable
        self.dam       = dam

        if dam is not None: self.lake = True
        else:               self.lake = False

        self.HYFG = HYFG
        self.ADFG = ADFG
        self.CNFG = CNFG
        self.HTFG = HTFG
        self.SDFG = SDFG
        self.GQFG = GQFG
        self.OXFG = OXFG
        self.NUFG = NUFG
        self.PKFG = PKFG
        self.PHFG = PHFG

    def set_hydr_parms(self, 
                       units # English or Metric
                       ):
        """Shortcut function to set each PWAT block."""

        self.units = units 

        self.set_hydr_parm1()
        self.set_hydr_parm2()
        self.set_hydr_init()

        if self.ftable is None: self.set_ftable()

    def set_hydr_parm1(self, 
                       VCFG   = 0, 
                       ODFVFG = 4, 
                       ODGTFG = 0, 
                       A1FG   = 1,
                       A2FG   = 1, 
                       A3FG   = 1, 
                       FUNCT  = 1
                       ):
        """Sets the hydraulic flags."""

        self.VCFG   = VCFG
        self.ODFVFG = ODFVFG
        self.ODGTFG = ODGTFG
        self.A1FG   = A1FG
        self.A2FG   = A2FG
        self.A3FG   = A3FG
        self.FUNCT  = FUNCT

        # hack way to set English defaults

        if self.units == 'English': self.DB50 = 0.01

    def get_hydr_parm1(self):
        """Returns the HYDR-PARM1."""
        
        return ((self.operation, self.VCFG, self.A1FG, self.A2FG, self.A3FG,) +
                tuple(self.ODFVFG for i in range(self.nexits)) + 
                tuple(0 for i in range(5 - self.nexits)) +
                tuple(self.ODGTFG for i in range(self.nexits)) +
                tuple(0 for i in range(5 - self.nexits)) +
                tuple(self.FUNCT for i in range(5)))

    def set_hydr_parm2(self, 
                       FDSN  = 0, 
                       STCOR = 0, 
                       KS    = 0.5, 
                       DB50  = 0.25
                       ):
        """Sets some of the hydraulic parameters."""

        self.FDSN  = FDSN
        self.STCOR = STCOR
        self.KS    = KS
        self.DB50  = DB50

    def get_hydr_parm2(self):
        """Returns the HYDR-PARM2 values."""

        return (self.operation, self.FDSN, self.operation, self.length, 
                self.delth, self.STCOR, self. KS, self.DB50)

    def set_hydr_init(self, 
                      VOL    = None, 
                      CAT    = 0, 
                      CATI   = 4, 
                      OUTDGT = 0
                      ):
        """Sets the intial values for the HYDR state variables."""

        if VOL is None:

            if self.units == 'Metric':
                conv1 = 10.764 # ft2 per m2
                conv2 = 1000   # km2 * mm to Mm3
            elif self.units == 'English':
                conv1 = 1
                conv2 = 43560 / 5280 # miles ft2 to acre-ft

            if self.lake:

                v = self.dam.norm_storage

                # convert

                if   self.units == 'Metric':  conv = 0.0012335
                elif self.units == 'English': conv = 1

                # set the lake volume 1 % above the minimum level to prevent 
                # numerical errors

                self.VOL = v * conv * 1.01

            else:

                if self.flow is None or self.velocity is None:

                    area = 10
                    volume = 1

                else:

                    area   = self.flow / self.velocity / conv1  # m2 or ft2
                    volume = self.length * area / conv2         # Mm3 or acre-ft
            
                # winter flows are well below average so set to 1/5

                self.VOL = volume / 5

        else: self.VOL = VOL

        self.CAT    = CAT
        self.CATI   = CATI
        self.OUTDGT = OUTDGT

    def get_hydr_init(self):
        """Gets the values for HYDR-INIT."""

        return ((self.operation, self.VOL,) + 
                tuple(self.CATI for i in range(5)) +
                tuple(self.OUTDGT for i in range(5)))
 
    def set_ftable(self):
        """Sets the FTABLE for the reach."""

        from pyhspf.core.ftable import make_ftable, lake_ftable

        s = self.delth / self.length / 1000

        # work around for very flat areas

        if s < 0.00001: s = 0.00001

        if self.flow is None or self.velocity is None:
            print('warning, insufficient information to build ftable')
            raise

        if not self.lake:
            self.ftable = make_ftable(self.flow, self.velocity, self.length, s)

        elif self.dam is not None:
            self.ftable = lake_ftable(self.flow, self.velocity, self.length, s,
                                      self.dam)

    def get_ftable(self):
        """Returns the FTABLE for the reach."""

        return self.ftable

    def set_sandfg(self, 
                   SANDFG = 3
                   ):
        """Sets the SANDFG for the reach."""

        self.SANDFG = SANDFG # sand algorithm (1:Toffaleti, 2:Colby, 3:Power) 

    def get_sandfg(self):
        """Returns the SANDFG for the reach."""

        return self.operation, self.SANDFG

    def set_genparm(self, 
                    BEDWID = None, 
                    BEDWRN = None, 
                    POR = 0.5
                    ):
        """Sets the physical characterstics of the bed. Uses the average flow
        and velocity and assumes a 3:1 width:depth ratio. Since sediment 
        transport is driven by higher flow, assumes 3 times average."""
        
        if not self.lake:

            areasqft = self.flow / self.velocity

            # convert average area to 3x in meters for higher flows

            area     = 3 * areasqft * 0.3048**2
            width    = 3 * math.sqrt(area / 3)
            depth    = math.sqrt(area / 3)

            if BEDWID is None: self.BEDWID = max(0.3, width)    # width (m)
            if BEDWRN is None: self.BEDWRN = max(0.0003, depth) # depth (m)

        else:

            self.BEDWRN = self.dam.height
            self.BEDWID = 10 * math.sqrt(self.dam.max_storage / 10 / 
                                         self.dam.height)

        self.POR = POR  # porostiy (-)

    def get_genparm(self):
        """Returns the SED-GENPARM values."""

        return self.operation, self.BEDWID, self.BEDWRN, self.POR

    def set_sand_pm(self, 
                    W      = 0.5, 
                    RHO    = 2.5, 
                    KSAND  = 0.1, 
                    EXPSND = 2.
                    ):
        """Sets the values for physical properties of the sand particles."""

        # These parameters are not unique in HSPF so had to add "sand"

        self.Dsand   = self.DB50 # effective particle diameter (mm)
        self.Wsand   = W         # sand fall velocity (mm/s); note inconsistency
        self.RHOsand = RHO       # sand particle density (g/cm3)

        # note the following are only used for sandfg = 3

        self.KSAND  = KSAND   # sandload coefficient (complex)
        self.EXPSND = EXPSND  # sandload exponent (-)

    def get_sand_pm(self):
        """Returns the SAND-PM values."""

        return (self.operation, self.Dsand, self.Wsand, self.RHOsand, 
                self.KSAND, self.EXPSND)

    def set_silt_pm(self, 
                    D     = 0.05, 
                    W     = 0.007, 
                    RHO   = 2.65, 
                    TAUCD = 0.005,
                    TAUCS = 0.01, 
                    M     = 0.001
                    ):
        """Sets the values for the physical properties of silt."""

        # These parameters are not unique in HSPF so had to add "silt"

        self.Dsilt     = D      # silt diameter (mm)
        self.Wsilt     = W      # silt velocity (mm/s)
        self.RHOsilt   = RHO    # silt particle density (g/cm3)
        self.TAUCDsilt = TAUCD  # silt critical bed shear stress (kg/m2)
        self.TAUCSsilt = TAUCS  # silt critical scour shear stress (kg/m2)
        self.Msilt     = M      # silt erobability coefficient (kg/m2/day)

    def get_silt_pm(self):
        """Returns the SILT-CLAY-PM values (for silt)."""

        return (self.operation, self.Dsilt, self.Wsilt, self.RHOsilt,
                self.TAUCDsilt, self.TAUCSsilt, self.Msilt)

    def set_clay_pm(self, 
                    D     = 0.01, 
                    W     = 0.0002, 
                    RHO   = 2.65, 
                    TAUCD = 0.005,
                    TAUCS = 0.01, 
                    M     = 0.001
                    ):
        """Sets the values for the physical properties of silt."""

        # These parameters are not unique in HSPF so had to add "clay"

        self.Dclay     = D      # clay diameter (mm)
        self.Wclay     = W      # clay velocity (mm/s)
        self.RHOclay   = RHO    # clay particle density (g/cm3)
        self.TAUCDclay = TAUCD  # clay critical bed shear stress (kg/m2)
        self.TAUCSclay = TAUCS  # clay critical scour shear stress (kg/m2)
        self.Mclay     = M      # clay erobability coefficient (kg/m2/day)

    def get_clay_pm(self):
        """Returns the SILT-CLAY-PM values (for silt)."""

        return (self.operation, self.Dclay, self.Wclay, self.RHOclay,
                self.TAUCDclay, self.TAUCSclay, self.Mclay)
    
    def set_ssed_init(self, 
                      Sand = 5., 
                      Silt = 25., 
                      Clay = 25.
                      ):
        """Sets the initial values for the suspended solids concentrations."""

        self.Sand = Sand  # sand concentration (mg/L)
        self.Silt = Silt  # silt concentration (mg/L)
        self.Clay = Clay  # clay concentration (mg/L)

    def get_ssed_init(self):
        """Returns the initial values for suspended solids concentrations."""

        return self.operation, self.Sand, self.Silt, self.Clay

    def set_bed_init(self, 
                     BEDDEP = 1., 
                     Sand = 0.8, 
                     Silt = 0.1, 
                     Clay = 0.1
                     ):
        """Sets the initial values for the bed depth and particle fractions."""

        # note that again these are not unique names in HSPF so modified

        self.BEDDEP = min(BEDDEP, self.BEDWRN * 0.99)
        self.FSand  = Sand
        self.FSilt  = Silt
        self.FClay  = Clay

    def get_bed_init(self):
        """Returns the BED-INIT values."""

        return self.operation, self.BEDDEP, self.FSand, self.FSilt, self.FClay
