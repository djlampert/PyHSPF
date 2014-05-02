# specialactions.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Purpose: A class to store information about special actions for HSPF.
#
# last updated: 04/27/2014

class SpecialAction:
    """A class to store information about a special action and communicate
    it to the HSPF model."""

    def __init__(self, action, subbasin, landtype, date, tstep, units,
                 recurrence, value):
        """Constructor method."""

        self.action     = action
        self.subbasin   = subbasin
        self.landtype   = landtype
        self.date       = date
        self.tstep      = tstep
        self.units      = units
        self.recurrence = recurrence
        self.value      = value

    def get_action(self, landtypes):
        """Returns info for the special action."""

        # find the operation type

        if   self.landtype == 'Impervious': op = 'IMPLND'
        elif self.landtype == 'Reach':      op = 'RCHRES'
        else:                               op = 'PERLND'

        # find the operation number

        n = landtypes[self.subbasin][self.landtype].operation

        # format the date string

        it = self.date.year, self.date.month, self.date.day, self.date.hour
        date = '{:04d}/{:02d}/{:02d} {:2d}'.format(*it)

        # repeat

        if self.recurrence == 'annual':

            tc  = 'YR'
            ts  = 1
            num = 999
            y   = ' EVERY YEAR'
            
        else: y = ' ONCE'

        # increase infiltration due to thawed ground

        if self.action == 'thaw': 

            var   = 'INFILT' # operation variable name
            tcode = 3        # time code if deferral (same as wdm tcode; 3=days)
            s1    = 0        # almost always 1, unless it's mulitphase variable
            s2    = 0        # almost always 1, unless it's mulitphase variable
            s3    = 0        # almost always 1, unless it's mulitphase variable
            acode = 1        # see special actions; 1 means assign value

            # get the value (and annoyingly need to convert to internal units)

            if self.units == 'Metric': mfact = self.tstep / 60 / 25.4
            else:                      mfact = self.tstep / 60

            if self.value is None: val = 0.2 * mfact
            else:                  val = self.value * mfact

            l = ('  *** INCREASE INFILT DUE TO THAWED GROUND{}\n'.format(y) +
                 '  *** op nos.  dcdts  yr mo dy hr mn d t   vnam  ' +
                 's1 s2 s3 ac  Quantity tc  ts rp\n' +
                 '  <****><-><--><><-><--> <> <> <> <><><>  <----><->' +
                 '<-><-><-><--------> <> <-><->\n')

        # decrease infiltration due to frozen ground

        elif self.action == 'frozen':
 
            var   = 'INFILT' # operation variable name
            tcode = 3        # time code if deferral (same as wdm tcode; 3=days)
            s1    = 0        # almost always 1, unless it's mulitphase variable
            s2    = 0        # almost always 1, unless it's mulitphase variable
            s3    = 0        # almost always 1, unless it's mulitphase variable
            acode = 1        # see special actions; 1 means assign value

            # get the value (and annoyingly need to convert to internal units)

            if self.units == 'Metric': mfact = self.tstep / 60 / 25.4
            else:                      mfact = self.tstep / 60

            if self.value is None: val = 1. * mfact
            else:                  val = self.value * mfact

            l = ('  *** DECREASE INFILT DUE TO FROZEN GROUND{}\n'.format(y) +
                 '  *** op nos.  dcdts  yr mo dy hr mn d t   vnam  ' +
                 's1 s2 s3 ac  Quantity tc  ts rp\n' +
                 '  <****><-><--><><-><--> <> <> <> <><><>  <----><->' +
                 '<-><-><-><--------> <> <-><->\n')

        else:
            print('warning: unknown action {} specified'.format(self.action))
            raise

        it1 = op, n, date, tcode, var
        it2 = s1, s2, s3, acode, val, tc, ts, num

        return (l + '{:>8s}{:3d}{:>22s}{:>7d}  {:<6s}'.format(*it1) +
                '{:3d}{:3d}{:3d}{:3d}{:>10.4f} {:2s}{:3d}{:4d}\n'.format(*it2))
