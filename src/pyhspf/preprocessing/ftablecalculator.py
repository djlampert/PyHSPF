# ftablecalculator.py
#
# David Lampert (djlampert@gmail.com)
#
# last updated: 09/02/2015
#

import os, pickle

from scipy      import log, exp, sqrt, stats, special, optimize
from matplotlib import pyplot

class FtableCalculator:
    """
    A class to estimate the FTABLE for an HSPF model using data from a gage
    station from the National Water Information System (NWIS).
    """

    def __init__(self,
                 gagedata,
                 a1 = None,
                 a2 = None,
                 b1 = None,
                 b2 = None,
                 xmin = None,
                 xupper = None,
                 ):
        """
        Opens the NWIS gage station data stored as an instance of the PyHSPF 
        GageStation class from the file path and reads the stage-discharge
        data and channel geometry estimates.
        """

        with open(gagedata, 'rb') as f: station = pickle.load(f)        

        self.station = station

        # gage station data from NWIS

        self.flows   = []
        self.heights = []
        self.areas   = []
        self.widths  = []

        # get the data from the measurements

        for t, data in self.station.measurements:

            self.flows.append(data['flow (cfs)'])
            self.heights.append(data['height (ft)'])
            self.areas.append(data['area (ft2)'])
            self.widths.append(data['width (ft)'])

        # pointers to regression values

        self.a1     = a1     # regression coefficient for stage-discharge
        self.a2     = a2     # regression coefficient for stage-discharge
        self.b1     = b1     # regression coefficient for stage-width
        self.b2     = b2     # regression coefficient for stage-width
        self.xmin   = xmin   # minimum stage value for FTABLE
        self.xupper = xupper # maximum stage value for FTABLE

    def power_fit(self,
                  xs, 
                  ys,
                  ):
        """
        Performs a power law fit to the data using a log-log transform followed
        by simple linear regression.
        """

        xt = log(xs)
        yt = log(ys)

        m, b, r, p, e = stats.linregress(xt, yt)

        return exp(b), m, r

    def get_logspaced(self,
                      xmin,
                      xmax,
                      n = 16,
                      ):
        """
        Returns a list of equidistant values in log space between xmin and xmax.
        """

        values = []
        for i in range(n + 1):

            # calculate the log value

            logv = log(xmin) + (log(xmax) - log(xmin)) * i / n

            # transform it and add it to the list

            values.append(exp(logv))

        return values

    def calculate_regressions(self,
                              ):
        """
        Performs power law regression on the stage-discharge and channel width
        data from the gage station.
        """

        # estimate the flow depths assuming a triangular profile

        depths = [2 * a / w for a, w in zip(self.areas, self.widths)]

        # do a power law fit for the stage-discharge

        a1, b1, r1 = self.power_fit(depths, self.flows)

        # save the regression

        self.a1 = a1
        self.b1 = b1
        self.r1 = r1

        # do a power law fit for the widths

        a2, b2, r2 = self.power_fit(depths, self.widths)

        # save the fit

        self.a2 = a2
        self.b2 = b2
        self.r2 = r2

        # get min and max points for a line

        self.xmin = min(depths)
        self.xmax = max(depths)

        qmin = self.a1 * self.xmin**self.b1
        qmax = self.a1 * self.xmax**self.b1

        self.xupper = ((5 * qmax) / self.a1)**(1 / self.b1)

    def create_ftable(self,
                      length,
                      units = 'Metric',
                      ):
        """
        Uses the regressions for channel width and flow and the supplied
        channel length to compute an HSPF FTABLE for the reach. The FTABLE
        is a list of up to 18 values of channel discharge, volume, and surface
        area as a function of the channel depth. The algorithm assumes the 
        data are in default NWIS units (cfs, ft, etc.) but will compute in 
        either English or Metric units. The length must be provided in miles 
        or kilometers consistent with the unit selection.
        """

        required = self.a1, self.a2, self.b1, self.b2, self.xmin, self.xupper

        if any([r is None for r in required]):
            
            print('error, insufficient information supplied')
            raise

        if units == 'Metric':

            # convert the regression coefficients from feet to meters

            a1 = self.a1 * 0.3048**(3 - self.b1)
            a2 = self.a2 * 0.3048**(1 - self.b2)

            # convert the x values from feet to meters

            xmin   = self.xmin * 0.3048
            xupper = self.xupper * 0.3048

            # conversion factors
            
            c1 = 0.1     # m * km to hectares
            c2 = 0.001   # m2 * km to Mm3

        elif units == 'English':

            a1 = self.a1
            a2 = self.a2
 
            xmin = self.xmin
            xupper = self.xupper

            c1 = 5280 / 43560  # ft * mi to acres
            c2 = 5280 / 43560  # ft2 * mi to acre-ft

        b1 = self.b1
        b2 = self.b2

        # make a list of depths

        xs = self.get_logspaced(xmin, xupper)

        # calculate the corresponding flows

        qs = [a1 * x**b1 for x in xs]

        # calculate the corresponding widths

        ws = [a2 * x**b2 for x in xs]

        # the ftable is a list of lists; each sublist contains four elements:
        # stage (ft or m), surface area (acres or ha), volume (acre-ft or
        # Mm3), and outflow (ft3/s or m3/s). the first row must be zeros.

        ftable = [[0, 0, 0, 0]]

        # iterate through the values and add them to the table

        for x, w, q in zip(xs, ws, qs):

            # calculate the surface area and the volume of the reach

            area = w * length * c1
            volume = x * w * length / (b2 + 1) * c2
            
            # add the data to the table

            ftable.append([x, area, volume, q])

        return ftable

    def extend_ftable(self,
                      qref,
                      qavg,
                      length,
                      units = 'Metric',
                      ):
        """
        Estimates an FTABLE for a hydraulically-similar reach to the gage
        with average flow "qref" to a new reach based on its average flow 
        "qavg" and length "length." The units can be Metric or English, and
        the cutoff is the lower limit on adjustments to depth limits.
        """

        required = self.a1, self.a2, self.b1, self.b2, self.xmin, self.xupper

        if any([r is None for r in required]):
            
            print('error, insufficient information supplied')
            raise

        # units and conversion factors

        if units == 'Metric':

            # conversion factors
            
            c1 = 0.3048                          # ft to m
            c2 = 0.1                             # m * km to hectares
            c3 = 0.001                           # m2 * km to Mm3

        elif units == 'English':

            xmin = self.xmin
            xupper = self.xupper

            c1 = 1             # ft to ft
            c2 = 5280 / 43560  # ft * mi to acres
            c3 = 5280 / 43560  # ft2 * mi to acre-ft

        # estimate the regression parameters for the flow (q = a1 * H**b1)
        # assume b1 is the same, but a1 is square root of the flow ratio
        # since q1 / q2 ~ A1 / A2 ~ (H1 / H2)**2

        a1 = self.a1 * (qavg / qref)**0.5
        b1 = self.b1

        # estimate the regression parameters for the width (w = a2 * H**b2)
        # assume b2 is the same, but a2 is square root of the flow ratio
        # since q1 / q2 ~ A1 / A2 ~ (W1 / W2) * (H1 / H2)

        a2 = self.a2 * (qavg / qref)**0.5
        b2 = self.b2
       
        # iterate through each depth from the reference equation, adjust it
        # to the new depth given the relative flows, calculate the width
        # and cross-sectional area, then get the flow with Manning's Equation

        ftable = [[0, 0, 0, 0]]

        for H in self.get_logspaced(self.xmin, self.xupper):

            # calculate the width (ft)

            W = a2 * H**(b2)

            # calculate the cross-sectional area (ft2)

            A = a2 * H**(b2 + 1) / (b2 + 1)

            # calculate the flow (ft3/s)

            Q = a1 * H**b1

            # use the reach length and conversion factors to fill in the table

            row = [H * c1, 
                   W * c1 * length * c2, 
                   A * c1**2 * length * c3, 
                   Q * c1**3]

            ftable.append(row)

        return ftable

    def plot_regressions(self,
                         show = False,
                         output = None,
                         ):
        """
        Makes a plot of the regressions for the station.
        """

        required = self.a1, self.a2, self.b1, self.b2, self.xmin, self.xupper

        if any([r is None for r in required]):
            
            print('error, insufficient information supplied')
            raise

        # calculate the depths

        depths = [2 * a / w for a, w in zip(self.areas, self.widths)]

        # calculate a range of data for the plot

        xs = self.get_logspaced(self.xmin, self.xupper)
        qs = [self.a1 * x**self.b1 for x in xs]

        qmin = self.a1 * self.xmin**self.b1
        qmax = self.a1 * self.xmax**self.b1

        # formula for the text box

        q_regression = ('$q={:.3f}$'.format(self.a1) + '$H^{' + 
                        '{:.2f}'.format(self.b1) + '}$\n' + '$r^2\!=' + 
                        '{:.3f}$'.format(self.r1**2))

        # calculate the widths for each stage point

        ws = [self.a2 * x**self.b2 for x in xs]

        wmin = self.a2 * self.xmin**self.b2
        wmax = self.a2 * self.xmax**self.b2

        w_regression = ('$w={:.3f}$'.format(self.a2) + '$H^{' + 
                        '{:.2f}'.format(self.b2) + '}$\n' + '$r^2\!=' + 
                        '{:.3f}$'.format(self.r2**2))

        # calculate the implied area

        implied_areas = [self.a2 * x**(self.b2 + 1) / (self.b2 + 1) for x in xs]

        amin = self.a2 * self.xmin**(self.b2 + 1) / (self.b2 + 1)
        amax = self.a2 * self.xmax**(self.b2 + 1) / (self.b2 + 1)

        a_equation = ('$A={:.3f}$'.format(self.a2 / (self.b2 + 1)) + 
                      '$H^{' + '{:.2f}'.format(self.b2 + 1) + '}$')

        # make the plot

        fig = pyplot.figure(figsize = (7,10))

        its = self.station.gageid, self.station.name
        fig.suptitle('Flow and Channel Measurements and Power-Law Fits\n' +
                     'NWIS Gage {}\n{}'.format(*its), y = 0.97)

        sub1 = fig.add_subplot(311)
        sub2 = fig.add_subplot(312, sharex = sub1)
        sub3 = fig.add_subplot(313, sharex = sub1)

        # stage-discharge

        sub1.plot(depths, self.flows, '+', color = 'blue', 
                  label = 'observations')
        sub1.plot((self.xmin, self.xmax), (qmin, qmax), '-', color = 'black', 
                  label = 'regression')
        sub1.plot(xs, qs, '*', color = 'red', label = 'ftable values')

        # formatting

        sub1.set_xlabel('Stage, $H$ (ft)')
        sub1.set_ylabel('Discharge, $q$ (cfs)')

        # add a text label for the fitting

        sub1.text(self.xmin, qmax, q_regression, ha = 'left', va = 'top')

        # legend

        sub1.legend(loc = 'lower right', fontsize = 10)

        # stage-width

        sub2.plot(depths, self.widths, '+', color = 'blue', 
                  label = 'observations')
        sub2.plot((self.xmin, self.xmax), (wmin, wmax), '-', color = 'black', 
                  label = 'regression')
        sub2.plot(xs, ws, '*', color = 'red', label = 'ftable values')

        # add a text label for the fitting

        sub2.text(self.xmin, wmax, w_regression, ha = 'left', va = 'top')

        # formatting

        sub2.set_xlabel('Stage, $H$ (ft)')
        sub2.set_ylabel('Channel Width, $w$ (ft)')

        # legend

        sub2.legend(loc = 'lower right', fontsize = 10)

        # stage-area

        sub3.plot(depths, self.areas, '+', color = 'blue', 
                  label = 'observations')
        sub3.plot((self.xmin, self.xmax), (amin, amax), '-', color = 'black',
                  label = 'regression')
        sub3.plot(xs, implied_areas, '*', color = 'red', 
                  label = 'ftable values')

        # formatting

        sub3.set_xlabel('Stage, $H$ (ft)')
        sub3.set_ylabel('Cross-Sectional Area, $A$ (ft$^2\!$)')

        # add a text box with the equation

        sub3.text(self.xmin, amax, a_equation, ha = 'left', va = 'top')

        # legend

        sub3.legend(loc = 'lower right', fontsize = 10)

        # formatting

        sub1.set_xscale('log')
        sub1.set_yscale('log')
        sub2.set_xscale('log')
        sub2.set_yscale('log')
        sub3.set_xscale('log')
        sub3.set_yscale('log')

        pyplot.subplots_adjust(hspace = .35)

        if output is not None: pyplot.savefig(output)

        if show: pyplot.show()

        pyplot.clf()
        pyplot.close()
