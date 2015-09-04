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
        area as a function of the channel depth. Assumes the data are in 
        default NWIS units (cfs, ft, etc.) but will compute in either English
        or Metric units. Must provide length in miles or kilometers consistent
        with the unit selection.
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

    def width(self,
              H,
              a,
              b,
              ):
        """
        Returns the width of a channel as a function of the depth, H,
        from a log-log fitting of channel width versus depth with parameters 
        a and b of the form:
        
        W = a * H**b
        """

        return a * H**b

    def area(self,
             H, 
             a, 
             b,
             ):
        """
        Estimates the area, A, of a channel as a function of the depth, H,
        from a log-log fitting of channel width versus depth with parameters 
        a and b of the form:

        W = a * H**b
        """

        A = a * H**(b + 1) / (b + 1)

        return A

    def perimeter(self,
                  H, 
                  a, 
                  b,
                  ):
        """
        Estimates the perimeter, P, of a channel as a function of the depth, H, 
        from a log-log fitting of channel width versus depth with parameters 
        a and b of the form:

        W = a * H**b
        """

        # break the equation into pieces for clarity (see png file)
        
        s1 = sqrt(a**2 * b**2 * H**(2 * b - 2) + 4)
        s2 = sqrt(4 * H**(2 - 2 * b) / a**2 / b**2 + 1)

        # hypergeometric function

        hyp = special.hyp2f1(0.5, 
                             (b - 2) / 2 / (b - 1), 
                             (4 - 3 * b) / (2 - 2 * b), 
                             -4 * H**(2 - 2 * b) / a**2 / b**2,
                             ) 

        last = (b - 2) * (a**2 * b**2 * H**(2 * b) + 4 * H**2)

        denom = (b - 2) * b * (a**2 * b**2 * H**(2 * b) + 4 * H**2)

        P = -1 * (H * s1 * (4 * (b - 1) * H**2 * s2 * hyp - last)) / denom

        return P

    def get_a2(self,
               b, 
               A, 
               P,
               initial = None,
               ):
        """
        Calculates the value of the fitting parameter "a2" given the 
        cross-sectional area, A, and wetted perimeter "P" for a channel with 
        a discharge relationship of the form:

        W = a2 * H**b2

        where W and H are the channel width and channel depth, respectively.
        """

        if initial is None:

            # make an initial guess assuming a 90 degree v-notched channel

            initial = (A / P * 5.65, P / 2.82) 

        # set up two equations and two unknowns for the cross-sectional area 
        # and perimeter of the channel as a function of a and H

        def equations(variables):

            a2, H = variables

            f1 = self.area(H, a2, b) - A
            f2 = self.perimeter(H, a2, b) - P

            return f1, f2

        # solve the nonlinear system using scipy optimize

        try: 

            a2, H = optimize.fsolve(equations, initial)

        except:

            print('warning: unable to solve for the fitting parameter\n')
            a2 = A / P / 5.65

        return a2

    def extend_ftable(self,
                      qref,
                      length,
                      qavg,
                      vavg,
                      slope,
                      n = 0.04,
                      units = 'Metric',
                      ):
        """
        Estimates an FTABLE for a hydraulically-similar reach to the gage
        with average flow qref to a new reach based on its length, 
        average flow, average velocity, and slope.
        """

        required = self.a1, self.a2, self.b1, self.b2, self.xmin, self.xupper

        if any([r is None for r in required]):
            
            print('error, insufficient information supplied')
            raise

        # estimate the area using the average flow and velocity

        A = qavg / vavg

        # estimate the hydraulic radius using Manning's equation

        if units == 'Metric':

            Rh = (n * vavg / slope**0.5)**1.5

            # convert the x values from feet to meters

            xmin   = self.xmin * 0.3048
            xupper = self.xupper * 0.3048

            # conversion factors
            
            a = self.a2 * 0.3048**(1 - self.b2) # ft to m
            c1 = 0.1     # m * km to hectares
            c2 = 0.001   # m2 * km to Mm3
            k  = 1

        elif units == 'English':

            Rh = (n / 1.49 * vavg / slope**0.5)**1.5

            xmin = self.xmin
            xupper = self.xupper

            a  = 1
            c1 = 5280 / 43560  # ft * mi to acres
            c2 = 5280 / 43560  # ft2 * mi to acre-ft
            k  = 1.49

        # estimate the wetted perimeter
            
        P = A / Rh

        # extend the fitting for width vs depth assuming the exponent is the 
        # same by fitting the depth and first coefficient

        a2 = self.get_a2(self.b2, A, P) * a

        # iterate through each depth from the reference equation, adjust it
        # to the new depth given the relative flows, calculate the width
        # and cross-sectional area, then get the flow with Manning's Equation

        ftable = [[0, 0, 0, 0]]

        for x in self.get_logspaced(xmin, xupper):

            # adjust the depth to the new profile (q1 / q2 = (H1 / H2)**b)

            H = x * (qavg / qref)**(1 / self.b1)

            # calculate the width

            W = a2 * H**(self.b2)

            # calculate the cross-sectional area

            A = a2 * H**(self.b2 + 1) / (self.b2 + 1)

            # calculate the wetted perimeter

            P = self.perimeter(H, a2, self.b2)

            # calculate the hydraulic radius

            Rh = A / P

            # calculate the velocity using Manning's equation

            V = k / n * slope**0.5 * Rh**(2/3)

            # calculate the flow rate

            Q = V * A

            # use the reach length and conversion factors to fill in the table

            row = [H, W * c1 * length, A * c2 * length, Q]

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
