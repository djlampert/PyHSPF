# gagestation.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 01/10/2015
#
# Contains the GageStation class that is used to extract, store and manipulate
# data from the USGS National Water Information System gages.

import os, csv, pickle, datetime, io

from urllib     import request
from scipy      import stats
from matplotlib import pyplot, dates

class GageStation:
    """A class to store data from a USGS NWIS gage station."""

    def __init__(self, gageid, name, state, day1, dayn, drain, ave, web):
        """Create a gage and provide the basic information about it."""

        self.gageid = gageid
        self.name   = name
        self.state  = state
        self.day1   = day1
        self.dayn   = dayn
        self.drain  = drain
        self.ave    = float(ave)
        self.web    = web

    def import_csv(self, csvfile, verbose = True):
        """Import data directly from a csv file formatted like the NWIS 
        website."""

        with open(csvfile, 'r') as f:
            reader = csv.reader(f, delimiter = '\t')
            rows = [row for row in reader]

        # get rid of the comment rows

        rows = [row for row in rows if row[0][0] != '#'][2:]

        # transpose the rows and get the dates/flows

        org, gagename, gagedates, gageflows, gageflags = zip(*rows)

        # convert dates to datetime and flows to integers

        self.gagedates = [datetime.datetime.strptime(d, '%Y-%m-%d') 
                          for d in gagedates]
        self.gageflows = [float(g) if g else None for g in gageflows]

    def download_daily_discharge(self, start, end, verbose = True):
        """Downloads the daily discharge data for an NWIS station for the 
        dates (default set in the init method)."""

        if verbose: print('attempting to download daily discharge data ' +
                          'directly from NWIS for {}'.format(self.gageid))

        t = start.year, start.month, start.day, end.year, end.month, end.day - 1

        url = ('http://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&' +
               'period=&begin_date=' +
               '{:4d}-{:02d}-{:02d}&end_date={:4d}-{:02d}-{:02d}'.format(*t) +
               '&site_no={}&referred_module=sw'.format(self.gageid))

        with io.StringIO(request.urlopen(url).read().decode('utf-8')) as input:
            reader = csv.reader(input, delimiter = '\t')
            rows = [row for row in reader]

        # snip the headings and comments

        try:
            rows = [row for row in rows if row[0][0] != '#'][2:]

            # organize the data by type

            org, gagename, gagedates, gageflows, gageflags = zip(*rows)

            self.gagedates = [datetime.datetime.strptime(d, '%Y-%m-%d') 
                              for d in gagedates]
            self.gageflows = [float(g) if g else None for g in gageflows]

            if verbose: print('successfully downloaded data\n')

        except: 
            print('warning: unable to download daily discharge data\n')
            
            self.gagedates = []
            self.gageflows = []

    def download_water_quality(self, verbose = True):
        """Downloads the water quality data for an NWIS station for the 
        dates (default set in the init method)."""

        if verbose: print('attempting to download water quality data ' +
                          'directly from NWIS for {}'.format(self.gageid))

        l = (self.state, self.gageid)

        url = ('http://nwis.waterdata.usgs.gov/' +
               '{:2s}/nwis/qwdata?site_no={}'.format(*l) +
               '&agency_cd=USGS&format=serial_rdb')

        with io.StringIO(request.urlopen(url).read().decode('utf-8')) as input:
            reader = csv.reader(input, delimiter = '\t')
            rows = [row for row in reader]

        # snip the headings and comments

        try:
            rows = [row for row in rows if row[0][0] != '#'][2:]

            # organize the data by type

            (org, n, dates, times, x, x, x, x, x, x, x, x, parm_cds, x, results,
             x, x, x, x, x, x, x) = zip(*rows)

            self.waterquality = {}

            for d, t, parm, r in zip(dates, times, parm_cds, results):

                try:
                    yr, mo, da = int(d[:4]), int(d[5:7]), int(d[8:10])

                    try:    hr, mi = int(t[:2]), int(t[3:5])
                    except: hr, mi = 0, 0

                    value = float(r)

                    day = datetime.datetime(yr, mo, da, hr, mi)

                    if parm in self.waterquality:
                        self.waterquality[parm].append((day, value))
                    else:
                        self.waterquality[parm] = [(day, value)]

                except: pass

            if verbose: print('successfully downloaded data\n')

        except: 
            print('warning: unable to download water quality data\n')
            
            self.waterquality = {}

    def plot(self, output, fmin = 0.001, titlesize = 12, axsize = 11):
        """makes a plot of the flows over time and the flow-duration curve."""

        fig = pyplot.figure(figsize = (8,8))
        s1 = fig.add_subplot(211)
        s2 = fig.add_subplot(212)

        start, end = min(self.gagedates), max(self.gagedates)

        s1.plot_date(self.gagedates, self.gageflows, fmt = 'r-')
        s1.set_title('Hydrograph for {}: {}'.format(self.gageid, self.name), 
                     size = titlesize)
        s1.set_xlim(start, end)
        s1.set_xlabel('Date', size = axsize)
        s1.set_ylabel('Daily Flow (ft\u00B3/s)', size = axsize)

        locator = dates.AutoDateLocator()
        s1.xaxis.set_major_locator(locator)
        s1.xaxis.set_major_formatter(dates.AutoDateFormatter(locator))

        # calculate the cdfs for the flows and transform to z

        norm = stats.norm(0,1)

        # copy the daily flows to a new list

        observed_daily = [f if f > 0 else fmin for f in self.gageflows]
        observed_daily.sort()

        # get the length and cdf

        L = len(observed_daily)
        obs_daily_cdf = [norm.ppf(i / L) for i in range(L)]
        obs_daily_cdf.reverse()

        # make tick marks (had to do this hack style for matplotlib)

        ticks = [0.001, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.98, 0.999]

        norm_ticks = [norm.ppf(t) for t in ticks]

        # daily flow duration curve

        s2.set_title('Flow Duration Curve', size = titlesize)
        s2.set_yscale('log')
        s2.set_ylabel('Daily Flow (ft\u00B3/s)', size = axsize)
        s2.set_xlabel('Probability of Exceedance', size = axsize)
        s2.set_xlim([norm.ppf(0.0002), norm.ppf(0.9998)])
        s2.xaxis.set_ticks(norm_ticks)
        s2.set_xticklabels(ticks)

        s2.plot(obs_daily_cdf, observed_daily,  '-', color = 'blue',
                label = 'observed daily')

        pyplot.tight_layout()
        pyplot.savefig(output)

    def make_timeseries(self, start = None, end = None):
        """returns a time series of daily flows."""

        if start is None: start = self.gagedates[0]
        if end is None: end = self.gagedates[-1]

        tstep = datetime.timedelta(days = 1)

        series = []

        t = start

        if start in self.gagedates:
            i = self.gagedates.index(start)

        else:
            
            print('warning, requested dates exceed available data' +
                  ', filling with Nones\n')
            while t < self.gagedates[0]:
                series.append(None)
                t += tstep

            i = 0

        if end <= self.gagedates[-1]:

            while t < end:

                series.append(self.gageflows[i])
                t += tstep
                i += 1

        else:

            print('warning, requested dates exceed available data' +
                  ', filling with Nones\n')

            while t < self.gagedates[-1]:

                series.append(self.gageflows[i])
                t += tstep
                i += 1

            while t < end:

                series.append(None)
                t += tstep

        return series
