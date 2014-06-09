#!/usr/bin/env python3
#
# File: make_gages.py
#
# by David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Last updated: 4/12/2013
#
# Purpose: imports gage files to Python pickled files for easy access to data
#

import os, csv, pickle, datetime, io

from urllib import request

class GageStation:
    """A class to store data from a USGS NWIS gage station."""

    def __init__(self, gageid, comid, state, gnis, day1, dayn, drain, ave, web):
        """Create a gage and provide the basic information about it."""

        self.gageid = gageid
        self.comid  = comid
        self.state  = state
        self.gnis   = gnis
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
                          'directly from NWIS\n')

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
                          'directly from NWIS\n')

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
