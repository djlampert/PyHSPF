#!/usr/bin/env python3
#
# File: ncdcstations.py
#
# by David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Last updated: 11/16/2013
#
# Purpose: classes to import climate data files to Python

import shutil, os, subprocess, pickle, io, gzip, datetime, tarfile

from urllib     import request
from calendar   import monthrange
from matplotlib import pyplot, dates, ticker

from .climateplots import plot_3240precip

def is_integer(n):
    """Tests if n is an integer."""

    try:
        int(n)
        return True
    except: return False

class GHCNDStation:
    """A class to store meteorology data from the Daily Global Historical 
    Climatology Network."""

    def __init__(self, station, name, lat, lon, elev, dtype = None):

        self.station = station
        self.name    = name
        self.lat     = lat
        self.lon     = lon
        self.elev    = elev
        self.dtype   = dtype
    
        # time series -- lists of datetime.datetime/value pairs

        # variable       list    pyhspf type/units           GHCND type/units

        self.precip    =  []   # precipitation (mm)          PRCP (mm/10)
        self.tmax      =  []   # daily max temperature (C)   TMAX (C/10)
        self.tmin      =  []   # daily min temperature (C)   TMIN (C/10)
        self.snowdepth =  []   # daily snowdepth (mm)        SNWD (mm)
        self.snowfall  =  []   # daily snowfall (mm)         SNOW (mm) 
        self.wind      =  []   # daily avg wind speed (m/s)  AWND (m/s/10)
        self.evap      =  []   # pan evaporation (mm)        EVAP (mm/10)

    def download_data(self, directory, types = 'all', start = None, end = None,
                      GHCND = 'ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily',
                      plot = True, verbose = True):
        """Downloads the data for the desired time period from the GHCND."""

        if not os.path.isdir(directory):

            print('\nerror: directory "{}" does not exist\n'.format(directory))
            raise

        destination = '{}/{}'.format(directory, self.station)

        if not os.path.isfile(destination):

            if verbose: print('attempting to download data for', self.station)

            # source url

            f = '{}/all/{}.dly'.format(GHCND, self.station)
            r = request.Request(f)

            try:
            
                with io.StringIO(request.urlopen(r).read().decode()) as s:

                    # parse it

                    data = [(r[:11], r[11:15], r[15:17], r[17:21], 
                             r[21:]) for r in s.read().split('\n')]

                for s, year, month, element, values in data:
                    self.add_monthly(year, month, element, values)

                with open(destination, 'wb') as f: pickle.dump(self, f)

            except PermissionError:
                print('\nWarning: destination path appears invalid\n')
                raise

            except:
                print('warning: unable to download data from ' +
                      '{}/all/{}.dly'.format(GHCND, self.station))
                
        if plot and not os.path.isfile(destination + '.png'):

            from pyhspf.preprocessing.climateplots import plot_ghcnd

            try: plot_ghcnd(self, start = start, end = end, 
                            output = destination)
            except: print('warning: unable to plot GHCND data')

    def add_monthly(self, year, month, element, data):
        """Adds the monthly data of type "element" to a timeseries."""

        # figure out which list to append

        if   element == 'PRCP': l, m = self.precip, 10
        elif element == 'TMAX': l, m = self.tmax, 10
        elif element == 'TMIN': l, m = self.tmin, 10
        elif element == 'SNWD': l, m = self.snowdepth, 1
        elif element == 'SNOW': l, m = self.snowfall, 1
        elif element == 'AWND': l, m = self.wind, 10
        elif element == 'EVAP': l, m = self.evap, 10
        else: return

        # split the data up by dates

        dates = [datetime.datetime(int(year), int(month), i) 
                 for i in range(1, monthrange(int(year), int(month))[1] + 1)]
        values = [int(data[i:i+5]) for i in range(0, 248, 8)]
        qflags = [data[i+6]        for i in range(0, 248, 8)]

        for d, v, q in zip(dates, values, qflags): 
            if   q != 'X': l.append((d, v / m))
            else:          l.append((d, -9999))

class GSODStation:
    """A class to store meteorology data from the Global Summary of the Day 
    Network."""

    def __init__(self, 
                 airforce, 
                 wban, 
                 name, 
                 latitude, 
                 longitude, 
                 elevation, 
                 start, 
                 end,
                 ):
        """info from the NCDC GSOD database."""

        self.airforce  = airforce   # Air Force Datsav3 Station Number
        self.wban      = wban       # Weather Bureau Air Force Navy Number
        self.name      = name       # Description
        self.latitude  = latitude
        self.longitude = longitude
        self.elevation = elevation
        self.start     = start
        self.end       = end

        # time series for the station -- lists of datetime.datetime/value pairs

        # variable       list    pyhspf type           units   GSOD units
        self.precip    =  []   # precipitation         (mm)    (in)
        self.tmax      =  []   # daily max temperature (C)     (F)
        self.tmin      =  []   # daily min temperature (C)     (F)
        self.wind      =  []   # daily avg wind speed  (m/s)   (knots)
        self.dewpoint  =  []   # daily avg dewpoint    (mm)    (F)

    def is_integer(self, s):
        """Tests if string "s" is an integer."""
        try: int(s) 
        except ValueError: return False
        return True

    def is_number(self, s):
        """Tests if string "s" is a number."""
        try: float(s) 
        except ValueError: return False
        return True

    def far_cel(self, T):
        """Converts "T" from Farenheit to Celsius."""

        return 5 / 9 * (T - 32)

    def download_data(self, directory, start = None, end = None, plot = True,
                      GSOD = 'ftp://ftp.ncdc.noaa.gov/pub/data/gsod',
                      verbose = False):
        """Dowloads the GSOD data and saves this instance to the directory 
        provided and (optionally) plots it."""

        # figure out what files are available and needed

        if self.start is None or self.end is None:    
            years = []
            return
        else:
            if self.start is not None: # figure out the start date
                if start is not None:
                    start = max(start, self.start)
                else:
                    start = self.start
            if self.end is not None:
                if end is not None:
                    end = min(end, self.end)
                else:            
                    end = self.end
            years = [start.year + i for i in range(end.year - start.year)]

        # see if the data file is there, if not download it

        var = directory, self.airforce
        destination = '{0}/{1:06d}'.format(*var)

        if not os.path.isfile(destination):
            var = self.name, start.year, end.year
            print('attempting to download data from ' +
                  '{} for {} to {}'.format(*var))
            
            # data are stored by year and then station

            for year in years:

                var = GSOD, year, self.airforce, self.wban
                url = '{0}/{1:04d}/{2:06d}-{3:05d}-{1:04d}.op.gz'.format(*var)
                if verbose: print('attempting to download data from', url)
            
                try:
                    r = request.Request(url)

                    # read the binary data

                    with io.BytesIO(request.urlopen(r).read()) as b:

                        # create a gGzipFile to work with the binary string
               
                        with gzip.GzipFile(fileobj = b, mode = 'rb') as zf:

                            # read and decode the binary

                            lines = zf.read().decode().split('\n')

                    if len(lines) > 0:
                        
                        # parse the lines in the file and pull out the 
                        # relevant data (date, pressure, max temp, min temp,
                        # dewpoint, wind speed)

                        for l in lines[1:]:

                            try: 
                                (s, w, d, T, h, D, h, slp, h, stp, h, v, h, W, 
                                 h, m, g, Tmax, Tmin, P, s, f) = l.split()
                                self.add_daily(d, P, Tmax, Tmin, D, W)
                            except: pass

                except:

                    print('unable to download data from', url)
                    print('listed record:', self.start, self.end)
        
            self.precip.sort()
            self.tmax.sort()
            self.tmin.sort()
            self.wind.sort()
            self.dewpoint.sort()

            with open(destination, 'wb') as f: pickle.dump(self, f)
                    
    def add_daily(self, d, prec, tmax, tmin, dewt, wind):
        """Adds daily observations of precipitation, max temperature, min
        temperature, dew point, and avg wind speed."""

        if self.is_integer(d):
            date = datetime.datetime(int(d[:4]), int(d[4:6]), int(d[6:8]))
        else: return

        if self.is_number(prec[:-1]):
            if 0 < float(prec[:-1]) and float(prec[:-1]) < 99:
                self.precip.append((date, float(prec[:-1]) * 25.4))
        else: print(prec[:-1])

        if self.is_number(tmax[:4]):
            if float(tmax[:4]) < 9999:
                self.tmax.append((date, self.far_cel(float(tmax[:4]))))

        if self.is_number(tmin[:4]):
            if float(tmin[:4]) < 9999:
                self.tmin.append((date, self.far_cel(float(tmin[:4]))))

        if self.is_number(dewt[:4]):
            if float(dewt[:4]) < 9999:
                self.dewpoint.append((date, self.far_cel(float(dewt[:4]))))

        if self.is_number(wind):
            if float(wind) < 99:
                self.wind.append((date, float(wind) * 0.5144))

    def make_timeseries(self, tstype, start = None, end = None, verbose = True):
        """Constructs time series of type "tstype" between times start and end 
        (start and end are instances of datetime.datetime)."""

        if   tstype == 'dewpoint':      timeseries = self.dewpoint
        elif tstype == 'tmax':          timeseries = self.tmax
        elif tstype == 'tmin':          timeseries = self.tmin
        elif tstype == 'wind':          timeseries = self.wind
        elif tstype == 'precipitation': timeseries = self.precip
        else:

            print('error: unknown time series type specified')
            print('options are dewpoint, tmax, tmin, wind, or precipitation\n')
            raise

        if len(timeseries) == 0: 
            print('warning: station contains no point data')
            return
        
        if start is None: start = timeseries[0][0]
        if end is None: end = timeseries[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('start must be less than end')
            return None

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            return None

        if start < timeseries[0][0] and timeseries[-1][0] < end:
            if verbose:
                print('warning: specified range (%s to %s) is outside of ' % 
                      (start, end) + 'available dewpoint data')

        data = []

        dates = [start + i * datetime.timedelta(days = 1) 
                 for i in range(int((end - start).total_seconds() / 86400))]

        ts_dates, values = zip(*timeseries)
       
        for date in dates:
            if date in ts_dates:
                data.append(values[ts_dates.index(date)])
            else: 
                data.append(None)

        if len(data) == 0:
            print('warning: unable to generate time series')
            return None

        return data

    def plot(self, start = None, end = None, output = None, show = False):
        """make a plot."""

        from .climateplots import plot_gsod

        if 1==1:
#        try: 
            plot_gsod(self, start = start, end = end, output = output,
                      show = show)
#        except: print('warning: unable to plot GSOD data')

class Precip3240Station:
    """A class to store meteorology data from the NCDC Hourly Precipitation
    Dataset."""

    def __init__(self, coop, wban, desc, lat, lon, elev, st, code,
                 NCDC = 'ftp://ftp.ncdc.noaa.gov/pub/data',
                 ):

        self.coop        = coop
        self.wban        = wban
        self.desc        = desc
        self.latitude    = lat
        self.longitude   = lon
        self.elevation   = elev
        self.state       = st
        self.code        = code
        
        # ftp location for the NCDC data

        self.NCDC = NCDC

        # precipitation events

        self.events = []

    def is_integer(self, s):
        """Tests if string "s" is an integer."""

        try: int(s) 
        except ValueError: return False
        return True

    def import_tar(self, f, verbose = True):
        """Imports the hourly precipitation events in the tarfile "f." """
        
        with tarfile.open(f, mode= 'r') as tar:

            # find all the files in the archive for the station

            members = [m for m in tar.getmembers()
                       if m.name[5:11] == '{}'.format(self.coop)]

            # import the data for each file

            for member in members:

                if verbose: 
                    print('importing precipitation data from ' +
                          '{}'.format(member.name))

                # read the data into memory and split it up by lines

                b = tar.extractfile(member).read()
                data = [(l[17:21], l[21:23], l[23:27], l[30:])
                        for l in b.decode().split('\n')
                        if all([self.is_integer(v) for v in 
                                [l[17:21], l[21:23], l[23:27]]])]

                # read all the hourly events into a list

                events = []

                # iterate through the dates in the file and get the
                # hourly observations (year, month, day, hourly data)

                for y, m, d, h in data: 

                    # put the data into more useful form

                    try:
                        date = datetime.datetime(int(y), int(m), int(d))

                        for i in range(0, len(h), 12):

                            if   h[i:i+2] == '25' and h[i+10] == 'I':
                                t = date + datetime.timedelta(hours = 25)
                                v = int(h[i+5:i+10]) / 100
                                events.append([t, v, 'I', h[i+11]])

                            elif h[i:i+2] != '25' and self.is_integer(h[i:i+2]):
                                hour = int(h[i:i+2]) 
                                t = date + datetime.timedelta(hours = hour)
                                if self.is_integer(h[i+5:i+10]):
                                    v = int(h[i+5:i+10]) / 100
                                    events.append([t, v, h[i+10], h[i+11]])

                            elif i == 0: # only daily total available

                                if self.is_integer(h[i+5:i+10]):
                                    v = int(h[i+5:i+10]) / 100
                                    events.append([date, v, h[i+10], h[i+11]])

                    except: print('import error', y, m, d)

                # append the event list

                self.events += events

    def import_data(self, directory, start, end, verbose = True):
        """Downloads the data for the desired time period from the NCDC."""

        if not os.path.isdir(directory):
            print('\nerror: directory "{}" does not exist\n'.format(directory))
            raise

        if verbose: 
            print('attempting to import data for {}\n'.format(self.coop))

        # find and sort all the tarfiles in the directory

        tars = ['{}/{}'.format(directory, f) for f in os.listdir(directory) 
                if f[-3:] == 'tar' and f[5:7] == '{}'.format(self.code)]
        tars.sort()

        # parse the tars and read in the data

        for f in tars:

            year1, year2 = int(f[-13:-9]), int(f[-8:-4])

            if year1 <= end.year and start.year <= year2:

                # import the archive

                try: 
                    self.import_tar(f)

                except: 
                    print('failed to import data from {}'.format(f))
                    raise

        if verbose: print('')

        # sort all the event data

        self.events.sort()

        # add a zero event to the beginning and end for timeseries handling

        if len(self.events) > 0:
            if start < self.events[0][0]: 
                self.events.insert(0, [start, None, ' ', ' ']) 

            if self.events[-1][0] < end: 
                self.events.append([end, None, ' ', ' ']) 

    def download_state_precip3240(self,
                                  directory,
                                  verbose = True):
        """Downloads the Precip 3240 data for a state."""

        if not os.path.isdir(directory):
            print('\nerror: directory "{}" does not exist\n'.format(directory))
            raise

        if verbose:
 
            print('downloading hourly precipitation data for state ' + 
                  '{}\n'.format(self.code))

        # figure out which files are on the website

        baseurl = '{0}/hourly_precip-3240/{1}'.format(self.NCDC, self.code)

        req = request.Request(baseurl)

        # read the state's web page and find all the compressed archives

        try:

            with io.StringIO(request.urlopen(req).read().decode()) as s:

                archives = [a[-17:] for a in s.read().split('.tar.Z')]
            
            archives = [a for a in archives if is_integer(a[-4:])]

        except: 

            print('unable to connect to the hourly precipitation database')
            print('make sure that you are online')
            raise

        for a in archives:

            url        = '{0}/{1}.tar.Z'.format(baseurl, a)
            compressed = '{}/{}.tar.Z'.format(directory, a)

            if not os.path.isfile(compressed):

                if verbose: print(url)

                try: 

                    req = request.Request(url)

                    # write the compressed archive into the directory

                    with open(compressed, 'wb') as f: 
                    
                        f.write(request.urlopen(req).read())

                except:

                    print('error: unable to connect to {}'.format(url))
                    raise

    def decompress7z(self, filename, directory,
                     path_to_7z = r'C:/Program Files/7-Zip/7z.exe'):
        """Decompresses a Unix-compressed archive on Windows using 7zip."""
        
        c = '{0} e {1} -y -o{2}'.format(path_to_7z, filename, directory)

        subprocess.call(c)

    def decompresszcat(self, filename, directory):
        """Decompresses a Unix-compressed archive on Windows using 7zip."""

        with subprocess.Popen(['zcat', filename], 
                              stdout = subprocess.PIPE).stdout as s:

            with open(filename[:-2], 'wb') as f: f.write(s.read())

    def decompress(self, filepath, directory):
        """Calls 7zip to decompress the files."""

        if os.name == 'nt':
            self.decompress7z(filepath, directory, path_to_7z = self.path_to_7z)
        else:
            self.decompresszcat(filepath, directory)

    def check_filenames(self, directory, start, end):
        """returns the names of the downloaded files for the state.
        files are grouped by state and with all years prior to 1999 in one 
        file and each year up to 2011 in individual files; after that the 
        data are grouped by month (so data from 2012 - present cannot be 
        downloaded)
        """

        # make a list of the end year for all the files needed

        years = []

        # check if pre-1999 data area needed

        if start < datetime.datetime(1999, 1, 1): years.append(1998)

        # check if other year data are needed

        for year in range(1999, 2012):

            if (start <= datetime.datetime(year, 1, 1) and
                datetime.datetime(year, 1, 1) < end):
                
                years.append(year)

        # make a list of the last 8 characters in the file string to check

        needed = ['{:4d}.tar'.format(y) for y in years]

        # make a list of the archives in the directory

        existing = [f[-8:] for f in os.listdir(directory) if f[-3:] == 'tar']

        # check if any of the needed files do not exist

        return any([f not in existing for f in needed])
                        
    def download_data(self, directory, start, end, clean = False, plot = True,
                      path_to_7z = r'C:/Program Files/7-Zip/7z.exe',
                      ):
        """Downloads and imports all the data for the station."""

        # path to 7zip executable (only matters on Windows)

        self.path_to_7z = path_to_7z

        if not os.path.isdir(directory):
            print('\nerror: directory "{}" does not exist\n'.format(directory))
            raise

        precip3240 = '{}/precip3240'.format(directory)
        if not os.path.isdir(precip3240):
            os.mkdir(precip3240)
        elif clean:
            print('\nerror, working directory exists, cannot clean\n')
            raise

        # figure out if the files are already available

        needed = self.check_filenames(precip3240, start, end)

        # if the files aren't available, then download and decompress them

        if needed:

            self.download_state_precip3240(precip3240)

            # decompress the archives

            for a in os.listdir(precip3240):
                if a[-6:] == '.tar.Z': 
                    p = '{}/{}'.format(precip3240, a)

                    self.decompress(p, precip3240)

        # import the decompressed data

        self.import_data(precip3240, start, end)

        # save the results to this destination

        destination = '{}/{}'.format(directory, self.coop)
        if plot and not os.path.isfile(destination + '.png'):

            #try: 
            if 1==1:    
                self.plot(start = start, end = end, output = destination)
            #except: print('warning: unable to plot precipitation data')

        # save the import

        with open(destination, 'wb') as f: pickle.dump(self, f)

        # remove the intermediate files if desired

        if clean: shutil.rmtree('{}/precip3240'.format(directory))

    def pct_missing(self, start = None, end = None):
        """Determines the percentage of missing data across the specified
        period."""

        precip = self.make_timeseries(start = start, end = end, 
                                      tstep = 'hourly')

        return len([p[1] for p in precip if p[1] is None]) / len(precip)

    def total_precipitation(self, start = None, end = None):
        """Determines the total precipitation across the time period."""

        precip = self.make_timeseries(start = start, end = end, 
                                      tstep = 'hourly')

        return sum([p for t,p in precip if p is not None])

    def make_timeseries(self, start = None, end = None, tstep = 'hourly'):
        """Makes a timeseries from the events."""

        if len(self.events) == 0: 
            print('warning: station has no data')
            return

        if start is None: start = self.events[0][0]
        if end   is None: end   = self.events[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('start date must be less than end date')
            return

        if start < self.events[0][0] or self.events[-1][0] < end:
            print('specified range (%s to %s) is outside of ' % 
                  (start, end) + 'available gage data\n')

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            return

        # keep the timeseries in a list

        series = []

        # find the first event after start

        i = 0
        while self.events[i][0] <= start: i+=1

        # fill in Nones until reaching the first event

        t = start
        while t < self.events[i][0]:
            series.append((t, None))            
            t += datetime.timedelta(hours = 1)
        
        # iterate through to the end

        while t < end and t < self.events[-1][0]:

            # append zeros until reaching an event

            while t < self.events[i][0] and t < end:
                series.append((t, 0.))            
                t += datetime.timedelta(hours = 1)

            # collect as normal

            if ((self.events[i][2] == ' ' or self.events[i][2] == 'E') and
                t < end):
                series.append((t, self.events[i][1]))
                t += datetime.timedelta(hours = 1)

            # have to distribute volume

            elif self.events[i][2] == 'a':

                # find the end

                j = i
                while self.events[j][2] != 'A': j+=1

                hours = ((self.events[j][0] - 
                          self.events[i][0]).total_seconds() / 3600)

                # work around--sometimes missing data are listed as accumulation

                if self.events[j][1] < 999:
                    hourly = self.events[j][1] / hours
                else: 
                    hourly = None

                i = j
                while t < self.events[i][0] and t < end:
                    series.append((t, hourly))
                    t += datetime.timedelta(hours = 1)

            # missing period or record

            elif self.events[i][2] == '{' or self.events[i][2] == '[':

                while self.events[i][2] != '}' and self.events[i][2] != ']':
                    i += 1

                while t < self.events[i][0] and t < end:
                    series.append((t, None))
                    t += datetime.timedelta(hours = 1)

            # incomplete period of record

            elif self.events[i][2] == 'I':

                while t < self.events[i+1][0] and t < end:
                    series.append((t, None))
                    t += datetime.timedelta(hours = 1)
                
            elif self.events[i][1] > 999: 
                
                series.append((t, None))
                t += datetime.timedelta(hours = 1)

            elif t < end:
                series.append((t, 0))
                t += datetime.timedelta(hours = 1)

            i += 1

        while t < end:
            series.append((t, 0.))            
            t += datetime.timedelta(hours = 1)

        if   tstep == 'hourly': return series
        elif tstep == 'daily':  
            series = [sum([s for s in series[i:i+24] if s is not None]) 
                      for i in range(0, len(series), 24)]
            return series
        else: 
            print('warning, unknown time step specified')
            return

    def plot(self, start = None, end = None, tstep = 'daily', 
             show = False, output = None, verbose = True):
        """Generates a series of subplots of the time series of precipitation
        data for a watershed."""

        if verbose: print('plotting the precipitation data\n')

        if len(self.events) == 0:
            print('warning: station contains no data')
            return

        if start is None: start = self.events[0][0]
        if end is None:   end   = self.events[-1][0]

        # make the figure and subplots

        fig = pyplot.figure()
        sub = fig.add_subplot(111)

        v = self.coop, self.desc
        f = sub.set_title('Coop Station {}: {} Precipitation Data'.format(*v), 
                          size = 14)

        pct = self.pct_missing(start = start, end = end)
        tot = self.total_precipitation(start = start, end = end)

        avg = tot / (end - start).days * 365.25

        ts = self.make_timeseries(start = start, end = end, 
                                  tstep = 'hourly')

        times  = [p[0] for p in ts]
        precip = [p[1] for p in ts]

        if tstep == 'hourly':
            nones  = [-1 if p[1] is None else 0 for p in ts]
            precip = [p[1] if p[1] is not None else 0 for p in ts]
            l = 'hr'
                
        elif tstep == 'daily':
            nones  = [-1 if all([p is None for p in precip[i:i+24]]) else 0 
                       for i in range(0, len(precip), 24)]
            precip = [p[1] if p[1] is not None else 0 for p in ts]
            precip = [sum([p for p in precip[i:i+24]]) 
                      for i in range(0, len(precip), 24)]
            times = [start + i * datetime.timedelta(hours = 24) 
                     for i in range(len(precip))]
            l = 'd'

        else:
            print('error: unknown timestep specified')
            raise

        t = ('Average Precipitation = ' +
             '{:.1f} in\n{:.1%} Missing Data'.format(avg, pct))

        sub.fill_between(times, 0, precip, color = 'blue', alpha = 0.5)
        sub.fill_between(times, 0, nones, color = 'red', alpha = 0.5)

        sub.set_ylim([-1, sub.get_ylim()[1]])
        sub.set_ylabel('Prec (in)')
        sub.xaxis.set_major_locator(dates.YearLocator(3))
        sub.yaxis.set_major_locator(ticker.MaxNLocator(4))
        sub.text(0.98, 0.95, t, ha = 'right', va = 'top', size = 10, 
                 transform = sub.transAxes)

        #fig.autofmt_xdate()
        sub.set_xlabel('Date')
        for tick in sub.xaxis.get_major_ticks():
            tick.label.set_fontsize(10)

        if output is not None: pyplot.savefig(output)
        if show: pyplot.show()

        pyplot.clf()
        pyplot.close()

class NSRDBStation:
    """A class to store and retrieve data from the National Solar
    Radiation Database."""

    def __init__(self, usaf, wban, cl, mflag, station, lat, lon, elev):

        self.usaf      = usaf    # United States Air Force number
        self.wban      = wban    # Weather Bureau Army Navy number
        self.sclass    = cl      # class of data
        self.mflag     = mflag   # measured (1) or modeled (0)
        self.station   = station # station name
        self.latitude  = lat     # latitude
        self.longitude = lon     # longitude
        self.elevation = elev    # elevation (m)

        # data

        self.suny     = []  # SUNY model (W/m2)
        self.metstat  = []  # METSTAT model (W/m2)
        self.observed = []  # Measured (W/m2)
        self.legacy   = []  # Legacy data (may not exist)

    def is_integer(self, s):
        """Tests if string "s" is an integer."""
        try: int(s) 
        except ValueError: return False
        return True

    def download_data(self, destination, dates = None,
                      NSRDB = 'http://rredc.nrel.gov/solar/old_data/nsrdb'):
        """Downloads the data and pickles it to the destination directory."""

        # figure out if the old and new data sources are needed

        if dates is None: old, new = False, True
        else:
            old = dates[0].year <  1991
            new = dates[1].year >= 1991

        # temporary file to download zipped files before they are extracted

        temp = '{}/temp'.format(destination)

        if new:

            print('getting the new data for {}'.format(self.station))

            # path to the source data on the NSRDB

            url = '{}/1991-2010/targzs/{}.tar.gz'.format(NSRDB, self.usaf)

            # get the data and download it to a temporary file 

            req = request.Request(url)

            content = request.urlopen(req).read()

            with open(temp, 'wb') as f: f.write(content)

            # extract the archive and read in the data

            with tarfile.open(temp, 'r:gz') as tar:
                members = tar.getmembers()

                for m in members:
                    if m.isfile(): year = int(m.name[14:18])
                    else:          year = 0

                    if dates is None: collect = True
                    else:
                        collect = (dates[0].year <= year and 
                                   year <= dates[1].year)

                    # if the year is in the desired period, collect it

                    if collect:

                        data = [l.split(',') for l in
                                tar.extractfile(m).read().decode().split('\n')
                                if len(l.split(',')) == 43]

                        suny = [int(row[6])  for row in data[1:]]
                        mets = [int(row[15]) for row in data[1:]]
                        meas = [int(row[27]) for row in data[1:]]
                    
                        times = [datetime.datetime(year, 1, 1) + 
                                 datetime.timedelta(hours = 1) * i 
                                 for i in range(len(suny))]

                        if any([s > 0 for s in suny]): 
                            self.suny += [(t,s) for t,s in zip(times, suny)]

                        if any([s > 0 for s in mets]): 
                            self.metstat += [(t,s) for t,s in zip(times, mets)]

                        if any([s > 0 for s in meas]): 
                            self.observed += [(t,s) for t,s in zip(times, meas)]

        if old and self.wban is not None:

            print('getting the legacy data for {}'.format(self.station))

            # path to the source data on the NSRDB

            v = NSRDB, self.wban
            url = '{}/1961-1990/hourly/compressed/{}.tar.gz'.format(*v)

            # get the data and download it to a temporary file 

            req = request.Request(url)

            temp = '{}/temp'.format(destination)

            content = request.urlopen(req).read()

            with open(temp, 'wb') as f: f.write(content)

            # extract the archive and read in the data

            with tarfile.open(temp, 'r:gz') as tar:
                members = tar.getmembers()

                for m in members:
                    if m.isfile(): year = int('19{}'.format(m.name[6:8]))
                    else:          year = 0

                    if dates is None: collect = True
                    else:
                        collect = (dates[0].year <= year and 
                                   year <= dates[1].year)

                    # if the year is in the desired period, collect it

                    if collect:

                        # values are total, direct normal, diffuse horizontal

                        data = [[l[23:27], l[31:35], l[39:43]] for l in
                                tar.extractfile(m).read().decode().split('\n')]

                        # just take total

                        legacy = [int(row[0]) for row in data[1:]
                                  if self.is_integer(row[0])]

                        times = [datetime.datetime(year, 1, 1) + 
                                 datetime.timedelta(hours = 1) * i 
                                 for i in range(len(legacy))]

                        self.legacy += [(t,s) for t,s in zip(times, legacy)]

        # clean up the temporary file

        if os.path.isfile(temp): os.remove(temp)

        # dump the info to the destination

        with open('{}/{}'.format(destination, self.usaf), 'wb') as f: 
            pickle.dump(self, f)

class PrecipStation:
    """A class to store data from an NCDC hourly precipitation gage station."""

    def add_precip3240(self, station):
        """Get some basic info about the station."""

        self.station   = station.coop
        self.name      = station.desc
        self.elevation = station.elevation
        self.latitude  = station.latitude
        self.longitude = station.longitude

        precip = station.make_timeseries(tstep = 'hourly')

        start = station.events[0][0]
        end   = station.events[-1][0]

        times = [start + i * datetime.timedelta(hours = 1) 
                 for i in range((end - start).days * 24)]

        self.precip = [v for v in zip(times, precip)]
        
    def add_location(self, station):
        """Get some basic info about the station."""

        self.station = station.coop
        self.name    = station.desc

        self.elevation = station.elevation
        self.latitude  = station.latitude
        self.longitude = station.longitude

        self.precip = []

    def add_timeseries(self, data):
        """Add a timeseries (including missing data) to the station."""

        self.precip += data
        
    def pct_missing(self, start = None, end = None):
        """Determines the percentage of missing data across the specified
        period."""

        if start is None: start = self.precip[0][0]
        if end is None:   end = self.precip[-1][0]

        precip = [(t, p) for t, p in self.precip if start <= t and t <= end]

        return len([p for t, p in precip if p is None]) / len(precip)

    def total_precipitation(self, start = None, end = None):
        """Determines the total precipitation across the time period."""

        if start is None: start = self.precip[0][0]
        if end is None:   end = self.precip[-1][0]

        precip = self.make_timeseries(start = start, end = end, 
                                      tstep = 'hourly')

        if precip is not None:
            return sum([p for p in precip if p is not None])
        else: 
            return None

    def make_timeseries(self, start = None, end = None, tstep = 'hourly'):
        """Constructs an hourly time series between times t1 and t2 
        (t1 and t2 are instances of datetime.datetime)."""

        if start is None: start = self.precip[0][0]
        if end is None:   end = self.precip[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('t1 must be less than t2')
            return

        if (not isinstance(start, datetime.datetime) or 
              not isinstance(end, datetime.datetime)):
            print('t1 and t2 must be datetime.datetime instances')
            return

        ts = [p for t, p in self.precip if start <= t and t <= end]

        if start < self.precip[0][0] or self.precip[-1][0] < end:
            print('warning: specified range ({} to {}) is '.format(start, end) +
                  'outside of available gage data ' +
                  '({} to {})\n'.format(self.precip[0][0], self.precip[-1][0]))
        
            t = start
            while t < self.precip[0][0]: 
                ts.insert(0, None)
                t += datetime.timedelta(hours = 1)
            t = self.precip[-1][0]
            while t < end: 
                ts.append(None)
                t += datetime.timedelta(hours = 1)

        return ts

class TempStation:
    """A class to store data from an NCDC temperature station."""

    def add_location(self, station):
        """Copy some basic info about the station."""

        self.station = station.station
        self.name    = station.name

        try:    self.elevation = float(station.elev)
        except: self.elevation = -1

        try:    self.latitude  = float(station.lat)
        except: self.latitude = -1

        try:    self.longitude = float(station.lon)
        except: self.longitude = -1

    def add_tmin(self, tmin): self.tmin = tmin

    def add_tmax(self, tmax): self.tmax = tmax

    def make_timeseries(self, start = None, end = None, verbose = True):
        """Constructs daily tmax and tmin time series between times t1 and t2 
        (t1 and t2 are instances of datetime.datetime)."""

        if start is None: start = self.tmax[0][0]
        if end is None: end = self.tmax[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('start must be less than end')
            return None

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            return None

        if start < self.tmax[0][0] and self.tmax[-1][0] < end:
            print('warning: specified range (%s to %s) is outside of ' % 
                  (start, end) + 'available gage data')

        tmax = []
        tmin = []

        dates = [start + i * datetime.timedelta(days = 1) 
                 for i in range((end - start).days)]

        tmax_dates, tmax_values = zip(*self.tmax)
        tmin_dates, tmin_values = zip(*self.tmin)
        
        for date in dates:
            if date in tmax_dates:
                if tmax_values[tmax_dates.index(date)] < 100:
                    tmax.append(tmax_values[tmax_dates.index(date)])
                else: tmax.append(None)
            else: tmax.append(None)

            if date in tmin_dates:
                if tmin_values[tmin_dates.index(date)] < 100:
                    tmin.append(tmin_values[tmin_dates.index(date)])
                else: tmin.append(None)
            else: tmin.append(None)

        return tmax, tmin        

class SnowStation:
    """A class to store data from an NCDC snowdepth station."""

    def add_location(self, station):
        """Get some basic info about the station."""

        self.station = station.station
        self.name    = station.name

        try:    self.elevation = float(station.elev)
        except: self.elevation = -1

        try:    self.latitude  = float(station.lat)
        except: self.latitude = -1

        try:    self.longitude = float(station.lon)
        except: self.longitude = -1

    def add_snowdepth(self, snowdepth): self.snowdepth = snowdepth

    def add_snowfall(self, snowfall): self.snowfall = snowfall

    def make_timeseries(self, t1 = None, t2 = None, tstype = 'depth', 
                        verbose = True):
        """Constructs daily tmax and tmin time series between times t1 and t2 
        (t1 and t2 are instances of datetime.datetime)."""

        if   tstype == 'depth': ts = self.snowdepth
        elif tstype == 'fall':  ts = self.snowfall

        if t1 is None: t1 = ts[0][0]
        if t2 is None: t2 = ts[-1][0]

        # make sure the function inputs are correct
        
        if t1 >= t2:
            print('t1 must be less than t2')
            return None

        if (not isinstance(t1, datetime.datetime) or 
            not isinstance(t2, datetime.datetime)):
            print('t1 and t2 must be datetime.datetime instances')
            return None

        if t1 < ts[0][0] and ts[-1][0] < t2:
            if verbose: 
                print('warning: specified range ' +
                      '(%s to %s) is outside of '.format(t1, t2) + 
                      'available snowdepth data')

        snow = []

        dates = [t1 + i * datetime.timedelta(days = 1) 
                 for i in range(int((t2 - t1).total_seconds() / 86400))]

        snow_dates, values = zip(*ts)
       
        for date in dates:
            if date in snow_dates:
                snow.append(values[snow_dates.index(date)])
            else: snow.append(None)

        return snow

    def get_depth(self, t):
        """Returns the depth at a given time (interpolates if needed)."""

        if not isinstance(t, datetime.datetime):
            print('time must be datetime.datetime instance')
            return None

        if self.snowdepth[0][0] <= t and t <= self.snowdepth[-1][0]:

            times = [d[0] for d in zip(*self.snowdepth)]
            if t not in times:
                i = 0
                while self.snowdepth[i][0] < t: i+=1
                depth = (self.snowdepth[i-1][1] + self.snowdepth[i][1]) / 2.
            else: depth = self.snowdepth[times.index(t)][1]

            return depth

        else: return None

class EvapStation:
    """A class to store data from an NCDC pan evaporation station."""

    def add_ghcnd_data(self, station):
        """Get some basic info about the station."""

        self.station = station.station
        self.name    = station.name

        try:    self.elevation = float(station.elevation)
        except: self.elevation = -1

        try:    self.latitude  = float(station.latitude)
        except: self.latitude = -1

        try:    self.longitude = float(station.longitude)
        except: self.longitude = -1

        self.events = [(d, e) for d, e in station.evap if e >= 0]

    def add_location(self, station):
        """Get some basic info about the station."""

        self.station = station.station
        self.name    = station.name

        try:    self.elevation = float(station.elevation)
        except: self.elevation = -1

        try:    self.latitude  = float(station.latitude)
        except: self.latitude = -1

        try:    self.longitude = float(station.longitude)
        except: self.longitude = -1

        self.events = []
    
    def add_data(self, data): self.events = [(d, e) for d, e in data
                                             if e >= 0]

    def get_evaporation(self, t1 = None, t2 = None):
        """Returns the total evaporation at the station between times t1 and t2
        (datetime.datetime instances).
        """

        if t1 is None: t1 = self.events[0][0]
        if t2 is None: t2 = self.events[-1][0]
        
        # make sure the function inputs are correct
        
        if t1 >= t2:
            print('t1 must be less than t2')
            return

        if (not isinstance(t1, datetime.datetime) or 
              not isinstance(t2, datetime.datetime)):
            print('t1 and t2 must be datetime.datetime instances')
            return

        total = 0

        # find the first event after t1

        i = 0
        while self.events[i][0] < t1: i+=1

        # add all the events 

        while self.events[i][0] < t2:
            total += self.events[i][1]
            i += 1

        return total

    def make_timeseries(self, t1, t2):
        """Constructs a daily time series between times t1 and t2 
        (t1 and t2 are instances of datetime.datetime)."""

        # make sure the function inputs are correct
        
        if t1 >= t2:
            print('t1 must be less than t2')
            return

        dates, values = zip(*self.events)

        series = []
        t = t1

        # go through time period and fill values as available; nones otherwise

        while t < self.events[0][0] and t < t2:  
            series.append(None)
            t += datetime.timedelta(days = 1)

        while t < t2:

            if t in dates:

                i = dates.index(t)
                series.append(values[i])

            else: series.append(None)

            t += datetime.timedelta(days = 1)

        return series

class WindStation:
    """A class to store data from an NCDC snowdepth station."""

    def add_location(self, station):
        """Get some basic info about the station."""

        try:                   self.name = station.name
        except AttributeError: self.name = ''

        try:    self.elevation = float(station.elev)
        except: self.elevation = -1

        try:    self.latitude  = float(station.lat)
        except: self.latitude = -1

        try:    self.longitude = float(station.lon)
        except: self.longitude = -1

        self.wind = []

    def add_data(self, data):
        """Adds data to existing (useful if located in different files)."""

        self.wind += data

    def make_timeseries(self, t1 = None, t2 = None, verbose = True):
        """Constructs daily avg wind time series between times t1 and t2 
        (t1 and t2 are instances of datetime.datetime)."""

        if t1 is None: t1 = self.wind[0][0]
        if t2 is None: t2 = self.wind[-1][0]

        # make sure the function inputs are correct
        
        if t1 >= t2:
            print('t1 must be less than t2')
            return None

        if (not isinstance(t1, datetime.datetime) or 
            not isinstance(t2, datetime.datetime)):
            print('t1 and t2 must be datetime.datetime instances')
            return None

        if t1 < self.wind[0][0] and self.wind[-1][0] < t2:
            if verbose:
                print('warning: specified range (%s to %s) is outside of ' % 
                      (t1, t2) + 'available wind data')

        wind = []

        dates = [t1 + i * datetime.timedelta(days = 1) 
                 for i in range(int((t2 - t1).total_seconds() / 86400))]

        wind_dates, values = zip(*self.wind)
       
        for date in dates:
            if date in wind_dates:
                wind.append(values[wind_dates.index(date)])
            else: wind.append(None)

        if len(wind) == 0:
            print('warning: unable to generate time series')
            return None
        return wind

class DewStation:
    """A class to store dewpoint data from a GSOD station."""

    def add_station(self, station):
        """Get some basic info about the station."""

        try:                   self.name = station.name
        except AttributeError: self.name = ''

        try:    self.elevation = float(station.elev)
        except: self.elevation = -1

        try:    self.latitude  = float(station.lat)
        except: self.latitude = -1

        try:    self.longitude = float(station.lon)
        except: self.longitude = -1

        self.dewpoint = station.dewpoint

    def make_timeseries(self, start = None, end = None, verbose = True):
        """Constructs daily dewpoint time series between times start and end 
        (start and end are instances of datetime.datetime)."""

        if len(self.dewpoint) == 0: 
            print('warning: station contains no point data')
            return
        
        if start is None: start = self.dewpoint[0][0]
        if end is None: end = self.dewpoint[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('start must be less than end')
            return None

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            return None

        if start < self.dewpoint[0][0] and self.dewpoint[-1][0] < end:
            if verbose:
                print('warning: specified range (%s to %s) is outside of ' % 
                      (start, end) + 'available dewpoint data')

        dewpoint = []

        dates = [start + i * datetime.timedelta(days = 1) 
                 for i in range(int((end - start).total_seconds() / 86400))]

        dew_dates, values = zip(*self.dewpoint)
       
        for date in dates:
            if date in dew_dates:
                dewpoint.append(values[dew_dates.index(date)])
            else: dewpoint.append(None)

        if len(dewpoint) == 0:
            print('warning: unable to generate time series')
            return None

        return dewpoint

class SolarStation:

    def __init__(self, station):
        """Creates a solar station based on the METSTAT data from the NSRDB."""

        self.station = station.usaf
        self.name    = station.station

        try:    self.elevation = float(station.elevation)
        except: self.elevation = -1

        try:    self.latitude  = float(station.latitude)
        except: self.latitude = -1

        try:    self.longitude = float(station.longitude)
        except: self.longitude = -1

        station.metstat.sort()
        station.legacy.sort()

        self.solar = [(d, v) for d, v in station.legacy + station.metstat
                      if v >= 0]

    def make_timeseries(self, start, end, tstep = 'hourly', function = 'sum',
                        verbose = False):
        """Returns a time series of values for the station."""

        # make sure the function inputs are correct
        
        if start >= end:
            print('start must be less than end')
            return None

        if start < self.solar[0][0] and self.solar[-1][0] < end:
            print('warning: specified range (%s to %s) is outside of ' % 
                  (start, end) + 'available gage data')

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            return None

        times = [start + i * datetime.timedelta(hours = 1)
                 for i in range((end - start).days * 24)]

        dataset = [(t, v) for t, v in self.solar if start <= t and t < end]
 
        if all([v is None for d, v in dataset]): return None
        else:                 hourly, values = zip(*dataset)

        if len(times) == len(hourly):
            
            # make an hourly time series
        
            if tstep == 'hourly': return values

            else:

                daily   = [start + i * datetime.timedelta(days = 1)
                           for i in range((end - start).days + 1)]

                if function == 'sum':
                    dvalues = [sum(values[i:i+24]) 
                               if all([v is not None for v in values[i:i+24]])
                               else None
                               for i in range(0, len(values), 24)]

                if function == 'average':
                    dvalues = [sum(values[i:i+24]) / 24 
                               if all([v is not None for v in values[i:i+24]])
                               else None
                               for i in range(0, len(values), 24)]

                if tstep == 'daily': return dvalues

                days  =  monthrange(daily[0].year, daily[0].month)[1]
                delta =  datetime.timedelta(days = days)
                monthly = [daily[0]]

                if function == 'sum':
                    mvalues = [sum(dvalues[:delta.days])]

                if function == 'average':
                    mvalues = [sum(dvalues[:delta.days])/ 
                               len(dvalues[:delta.days])]

                while monthly[-1] + delta < daily[-1]:
                    i = daily.index(monthly[-1])
                    j = daily.index(monthly[-1] + delta)

                    monthly.append(monthly[-1] + delta)

                    if function == 'sum':
                        mvalues.append(sum(dvalues[i:j]))

                    if function == 'average':
                        mvalues.append(sum(dvalues[i:j]) / len(dvalues[i:j]))

                    days  = monthrange(monthly[-1].year, monthly[-1].month)[1]
                    delta = datetime.timedelta(days = days)
         
                return mvalues

        else:
            print('warning: missing data, filling with Nones')

            delta = datetime.timedelta(hours = 1)

            # see if the first value if in the timeseries

            if times[0] < hourly[0]: dataset, i = [[times[0], None]], 0
            else:                    dataset, i = [[times[0], values[0]]], 1

            # iterate through the times and fill with Nones until 
            # reaching an observation

            t = dataset[-1][0]
            while t < times[-1]:
                while t < hourly[i]:
                    dataset.append([t, None])
                    t += delta
                dataset.append([t, values[i]])
                t += delta
                i += 1
            
            times, values = zip(*dataset)

            if tstep == 'hourly': return values

            else:
                daily   = [start + i * datetime.timedelta(days = 1)
                           for i in range((end - start).days + 1)]

                if function == 'sum':
                    dvalues = [sum(values[i:i+24]) 
                               if all([v is not None for v in values[i:i+24]])
                               else None
                               for i in range(0, len(values), 24)]

                if function == 'average':
                    dvalues = [sum(values[i:i+24]) / 24 
                               if all([v is not None for v in values[i:i+24]])
                               else None
                               for i in range(0, len(values), 24)]

                if tstep == 'daily': return dvalues

                days  =  monthrange(daily[0].year, daily[0].month)[1]
                delta =  datetime.timedelta(days = days)
                monthly = [daily[0]]

                if function == 'sum':
                    mvalues = [sum(dvalues[:delta.days])]

                if function == 'average':
                    mvalues = [sum(dvalues[:delta.days])/ 
                               len(dvalues[:delta.days])]

                while monthly[-1] + delta < daily[-1]:
                    i = daily.index(monthly[-1])
                    j = daily.index(monthly[-1] + delta)

                    monthly.append(monthly[-1] + delta)

                    if function == 'sum':
                        mvalues.append(sum(dvalues[i:j]))

                    if function == 'average':
                        mvalues.append(sum(dvalues[i:j]) / len(dvalues[i:j]))

                    days  = monthrange(monthly[-1].year, monthly[-1].month)[1]
                    delta = datetime.timedelta(days = days)
         
                return mvalues
