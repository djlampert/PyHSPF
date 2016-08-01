# ncdcstations.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 02/15/2015
#
# Purpose: classes to import climate data files to Python

import shutil, os, subprocess, pickle, io, gzip, datetime, tarfile

from urllib     import request
from calendar   import monthrange
from matplotlib import pyplot, dates, ticker

class ClimateMetadata:
    """
    A class to store metadata from the various climate databases for a
    given geographical area.
    """

    def __init__(self):
        """
        Create data structures to store the important information about each
        of the climate data types.
        """

        self.ghcndstations      = {}
        self.gsodstations       = {}
        self.precip3240stations = {}
        self.nsrdbstations      = {}

    def add_ghcndstation(self, 
                         filename, 
                         ghcndstation,
                         ):
        """Adds the metadata for the GHCNDStation class instance."""

        self.ghcndstations[filename] = {
            'name':      ghcndstation.name,
            'latitude':  ghcndstation.latitude,
            'longitude': ghcndstation.longitude,
            'elevation': ghcndstation.elevation,
            'precip':    len(ghcndstation.precip),
            'tmax':      len(ghcndstation.tmax),
            'tmin':      len(ghcndstation.tmin),
            'snowdepth': len(ghcndstation.snowdepth),
            'snowfall':  len(ghcndstation.snowfall),
            'wind':      len(ghcndstation.wind),
            'evap':      len(ghcndstation.evap),
            }

    def add_gsodstation(self,
                        filename,
                        gsodstation,
                        ):
        """Adds the metadata for the GSODStation class instance."""

        self.gsodstations[filename] = {
            'name':      gsodstation.name,
            'latitude':  gsodstation.latitude,
            'longitude': gsodstation.longitude,
            'elevation': gsodstation.elevation,
            'precip':    len(gsodstation.precip),
            'tmax':      len(gsodstation.tmax),
            'tmin':      len(gsodstation.tmin),
            'wind':      len(gsodstation.wind),
            'dewpoint':  len(gsodstation.dewpoint),
            }

    def add_precip3240station(self,
                              filename,
                              precip3240station,
                              ):
        """Adds the metadata for the Precip3240Station class instance."""

        self.precip3240stations[filename] = {
            'name':      precip3240station.desc,
            'latitude':  precip3240station.latitude,
            'longitude': precip3240station.longitude,
            'elevation': precip3240station.elevation,
            'precip':    len(precip3240station.events),
            }

    def add_nsrdbstation(self,
                         filename,
                         nsrdbstation,
                         ):
        """Adds the metadata for the NSRDBStation class instance."""

        self.nsrdbstations[filename] = {
            'name':      nsrdbstation.station,
            'latitude':  nsrdbstation.latitude,
            'longitude': nsrdbstation.longitude,
            'elevation': nsrdbstation.elevation,
            'suny':      len(nsrdbstation.suny),
            'metstat':   len(nsrdbstation.metstat),
            'observed':  len(nsrdbstation.observed),
            'legacy':    len(nsrdbstation.legacy),
            }

class GHCNDStation:
    """A class to store meteorology data from the Daily Global Historical 
    Climatology Network."""

    def __init__(self, 
                 station, 
                 name, 
                 lat, 
                 lon, 
                 elev, 
                 dtype = None,
                 ):

        self.station   = station
        self.name      = name
        self.latitude  = lat
        self.longitude = lon
        self.elevation = elev
        self.dtype     = dtype
    
        # time series -- lists of datetime.datetime/value pairs

        # variable       list    pyhspf type/units           GHCND type/units

        self.precip    =  []   # precipitation (mm)          PRCP (mm/10)
        self.tmax      =  []   # daily max temperature (C)   TMAX (C/10)
        self.tmin      =  []   # daily min temperature (C)   TMIN (C/10)
        self.snowdepth =  []   # daily snowdepth (mm)        SNWD (mm)
        self.snowfall  =  []   # daily snowfall (mm)         SNOW (mm) 
        self.wind      =  []   # daily avg wind speed (m/s)  AWND (m/s/10)
        self.evap      =  []   # pan evaporation (mm)        EVAP (mm/10)

    def download_data(self, 
                      directory, 
                      types = 'all', 
                      start = None, 
                      end = None,
                      GHCND = 'ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily',
                      plot = True, 
                      verbose = True,
                      ):
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

            try: 
                self.plot_ghcnd(start = start, end = end, output = destination)
            except: print('warning: unable to plot GHCND data')

    def add_monthly(self, 
                    year, 
                    month, 
                    element, 
                    data,
                    ):
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

    def get_series(self, 
                   tstype,
                   ):
        """Private method to return the pointer to the right time series."""

        if   tstype == 'precipitation': events = self.precip
        elif tstype == 'tmax':          events = self.tmax
        elif tstype == 'tmin':          events = self.tmin
        elif tstype == 'snowdepth':     events = self.snowdepth
        elif tstype == 'snowfall':      events = self.snowfall
        elif tstype == 'wind':          events = self.wind
        elif tstype == 'evaporation':   events = self.evap
        else:
            print('error: unknown time series type {}\n'.format(tstype))
            print('options are precipitation, tmax, tmin, snowdepth, snowfall')
            print('wind, or evaporation.\n')
            raise
            
        return events

    def get_total(self, 
                  tstype, 
                  start = None, 
                  end = None,
                  ):
        """
        Returns the total of variable "tstype" at the station between times
        start and end (datetime.datetime instances) if supplied; otherwise
        returns the whole period of record.
        """

        events = self.get_series(tstype)

        if start is None: start = events[0][0]
        if end is None: end = events[-1][0]
        
        # make sure the function inputs are correct
        
        if start >= end:
            print('start must be less than end')
            return

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            return

        total = 0

        # find the first event after start

        i = 0
        while events[i][0] < start: i+=1

        # add all the events 

        while events[i][0] < end:
            if events[i][1] > 0: total += events[i][1]
            i += 1

        return total

    def make_timeseries(self, 
                        tstype, 
                        start = None, 
                        end = None,
                        ):
        """Constructs a daily time series between times start and end 
        (start and end should be instances of datetime.datetime).
        """

        events = self.get_series(tstype)

        if len(events) == 0: return

        if start is None: start = events[0][0]
        if end is None: end = events[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('start must be less than end')
            return

        dates, values = zip(*events)

        series = []
        t = start

        # go through time period and fill values as available; nones otherwise

        while t < events[0][0] and t < end:  
            series.append(None)
            t += datetime.timedelta(days = 1)

        while t < end:

            if t in dates:

                # missing values are -999

                i = dates.index(t)
                if values[i] > -90: 
                    series.append(values[i])
                else:
                    series.append(None)

            else: series.append(None)

            t += datetime.timedelta(days = 1)

        return series

    def plot_ghcnd(self,
                   start = None, 
                   end = None, 
                   show = False, 
                   output = None,
                   verbose = True,
                   ):
        """
        Makes a plot of the data from a GHCND station.
        """

        # some error handling

        precipdata = [(t, p) for t, p in self.precip 
                      if p >= 0 and start <= t and t <= end]
        tmaxdata = [(t, T) for t, T in self.tmax
                    if -50 < T and start <= t and t <= end]
        tmindata = [(t, T) for t, T in self.tmin
                    if -50 < T and start <= t and t <= end ]
        winddata = [(t, w) for t, w in self.wind
                    if 0 <= w and start <= t and t <= end]
        snowdata = [(t, s) for t, s in self.snowdepth
                    if 0 <= s and start <= t and t <= end]
        evapdata = [(t, e) for t, e in self.evap
                    if 0 <= e and start <= t and t <= end]

        if (len(precipdata) == 0 and len(tmaxdata) == 0 and 
            len(tmindata) == 0   and len(winddata) == 0 and 
            len(snowdata) == 0   and len(evapdata) == 0): 
            return

        if len(precipdata) > 0: 
            times, precip = zip(*precipdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            precip = [0, None, None, 1]

        try:
            if start is None: start = self.precip[0][0]
            if end is None:   end   = self.precip[-1][0]
        except:
            print('warning: no data present')
            return

        if verbose: print('making a plot for {}'.format(self.name))

        # make the plot

        fig, subs = pyplot.subplots(5, 1, sharex = True, figsize = (8, 10))

        its = self.name, start, end
        fig.suptitle('{} Climate Data {:%m-%d-%Y} to {:%m-%d-%Y}'.format(*its), 
                     size = 14)

        i = 0

        subs[i].plot_date(times, precip, color = 'cyan', fmt = '-',
                          label = 'precipitation')
        subs[i].set_ylabel('Precipitation (mm)', color = 'cyan')

        i = 1

        if len(tmaxdata) > 0: 
            times, temp = zip(*tmaxdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            temp = [0, None, None, 1]

        subs[i].plot_date(times, temp, fmt = '-', color = 'red', lw = 0.5, 
                          label = 'max temperature')

        if len(tmindata) > 0: 
            times, temp = zip(*tmindata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            temp = [0, None, None, 1]

        subs[i].plot_date(times, temp, fmt = '-', color = 'blue', lw = 0.5, 
                          label = 'min temperature')
        subs[i].set_ylabel('Temperature (\u00B0C)', color = 'red')

        i = 2
    
        if len(winddata) > 0: 
            times, wind = zip(*winddata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            wind = [0, None, None, 1]

        subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                          label = 'wind')
        subs[i].set_ylabel('Wind Speed (m/s)', color = 'purple')

        i = 3
    
        if len(snowdata) > 0: 
            times, snow = zip(*snowdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            snow = [0, None, None, 1]
        subs[i].plot_date(times, snow, color = 'gray', lw = 0.5, fmt = '-', 
                          label = 'snowdepth')

        subs[i].set_ylabel('Snow Depth (mm)', color = 'gray')

        i = 4

        if len(evapdata) > 0:
            times, evap = zip(*evapdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            evap = [0, None, None, 1]
        subs[i].plot_date(times, evap, label = 'evaporation', color = 'green', 
                          fmt = '-')

        subs[i].set_ylabel('Pan Evaporation (mm)', color = 'green')

        subs[-1].set_xlabel('Date', size = 12)
        subs[0].xaxis.set_major_locator(ticker.MaxNLocator(10))

        if output is not None: pyplot.savefig(output)

        if show: pyplot.show()

        pyplot.clf()
        pyplot.close()

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

    def download_data(self, 
                      directory, 
                      start = None, 
                      end = None, 
                      plot = True,
                      GSOD = 'ftp://ftp.ncdc.noaa.gov/pub/data/gsod',
                      verbose = False,
                      ):
        """Dowloads the GSOD data and saves this instance to the directory 
        provided and (optionally) plots it."""

        # figure out what files are available and needed

        if self.start is None or self.end is None:
            years = []
            return

        else:

            if self.start is not None: 

                # figure out the start date

                if start is not None:
                    start = max(start, self.start)
                else:
                    start = self.start

            if self.end is not None:
                if end is not None:
                    end = min(end, self.end)
                else:            
                    end = self.end

            if start < end:
                years = [start.year + i for i in range(end.year - start.year)]
            else:
                years = []
                return

        # see if the data file is there, if not download it

        var = directory, self.airforce
        destination = '{0}/{1:06d}'.format(*var)

        if not os.path.isfile(destination):
            var = self.name, start.year, end.year
            print('attempting to download data for ' +
                  '{} from {} to {}'.format(*var))
            
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
                    
    def add_daily(self, 
                  d, 
                  prec, 
                  tmax, 
                  tmin, 
                  dewt, 
                  wind,
                  ):
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

    def make_timeseries(self, 
                        tstype, 
                        start = None, 
                        end = None, 
                        verbose = True,
                        ):
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
            raise

        if start < timeseries[0][0] and timeseries[-1][0] < end:
            if verbose:
                print('warning: specified range ' +
                      '({} to {}) is outside of '.format(start, end) + 
                      'available {} data'.format(tstype))

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

    def plot(self, 
             start = None, 
             end = None, 
             output = None, 
             show = False,
             verbose = True,
             ):
        """make a plot."""

        # set the dates if not provided

        try:
            if start is None: start = self.tmax[0][0]
            if end is None:   end   = self.tmax[-1][0]
        except:
            print('warning: no data present\n')
            return

        if verbose: print('plotting GSOD data for {}\n'.format(self.name))

        # some error handling

        precipdata = [(t, p) for t, p in self.precip 
                      if p >= 0 and start <= t and t <= end]
        tmaxdata = [(t, T) for t, T in self.tmax
                    if -50 < T and start <= t and t <= end]
        tmindata = [(t, T) for t, T in self.tmin
                    if -50 < T and start <= t and t <= end ]
        dewtdata = [(t, T) for t, T in self.dewpoint
                    if -50 < T and start <= t and t <= end ]
        winddata = [(t, w) for t, w in self.wind
                    if 0 <= w and start <= t and t <= end]

        # make the plot

        fig, subs = pyplot.subplots(3, 1, sharex = True, figsize = (8, 8))

        var = self.name, start, end
        fig.suptitle('{} Climate Data {:%m-%d-%Y} to {:%m-%d-%Y}'.format(*var), 
                     size = 14)

        i = 0

        if len(precipdata) > 0: 
            times, precip = zip(*precipdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            precip = [0, None, None, 1]

        subs[i].plot_date(times, precip, fmt = '-', color = 'cyan', lw = 0.5, 
                          label = 'precipitation')

        subs[i].set_ylabel('Precipitation (mm)', color = 'cyan')

        i = 1

        if len(tmaxdata) > 0: 
            times, temp = zip(*tmaxdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            temp = [0, None, None, 1]

        subs[i].plot_date(times, temp, fmt = '-', color = 'red', lw = 0.5, 
                          label = 'max temperature')

        if len(tmindata) > 0: 
            times, temp = zip(*tmindata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            temp = [0, None, None, 1]

        subs[i].plot_date(times, temp, fmt = '-', color = 'blue', lw = 0.5, 
                          label = 'min temperature')
        subs[i].set_ylabel('Temperature (\u00B0C)', color = 'red')

        if len(dewtdata) > 0: 
            times, temp = zip(*dewtdata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            temp = [0, None, None, 1]

        subs[i].plot_date(times, temp, fmt = '-', color = 'green', lw = 0.5, 
                          label = 'dewpoint')
        subs[i].set_ylabel('Temperature (\u00B0C)', color = 'red')

        subs[i].legend(fontsize = 10, loc = 'lower left')

        i = 2
    
        if len(winddata) > 0: 
            times, wind = zip(*winddata)
        else:
            times = [start, start + datetime.timedelta(days = 1), 
                     end - datetime.timedelta(days = 1), end]
            wind = [0, None, None, 1]

        subs[i].plot_date(times, wind, fmt = '-', color = 'purple', lw = 0.5, 
                          label = 'wind')
        subs[i].set_ylabel('Wind Speed (m/s)', color = 'purple')

        if output is not None: pyplot.savefig(output)

        if show: pyplot.show()

        pyplot.clf()
        pyplot.close()

class Precip3240Station:
    """A class to store meteorology data from the NCDC Hourly Precipitation
    Dataset."""

    def __init__(self, 
                 coop, 
                 wban, 
                 desc, 
                 lat, 
                 lon, 
                 elev, 
                 st, 
                 code,
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

    def import_tar(self, 
                   f, 
                   verbose = True,
                   ):
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

    def import_data(self, 
                    directory, 
                    start, 
                    end, 
                    verbose = True,
                    ):
        """Downloads the data for the desired time period from the NCDC."""

        if not os.path.isdir(directory):
            print('\nerror: directory "{}" does not exist\n'.format(directory))
            raise

        if verbose: 
            print('attempting to import data for {}'.format(self.coop))

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

    def download_state_precip3240(self,
                                  directory,
                                  verbose = True,
                                  ):
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
            
            archives = [a for a in archives if self.is_integer(a[-4:])]

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

    def decompress7z(self, 
                     filename, 
                     directory,
                     path_to_7z = r'C:/Program Files/7-Zip/7z.exe',
                     ):
        """Decompresses a Unix-compressed archive on Windows using 7zip."""
        
        c = '{0} e {1} -y -o{2}'.format(path_to_7z, filename, directory)

        subprocess.call(c)

    def decompresszcat(self, 
                       filename, 
                       directory,
                       ):
        """Decompresses a Unix-compressed archive on Windows using 7zip."""

        with subprocess.Popen(['zcat', filename], 
                              stdout = subprocess.PIPE).stdout as s:

            with open(filename[:-2], 'wb') as f: f.write(s.read())

    def decompress(self, 
                   filepath, 
                   directory,
                   ):
        """Calls 7zip to decompress the files."""

        if os.name == 'nt':
            if not os.path.isfile(self.path_to_7z):
                print('\nerror: path to 7zip ' +
                      '{} does not exist'.format(self.path_to_7z))
                raise
            self.decompress7z(filepath, directory, path_to_7z = self.path_to_7z)
        else:
            self.decompresszcat(filepath, directory)

    def check_filenames(self, 
                        directory, 
                        start, 
                        end,
                        ):
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
                        
    def download_data(self, 
                      directory, 
                      start, 
                      end, 
                      clean = False, 
                      plot = True,
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
        if (plot and not os.path.isfile(destination + '.png') and 
            len(self.events) > 0):

            try: self.plot(start = start, end = end, output = destination)
            except: print('warning: unable to plot precipitation data')

        # save the import

        if len(self.events) > 0:
            
            with open(destination, 'wb') as f: pickle.dump(self, f)

        else:

            print('station {} contains no data, ignoring\n'.format(self.coop))

        # remove the intermediate files if desired

        if clean: shutil.rmtree('{}/precip3240'.format(directory))

    def make_timeseries(self, 
                        tstype = 'precip',
                        start = None, 
                        end = None, 
                        tstep = 'hourly',
                        ):
        """Makes a timeseries from the events."""

        if len(self.events) == 0: 
            print('warning: station has no data')
            return

        if start is None: start = self.events[0][0]
        if end   is None: end   = self.events[-1][0]

        # make sure the function inputs are correct

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            raise
        
        if start >= end:
            print('start date must be less than end date')
            raise

        if start < self.events[0][0] or self.events[-1][0] < end:
            print('warning: specified range ' +
                  '({} to {}) is outside of '.format(start, end) + 
                  'available gage data range ' +
                  '({} to {})\n'.format(self.events[0][0], self.events[-1][0]))

        # if there is no overlap don't bother anything

        if start > self.events[-1][0] or end < self.events[0][0]:

            if tstep == 'hourly':
                series = [None for i in range((end-start).days * 24)]
            elif tstep == 'daily':
                series = [None for i in range((end-start).days)]
            else:
                print('error: unknown time step specified\n')
                raise
            return series

        # keep the timeseries in a list

        series = []

        # find the first event after start

        i = 0
        while self.events[i][0] < start: i+=1

        # fill in Nones until reaching the first event if it is later than 
        # the start date, otherwise use zeros

        t = start
        while t < self.events[i][0] and t < end:
            if self.events[0][0] > start: 
                series.append(None)
            else: 
                series.append(0.)
            t += datetime.timedelta(hours = 1)
        
        # iterate through to the end

        while t < end and t < self.events[-1][0]:

            # append zeros until reaching an event

            while t < self.events[i][0] and t < end:
                series.append(0.)            
                t += datetime.timedelta(hours = 1)

            # collect as normal

            if ((self.events[i][2] == ' ' or self.events[i][2] == 'E') and
                t < end):
                series.append(self.events[i][1])
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
                    series.append(hourly)
                    t += datetime.timedelta(hours = 1)

            # missing period or record

            elif self.events[i][2] == '{' or self.events[i][2] == '[':

                while self.events[i][2] != '}' and self.events[i][2] != ']':
                    i += 1

                while t < self.events[i][0] and t < end:
                    series.append(None)
                    t += datetime.timedelta(hours = 1)

            # incomplete period of record

            elif self.events[i][2] == 'I':

                # if it's the last event, just fill to the end

                if i == len(self.events) - 1:

                    while t < end:
                        series.append(None)
                        t += datetime.timedelta(hours = 1)

                # otherwise fill to the end of the missing period

                else:

                    while t < self.events[i+1][0] and t < end:
                        series.append(None)
                        t += datetime.timedelta(hours = 1)
                
            elif self.events[i][1] > 999: 
                
                series.append(None)
                t += datetime.timedelta(hours = 1)

            elif t < end:
                series.append(0)
                t += datetime.timedelta(hours = 1)

            i += 1

        while t < end:
            series.append(0.)            
            t += datetime.timedelta(hours = 1)

        if   tstep == 'hourly': return series
        elif tstep == 'daily':  
            series = [sum([s for s in series[i:i+24] if s is not None]) 
                      for i in range(0, len(series), 24)]
            return series
        else: 
            print('warning, unknown time step specified')
            return

    def pct_missing(self,
                    start = None, 
                    end = None,
                    ):
        """Determines the percentage of missing data across the specified
        period."""

        precip = self.make_timeseries(start = start, end = end) 

        return len([p for p in precip if p is None]) / len(precip)

    def total_precipitation(self, 
                            start = None, 
                            end = None,
                            ):
        """Determines the total precipitation across the time period."""

        precip = self.make_timeseries(start = start, end = end) 

        return sum([p for p in precip if p is not None])

    def plot(self, 
             start = None, 
             end = None, 
             tstep = 'daily', 
             show = False, 
             output = None, 
             verbose = True,
             ):
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

        precip = self.make_timeseries(start = start, end = end) 

        times  = [start + i * datetime.timedelta(hours = 1)
                  for i in range((end-start).days * 24)]

        if tstep == 'hourly':
            nones  = [-1 if p[1] is None else 0 for p in ts]
            precip = [p if p is not None else 0 for p in precip]
            l = 'hr'
                
        elif tstep == 'daily':
            nones  = [-1 if all([p is None for p in precip[i:i+24]]) else 0 
                       for i in range(0, len(precip), 24)]
            precip = [p if p is not None else 0 for p in precip]
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

    def __init__(self, 
                 usaf, 
                 wban, 
                 cl, 
                 mflag, 
                 station, 
                 lat, 
                 lon, 
                 elev,
                 ):

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

    def is_integer(self, 
                   s,
                   ):
        """Tests if string "s" is an integer."""
        try: int(s) 
        except ValueError: return False
        return True

    def download_data(self, 
                      destination, 
                      dates = None,
                      NSRDB = 'http://rredc.nrel.gov/solar/old_data/nsrdb',
                      ):
        """Downloads the data and pickles it to the destination directory."""

        # figure out if the old and new data sources are needed

        if dates is None: old, new = False, True
        else:
            old = dates[0].year <  1991
            new = dates[1].year >= 1991

        if new:

            print('getting the new data for {}'.format(self.station))

            # path to the source data on the NSRDB

            url = '{}/1991-2010/targzs/{}.tar.gz'.format(NSRDB, self.usaf)

            # get the data and download it to a file-like bytes object in memory

            req = request.Request(url)

            with io.BytesIO(request.urlopen(req).read()) as b:

                # use the tarfile module to read the archive in memory

                with tarfile.open(fileobj = b, mode = 'r:gz') as t:

                    for m in t.getmembers():

                        if m.isfile(): 
                            year = int(m.name[14:18])
                        else:          
                            year = 0

                        if dates is None: 
                            collect = True
                        else:
                            collect = (dates[0].year <= year and 
                                       year <= dates[1].year)

                        # if the year is in the desired period, collect it

                        if collect:

                            data = [l.split(',') for l in
                                    t.extractfile(m).read().decode().split('\n')
                                    if len(l.split(',')) == 43]

                            suny = [int(row[6])  for row in data[1:]]
                            mets = [int(row[15]) for row in data[1:]]
                            meas = [int(row[27]) for row in data[1:]]
                    
                            times = [datetime.datetime(year, 1, 1) + 
                                     datetime.timedelta(hours = 1) * i 
                                     for i in range(len(suny))]

                            if any([s > 0 for s in suny]): 
                                self.suny += [s for s in zip(times, suny)]

                            if any([s > 0 for s in mets]): 
                                self.metstat += [s for s in zip(times, mets)]

                            if any([s > 0 for s in meas]): 
                                self.observed += [s for s in zip(times, meas)]

        if old and self.wban is not None:

            print('getting the legacy data for {}'.format(self.station))

            # path to the source data on the NSRDB

            v = NSRDB, self.wban
            url = '{}/1961-1990/hourly/compressed/{}.tar.gz'.format(*v)

            # get the data and download it to a file-like object in memory

            req = request.Request(url)

            with io.BytesIO(request.urlopen(req).read()) as b:

                # use the tarfile module to read the archive in memory

                with tarfile.open(fileobj = b, mode = 'r:gz') as t:

                    for m in t.getmembers():

                        if m.isfile(): 
                            year = int('19{}'.format(m.name[6:8]))
                        else:          year = 0

                        if dates is None: 
                            collect = True
                        else:
                            collect = (dates[0].year <= year and 
                                       year <= dates[1].year)

                        # if the year is in the desired period, collect it

                        if collect:

                            # values: total, direct normal, diffuse horizontal

                            d = [[l[23:27], l[31:35], l[39:43]] for l in
                                 t.extractfile(m).read().decode().split('\n')]

                            # just take total

                            legacy = [int(row[0]) for row in d[1:]
                                      if self.is_integer(row[0])]

                            times = [datetime.datetime(year, 1, 1) + 
                                     datetime.timedelta(hours = 1) * i 
                                     for i in range(len(legacy))]

                            self.legacy += [s for s in zip(times, legacy)]

                            # work around for last value (for some reason 9999)

                            last = datetime.datetime(1990, 12, 31, 23)
                            if self.legacy[-1][0] == last: 
                                print('replacing the last value')
                                self.legacy[-1] = (last, 0)

        # dump the info to the destination

        with open('{}/{}'.format(destination, self.usaf), 'wb') as f: 
            pickle.dump(self, f)

    def aggregate_daily_monthly(self, 
                                daily, 
                                start, 
                                end, 
                                option = 'average',
                                ):
        """Aggregates a daily timeseries into a monthly one."""

        dates = [start + i * datetime.timedelta(days = 1)
                 for i in range((end-start).days)]

        if len(dates) < 30:
            print('warning: insufficiently short time series')
            return

        if len(dates) != len(daily):
            print('error: dates and data length must be the same')
            raise

        months, monthly = [dates[0]], [daily[0]]

        # iterate through the data

        for t, v in zip(dates[1:], daily[1:]):

            # check if it's a new month

            if months[-1].month != t.month:

                # start a new month

                months.append(t)
                monthly.append(v)

            # otherwise add the value to the monthly total

            elif v is not None and monthly[-1] is not None: monthly[-1] += v

            else: monthly[-1] = None

        # change from total to average if needed

        if option == 'average': 
        
            for i in range(len(months)):

                # get the number of days in the last month

                day, ndays = monthrange(months[i].year, months[i].month)

                # and divide to get the average

                if monthly[i] is not None:
                    monthly[i] = monthly[i] / ndays
                else: 
                    monthly[i] = None

        elif option != 'total':

            print('Warning: unknown option specified')
            raise

        return months, monthly

    def make_timeseries(self,
                        dataset = 'metstat',
                        start = None, 
                        end = None, 
                        tstep = 'hourly', 
                        option = 'average',
                        verbose = False,
                        ):
        """Returns a time series of values for the station."""

        if   dataset == 'metstat':  solar = self.metstat
        elif dataset == 'suny':     solar = self.suny
        elif dataset == 'observed': solar = self.observed

        self.solar = [(d, v) for d, v in self.legacy + solar if v >= 0]

        if start is None: start = self.solar[0][0]
        if end is None:   end = self.solar[-1][0]

        # make sure the function inputs are correct
        
        if start >= end:
            print('error: start must be less than end\n')
            raise

        if (not isinstance(start, datetime.datetime) or 
            not isinstance(end, datetime.datetime)):
            print('start and end must be datetime.datetime instances')
            raise

        # check if data are available for the period

        if len(self.solar) == 0:
            print('warning: specified range ({} to {}) '.format(start, end) + 
                  'is outside of available station data')
            hourly = [None for i in range((end-start).days * 24)]

        else:

            if start < self.solar[0][0] or self.solar[-1][0] < end:
                print('warning: specified range ({} to {}) '.format(start, end)
                      + 'is outside of available station data')

            hourly = []

            # find the first event after start

            i = 0
            while self.solar[i][0] < start: i+=1

            # fill in Nones until reaching the first event

            t = start
            while t < self.solar[i][0] and t < end:
                hourly.append(None)            
                t += datetime.timedelta(hours = 1)

            # iterate through to the end

            while t < end and t < self.solar[-1][0]:

                # append zeros until reaching an event

                while t < self.solar[i][0] and t < end:
                    hourly.append(0.)            
                    t += datetime.timedelta(hours = 1)

                # collect as normal

                if t < end:
                    hourly.append(self.solar[i][1])
                    t += datetime.timedelta(hours = 1)

                i += 1

            # collect to the end if data aren't present

            while t < end:
                hourly.append(None)   
                t += datetime.timedelta(hours = 1)

        if   tstep == 'hourly': return hourly

        elif tstep == 'daily':  

            # do a simple aggregation of every 24 hours

            series = [sum([v for v in hourly[i:i+24] if v is not None])
                      for i in range(0, len(hourly), 24)]

            return series

        elif tstep == 'monthly':

            # aggregate to the daily level first

            series = [sum([v for v in hourly[i:i+24] if v is not None])
                      for i in range(0, len(hourly), 24)]

            # now aggregate the daily values to monthly (returns both dates 
            # and values)
            
            times, values = self.aggregate_daily_monthly(series, start, end,
                                                         option = 'average')

            # returns only values (needed for consistency but perhaps not ideal)

            return values

        else: 

            print('error: unknown time step specified')
            raise

    def plot(self,
             start = None,
             end = None,
             tstep = 'hourly',
             verbose = True,
             output = None,
             ):

        if verbose: print('plotting NSRDB data\n')

        # make the figure and subplots

        fig = pyplot.figure()
        sub = fig.add_subplot(111)

        # title

        t = 'NSRDB Solar Radiation Data for {}'.format(self.station)
        sub.set_title(t)

        if tstep == 'hourly': 

            times = [start + i * datetime.timedelta(hours = 1)
                     for i in range((end-start).days * 24)]

        if tstep == 'daily':  

            times = [start + i * datetime.timedelta(days = 1)
                     for i in range((end-start).days)]

        if tstep == 'monthly':

            times = [start]

            # iterate through the data

            t = start
            while t < end:

                # see if it's a new month

                if t.month != times[-1].month: times.append(t)

                # move along

                t += datetime.timedelta(days = 1)

        metstat = self.make_timeseries(dataset = 'metstat', start = start, 
                                       end = end, tstep = tstep)

        # convert units

        if tstep == 'daily' or tstep == 'monthly' and metstat is not None:   

            metstat = [v / 1000 if v is not None else None for v in metstat]

        if metstat is not None:

            sub.plot_date(times, metstat, color = 'yellow', lw = 1, fmt = '-',
                          label = 'metstat')

        suny = self.make_timeseries(dataset = 'suny', start = start, 
                                    end = end, tstep = tstep)

        # convert units

        if tstep == 'daily' or tstep == 'monthly' and suny is not None: 

            suny = [v / 1000 if v is not None else None for v in suny]

        sub.plot_date(times, suny, color = 'green', fmt = '-', lw = 1,
                      label = 'suny')

        # units

        if   tstep == 'hourly':  
            l = 'Solar Radiation\n(W/m\u00B2)'
        elif tstep == 'daily':   
            l = 'Solar Radiation\n(kWhr/m\u00B2/day)'
        elif tstep == 'monthly': 
            l = 'Average Solar Radiation\n(kWhr/m\u00B2/day)'

        # labels 

        sub.set_xlabel('Date', size = 12)
        sub.set_ylabel(l)
        sub.yaxis.set_major_locator(ticker.MaxNLocator(8))

        pyplot.legend()

        fig.autofmt_xdate()
        for tick in sub.xaxis.get_major_ticks():
            tick.label.set_fontsize(10)
            
        if output is not None: pyplot.savefig(output)

        pyplot.clf()
        pyplot.close()
