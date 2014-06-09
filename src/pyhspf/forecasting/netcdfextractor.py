#!/usr/bin/env python3
#
# netcdfextractor.py
#
# used to read data from netcdf files in a folder

import os, fnmatch, pickle, math, time, datetime, numpy

from scipy.io        import netcdf
from dateutil        import tz
from multiprocessing import Pool, cpu_count

from pyhspf.forecasting.nrcm import NRCMGrid

class NetCDFExtractor:
    """
    A class to open and re-organize data from Nested Regional Climate
    Model netcdf files.
    """

    def __init__(self, source, destination, start, end, timezone):

        if not os.path.isdir(destination): os.mkdir(destination)

        self.source      = source
        self.destination = destination
        self.timezone    = timezone
        self.years       = [start + i for i in range(end - start)]

        # make a list of all the source files

        ncfiles = []
        for root, dirnames, filenames in os.walk(self.source):
            for filename in fnmatch.filter(filenames, '*.nc'):
                ncfiles.append(os.path.join(root, filename))

        # use the first to get the coordinates

        netcdffile = ncfiles[0]

        self.get_latslons(netcdffile)

    def get_latslons(self, netcdffile):
        """Gets the coordinates for the file."""

        f = netcdf.netcdf_file(netcdffile, 'r')

        lats = f.variables['lat'][:].flatten()
        lons = f.variables['lon'][:].flatten()

        f.close()

        self.names  = ['{:3.4f}_{:3.4f}'.format(lon, lat) 
                       for lat, lon in zip(lats, lons)]

    def rounddown(self, x, base):
        """Rounds "x" down to the nearest "base" """

        return int(base * math.floor(x / base))

    def roundup(self, x, base):
        """Rounds "x" down to the nearest "base" """

        return int(base * math.ceil(x / base))

    def get_pfile(self, directory, t):
        """Returns the file path to precipitation file for the given time."""

        y  = t.year
        y1 = self.rounddown(y, 5)

        if    y1 == 2000 or y1 == 2005: y1, y2 = 2000, 2005
        else:                           y1, y2 = y1,   y1 + 4

        m = t.month

        m2 = self.roundup(m, 3)
        m1 = m2 - 2

        it = directory, y1, y2, y, m1, m2

        return '{}/{:04d}-{:04d}/{:04d}_{:02d}-{:02d}_3hr_rain.nc'.format(*it)

    def get_3hr_file(self, directory, t):
        """Returns the file path to precipitation file for the given time."""

        y = t.year
        m = t.month

        m2 = self.roundup(m, 3)
        m1 = m2 - 2

        it = directory, y, m1, m2

        return '{}/T2_{:04d}_{:02d}-{:02d}_t2m_3hr.nc'.format(*it)

    def get_solar_snow_evp_file(self, directory, t):
        """Returns the file path to the solar file for the given time."""

        y = t.year
        m = t.month

        m2 = self.roundup(m, 3)
        m1 = m2 - 2

        it = directory, y, m1, m2

        return '{}/{:04d}_{:02d}-{:02d}_3hr_snow_solar_evp.nc'.format(*it)

    def get_daily_file(self, tstype, directory, t):
        """Returns the file path to precipitation file for the given time."""

        if   tstype == 'humidity': v = 'rh2'
        elif tstype == 'solar':    v = 'swd'
        elif tstype == 'wind':     v = 'wind'
        else: 
            print('error: unknown time series type specified')
            raise

        return '{}/{:04d}_{}_daily.nc'.format(directory, t.year, v)

    def get_hfile(self, directory, t): 

        return self.get_daily_file('humidity', directory, t)

    def get_wfile(self, directory, t): 

        return self.get_daily_file('wind', directory, t)

    def get_precip_series(self, netcdffile):
        """Returns a list of timeseries of "tstype" for a netcdf file "f" """

        f = netcdf.netcdf_file(netcdffile, 'r')

        # get the data and reshape it to time series

        data = f.variables['rain'][:]
        data = data.swapaxes(0,2).swapaxes(1,3)
                                       
        w, x, y, z = data.shape

        return data.reshape(w * x, y * z)

    def get_3d_series(self, tstype, netcdffile):
        """Returns a list of timeseries of type "tstype" for a netcdf file."""

        f = netcdf.netcdf_file(netcdffile, 'r')

        # get the data and reshape it to time series

        data = f.variables[tstype][:]
                                       
        x, y, z = data.shape

        data = numpy.swapaxes(data, 0, 1)
        data = numpy.swapaxes(data, 1, 2)

        return data.reshape(y * z, x)

    def get_temp_series(self, netcdffile): 
        
        return self.get_3d_series('temp', netcdffile)

    def get_rh2_series(self, netcdffile): 
        
        return self.get_3d_series('rh2', netcdffile)

    def get_swd_series(self, netcdffile): 

        return self.get_3d_series('SWDOWN', netcdffile)

    def get_solar_series(self, netcdffile): 

        return self.get_3d_series('Solar', netcdffile)

    def get_swe_series(self, netcdffile): 

        return self.get_3d_series('SWE', netcdffile)

    def get_pot_series(self, netcdffile): 

        return self.get_3d_series('POTEVP', netcdffile)

    def get_wind_series(self, netcdffile): 

        return (self.get_3d_series('u10', netcdffile)**2 + 
                self.get_3d_series('v10', netcdffile)**2)**0.5

    def get_months(self, tstype, year):
        """Returns the iterables for the 3-month periods of a year."""

        z = datetime.timezone.utc

        if tstype in ['rain', 'temperature', 'evaporation', 'snowdepth',
                      'solar']:

            return [(datetime.datetime(year,     1, 1, tzinfo = z), 
                     datetime.datetime(year,     4, 1, tzinfo = z)),
                    (datetime.datetime(year,     4, 1, tzinfo = z), 
                     datetime.datetime(year,     7, 1, tzinfo = z)),
                    (datetime.datetime(year,     7, 1, tzinfo = z), 
                     datetime.datetime(year,    10, 1, tzinfo = z)),
                    (datetime.datetime(year,    10, 1, tzinfo = z), 
                     datetime.datetime(year + 1, 1, 1, tzinfo = z)),
                    ]

        else:

            return [(datetime.datetime(year,     1, 1, tzinfo = z), 
                     datetime.datetime(year + 1, 1, 1, tzinfo = z)),
                    ]

    def organize(self, variable):
        """Reads the netcdf files in "source" and re-organizes them by their
        coordinates "names" in directory "destination" for "years" and time 
        series variables "tstype." 
        """

        if variable == 'rain': 
            get_file = self.get_pfile
            get_netcdf_series = self.get_precip_series
            source = '{}/3hr_rainfall'.format(self.source)
            delta = datetime.timedelta(hours = 3)
            mi, ma = 0, 200
        elif variable == 'temperature': 
            get_file = self.get_3hr_file
            get_netcdf_series = self.get_temp_series
            source = '{}/3hr_temp'.format(self.source)
            delta = datetime.timedelta(hours = 3)
            mi, ma = -100, 100
        elif variable == 'humidity':
            get_file = self.get_hfile
            get_netcdf_series = self.get_rh2_series
            source = '{}/Daily_rhum_2m'.format(self.source)
            delta = datetime.timedelta(days = 1)
            mi, ma = 0, 100
        elif variable == 'solar':
            get_file = self.get_solar_snow_evp_file
            get_netcdf_series = self.get_solar_series
            source = '{}/3hr_solar_snow'.format(self.source)
            delta = datetime.timedelta(hours = 3)
            mi, ma = 0, 10000
        elif variable == 'evaporation':
            get_file = self.get_solar_snow_evp_file
            get_netcdf_series = self.get_pot_series
            source = '{}/3hr_solar_snow'.format(self.source)
            delta = datetime.timedelta(hours = 3)
            mi, ma = 0, 10000
        elif variable == 'snowdepth':
            get_file = self.get_solar_snow_evp_file
            get_netcdf_series = self.get_swe_series
            source = '{}/3hr_solar_snow'.format(self.source)
            delta = datetime.timedelta(hours = 3)
            mi, ma = 0, 10000
        elif variable == 'wind':
            get_file = self.get_wfile
            get_netcdf_series = self.get_wind_series
            source = '{}/Daily_wind_10m'.format(self.source)
            delta = datetime.timedelta(days = 1)
            mi, ma = 0, 200

        destination = '{}/{}'.format(self.destination, variable)
        z = tz.gettz(self.timezone)

        if not os.path.isdir(destination): os.mkdir(destination)
    
        for year in self.years:
    
            ydir = '{}/{}'.format(destination, year)
            if not os.path.isdir(ydir): 
    
                os.mkdir(ydir)
    
                datas = [[] for i in range(len(self.names))]
                for m1, m2 in self.get_months(variable, year):
    
                    name = get_file(source, m1)
    
                    print('adding data from {}'.format(name))
    
                    # get the data
    
                    data = get_netcdf_series(name)

                    # make a timeseries from the data
    
                    times = [m1 + delta * i for i in range(len(data[0]))]
                    times = [t.astimezone(z) for t in times]
                    
                    for l, da in zip(datas, data):

                        l += [(t, d) if mi <= d and d <= ma else (t, 0) 
                              for t, d in zip(times, da)]
    
                print('dumping {}...'.format(year))
                for n, d in zip(self.names, datas):
                    o = '{}/{}'.format(ydir, n)
                    with open(o, 'wb') as f: pickle.dump(d, f)

    def extract_gridpoints(self, s, 
                           dtypes = ['rain', 'humidity', 'solar', 'temperature',
                                     'wind', 'snowdepth', 'evaporation'],
                           tsteps = [180, 1440, 1440, 180, 1440, 180, 180]):
        """make a function of one variable to run in parallel."""

        stationdata = '{}/gridpoints'.format(self.destination)

        p = '{}/{}'.format(stationdata, s)
        if not os.path.isfile(p):
            i = s.index('_')
            lon = float(s[:i])
            lat = float(s[i+1:])
            print('processing grid point {}, {}'.format(lon, lat))
   
            t1 = datetime.datetime(self.years[0], 1, 1, 
                                   tzinfo = datetime.timezone.utc)
            t2 = t1.astimezone(tz.gettz(self.timezone))
            station = NRCMGrid(lat, lon, t2)

            for dtype, tstep in zip(dtypes, tsteps):
                for y in self.years:
                    sfile = '{}/{}/{}/{}'.format(self.destination, dtype, y, s)
                    with open(sfile, 'rb') as f: ts = pickle.load(f)
                    station.add_data(dtype, tstep, ts)

            with open(p, 'wb') as f: pickle.dump(station, f)

    def extract(self, parallel = True, metrics = True):
        """Extracts data from the netcdf files into a more useful format."""

        self.pyhspf_names = ['rain',
                             'temperature',
                             'humidity',
                             'solar',
                             'snowdepth',
                             'evaporation',
                             'wind',
                             ]
        
        if parallel:
            pool = Pool(len(self.pyhspf_names))
            pool.map(self.organize, self.pyhspf_names)
            pool.close()

        else: 
            for v in pyhspf_names: self.organize(v)
                  
        # re-organize into NRCM classes at each grid point

        stationdata = '{}/gridpoints'.format(self.destination)
        if not os.path.isdir(stationdata): os.mkdir(stationdata)

        if parallel:
            pool = Pool(cpu_count())
            x = pool.map(self.extract_gridpoints, self.names)
            pool.close()

        else:
            for n in self.names: self.extract_gridpoints(n)

        if metrics: self.extract_metrics()

    def calculate_metrics(self, gridfile):
        """calculates the average precipitation, min and max temperature,
        average humidity, average solar radiation, and average wind speed
        from the gridfile."""

        with open(gridfile, 'rb') as f: s = pickle.load(f)

        times, prec = zip(*s.data['rain'])
            
        years = (times[-1] - times[0]).days / 365.25

        pavg = sum(prec) / years

        times, temp = zip(*s.data['temperature'])

        tmin, tmax = min(temp), max(temp)

        times, solar = zip(*s.data['solar'])

        savg = sum(solar) / years

        times, humidity = zip(*s.data['humidity'])

        havg = sum(humidity) / len(humidity)

        times, wind = zip(*s.data['wind'])

        wavg = sum(wind) / len(wind)

        return s.lon, s.lat, pavg, tmin, tmax, savg, havg, wavg 

    def extract_metrics(self, output = 'NRCM_grid_stats.txt'):
        """Extracts some interesting statistics from the gridfiles."""

        s = '\n{:9.4f}{:9.4f}{:6.0f}{:7.1f}{:6.1f}{:8.0f}{:6.1f}{:7.2f}'
        header = ('      Lon      Lat  Rain   Tmin  Tmax   Solar   Hum   Wind\n'
                  +
                  '                    (mm)    (C)   (C)     (W)   (%)  (m/s)')
        if not os.path.isfile(output):
            print('\ncalculating statistics...\n')

            source = os.path.join(self.destination, 'gridpoints')
            gridfiles = [os.path.join(source, f) for f in os.listdir(source)]
            pool = Pool(cpu_count())
            metrics = pool.map(self.calculate_metrics, gridfiles)
            pool.close()

            with open(output, 'w') as outputfile:
                outputfile.write(header)
                for m in metrics: outputfile.write(s.format(*m))

if __name__ == '__main__':

    # path to files

    if os.name == 'nt':
        source      = 'Z:/NRCM'
        #source      = 'D:/NRCM'
        destination = 'C:/HSPF_data/NRCM'
    elif os.name == 'posix':
        source      = '/media/dave/Data/NRCM'
        destination = '/home/dave/HSPF_data/NRCM'

    start       = 1985
    end         = 2006
    timezone    = 'Iowa'
    parallel    = True
    metrics     = True

    go = time.time()

    extractor = Extractor(source, destination, start, end, timezone)

    # aggregate the data by grid cells

    extractor.extract(parallel = parallel, metrics = metrics)

    print('\ndone extracting all in {:.0f} seconds'.format(time.time() - go))
