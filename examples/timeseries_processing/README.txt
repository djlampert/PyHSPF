The raw time series data gathered from the various databases are typically not suitable for hydrologic modeling for a variety of reasons. For example, some of the data may be missing or there may have been errors in reporting. HSPF models require estimates of the conditions throughout the watershed, so "point" observations must be interpolated/extrapolated throughout the watershed. There are an infinite number of different methods for aggregating and disaggregating climate data series. PyHSPF has a ClimateProcessor class to assist with these tasks. The ClimateProcessor class is designed to help gather and aggregate all the data in several pre-determined ways, which eliminates the needs to use climateutils in the other section. However, this costs some flexibility so it is probably worthwhile to understand how to get the data using those methods before using these higher level methods. The following scripts illustrate some features of the ClimateProcessor class. 

climateprocessor01.py -- downloads all climate data using the ClimateProcessor
climateprocessor02.py -- manually aggregates GHCND tmax data into a timeseries
climateprocessor03.py -- automatically aggregates GHCND data
climateprocessor04.py -- automatically aggregates GSOD data
climateprocessor05.py -- automatically aggregates NSRDB data
climateprocessor06.py -- automatically aggregates hourly precipitation data
climateprocessor07.py -- manually aggregation using inverse distance weighting
