The raw time series data gathered from the various databases is typically not suitable for hydrologic modeling for a variety of reasons. For example, some of the data may be missing or errors may have been made in reporting. HSPF models require estimates of the conditions throughout the watershed, so "point" observations must be interpolated/extrapolated throughout the watershed. There are an infinite number of different methods for aggregating and disaggregating climate data series. PyHSPF has a ClimateProcessor class to assist with these tasks. The following scripts illustrate the climate processing tools available.

climateprocessor01.py -- downloads all climate data for a region into an organized structure
climateprocessor02.py -- aggregates GHCND temperature data into a single timeseries

