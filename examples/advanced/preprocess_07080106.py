# preprocess_07080106.py
#
# author: David J. Lampert (djlampert@gmail.com)
#
# last updated: 05/18/2015
# 
# Purpose: shows how to use the Preprocessor class to gather all the input
# data needed to create an HSPF model for an 8-digit hydrologic unit code 
# (HUC8). The Preprocessor downloads the raw hydrography data from the 
# National Hydrography Dataset Plus (NHDPlus), land use data from the Cropland 
# Data Layer (CDL), streamflow and water quality data from the National Water 
# Information System (NWIS), and the climate data from a number of different 
# databases from the National Climate Data Center (NCDC) as illustrated in 
# great detail in other examples. The raw data are then aggregated, 
# disaggregated, processed, etc into formats consistent with PyHSPF's 
# built-in HSPFModel class as illustrated in other examples.

import os, datetime

# Because the raw data files are pretty large, it may make sense to keep
# them on a server locally although they can be placed anywhere where there
# is sufficient space. The raw NHDPlus data for this example include the 
# whole Upper Mississippi River Basin (approximately 7 GB including the 
# compressed and uncompressed files) since that is how the data are formatted 
# and distributed by the publisher, Horizon Systems. More info at
#
# http://www.horizon-systems.com/nhdplus
# 
# The CDL is distributed by state, and in this case the data include land use 
# estimates for Iowa from 2001 to 2010 which are approximately 2 GB including 
# the compressed and uncompressed files.
#
# The NWIS and and NID metadata are distributed as point shapefiles.
# 
# So what the preprocessor does is check to see that all these raw files above 
# have been downloaded to the "network" location and if not, it downloads them;
# then it checks if the HUC8 data have been extracted to the specified 
# "destination" location and if not, it extracts them; then it checks if the 
# subbasin-level data have been aggregated and if not, it aggregates them; then
# it checks if the raw time series data have been downloaded and if not, it 
# downloads them; then it checks if the processed time series data exist and if
# not, it processes them. The method thus provides all the data needed to build 
# an instance of the HSPFModel class for the whole HUC8. It's worth noting that
# the methodology for any of these steps could be modified, and for models 
# larger than a HUC8, it shouldn't be too difficult to add smaller pieces 
# together using the power of Python. They idea is to eliminate limitations
# imposed by other HSPF software. Finally, submodels for individual gages can 
# be created easily starting from the HUC8 base model.
# 
# In the example below, the raw files exist/will be downloaded to:
#
# /media/data/DATA (for Linux) 
# 
# or 
# 
# D: (for Windows)
#
# The preprocessor does all of the following as illustrated in detail by the
# examples in parenthesis that are located elsewhere with the source code.
# 
#    1.  download and extract NHDPlus data for a HUC8 (nhdplus01.py)
#    2.  download and extract CDL data for a HUC8 (cdlexample.py)
#    3.  download and extract NID data for a HUC8 (nidexample.py)
#    4.  download and extract NWIS gage locations (nwisexample.py)
#    5.  download NWIS flow and water quality data (nwisexample.py)
#    6.  subdivide the HUC8 into basins with outlets co-located with dams and
#        gages with a maximal subbasin area (huc8example.py)
#    7.  calculate the land use fractions in each subbasin based on the CDL
#        data and the specifications in the aggregate.csv file (cdlexample.py)
#    8.  use the data from the preceeding steps to build an instance of the 
#        Watershed class that acts as a container for the GIS data inputs to 
#        the HSPFModel class (intro01.py)
#    9.  download and average 10 most complete daily tmin, tmax, dewpoint, and 
#        wind time series from GSOD database (climateprocessor04.py)
#    10. download and average 10 most complete daily snowfall and snowdepth
#        and any evaporation time series from GHCND (climateprocessor03.py)
#    11. download and average 10 most complete hourly solar radiation 
#        time series from NSRDB (climateprocessor05.py)
#    12. download and average hourly precipitation data using inverse-distance
#        weighted for each subbasin (climateprocessor08.py)
#    13. calculate daily reference evapotranspiration using the climate data 
#        and compare to pan evaporation (etcalculator01.py)
#    14. calculate hourly reference evapotranspiration using the climate data 
#        and compare to pan evaporation (etcalculator03.py)
#    15. calculate land use-specific potential evapotranspiration for each
#        of the categories in the aggregate.csv file (etcalculator06.py)
#
# The data above are everything needed for a base HSPF hydrology model.
#
# After downloading all the NHDPlus and CDL data, this script took about 20 
# minutes to run on my laptop. The NHDPlus download can take a few hours.

# Paths to working directories for source NHDPlus, CDL, NWIS, NID datasets
# (modify as needed for the PC of interest)

if os.name == 'posix':
    network     = '/media/dave/DATA'
    destination = '/home/dave/HSPF_data'
elif os.name == 'nt':
    network     = 'D:'
    destination = 'C:/HSPF_data'

# import the Preprocessor

from pyhspf.preprocessing import Preprocessor

# 8-digit hydrologic unit code of interest (North Skunk River, IA)

HUC8 = '07080106'

# two-digit state abbreviation for the CDL

state = 'ia'

# if the watershed is in more than one state, this will probably not work 
# (this is a feature that should be added in the future).

# start and end dates (2001 to 2010)

start = datetime.datetime(2001, 1, 1)
end   = datetime.datetime(2011, 1, 1)

# comma separated value file linking land use codes from the Cropland Data
# Layer to aggregated land use categories for HSPF land segments

aggregation = 'cdlaggregation.csv'

# comma separated value file of parameters for the HSPF land use categories
# including RGB values for plots and evapotranspiration crop coefficients

landuse = 'lucs.csv'

# because parallel processing is (optionally) used, the process method has 
# to be called at runtime as shown below

if __name__ == '__main__': 

    # make an instance of the preprocessor

    processor = Preprocessor(network, destination, cdlaggregate = aggregation,
                             landuse = landuse)

    # preprocess the HUC8

    processor.preprocess(HUC8, state, start, end)

    # using the preprocessor in other watersheds *should* be as simple as
    # supplying the start and end date, state and 8-digit HUC; if you try and 
    # get an error please report it!
