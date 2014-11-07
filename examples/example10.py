# example10.py
#
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to download data from the Cropland Data Layer (CDL),
# extract a Geotiff for a HUC8 from the source, and calculate the landuse
# data for each polygon in the shapefile. Example utilizes the subbasin 
# shapefile for the Patuxent River Watershed (HUC 02060006), which is in 
# Maryland.
#
# last updated: 11/04/2014

import os

output = 'CDL'                      # path to place state CDL source rasters
state  = 'MD'                       # Maryland (two-digit state abbreviation)
years  = [2008, 2009, 2010]         # years to extract data (CDL is annual)
cfile  = 'data/patuxent/catchments' # catchment file for land use calculation

# the aggregate file (CSV); maps integers in the CDL raster to landuse groups

aggregate = 'data/patuxent/aggregate.csv'

# make the output directory

if not os.path.isdir(output): os.mkdir(output)

# extract cropland data for the state from NASS using the CDLExtractor class

from pyhspf.preprocessing import CDLExtractor

# provide the directory where the source data are located (or will be placed)

cdlextractor = CDLExtractor(output)

# download the data for the state for each year

cdlextractor.download_data(state, years)

# extract the data for the bounding box of the patuxent catchment shapefile 
# that is located in the "data" directory (this could be generated from another
# example) and place it in the "output" directory

cdlextractor.extract_shapefile(cfile, output)

# calculate the 2008 landuse for each shape in the catchmentfile using the 
# "FEATUREID" feature attribute, and make the (optional) csv file of the output

year = 2008
extracted = '{}/{}landuse.tif'.format(output, year)
csvfile   = 'landuse.csv'.format(output, year)

# the results are stored in a dictionary of dictionaries--the keys are the 
# 'FEATUREID' attributes from the shapefile each of which has a value that
# is a dictionary with values that are dictionaries with keys that are
# the categories from the 3rd column in the aggregate.csv file and values 
# that are the fractions of the polygon. the landuse for the watershed is
# is plotted before and after processing to visualize the results.

bfile = 'data/patuxent/boundary'
cdlextractor.calculate_landuse(extracted, bfile, aggregate, 'FEATUREID', 
                               csvfile = csvfile, plot = True)

