# cdlexample02.py
#
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to download data for a bounding box for a year from the
# Cropland Data Layer (CDL) and then calculate the landuse data for each
# polygon in the shapefile utilizing the subbasin shapefile for the
# Patuxent River Watershed (HUC 02060006), in Maryland (similar to the last
# but downloads the clipped file directly rather than the state)
#
# last updated: 07/31/2015

import os

# adjustable input parameters
cwd = os.path.dirname(__file__)
output = os.path.join(cwd,'CDL')             # path to place state CDL source rasters
year   = 2008              # year to extract data (CDL is annual)
sfile  = os.path.join(cwd,'data/catchments') # catchment file for land use calculation

# the aggregate file (CSV); maps integers in the CDL raster to landuse groups

aggregate = os.path.join(cwd,'data/cdlaggregation.csv')

# the land use codes file (CSV); maps land use categories defined in the
# aggregate file to properties including RGB color tuple and crop coefficients

lucfile = os.path.join(cwd, 'data/lucs.csv')

# look at these files to understand how the raw categories map to user-specified
# information for each land use category

# make sure the essential input data files exist

if not os.path.isdir(os.path.join(cwd,'data')):
    print('\nerror: please ensure that the "data" directory has been created ' +
          'and contains the essential files before running the script!\n')
    raise

required = sfile + '.shp', aggregate, lucfile

for f in required:
    if not os.path.isfile(f):
        print('error: to run this script please ensure the file ' +
              '{} has been placed in the "data" directory'.format(f))
        raise

# extract cropland data for the state from NASS using the CDLExtractor class

from pyhspf.preprocessing import CDLExtractor

# provide the directory where the source data are located (or will be placed)

cdlextractor = CDLExtractor(output)

# download the data for the extent of the bounding box for the given year to
# the "output" location

cdlextractor.download_shapefile(sfile, year)

# calculate the 2008 land use fraction for each category in each shape in the
# catchmentfile using the "FEATUREID" feature attribute, and then make an
# (optional) csv file of the output

extracted = '{}/{}landuse.tif'.format(output, year)
csvfile   = '{}/catchment_landuse.csv'.format(output)
attribute = 'FEATUREID'

landuse = cdlextractor.calculate_landuse(extracted, sfile, aggregate,
                                         attribute, csvfile = csvfile)

# the results returned to the "landuse" variable are return as a dictionary of
# dictionaries as follows:
#
# the keys to the first dictionary level are the field values for the
# 'FEATUREID' attribute (or whatever field from the shapefile is supplied)
#
# the keys to the second level dictionary are the unique values of the landuse
# specified in "aggregate" (data/patuxent/cdlaggregation.csv)
#
# to understand how this works, it's a good idea to spend some time looking at
# the aggregate file and CDL rasters and the output; there are 255 unique codes
# in the CDL that are mapped into 10 categories in this example

# the processing can be visualized using the plot_landuse method. the
# "datatype" keyword of 'raw' or 'results' can be used to see the aggregated
# values before and a "band chart" where the area of the stripes corresponds
# to the area of the land use fraction (this is how HSPF "thinks" about landuse)
# the colors are specified in the lucfile as RGB tuples

cdlextractor.plot_landuse(extracted, sfile, attribute, lucfile,
                          output = '{}/{}raw'.format(output, year),
                          datatype = 'raw')

# and make the land use image file

print('this will take a while...\n')

cdlextractor.plot_landuse(extracted, sfile, attribute, lucfile, lw = 0.1,
                          output = '{}/results'.format(output, extracted),
                          datatype = 'results')
