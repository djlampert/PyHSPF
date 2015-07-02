# cdlexample.py
#
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to download data from the Cropland Data Layer (CDL),
# extract a Geotiff for a HUC8 from the source, and calculate the landuse
# data for each polygon in the shapefile. Example utilizes the subbasin 
# shapefile for the Patuxent River Watershed (HUC 02060006), in Maryland.
#
# last updated: 05/10/2015

import os

# adjustable input parameters

output = 'CDL'                  # path to place state CDL source rasters
state  = 'Maryland'             # state name
years  = [2008, 2009, 2010]     # years to extract data (CDL is annual)
sfile  = 'data/boundary'        # catchment file for land use calculation

# the aggregate file (CSV); maps integers in the CDL raster to landuse groups

aggregate = 'data/cdlaggregation.csv'

# the land use codes file (CSV); maps land use categories defined in the 
# aggregate file to properties including RGB color tuple and crop coefficients

lucfile = 'data/lucs.csv'

# look at these files to understand how the raw categories map to user-specified
# information for each land use category

# make sure the essential input data files exist

if not os.path.isdir('data'):
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

# download the data for the state for each year to the "output" location

cdlextractor.download_data(state, years)

# extract the data for the bounding box of the patuxent catchment shapefile 
# that is located in the "output" directory in the previous step and place it 
# in the "output" directory (in this case the same directory is used for both)

cdlextractor.extract_shapefile(sfile, output)

# calculate the 2008 land use fraction for each category in each shape in the 
# catchmentfile using the "FEATUREID" feature attribute, and then make an 
# (optional) csv file of the output

year = 2008
extracted = '{}/{}landuse.tif'.format(output, year)
csvfile   = 'basin_landuse.csv'
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
cdlextractor.plot_landuse(extracted, sfile, attribute, lucfile,
                          output = '{}/{}results'.format(output, year),
                          datatype = 'results')

# now repeat it using the catchment rather than just the boundary

sfile   = 'data/catchments'
csvfile = 'catchment_landuse.csv'
landuse = cdlextractor.calculate_landuse(extracted, sfile, aggregate, 
                                         attribute, csvfile = csvfile)

# this WILL take a while (adjust the border linewidth for clarity)

print('this will take a while...\n')

cdlextractor.plot_landuse(extracted, sfile, attribute, lucfile, lw = 0.1,
                          output = 'catchmentresults'.format(extracted), 
                          datatype = 'results')
