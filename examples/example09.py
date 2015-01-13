# example09.py
#
# David J. Lampert (djlampert@gmail.com)
#
# illustrates how to download data from the Cropland Data Layer (CDL),
# extract a Geotiff for a HUC8 from the source, and calculate the landuse
# data for each polygon in the shapefile. Example utilizes the subbasin 
# shapefile for the Patuxent River Watershed (HUC 02060006), which is in 
# Maryland.
#
# last updated: 11/10/2014

output = 'CDL'                      # path to place state CDL source rasters
state  = 'MD'                       # Maryland (two-digit state abbreviation)
years  = [2008, 2009, 2010]         # years to extract data (CDL is annual)
sfile  = 'data/patuxent/boundary'   # catchment file for land use calculation

# the aggregate file (CSV); maps integers in the CDL raster to landuse groups
# make sure you look at this file to understand what these commands are doing

aggregate = 'data/patuxent/aggregate.csv'

# extract cropland data for the state from NASS using the CDLExtractor class

from pyhspf.preprocessing import CDLExtractor

# provide the directory where the source data are located (or will be placed)

cdlextractor = CDLExtractor(output)

# download the data for the state for each year

cdlextractor.download_data(state, years)

# extract the data for the bounding box of the patuxent catchment shapefile 
# that is located in the "data" directory (this could be generated from another
# example) and place it in the "output" directory

cdlextractor.extract_shapefile(sfile, output)

# calculate the 2008 landuse for each shape in the catchmentfile using the 
# "FEATUREID" feature attribute, and make an (optional) csv file of the output

year = 2008
extracted = '{}/{}landuse.tif'.format(output, year)
csvfile   = 'basin_landuse.csv'

# the results are returned as a dictionary of dictionaries--the keys are the 
# 'FEATUREID' attributes from the shapefile and the categories from the 3rd 
# column in the aggregate.csv file. the values in the dictionary are the 
# fractions of the polygon for the shapefile. 

attribute = 'FEATUREID'
landuse = cdlextractor.calculate_landuse(extracted, sfile, aggregate, 
                                         attribute, csvfile = csvfile)

# the results are returned as a dictionary of dictionaries as follows:
#
# the keys to the first dictionary are the field values for the 'FEATUREID' 
# attribute (or whatever field you want to use)
#
# the keys to the second dictionary are the unique values of the landuse
# specified the data/patuxent/aggregate.csv file
#
# to understand this works, it's a good idea to spend some time looking at 
# the aggregate file and CDL rasters and the output.

# the processing can be visualized using the plot_landuse method. the 
# "datatype" keyword of 'raw' or 'results' can be used to see the aggregated
# values before and a "band chart" where the area of the stripes corresponds
# to the area of the land use fraction (this is how HSPF "thinks" about landuse)

cdlextractor.plot_landuse(extracted, sfile, attribute, 
                          output = '{}/{}raw'.format(output, year),
                          datatype = 'raw')
cdlextractor.plot_landuse(extracted, sfile, attribute, 
                          output = '{}/{}results'.format(output, year),
                          datatype = 'results')

# now let's repeat it using the catchment rather than just the boundary

sfile   = 'data/patuxent/catchments'
csvfile = 'catchment_landuse.csv'
landuse = cdlextractor.calculate_landuse(extracted, sfile, aggregate, 
                                         attribute, csvfile = csvfile)

# this WILL take a while (adjust the border linewidth for clarity)

print('this will take a while...\n')

cdlextractor.plot_landuse(extracted, sfile, attribute, lw = 0.1,
                          output = 'catchmentresults'.format(extracted), 
                          datatype = 'results')
