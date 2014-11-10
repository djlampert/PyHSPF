# example08.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Shows how to use the NHDPlusDelineator to delineate the watershed for a point
# within a HUC8. Assumes the NHDPlus Hydrography data exist already. This 
# example delineates the watershed for the location of the discontinued flow
# gage at Hunting Creek, MD.

# pyhspf imports

from pyhspf.preprocessing import NHDPlusDelineator

# paths to the downloaded and extracted files from NHDPlus for the HUC8.
# these are provided with the distribution now, though they can be generated
# from the previous example.

output = 'data/patuxent'

# paths to the different source files for the model data

flowfile  = '{}/flowlines'.format(output)      # HUC8 flowline shapefile
cfile     = '{}/catchments'.format(output)     # HUC8 catchment shapefile
VAAfile   = '{}/flowlineVAAs'.format(output)   # NHDPlus value added attributes
elevfile  = '{}/elevations.tif'.format(output) # NED raster file
watershed = '{}/delineated'.format(output)     # directory for delineated files

# create an instance of the delineator and supply the path to the source files

delineator = NHDPlusDelineator(VAAfile, flowfile, cfile, elevfile)

# longitude, latitude of the point to delineate (the delineator looks for the
# closest flowline to this point)

longitude = -76.6056
latitude  =  38.5839

# extracts the catchments and flowlines for the gage's watershed and merge
# the shapes together to make a boundary file

gagewatershed = '{}/01594670'.format(output)
delineator.delineate_watershed(longitude, latitude, output = gagewatershed)

# make a plot of the watershed

point = longitude, latitude
plot  = 'hunting_watershed'
delineator.plot_delineated_watershed(point = point, output = plot)
