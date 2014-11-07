# example07.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Illustrates how to extract data from the NHDPlus website for the Middle 
# Atlantic Region (drainid MA, VPU 02), then extract data for Hydrologic
# Unit Code (HUC) 02060006 (Patuxent River Basin). This takes a while since
# the source files are huge. If the source files are present the extractor
# will not download them; thus this could very easily be adapted to look at
# other HUC8s. The extractor will merge all the catchments together into
# a new shapefile for the boundary which takes a while (~1 minute).

from pyhspf.preprocessing import NHDPlusExtractor

# paths for NHDPlus source data files (modified as needed)

NHDPlus = 'NHDPlus'

# path for HUC8 output (modify as needed)

output = 'HSPF_data'

# HUC8 NHDPlus info (get from Horizon Systems website)

VPU     = '02'        # NHDPlus Vector Processing Unit
HUC8    = '02060006'  # 8-digit HUC

# paths for the extracted files for the HUC8 (this represents the "output")

flowfile  = '{}/flowlines'.format(output)      # HUC8 flowline shapefile
cfile     = '{}/catchments'.format(output)     # HUC8 catchment shapefile
bfile     = '{}/boundary'.format(output)       # HUC8 boundary shapefile
VAAfile   = '{}/flowlineVAAs'.format(output)   # NHDPlus value added attributes
elevfile  = '{}/elevations.tif'.format(output) # NED raster file
waterplot = '{}/watershed.png'.format(output)  # plot of the data

# title for the plot

title  = ('Cataloging Unit {}\n'.format(HUC8) +
          'NHDPlus Catchments and Flowlines on 30 meter NED DEM')

# create an instance of the NHDPlus extractor

nhdplusextractor = NHDPlusExtractor(VPU, NHDPlus)

# download and decompress the source data

nhdplusextractor.download_data()

# extract the HUC8 data for the Patuxent watershed

nhdplusextractor.extract_HUC8(HUC8, output)
