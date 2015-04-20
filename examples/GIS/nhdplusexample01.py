# nhdplusexample01.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Illustrates how to extract data from the NHDPlus website for the Middle 
# Atlantic Region (VPU 02), then extracts data for Hydrologic Unit Code (HUC) 
# 02060006 (Patuxent River Basin). This takes a while since the source files 
# are huge. If the source files are present the extractor will not download 
# them; thus this could very easily be adapted to look at other HUC8s within
# a given VPU once the source data have been downloaded. The extractor will 
# merge all the catchments together into a new shapefile for the boundary 
# which also takes a while (~1 minute).

import os

from pyhspf.preprocessing import NHDPlusExtractor

# local directory for NHDPlus source data files either to download or the 
# existing location; modified as needed

NHDPlus = 'NHDPlus'

# make a working directory for HSPF data files

output = 'HSPF_data'
if not os.path.isdir(output): os.mkdir(output)

# HUC8 NHDPlus info

VPU     = '02'        # NHDPlus Vector Processing Unit
HUC8    = '02060006'  # 8-digit HUC

# path for the output files

HUC8output = '{}/{}'.format(output, HUC8)

# create an instance of the NHDPlus extractor

nhdplusextractor = NHDPlusExtractor(VPU, NHDPlus)

# download and decompress the source data. worth noting--if you point the 
# extractor to the source files, it will skip the (lengthy) download.

nhdplusextractor.download_data()

# extract the HUC8 data for the Patuxent watershed. same thing here--if any of
# these files already exist the extraction step will be skipped.

nhdplusextractor.extract_HUC8(HUC8, HUC8output)
