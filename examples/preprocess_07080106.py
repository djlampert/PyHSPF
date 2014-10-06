# !/usr/bin/env python3
#
# preprocess.py
#
# David J. Lampert, PhD, PE
#
# last updated: 10/04/2014
# 
# Purpose: Extracts GIS data from sources and builds the input file for HSPF
# for a given set of assumptions for a watersheds. Details below.

import os

if os.name == 'posix':
    source     = '/media/dave/Data'
    destination = '/home/dave/HSPF_data'
elif os.name == 'nt':
    #source     = 'Z:'
    source     = 'D:'
    source = 'c:/users/dave/desktop'
    destination = 'C:/HSPF_data'

from pyhspf.preprocessing import preprocess

# 8-digit hydrologic unit code of interest; the lists here of states, years,
# and RPUs are just used to point to location of the data files below

VPU       = '07'           # NHDPlus Vector Processing Unit
HUC8   = '07080106'
state = 'ia'
RPU   = '7b'
start = 2001
end   = 2010

########### you shouldn't need to modify anything below this line #############

if __name__ == '__main__': 
    
    preprocess(source, destination, HUC8, state, start, end)
