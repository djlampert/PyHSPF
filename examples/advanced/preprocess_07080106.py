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
    source     = '/media/dave/DATA'
    destination = '/home/dave/HSPF_data'
elif os.name == 'nt':
    #source     = 'Z:'
    source     = 'D:'
    source = 'c:/users/dave/desktop'
    destination = 'C:/HSPF_data'

from pyhspf.preprocessing import Preprocessor

# 8-digit hydrologic unit code of interest; the lists here of states, years,
# and RPUs are just used to point to location of the data files below

HUC8   = '07080106'
state = 'ia'
start = 2001
end   = 2010
landcodes = 'aggregate.csv'

########### you shouldn't need to modify anything below this line #############

if __name__ == '__main__': 
    
    processor = Preprocessor(source, destination, landcodes = landcodes)

    processor.preprocess(HUC8, state, start, end)
