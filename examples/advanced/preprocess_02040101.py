# preprocess_02040101.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 09/17/2015
# 
# Purpose: Extracts GIS data from sources and builds the input file for HSPF
# for a given set of assumptions for HUC 02040101, the Delaware River.

import os, datetime

source      = 'Z:'
destination = 'C:/HSPF_data'

from pyhspf.preprocessing import Preprocessor

# 8-digit hydrologic unit code of interest; the lists here of states, years,
# and RPUs are just used to point to location of the data files below

HUC8        = '02040101'
start       = datetime.datetime(1980, 1, 1)
end         = datetime.datetime(2011, 1, 1)
drainmax    = 400
aggregation = 'cdlaggregation.csv'
landuse     = 'lucs.csv'

if __name__ == '__main__': 
    
    processor = Preprocessor()

    processor.set_network(source)
    processor.set_output(destination)
    processor.set_parameters(HUC8 = HUC8,
                             start = start,
                             end = end,
                             cdlaggregate = aggregation,
                             landuse = landuse)
    processor.preprocess(drainmax = drainmax, parallel = False)

# this took about 40 minutes to run on my 3 year old laptop not counting the
# time to download the raw data from the NHDPlus and CDL
