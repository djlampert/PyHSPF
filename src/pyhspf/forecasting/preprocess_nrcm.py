# extract_HUC8_nrcm.py
# David J. Lampert
# 
# extracts the grid point for a watershed from the preprocessed NRCM data

import pickle, datetime

from pyhspf.forecasting.extract_NRCM    import extract_raw
from pyhspf.forecasting.extract_NRCM    import extract_timeseries
from pyhspf.forecasting.make_timeseries import make_timeseries

def preprocess_NRCM(source, destination, HUC8, start, end, plot = True):

    # extract the raw gridpoint files

    extract_raw(source, destination, HUC8, plot = plot, show = False)

    # make raw timeseries

    s = datetime.datetime(start, 1, 1)
    e = datetime.datetime(end,   1, 1)

    d = '{}/{}/NRCM'.format(destination, HUC8)

    extract_timeseries(d, s, e)

    # aggregate the raw timeseries

    make_timeseries(destination, HUC8, start, end)
