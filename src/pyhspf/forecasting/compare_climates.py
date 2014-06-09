# compare_climates.py
#
# David J. Lampert
#
# makes some plots from the NRCM runs

import pickle, datetime

from pyhspf.forecasting.forecastplots import plot_climates

def compare_climates(directory, HUC8, start_year, end_year):

    start = datetime.datetime(start_year, 1, 1)
    end   = datetime.datetime(end_year, 1, 1)

    # historical

    p = '{}/{}/watershedtimeseries'.format(directory, HUC8)

    with open('{}/hourlytemperature'.format(p), 'rb') as f: 
        s, t, htemp = pickle.load(f)
    with open('{}/dewpoint'.format(p), 'rb') as f: 
        s, t, hdewpoint = pickle.load(f)
    with open('{}/wind'.format(p), 'rb') as f: 
        s, t, hwind = pickle.load(f)
    with open('{}/hourlysolar'.format(p), 'rb') as f: 
        s, t, hsolar = pickle.load(f)
    with open('{}/hourlyRET'.format(p), 'rb') as f: 
        s, t, hRET = pickle.load(f)

    evaporations = {}

    # simulated

    p = '{}/{}/NRCM/watershedtimeseries'.format(directory, HUC8)

    with open('{}/hourlytemperature'.format(p), 'rb') as f: 
        s, t, stemp = pickle.load(f)
    with open('{}/dewpoint'.format(p), 'rb') as f: 
        s, t, sdewpoint = pickle.load(f)
    with open('{}/wind'.format(p), 'rb') as f: 
        s, t, swind = pickle.load(f)
    with open('{}/hourlysolar'.format(p), 'rb') as f: 
        s, t, ssolar = pickle.load(f)
    with open('{}/hourlyRET'.format(p), 'rb') as f: 
        s, t, sRET = pickle.load(f)

    # convert the solar to the right units
    
    hsolar = [s * 0.024 for s in hsolar]
    ssolar = [s * 0.024 for s in ssolar]

    plot_climates(htemp, stemp, hdewpoint, sdewpoint, hwind, swind, 
                  hsolar, ssolar, hRET, sRET, start, end)

