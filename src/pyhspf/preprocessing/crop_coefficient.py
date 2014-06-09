# crop_coefficient.py
#
# David J. Lampert, PhD, PE
#
# last updated: 12/04/2013
#
# estimates the reference PET and precipitation for a watershed from 
# climate data

import numpy as np, datetime, os, io, pickle, math

from pyhspf.preprocessing.climateplots import plot_dailyET, plot_dayofyearET
from pyhspf.preprocessing.climateplots import plot_hourlyET

def crop_coefficient(Ki, Km, Kl, Li, Lg, Lm, Ll, mo, da, year, tstep):
    """Creates a time series of daily values for the crop coefficient using
    the Food and Agriculture Organization (FAO) "single coefficient" method."""

    if tstep == 'hourly': 
        delta = datetime.timedelta(hours = 1)
        f     = 24
    elif tstep == 'daily':  
        delta = datetime.timedelta(days = 1)
        f     = 1
    else:
        print('warning: unknown time step specified')
        raise

    # keep track of the daily coefficients in a list

    K = []
        
    # use Ki until the after emergence (plant date + initial development)

    emergence = (datetime.datetime(year, mo, da) + 
                 datetime.timedelta(days = Li))

    date  = datetime.datetime(year, 1, 1)
    while date < emergence:
        K.append(Ki)
        date += delta

    # scale up K to Km linearly during the growth phase

    growth = emergence + datetime.timedelta(days = Lg)
    Kslope = (Km - Ki) / Lg / f

    while date < growth:
        K.append(K[-1] + Kslope)
        date += delta

    # append Km through the full growth period

    full = growth + datetime.timedelta(days = Lm)
        
    while date < full:
        K.append(Km)
        date += delta

    # scale down K to Kl through the late growth period (until harvest)

    harvest = full + datetime.timedelta(days = Ll)
    Kslope = (Kl - Km) / Ll / f

    while date < harvest:
        K.append(K[-1] + Kslope)
        date += delta

    # use Ki after the harvest

    while date < datetime.datetime(year + 1, 1, 1):
        K.append(Ki)
        date += delta

    return K

def cropK_timeseries(crop, start, end, tstep = 'hourly'):
    """Returns a daily timeseries for the crop coefficient for the years 
    specified. Append the crop info as needed.
    """

    # crop coefficients during various growth stages as defined by FAO Table 12

    #crop_coefficients = {'Corn':        [0.30, 1.15, 0.40], # cereals
    #                     'Soybeans':    [0.40, 1.15, 0.55], # legumes
    #                     'Other grain': [0.30, 1.15, 0.40], # cereals
    #                     'Alfalfa':     [0.30, 0.95, 0.90], # avg cutting
    #                     'Fallow':      [0.30, 0.30, 0.30], # always 0.3
    #                     'Pasture':     [0.30, 0.85, 0.30], # rotated grazing
    #                     'Wetlands':    [1.00, 1.20, 1.00], # reed swamp
    #                     'Forest':      [1.00, 1.00, 1.00], # always 1.0
    #                     'Developed':   [1.00, 1.00, 1.00], # always 1.0
    #                     'Other':       [1.00, 1.00, 1.00], # always 1.0
    #                     }
    crop_coefficients = {'cereals':     [0.30, 1.15, 0.40], # cereals
                         'legumes':     [0.40, 1.15, 0.55], # legumes
                         'alfalfa':     [0.30, 0.95, 0.90], # avg cutting
                         'fallow':      [0.30, 0.30, 0.30], # always 0.3
                         'pasture':     [0.30, 0.85, 0.30], # rotated grazing
                         'wetlands':    [1.00, 1.20, 1.00], # reed swamp
                         'others':      [1.00, 1.00, 1.00], # always 1.0
                         }

    # growth stages lengths (days) for crop as defined by FAO Table 25

    #growth_lengths = {'Corn':        [30, 50,  60, 40], # as above
    #                  'Soybeans':    [20, 30,  60, 30], # as above
    #                  'Other grain': [20, 30,  60, 40], # small grains
    #                  'Alfalfa':     [10, 10, 120, 10], # irrelevant
    #                  'Fallow':      [10, 10, 240, 10], # irrelevant
    #                  'Pasture':     [10, 10, 240, 10], # as above
    #                  'Wetlands':    [10, 10, 240, 10], # irrelevant
    #                  'Forest':      [10, 10, 240, 10], # irrelevant
    #                  'Developed':   [10, 10, 240, 10], # irrelevant
    #                  'Other':       [10, 10, 240, 10], # irrelevant
    #                  }
    growth_lengths = {'cereals':     [30, 50,  60, 40], # as above
                      'legumes':     [20, 30,  60, 30], # as above
                      'alfalfa':     [10, 10, 120, 10], # as above
                      'fallow':      [10, 10, 240, 10], # irrelevant
                      'pasture':     [10, 10, 240, 10], # irrelevant
                      'wetlands':    [10, 10, 240, 10], # irrelevant
                      'others':      [10, 10, 240, 10], # irrelevant
                      }

    # crop plant dates for Iowa (month, day)

    #plant_dates = {'Corn':        [4, 15],
    #               'Soybeans':    [5, 15],
    #               'Other grain': [4, 15],
    #               'Alfalfa':     [5, 15],
    #               'Fallow':      [3, 1],
    #               'Pasture':     [3, 1],
    #               'Wetlands':    [3, 1],
    #               'Forest':      [3, 1],
    #               'Developed':   [3, 1],
    #               'Other':       [3, 1],
    #               }
    plant_dates = {'cereals':     [4, 15],
                   'legumes':     [5, 15],
                   'alfalfa':     [5, 15],
                   'fallow':      [3, 1],
                   'pasture':     [3, 1],
                   'wetlands':    [3, 1],
                   'others':      [3, 1],
                   }

    Ki, Km, Kl     = crop_coefficients[crop]
    Li, Lg, Lm, Ll = growth_lengths[crop]
    mo, da         = plant_dates[crop]
    
    K = []
    for year in range(start, end):
        K += crop_coefficient(Ki, Km, Kl, Li, Lg, Lm, Ll, mo, da, year, tstep)

    return K

def calculate_cropPET(directory, HUC8, start, end, tstep = 'hourly', 
                      output = None, evaporations = True, plot = True):

    #crops = ['Corn', 'Soybeans', 'Pasture', 'Fallow', 'Forest', 
    #         'Other grain', 'Alfalfa', 'Wetlands', 'Developed', 'Other']
    crops = ['cereals', 'legumes', 'alfalfa', 'fallow', 'pasture',
             'wetlands', 'others']

    if output is None: p = '{}/{}/watershedtimeseries'.format(directory, HUC8)
    else:              p = output

    st, en = start.year, end.year

    if tstep == 'daily':

        # open the reference ET

        dailyRET = '{}/dailyRET'.format(p)
        with open(dailyRET, 'rb') as f: s, t, RET = pickle.load(f)

        RET = np.array(RET)

        # go through the crops and make the PET timeseries based on RET and K

        PETs = [RET * np.array(cropK_timeseries(crop, st, en, tstep = 'daily'))
                for crop in crops]

        plotfile = '{}/dailycropPET'.format(p)

        # make plot of the time series and day of year averages

        if plot:
            with open('{}/tmin'.format(p), 'rb') as f: 
                s, t, tmin = pickle.load(f)
            with open('{}/tmax'.format(p), 'rb') as f: 
                s, t, tmax = pickle.load(f)
            with open('{}/dewpoint'.format(p), 'rb') as f: 
                s, t, dewpoint = pickle.load(f)
            with open('{}/wind'.format(p), 'rb') as f: 
                s, t, wind = pickle.load(f)
            with open('{}/dailysolar'.format(p), 'rb') as f: 
                s, t, solar = pickle.load(f)
            
            if evaporations:
                v = directory, HUC8
                evapfile = '{}/{}/evaporations/evaporation'.format(*v)
                with open(evapfile, 'rb') as f: evaporations = pickle.load(f)
            else: evaporations = {}

            # Watts/m2 to kW hr/m2

            solar = [s * 0.024 for s in solar]

            plot_dayofyearET(HUC8, start, end, evaporations, PETs, tmin, tmax, 
                             dewpoint, wind, solar, labels = crops, 
                             output = plotfile)

    elif tstep == 'hourly':

        hourlyRET = '{}/hourlyRET'.format(p)
        with open(hourlyRET, 'rb') as f: s, t, RET = pickle.load(f)

        RET = np.array(RET)

        # make a list of timeseries of the values

        PETs = {crop: (start, 60, RET * np.array(cropK_timeseries(crop, st,en)))
                for crop in crops}

        hourlyPETs = '{}/hourlyPETs'.format(p)
        with open(hourlyPETs, 'wb') as f: pickle.dump(PETs, f)

        if plot:

            PETs   = [PETs[crop][2] for crop in crops]

            with open('{}/hourlytemperature'.format(p), 'rb') as f: 
                s, t, temp = pickle.load(f)
            with open('{}/hourlysolar'.format(p), 'rb') as f: 
                s, t, solar = pickle.load(f)
            with open('{}/dewpoint'.format(p), 'rb') as f: 
                s, t, dewpoint = pickle.load(f)
            with open('{}/wind'.format(p), 'rb') as f: 
                s, t, wind = pickle.load(f)

            if evaporations:
                v = directory, HUC8
                evapfile = '{}/{}/evaporations/evaporation'.format(*v)
                with open(evapfile, 'rb') as f: evaporations = pickle.load(f)
            else: evaporations = {}

            # Watts/m2 to kW hr/m2

            solar = [s * 0.024 for s in solar]

            plot_hourlyET(HUC8, start, end, evaporations, PETs, temp, dewpoint, 
                          wind, solar, labels = crops, output = hourlyPETs)

    else: 
        print('warning: unknown time step specified\n')
        raise

#directory = 'c:/hspf_data'
#HUC8      = '07080106'
#start     = datetime.datetime(2001, 1, 1)
#end       = datetime.datetime(2010, 1, 1)

#calculate_cropPET(directory, HUC8, start, end)
