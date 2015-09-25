# create_single_gage.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Uses the basemodel from PyHSPF for 07080106 to create a new model with 
# BASINS single gage data from two "met" files. Assumes the PyHSPF derived 
# data have already been generated (see file calibrate_05472500.py)
#
# last updated: 09/21/2015
#

import os, pickle, datetime

from pyhspf import WDMUtil, HSPFModel, Postprocessor

# get the climate time series data from the BASINS database for the Des 
# Moines International Airport

start = datetime.datetime(1980, 1, 1)
end   = datetime.datetime(2009, 1, 1)

# warm up days

warmup = 366

# verbosity of the WDM commands

verbose = False

# met data from BASINS for HUC 07100008, which is adjacent to 07080106 and
# has data from 1981 to 2009 for all major HSPF timeseries

f1 = 'data/met.wdm'

# precipitation data from BASINS for GRINNEL, IA in 07080106

f2 = 'data/grinnel_prec.wdm'

# PyHSPF directory and HUC8

directory = 'C:/HSPF_data'
HUC8      = '07080106'

# path to base model created by PyHSPF to modify with new climate time series

basemodel = '{}/{}/hspf/2001baseline'.format(directory, HUC8)

# path to new model with simplified climate forcing

newmodel = '{}/{}/hspf/simplified'.format(directory, HUC8)

# climate data folders created using PyHSPF climate extraction tools

climatedata = '{}/{}/climate'.format(directory, HUC8)
pdata       = '{}/hourlyprecipitation'.format(climatedata)
edata       = '{}/evapotranspiration'.format(climatedata)
cdata       = '{}/hourly'.format(climatedata)

# NWIS gage identifier for the model

gageid = '05472500'

# path to place model output

output = '{}/{}/simplified/preliminary'.format(directory, HUC8)

if not os.path.isdir('{}/{}/simplified'.format(directory, HUC8)): 
    os.mkdir('{}/{}/simplified'.format(directory, HUC8))
if not os.path.isdir(output): os.mkdir(output)

# make sure the data files exist

for f in (f1, f2, basemodel):

    if not os.path.isfile(f):

        print('error: required file {} does not exist!\n'.format(f))
        raise

# make sure the PyHSPF data have been generated

for d in (climatedata, pdata, edata, cdata):

    if not os.path.isdir(d):

        print('error: required data in {} do not exist!\n'.format(d))
        raise

# use WDMUtil to read the BASINS data

wdm = WDMUtil(verbose = verbose)

# open the precipitation file and the other climate data file

wdm.open(f1, 'r')
wdm.open(f2, 'r')

# make a list of the datasets and numbers

dsns    = wdm.get_datasets(f2)
tstypes = [wdm.get_attribute(f2, n, 'TSTYPE') for n in dsns]

# start date for the BASINS data (after the warmup period)

bstart = start + datetime.timedelta(days = warmup)

# get the precipitation data

i = tstypes.index('PREC')

prec = wdm.get_data(f2, dsns[i], start = bstart, end = end)

# get the potential evapotranspiration and other climate data

dsns    = wdm.get_datasets(f1)
tstypes = [wdm.get_attribute(f1, n, 'TSTYPE') for n in dsns]
dates   = [wdm.get_dates(f1, n)               for n in dsns]
tssteps = [wdm.get_attribute(f1, n, 'TSSTEP') for n in dsns]
tcodes  = [wdm.get_attribute(f1, n, 'TCODE ') for n in dsns]

print('Time series available in WDM file {}:\n'.format(f1))
for n, t, d, tstep, tcode in zip(dsns, tstypes, dates, tssteps, tcodes): 
    s, e = d
    print('{:02d}'.format(n), t, s)

# get the PyHSPF evapotranspiration data

i = tstypes.index('PEVT')
evap = wdm.get_data(f1, dsns[i], start = bstart, end = end)

# get the PyHSPF wind speed data

i = tstypes.index('WIND')
wind = wdm.get_data(f1, dsns[i], start = bstart, end = end)

# get the PyHSPF dewpoint data

i = tstypes.index('DEWP')
dewt = wdm.get_data(f1, dsns[i], start = bstart, end = end)

# get the PyHSPF solar radiation data

i = tstypes.index('SOLR')
solar = wdm.get_data(f1, dsns[i], start = bstart, end = end)

# get the PyHSPF air temperature data

i = tstypes.index('ATEM')
temp = wdm.get_data(f1, dsns[i], start = bstart, end = end)

# close the WDM files

wdm.close(f1)
wdm.close(f2)

# number of years

years = (end - bstart).days / 365.25

# compare BASINS with PyHSPF generated timeseries for 07080106

pfiles = [f for f in os.listdir(pdata)]

pseries = []

for pfile in pfiles:

    with open('{}/{}'.format(pdata, pfile), 'rb') as f:

        s, tstep, data = pickle.load(f)

    pseries.append(data)

pavg = [sum(d) / len(d) for d in zip(*pseries)]

with open('{}/hourlyRET'.format(edata), 'rb') as f:

    s, tstep, eavg = pickle.load(f)

with open('{}/wind'.format(cdata), 'rb') as f: 

    s, tstep, wavg = pickle.load(f)

with open('{}/solar'.format(cdata), 'rb') as f: 

    s, tstep, savg = pickle.load(f)

with open('{}/temperature'.format(cdata), 'rb') as f: 

    s, tstep, tavg = pickle.load(f)

with open('{}/dewpoint'.format(cdata), 'rb') as f: 

    s, tstep, davg = pickle.load(f)

# show the climate time series comparison

print('')

i = sum(prec) / years
print('average BASINS gage precipitation: {:4.1f} in/yr'.format(i))
i = sum(pavg) / len(pavg) / 25.4 * 24 * 365.25
print('average aggregated precipitation:  {:4.1f} in/yr'.format(i))

print('')

i = sum(evap) / years
print('average BASINS gage PET:           {:4.1f} in/yr'.format(i))
i = sum(eavg) / len(eavg) / 25.4 * 24 * 365.25
print('average aggregated PET:            {:4.1f} in'.format(i))

print('')

i = sum(wind) / len(wind)
print('average BASINS gage wind:          {:4.1f} mph'.format(i))
i = sum(wavg) / len(wavg) / 0.3048 / 5280 * 3600
print('average aggregated wind:           {:4.1f} mph'.format(i))

print('')

i = sum(solar) / len(solar)
print('average BASINS gage solar:         {:4.1f} Ly/hr'.format(i))
i = sum(savg) / len(savg) * 0.086
print('average aggregated solar:          {:4.1f} Ly/hr'.format(i))

print('')

i = sum(temp) / len(temp)
print('average BASINS gage temperature:   {:4.1f} F'.format(i))
i = sum(tavg) / len(tavg) * 1.8 + 32
print('average aggregated temperature:    {:4.1f} F'.format(i))

print('')

i = sum(dewt) / len(dewt)
print('average gage dewpoint:             {:4.1f} F'.format(i))
i = sum(davg) / len(davg) * 1.8 + 32
print('average aggregated dewpoint:       {:4.1f} F'.format(i))

print('')

# use climate data from the PyHSPF base model for the warm up period since
# is incomplete (first find the cutoff index for the data)

cutoff = (bstart - start).days * 24

if not os.path.isfile(newmodel):

    with open(basemodel, 'rb') as f: hspfmodel = pickle.load(f)

    # create a new model for the simplified climate data

    print('building a new model with the simplified time series\n')

    simplified = HSPFModel()

    # build new model parameters from the base model; the build_from_existing
    # method can be used to copy the perlnds, implnds, rchreses, special 
    # actions, and reach network from the old file but contains no time series
    # or time series assignments

    simplified.build_from_existing(hspfmodel, newmodel)

    # find the comid of the gage and add the flow data to the new model

    d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
    comid = d[gageid]

    s, tstep, data = hspfmodel.flowgages[gageid]

    simplified.add_timeseries('flowgage', gageid, s, data, tstep = tstep)
    simplified.assign_subbasin_timeseries('flowgage', comid, gageid)

    # add and assign the snowfall and snowdepth from the old model

    s, tstep, data = hspfmodel.snowdepths[HUC8]
    simplified.add_timeseries('snowdepth', HUC8, s, data, tstep = tstep)
    simplified.assign_watershed_timeseries('snowdepth', HUC8)

    s, tstep, data = hspfmodel.snowfalls[HUC8]
    simplified.add_timeseries('snowfall', HUC8, s, data, tstep = tstep)
    simplified.assign_watershed_timeseries('snowfall', HUC8)

    # add other time series to the model and assign them to the whole watershed
    # note that the first year from the PyHSPF data is used for the warmup
    # since the BASINS station data are incomplete

    name = 'precipitation'
    data = [p for p in pavg[:cutoff]] + [p * 25.4 for p in prec]
    simplified.add_timeseries(name, 'simplified', start, data)
    simplified.assign_watershed_timeseries(name, 'simplified')

    name = 'evaporation'
    data = [e for e in eavg[:cutoff]] + [e * 25.4 for e in evap]
    simplified.add_timeseries(name, 'simplified', start, data)
    simplified.assign_watershed_timeseries(name, 'simplified')

    name = 'wind'
    data = wavg[:cutoff] + [w * 5280 * 0.3048 / 1000 for w in wind]
    simplified.add_timeseries(name, 'simplified', start, data)
    simplified.assign_watershed_timeseries(name, 'simplified')

    name = 'solar'
    data = [s for s in savg[:cutoff]] + [s for s in solar]
    simplified.add_timeseries(name, 'simplified', start, data)
    simplified.assign_watershed_timeseries(name, 'simplified')

    name = 'temperature'
    data = [t for t in tavg[:cutoff]] + [(t - 32) * 5 / 9 for t in temp]
    simplified.add_timeseries(name, 'simplified', start, data)
    simplified.assign_watershed_timeseries(name, 'simplified')

    name = 'dewpoint'
    data = [d for d in davg[:cutoff]] + [(t - 32) * 5 / 9 for t in dewt]
    simplified.add_timeseries(name, 'simplified', start, data)
    simplified.assign_watershed_timeseries(name, 'simplified')

    # save the model for later

    with open(newmodel, 'wb') as f: pickle.dump(simplified, f)

else:

    with open(newmodel, 'rb') as f: simplified = pickle.load(f)

    d = {v:k for k, v in simplified.subbasin_timeseries['flowgage'].items()}
    comid = d[gageid]

# change the filename

its = directory, HUC8, gageid
simplified.filename = '{}/{}/simplified/preliminary/{}'.format(*its)

# build the input WDM file

simplified.build_wdminfile()

# output variables

targets = ['water_state', 
           'reach_outvolume', 
           'evaporation', 
           'runoff', 
           'groundwater',
           'snow_state', 
           'snowpack', 
           'snowfall',
           ]

# build the UCI and output WDM files

simplified.build_uci(targets, start, end, atemp = True, snow = True,
                     hydrology = True)

# run it

simplified.run(verbose = True)

# use the Postprocessor to analyze and save the results to the folder

dates = start + datetime.timedelta(days = warmup), end

postprocessor = Postprocessor(simplified, dates, comid = comid)

postprocessor.get_hspexp_parameters(verbose = False)
postprocessor.plot_hydrograph(tstep = 'monthly', show = False,
                              output = '{}/hydrography'.format(output))
postprocessor.plot_calibration(output = '{}/statistics'.format(output),
                               show = False)
postprocessor.plot_runoff(tstep = 'daily', show = False,
                          output = '{}/runoff'.format(output))
report = '{}/output_report.csv'.format(output)
postprocessor.calibration_report(output = report)
postprocessor.plot_dayofyear(output = '{}/dayofyear'.format(output),
                             show = False)
postprocessor.plot_storms(season = 'all', show = False, 
                          output = '{}/storms'.format(output))
postprocessor.plot_snow(output = '{}/snow'.format(output), 
                        show = False)
