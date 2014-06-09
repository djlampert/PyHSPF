#!/usr/bin/env python3
#
# File: make_gages.py
#
# by David J. Lampert, PhD, PE (djlampert@gmail.com)
#
# Last updated: 4/12/2013
#
# Purpose: imports gage files to Python pickled files for easy access to data
#

import os, pickle

from shapefile import Reader

from pyhspf.preprocessing.gagestation import GageStation

def make_gagestations(directory, HUC8, state, start, end,
                      verbose = True, vverbose = False):
    """Makes pickled instances of the GageStation class for all the gages
    meeting the calibration criteria for an 8-digit watershed."""

    # paths for the watershed shapefiles

    outletfile = '{0}/{1}/{1}subbasin_outlets'.format(directory, HUC8)
    gagefile   = '{0}/{1}/{1}gagestations'.format(directory, HUC8)

    # make a folder for the files

    if not os.path.isdir('{0}/{1}/NWIS'.format(directory, HUC8)): 
        os.mkdir('{0}/{1}/NWIS'.format(directory, HUC8))

    # check the gages and see if they meet the criteria for outlets

    gagereader  = Reader(gagefile, shapeType = 1)
    gagerecords = gagereader.records()

    # figure out which field codes are the HUC8, the first day, the site
    # number, the drainage area, and the average 

    day1_index  = gagereader.fields.index(['DAY1',       'N', 19, 0]) - 1
    dayn_index  = gagereader.fields.index(['DAYN',       'N', 19, 0]) - 1
    drain_index = gagereader.fields.index(['DA_SQ_MILE', 'N', 19, 2]) - 1
    HUC8_index  = gagereader.fields.index(['HUC',        'C',  8, 0]) - 1
    site_index  = gagereader.fields.index(['SITE_NO',    'C', 15, 0]) - 1
    nwis_index  = gagereader.fields.index(['NWISWEB',    'C', 75, 0]) - 1
    ave_index   = gagereader.fields.index(['AVE',        'N', 19, 3]) - 1

    gages  = [r[site_index]  for r in gagerecords]
    day1s  = [r[day1_index]  for r in gagerecords]
    dayns  = [r[dayn_index]  for r in gagerecords]
    drains = [r[drain_index] for r in gagerecords]
    webs   = [r[nwis_index]  for r in gagerecords]
    aves   = [r[ave_index]   for r in gagerecords]

    # open the outlet file and find the gages to be used for calibration

    outletreader  = Reader(outletfile, shapeType = 1)
    outletrecords = outletreader.records()

    comid_index = outletreader.fields.index(['COMID',     'N',  9, 0]) - 1
    nwis_index  = outletreader.fields.index(['SITE_NO',   'C', 15, 0]) - 1
    gnis_index  = outletreader.fields.index(['GNIS_NAME', 'C', 65, 0]) - 1

    comids  = ['{}'.format(record[comid_index]) for record in outletrecords]
    nwisids = [record[nwis_index]  for record in outletrecords]
    gnisids = [record[gnis_index]  for record in outletrecords]

    # iterate through all the gages

    nwisids = [n if isinstance(n, str) else '' for n in nwisids]

    for gage, day1, dayn, drain, web, ave in zip(gages, day1s, dayns, drains,
                                                 webs, aves):

        # filename to use for data

        filename = '{}/{}/NWIS/{}'.format(directory, HUC8, gage)

        if not os.path.exists(filename) and gage in nwisids:

            # find the comid and gnis index

            comid = comids[nwisids.index(gage)]
            gnis  = gnisids[nwisids.index(gage)]

            if verbose: 
                print('creating gage station ' +
                      '{} at comid {}\n'.format(gage, comid))

            gagestation = GageStation(gage, comid, state, gnis, day1, dayn,
                                      drain, ave, web)

            # download the daily discharge data

            gagestation.download_daily_discharge(start, end)

            # download the water quality data

            gagestation.download_water_quality()

            # save for later

            with open(filename, 'wb') as f: pickle.dump(gagestation, f)

        elif vverbose: print('excluding gage {}\n'.format(gage))

#directory = 'C:/HSPF_data'
#HUC8      = '07080106'
#gageid    = '05472500'

#HUC8      = '07100008'
#gageid    = '05487470'
#gageid    = '05485640'

#HUC8 = '07130004'

#make_gagestations(directory, HUC8)
