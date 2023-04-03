# nwisextractor.py
#
# David J. Lampert (djlampert@gmail.com)
#
# Last updated: 08/31/2015
#
# Contains the NWISExtractor class that can be used to download and import
# data on daily discharge, instantaneous streamflow, field stage-discharge
# estimates, and water quality from the National Water Information Service
# (NWIS).

import shutil, os, pickle, zipfile, datetime
import dataretrieval.nwis as nwis
from shapefile import Reader, Writer
from urllib    import request

from .gagestation import GageStation

class NWISExtractor:
    """
    A class to download the NWIS station source metadata for the USGS daily
    discharge stations, and then retrieve flow and water quality data
    for a particular watershed (8-digit HUC).
    """

    def __init__(self,
                 gages = None,
                 ):

        self.gages = gages

    def report(self,
               n,
               block,
               size,
               ):

        if n % 100 == 0:
            it = block * n / 10**6, size / 10**6
            print('{:.1f} MB of {:.1f} MB transferred'.format(*it))


    def extract_HUC8(self,
                     HUC8,
                     output,
                     gagefile = 'gagestations',
                     srs_id = 1645423,
                     stat_cd = 3,
                     verbose = True,
                     ):
        """
        Extracts the USGS gage stations for a watershed from the gage
        station shapefile into a shapefile for the 8-digit hydrologic unit
        code of interest.

        srs_id 1645423 is the EPA ID for daily mean stream flow and
        stat_cd 3 is the USGS code for mean values. This should filter
        for unique sites with daily mean flow, but could be adjusted if
        needed in special cases.
        stat codes can be found here: https://help.waterdata.usgs.gov/stat_code
        srs ID's can be looked up here:
        https://ofmpub.epa.gov/sor_internet/registry/substreg/LandingPage.do
        """

        # make sure the output destination exists

        if not os.path.isdir(output): os.mkdir(output)

        sfile = '{}/{}'.format(output, gagefile)

        # get a list of all the sites in the HUC
        sites = nwis.what_sites(huc=HUC8,seriesCatalogOutput='true')

        # filter the list for sites with daily mean stream flow only
        mask = (sites[0]['srs_id']==srs_id) & (sites[0]['stat_cd']==stat_cd)
        sites = sites[0].loc[mask]
        # just in case we also drop duplicates
        sites.drop_duplicates(subset='site_no', keep='first', inplace=True)
        sites.reset_index(inplace=True)

        # get the drainage area and state
        siteids = sites['site_no'].to_list()
        info = nwis.get_info(sites=siteids)
        sites['drain_area_va'] = info[0]['drain_area_va']
        sites['state_cd'] = info[0]['state_cd']

        # get mean flow
        stats = nwis.get_stats(sites=siteids,statReportType='annual',statTypeCd='mean',parameterCd='00060')
        for sid in siteids:
            # get_stats gives us the annual mean for every year reported
            # take the mean of the annual values to get the overall mean
            mean = stats[0].loc[stats[0]['site_no']==sid,'mean_va'].mean()
            sites.loc[sites['site_no']==sid,'mean_flow'] = mean

        # create a shapefile of the sites
        w = Writer(sfile, shapeType = 1)

        # add the relevant fields
        w.field('DAY1','N',19,0)
        w.field('DAYN','N',19,0)
        w.field('DA_SQ_MILE','N',19,2)
        w.field('HUC','C',8,0)
        w.field('STATE','C',2,0)
        w.field('SITE_NO','C',15,0)
        w.field('NWISWEB','C',75,0)
        w.field('AVE','N',19,3)
        w.field('STATION_NM','C',60,0)

        # add a point shape and record for each of the sites
        for sid in siteids:
            # PyHSPF expects the dates to be integers in the format
            # of YYYYMMDD so we do this little dance to convert it
            day1 = sites.loc[sites['site_no']==sid,'begin_date'].item()
            day1dt = datetime.datetime.strptime(day1,'%Y-%m-%d')
            day1 = int(day1dt.strftime('%Y%m%d'))
            dayn = sites.loc[sites['site_no']==sid,'end_date'].item()
            dayndt = datetime.datetime.strptime(dayn,'%Y-%m-%d')
            dayn = int(dayndt.strftime('%Y%m%d'))
            da = int(sites.loc[sites['site_no']==sid,'drain_area_va'].item())
            huc = str(sites.loc[sites['site_no']==sid,'huc_cd'].item())
            ave = sites.loc[sites['site_no']==sid,'mean_flow'].item()
            name = sites.loc[sites['site_no']==sid,'station_nm'].item()
            lat = sites.loc[sites['site_no']==sid,'dec_lat_va'].item()
            lon = sites.loc[sites['site_no']==sid,'dec_long_va'].item()
            # TODO: fix this
            state = 'OK'
            # this field is never actually used
            web = ''
            w.record(day1,dayn,da,huc,state,sid,web,ave,name)
            w.point(lon,lat)

        w.close()
        self.set_metadata(sfile)

    def set_metadata(self,
                     gagefile,
                     ):
        """
        Opens the gage file with the station metadata.
        """

        # metadata for stations

        self.gages  = []
        self.day1s  = []
        self.dayns  = []
        self.drains = []
        self.states = []
        self.sites  = []
        self.nwiss  = []
        self.aves   = []
        self.names  = []

        gagereader = Reader(gagefile)

        # get the fields with pertinent info

        day1_index  = gagereader.fields.index(['DAY1',       'N', 19, 0]) - 1
        dayn_index  = gagereader.fields.index(['DAYN',       'N', 19, 0]) - 1
        drain_index = gagereader.fields.index(['DA_SQ_MILE', 'N', 19, 2]) - 1
        HUC8_index  = gagereader.fields.index(['HUC',        'C',  8, 0]) - 1
        state_index = gagereader.fields.index(['STATE',      'C',  2, 0]) - 1
        site_index  = gagereader.fields.index(['SITE_NO',    'C', 15, 0]) - 1
        nwis_index  = gagereader.fields.index(['NWISWEB',    'C', 75, 0]) - 1
        ave_index   = gagereader.fields.index(['AVE',        'N', 19, 3]) - 1
        name_index  = gagereader.fields.index(['STATION_NM', 'C', 60, 0]) - 1

        # iterate through the records

        for r in gagereader.records():

            gage  = r[site_index]
            day1  = r[day1_index]
            dayn  = r[dayn_index]
            drain = r[drain_index]
            state = r[state_index]
            nwis  = r[nwis_index]
            ave   = r[ave_index]
            name  = r[name_index]
            site  = r[site_index]

            self.gages.append(gage)
            self.day1s.append(day1)
            self.dayns.append(dayn)
            self.drains.append(drain)
            self.states.append(state)
            self.sites.append(site)
            self.nwiss.append(nwis)
            self.aves.append(ave)
            self.names.append(name)

    def download_gagedata(self,
                          gageid,
                          start,
                          end,
                          output = None,
                          plot = True,
                          srs_id = 1645423,
                          stat_cd = 3,
                          ):
        """
        Downloads the daily instantaneous flow and water quality data for the
        given period of time for a particular gage in the metadata.
        """
        gage  = None
        day1  = None
        dayn  = None
        drain = None
        state = None
        nwis  = ''
        ave   = None
        name  = None
        # if the gage is not already in our metadata, the retrieve the
        # required site info
        if not gageid in self.gages:
            site = nwis.what_sites(sites=gageid,seriesCatalogOutput='true')
            # if the site doesn't exist, dataretrieval returns an empty dataframe
            if site[0].empty:
                print('error: could not find gageid\n')
                raise ValueError
                
            # filter the list for sites with daily mean stream flow only
            mask = (site[0]['srs_id']==srs_id) & (site[0]['stat_cd']==stat_cd)
            site = site[0].loc[mask]
            # just in case we also drop duplicates
            site.drop_duplicates(subset='site_no', keep='first', inplace=True)
            site.reset_index(inplace=True)

            day1 = sites.loc[0,'begin_date']
            day1dt = datetime.datetime.strptime(day1,'%Y-%m-%d')
            day1 = int(day1dt.strftime('%Y%m%d'))
            dayn = sites.loc[0,'end_date']
            dayndt = datetime.datetime.strptime(dayn,'%Y-%m-%d')
            name = sites.loc[0,'station_nm']
            # get the drainage area and state
            info = nwis.get_info(sites=gageid)
            drain = info[0].loc[0,'drain_area_va']
            # TODO: fix
            state = 'OK'

            # get mean flow
            stats = nwis.get_stats(sites=gageid,statReportType='annual',statTypeCd='mean',parameterCd='00060')
            ave = stats[0].loc[stats[0]['site_no']==gageid,'mean_va'].mean()

        else:
            # if we already have the site info just pull it from our metadata
            i = self.gages.index(gageid)

            gage  = self.gages[i]
            day1  = self.day1s[i]
            dayn  = self.dayns[i]
            drain = self.drains[i]
            state = self.states[i]
            nwis  = self.nwiss[i]
            ave   = self.aves[i]
            name  = self.names[i]

        if not os.path.exists(output):

            gagestation = GageStation(gage, name, state, day1, dayn,
                                      drain, ave, nwis)

            # download the daily discharge data if it exists

            try:

                gagestation.download_daily_discharge(start, end)

                # download the stage-discharge measurements

                gagestation.download_measurements()

                # make a plot of the data

                if plot: gagestation.plot(output)

                # download the water quality data if it exists

                try: gagestation.download_water_quality()
                except:
                    print('warning, unable to download water quality ' +
                          'data for {}\n'.format(gage))

                # download the instantaneous flow data if it exists

                gagestation.download_instant_flows(start, end)

                # save for later

                with open(output, 'wb') as f: pickle.dump(gagestation, f)

            except:

                print('warning, unable to download daily flow data ' +
                      'for {}\n'.format(gage))

        else: print('gage data for {} exist\n'.format(gage))


    def download_all(self,
                     start,
                     end,
                     output = None,
                     plot = True,
                     ):
        """
        Downloads the daily flow data for the given period of time for
        all gages in the metadata.
        """

        if output is None: output = 'gagestations'
        if not os.path.isdir(output): os.mkdir(output)

        # iterate through the records

        iters = zip(self.gages, self.day1s, self.dayns, self.drains,
                    self.states, self.nwiss, self.aves, self.names)

        for gage, day1, dayn, drain, state, nwis, ave, name in iters:

            s = float('{}'.format(dayn)[:4])
            print('start {} s {}'.format(start.year,s))
            if start.year <= s:

                # filename to use for data

                filename = '{}/{}'.format(output, gage)

                if not os.path.exists(filename):

                    gagestation = GageStation(gage, name, state, day1, dayn,
                                              drain, ave, nwis)

                    # download the daily discharge data if it exists

                    try:

                        gagestation.download_daily_discharge(start, end)

                        # download the stage-discharge measurements

                        gagestation.download_measurements()

                        # make a plot of the data

                        if plot: gagestation.plot(filename)

                        # download the water quality data if it exists

                        try: gagestation.download_water_quality()
                        except:
                            print('warning, unable to download water quality ' +
                                  'data for {}\n'.format(gage))

                        # download the instantaneous flow data if it exists

                        gagestation.download_instant_flows(start, end)

                        # save for later

                        with open(filename, 'wb') as f:
                            pickle.dump(gagestation, f)

                    except: pass

            else:

                its = gage, start, end
                print('no data available for station ' +
                      '{} for years {:%Y}-{:%Y}\n'.format(*its))
