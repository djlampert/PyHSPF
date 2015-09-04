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
                 destination, 
                 url = 'http://water.usgs.gov/GIS/dsdl',
                 gages = None,
                 ):

        self.url         = url
        self.destination = destination

        if not os.path.isdir(destination):

            print('destination directory does not exist\n')
            
            try: os.mkdir(destination)
            except:
                print('warning, unable to create directory ' +
                      '{}'.format(destination))
                raise

        self.gages = gages

    def report(self, 
               n, 
               block, 
               size,
               ):

        if n % 100 == 0:
            it = block * n / 10**6, size / 10**6
            print('{:.1f} MB of {:.1f} MB transferred'.format(*it))

    def download_metadata(self,
                          webfile = 'USGS_Streamgages-NHD_Locations_Shape.zip', 
                          sfile   = 'USGS_Streamgages-NHD_Locations',
                          verbose = True,
                          ):
        """
        Downloads the source data files.
        """
        
        # directory for output

        if not os.path.isdir(self.destination):

            if verbose:
 
                print(self.destination)
                print('The NWIS metadata are not present in the destination\n')
                os.mkdir(self.destination)

        else: print('NWIS directory {} exists\n'.format(self.destination))

        # source zip file download

        zfile = '{}/{}'.format(self.destination, webfile)
        if not os.path.isfile(zfile):

            url = '{}/{}'.format(self.url, webfile)
            print('the source zip file for the NWIS metadata is not present\n')
            request.urlretrieve(url, zfile, self.report)
            print('')

        elif verbose: 

            print('NWIS source metadata file {} is present\n'.format(zfile))

        # unzip 

        self.NWIS = '{}/{}'.format(self.destination, sfile)
        if not os.path.isfile(self.NWIS + '.shp'):
            
            print('extracting the gage shapefile from the archive')

            zf = zipfile.ZipFile(zfile)
            zf.extractall(self.destination)
            print('')

        elif verbose: print('gage metadata {} is present\n'.format(self.NWIS))

    def extract_HUC8(self, 
                     HUC8, 
                     output, 
                     gagefile = 'gagestations', 
                     verbose = True,
                     ):
        """
        Extracts the USGS gage stations for a watershed from the gage 
        station shapefile into a shapefile for the 8-digit hydrologic unit 
        code of interest. 
        """

        # make sure the metadata exist locally

        self.download_metadata()

        # make sure the output destination exists

        if not os.path.isdir(output): os.mkdir(output)

        sfile = '{}/{}'.format(output, gagefile)
        if not os.path.isfile(sfile + '.shp'):

            # copy the projection

            shutil.copy(self.NWIS + '.prj', sfile + '.prj')

            # read the file

            gagereader  = Reader(self.NWIS, shapeType = 1)
            gagerecords = gagereader.records()

            # pull out the HUC8 record to parse the dataset

            HUC8_index  = gagereader.fields.index(['HUC',  'C', 8, 0]) - 1

            # iterate through the field and find gages in the watershed

            its = HUC8, sfile
            print('extracting gage stations in {} to {}\n'.format(*its))

            gage_indices = []

            i = 0
            for record in gagerecords:
                if record[HUC8_index] == HUC8: gage_indices.append(i)
                i+=1

            # write the data from the HUC8 to a new shapefile

            w = Writer(shapeType = 1)

            for field in gagereader.fields:  w.field(*field)

            for i in gage_indices:
                point = gagereader.shape(i).points[0]
                w.point(*point)
                w.record(*gagerecords[i])

            w.save(sfile)

            if verbose: 

                print('successfully extracted NWIS gage stations\n')

        elif verbose: 

            print('gage station file {} exists\n'.format(sfile))

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

        gagereader = Reader(gagefile, shapeType = 1)

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
                          ):
        """
        Downloads the daily instantaneous flow and water quality data for the 
        given period of time for a particular gage in the metadata.
        """

        if self.gages is None:

            try:

                f = '{}/USGS_Streamgages-NHD_Locations'.format(self.destination)
                self.set_metadata(f)

            except:

                print('error: please specify the path to the metadata')
                raise

        if gageid in self.gages:

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

        else:

            print('error: gageid not in metadata\n')
            raise

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

