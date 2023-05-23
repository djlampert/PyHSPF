# nidextractor.py
#
# David J. Lampert, PhD, PE
# last uptdated: 04/03/2023
# Purpose: Contains the NIDExtractor class to download data from the National
# Inventory of Dams and then extract the dams for a given HUC8.

import os, shutil, subprocess
import pandas as pd
from urllib    import request
from shapefile import Reader, Writer

class NIDExtractor:

    def __init__(self, 
                 NID, 
                 website = 'https://nid.usace.army.mil/api/nation/csv',
                 ):

        self.NID     = NID
        self.website = website

        if not os.path.isdir(NID):

            print('destination directory {} does not exist\n'.format(NID))
            
            try: os.mkdir(NID)
            except:
                print('warning, unable to create directory {}'.format(NID))
                raise

    def inside_box(self, p1, p2, p3):
        """Checks if p3 is inside a box formed by p1 and p2."""

        if (p1[0] < p3[0] and p3[0] < p2[0] or 
            p1[0] > p3[0] and p3[0] > p2[0]):

            # x value is inside

            if (p1[1] < p3[1] and p3[1] < p2[1] or 
                p1[1] > p3[1] and p3[1] > p2[1]):
            
                # y value is inside

                return True

            else: return False

        else: return False

    def generate_shape(self, 
                            csv_name  = 'nation.csv',
                            shapefile = 'dams00x020',
                            height    = 50,      # ft
                            storage   = 5000,    # acre-feet
                            maxstore  = 25000,   # acre-feet,
                            rebuild   = False,
                            verbose   = True,
                            ):
        """ This downloads the complete dam inventory and generates
            a shapefile from it with the relevant fields. The old
            shapefile that was used followed these rules:
            -dams 50 feet or more in height
            -or with a normal storage capacity of 5,000 acre-feet or more
            -or with a maximum storage capacity of 25,000 acre-feet or more.
            
            By default this method uses those but they can be adjusted if
            required.
            
            If the rebuild parameter is True, a new inventory will be
            downloaded and the shapefile regenerated. Otherwise, if a
            shapefile already exists then it will be used."""

        # source csv

        csv = '{}/{}'.format(self.NID, csv_name)

        if not os.path.isfile(csv) or rebuild:

            if verbose: 
                print('downloading NID source file {}\n'.format(url))
                request.urlretrieve(website, csv)

        elif verbose: print('NID source file {} exists\n'.format(csv))

        # generate the source shapefile

        self.source = '{}/{}'.format(self.NID, shapefile)

        if not os.path.isfile(self.source + '.shp') or rebuild:

            # read in the csv to a dataframe
            dams = pd.read_csv(csv,header=1,low_memory=False)

            # create a shapefile for the dams
            w = Writer(self.source, shapeType = 1)

            # add the relevant fields
            w.field('DAM_NAME',   'C', 65,   0)
            w.field('NIDID',      'C', 7,    0)
            w.field('LONGITUDE',  'N', 19,  11)
            w.field('LATITUDE',   'N', 19,  11)
            w.field('RIVER',      'C', 65,   0)
            w.field('OWN_NAME',   'C', 65,   0)
            w.field('DAM_TYPE',   'C', 10,   0)
            w.field('PURPOSES',   'C', 254,  0)
            w.field('YR_COMPL',   'C', 10,   0)
            w.field('NID_HEIGHT', 'N', 19,  11)
            w.field('MAX_STOR',   'N', 19,  11)
            w.field('NORMAL_STO', 'N', 19,  11)
            w.field('SURF_AREA',  'N', 19,  11)

            # iterate through the dataframe adding the shapes and records
            for i in range(0,len(dams)):
                dam_name = dams.loc[i,'Dam Name']
                nidid = dams.loc[i,'NID ID']
                lon = dams.loc[i,'Longitude']
                lat = dams.loc[i,'Latitude']
                river = dams.loc[i,'River or Stream Name']
                own_name = dams.loc[i,'Owner Names']
                dam_type = dams.loc[i,'Dam Types']
                purposes = dams.loc[i,'Purposes']
                yr_complete = dams.loc[i,'Year Completed']
                nid_height = dams.loc[i,'NID Height (Ft)']
                max_store = dams.loc[i,'Max Storage (Acre-Ft)']
                norm_store = dams.loc[i,'Normal Storage (Acre-Ft)']
                surface_area = dams.loc[i,'Surface Area (Acres)']

                # check if the dam meets the specifications
                if nid_height >= height or norm_store >= storage or max_store >= maxstore:
                    w.point(lon,lat)
                    w.record(dam_name,nidid,lon,lat,river,own_name,dam_type,purposes,
                             yr_complete,nid_height,max_store,norm_store,surface_area)

            # write to the file
            w.close()

    def extract_bbox(self, bbox, output, verbose = True):
        """Extracts the NID dam locations for a watershed from the dam 
        shapefile and the 8-digit hydrologic unit code of interest. 
        """

        self.generate_shape()

        xmin, ymin, xmax, ymax = bbox

        # copy the projection files

        if verbose: print('copying the projections from the NID source\n')

        projection = self.source + '.prj'

        shutil.copy(projection, output + '.prj')

        # get the dams within the watershed

        if verbose: print('reading the dam file\n')

        sf = Reader(self.source)

        # work around for issues with pyshp

        damrecords   = []
        for i in range(len(sf.shapes())):
            try: damrecords.append(sf.record(i))
            except: damrecords.append([-100 for i in range(len(sf.fields))])

        name_index  = sf.fields.index(['DAM_NAME',   'C', 65,   0]) - 1
        nid_index   = sf.fields.index(['NIDID',      'C', 7,    0]) - 1
        long_index  = sf.fields.index(['LONGITUDE',  'N', 19,  11]) - 1
        lat_index   = sf.fields.index(['LATITUDE',   'N', 19,  11]) - 1
        river_index = sf.fields.index(['RIVER',      'C', 65,   0]) - 1
        owner_index = sf.fields.index(['OWN_NAME',   'C', 65,   0]) - 1
        type_index  = sf.fields.index(['DAM_TYPE',   'C', 10,   0]) - 1
        purp_index  = sf.fields.index(['PURPOSES',   'C', 254,  0]) - 1
        year_index  = sf.fields.index(['YR_COMPL',   'C', 10,   0]) - 1
        high_index  = sf.fields.index(['NID_HEIGHT', 'N', 19,  11]) - 1
        mstor_index = sf.fields.index(['MAX_STOR',   'N', 19,  11]) - 1
        nstor_index = sf.fields.index(['NORMAL_STO', 'N', 19,  11]) - 1
        area_index  = sf.fields.index(['SURF_AREA',  'N', 19,  11]) - 1

        # iterate through the fields and determine which points are in the box

        if verbose: print('extracting dams into new file\n')

        dam_indices = []

        i = 0
        for record in damrecords:

            lat = record[lat_index]
            lon = record[long_index]

            if self.inside_box([xmin, ymin], [xmax, ymax], [lon, lat]):
                dam_indices.append(i)
            i+=1

        # write the data from the bbox to a new shapefile

        w = Writer(output, shapeType = 1)

        for field in sf.fields:  w.field(*field)

        for i in dam_indices:
            point = sf.shape(i).points[0]
            w.point(*point)

            values = damrecords[i]

            rs = []

            for value in values:

                if isinstance(value, bytes): value = value.decode('utf-8')
                rs.append(value)

            w.record(*rs)

        w.close()

        if verbose: 

            print('successfully extracted NID dam locations to new file\n')

    def extract_shapefile(self, shapefile, output):
        """Extracts the dams within the bounding box of the shapefile."""

        if not os.path.isfile(output + '.shp'):

            if os.path.isfile(shapefile + '.shp'):

                r = Reader(shapefile)

            else:

                print('error: shapefile {} does not exist'.format(shapefile))
                raise

            self.extract_bbox(r.bbox, output)
            
        else: print('dam shapefile {} exists\n'.format(output))
            
