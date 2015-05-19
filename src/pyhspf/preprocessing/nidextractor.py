# nidextractor.py
#
# David J. Lampert, PhD, PE
# last uptdated: 10/07/2014
# Purpose: Contains the NIDExtractor class to download data from the National
# Inventory of Dams and then extract the dams for a given HUC8.

import os, shutil, subprocess, tarfile

from urllib    import request
from shapefile import Reader, Writer

class NIDExtractor:

    def __init__(self, 
                 NID, 
                 website = 'http://dds.cr.usgs.gov/pub/data/nationalatlas'):

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

    def download_compressed(self, 
                            webfile = 'dams00x020_nt00010.tar.gz',
                            name    = 'dams00x020',
                            verbose = True,
                            ):
        """Private method to download a compressed file."""

        # compressed file name on local machine

        compressed = '{}/{}'.format(self.NID, webfile)

        # location of the file on the NHDPlus ftp server (the NHDPlus file
        # structure is not consistent so this if statement is needed)

        url = '{}/{}'.format(self.website, webfile)

        if not os.path.isfile(compressed):

            if verbose: 
                print('downloading NID source file {}\n'.format(url))
                request.urlretrieve(url, compressed)

        elif verbose: print('NID source file {} exists\n'.format(compressed))

        # decompress the file

        self.source = '{}/{}'.format(self.NID, name)

        if not os.path.isfile(self.source + '.shp'):

            with tarfile.open(compressed) as f: f.extractall(self.NID)

    def extract_bbox(self, bbox, output, verbose = True):
        """Extracts the NID dam locations for a watershed from the dam 
        shapefile and the 8-digit hydrologic unit code of interest. 
        """

        self.download_compressed()

        xmin, ymin, xmax, ymax = bbox

        # copy the projection files

        if verbose: print('copying the projections from the NID source\n')

        projection = self.source + '.prj'

        shutil.copy(projection, output + '.prj')

        # get the dams within the watershed

        if verbose: print('reading the dam file\n')

        sf = Reader(self.source, shapeType = 1)

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

        w = Writer(shapeType = 1)

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

        w.save(output)

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
            
