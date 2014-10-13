# cdlextractor.py
#
# David J. Lampert, PhD, PE
#
# last updated: 10/12/2014
#
# Calculates the land use data from a raster file within each of the shapes
# in a shapefile.

import os, pickle, csv, zipfile, gdal, osr

from shapefile       import Reader
from numpy           import array, unique, argwhere
from multiprocessing import Process
from urllib          import request

from .rasterutils import get_raster, get_raster_table, get_raster_in_poly

class CDLExtractor:
    """A class to download and extract data from the Cropland Data Layer for
     a given state and period of years."""

    def __init__(self,
                 destination,
                 website = 'http://nassgeodata.gmu.edu/nass_data_cache/byfips',
                 ):

        self.destination = destination
        self.website     = website

        # make the destination directory if needed

        if not os.path.isdir(destination):

            print('destination directory for CDL data does not exist\n')

            try: 

                print('creating destination directory for CDL data\n')
                os.mkdir(destination)

            except:

                print('error, unable to create destination directory\n')
                raise

        # state codes

        self.statecodes = {
            'AL': '01',
            'AK': '02',
            'AZ': '04',
            'AR': '05',
            'CA': '06',
            'CO': '08',
            'CT': '09',
            'DC': '11',
            'DE': '10',
            'FL': '12',
            'GA': '13',
            'ID': '16',
            'IL': '17',
            'IN': '18',
            'IA': '19',
            'KS': '20',
            'KY': '21',
            'LA': '22',
            'ME': '23',
            'MD': '24',
            'MA': '25',
            'MI': '26',
            'MN': '27',
            'MS': '28',
            'MO': '29',
            'MT': '30',
            'NE': '31',
            'NV': '32',
            'NH': '33',
            'NJ': '34',
            'NM': '35',
            'NY': '36',
            'NC': '37',
            'ND': '38',
            'OH': '39',
            'OK': '40',
            'OR': '41',
            'PA': '42',
            'RI': '44',
            'SC': '45',
            'SD': '46',
            'TN': '47',
            'TX': '48',
            'UT': '49',
            'VT': '50',
            'VA': '51',
            'WA': '53',
            'WV': '54',
            'WI': '55',
            'WY': '56',
            }
 
    def is_number(self, s):
        """Test if the string is a number."""

        try: float(s) 
        except ValueError: return False
        return True

    def report(self, 
               n, 
               block, 
               size,
               ):
        """Private method to report the status of the file download."""

        if n % 100 == 0:
            it = block * n / 10**6, size / 10**6
            print('{:.1f} MB of {:.1f} MB transferred'.format(*it))

    def download_data(self, 
                      state,
                      years,
                      ):

        self.state = state
        self.years = []

        for year in years:

            # compressed filename on the CDL server

            webfile = 'CDL_{}_{}.zip'.format(year, self.statecodes[state])

            # path to compressed filename locally

            compressed = '{}/{}'.format(self.destination, webfile)

            # url to file on CDL server

            url = '{}/{}'.format(self.website, webfile)

            # download the compressed file if needed

            its = year, state
            if not os.path.isfile(compressed):

                print('downloading compressed file for {} {}\n'.format(*its))
                
                try: 

                    request.urlretrieve(url, compressed, self.report)
                    self.years.append(year)

                except:

                    print('unable to download CDL data')
 
            else: 

                print('compressed file for {} {} exists'.format(*its))

            # decompress the files

            its = self.destination, year, self.statecodes[state]
            decompressed = '{}/CDL_{}_{}.tif'.format(*its)
            if not os.path.isfile(decompressed):

                print('decompressing {} archive\n'.format(compressed))
                f = zipfile.ZipFile(compressed)
                f.extractall(self.destination)

        print('')

    def extract_bbox(self,
                     state,
                     year,
                     bbox,
                     output,
                     space = 0.05,
                     verbose = True,
                     ):
        """Extracts NASS CDL data from the source file for the bounding box."""
        
        # file path to the CDL file for the state and year

        its = self.destination, year, self.statecodes[state]
        decompressed = '{}/CDL_{}_{}.tif'.format(*its)

        if not os.path.isfile(decompressed):

            print('error, source file does not exist\n')
            raise

        # get the extents

        xmin, ymin, xmax, ymax = bbox

        # adjust to make the map just larger than the extents

        xmin, xmax = xmin - space * (xmax - xmin), xmax + space * (xmax - xmin)
        ymin, ymax = ymin - space * (ymax - ymin), ymax + space * (ymax - ymin)

        # extract the values of the DEM raster and the origin from NASS

        values, corner = get_raster_table(decompressed, [xmin,ymin, xmax, ymax],
                                          'uint8')

        # get the source, source reference, and the source band

        source = gdal.Open(decompressed)
        source_band = source.GetRasterBand(1)
        source_reference = osr.SpatialReference()
        source_reference.ImportFromWkt(source.GetProjectionRef())

        # set the transform to the new origin

        transform = source.GetGeoTransform()
        transform = (corner[0], transform[1], transform[2], corner[1],
                     transform[4], transform[1])

        # get a driver and set the projection and georeference

        driver = gdal.GetDriverByName('GTiff')

        destination = driver.Create(output, 
                                    len(values[0]), 
                                    len(values), 
                                    1, 
                                    gdal.GDT_Byte)
        destination.SetGeoTransform(transform)

        # this probably isn't exactly right--need to re-project this 
        # using gdalwarp or something similar (future effort???)

        destination_reference = osr.SpatialReference()
        destination_reference.SetUTM(source_reference.GetUTMZone())
        #destination_reference.SetWellKnownGeogCS('NAD83')

        destination.SetProjection(destination_reference.ExportToWkt())

        # set the metadata and get the destination band

        destination.SetMetadata(source.GetMetadata())
        destination_band = destination.GetRasterBand(1)

        # copy the pertinent attributes to the band

        destination_band.WriteArray(values, 0, 0)
        destination_band.SetColorTable(source_band.GetColorTable().Clone())

        # transform the projection from WGS 1984 to NAD 1983

        gdal.ReprojectImage(source, 
                            destination, 
                            source_reference.ExportToWkt(), 
                            destination_reference.ExportToWkt()
                            )

        # close up the files

        source      = None
        destination = None

        if verbose: print('successfully extracted cropland data to new file\n')


def yearly_landuse(rasterfile, shapefile, filepath, overwrite = False, 
                   verbose = True):
    """Parses through the rasterfile and determines the number of pixels in 
    each shape in the shapefile, and then creates a dictionary linking the
    "comid" attribute to the unique pixel values and their corresponding areas 
    for each shape in the shapefile.
    """

    if os.path.isfile(filepath) and not overwrite:
        if verbose: print('file {} exists'.format(filepath))
        return

    if verbose: print('calculating land use statistics')

    # open the subbasin shapefiles

    sf = Reader(shapefile, shapeType = 5)

    comid_index = sf.fields.index(['ComID',    'N',  9, 0]) - 1
    area_index  = sf.fields.index(['AreaSqKm', 'N', 10, 2]) - 1

    subbasins = {}
    for i in range(len(sf.records())):

        # get the record info and geometry for the subbasin

        points = array(sf.shape(i).points)
        record = sf.record(i)

        comid    = '{}'.format(record[comid_index])
        tot_area = record[area_index]

        if verbose: print('calculating land use data for {}'.format(comid))
 
        # read all the pixels in the area

        values, origin = get_raster_in_poly(rasterfile, points, verbose = False)
        values = values.flatten()
        values = values[values.nonzero()]

        tot_pixels = len(values)

        # count the number of pixels of each land use type

        landcodes = []
        areas     = []
        for v in unique(values):
            pixels = len(values[argwhere(values == v)])
            area   = tot_area * pixels / tot_pixels

            landcodes.append(v)
            areas.append(area)

        subbasins['{}'.format(comid)] = landcodes, areas

    # pickle the data into a file for later

    with open(filepath, 'wb') as f: pickle.dump(subbasins, f)

