# nhdplusextractor.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 07/21/2014
#
# contains the NHDPlus Extractor class, which can be used to retrieve source
# data from the NHDPlus V2 dataset online, and then extract the data from
# the larger source files for a given 8-digit Hydrologic Unit Code (HUC8).
# Example at the bottom for the Patuxent watershed.

from urllib import request

import shutil, os, io, time, pickle, subprocess, gdal, numpy

from gdalconst               import GA_ReadOnly
from shapefile               import Reader, Writer
from .dbf                    import read_dbf
from .raster                 import get_raster_table
from .raster                 import get_degree_transform
from .raster                 import get_raster
from .merge_shapes           import merge_shapes
from matplotlib              import pyplot, path, ticker
from matplotlib              import patches, colors 
from mpl_toolkits.axes_grid1 import make_axes_locatable

from .flowline import Flowline

class NHDPlusExtractor:
    """A class to download and extract data from the NHDPlus V2 database."""
        
    def __init__(self, 
                 abbreviation,
                 VPU,
                 destination,
                 url ='ftp://www.horizon-systems.com/NHDPlus/NHDPlusV21/Data',
                 ):

        self.url          = url
        self.destination  = destination
        self.abbreviation = abbreviation
        self.VPU          = VPU

    def report(self, 
               n, 
               block, 
               size,
               ):

        if n % 100 == 0:
            it = block * n / 10**6, size / 10**6
            print('{:.1f} MB of {:.1f} MB transferred'.format(*it))

    def download_data(self,
                      catchment_n = '01',
                      database_n = '03',
                      ned_n = '01',
                      verbose = True,
                      ):
        """Downloads the source data files."""

        if not os.path.isdir(self.destination): 

            if verbose: print('No NHDPlus data present in destination\n')
            os.mkdir(self.destination)

        else: print('NHDPlus destination directory exists\n')

        destination = '{}/NHDPlus{}'.format(self.destination, self.abbreviation)
        if not os.path.isdir(destination):

            if verbose: 
                print('NHDPlus data for ' +
                      '{} not present'.format(self.abbreviation))
            os.mkdir('{}'.format(destination))

        elif verbose: print('Data for {} exist\n'.format(self.abbreviation))
        
        # decompressed NHDPlus directory

        self.NHDPlus = '{}/NHDPlus{}'.format(destination, self.VPU)

        # catchment shapefile

        its = self.abbreviation, self.VPU, catchment_n
        webfile = 'NHDPlusV21_{}_{}_NHDPlusCatchment_{}.7z'.format(*its)
        its = self.destination, self.abbreviation, webfile
        catchmentfile = '{}/NHDPlus{}/{}'.format(*its)
        its = self.NHDPlus, 'Catchment', 'Catchment'
        self.catchmentfile = '{}/NHDPlus{}/{}'.format(*its)

        if not os.path.isfile(catchmentfile):

            its = self.url, self.abbreviation, webfile
            url = '{}/NHDPlus{}/{}'.format(*its)
            if verbose: 
                print('downloading catchment shapefile from {}\n'.format(url))
            request.urlretrieve(url, catchmentfile, self.report)

        elif verbose: print('catchment source shapefile exists\n')

        if not os.path.isfile(catchmentfile[:-3] + '.txt'): 
            self.decompress(catchmentfile)

        # flowline shapefile

        its = self.abbreviation, self.VPU, database_n
        webfile = 'NHDPlusV21_{}_{}_NHDSnapshot_{}.7z'.format(*its)
        its = self.destination, self.abbreviation, webfile
        flowlinefile = '{}/NHDPlus{}/{}'.format(*its)
        its = self.NHDPlus, 'NHDSnapshot', 'Hydrography', 'NHDFlowline'
        self.flowlinefile = '{}/{}/{}/{}'.format(*its)
        self.projection   = '{}/{}/{}/{}.prj'.format(*its)

        if not os.path.isfile(flowlinefile):

            its = self.url, self.abbreviation, webfile
            url = '{}/NHDPlus{}/{}'.format(*its)
            if verbose: 
                print('downloading flowline shapefile from {}\n'.format(url))
            request.urlretrieve(url, flowlinefile, self.report)

        elif verbose: print('flowline source shapefile exists\n')

        if not os.path.isfile(flowlinefile[:-3] + '.txt'): 
            self.decompress(flowlinefile)

        # attribute databases

        its = self.abbreviation, self.VPU, database_n
        webfile = 'NHDPlusV21_{}_{}_NHDPlusAttributes_{}.7z'.format(*its)
        its = self.destination, self.abbreviation, webfile
        attributefile = '{}/NHDPlus{}/{}'.format(*its)
        its = self.NHDPlus, 'NHDPlusAttributes'
        self.elevslopefile       = '{}/{}/elevslope.dbf'.format(*its)
        self.PlusFlowlineVAAfile = '{}/{}/PlusFlowlineVAA.dbf'.format(*its)

        if not os.path.isfile(attributefile):

            its = self.url, self.abbreviation, webfile
            url = '{}/NHDPlus{}/{}'.format(*its)
            if verbose: 
                print('downloading attribute databases from {}\n'.format(url))
            request.urlretrieve(url, attributefile, self.report)

        elif verbose: print('attribute source databases exist\n')

        if not os.path.isfile(attributefile[:-3] + '.txt'): 
            self.decompress(attributefile)

        # EROM databases

        its = self.abbreviation, self.VPU, database_n
        webfile = 'NHDPlusV21_{}_{}_EROMExtension_{}.7z'.format(*its)
        its = self.destination, self.abbreviation, webfile
        eromfile = '{}/NHDPlus{}/{}'.format(*its)
        self.eromfile = '{}/EROMExtension/EROM_MA0001.DBF'.format(self.NHDPlus)

        if not os.path.isfile(eromfile):

            its = self.url, self.abbreviation, webfile
            url = '{}/NHDPlus{}/{}'.format(*its)
            if verbose: 
                print('downloading attribute databases from {}\n'.format(url))
            request.urlretrieve(url, eromfile, self.report)

        elif verbose: print('EROM source database exists\n')

        if not os.path.isfile(eromfile[:-3] + '.txt'): 
            self.decompress(eromfile)

        # NED rasters -- these are in different files with a letter so iterate

        letters       = 'abcd'
        self.nedfiles = []

        for l in letters:

            its = self.abbreviation, self.VPU, self.VPU, l, ned_n
            webfile = 'NHDPlusV21_{}_{}_{}{}_NEDSnapshot_{}.7z'.format(*its)
            its = self.destination, self.abbreviation, webfile
            temp = '{}/NHDPlus{}/{}'.format(*its)

            if not os.path.isfile(temp):

                its = self.url, self.abbreviation, webfile
                url = '{}/NHDPlus{}/{}'.format(*its)

                try:
                    request.urlopen(url)
                    if verbose: 
                        print('downloading raster from {}\n'.format(url))
                    request.urlretrieve(url, temp, self.report)

                except: pass

            elif verbose: print('NED source raster {} exists\n'.format(temp))

            if os.path.isfile(temp): 

                its = self.NHDPlus, self.VPU, l
                nedfile = '{}/NEDSnapshot/Ned{}{}/elev_cm'.format(*its)
                self.nedfiles.append(nedfile)

            if not os.path.isfile(temp[:-3] +'.txt') and os.path.isfile(temp):
                self.decompress(temp)

    def decompress(self, filename):
        """points to the right method for the operating system."""

        if os.name == 'posix': 
            self.decompress_linux(filename)
        elif os.name == 'nt':
            self.decompress_windows(filename)
        else:     
            print('unknown operating system')
            raise

    def decompress_windows(self, 
                           filename, 
                           path_to_7zip = r'C:\Program Files\7-Zip\7z.exe',
                           ):
        """opens a subprocess to use 7zip to decompress the file."""

        args = [path_to_7zip, 'x', '-o{}'.format(self.destination), filename]

        with subprocess.Popen(args, stdout = subprocess.PIPE).stdout as s:

            print(s.read().decode())

    def decompress_linux(self, filename):
        """Unzips the archive on linux."""

        args = ['7z', 'x', '-o{}'.format(self.destination), filename]

        with subprocess.Popen(args, stdout = subprocess.PIPE).stdout as s:

            print(s.read().decode())

    def get_comids(self, flowlinefile):
        """Finds the comids from the flowline file."""

        # open the file

        shapefile = Reader(flowlinefile)

        # find the index of the comids

        comid_index = shapefile.fields.index(['COMID', 'N', 9,  0]) - 1

        # make a list of the comids

        comids = [r[comid_index] for r in shapefile.records()]

        return comids

    def extract_flowlines(self, source, destination, HUC8, verbose = True):
        """Extracts flowlines from the source datafile to the destination using
        the HUC8 for the query."""

        # open the flowline file
    
        if verbose: print('reading the flowline file\n')
    
        shapefile = Reader(source, shapeType = 3)
        records   = shapefile.records()
    
        # figure out which field codes are the Reach code and comid
    
        reach_index = shapefile.fields.index(['REACHCODE', 'C', 14, 0]) - 1
    
        # go through the reach indices, add add them to the list of flowlines
        # if in the watershed; also make a list of the corresponding comids
    
        if verbose: print('searching for flowlines in the watershed\n')
    
        indices = []
       
        i = 0
        for record in records:
            if record[reach_index][:8] == HUC8: indices.append(i)
            i+=1

        if len(indices) == 0:
            if verbose: print('error: query returned no values')
            raise
    
        # write the data from the HUC8 to a new shapefile
    
        w = Writer(shapeType = 3)
    
        for field in shapefile.fields:  w.field(*field)
    
        for i in indices:
            shape = shapefile.shape(i)
            w.poly(shapeType = 3, parts = [shape.points])
    
            record = records[i]
    
            # little work around for blank GNIS_ID and GNIS_NAME values
    
            if isinstance(record[3], bytes):
                record[3] = record[3].decode('utf-8')
            if isinstance(record[4], bytes):
                record[4] = record[4].decode('utf-8')
    
            w.record(*record)
    
        w.save(destination)
    
        if verbose: 
            l = len(indices)
            print('queried {} flowlines from original shapefile\n'.format(l))

    def extract_catchments(self, source, destination, flowlinefile, 
                           verbose = True):
        """Extracts the catchments from the source data file to the destination
        using the list of comids for the query."""

        # make a list of the comids

        comids = self.get_comids(flowlinefile)

        # open the catchment shapefile
    
        if verbose: print('reading the catchment shapefile\n')
    
        shapefile = Reader(source)
    
        # get the index of the feature id, which links to the flowline comid
    
        featureid_index = shapefile.fields.index(['FEATUREID', 'N', 9, 0]) - 1
    
        # go through the comids from the flowlines and add the corresponding 
        # catchment to the catchment list
    
        if verbose: print('searching the catchments in the watershed\n')
    
        records = shapefile.records()
        indices = []
    
        i = 0
        for record in records:
            if record[featureid_index] in comids: indices.append(i)
            i+=1
    
        if len(indices) == 0:
            print('query returned no values, returning\n')
            raise

        # create the new shapefile
    
        if verbose: print('writing the new catchment shapefile\n')
        
        w = Writer()
    
        for field in shapefile.fields:  w.field(*field)
    
        for i in indices:
            shape = shapefile.shape(i)
            w.poly(shapeType = 5, parts = [shape.points])
            w.record(*records[i])
    
        w.save(destination)

    def overlaps(self, bbox1, bbox2):
        """Tests if the two bounding boxes overlap."""

        xmin1, ymin1, xmax1, ymax1 = bbox1
        xmin2, ymin2, xmax2, ymax2 = bbox2

        if ((xmin1 < xmax2 and xmax1 > xmin2) and
            (ymin1 < ymax2 and ymax1 > ymin2)):

            return True

        else: return False

    def find_NED(self, catchmentfile):
        """Parses the elevation rasters to find the one where the HUC8 is
        located."""

        shapefile = Reader(catchmentfile)

        f = None
        for nedfile in self.nedfiles:

            t,v = get_raster_table(nedfile, shapefile.bbox, 'int32')

            if t is not None: 
                f = nedfile
                break

        if f is not None: 
            return f
        else:
            print('warning: unable to find NED file')
            raise

    def extract_NED(self, 
                    NED, 
                    catchmentfile, 
                    output, 
                    space = 0.05, 
                    verbose = True,
                    quiet = True,
                    ):
        """Extracts elevation data as a raster file from the National Elevation
        Dataset located in the NHDPlus directory.
        """

        if verbose: print('copying the elevation data from NED\n')

        # get the coordinates for the bounding box from the flowline shapefile

        shapefile = Reader(catchmentfile)

        xmin, ymin, xmax, ymax = shapefile.bbox

        if quiet: gdal.PushErrorHandler('CPLQuietErrorHandler') 

        #adjust to make the map just larger than the extents

        xmin = xmin - space * (xmax - xmin)
        ymin = ymin - space * (ymax - ymin)
        xmax = xmax + space * (xmax - xmin)
        ymax = ymax + space * (ymax - ymin)

        # extract the values of the DEM raster and the origin from the NED

        values, corner = get_raster_table(NED, [xmin, ymin, xmax, ymax], 
                                          dtype = 'int32')

        # open the file

        source = gdal.Open(NED)

        # set the transform to the new origin

        transform = source.GetGeoTransform()
        transform = (corner[0], transform[1], transform[2], corner[1],
                     transform[4], transform[1])

        # get the source band

        band = source.GetRasterBand(1)

        # get a driver and make the new file

        driver = gdal.GetDriverByName('GTiff')

        dest = driver.Create(output, len(values[0]), len(values), 1,  
                             gdal.GDT_UInt16)

        dest.SetProjection(source.GetProjection())
        dest.SetMetadata(source.GetMetadata())
        dest.SetGeoTransform(transform)
    
        dest.GetRasterBand(1).WriteArray(values, 0, 0)
        #dest.GetRasterBand(1).SetStatistics(*band.GetStatistics(0,1))

        # close the files

        source = None
        dest   = None

        if verbose: print('successfully extracted elevation data to new file\n')

    def extract_HUC8(self, 
                     HUC8,                             # HUC8
                     output,                           # output directory
                     flowlinefile  = 'flowlines',      # flowline shapefile
                     catchmentfile = 'catchments',     # catchment shapefile
                     boundaryfile  = 'boundary',       # merged catchment file
                     VAAfile       = 'flowlineVAAs',   # VAA file
                     elevfile      = 'elevations.tif', # NED raster file
                     verbose       = True,             # print verbosity
                     vverbose      = False,            # print verbosity
                     ):
        """Creates shapefiles for the NHDPlus flowlines and catchments for an 
        8-digit hydrologic unit code from the NHDPlus Version 2 source data.
        Output shapefiles are written to the optional "output" directory.
        """

        start = time.time()

        # if the destination folder does not exist, make it

        if not os.path.isdir(output): os.mkdir(output)

        # start by copying the projection files

        if vverbose: print('\ncopying the projections from NHDPlus\n')

        f = '{}/{}.prj'.format(output, flowlinefile)
        if not os.path.isfile(f): shutil.copy(self.projection, f)

        f = '{}/{}.prj'.format(output, catchmentfile)
        if not os.path.isfile(f): shutil.copy(self.projection, f)

        # extract the flowlines and get the NHDPlus comids

        f = '{}/{}'.format(output, flowlinefile)
        if not os.path.isfile(f + '.shp'):
            if verbose: print('extracting flowline shapefile\n')
            self.extract_flowlines(self.flowlinefile, f, HUC8,
                                   verbose = vverbose)

        # extract the different files from the sources sequentially

        p = '{}/{}'.format(output, catchmentfile)
        if not os.path.isfile(p + '.shp'):
            if verbose: print('extracting catchment shapefile\n')
            self.extract_catchments(self.catchmentfile, p, f,
                                    verbose = vverbose)

        p = '{}/{}'.format(output, VAAfile)
        if not os.path.isfile(p):

            # get the comids

            comids = self.get_comids(f)

            # read hydrologic sequence and drainage attributes from the database
    
            flowattributes = ['ComID', 'Hydroseq', 'DnHydroseq', 'UpHydroseq', 
                              'TotDASqKM', 'AreaSqKM', 'DivDASqKM','ReachCode']
    
            if verbose: print('reading flowline value added attributes\n')
            flowvalues = read_dbf(self.PlusFlowlineVAAfile, 
                                  attributes = flowattributes, 
                                  comids = comids, 
                                  verbose = vverbose)
    
            # read the slope data from the database
    
            slopeattributes = ['COMID', 'MAXELEVSMO', 'MINELEVSMO', 
                               'SLOPELENKM']
    
            if verbose: print('reading slope and elevation attributes\n')
            slopevalues = read_dbf(self.elevslopefile, 
                                   attributes = slopeattributes, 
                                   comids = comids, 
                                   verbose = vverbose)
            
            # get the flow and velocity data
    
            eromattributes = ['Comid', 'Q0001E', 'V0001E', 'SMGageID']

            if verbose: print('reading EROM model attributes\n')
            eromvalues = read_dbf(self.eromfile, 
                                  attributes = eromattributes, 
                                  comids = comids, 
                                  verbose = vverbose)
    
            # store the flowline data in a dictionary using hydroseqs as keys 
            # and make a dictionary linking the comids to hydroseqs
    
            flowlines = {}
    
            for flowlineVAAs in zip(*(flowvalues[a] for a in flowattributes)):
                flowlines[flowlineVAAs[1]] = Flowline(*flowlineVAAs)

            for f in flowlines:
                i = slopevalues['COMID'].index(flowlines[f].comid)
                flowlines[f].add_slope(slopevalues['MAXELEVSMO'][i], 
                                       slopevalues['MINELEVSMO'][i],
                                       slopevalues['SLOPELENKM'][i])
                i = eromvalues['Comid'].index(flowlines[f].comid)
                flowlines[f].add_flow(eromvalues['Q0001E'][i], 
                                      eromvalues['V0001E'][i],
                                      eromvalues['SMGageID'][i])
                flowlines[f].estimate_traveltime()
    
            # save the data in a dictionary for future use
    
            with open(p, 'wb') as f: pickle.dump(flowlines, f)

        # find the right NED DEM and extract the elevation raster

        f = '{}/{}'.format(output, elevfile)
        if not os.path.isfile(f):
            if verbose: print('extracting the NED raster file\n')
            cfile = '{}/{}'.format(output, catchmentfile)
            NED = self.find_NED(cfile)
            self.extract_NED(NED, cfile, f, verbose = vverbose)

        end = time.time()
        t = end - start

        if verbose: 
            print('successfully queried data in {:.1f} seconds\n'.format(t))
    
        # merge the shapes into a watershed

        bfile = '{}/{}.shp'.format(output, boundaryfile)
        if not os.path.exists(bfile):

            cfile = '{}/{}'.format(output, catchmentfile)
            merge_shapes(cfile, outputfile = bfile)

    def get_distance(self, p1, p2):
        """Approximates the distance in kilometers between two points on the 
        Earth's surface designated in decimal degrees using an ellipsoidal 
        projection. per CFR 73.208 it is applicable for up to 475 kilometers.
        p1 and p2 are listed as (longitude, latitude).
        """

        deg_rad = numpy.pi / 180

        dphi = p1[1] - p2[1]
        phim = 0.5 * (p1[1] + p2[1])
        dlam = p1[0] - p2[0]

        k1 = (111.13209 - 0.56605 * numpy.cos(2 * phim * deg_rad) + 0.00120 * 
              numpy.cos(4 * phim * deg_rad))
        k2 = (111.41513 * numpy.cos(phim * deg_rad) - 0.09455 * 
              numpy.cos(3 * phim * deg_rad) + 0.0012 * 
              numpy.cos(5 * phim * deg_rad))

        return numpy.sqrt(k1**2 * dphi**2 + k2**2 * dlam**2)

    def get_boundaries(self, shapes, space = 0.1):
        """Gets the boundaries for the plot."""

        boundaries = shapes[0].bbox
        for shape in shapes[0:]:
            b = shape.bbox
            if b[0] < boundaries[0]: boundaries[0] = b[0]
            if b[1] < boundaries[1]: boundaries[1] = b[1]
            if b[2] > boundaries[2]: boundaries[2] = b[2]
            if b[3] > boundaries[3]: boundaries[3] = b[3]

        xmin = boundaries[0] - (boundaries[2] - boundaries[0]) * space
        ymin = boundaries[1] - (boundaries[3] - boundaries[1]) * space
        xmax = boundaries[2] + (boundaries[2] - boundaries[0]) * space
        ymax = boundaries[3] + (boundaries[3] - boundaries[1]) * space

        return xmin, ymin, xmax, ymax

    def make_patch(self,
                   points, 
                   facecolor, 
                   edgecolor = 'Black', 
                   width = 1, 
                   alpha = None,
                   hatch = None, 
                   label = None,
                   ):
        """Uses a list or array of points to generate a matplotlib patch."""

        vertices = [(point[0], point[1]) for point in points]
        vertices.append((points[0][0], points[0][1]))

        codes     = [path.Path.LINETO for i in range(len(points) + 1)]
        codes[0]  = path.Path.MOVETO

        patch = patches.PathPatch(path.Path(vertices, codes), 
                                  facecolor = facecolor,
                                  edgecolor = edgecolor, 
                                  lw = width, 
                                  hatch = hatch,
                                  alpha = alpha, 
                                  label = label)
        return patch

    def add_raster(self, 
                   fig, 
                   filename, 
                   resolution, 
                   extent, 
                   colormap, 
                   scale,
                   ):
        """adds a rectangular raster image with corners located at the extents
        to a plot.
        """

        # flatten the arrays and set up an array for the raster

        xmin, ymin, xmax, ymax = extent

        xs = numpy.array([xmin + (xmax - xmin) / resolution * i 
                          for i in range(resolution + 1)])
        ys = numpy.array([ymax  - (ymax  - ymin)  / resolution * i 
                          for i in range(resolution + 1)])

        zs = numpy.zeros((resolution + 1, resolution + 1))

        # iterate through the grid and fill the array

        for i in range(len(ys)):
            zs[i, :] = get_raster(filename, zip(xs, [ys[i]] * (resolution + 1)),
                                  quiet = True)

        # scale the values

        zs = zs / scale
        space = 0.1
        mi, ma = zs.min(), zs.max()
        mi, ma = mi - space * (ma - mi), ma + space * (ma - mi)
        norm = colors.Normalize(vmin = mi, vmax = ma)

        # plot the grid

        return fig.imshow(zs, extent = [xmin, xmax, ymin, ymax], norm = norm, 
                          cmap = colormap)

    def plot_HUC8(self, 
                  flowfile, 
                  cfile,
                  bfile,
                  VAAfile, 
                  elevfile,
                  patchcolor = None,
                  resolution = 400, 
                  colormap = 'gist_earth',
                  grid = False,
                  title = None, 
                  verbose = True,
                  output = None,
                  show = False,
                  ):
        """Makes a plot of the raw NHDPlus data."""

        if verbose: print('generating plot of the watershed\n')

        fig = pyplot.figure()
        subplot = fig.add_subplot(111, aspect = 'equal')
        subplot.tick_params(axis = 'both', which = 'major', labelsize = 10)

        # add the title

        if title is not None: subplot.set_title(title, fontsize = 14)

        if patchcolor is None: facecolor = (1,0,0,0.)
        else:                  facecolor = patchcolor

        # open up and show the boundary

        b = Reader(bfile, shapeType = 5)

        boundary = b.shape(0)
        points = numpy.array(boundary.points)
        subplot.add_patch(self.make_patch(points, facecolor, width = 0.3))

        # open up and show the catchments

        c = Reader(cfile, shapeType = 5)

        extent = self.get_boundaries(c.shapes(), space = 0.02)

        xmin, ymin, xmax, ymax = extent

        # figure out how far one foot is on the map

        points_per_width = 72 * 8
        ft_per_km = 3280.84
        scale_factor = (points_per_width / 
                        self.get_distance([xmin, ymin], [xmax, ymin]) / 
                        ft_per_km)

        # make patches of the catchment area

        for i in range(len(c.records())):
            catchment = c.shape(i)
            points = numpy.array(catchment.points)
            subplot.add_patch(self.make_patch(points, facecolor, width = 0.1))

        # get the flowline attributes, make an "updown" dictionary to follow 
        # flow, and change the keys to comids

        with open(VAAfile, 'rb') as f: flowlineVAAs = pickle.load(f)

        updown = {}
        for f in flowlineVAAs:
            if flowlineVAAs[f].down in flowlineVAAs:
                updown[flowlineVAAs[f].comid] = \
                    flowlineVAAs[flowlineVAAs[f].down].comid

        flowlineVAAs = {flowlineVAAs[f].comid:flowlineVAAs[f] 
                        for f in flowlineVAAs}

        # open up and show the flowfiles

        f = Reader(flowfile, shapeType = 3)
        comid_index = f.fields.index(['COMID', 'N',  9, 0]) - 1

        all_comids = [r[comid_index] for r in f.records()]
        
        # get the flows and velocities from the dictionary
        
        widths = []
        comids = []
        for comid in all_comids:
            if comid in flowlineVAAs:
                flow = flowlineVAAs[comid].flow
                velocity = flowlineVAAs[comid].velocity

                # estimate flow width (ft) assuming triangular 90 d channel 

                comids.append(comid)
                widths.append(numpy.sqrt(4 * flow / velocity))
        
        # convert widths in feet to points on the figure; exaggerated by 10

        widths = [w * scale_factor * 20 for w in widths]

        # get the flowline and the corresponding catchment

        for comid, w in zip(comids, widths):

            i = all_comids.index(comid)
            flowline = numpy.array(f.shape(i).points)

            # plot it

            subplot.plot(flowline[:, 0], flowline[:, 1], 'b', lw = w)

        subplot.set_xlabel('Longitude, Decimal Degrees', size = 13)
        subplot.set_ylabel('Latitude, Decimal Degrees',  size = 13)

        # add the NED raster

        im = self.add_raster(subplot, elevfile, resolution, extent, 
                             colormap, 100) 

        divider = make_axes_locatable(subplot)
        cax = divider.append_axes('right', size = 0.16, pad = 0.16)
        colorbar = fig.colorbar(im, cax = cax, orientation = 'vertical')
        colorbar.set_label('Elevation, m', size = 12)
        cbax = pyplot.axes(colorbar.ax)

        for t in cbax.get_yaxis().get_majorticklabels(): t.set_fontsize(10)

        subplot.xaxis.set_major_locator(ticker.MultipleLocator(0.2))
        subplot.yaxis.set_major_locator(ticker.MultipleLocator(0.2))

        if grid:

            subplot.xaxis.grid(True, 'minor', linestyle = '-', linewidth = 0.5)
            subplot.yaxis.grid(True, 'minor', linestyle = '-', linewidth = 0.5)

        # show it

        pyplot.tight_layout()

        if output is not None:  pyplot.savefig(output)

        if show: pyplot.show()

        pyplot.close()
        pyplot.clf()

if __name__ == '__main__': 

    NHDPlus   = '{}/NHDPlus'.format(os.getcwd())      # directory for NHDPlus
    drainid   = 'MA'                                  # NHDPlus drainage area id
    VPU       = '02'                                  # NHDPlus VPU
    directory = 'HSPF_data'                           # HUC8 directory
    HUC8      = '02060006'                            # 8-digit HUC
    flowfile  = '{}/flowlines'.format(directory)      # flowline shapefile
    cfile     = '{}/catchments'.format(directory)     # catchment shapefile
    bfile     = '{}/boundary'.format(directory)       # boundary shapefile
    VAAfile   = '{}/flowlineVAAs'.format(directory)   # VAA file
    elevfile  = '{}/elevations.tif'.format(directory) # NED raster file
    waterplot = '{}/NHDPlus.png'.format(directory)    # plot of the data

    # create an instance of the NHDPlus extractor

    extractor = NHDPlusExtractor(drainid, VPU, NHDPlus)

    # download and decompress the source data

    extractor.download_data()

    # extract the HUC8 data for the Patuxent watershed

    extractor.extract_HUC8(HUC8, directory)

    # make a plot using the extracted files
   
    if not os.path.isfile(waterplot):

        extractor.plot_HUC8(flowfile, cfile, bfile, VAAfile, elevfile,
                            output = waterplot)
