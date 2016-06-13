# delineators.py
#                                                                             
# David J. Lampert (djlampert@gmail.com)
#                                                                             
# last updated: 08/09/2015
#                                                                              
# Purpose: Contains the NHDPlusDelineator class to analyze the NHDPlus data 
# for a watershed and subdivide it according to the criteria specified.

import os, shutil, time, pickle, numpy

from multiprocessing         import Process
from matplotlib              import pyplot, path, patches, colors, ticker
from shapefile               import Reader, Writer
from mpl_toolkits.axes_grid1 import make_axes_locatable
from pyhspf.core             import Watershed, Subbasin

from .vectorutils import combine_shapes, merge_shapes
from .rasterutils import get_raster, get_raster_on_poly, get_raster_in_poly

class NHDPlusDelineator:
    """
    A class to delineate a watershed using the NHDPlus data.
    """

    def __init__(self, 
                 attributes, 
                 flowlines, 
                 catchments, 
                 elevations,
                 gageid   = None,
                 gagefile = None, 
                 damfile  = None,
                 landuse  = None,
                 ):

        self.attributes = attributes
        self.flowlines  = flowlines
        self.catchments = catchments
        self.elevations = elevations
        self.gageid     = gageid
        self.gagefile   = gagefile
        self.damfile    = damfile
        self.landuse    = landuse
        self.gagecomid  = None
        self.updown     = None
        
    def get_distance2(self, 
                      p1, 
                      p2,
                      ):
        """
        Returns the square of the distance between two points.
        """

        return((p1[0] - p2[0])**2 + (p1[1] - p2[1])**2)

    def closest_index(self, 
                      point, 
                      shapes, 
                      warning = False,
                      ):
        """
        Determines the index of the shape in the shapefile that is 
        closest to the point.
        """

        x, y = point[0], point[1]

        # find all flowlines that have a bounding box around the point

        matches = []

        i = 0
        for shape in shapes:
            bbox = shape.bbox
            xmin, ymin, xmax, ymax = bbox[0], bbox[1], bbox[2], bbox[3]

            if xmin < x and x < xmax and ymin < y and y < ymax: 
                matches.append(i)
            i+=1

        if len(matches) == 0:

            if warning: print('unable to find a flowline with appropriate ' +
                              'bounding box, increasing tolerance\n')

            i = 0
            for shape in shapes:

                bbox = shape.bbox

                xmin = bbox[0] - (bbox[2] - bbox[0])
                xmax = bbox[2] + (bbox[2] - bbox[0])
                ymin = bbox[1] - (bbox[3] - bbox[1])
                ymax = bbox[3] + (bbox[3] - bbox[1])

                if xmin < x and x < xmax and ymin < y and y < ymax: 
                    matches.append(i)
                i+=1

        if len(matches) > 1:

            # if more than one bounding box contains the outlet, then find the
            # line with the point closest to the outlet

            if warning:
                print('multiple possible matches found, determining best\n')

            distances = []
            for i in matches:
                shape   = shapes[i]
                bbox   = shape.bbox
                points = shape.points

                distance = max(bbox[2] - bbox[0], bbox[3] - bbox[1])**2
                for p in points:
                    distance = min(distance, self.get_distance2(p, [x, y]))
                distances.append(distance)

            matches = [matches[distances.index(min(distances))]]

        if len(matches) != 1: 
            if warning: print('warning: unable to determine closest flowline')
            return None
        else: return matches[0]

    def get_distance(self, 
                     p1, 
                     p2,
                     ):
        """
        Approximates the distance in kilometers between two points on the 
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

    def get_distance_vector(self, 
                            catchpoints, 
                            closest,
                            ):
        """
        Vectorized version of get_distance method 
        (for computational efficiency).
        """

        deg_rad = numpy.pi / 180
          
        dphis = catchpoints[:, 1] - closest[:, 1]
        phims = 0.5 * (catchpoints[:, 1] + closest[:, 1])
        dlams = catchpoints[:,0] - closest[:,0]

        k1s = (111.13209 - 0.56605 * numpy.cos(2 * phims * deg_rad) + 
               0.00120 * numpy.cos(4 * phims * deg_rad))
        k2s = (111.41513 * numpy.cos(phims * deg_rad) - 0.09455 * 
               numpy.cos(3 * phims * deg_rad) + 0.0012 * 
               numpy.cos(5 * phims * deg_rad))
    
        return numpy.sqrt(k1s**2 * dphis**2 + k2s**2 * dlams**2)

    def get_centroid(self, 
                     points,
                     ):
        """
        Calculates the centroid of a polygon with paired x-y values.
        """

        xs, ys = points[:, 0], points[:, 1]

        a = xs[:-1] * ys[1:]
        b = ys[:-1] * xs[1:]

        A = numpy.sum(a - b) / 2.

        cx = xs[:-1] + xs[1:]
        cy = ys[:-1] + ys[1:]

        Cx = numpy.sum(cx * (a - b)) / (6. * A)
        Cy = numpy.sum(cy * (a - b)) / (6. * A)

        return Cx, Cy

    def get_boundaries(self, 
                       shapefile, 
                       space = 0.1,
                       ):
        """
        Gets the boundaries for the plot.
        """

        boundaries = shapefile.bbox

        xmin = boundaries[0] - (boundaries[2] - boundaries[0]) * space
        ymin = boundaries[1] - (boundaries[3] - boundaries[1]) * space
        xmax = boundaries[2] + (boundaries[2] - boundaries[0]) * space
        ymax = boundaries[3] + (boundaries[3] - boundaries[1]) * space

        return xmin, ymin, xmax, ymax

    def add_raster(self, 
                   fig, 
                   filename, 
                   resolution, 
                   extent, 
                   colormap, 
                   scale,
                   ):
        """
        Adds a rectangular raster image with corners located at the extents
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

        # scale the values (this is converting cm to m)

        zs = zs / scale
        space = 0.1

        # deal with missing values

        mi = numpy.min(zs[numpy.where(zs > 0)])
        ma = zs.max()

        mi, ma = mi - space * (ma - mi), ma + space * (ma - mi)
        norm = colors.Normalize(vmin = mi, vmax = ma)

        # plot the grid

        return fig.imshow(zs, extent = [xmin, xmax, ymin, ymax], norm = norm, 
                          cmap = colormap)

    def make_patch(self,
                   points, 
                   facecolor, 
                   edgecolor = 'Black', 
                   width = 1, 
                   alpha = None,
                   hatch = None, 
                   label = None,
                   ):
        """
        Uses a list or array of points to generate a matplotlib patch.
        """

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

    def find_flowlines(self, 
                       points,
                       ):
        """
        Determines the comids of the flowlines in the flowline shapefile 
        that correspond to the points argument.
        """

        # open the flowline shapefile

        flowlines = Reader(self.flowlines, shapeType = 3)

        # find all the flowline feature attributes

        lines = flowlines.shapes()

        # find the indices of closest flowline for each point

        indices = [self.closest_index(point, lines) for point in points]

        # make a dictionary linking the outlet site index numbers to the 
        # corresponding flowline comids

        comid_index = flowlines.fields.index(['COMID', 'N', 9, 0])  - 1

        comids =[]
        for i in indices:
            if i is not None: comids.append(flowlines.record(i)[comid_index])
            else:             comids.append(None)

        return comids

    def find_comid(self, 
                   point,
                   ):
        """
        Finds the comid of the flowline closest to the point.
        """

        # open the flowline shapefile

        flowlines = Reader(self.flowlines, shapeType = 3)

        # find the index of the comid in the flowline shapefile

        i = self.closest_index(point, flowlines.shapes())

        # find the comid feature attribute

        comid_index = flowlines.fields.index(['COMID', 'N', 9, 0])  - 1

        return flowlines.record(i)[comid_index]

    def find_gagepoint(self, 
                       gageid,
                       ):
        """
        Finds the location of a gage in the gage file.
        """

        # open the gage file

        gagereader = Reader(self.gagefile, shapeType = 1)

        # find the field index of the site number

        site_index = gagereader.fields.index(['SITE_NO', 'C', 15, 0]) - 1
        
        # make a list of all the sites

        sites = [r[site_index] for r in gagereader.records()] 

        # find the index of the gage id

        i = sites.index(gageid)

        # get the longitude and latitude

        p = gagereader.shape(i).points[0]

        return p

    def find_gagecomid(self, 
                       gageid,
                       ):
        """
        Finds the comid of the gage.
        """
        
        if self.gageid is not None:

            # open the attribute file and try to find the gage site id

            with open(self.attributes, 'rb') as f: flowlines = pickle.load(f)

            for f in flowlines:

                if flowlines[f].gageid == self.gageid:

                    print(flowlines[f].comid, flowlines[f].gageid)
                    return flowlines[f].comid

            print('error: unable to locate gage {}\n'.format(self.gageid))
            raise
            
        elif self.gagefile is None:
            print('error: no gage file specified\n')
            raise

        p = self.find_gagepoint(gageid)

        # find the comid

        self.gagecomid = self.find_comid(p)

    def find_subbasin_comids(self, 
                             outletcomid, 
                             verbose = True
                             ):
        """
        Finds the comids of all the flowlines upstream of the outletcomid.
        """

        # open up the attribute file

        with open(self.attributes, 'rb') as f: flowlines = pickle.load(f)

        # make a dictionary linking the hydrologic sequences

        hydroseqs = {flowlines[f].comid: f for f in flowlines}

        # group flowlines into subbasins associated with each outlet

        comids = [outletcomid]

        # make a list of all the comids in the subbasin

        current = [hydroseqs[outletcomid]]
        while len(current) > 0:
            last    = current[:]
            current = []

            for comid in hydroseqs:
                if flowlines[hydroseqs[comid]].down in last:
                    comids.append(comid)
                    current.append(hydroseqs[comid])

        if verbose: print('found {} flowlines\n'.format(len(comids)))

        # convert the comid list to an updown dictionary

        self.updown = {}
        for comid in comids:

            if flowlines[hydroseqs[comid]].down in flowlines:

                down = flowlines[flowlines[hydroseqs[comid]].down].comid
                self.updown[comid] = down

            else: self.updown[comid] = 0

        self.updown[outletcomid] = 0

    def delineate_gage_watershed(self, 
                                 gageid, 
                                 output       = None,
                                 flowlines    = 'flowlines',
                                 catchments   = 'catchments',
                                 boundaryfile = 'boundary',
                                 plotfile     = 'watershed',
                                 verbose      = True,
                                 ):
        """
        Delineates the watershed for the provided NWIS gage id.
        """

        if output is None:              output = os.getcwd()
        elif not os.path.isdir(output): os.mkdir(output)

        # path to the delineated flowline file

        self.subbasinflowlines = '{}/{}'.format(output, flowlines)

        # path to the delineated subbasin file

        self.subbasincatchments = '{}/{}'.format(output, catchments)

        # path to the watershed boundary file

        self.boundary = '{}/{}'.format(output, boundaryfile)
        
        if (not os.path.isfile(self.subbasinflowlines + '.shp') or 
            not os.path.isfile(self.subbasincatchments + '.shp')):

            # find the comid of the flowline associated with the gage

            self.find_gagecomid(gageid)

            # find the upstream comids

            self.find_subbasin_comids(self.gagecomid)

        # extract the flowline shapes from the watershed files

        if not os.path.isfile(self.subbasinflowlines + '.shp'):

            # copy the projection

            shutil.copy(self.flowlines + '.prj', 
                        self.subbasinflowlines + '.prj')

            if verbose: print('reading the flowline file\n')
    
            shapefile = Reader(self.flowlines, shapeType = 3)
            records   = shapefile.records()
    
            # figure out which field codes are the comid
    
            comid_index = shapefile.fields.index(['COMID', 'N',  9, 0]) - 1
    
            # go through the indices and find the comids
    
            if verbose: 
                print('searching for upstream flowlines in the watershed\n')
    
            indices = []
       
            i = 0
            for record in records:
                if record[comid_index] in self.updown: 
                    indices.append(i)
                i+=1

            if len(indices) == 0:
                if verbose: print('error: query returned no values')
                raise
    
            # write the data for the comids to a new shapefile
    
            w = Writer(shapeType = 3)
    
            for field in shapefile.fields: w.field(*field)
    
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
    
            w.save(self.subbasinflowlines)
    
            if verbose: 
                l = len(indices)
                print('queried {} flowlines\n'.format(l))

        # extract the catchment shapes from the watershed files

        if not os.path.isfile(self.subbasincatchments + '.shp'):

            # copy the projection

            shutil.copy(self.flowlines + '.prj', 
                        self.subbasincatchments + '.prj')

            if verbose: print('reading the catchment file\n')
    
            shapefile = Reader(self.catchments, shapeType = 5)
            records   = shapefile.records()
    
            # get the index of the feature id, which links to the flowline comid
    
            feature_index = shapefile.fields.index(['FEATUREID', 'N', 9, 0]) - 1
    
            # go through the indices and find the comids
    
            if verbose: 
                print('searching for upstream catchments in the watershed\n')
    
            indices = []
       
            i = 0
            for record in records:
                if record[feature_index] in self.updown: 
                    indices.append(i)
                i+=1

            if len(indices) == 0:
                if verbose: print('error: query returned no values')
                raise
    
            # write the data for the comids to a new shapefile
    
            w = Writer(shapeType = 5)
    
            for field in shapefile.fields: w.field(*field)
    
            for i in indices:
                shape = shapefile.shape(i)
                w.poly(shapeType = 5, parts = [shape.points])    
                record = records[i]
                w.record(*record)
    
            w.save(self.subbasincatchments)

        # merge the catchments together to form the watershed boundary

        if not os.path.isfile(self.boundary + '.shp'):
        
            if verbose: print('merging the catchments to form a boundary\n')
    
            print('{}/{}'.format(output, catchments))
            merge_shapes('{}/{}'.format(output, catchments), 
                         outputfile = self.boundary)

        # make a plot of the watershed

        pfile = '{}/{}.png'.format(output, plotfile)
        if not os.path.isfile(pfile):
            self.plot_delineated_watershed(gageid = gageid, output = pfile)

    def delineate_watershed(self, 
                            lon,
                            lat,
                            output       = None,
                            flowlines    = 'flowlines',
                            catchments   = 'catchments',
                            boundaryfile = 'boundary',
                            plot         = None,
                            verbose      = True,
                            ):
        """
        Delineates the watershed for the provided point using the NHDPlus
        data for the given longitude, latitude.
        """

        if output is None:              output = os.getcwd()
        elif not os.path.isdir(output): os.mkdir(output)

        # path to the delineated flowline file

        self.subbasinflowlines = '{}/{}'.format(output, flowlines)

        # path to the delineated subbasin file

        self.subbasincatchments = '{}/{}'.format(output, catchments)

        # path to the watershed boundary file

        self.boundary = '{}/{}'.format(output, boundaryfile)
        
        if (not os.path.isfile(self.subbasinflowlines + '.shp') or 
            not os.path.isfile(self.subbasincatchments + '.shp')):

            # find the comid of the flowline associated with the point
            # and then trace the upstream comids from that location

            comids = self.find_subbasin_comids(self.find_comid((lon, lat)))

        # extract the flowline shapes from the watershed files

        if not os.path.isfile(self.subbasinflowlines + '.shp'):

            # copy the projection

            shutil.copy(self.flowlines + '.prj', 
                        self.subbasinflowlines + '.prj')

            if verbose: print('reading the flowline file\n')
    
            shapefile = Reader(self.flowlines, shapeType = 3)
            records   = shapefile.records()
    
            # figure out which field codes are the comid
    
            comid_index = shapefile.fields.index(['COMID', 'N',  9, 0]) - 1
    
            # go through the indices and find the comids
    
            if verbose: 
                print('searching for upstream flowlines in the watershed\n')
    
            indices = []
       
            i = 0
            for record in records:
                if record[comid_index] in self.updown: 
                    indices.append(i)
                i+=1

            if len(indices) == 0:
                if verbose: print('error: query returned no values')
                raise
    
            # write the data for the comids to a new shapefile
    
            w = Writer(shapeType = 3)
    
            for field in shapefile.fields: w.field(*field)
    
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
    
            w.save(self.subbasinflowlines)
    
            if verbose: 
                l = len(indices)
                print('queried {} flowlines\n'.format(l))

        # extract the catchment shapes from the watershed files

        if not os.path.isfile(self.subbasincatchments + '.shp'):

            # copy the projection

            shutil.copy(self.flowlines + '.prj', 
                        self.subbasincatchments + '.prj')

            if verbose: print('reading the catchment file\n')
    
            shapefile = Reader(self.catchments, shapeType = 5)
            records   = shapefile.records()
    
            # get the index of the feature id, which links to the flowline comid
    
            feature_index = shapefile.fields.index(['FEATUREID', 'N', 9, 0]) - 1
    
            # go through the indices and find the comids
    
            if verbose: 
                print('searching for upstream catchments in the watershed\n')
    
            indices = []
       
            i = 0
            for record in records:
                if record[feature_index] in self.updown: 
                    indices.append(i)
                i+=1

            if len(indices) == 0:
                if verbose: print('error: query returned no values')
                raise
    
            # write the data for the comids to a new shapefile
    
            w = Writer(shapeType = 5)
    
            for field in shapefile.fields: w.field(*field)
    
            for i in indices:
                shape = shapefile.shape(i)
                w.poly(shapeType = 5, parts = [shape.points])    
                record = records[i]
                w.record(*record)
    
            w.save(self.subbasincatchments)

        # merge the catchments together to form the watershed boundary

        if not os.path.isfile(self.boundary + '.shp'):
        
            if verbose: print('merging the catchments to form a boundary\n')
    
            print('{}/{}'.format(output, catchments))
            merge_shapes('{}/{}'.format(output, catchments), 
                         outputfile = self.boundary)

        if plot is not None:

            f = '{}/{}'.format(output, plot)
            self.plot_delineated_watershed(point = (lon, lat), output = plot)

    def get_overland(self, 
                     catchpoints, 
                     closest, 
                     tol = 0.1, 
                     min_slope = 0.00001
                     ):
        """
        Returns the slope of the z-coordinate in the x-y plane between points
        p1 and p2.  Returns the min_slope if the points are too close together
        as specified by the tolerance (m).  Also return half the average length
        from the catchment boundary to the flowline (since the average length 
        across each line is half the total length).
        """

        length = self.get_distance_vector(catchpoints, closest)
        slope  = (catchpoints[:,2] - closest[:,2]) / length / 100000

        for l, s in zip(length, slope):
            if l < tol: l, s = tol, min_slope

        return (length / 2.).mean() * 1000, slope.mean()

    def calculate_flowplane(self, 
                            flowline, 
                            catchment, 
                            verbose = False,
                            ):
        """
        Gets the elevation data from the NED raster then estimates the value 
        of the overland flow plane length and slope.
        """

        catchpoints = get_raster_on_poly(self.elevations, catchment.points,
                                         verbose = verbose)
        catchpoints = numpy.array([p for p in catchpoints])

        zs = get_raster(self.elevations, flowline.points)

        flowpoints = numpy.array([[p[0], p[1], z] 
                                  for p, z in zip(flowline.points, zs)])

        # iterate through the raster values and find the closest flow point

        closest = numpy.empty((len(catchpoints), 3), dtype = 'float')

        for point, j in zip(catchpoints, range(len(catchpoints))):
            closest[j] = flowpoints[numpy.dot(flowpoints[:, :2], 
                                              point[:2]).argmin()]

        # estimate the slope and overland flow plane length

        f = self.get_overland(catchpoints, closest)

        if verbose: 
            print('flowplane length = {:5.0f} m; slope = {:.4f}'.format(*f))

        return f

    def add_basin_landuse(self, 
                          year, 
                          landuse,
                          ):
        """
        Adds basin-wide land use data to the extractor.
        """

        self.landuseyear = year
        self.landuse     = landuse

    def build_gage_watershed(self, 
                             gageid,
                             output,
                             landuse = None,
                             landuseyear = 1988,
                             masslinkplot = None,
                             ):
        """
        Uses the data to build an instance of the Watershed class.
        """

        # set the outlet comid

        self.find_gagecomid(gageid)

        # set the updown dictionary if needed

        if self.updown is None: 
            self.find_subbasin_comids(self.gagecomid, verbose = False)

        # create a dictionary to store subbasin data

        self.subbasins = {}

        # create a dictionary to keep track of subbasin inlets

        inlets = {}

        # open the catchment and flowline shapefiles

        cfile = Reader(self.subbasincatchments, shapeType = 5)
        ffile = Reader(self.subbasinflowlines, shapeType = 3)

        feature_index = cfile.fields.index(['FEATUREID', 'N',  9,  0]) - 1
        area_index    = cfile.fields.index(['AreaSqKM',  'N', 19,  6]) - 1
        comid_index   = ffile.fields.index(['COMID',     'N', 9,   0]) - 1
        gnis_index    = ffile.fields.index(['GNIS_NAME', 'C', 65,  0]) - 1
        len_index     = ffile.fields.index(['LENGTHKM',  'N', 19, 11]) - 1

        fcomids = ['{}'.format(r[comid_index]) for r in ffile.records()]

        # open the flowline attrbitue file

        with open(self.attributes, 'rb') as f: flowlineVAAs = pickle.load(f)

        # re-organize by comid

        flowlineVAAs = {'{}'.format(flowlineVAAs[f].comid):flowlineVAAs[f] 
                        for f in flowlineVAAs}

        # iterate through the catchments and make subbasins

        for i in range(len(cfile.records())):

            # get the catchment record

            crecord = cfile.record(i)

            # get the comid and catchment area

            comid = '{}'.format(crecord[feature_index])
            area  = crecord[area_index]

            # find the corresponding flowline

            j = fcomids.index(comid)

            # get the flowline record

            frecord = ffile.record(j)

            # get the VAAs

            VAAs = flowlineVAAs[comid]

            # get the flowline and catchment shapes

            flowline  = ffile.shape(j)
            catchment = cfile.shape(i)

            # calculate the length and slope of the flow plane

            length, slope = self.calculate_flowplane(flowline, catchment)

            # calculate the centroid

            combined = [[float(x), float(y)] for x, y in catchment.points]
            centroid = self.get_centroid(numpy.array(combined + [combined[0]]))

            # calculate the average elevation

            elev_matrix, origin = get_raster_in_poly(self.elevations, combined,
                                                     verbose = False)

            elev_matrix = elev_matrix.flatten()
            elev_matrix = elev_matrix[elev_matrix.nonzero()]
    
            elevation = round(elev_matrix.mean() / 100., 2)

            subbasin  = Subbasin(comid)

            # read the flow plane data

            subbasin.add_flowplane(length, slope, centroid, elevation)

            # get the GNIS name and length from the flowline shapefile

            gnis = frecord[gnis_index]

            if isinstance(gnis, bytes): gnis = ''

            lenkm = frecord[len_index]

            # get the max and min elevation, average flow and velocity from
            # the attribute file

            maxelev = VAAs.maxelev / 100 # convert to m
            minelev = VAAs.minelev / 100 # convert to m

            subbasin.add_reach(gnis, maxelev, minelev, lenkm, flow = VAAs.flow,
                               velocity = VAAs.velocity, 
                               traveltime = VAAs.traveltime)

            # add the landuse

            if landuse is None:

                # if None provided use the basin-wide data if available

                if self.landuse is None:
                
                    print('error no landuse data provided\n')
                    raise

                else:

                    landtypes, areas = zip(*self.landuse.items())

                    landtypes = list(landtypes)
                    data = [round(area * a / sum(areas), 3) for a in areas]

                    subbasin.add_landuse(self.landuseyear, landtypes, data)

            else:

                landtypes, data = landuse[comid]

                subbasin.add_landuse(landuseyear, landtypes, data)

            # store the subbasin info in the dictionary

            self.subbasins[comid] = subbasin

        # create an instance of the watershed class

        watershed = Watershed(gageid, self.subbasins)

        # establish the mass linkages between the subbasins (convert to strings)

        updown = {'{}'.format(up):(0 if down == 0 else '{}'.format(down))
                  for up, down in self.updown.items()}

        watershed.add_mass_linkage(updown)

        # add the gage location as an outlet

        watershed.add_outlet('{}'.format(self.gagecomid))

        if masslinkplot is not None: 

            self.plot_mass_flow(watershed, masslinkplot)

        # dump the watershed to the output file

        with open(output, 'wb') as f: pickle.dump(watershed, f)

    def plot_mass_flow(self, 
                       watershed, 
                       output, 
                       title = 'Mass Flow Diagram',
                       fontsize = 6, 
                       theight = 0.2, 
                       l = 8.5, 
                       w = 11, 
                       verbose = False, 
                       overwrite = False,
                       ):
        """
        Makes a schematic of the mass linkages between the various subbasins
        in a watershed.
        """

        if os.path.exists(output) and not overwrite:
            if verbose: print('file %s exists' % output)
            return
        elif verbose: print('generating a mass linkage plot\n')

        fontheight = fontsize / 72.
        rheight = 3 * fontheight
        rwidth  = 12 * fontheight
        xgap = fontheight
        ygap = rheight
        awidth = rheight / 4
        aheight = rheight / 3

        # set up a sheet to write the image

        fig = pyplot.figure(figsize = (w, l))

        ax  = fig.add_subplot(111, aspect = 'equal')
        ax.get_xaxis().set_visible(False)
        ax.get_yaxis().set_visible(False)
        t = ax.set_title(title)

        # divide the subbasins into rows and put them on the chart
        # start at the bottom to organize the linkages better

        rows = [watershed.outlets, ['outlet']]

        top = False
        while not top:
            row = []
            for next in rows[0]:
                for subbasin in watershed.updown:
                    if watershed.updown[subbasin] == next: row.append(subbasin)
            if len(row) > 0: rows.insert(0, row)
            else: top = True

        # add an inlet box in the row above each inlet

        for inlet in watershed.inlets: 
            i = 0
            while i < len(rows) - 1:
                for subbasin in rows[i]:
                    if subbasin == inlet:
                    
                        # find the position of the subbasin in the chart

                        j = rows[i].index(inlet)

                        if i > 0:

                            # figure out where the subbasins point
                        
                            updowns = [watershed.updown[s] for s in rows[i-1]]
                            
                            # if first or last, add it there in the row above

                            if   j == 0:                
                                rows[i-1].insert(0, 'inlet')
                            elif j == len(rows[i]) - 1: 
                                rows[i-1].append('inlet')
                            else:

                                # find the place to add in the preceeding row 

                                n = updowns.index(rows[i][j-1]) + 1
                                rows[i-1].insert(n, 'inlet')
                i += 1

        # write the subbasin boxes to the chart

        middle = numpy.ceil(w // (rwidth + xgap)) // 2
        last = 0

        # keep track of the bounding box of the plot

        xmin, ymin, xmax, ymax = middle, 0, middle, 0

        for i in range(len(rows)):

            row = rows[i]
        
            y = (ygap + rheight) * i + theight

            # figure out which cell to put in the main column

            if i == 0:
                main = row[(len(row) - 1) // 2]
            elif i < len(rows) - 1:
                main = watershed.updown[rows[i-1][last]]
            else: main = 'outlet'

            start = middle - row.index(main)

            if i < len(rows) - 1: next_row = rows[i + 1]

            for subbasin in row:
                x = (rwidth + xgap) * (start + row.index(subbasin))
                r = patches.Rectangle((x, y), rwidth, rheight, fill = False)

                # adjust the bounding box

                if x           < xmin: xmin = x
                if x + rwidth  > xmax: xmax = x + rwidth
                if y           < ymin: ymin = y
                if y + rheight > ymax: ymax = y + rheight

                if subbasin != 'outlet': ax.add_patch(r)

                b = ax.text(x + rwidth / 2, y + rheight / 2, subbasin,
                            horizontalalignment = 'center',
                            verticalalignment   = 'center')

                # draw the arrow

                if i < len(rows) - 1:

                    x1 = x + rwidth / 2

                    if i < len(rows) - 2 and subbasin != 'inlet':
                        next = watershed.updown[subbasin]
                        m    = watershed.updown[main]
                        next_start = middle - next_row.index(m)
                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(next))
                              + rwidth / 2)
                    elif subbasin == 'inlet':
                        next = watershed.inlets[0]
                        m = watershed.updown[main]
                        next_start = middle - next_row.index(m)
                        x2 = ((rwidth + xgap) * 
                              (next_start + next_row.index(next))
                              + rwidth / 2)
                    else:
                        next_start = middle
                        x2 = ((rwidth + xgap) * (middle) + rwidth / 2)

                    a = pyplot.arrow(x1, y + rheight, x2 - x1, ygap, 
                                     head_width = awidth, head_length = aheight,
                                     fc = 'k', ec = 'k', 
                                     length_includes_head = True)
                    ax.add_patch(a)

            last = row.index(main)
            i += 1
        
        pad = 0.02

        xmin = xmin - (xmax - xmin) * pad
        xmax = xmax + (xmax - xmin) * pad
        ymin = ymin - (ymax - ymin) * pad
        ymax = ymax + (ymax - ymin) * pad

        ax.set_xlim(xmin, xmax)
        ax.set_ylim(ymax, ymin)
        pyplot.axis('off')

        pyplot.savefig(output, dpi = 200)
        pyplot.clf()
        pyplot.close()

    def plot_delineated_watershed(self,
                                  gageid     = None,
                                  point      = None,
                                  title      = None,
                                  cmap       = 'gist_earth',
                                  resolution = 200,
                                  vmin       = 0.1,
                                  output     = None,
                                  show       = False,
                                  verbose    = True,
                                  ):
        """
        Makes a plot of the delineated watershed.
        """

        if verbose: 
            print('generating plot of watershed for point\n')

        fig = pyplot.figure()
        subplot = fig.add_subplot(111, aspect = 'equal')
        subplot.tick_params(axis = 'both', which = 'major', labelsize = 10)

        # open up and show the catchments

        facecolor = (1,0,0,0.)

        b = Reader(self.boundary, shapeType = 5)

        points = numpy.array(b.shape(0).points)
        subplot.add_patch(self.make_patch(points, facecolor = facecolor, 
                                          width = 1.))

        xmin, ymin, xmax, ymax = self.get_boundaries(b, space = 0.02)

        # figure out how far one foot is on the map

        points_per_width = 72 * 8
        ft_per_km = 3280.84
        scale_factor = (points_per_width / 
                        self.get_distance([xmin, ymin], [xmax, ymin]) / 
                        ft_per_km)

        s = Reader(self.subbasincatchments, shapeType = 5)

        # make patches of the subbasins

        for i in range(len(s.records())):
            shape = s.shape(i)
            points = numpy.array(shape.points)
            subplot.add_patch(self.make_patch(points, facecolor, width = 0.2))

        # get all the comids in the watershed

        f = Reader(self.subbasinflowlines, shapeType = 3)
        comid_index = f.fields.index(['COMID', 'N',  9, 0]) - 1

        all_comids = [r[comid_index] for r in f.records()]
    
        # get the flowline attributes, make an "updown" dictionary to follow 
        # flow, and change the keys to comids
    
        with open(self.attributes, 'rb') as f: flowlineVAAs = pickle.load(f)
    
        updown = {item.comid: flowlineVAAs[flowlineVAAs[key].down].comid
                  for key, item in flowlineVAAs.items()
                  if item.comid in all_comids}
    
        flowlineVAAs = {flowlineVAAs[f].comid:flowlineVAAs[f] 
                        for f in flowlineVAAs
                        if flowlineVAAs[f].comid in all_comids}
        
        # find the flowlines in the main channel
    
        f = Reader(self.subbasinflowlines, shapeType = 3)
    
        comid_index = f.fields.index(['COMID', 'N',  9, 0]) - 1
        comids = [r[comid_index] for r in f.records()]

        # get the flows and velocities from the dictionary
    
        widths = []
        for comid in comids:
            flow     = flowlineVAAs[comid].flow
            velocity = flowlineVAAs[comid].velocity
    
            # estimate the flow width in feet assuming triangular 90 deg channel

            if velocity > 0: widths.append(numpy.sqrt(4 * flow / velocity))
            else:            widths.append(numpy.sqrt(4 * flow / vmin))
    
        # convert widths in feet to points on the figure; exaggerated by 10
    
        widths = [w * scale_factor * 10 for w in widths]
        
        # show the flowlines
    
        for comid, w in zip(comids, widths):
    
            i = all_comids.index(comid)
            flowline = numpy.array(f.shape(i).points)
    
            # plot it
    
            subplot.plot(flowline[:, 0], flowline[:, 1], 'b', lw = w)
    
        # find the outlet and get the GNIS name and elevations
    
        i = 0
        while updown[comids[i]] in updown: i+=1
        gnis_name = f.record(all_comids.index(comids[i]))[4]
    
        if gageid is not None: point = self.find_gagepoint(gageid)

        # show the point

        if point is not None:
            subplot.scatter(point[0], point[1], marker = 'o', c = 'r', s = 60)
    
        subplot.set_xlabel('Longitude, Decimal Degrees', size = 13)
        subplot.set_ylabel('Latitude, Decimal Degrees',  size = 13)
    
        subplot.xaxis.set_major_formatter(ticker.ScalarFormatter('%.1f'))
        subplot.ticklabel_format(useOffset=False)

        # add the raster using the min and max elev to set the countours
    
        extent = xmin, ymin, xmax, ymax
        im = self.add_raster(subplot, self.elevations, resolution, extent, 
                             cmap, 200) 
    
        divider = make_axes_locatable(subplot)
        cax = divider.append_axes('right', size = 0.16, pad = 0.16)
        colorbar = fig.colorbar(im, cax = cax, orientation = 'vertical')
        colorbar.set_label('Elevation, m', size = 12)
        cbax = pyplot.axes(colorbar.ax)
        yaxis = cbax.get_yaxis()
        ticks = yaxis.get_majorticklabels()
        for t in ticks: t.set_fontsize(10)
    
        # add the title
    
        if not isinstance(gnis_name, bytes) > 0: descrip = ', ' + gnis_name
        else:                                    descrip = ''
    
        if title is None and point is not None: 
            its = point[0], point[1], descrip
            title = 'Watershed for {:7.4f}, {:7.4f}{}'.format(*its)
        elif title is None and gageid is not None:
            title = 'Watershed for {}{}'.format(gageid, descrip)
    
        subplot.set_title(title, fontsize = 14)
    
        # show it
    
        pyplot.tight_layout()
    
        if output is not None: pyplot.savefig(output)

        if show: pyplot.show()
    
        pyplot.close()

class HUC8Delineator(NHDPlusDelineator):
    """
    An NHDPlusDelineator child to perform watershed delineation with 
    NHDPlus data from a HUC8 that builds subbasins based on a maximum area, 
    location of dams, and location of NWIS gages following extraction with
    the NHDPlusExtractor.
    """

    def __init__(self,
                 HUC8,
                 attributes, 
                 flowlines, 
                 catchments, 
                 elevations,
                 gagefile, 
                 damfile,
                 VAAs = None,
                 landuse = None,
                 ):

        NHDPlusDelineator.__init__(self, 
                                   attributes, 
                                   flowlines, 
                                   catchments, 
                                   elevations,
                                   gageid   = None,
                                   gagefile = gagefile, 
                                   damfile  = damfile,
                                   landuse  = landuse,
                                   )
        self.HUC8 = HUC8
        self.VAAs = VAAs

    def combine_flowlines(self, 
                          inputfile,
                          outputfile, 
                          overwrite = False,
                          verbose = True
                          ):
        """
        Merges the major flowlines in the input shapefile into a single
        shape (the principal stream) and writes it to the output shapefile.
        """

        if os.path.isfile(outputfile + '.shp') and not overwrite:
            if verbose: print('combined flowline shapefile ' +
                              '{} exists'.format(outputfile))
            return

        # start by copying the projection files

        shutil.copy(inputfile + '.prj', outputfile + '.prj')

        if self.VAAs is None: self.get_flowlines(verbose = False)

        flowlines = self.VAAs

        # all the fields for the combined flowline feature class

        fields = [['OutComID',   'N',  9, 0], 
                  ['GNIS_NAME',  'C', 65, 0],
                  ['REACHCODE',  'C',  8, 0],
                  ['InletComID', 'N',  9, 0],
                  ['MaxElev',    'N',  9, 2],
                  ['MinElev',    'N',  9, 2],
                  ['SlopeLenKM', 'N',  6, 2],
                  ['Slope',      'N',  8, 5],
                  ['InFlowCFS',  'N',  8, 3],
                  ['OutFlowCFS', 'N',  8, 3],
                  ['VelFPS',     'N',  7, 4],
                  ['TravTimeHR', 'N',  8, 2],
                  ]

        # go through the reach indices, add add them to the list of flowlines if
        # they are in the watershed, and make a list of the corresponding comids
  
        shapefile = Reader(inputfile, shapeType = 3)
        records   = shapefile.records()

        # figure out which field code is the comid, reachcode, and gnis name

        comid_index = shapefile.fields.index(['COMID',     'N',  9, 0]) - 1
        reach_index = shapefile.fields.index(['REACHCODE', 'C', 14, 0]) - 1
        gnis_index  = shapefile.fields.index(['GNIS_NAME', 'C', 65, 0]) - 1

        all_comids = [r[comid_index] for r in records]
    
        # make a dictionary linking the hydrologic sequence

        updown = {f: flowlines[f].down for f in flowlines 
                  if flowlines[f].comid in all_comids}
        downup = {f: flowlines[f].up   for f in flowlines
                  if flowlines[f].comid in all_comids}

        # check if there is an inlet

        inlets = []
        for f in downup:
            if downup[f] != 0 and downup[f] not in downup:
                inlets.append(f)

        if len(inlets) == 0:

            # then pick a flowline and follow it to the end of the watershed

            current = list(updown.keys())[0]

            while updown[current] in updown: current = updown[current]

            primary = [current]
            while downup[current] in downup:
                current = downup[current]
                primary.insert(0, current)

        else:

            # start at the inlet and follow it to the outlet

            current = inlets[0]
            primary = [inlets[0]]
            while updown[current] in updown:
                current = updown[current]
                primary.append(current)

        inlet_comid = flowlines[primary[0]].comid
        last_comid  = flowlines[primary[-1]].comid

        inlet_flow  = round(flowlines[primary[0]].flow, 3)
        outlet_flow = round(flowlines[primary[-1]].flow, 3)
        velocity    = round(flowlines[primary[-1]].velocity, 4)
        traveltime  = round(sum([flowlines[f].traveltime for f in primary]), 3)

        # use the attributes for the last flowline for the combined flowline

        top    = flowlines[primary[0]].maxelev
        bottom = flowlines[primary[-1]].minelev
        length = round(sum([flowlines[f].length for f in primary]), 2)

        estimated_slope = round((top - bottom) / 100000 / length, 6)

        if estimated_slope < 0.00001: slope = 0.00001
        else:                         slope = estimated_slope

        # write the data from the HUC8 to a new shapefile

        w = Writer(shapeType = 3)

        # the fields will be effluent comid, GNIS name, the length (km), 
        # the 8-digit reach code, the slope, the flow at the inlet and outlet,
        # the velocity in ft/s, and the travel time in hours

        for field in fields: w.field(*field)

        last_index = all_comids.index(last_comid)
        if isinstance(records[last_index][gnis_index], bytes):
            gnis = records[last_index][gnis_index].decode('utf-8')
        else: gnis = records[last_index][gnis_index]

        r = [last_comid, gnis, records[last_index][reach_index][:8], 
             inlet_comid, top, bottom, length, slope, inlet_flow, 
             outlet_flow, velocity, traveltime]

        w.record(*r)

        points = []
        for f in primary:
            shape = shapefile.shape(all_comids.index(flowlines[f].comid))

            for p in shape.points:
                if p not in points: points.append(p)

        w.poly(shapeType = 3, parts = [points])

        w.save(outputfile)

        if verbose: 
            print('successfully combined subbasin ' +
                  '{} flowlines'.format(last_comid))

    def combine_catchments(self, 
                           catchments, 
                           flowlines, 
                           comid, 
                           output, 
                           overwrite = False, 
                           verbose = True,
                           vverbose = False,
                           ):
        """
        Combines together all the catchments in a basin catchment shapefile.
        Creates a new shapefile called "combined" in the same directory as the 
        original file.  Uses the elevation data from the raster file and the 
        flow data file to estimate the length and average slope of the 
        overland flow plane.
        """

        t0 = time.time()
        numpy.seterr(all = 'raise')

        if os.path.isfile(output + '.shp') and not overwrite:
            if verbose: 
                print('combined catchment shapefile {} exists'.format(output))
            return
   
        if verbose: print('combining catchments from {}'.format(catchments))

        # start by copying the projection files

        shutil.copy(catchments + '.prj', output + '.prj')

        # load the catchment and flowline shapefiles

        c = Reader(catchments, shapeType = 5)
        f = Reader(flowlines,  shapeType = 3)

        # make lists of the comids and featureids

        featureid_index = c.fields.index(['FEATUREID', 'N', 9, 0]) - 1
        comid_index     = f.fields.index(['COMID', 'N', 9,  0])    - 1

        featureids = [r[featureid_index] for r in c.records()]
        comids     = [r[comid_index]     for r in f.records()]

        # check that shapes are traceable--don't have multiple points and start
        # and end at the same place--then make an appropriate list of shapes
        # and records--note it's more memory efficient to read one at a time

        n = len(c.records())
        shapes  = []
        records = [] 
        bboxes  = []

        combined = combine_shapes(c.shapes(), verbose = vverbose)

        # iterate through the catchments and get the elevation data from NED
        # then estimate the value of the overland flow plane length and slope

        lengths = numpy.empty((n), dtype = 'float')
        slopes  = numpy.empty((n), dtype = 'float')

        for i in range(n):

            catchment = c.shape(i)
            flowline  = f.shape(comids.index(featureids[i]))

            catchpoints = get_raster_on_poly(self.elevations, 
                                             catchment.points,
                                             verbose = vverbose
                                             )
            catchpoints = numpy.array([p for p in catchpoints])

            zs = get_raster(self.elevations, 
                            flowline.points, 
                            quiet = True
                            )

            flowpoints = numpy.array([[p[0], p[1], z] 
                                      for p, z in zip(flowline.points, zs)])

            # iterate through the raster values and find the closest flow point

            closest = []
            for point in catchpoints:
                j = numpy.dot(flowpoints[:,:2], point[:2]).argmin()
                closest.append(flowpoints[j])

            closest = numpy.array(closest)

            # estimate the slope and overland flow plane length

            length, slope = self.get_overland(catchpoints, closest)

            if vverbose: 
                print('avg slope and length =', slope.mean(), length.mean())

            lengths[i], slopes[i] = length.mean(), slope.mean()

        if vverbose: print('\nfinished overland flow plane calculations\n')

        # get area of the subbasin from the catchment metadata

        areasq_index = c.fields.index(['AreaSqKM', 'N', 19, 6]) - 1
        areas        = numpy.array([r[areasq_index] for r in c.records()])

        # take the area weighted average of the slopes and flow lengths

        tot_area   = round(areas.sum(), 2)
        avg_length = round(numpy.sum(areas * lengths) / tot_area, 1)
        avg_slope  = round(numpy.sum(areas * slopes) / tot_area, 4)

        # get the centroid and the average elevation

        combined = [[float(x), float(y)] for x, y in combined]
        centroid = self.get_centroid(numpy.array(combined + [combined[0]]))

        Cx, Cy = round(centroid[0], 4), round(centroid[1], 4)

        elev_matrix, origin = get_raster_in_poly(self.elevations, 
                                                 combined, 
                                                 verbose = vverbose
                                                 )

        elev_matrix = elev_matrix.flatten()
        elev_matrix = elev_matrix[elev_matrix.nonzero()]
    
        avg_elev = round(elev_matrix.mean() / 100., 2)

        # write the data to the shapefile

        w = Writer(shapeType = 5)

        fields = [['ComID',      'N',  9, 0],
                  ['PlaneLenM',  'N',  8, 2],
                  ['PlaneSlope', 'N',  9, 6],
                  ['AreaSqKm',   'N', 10, 2],
                  ['CenX',       'N', 12, 6],
                  ['CenY',       'N', 12, 6],
                  ['AvgElevM',   'N',  8, 2]]

        record = [comid, avg_length, avg_slope, tot_area, Cx, Cy, avg_elev]

        for field in fields:  w.field(*field)
    
        w.record(*record)
    
        w.poly(shapeType = 5, parts = [combined])

        w.save(output)

        if vverbose: print('\ncompleted catchment combination in ' +
                           '{:.1f} seconds\n'.format(time.time() - t0))

    def get_flowlines(self,
                      verbose = True
                      ):
        """
        Returns the (formatted) flowline value added attributes.
        """

        # open the catchment file to make sure only flowlines with a catchment
        # are used as subbasins

        cfile = Reader(self.catchments)        
        i = [f[0] for f in cfile.fields].index('FEATUREID') - 1
        catchments = [r[i] for r in cfile.records()]

        # open up the flowline data in a dictionary using hydroseqs as keys 
        # and make a dictionary linking the comids to hydroseqs

        with open(self.attributes, 'rb') as f: flowlines = pickle.load(f)

        # find flowlines without catchments but with flow in and out

        missing = [f for f in flowlines 
                   if flowlines[f].comid not in catchments]

        # keep track of removed flowlines

        self.removed = {}

        for f in missing:

            up = [o for o in flowlines 
                  if flowlines[o].down == f]
            down = [o for o in flowlines 
                    if flowlines[o].up == f]

            branches = [o for o in flowlines 
                        if flowlines[o].up == flowlines[f].up and
                        flowlines[o].up != 0 and o != f]

            if len(branches) == 0 and flowlines[f].up != 0:

                its = [flowlines[f].comid, flowlines[flowlines[f].up].comid, 
                       flowlines[flowlines[f].down].comid]
                if verbose:

                    print('{} is in between {} and {}'.format(*its) +
                          ', short-circuiting {}'.format(its[0]))

                # short-circuit the problem reach

                flowlines[flowlines[f].up].down        = flowlines[f].down
                flowlines[flowlines[f].up].length     += flowlines[f].length
                flowlines[flowlines[f].up].traveltime += flowlines[f].traveltime
                flowlines[flowlines[f].up].minelev     = flowlines[f].minelev
                flowlines[flowlines[f].down].up        = flowlines[f].up

                # remove flowlines without catchments

                removed = flowlines.pop(f)
                self.removed[removed.hydroseq] = removed

            else:

                if verbose:

                    i = flowlines[f].comid
                    print('{} is not between streams, ignoring'.format(i))

        self.VAAs = flowlines

    def make_subbasin_outlets(self,
                              extras   = None,
                              years    = None,
                              drainmax = None,
                              verbose  = True,
                              ):
        """
        Creates a feature class of outlets containing all the data needed 
        for HSPF simulations.
        """

        verbose = True
        if verbose: print('subdividing watershed\n')

        # use subbasin delineation criteria to make a list of inlets and outlets

        inlets = []

        if extras is None: outlets = []
        else:              outlets = extras
        
        # get the flowline value-added attributes

        if self.VAAs is None: self.get_flowlines()

        flowlines = self.VAAs

        # map comids to hydrological sequence

        hydroseqs  = {flowlines[f].comid: f for f in flowlines}

        # find the dam comids if a dam shapefile is provided

        if self.damfile is not None:

            # read the dam file to find the outlet points

            damreader  = Reader(self.damfile, shapeType = 1)
            dampoints  = [s.points[0] for s in damreader.shapes()]
            damrecords = damreader.records()

            nid_index = damreader.fields.index(['NIDID', 'C', 7, 0]) - 1

            dam_comids = self.find_flowlines(dampoints)

            for comid in dam_comids:
                
                if comid is not None and comid in hydroseqs: 
                    
                    outlets.append(comid)

        else: dam_comids = []

        # find the gages if a gage shapefile is provided

        if self.gagefile is not None:

            # check the gages and see if they meet the criteria for outlets

            gagereader  = Reader(self.gagefile, shapeType = 1)
            gagerecords = gagereader.records()

            # figure out which field codes are the HUC8, the first day, the site
            # number, the drainage area, and the average 

            day1_index  = gagereader.fields.index(['DAY1',    'N', 19, 0]) - 1
            dayn_index  = gagereader.fields.index(['DAYN',    'N', 19, 0]) - 1
            drain_index = gagereader.fields.index(['DA_SQ_MILE','N',19,2]) - 1
            HUC8_index  = gagereader.fields.index(['HUC',     'C',  8, 0]) - 1
            site_index  = gagereader.fields.index(['SITE_NO', 'C', 15, 0]) - 1
            nwis_index  = gagereader.fields.index(['NWISWEB', 'C', 75, 0]) - 1
            ave_index   = gagereader.fields.index(['AVE',     'N', 19, 3]) - 1

            gage_comids = self.find_flowlines([s.points[0] 
                                               for s in gagereader.shapes()])

            # list of the comids of the gage outlets

            gage_outlets = []

            # iterate through the gages and see that they meet the criteria

            for record, comid in zip(gagerecords, gage_comids):

                # make sure there are data

                data_criteria = (comid is not None)

                # check the gage has data during the years if provided

                if years is not None:

                    first_criteria, last_criteria = years
                    first_gage = int(str(record[day1_index])[:4])
                    last_gage  = int(str(record[dayn_index])[:4])

                    year_criteria = (first_gage <= last_criteria and
                                     last_gage  >= first_criteria)

                else: year_criteria = True

                # make sure it is not an inlet and that it's in the watershed
            
                watershed_criteria = (record[HUC8_index] == self.HUC8)

                existing = comid not in outlets

                if all([data_criteria, year_criteria, watershed_criteria, 
                        existing]):

                    gage_outlets.append(comid)

                    if verbose:
 
                        i = comid, record[site_index]
                        print('adding outlet {} for gage station {}'.format(*i))

        else: gage_outlets = []

        # add the gage stations meeting the criteria as outlets

        for comid in gage_outlets: outlets.append(comid)

        # find all the inlets

        for f in flowlines:
            if flowlines[f].up not in flowlines and flowlines[f].up != 0:
                if verbose:
                    its = flowlines[f].comid, flowlines[f].up
                    print('found inlet {} ({}) not in watershed'.format(*its))
                inlets.append(flowlines[f].comid)

        # find the watershed outlet using the drainage area

        max_area   = max([flowlines[f].drain for f in flowlines])
        last_comid = [flowlines[f].comid for f in flowlines 
                      if flowlines[f].drain == max_area][0]

        if last_comid not in outlets: outlets.append(last_comid)

        # check to see if there are two flowlines feeding the watershed outlet

        for k, v in flowlines.items():
            if (v.down == flowlines[hydroseqs[last_comid]].down and
                v.comid != last_comid):
                print('adding outlet for additional watershed outlet at', 
                      v.comid, '\n')
                outlets.append(v.comid)

        # trace the main channels from the inlet hydroseqs

        main = []
        for inlet in inlets:
            flowline = flowlines[hydroseqs[inlet]]
            if flowline not in main: main.append(flowline)
            while flowline.down in flowlines:
                flowline = flowlines[flowline.down]
                if flowline not in main: main.append(flowline)

        # make the main channel if there is no inlet

        if len(inlets) == 0:
            flowline = flowlines[hydroseqs[last_comid]]
            main.append(flowline)
            while flowline.up != 0:
                flowline = flowlines[flowline.up]
                main.append(flowline)

        # add outlets to connect outlets to the main channel as needed

        disconnected = []
        for outlet in outlets:

            # make sure the downstream reach is still in the watershed

            if flowlines[hydroseqs[outlet]].down in flowlines:

                # check if the downstream flowline is included

                if flowlines[flowlines[hydroseqs[outlet]].down] not in main: 
                    
                    disconnected.append(outlet)

        while len(disconnected) > 0:

            # go downstream and add outlets to connect

            for outlet in disconnected:

                flowline = flowlines[hydroseqs[outlet]]

                if verbose: print(flowline.comid, 'is not connected')

                # then need to add outlets to connect to the main line

                while flowlines[flowline.down] not in main:
                    main.append(flowline)
                    flowline = flowlines[flowline.down]
                    if flowline.down not in flowlines: 
                        if verbose: print('reached the watershed outlet')
                        break

                if flowline.comid not in outlets: 
                    outlets.append(flowline.comid)
                    main.append(flowline)

                    if verbose: 

                        print('adding outlet ' +
                              '{} for connectivity'.format(flowline.comid))

                # add outlets for any others streams at the new junction

                others = [v for k, v in flowlines.items() 
                          if (v.down == flowline.down and v != flowline)]

                for other in others:

                    if other.comid not in outlets:

                        outlets.append(other.comid)
                        main.append(other)

                        if verbose: 

                            print('adding another outlet ' +
                                  '{} for connectivity'.format(other.comid))

            # check to see if any of the outlets are still disconnected

            disconnected = []
            for outlet in outlets:

                # make sure the downstream reach is still in the watershed

                if flowlines[hydroseqs[outlet]].down in flowlines:

                    # check if the downstream flowline is included

                    down = flowlines[flowlines[hydroseqs[outlet]].down]
                    if down not in main and down.comid not in outlets:
                    
                        disconnected.append(outlet)

                        if verbose:

                            print(outlet, 'is still disconnected')
                            print('')

        # remove any outlets that are inlets (since this makes no sense)

        outlets = [outlet for outlet in outlets
                   if outlet not in inlets]
    
        # check the drainage areas to make sure subbasins are not too large
        # start at the main outlet and move upstream adding outlets as needed

        if drainmax is None: drainmax = max_area

        n = -1
        while len(outlets) != n:

            # move upstream and look at the changes in drainage area for 
            # each outlet

            n = len(outlets)

            for outlet in outlets:
                flowline = flowlines[hydroseqs[outlet]] # current flowline
                boundaries     = [0] + [hydroseqs[b] for b in inlets + outlets]
                drain_area     = flowline.divarea

                # check until reaching another outlet or the top of the 
                # watershed; additional checks for max basin drainage area 
                # and major tributaries

                while flowline.up not in boundaries:

                    # find all the tributaries

                    tributaries = [f for f in flowlines 
                                   if flowlines[f].down == flowline.hydroseq]

                    # find the major tributary

                    major = flowlines[flowline.up]

                    # if any tributary is an outlet or if the minor tributaries
                    # exceeds drainage max make them all outlets

                    if (any([flowlines[f].comid in outlets 
                             for f in tributaries]) or
                        flowline.divarea - major.divarea > drainmax):

                        for f in tributaries: 
                            if flowlines[f].comid not in outlets: 
                                outlets.append(flowlines[f].comid)

                                if verbose: 

                                    print('adding outlet %d for major tributary'
                                          % flowlines[f].comid)

                        break

                    elif drain_area - flowline.divarea > drainmax:

                        if flowlines[flowline.down].comid not in outlets: 
                            outlets.append(flowlines[flowline.down].comid)

                            if verbose: 
                                print('adding outlet %d for drainage area' % 
                                      flowlines[flowline.down].comid)

                        break

                    else: flowline = flowlines[flowline.up]

        # group flowlines into subbasins associated with each outlet

        subbasins = {outlet: [outlet] for outlet in outlets}

        # check to see if there are multiple outlets at the watershed outlet

        downhydroseq = flowlines[hydroseqs[last_comid]].down

        for comid in hydroseqs:
            if (flowlines[hydroseqs[comid]].down == downhydroseq and 
                comid not in outlets):
                subbasins[last_comid].append(comid)

        # go through each gage and make a list of all the comids in the subbasin

        for subbasin in subbasins:
            current = [hydroseqs[outlet] for outlet in subbasins[subbasin]]
            while len(current) > 0:
                last    = current[:]
                current = []

                for comid in hydroseqs:
                    if (flowlines[hydroseqs[comid]].down in last and 
                        comid not in outlets):

                        subbasins[subbasin].append(comid)
                        current.append(hydroseqs[comid])

        # keep track of the subbasin dictionary

        self.subbasins = subbasins

        # make a shapefile containing the outlet points

        if verbose: print('copying the projections\n')

        # read the flowline and gage files

        flowreader  = Reader(self.flowlines, shapeType = 3)
        flowrecords = flowreader.records()

        # read the gage file

        gagereader  = Reader(self.gagefile, shapeType = 1)
        gagepoints  = [s.points[0] for s in gagereader.shapes()]

        # find the Reach code and comid fields in the flow file

        comid_index = flowreader.fields.index(['COMID',     'N',  9, 0]) - 1
        reach_index = flowreader.fields.index(['REACHCODE', 'C', 14, 0]) - 1
        gnis_index  = flowreader.fields.index(['GNIS_NAME', 'C', 65, 0]) - 1

        # make a list of the comids

        comids = [record[comid_index] for record in flowrecords]

        # make the inlet file

        if len(inlets) > 0:

            w = Writer(shapeType = 1)

            w.field(*['COMID',      'N',  9, 0])
            w.field(*['REACHCODE',  'C', 14, 0])
            w.field(*['SITE_NO',    'C', 15, 0])
            w.field(*['DRAIN_SQKM', 'N', 15, 3])
            w.field(*['AVG_FLOW',   'N', 15, 3])
            w.field(*['GNIS_NAME',  'C', 65, 0])
            w.field(*['NWISWEB',    'C', 75, 0])

            for inlet in inlets:
                index = comids.index(inlet)
                shape = flowreader.shape(index)
                point = shape.points[0]

                # get the parameters from the flow file

                reachcode = flowrecords[index][reach_index]
                comid     = flowrecords[index][comid_index]
                gnis      = flowrecords[index][gnis_index]

                # work around for blank records

                if isinstance(gnis, bytes): gnis = gnis.decode().strip()

                # get the area from the flowline database

                area = flowlines[hydroseqs[inlet]].drain

                if inlet in gage_outlets:

                    distances = [self.get_distance(point, p) 
                                 for p in gagepoints]
                    closest   = distances.index(min(distances))

                    site_no = gagerecords[closest][site_index]
                    nwis    = gagerecords[closest][nwis_index]
                    flow    = round(gagerecords[closest][ave_index], 3)

                else:

                    site_no = ''
                    nwis    = ''

                    # estimate the flow from the nearest gage, start by going 
                    # upstream until reaching a gage comid

                    next_gage = flowlines[hydroseqs[comid]]
                    current_area = next_gage.drain

                    while (next_gage.comid not in gage_comids and 
                           next_gage.down in flowlines):
                        next_gage = flowlines[next_gage.down]
                    if next_gage.comid == last_comid:
                        flow = round(max([record[ave_index] 
                                          for record in gagerecords]), 3)
                    else:
                        # get the flow from the gage file (note units)

                        distances = [self.get_distance(point, p) 
                                     for p in gagepoints]
                        closest   = distances.index(min(distances))
                        next_flow = gagerecords[closest][ave_index]
                        next_area = gagerecords[closest][drain_index] * 2.59

                        flow = round(next_flow * current_area / next_area, 3)

                w.point(point[0], point[1])
                w.record(comid, reachcode, site_no, area, flow, gnis, nwis)
    
            w.save(self.inletfile)

            # copy the projection files

            shutil.copy(self.flowlines + '.prj', self.inletfile  + '.prj')

        # create the outlet point file that will store the comid and reachcode

        w = Writer(shapeType = 1)

        w.field(*['COMID',      'N',  9, 0])
        w.field(*['REACHCODE',  'C', 14, 0])
        w.field(*['NIDID',      'C',  7, 0])
        w.field(*['SITE_NO',    'C', 15, 0])
        w.field(*['DRAIN_SQKM', 'N', 15, 3])
        w.field(*['AVG_FLOW',   'N', 15, 3])
        w.field(*['GNIS_NAME',  'C', 65, 0])
        w.field(*['NWISWEB',    'C', 75, 0])

        for outlet in outlets:

            # find the flowline and use the last point as the outlet

            index = comids.index(outlet)
            shape = flowreader.shape(index)
            point = shape.points[-1]

            # get the parameters from the flow file

            reachcode = flowrecords[index][reach_index]
            comid     = flowrecords[index][comid_index]
            gnis      = flowrecords[index][gnis_index]

            if isinstance(gnis, bytes): gnis = gnis.decode().strip()

            # get the area from the flowline database

            area = flowlines[hydroseqs[outlet]].divarea

            # find the nearest dam if the outlet is co-located with a dam

            if outlet in dam_comids:

                distances = [self.get_distance(point, p) for p in dampoints]
                closest   = distances.index(min(distances))

                dam_no = damrecords[closest][nid_index]

            else:

                dam_no = ''

            # find the gage station if the outlet is co-located with a gage

            if outlet in gage_outlets:

                distances = [self.get_distance(point, p) for p in gagepoints]
                closest   = distances.index(min(distances))

                site_no = gagerecords[closest][site_index]
                nwis    = gagerecords[closest][nwis_index]
                flow    = round(gagerecords[closest][ave_index], 3)

            else:

                site_no = ''
                nwis    = ''

                # estimate the flow by interpolating from the nearest gage,
                # start by going downstream until reaching a gage comid and get
                # the drainage area and then repeat going upstream

                next_gage = flowlines[hydroseqs[comid]]
                while (next_gage.comid not in gage_comids and 
                       next_gage.down in flowlines):
                    next_gage = flowlines[next_gage.down]

                # find the area for interpolating flows

                i = comids.index(next_gage.comid)
                next_point = flowreader.shape(i).points[-1]
                distances  = [self.get_distance(next_point, p) 
                              for p in gagepoints]
                closest    = distances.index(min(distances))
                next_flow  = gagerecords[closest][ave_index]
                next_area  = gagerecords[closest][drain_index] * 2.59

                last_gage = flowlines[hydroseqs[comid]]
                while (last_gage.comid not in gage_comids and
                       last_gage.up in flowlines):
                    last_gage = flowlines[last_gage.up]

                # see whether it's at the top of the watershed or an inlet
                # otherwise get the flows from the gage file (note units)

                if last_gage.up == 0 or last_gage.up not in flowlines:
                    last_flow = 0
                    last_area = 0
                else: 
                    i = comids.index(last_gage.comid)
                    last_point = flowreader.shape(i).points[-1]
                    distances  = [self.get_distance(last_point, p) 
                                  for p in gagepoints]
                    closest    = distances.index(min(distances))
                    last_flow  = gagerecords[closest][ave_index]
                    last_area  = gagerecords[closest][drain_index] * 2.59

                if last_flow == next_flow: flow = last_flow
                else:
                    flow = round(last_flow + (next_flow - last_flow) * 
                                 (area - last_area) / (next_area - last_area),3)

            w.point(point[0], point[1])
            w.record(comid, reachcode, dam_no, site_no, area, flow, gnis, nwis)
    
        w.save(self.outletfile)

        # copy the projection files

        shutil.copy(self.flowlines + '.prj', self.outletfile + '.prj')

        if verbose: print('successfully subdivided watershed\n')

    def extract_flowlines(self, 
                          comids, 
                          output, 
                          verbose = True,
                          ):
        """
        Makes a shapefile containing the major flowlines above a USGS gage
        within a HUC8.
        """

        if output is None: output = '{}/subbasin_flowlines'.format(os.getcwd())

        # start by copying the projection files

        shutil.copy(self.flowlines + '.prj', output + '.prj')

        # open the flowline shapefile
  
        shapefile = Reader(self.flowlines, shapeType = 3)
        records   = shapefile.records()

        # figure out which field code is the comid

        comid_index = shapefile.fields.index(['COMID', 'N', 9,  0]) - 1

        # go through the flowline comids and find the ones in the subbasin

        if verbose: print('extracting subbasin flowlines')

        indices = []
   
        i = 0
        for record in records:
            if record[comid_index] in comids: indices.append(i)
            i+=1

        # write the data to a new shapefile

        w = Writer(shapeType = 3)

        for field in shapefile.fields:  w.field(*field)

        for i in indices:
            shape  = shapefile.shape(i)

            w.poly(shapeType = 3, parts = [shape.points])

            record = records[i]

            # little work around for blank GNIS_ID and GNIS_NAME values

            if isinstance(record[3], bytes):
                record[3] = record[3].decode('utf-8')
            if isinstance(record[4], bytes):
                record[4] = record[4].decode('utf-8')

            w.record(*record)

        w.save(output)

        if verbose: print('successfully extracted flowlines')

    def extract_catchments(self, 
                           comids, 
                           output, 
                           verbose = True,
                           ):
        """
        Iterates through a catchment shapefile for a basin and makes a new
        shapefile containing only catchments with comids from the "comids" list.
        """

        # start by copying the projection files

        shutil.copy(self.catchments + '.prj', output + '.prj')
  
        shapefile = Reader(self.catchments, shapeType = 5)
        records   = shapefile.records()

        # figure out which field code is the comid

        feature_index = shapefile.fields.index(['FEATUREID', 'N', 9,  0]) - 1

        # go through the reach indices, add add them to the list of flowlines if
        # they are in the watershed, and make a list of the corresponding comids

        if verbose: print('searching for catchments\n')

        indices = []
   
        i = 0
        for record in records:
            if record[feature_index] in comids:
                indices.append(i)
            i+=1

        # write the data from the HUC8 to a new shapefile

        w = Writer(shapeType = 5)

        for field in shapefile.fields:  w.field(*field)

        for i in indices:
            shape  = shapefile.shape(i)
            w.poly(shapeType = 5, parts = [shape.points])
            w.record(*records[i])

        w.save(output)

        if verbose: print('successfully extracted catchments\n')

    def combine_subbasin_flowlines(self,
                                   output,
                                   overwrite = False, 
                                   verbose = True,
                                   ):
        """
        Combines outlet subbasin flowlines for an 8-digit hydrologic unit 
        into a single shapefile.  Assumes directory structure of:

        path_to_HUC8\comids\combined_flowline.shp 
    
        where comids are all the elements in a list of the subbasin outlets 
        from  the NHDPlus dataset.
        """

        l = Writer(shapeType = 3)

        # copy the fields

        comid = [s for s in self.subbasins.keys()][0]
        filename = '{}/{}/combined_flowline'.format(output, comid)
        for field in Reader(filename).fields: l.field(*field)

        # iterate to get the vector data from each combined flowline file

        for comid in self.subbasins:

            filename = '{}/{}/combined_flowline'.format(output, comid)

            if os.path.isfile(filename + '.shp'):

                if verbose: print('found combined file {}\n'.format(filename))

                # read the new file
  
                r = Reader(filename, shapeType = 3)

                # get the shape (only 1)

                shape = r.shape(0)

                # write the shape and record to the new file

                l.poly(shapeType = 3, parts = [shape.points])
                record = r.record(0)
                if isinstance(record[1], bytes): record[1] = ''
                if record[1] == 65 * ' ': record[1] = ''
                l.record(*record)

            elif verbose: print('unable to locate {}\n'.format(filename))

        # save the merged file

        l.save(self.subbasinflowlines)

        # copy the projection files

        shutil.copy(filename + '.prj', self.subbasinflowlines + '.prj')

        if verbose: print('successfully combined flowline shapefiles')

    def make_subbasins(self, 
                       output,
                       parallel = False, 
                       verbose  = True, 
                       vverbose = False, 
                       form     = 'png',
                       ):
        """
        Extracts catchments from the watershed catchment file for each 
        subbasin and then combines the catchments together and calculates 
        slope parameters.
        """

        if verbose: print('aggregating catchments into subbasins\n')

        start = time.time()

        # divide the catchment shapefile into subbasin catchment shapefiles 

        if parallel:

            processes = []
            for subbasin, names in self.subbasins.items():
                its = output, subbasin
                catchments = '{}/{}/catchments'.format(*its)

                if not os.path.isfile(catchments + '.shp'):

                    processes.append(Process(target = self.extract_catchments,
                                             args = (names, catchments),
                                             kwargs = {'verbose': vverbose}
                                             )
                                     )

            for p in processes: p.start()
            for p in processes: p.join()
            processes = None

        else:

            for subbasin, names in self.subbasins.items():

                its = output, subbasin
                catchments = '{}/{}/catchments'.format(*its)

                if not os.path.isfile(catchments + '.shp'):

                    self.extract_catchments(names,
                                            catchments, 
                                            verbose = vverbose,
                                            )

        # combine the catchments in each subbasin into a single shapefile

        if parallel and not os.path.isfile(self.subbasincatchments + '.shp'):

            if verbose: print('attempting to combine subbasin catchments ' +
                              'in parallel, this may take a while...\n')

            processes = []
            for subbasin in self.subbasins:

                its = output, subbasin
                catchments = '{}/{}/catchments'.format(*its)
                flowlines  = '{}/{}/flowlines'.format(*its)
                combined   = '{}/{}/combined'.format(*its)

                if not os.path.isfile(combined + '.shp'):

                    processes.append(Process(target = self.combine_catchments, 
                                             args   = (catchments, 
                                                       flowlines,
                                                       subbasin,
                                                       combined,
                                                       ),
                                             kwargs = {'verbose':  verbose,
                                                       'vverbose': vverbose}
                                             )
                                     )

            for p in processes: p.start()
            for p in processes: p.join()
            processes = None

            if verbose: print('successfully combined catchments in parallel ' +
                              'in {:.1f} seconds \n'.format(time.time() -start))

        elif not os.path.isfile(self.subbasincatchments + '.shp'):

            if verbose: print('attempting to combine subbasin catchments' +
                              ', this may take a while...\n')

            for subbasin in self.subbasins:

                its = output, subbasin
                catchments = '{}/{}/catchments'.format(*its)
                flowlines  = '{}/{}/flowlines'.format(*its)
                combined   = '{}/{}/combined'.format(*its)

                if not os.path.isfile(combined + '.shp'):

                    try:

                        self.combine_catchments(catchments, 
                                                flowlines, 
                                                subbasin, 
                                                combined,
                                                verbose  = verbose,
                                                vverbose = vverbose,
                                                )

                        if verbose: 

                            print('successfully combined catchments in ' +
                                  'subbasin {}\n'.format(subbasin))

                    except:

                        if verbose: 

                            print('warning: unable to combine catchments ' + 
                                  'in {}\n'.format(subbasin))

            if verbose: print('successfully combined catchments in parallel ' +
                              'in {:.1f} seconds \n'.format(time.time() -start))

        # put together the combined subbasins into a single file

        if not os.path.isfile(self.subbasincatchments + '.shp'):

            self.combine_subbasins(output, verbose = verbose)

        if not os.path.isfile(self.boundary + '.shp'):

            if verbose: 

                print('merging catchments into the watershed boundary\n')

            merge_shapes(self.subbasincatchments, 
                         outputfile = boundaries, 
                         verbose = verbose,
                         vverbose = vverbose
                         )

        if verbose: 
            print('completed subbasin delineation in ' +
                  '{:.1f} seconds\n'.format(time.time() - start))

    def combine_subbasins(self, 
                          output,
                          verbose = True,
                          ):
        """
        Combines outlet subbasins for an 8-digit hydrologic unit into a 
        single shapefile.  Assumes directory structure of:

        directory\HUC8\comids\combined.shp 
    
        where comids are all the elements in a list of the subbasin outlets 
        from the NHDPlus dataset.
        """

        if verbose: 
            print('trying to combine subbasin shapefiles into a single file\n')

        w = Writer(shapeType = 5)

        projection = None
        fields     = None

        for comid in self.subbasins:

            its = output, comid
            filename = '{}/{}/combined'.format(*its)
            if os.path.isfile(filename + '.shp'):

                # start by copying the projection files

                if projection is None:
                    projection = self.subbasincatchments + '.prj'
                    shutil.copy(filename + '.prj', projection)

                # read the new file
  
                r = Reader(filename, shapeType=5)

                if fields is None:
                    fields = r.fields
                    for field in fields: w.field(*field)

                shape = r.shape(0)

                # write the shape and record to the new file

                w.poly(shapeType = 5, parts = [shape.points])
                record = r.record(0)
                w.record(*record)

            elif verbose: print('unable to locate {}'.format(filename))

        if fields is not None: 
            w.save(self.subbasincatchments)
            if verbose: print('successfully combined subbasin shapefiles\n')
        elif verbose: print('unable to combine subbasins\n')

    def delineate(self, 
                  output,
                  extra_outlets  = None,
                  drainmax       = None,
                  years          = None,
                  parallel       = True,
                  watershedplots = True,
                  form           = 'png',
                  verbose        = True, 
                  vverbose       = False,
                  ):
        """
        Analyzes the GIS data, subdivides the watershed into subbasins 
        that are co-located with the gages and have drainage areas no larger 
        than the max specified.

        extra_outlets -- list of longitudes and latitudes for additional outlets
        outputpath    -- path to write output
        """

        start = time.time()

        # subdivide the watershed using the USGS NWIS stations and any 
        # additional subbasins

        self.outletfile         = '{}/subbasin_outlets'.format(output)
        self.inletfile          = '{}/subbasin_inlets'.format(output)
        self.subbasinflowlines  = '{}/subbasin_flowlines'.format(output)
        self.subbasincatchments = '{}/subbasin_catchments'.format(output)
        self.elevations         = '{}/elevations.tif'.format(output)
        self.boundary           = '{}/boundary'.format(output)

        # images

        its = output, form

        self.preliminary = '{}/preliminary.{}'.format(*its)
        self.delineated  = '{}/delineated.{}'.format(*its)

        if (not os.path.isfile(self.outletfile         + '.shp') or
            not os.path.isfile(self.subbasinflowlines  + '.shp') or
            not os.path.isfile(self.subbasincatchments + '.shp')
            ):

            if verbose: 
                print('delineating HSPF watershed for HUC ' +
                      '{}\n'.format(self.HUC8))

            # add any additional outlets as a list of points (or None) and 
            # divide the flowfiles into subbasins above each of the subbasin 
            # outlets subbasins is a dictionary of linking the outlet flowline 
            # comid to the comids of all the tributaries up to the 
            # previous outlet

            self.make_subbasin_outlets(drainmax = drainmax, 
                                       extras   = extra_outlets,
                                       years    = years,
                                       verbose  = vverbose,
                                       )

            # divide the flowline shapefile into subbasin flowline shapefiles 

            for subbasin, comids in self.subbasins.items():

                p        = '{}/{}'.format(output, subbasin)
                flow     = p + '/flowlines'
                combined = p + '/combined_flowline'
        
                # make a directory for the output if needed

                if not os.path.isdir(p): os.mkdir(p)

                # extract the flowlines

                if not os.path.isfile(flow + '.shp'):

                    self.extract_flowlines(comids,
                                           flow, 
                                           verbose = vverbose,
                                           )

                # combine the flowlines into a single shapefile

                if not os.path.isfile(combined + '.shp'):

                    self.combine_flowlines(flow, 
                                           combined, 
                                           verbose = vverbose
                                           )

            # merge the flowlines into a single file

            if not os.path.isfile(self.subbasinflowlines + '.shp'):

                combined = '{}'.format(output)
                self.combine_subbasin_flowlines(combined,
                                                overwrite = True, 
                                                verbose = vverbose
                                                )

            if verbose: 

                print('successfully divided watershed in ' +
                      '{:.1f} seconds\n'.format((time.time() - start)))

            if not os.path.isfile(self.subbasincatchments + '.shp'):

                self.make_subbasins(output,
                                    parallel = parallel, 
                                    verbose  = verbose, 
                                    vverbose = vverbose,
                                    )

        elif verbose: 

            print('hydrography data for {} exist\n'.format(self.HUC8))

        if not os.path.isfile(self.preliminary) and watershedplots:

            description = 'Catchments, Flowlines, Dams, and Gages'
            title = ('Cataloging Unit {}\n{}'.format(self.HUC8, description))
            self.plot_watershed(self.catchments,
                                title = title,
                                dams = True,
                                width = 0.06,
                                output = self.preliminary,
                                verbose = verbose,
                                )

        if not os.path.exists(self.delineated) and watershedplots:

            description = 'Subbasins, Major Flowlines, and Calibration Gages'
            title = ('Cataloging Unit {}\n{}'.format(self.HUC8, description))
            self.plot_watershed(self.subbasincatchments, 
                                title = title,
                                gages = 'calibration',
                                width = 0.2,
                                dams = True, 
                                output = self.delineated, 
                                verbose = verbose,
                                )

    def plot_watershed(self,
                       cfile,
                       outlets = False, 
                       gages = 'all',
                       dams = False, 
                       title = '',
                       raster = True,
                       legend = True, 
                       grid = False, 
                       patchcolor = None,
                       width = 0.2,
                       resolution = 400,
                       vmin       = 0.1,
                       colormap = 'gist_earth',
                       output = None, 
                       show = False, 
                       verbose = True,
                       ):
        """
        Makes a plot of all the flowlines and catchments of a basin on top 
        of a raster image file.
        """

        if verbose: print('generating plot of watershed {}\n'.format(self.HUC8))

        # paths to the files used herein

        flowlines  = self.flowlines
        combined   = self.subbasinflowlines

        # make a figure to work on

        fig = pyplot.figure()
        subplot = fig.add_subplot(111, aspect = 'equal')
        subplot.tick_params(axis = 'both', which = 'major', labelsize = 10)

        # add the title

        subplot.set_title(title, fontsize = 14)

        # open up and show the catchments

        if patchcolor is None: facecolor = (1,0,0,0.)
        else:                  facecolor = patchcolor

        b = Reader(self.boundary, shapeType = 5)

        points = numpy.array(b.shape(0).points)
        subplot.add_patch(self.make_patch(points, facecolor = facecolor, 
                                          width = 1.))

        extent = self.get_boundaries(b, space = 0.02)

        xmin, ymin, xmax, ymax = extent

        # figure out how far one foot is on the map

        points_per_width = 72 * 8
        ft_per_km = 3280.84
        scale_factor = (points_per_width / 
                        self.get_distance([xmin, ymin], [xmax, ymin]) / 
                        ft_per_km)

        c = Reader(cfile, shapeType = 5)

        # make patches of the catchment areas

        for i in range(len(c.records())):
            catchment = c.shape(i)
            points = numpy.array(catchment.points)
            subplot.add_patch(self.make_patch(points, facecolor, width = width))

        if legend:

            subplot.plot([-200, -199], [-200, -199], 'black', lw = 0.2,
                         label = 'subbasins')

        # get the flowline attributes, make an "updown" dictionary to follow 
        # flow, and change the keys to comids

        with open(self.attributes, 'rb') as f: flowlineVAAs = pickle.load(f)

        updown = {}
        for f in flowlineVAAs:
            if flowlineVAAs[f].down in flowlineVAAs:
                updown[flowlineVAAs[f].comid] = \
                    flowlineVAAs[flowlineVAAs[f].down].comid

        flowlineVAAs = {flowlineVAAs[f].comid:flowlineVAAs[f] 
                        for f in flowlineVAAs}

        # open up and show the flowfiles

        f = Reader(flowlines, shapeType = 3)
        comid_index = f.fields.index(['COMID', 'N',  9, 0]) - 1

        all_comids = [r[comid_index] for r in f.records()]

        # show all the flowlines

        if flowlines:     
        
            # get the flows and velocities from the dictionary
        
            widths = []
            comids = []
            for comid in all_comids:
                if comid in flowlineVAAs:
                    flow = flowlineVAAs[comid].flow
                    velocity = flowlineVAAs[comid].velocity

                    # estimate width (ft) assuming triangular 90 deg channel 

                    comids.append(comid)
                    if velocity > 0:
                        widths.append(numpy.sqrt(4 * flow / velocity))
                    else:
                        widths.append(numpy.sqrt(4 * flow / vmin))
        
            # convert widths in feet to points on the figure; exaggerated by 10

            widths = [w * scale_factor * 10 for w in widths]

            # get the flowline and the corresponding catchment

            for comid, w in zip(comids, widths):

                i = all_comids.index(comid)
                flowline = numpy.array(f.shape(i).points)

                # plot it

                subplot.plot(flowline[:, 0], flowline[:, 1], 'b', lw = w)

            if legend:

                avg_width = sum(widths) / len(widths)
                subplot.plot([-200, -199], [-200, -199], 'b', 
                             lw = 5 * avg_width,
                             label = 'flowlines')

        # otherwise use the subbasin combined flowlines

        else: 

            c = Reader(combined, shapeType = 3)

            comids = []
            for r in c.records():

                inlet_comid  = r[c.fields.index(['InletComID', 'N', 9, 0])  - 1]
                outlet_comid = r[c.fields.index(['OutComID', 'N', 9, 0]) - 1]

                # get the primary flowline from the hydroseqs

                comids.append(inlet_comid)

                if inlet_comid in updown:
                    while updown[comids[-1]] in updown:
                        comids.append(updown[comids[-1]])
                    if outlet_comid not in comids: comids.append(outlet_comid)

            # get the flows and velocities from the dictionary

            widths = []
            for comid in comids:
                flow     = flowlineVAAs[comid].flow
                velocity = flowlineVAAs[comid].velocity

                # estimate the flow width assuming triangular 90 deg channel

                if velocity > 0:
                    widths.append(numpy.sqrt(4 * flow / velocity))
                else:
                    widths.append(0)
                    
            # convert widths in feet to points on the figure; exaggerated by 10

            widths = [w * scale_factor * 10 for w in widths]

            # get the flowline and the corresponding catchment

            for comid, w in zip(comids, widths):

                i = all_comids.index(comid)
                flowline = numpy.array(f.shape(i).points)

                # plot it

                subplot.plot(flowline[:, 0], flowline[:, 1], 'b', lw = w)

            if legend:

                avg_width = sum(widths) / len(widths)
                subplot.plot([-200, -199], [-200, -199], 'b', 
                             lw = 3 * avg_width,
                             label = 'flowlines')

        if outlets:

            f = Reader(self.outletfile, shapeType = 1)

            outlet_shapes = f.shapes()
            outlet_records = f.records()
            flow_index = f.fields.index(['AVG_FLOW', 'N', 15, 3]) - 1
            flows = [r[flow_index] for r in outlet_records]
            outlet_points = [o.points[0] for o in outlet_shapes]
            x1, y1 = zip(*outlet_points)
            subplot.scatter(x1, y1, marker = 'o', c = 'r', s = 30, 
                            label = 'outlets')

            if os.path.isfile(self.inletfile + '.shp'): 
                f = Reader(self.inletfile, shapeType = 1)
                inlet_shapes = f.shapes()
                inlet_points = [s.points[0] for s in inlet_shapes]
                inlet_flows = [r[flow_index] for r in f.records()]
                x2, y2 = zip(*inlet_points)
                subplot.scatter(x2, y2, marker = 'o', c = 'b', s = 30)

        if gages == 'all':
        
            f = Reader(self.gagefile, shapeType = 1)

            gage_shapes = f.shapes()
            gage_points = [g.points[0] for g in gage_shapes]

            x1, y1 = zip(*gage_points)
            subplot.scatter(x1, y1, marker = 'o', c = 'r', s = 30, 
                            label = 'gauges')

        # show only gages used for calibration

        elif gages == 'calibration': 

            f1 = Reader(self.outletfile, shapeType = 1)

            outlet_records = f1.records()

            site_index = f1.fields.index(['SITE_NO', 'C', 15, 0]) - 1
            flow_index = f1.fields.index(['AVG_FLOW', 'N', 15, 3]) - 1

            flows = [r[flow_index] for r in outlet_records]
            sites = [r[site_index] for r in outlet_records]

            f2 = Reader(self.gagefile, shapeType = 1)

            gage_shapes  = f2.shapes()
            gage_records = f2.records()

            site_index = f2.fields.index(['SITE_NO', 'C', 15, 0]) - 1

            gage_points = []
            for shape, record in zip(gage_shapes, gage_records):

                if record[site_index] in sites:

                    gage_points.append(shape.points[0])

            x1, y1 = zip(*gage_points)
            subplot.scatter(x1, y1, marker = 'o', c = 'r', s = 30, 
                            label = 'gauges')

        # show dams

        if dams:

            f = Reader(self.outletfile, shapeType = 1)

            outlet_records = f.records()

            dam_index = f.fields.index(['NIDID', 'C', 7, 0]) - 1

            nidids = [r[dam_index] for r in outlet_records]

            dam_points = []
            for nid, s in zip(nidids, f.shapes()):
                if isinstance(nid, bytes): nid = nid.decode('utf-8')
                nid = nid.strip()

                if len(nid) > 0:
                
                    dam_points.append(s.points[0])

            x1, y1 = zip(*dam_points)
            subplot.scatter(x1, y1, marker = 's', c = 'y', s = 30, 
                            label = 'dams')

        subplot.set_xlabel('Longitude, Decimal Degrees', size = 13)
        subplot.set_ylabel('Latitude, Decimal Degrees',  size = 13)

        # add the elevation raster (could be adapted for something else)

        if raster:

            im = self.add_raster(subplot, self.elevations, resolution, extent, 
                                 colormap, scale = 100) 

            divider = make_axes_locatable(subplot)
            cax = divider.append_axes('right', size = 0.16, pad = 0.16)
            colorbar = fig.colorbar(im, cax = cax, orientation = 'vertical')
            colorbar.set_label('Elevation, m', size = 12)
            cbax = pyplot.axes(colorbar.ax)
            yaxis = cbax.get_yaxis()
            ticks = yaxis.get_majorticklabels()
            for t in ticks: t.set_fontsize(10)

        else:

            pyplot.xlim([xmin, xmax])
            pyplot.ylim([ymin, ymax])

        if grid:

            subplot.xaxis.set_minor_locator(MultipleLocator(0.1))
            subplot.yaxis.set_minor_locator(MultipleLocator(0.1))

            subplot.xaxis.grid(True, 'minor', linestyle = '-', linewidth = 0.5)
            subplot.yaxis.grid(True, 'minor', linestyle = '-', linewidth = 0.5)

        if legend: 
            leg = subplot.legend(loc = 'upper right')
            leg.get_frame().set_alpha(0.)
            legtext = leg.get_texts()
            pyplot.setp(legtext, fontsize = 10)

        # show it

        pyplot.tight_layout()

        if output is not None:  pyplot.savefig(output)

        if show: pyplot.show()

        pyplot.clf()
        pyplot.close()
