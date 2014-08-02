import gdal, os

from shapefile import Reader

from .raster import get_raster_table

def extract_elevations(NED, HUC8, flowlines, output, verbose = True):
    """Extracts elevation data as a raster file from the National Elevation
    Dataset located in the NHDPlus directory.
    """

    if verbose: print('copying the elevation data from NED\n')

    # get the coordinates for the bounding box from the shapefile

    shapefile = Reader(flowlines, shapeType = 3)

    xmin, ymin, xmax, ymax = shapefile.bbox
    shapefile = None

    #adjust to make the map just larger than the extents

    xmin = xmin - 0.05 * (xmax - xmin)
    ymin = ymin - 0.05 * (ymax - ymin)
    xmax = xmax + 0.05 * (xmax - xmin)
    ymax = ymax + 0.05 * (ymax - ymin)

    # extract the values of the DEM raster and the origin from the NED

    values, corner = get_raster_table(NED, [xmin, ymin, xmax, ymax], 
                                      dtype = 'uint16')
    
    # get the projection

    source     = gdal.Open(NED)
    projection = source.GetProjection()

    # set the transform to the new origin

    transform = source.GetGeoTransform()
    transform = (corner[0], transform[1], transform[2], corner[1],
                 transform[4], transform[1])

    # get a driver and make the new file

    driver = gdal.GetDriverByName('GTiff')
    dst_ds = driver.Create(output, len(values[0]), len(values), 1, 
                           gdal.GDT_UInt16)
    dst_ds.SetProjection(projection)
    dst_ds.SetGeoTransform(transform)
    
    dst_ds.GetRasterBand(1).WriteArray(values, 0, 0)

    # make sure to close the files

    src_ds = None
    dst_ds = None

    if verbose: print('successfully extracted elevation data to new file\n')
