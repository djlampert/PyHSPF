import gdal, osr, os, csv

from shapefile import Reader

from pyhspf.preprocessing.raster import get_raster, get_raster_table

def extract_cropland(NASS, shapefile, HUC8, year, output, verbose = True):
    """Extracts cropland data as a raster file from the National Agricultural
    Statistics Services into output using the bounding box from the shapefile.
    """

    if verbose: print('copying the cropland data from NASS')
    else:       gdal.PushErrorHandler('CPLQuietErrorHandler')

    # get the coordinates for the bounding box from the shapefile

    shapefile = Reader(shapefile, shapeType = 3)

    xmin, ymin, xmax, ymax = shapefile.bbox

    shapefile = None

    #adjust to make the map just larger than the extents

    xmin, xmax = xmin - 0.05 * (xmax - xmin), xmax + 0.05 * (xmax - xmin)
    ymin, ymax = ymin - 0.05 * (ymax - ymin), ymax + 0.05 * (ymax - ymin)

    # extract the values of the DEM raster and the origin from NASS

    values, corner = get_raster_table(NASS, [xmin, ymin, xmax, ymax],
                                      'uint8')

    # get the source, source reference, and the source band

    source      = gdal.Open(NASS)
    source_band = source.GetRasterBand(1)
    source_reference = osr.SpatialReference()
    source_reference.ImportFromWkt(source.GetProjectionRef())

    # set the transform to the new origin

    transform = source.GetGeoTransform()
    transform = (corner[0], transform[1], transform[2], corner[1],
                 transform[4], transform[1])

    # get a driver and set the projection and georeference

    driver = gdal.GetDriverByName('GTiff')

    destination = driver.Create(output, len(values[0]), len(values), 1, 
                                gdal.GDT_Byte)
    destination.SetGeoTransform(transform)

    destination_reference = osr.SpatialReference()
    destination_reference.SetUTM(source_reference.GetUTMZone())
    destination_reference.SetWellKnownGeogCS('NAD83')

    destination.SetProjection(destination_reference.ExportToWkt())

    # set the metadata and get the destination band

    destination.SetMetadata(source.GetMetadata())

    destination_band = destination.GetRasterBand(1)

    # copy the pertinent attributes to the band

    destination_band.WriteArray(values, 0, 0)
    destination_band.SetColorTable(source_band.GetColorTable().Clone())

    # transform the projection from WGS 1984 to NAD 1983

    gdal.ReprojectImage(source, destination, source_reference.ExportToWkt(), 
                        destination_reference.ExportToWkt())

    # close up the files

    source      = None
    destination = None

    if verbose: print('successfully extracted cropland data to new file\n')
