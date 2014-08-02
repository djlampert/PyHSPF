import os, shutil

from shapefile import Reader, Writer

from .gisplots import get_boundaries
from .raster   import inside_box

def extract_dams(directory, damfile, HUC8, output, verbose = True):
    """Extracts the NID dam locations for a watershed from the dam 
    shapefile and the 8-digit hydrologic unit code of interest. 
    """

    shapefile = Reader(directory + '/%s/%scatchments' % (HUC8, HUC8))

    xmin, ymin, xmax, ymax = get_boundaries(shapefile.shapes(), space = 0)

    # copy the projection files

    if verbose: print('copying the projections from the NID source\n')

    projection = damfile + '.prj'

    shutil.copy(projection, output + '.prj')

    # get the dams within the watershed

    if verbose: print('reading the dam file\n')

    sf         = Reader(damfile, shapeType = 1)

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

    # iterate through the field and determine which points are in the watershed

    if verbose: 
        print('extracting gage stations in the watershed into new file\n')

    dam_indices = []

    i = 0
    for record in damrecords:

        lat  = record[lat_index]
        long = record[long_index]

        if inside_box([xmin, ymin], [xmax, ymax], [long, lat], space = 0):
            dam_indices.append(i)
        i+=1

    # write the data from the HUC8 to a new shapefile

    w = Writer(shapeType = 1)

    for field in sf.fields:  w.field(*field)

    for i in dam_indices:
        point = sf.shape(i).points[0]
        w.point(*point)

        values = damrecords[i]

        rs = []

        for value in values:

            if isinstance(value, bytes):
                value = value.decode('utf-8')

            rs.append(value)

        w.record(*rs)

    w.save(output)

    if verbose: 
        print('successfully extracted NID dam locations for watershed\n')
