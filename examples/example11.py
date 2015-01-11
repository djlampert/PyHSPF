# example11.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 01/10/2015
#
# shows how to download, extract, and work with data from the US National 
# Inventory of Dams (NID) using PyHSPF's built-in NIDExtractor class.

from pyhspf.preprocessing import NIDExtractor

# extract data for the Patuxent River, MD, HUC 02060006, using the boundary
# shapefile in the data directory

sfile = 'data/patuxent/boundary'

# output shapefile name

damfile = '02060006dams'

# path to place the NID shapefile

NID = 'NID'

# make an instance of the extractor to work with; the base US NID shapefile
# will be downloaded and extracted automatically if it doesn't exist

nidextractor = NIDExtractor(NID)

# extract the data to a new shapefile

nidextractor.extract_shapefile(sfile, damfile)

# the extractor can also extract data to a new shapefile using a bounding box 

bbox = -78, 38, -75, 40

# output file name

bboxfile = 'bbox'

nidextractor.extract_bbox(bbox, bboxfile)

# let's use pyshp to open up the patuxent shapefile and get some info about the
# dams that we downloaded

from shapefile import Reader

sf = Reader(damfile)

# these are the attributes of each dam stored in the NID

name_index  = sf.fields.index(['DAM_NAME',   'C', 65,   0]) - 1
nid_index   = sf.fields.index(['NIDID',      'C', 7,    0]) - 1
lon_index   = sf.fields.index(['LONGITUDE',  'N', 19,  11]) - 1
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

# iterate through the records and get whatever information is needed

for r in sf.records():

    name  = r[name_index]
    nidid = r[nid_index]
    lon   = r[lon_index]
    lat   = r[lat_index]
    pur   = r[purp_index]

    print('Dam name:       ', name)
    print('NID ID:         ', nidid)
    print('Longitude:      ', lon)
    print('Latitude:       ', lat)
    print('Primary Purpose:', pur)
    print('')
