import shutil, os, time, pickle

from multiprocessing import Process, Queue
from shapefile       import Reader, Writer

from pyhspf.preprocessing.dbf import read_dbf

class Flowline:
    """A class to store the attributes of a flowline from the NHDPlus database.
    """

    def __init__(self, comid, hydroseq, down, up, drain, area, div, reachcode):
        """Attributes from NHDPlus PlusFlowVAA."""

        self.comid    = comid
        self.hydroseq = hydroseq
        self.down     = down
        self.up       = up
        self.drain    = drain
        self.area     = area
        self.divarea  = div
        self.reach    = reachcode

    def add_slope(self, maxelev, minelev, length):      
        """Attributes from NHDPlus ElevSlope."""

        self.maxelev  = maxelev
        self.minelev  = minelev
        self.length   = length

    def add_flow(self, flow, velocity, gageid):
        """Attributes from EROM_MA0001."""

        self.flow       = flow
        self.velocity   = velocity
        self.gageid     = gageid

    def estimate_traveltime(self):
        """Estimates the travel time in hours for a flowline."""

        ft_per_km = 3280.84
        s_per_hr  = 3600

        self.traveltime = self.length * ft_per_km / self.velocity / s_per_hr

def extract_flowlines(source, destination, HUC8, verbose = True):
    """Extracts flowlines from the source datafile to the destination using
    the HUC8 for the query."""

    # open the flowline file
    
    if verbose: print('reading the flowline file\n')
    
    shapefile = Reader(source, shapeType = 3)
    records   = shapefile.records()
    
    # figure out which field codes are the Reach code and comid
    
    reach_index = shapefile.fields.index(['REACHCODE', 'C', 14, 0]) - 1
    comid_index = shapefile.fields.index(['COMID',     'N', 9,  0]) - 1
    
    # go through the reach indices, add add them to the list of flowlines
    # if in the watershed; also make a list of the corresponding comids
    
    if verbose: print('searching for flowlines in the watershed\n')
    
    indices = []
    comids  = []
       
    i = 0
    for record in records:
        if record[reach_index][:8] == HUC8:
            indices.append(i)
            comids.append(record[comid_index])
        i+=1

    if len(indices) == 0:
        if verbose: print('query returned no values, returning\n')
        return
    
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
        print('queried %d flowlines from original shapefile\n' % len(indices))

    return comids

def extract_catchments(source, destination, comids, verbose = True):
    """Extracts the catchments from the source data file to the destination
    using the list of comids for the query."""

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
        return

    # create the new shapefile
    
    if verbose: print('writing the new catchment shapefile\n')
    
    w = Writer()
    
    for field in shapefile.fields:  w.field(*field)
    
    for i in indices:
        shape = shapefile.shape(i)
        w.poly(shapeType = 5, parts = [shape.points])
        w.record(*records[i])
    
    w.save(destination)

def read_dbf_processes(queue, database, attributes, comids, verbose = True):
    """Dummy routine to point use multiprocessing with read_dbf."""

    queue.put(read_dbf(database, attributes = attributes, comids = comids,
                       verbose = verbose))

def extract_NHDPlus(NHDPlus, HUC8, flowfile, catchmentfile, VAAfile,
                    outputpath = None, parallel = False, verbose = True,
                    vverbose = False):
    """Creates shapefiles for the NHDPlus flowlines and catchments for a USGS
    8-digit hydrologic unit code from the NHDPlus source data.  Assumes the
    NHDPlus Version 2 Dataset for the 2-digit watershed has been installed with 
    the appropriate directory structure in path "NHDPlus" 
    (e.g., C:/NHDPlusMS/NHDPlus07).  Output shapefiles are written to the 
    optional "output" directory.
    """

    start = time.time()

    # data sources

    flowdatabase  = NHDPlus + '/NHDPlusAttributes/PlusFlowlineVAA.dbf'
    slopedatabase = NHDPlus + '/NHDPlusAttributes/elevslope.dbf'
    eromdatabase  = NHDPlus + '/EROMExtension/EROM_MA0001.DBF'
    flowsource    = NHDPlus + '/NHDSnapshot/Hydrography/NHDFlowline.shp'
    catchsource   = NHDPlus + '/NHDPlusCatchment/Catchment.shp'
    projection    = NHDPlus + '/NHDSnapshot/Hydrography/NHDFlowline.prj'

    # start by copying the projection files

    if vverbose: print('\ncopying the projections from NHDPlus\n')

    shutil.copy(projection, flowfile + '.prj')
    shutil.copy(projection, catchmentfile + '.prj')

    # extract the flowlines and get the NHDPlus comids

    comids = extract_flowlines(flowsource, flowfile, HUC8, verbose = vverbose)

    # remaining extraction can be done in parallel or serially

    if parallel:

        processes, queues = [], []

        # make a process to extract the different files from the sources

        processes.append(Process(target = extract_catchments,
                                 args   = (catchsource, catchmentfile, comids),
                                 kwargs = {'verbose': vverbose}))

        # read hydrologic sequence and drainage attributes from the database
    
        flowattributes = ['ComID', 'Hydroseq', 'DnHydroseq', 'UpHydroseq', 
                          'TotDASqKM', 'AreaSqKM', 'DivDASqKM','ReachCode']

        queues.append(Queue())
        processes.append(Process(target = read_dbf_processes, 
                                 args = (queues[0], flowdatabase, 
                                         flowattributes, comids),
                                 kwargs = {'verbose': vverbose}))

        # read the slope attributes from the database
    
        slopeattributes = ['COMID', 'MAXELEVSMO', 'MINELEVSMO', 'SLOPELENKM']

        queues.append(Queue())
        processes.append(Process(target = read_dbf_processes, 
                                 args = (queues[1], slopedatabase, 
                                         slopeattributes, comids),
                                 kwargs = {'verbose': vverbose}))

        # get the flow and velocity data from EROM
    
        eromattributes = ['Comid', 'Q0001E', 'V0001E', 'SMGageID'] 

        queues.append(Queue())
        processes.append(Process(target = read_dbf_processes, 
                                 args = (queues[2], eromdatabase, 
                                         eromattributes, comids),
                                 kwargs = {'verbose': vverbose}))

        for p in processes: p.start()
        for p in processes: p.join(20)

        flowvalues  = queues[0].get()
        slopevalues = queues[1].get()
        eromvalues  = queues[2].get()

    else:

        # extract the different files from the sources sequentially

        extract_catchments(catchsource, catchmentfile, comids, 
                           verbose = vverbose)

        # read hydrologic sequence and drainage attributes from the database
    
        flowattributes = ['ComID', 'Hydroseq', 'DnHydroseq', 'UpHydroseq', 
                          'TotDASqKM', 'AreaSqKM', 'DivDASqKM','ReachCode']
    
        flowvalues = read_dbf(flowdatabase, attributes = flowattributes, 
                              comids = comids, verbose = vverbose)
    
        # read the slope data from the database
    
        slopeattributes = ['COMID', 'MAXELEVSMO', 'MINELEVSMO', 'SLOPELENKM']
    
        slopevalues = read_dbf(slopedatabase, attributes = slopeattributes, 
                               comids = comids, verbose = vverbose)
    
        # get the flow and velocity data
    
        eromattributes = ['Comid', 'Q0001E', 'V0001E', 'SMGageID'] 
        eromvalues = read_dbf(eromdatabase, attributes = eromattributes, 
                              comids = comids, verbose = vverbose)
    

    # store the flowline data in a dictionary using hydroseqs as keys and 
    # make a dictionary linking the comids to hydroseqs
    
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
    
    f = open(VAAfile, 'wb')
    pickle.dump(flowlines, f)
    f.close()

    end = time.time()
    
    if verbose: 
        print('successfully queried data in %.2f seconds\n' % (end - start))
