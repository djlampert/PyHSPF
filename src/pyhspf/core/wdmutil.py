# wdmutil.py
#
# David J. Lampert (djlampert@gmail.com)
#
# last updated: 07/13/2015
#
# The WDMUtil class can be used to ineract with WDM files using Python. 
# The class was inspired by the Python extensions "wdmtoolbox," although
# this version interacts with the hspf library compiled for Python rather than
# the hass_ent.dll library. WDM files are just lists of time series data
# identified by a unique number (the DSN) plus various "attributes" that 
# describe important information about the type of data (time step, type, etc.).

import os, datetime, pickle, hspf

class WDMUtil:
    """Class to open and read from WDM files."""

    def __init__(self, 
                 verbose = False, 
                 messagepath = None,
                 ):
        """
        Initialize WDM environment.
        """

        # path to hspfmsg.wdm

        directory = os.path.dirname(hspf.__file__)
        if messagepath is None:
            self.messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(directory)
        elif os.path.isfile(messagepath):
            self.messagepath = messagepath
        else:
            print('error: unable to open message file {}'.format(messagepath))
            raise

        self.verbose = verbose

        # start by making a dictionary of values of WDM attributes from the
        # adwdm message files source (this is done during compilation)

        with open('{}/pyhspf/core/attributes'.format(directory), 'rb') as f: 
            self.attributes = pickle.load(f)

        # FORTRAN subroutines from HSPF library
        #
        # timdifpy: Time difference
        # wdbopnpy: Open WDM file
        # wdbsacpy: Set string attribute
        # wdbsaipy: Set integer attribute
        # wdbsarpy: Set real attribute
        # wdbsgcpy: Get string attribute
        # wdbsgipy: Get integer attribute
        # wdbsgrpy: Get real attribute
        # wdckdtpy: Check if DSN exists
        # wdflclpy: Close WDM file
        # wdlbaxpy: Create label for new DSN
        # wdtgetpy: Get time-series data
        # wdtputpy: Write time-series data
        # wddsrnpy: Renumber a DSN
        # wddsdlpy: Delete a DSN
        # wddsnxpy: Finds next DSN

        if self.verbose: print('Initializing WDM environment...\n')

        self.openfiles = {} # dictionary mapping filenames and FORTRAN numbers
        self.dsns      = {} # dictionary mapping openfiles to DSN lists 
        
        # open the message file, which contains SEQ data of all the different
        # types of information that can be stored in a WDM file

        if self.verbose: print('opening the message file')
        if hspf.wdbopnpy(9, self.messagepath, 1) == 0:
            self.openfiles[self.messagepath] = 9
            if self.verbose: print('successfully opened message file')
        elif self.verbose: print('unable to open message file')
        self.message = 9

    def open(self, 
             wdmpath, 
             mode,
             ):
        """
        Opens a WDM file for read/write, read-only, or overwrite. Returns 
        the FORTRAN file number.
        """

        options = {'rw': 0, 'r': 1, 'w': 2}

        ronwfg  = options[mode]

        choice  = {0: 'for modification', 
                   1: 'as read-only', 
                   2: 'as a new file'}

        if os.path.exists(wdmpath) and mode == 'w':
            if self.verbose: 
                print('warning: file %s exists, overwriting\n' % wdmpath)
            os.remove(wdmpath)

        if wdmpath not in self.openfiles:
            if len(wdmpath) > 64:
                print('path {} has {} characters'.format(wdmpath, len(wdmpath)))
                print('error, the file path must be 64 characters or less\n')
                raise

            retcode = hspf.wdbopnpy(len(self.openfiles) + 11, wdmpath, ronwfg)

            # this is a hack fix i've run into with some simulations
            #
            #if retcode == -5004: 
            #
            #    if self.verbose: 
            #
            #        print('warning, file is already open')
            #        print('trying to close open files\n')
            #
            #    for i in range(100): hspf.seqclose(i)
            #
            #    retcode = hspf.wdbopnpy(len(self.openfiles) + 11, wdmpath, 
            #                            ronwfg)

            if retcode == 0 and self.verbose: 
                print('opened {} {}'.format(wdmpath, choice[ronwfg]))
            if retcode == 1 and self.verbose: 
                print('opened file {} but invalid filename'.format(wdmpath))
            if retcode  < 0:
                print('error: unable to open file {}\n'.format(wdmpath))
                raise
            self.openfiles[wdmpath] = len(self.openfiles) + 11

            # make a list of the dsns in file

            if ronwfg == 0 or ronwfg == 1: 
                n = hspf.wddsnxpy(self.openfiles[wdmpath], 1)
                dsns = []
                while n != -1:
                    dsns.append(n)
                    n = hspf.wddsnxpy(self.openfiles[wdmpath], n + 1)
            else: dsns = []
            self.dsns[len(self.openfiles) + 10] = dsns

        return self.openfiles[wdmpath]

    def close(self, wdmpath):
        """
        Closes a WDM file.
        """

        if wdmpath in self.openfiles:
            retcode = hspf.wdflclpy(self.openfiles[wdmpath])
            self.retcode_check(retcode, function='wdflcl')
            self.dsns.pop(self.openfiles[wdmpath])
            self.openfiles.pop(wdmpath)
            if retcode == 0 and self.verbose: 
                print('successfully closed file {}\n'.format(wdmpath))
            if retcode != 0: 
                print('error closing {}, retcode {}\n'.format(wdmpath, retcode))

    def close_message(self):
        """
        Closes the message file.
        """

        hspf.wdflclpy(self.message)

    def create_dataset(self, 
                       wdmpath, 
                       dsn, 
                       attributes, 
                       pointers = None,
                       ):
        """
        Creates new dataset in self.wdmfp/dsn. Parameters:

        wdmpath:     path to the WDM file to add the dataset
        dsn:         the dataset number
        pointers:    a class containing the pointer attributes
        attributes:  a dictionary of the attributes for the DSN, using the six
                     character designation from the message file. These are:
        
                     tstype:      the type of time series                      
                     step:        the time step size (some restrictions)
                     station:     station ID
                     scenario:    scenario (e.g., OBSERVED)
                     location:    location
                     description: description
                     constituent: constituent
                     fill:        value to fill missing data
        """

        wdm_number = self.openfiles[wdmpath]

        if self.verbose: print('creating DSN {} in {}'.format(dsn, wdmpath))

        if hspf.wdckdtpy(wdm_number, dsn) == 1 and self.verbose: 
            print('DSN {} already exists'.format(dsn))

        dataset = DSN(wdm_number, dsn, self.message)

        if pointers is None: pointers = Pointers()
        dataset.create(pointers)

        if self.verbose: 
            print('created new DSN {}'.format(dsn))
            print('writing attributes to DSN {}'.format(dsn))

        for k, v in attributes.items():
            retcode, var = dataset.add_attribute(v, self.attributes[k])

            if retcode == 0 and self.verbose:
                print('set {} to {}'.format(k, v))
            elif self.verbose:
                print('failed to set value of {}'.format(k))
                raise

        self.dsns[wdm_number] = dsn

    def add_data(self, 
                 wdmpath, 
                 dsn, 
                 data, 
                 start_date,
                 ):
        """
        start_date:  the start date (datetime.datetime instance)
        data:        the data to be added to the dataset
        """

        if 1==2:
        #if len(data) > 275000:
            print('warning: maximum length of a WDM data set is 275,000')
            raise

        wdm_number = self.openfiles[wdmpath]

        if self.verbose: print('adding data to DSN {}'.format(dsn))
        dataset = DSN(wdm_number, dsn, self.message)
        retcode = dataset.add_data(data, start_date)

        self.retcode_check(retcode, function = 'wdtput')
        if retcode == 0 and self.verbose: 
            print('added data to {} DSN {}'.format(wdmpath, dsn))
        elif retcode != 0:
            print('failed to add data to DSN {}'.format(dsn))
            raise

    def import_exp(self, 
                   source, 
                   destination,
                   ):
        """
        Imports data from an exp file to a wdm file.
        """

        # open the new WDM file

        self.open(destination, 'w')

        # read the source file

        with open(source, 'r') as f: lines = f.readlines()

        datarow = False
        for line in lines:

            # get the dataset number and pointer info

            if line[0:3] == 'DSN':    
                dsn      = int(line[12:16].strip())
                pointers = Pointers(ndn = int(line[38:40]), 
                                    nup = int(line[48:50]),
                                    nsa = int(line[58:60]), 
                                    nsp = int(line[68:70]),
                                    ndp = int(line[77:80]))
    
            # get the attributes
    
            if line[2:7] == 'LABEL':  values = {}

            if line[4:10] in self.attributes:
                a = self.attributes[line[4:10]]

                if a['type'] == 'INTEGER': 
                    values[line[4:10]] = int(line[11:].strip())
                if a['type'] == 'CHARACTER': 
                    values[line[4:10]] = line[11:].strip()
                if a['type'] == 'REAL': 
                    values[line[4:10]] = float(line[11:].strip())
    
            # at the end of the attributes line create the dataset
    
            if line[2:11] == 'END LABEL':
                self.create_dataset(destination, dsn, values, 
                                    pointers = pointers)

            # get the data
    
            if line[2:10] == 'END DATA': datarow = False
    
            if datarow:
                if line[4] == ' ': 
                    numbers = [float(s) for s in line[4:].split()]
                else:
                    numbers = [float(s) for s in line[53:].split()]
    
                    # check if value is a repeat
    
                    if line[43] == '1': 
    
                        numbers = numbers * int(line[38:41].strip())
    
                data = data + numbers
    
            if line[2:6] == 'DATA':
    
                datarow = True
                data    = []
    
                # read the start date
    
                yr, mo, da, hr, mi, sec = (int(line[21:25]),         
                                           int(line[26:28].strip()),
                                           int(line[29:31].strip()), 
                                           int(line[32:34].strip()),
                                           int(line[35:37].strip()), 
                                           int(line[38:40].strip()))
    
                date = datetime.datetime(yr, mo, da)
    
                start = date + datetime.timedelta(hours = hr, minutes = mi, 
                                                  seconds = sec)
    
            if line[:7] == 'END DSN': 
                self.add_data(destination, dsn, data, start)

        self.close(destination)

    def renumber(self, 
                 wdmpath, 
                 odsn, 
                 ndsn,
                 ):
        """
        Renumbers a data seres.
        """

        if self.verbose: print('renumbering dsn {}'.format(odsn))
        wdmfp = self.openfiles[wdmpath]

        retcode = hspf.wddsrn(wdmfp, odsn, ndsn)
        self.retcode_check(retcode, function = 'wddsrn')

    def delete_dataset(self, 
                       wdmpath, 
                       dsn,
                       ):
        """
        Deletes a data series.
        """

        if self.verbose: print('deleting dsn {}'.format(dsn))
        wdmfp = self.openfiles[wdmpath]
        retcode = self.wddsdl(wdmfp, int(dsn))
        self.retcode_check(retcode, function = 'wddsdl')

    def get_attribute(self, 
                      wdmpath, 
                      dsn, 
                      attribute,
                      ):
        """
        Gets an attribute's value from a dataset. "attribute" is from the 
        list in the USGS database in the lib3.0/msg/adwdm folder.
        """

        if self.verbose: 
            print('getting attribute {} from DSN {}'.format(attribute, dsn))

        wdm_number = self.openfiles[wdmpath]
        dataset = DSN(wdm_number, dsn, self.message)
                      
        i = self.attributes['{:<6s}'.format(attribute)]['index']
        a = self.attributes['{:<6s}'.format(attribute)]

        v, retcode, var = dataset.get_attribute(i, a)

        # work around for strings

        if var == '%s': value = v.tostring().strip().decode('utf-8')
        else:           value = v[0]
        if retcode == 0 and self.verbose: 
            print('read %s' % attribute + ' = ' + var % value)
        elif retcode != 0: 
            value = None
            self.retcode_check(retcode, function = 'wdbsac')

        return value

    def get_dates(self, 
                  wdmpath, 
                  dsn,
                  ):
        """
        Gets the start and end date.  Returns the start and end dates as d
        atetime.datetime instances.

        wdmpath    -- path to the WDM file
        dsn        -- the dataset number
        """

        if self.verbose: 
            print('getting start and end dates for dataset {}'.format(dsn))

        wdm_number = self.openfiles[wdmpath]
        tdsfrc, llsdat, lledat, retcode = hspf.wtfndtpy(wdm_number, dsn, 1)

        if retcode == -6: 
            print('error: no data present')
            return None, None

        # Determine the number of values in the dataset
            
        dataset = DSN(wdm_number, dsn, self.message)

        tcode  = dataset.get_attribute(17, self.attributes['TCODE '])[0]
        tsstep = int(dataset.get_attribute(33, self.attributes['TSSTEP'])[0][0])

        if tcode == 2: unit = datetime.timedelta(minutes = tsstep)
        if tcode == 3: unit = datetime.timedelta(hours   = tsstep)
        if tcode == 4: unit = datetime.timedelta(days    = tsstep)

        n = hspf.timdifpy(llsdat, lledat, tcode, tsstep)

        # work around for "24th" hour start date

        yr, mo, da, hr, mi, se = llsdat

        start = (datetime.datetime(yr, mo, da) + 
                 datetime.timedelta(hours = float(hr), minutes = float(mi), 
                                    seconds = float(se)))
        end   = start + unit * n

        return start, end

    def get_data(self, 
                 wdmpath, 
                 dsn, 
                 start = None, 
                 end = None,
                 ):
        """
        Gets attributes and data from a DSN in a WDM file.

        wdmpath    -- path to the WDM file
        dsn        -- the dataset number
        attributes -- a list of the six character code from LIB3.0
        start      -- the start date and time as Python datetime.datetime class
        end        -- the end date and time as Python datetime.datetime class
        """

        wdm_number = self.openfiles[wdmpath]
        if self.verbose: print('getting data from dataset number %d' % dsn)

        if hspf.wdckdtpy(wdm_number, dsn) == 0:
            if self.verbose: 
                print('DSN {} in file {} does not exist'.format(dsn, wdmpath))
            return None

        dataset = DSN(wdm_number, dsn, self.message)

        tdsfrc, llsdat, lledat, retcode = hspf.wtfndtpy(wdm_number, dsn, 1)
        if retcode == -6: 
            print('no data present')
            return None

        if self.verbose: print('reading data from DSN {}'.format(dsn))

        if start != None:
            llsdat = [start.year, start.month, start.day, start.hour, 
                      start.minute, start.second]
        if end != None:
            lledat = [end.year, end.month, end.day, end.hour, end.minute, 
                      end.second]

        # Determine the number of values in the dataset
            
        tcode  = dataset.get_attribute(17, self.attributes['TCODE '])[0]
        tsstep = dataset.get_attribute(33, self.attributes['TSSTEP'])[0]

        n = hspf.timdifpy(llsdat, lledat, tcode, tsstep)

        # Get the data and put it into dictionary

        data, retcode = hspf.wdtgetpy(wdm_number, dsn, tsstep, llsdat, n,
                                      0, 30, tcode)
        self.retcode_check(retcode, function = 'wdtget')

        return data

    def get_datasets(self, wdmpath):
        """
        Returns the DSNs for all the datasets in a wdm file.
        """

        try: return self.dsns[self.openfiles[wdmpath]]
        except:
            print('Error, file not found')
            return None

    def retcode_check(self, 
                      retcode, 
                      function = ' ',
                      ):
        """
        Checks to see if function worked (retcode >= 0).
        """

        if retcode < 0 and self.verbose: 
            print('WDM library function error {} {}'.format(retcode, function))

class Pointers:
    """
    A class to store the pointer attributes of a DSN. See LIB3.0 for more.
    """

    def __init__(self, 
                 dstype = 1, 
                 ndn = 10, 
                 nup = 10, 
                 nsa = 16, 
                 nsp = 60, 
                 ndp = 380,
                 ):

        self.dstype = dstype
        self.ndn    = ndn
        self.nup    = nup
        self.nsa    = nsa
        self.nsp    = nsp
        self.ndp    = ndp
        
class DSN:
    """
    A class to read and write data to a dataset in a WDM file.
    """

    def __init__(self, 
                 wdm, 
                 number, 
                 message,
                 ):
        
        self.wdm     = wdm     # WDM file number
        self.number  = number  # DSN number
        self.message = message # Message file number

        self.times = None
        self.data  = None

        self.timecodes = {1: 'seconds', 
                          2: 'minutes', 
                          3: 'hours', 
                          4: 'days',
                          5: 'months',  
                          6: 'years',
                          }

    def create(self, pointers):
        """
        Creates the DSN. The variables are pointers.
        """

        # psa - pointer to search attribute space; this has something to do
        # with FORTRAN definitions/pointers (see source code)

        psa = hspf.wdlbaxpy(self.wdm, self.number, pointers.dstype, 
                            pointers.ndn, pointers.nup, pointers.nsa, 
                            pointers.nsp, pointers.ndp)

    def add_attribute(self, v, a):
        """
        Adds value "v" for attribute "a" with index "i" to the DSN; 
        see the self.attributes dictionary for more info.
        """

        i = a['index']

        if a['type'] == 'INTEGER':
            retcode = hspf.wdbsaipy(self.wdm, self.number, self.message, i, v)
            var = '%d'
        if a['type'] == 'REAL':
            retcode = hspf.wdbsarpy(self.wdm, self.number, self.message, i, v)
            var = '%.6e'
        if a['type'] == 'CHARACTER':
            
            # this is a bit tricky since FORTRAN pre-allocates the character
            # size, so add blanks to the end

            v = v[:a['length']] + ' ' * (a['length'] - len(v))

            retcode = hspf.wdbsacpy(self.wdm, self.number, self.message, i, v)
            var = '%s'
            
            if retcode < 0: print('problem adding attribute to %d, retcode = %d'
                                  % (i, retcode))

        return retcode, var

    def get_attribute(self, i, a):
        """
        Reads the value of the attribute "a" with index "i" from the dataset; 
        see the self.attributes dictionary for more info.
        """

        if a['type'] == 'INTEGER':
            v, retcode = hspf.wdbsgipy(self.wdm,self.number, i, a['length'])
            var = '%d'
        if a['type'] == 'REAL':
            v, retcode = hspf.wdbsgrpy(self.wdm,self.number, i, a['length'])
            var = '%.6e'
        if a['type'] == 'CHARACTER':
            v, retcode = hspf.wdbsgcpy(self.wdm,self.number, i, a['length'])
            var = '%s'

        return v, retcode, var

    def add_data(self, data, start):
        """
        Add data to the dataset.
        """

        tcode, retcode = hspf.wdbsgipy(self.wdm, self.number, 17, 1)
        tstep, retcode = hspf.wdbsgipy(self.wdm, self.number, 33, 1)

        #Convert Python date to WDM format

        llsdat = [start.year, start.month, start.day, start.hour, start.minute,
                  start.second]

        self.data = data

        return hspf.wdtputpy(self.wdm, self.number, tstep, llsdat, 1, 
                                0, tcode, data) 

if __name__ == '__main__':

    # basic test that it works ok

    filename    = 'test.wdm'
    number      = 1003
    start       = datetime.datetime(2000,1,1)
    
    attributes = {
        'TSTYPE': 'EXAM',
        'TCODE ': 4,
        'TSSTEP': 1,
        'STAID ': '',
        'IDSCEN': 'OBSERVED',
        'IDLOCN': 'EXAMPLE',
        'STANAM': '',
        'IDCONS': '',
        'TSFILL': 0
        }
    
    data = [34.2, 35.0, 36.9, 38.2, 40.2 , 20.1, 18.4, 23.6]
    
    print('\noriginal list to input to file %s = ' % filename, data, '\n')

    # make the wdm file

    wdm = WDMUtil(verbose = True)
    wdm.open(filename, 'w')
    wdm.create_dataset(filename, number, attributes)
    wdm.add_data(filename, number, data, start)
    wdm.close(filename)

    # open the file and read it

    wdm.open(filename, 'r')

    # get the data

    data = wdm.get_data(filename, number)

    # get the dates for the time series

    dates = wdm.get_dates(filename, number)

    # make the times for the dataset

    times = [dates[0] + (dates[1] - dates[0]) / len(data) * i
             for i in range(len(data))]

    # get the attributes

    values = {}
    for attribute in attributes: 
        values[attribute] = wdm.get_attribute(filename, number, attribute)

    # print it out

    print('')
    print('Datasets in file {}: {}\n'.format(
            filename, wdm.get_datasets(filename)))
    print('Attributes of dataset number {}\n'.format(number))              
    for value in values.items(): print(*value)
    print('')
    print('Time series returned from dataset {} in file {}:\n'.format( 
            number, filename))
    for t, d in zip(times, data):
        print(t, d)

    print('')
    wdm.close(filename)

    # remove the file

    os.remove(filename)
 
    directory = os.path.abspath(os.path.dirname(__file__))

    # tests the exp import function (file from hspexp2.4/data/huntday)
        
    source = '{}/../examples/data/huntday/huntobs.exp'.format(directory)
    destination = 'testexp.wdm'
    
    # initialize the WDM environment
    
    if not os.path.isfile(source):
        print('warning, unable to import file')
        exit()

    wdm = WDMUtil(verbose = True)
    wdm.import_exp(source, destination)
    
    # test it out
    
    wdm.open(destination, 'r')
    data = wdm.get_data(destination, 106)
    
    print('')
    print('total precipitation (in):', data.sum())
    print('number of values in time series', len(data))
    print('maximum daily precipitation (in):', data.max())

    wdm.close(destination)

    # remove the file

    os.remove(destination)
