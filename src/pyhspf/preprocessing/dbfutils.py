# dbf.py
#
# David Lampert (djlampert@gmail.com)
#
# Reads attribute values for dbf files from the NHDPlus dataset

import struct, datetime

def dbf_format(value, typ, deci):
    """formats a value from a dbf file."""

    if   typ == 'C':
        value = value.strip()
    elif typ == 'D':
        if len(value.strip()) == 8:
            value=datetime.date(int(value[:4]),int(value[4:6]),int(value[6:8]))
        else: value = '<null>'
    elif typ == 'N' and deci > 0:
        if value == '': value = 0
        else: value = float(value.strip())
    elif typ == 'N' and deci == 0:
        if value == '': value = 0
        else: value = int(value.strip())
    elif typ == 'L':
        value = (value in 'YyTt' and 'T') or (value in 'NnFf' and 'F') or '?'
    elif typ == 'F':
        value = float(value)

    return value

def read_dbf(filename, attributes = None, comids = None, verbose = True):
    """ 
    Reads data from a .dbf dataset for a given set of comids.

    filename   -- The path to the dbf file
    attributes -- A list of the attributes to get. If None, returns all.
    comids     -- The unique identifiers in the database. If None, returns all.
    """

    if comids is not None: # make sure the format is correct
        for i in range(len(comids)):
            if isinstance(comids[i], int):
                comids[i] = str(comids[i])
                while len(comids[i]) < 9: comids[i] = ' ' + comids[i]
            assert isinstance(comids[i], str)
            assert len(comids[i]) == 9

    if attributes is not None:
        if isinstance(attributes, str): attributes = [attributes]
        assert isinstance(attributes, list)

    if verbose: print('reading database %s\n' % filename)

    f = open(filename, 'rb')

    # read the field descriptions

    numrec, lenheader = struct.unpack('<xxxxLH22x', f.read(32))
    numfields = (lenheader - 33) // 32

    names    = ['DeletionFlag']
    types    = ['C']
    lengths  = [1]
    decimals = [0]

    for fieldno in range(numfields):
        name, typ, length, deci = struct.unpack('<11sc4xBB14x', f.read(32))

        name = name.decode('utf-8').split('\x00')[0]
        names.append(name)
        types.append(typ.decode('utf-8'))
        lengths.append(length)
        decimals.append(deci)

    if attributes is None: attributes = names

    # find the comid and attribute indices

    try:        comid_index = names.index('COMID')
    except: 
        try:    comid_index = names.index('ComID')
        except: comid_index = names.index('Comid')

    # figure out the range of bytes that are comids

    start = sum(lengths[:comid_index])
    end = start + lengths[comid_index]

    # figure out the length of each field in the records

    fmt = ''.join(['%ds' % l for l in lengths])
    fmtsiz = struct.calcsize(fmt)

    # read the records into a numpy array; need a  work around for the 
    # first record, which has '\r' instead of a blank

    records = [struct.unpack('2' + fmt[1:], f.read(fmtsiz + 1))]

    # get the rest of the records as a list of types

    records = records + [struct.unpack(fmt, f.read(fmtsiz))
                         for i in range(1,numrec)]

    all_values = [v for v in zip(*records)]
    all_comids = all_values[comid_index]

    if comids is None:
        indices = (i for i in range(len(all_comids)))
    else:
        indices = (i for i in range(len(all_comids)) 
                   if all_comids[i].decode('utf-8') in comids)

    # go through the attributes and add them to dictionary of lists as needed

    values = {}
    ntdjs   = []
    for a in attributes:
        values[a] = []
        j = names.index(a)
        ntdjs.append((names[j], types[j], decimals[j], j))

    for i in indices:
        for n, t, d, j in ntdjs:
            values[n].append(dbf_format(all_values[j][i].decode('utf-8'), t, d))

    f.close()

    return values
