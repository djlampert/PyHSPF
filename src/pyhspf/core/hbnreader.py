# hbnreader.py
#
# adapted by David J. Lampert from Tim Cera's hspfbintoolbox
#
# contains the HBNReader class to read data from an HSPF binary output 
# file (.hbn extension)
#
# last updated: 02/10/2015

import os, struct, datetime

class HBNReader:
    """Reads a binary output file from an HSPF simulation."""

    def all_occurences(self, s):
        """
        Returns all the indices of 'PERLND', 'IMPLND', and 'RCHRES' in byte 
        string "s".
        """

        i = 0
        while True:
            perlnd = s.find(b'PERLND', i)
            implnd = s.find(b'IMPLND', i)
            rchres = s.find(b'RCHRES', i)
            if all([perlnd == -1, implnd == -1, rchres == -1]): 
                return
            else:
                if perlnd == -1: perlnd = len(s)
                if implnd == -1: implnd = len(s)
                if rchres == -1: rchres = len(s)
                i = min(perlnd, implnd, rchres)
                yield i - 8
                i += 6

    def read(self, binfilename):
        """
        Reads data in the file into a data structure and returns the structure.

        The data are packaged in a series of dictionaries as follows:
    
           -operation type (e.g., PERLND, IMPLND, RCHRES)
           -operation number (e.g., 101, 102)
           -operation section (e.g., PWATER, SEDMNT, HYDR)
           -section variable name (e.g., PERO, SURO, SURS)
           -list of (date, value) pairs (e.g., 10/01/2001, 3.4)
    
        so for example, to get the values of PERO from PERLND 101 in a file:

        results = hbnreader.read(file)
        data = results['PERLND'][101]['PWATER']['PERO']

        the data are packaged asx a list of (time, value) pairs.
        """
        
        # read the file into memory (if the files were huge this might not be
        # the most efficient way to do this, but most hbn files are small)

        with open(binfilename, 'rb') as f: data = f.read()

        # create data structures to organize the information in the file
        
        results   = {}   # values
        variables = {}   # keeps track of the variable order in the records

        # parse through the file to the end using the indices of all perlnd, 
        # implnd, and rchres byte strings in the data

        for i in self.all_occurences(data):

            # read the length of the record

            r1, r2, r3, r4 = struct.unpack('4B', data[i:i+4])

            # calculate the length

            reclen = r1 // 4 + r2 * 2**6 + r3 * 2**14 + r4 * 2**22

            # unpack information about the data set

            rectype, op, no, sec = struct.unpack('I8sI8s', data[i+4:i+28])

            op = op.strip().decode()
            sec = sec.strip().decode()

            # add an operation type dictionary to the data dictionary

            if op not in results: 
                results[op]   = {}
                variables[op] = {}

            # add operation number dictionary to the operation type dictionary

            if no not in results[op]: 
                results[op][no]   = {}
                variables[op][no] = {}

            # add the operation module to the operation number dictionary

            if sec not in results[op][no]: 
                results[op][no][sec]   = {}
                variables[op][no][sec] = []

            # rectype = 0 mean a list of the variables

            if rectype == 0:

                # loop through the variable names for the section

                j = i + 32

                while j + 4 < i + reclen:

                    # get the length of the variable name

                    l = struct.unpack('I', data[j-4:j])[0]

                    # read the variable name

                    v = struct.unpack('{}s'.format(l),data[j:j+l])[0].decode()

                    # add the variable name in the data structures

                    variables[op][no][sec].append(v)
                    results[op][no][sec][v] = []

                    j += 4 + l

            # rectype = 1 means data values for the variables

            if rectype == 1:

                # read the date

                u, l, yr, mo, da, hr, mi = struct.unpack('7I', data[i+28:i+56])

                # Data record

                n = len(variables[op][no][sec])

                # adjust the HSPF time to real time

                t = (datetime.datetime(yr, mo, da, 0, mi) +
                     datetime.timedelta(hours = hr))

                # read the data

                values = struct.unpack('{}f'.format(n), data[i+56:i+56+4*n])

                # package up the data in the output dictionary

                for n, v in zip(variables[op][no][sec], values):

                    results[op][no][sec][n].append((t, v))

        return results

