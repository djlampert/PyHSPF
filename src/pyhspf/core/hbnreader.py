# hbnreader.py
#
# adapted by David J. Lampert from Tim Cera's hspfbintoolbox
#
# contains the HBNReader class to read data from an HSPF binary output 
# file (.hbn extension)
#
# last updated: 02/01/2015

import os, struct, datetime

class HBNReader:
    """Reads a binary output file from an HSPF simulation."""

    def all_occurences(self, s):
        """
        Returns all the indices of 'PERLND', 'IMPLND', and 'RCHRES' in byte 
        string "s".
        """

        index = 0
        while True:
            perlnd = s.find(b'PERLND', index)
            implnd = s.find(b'IMPLND', index)
            rchres = s.find(b'RCHRES', index)
            if all([perlnd == -1, implnd == -1, rchres == -1]): 
                return
            else:
                if perlnd == -1: perlnd = len(s)
                if implnd == -1: implnd = len(s)
                if rchres == -1: rchres = len(s)
                index = min(perlnd, implnd, rchres)
                yield index
                index += 6

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

        the data are packaged as a list of (time, value) pairs.
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

            # unpack information about the data set

            s = data[i-4:i+20]
            rectype, operation, number, section = struct.unpack('I8sI8s', s)

            operation = operation.strip().decode()
            section = section.strip().decode()

            # add an operation type dictionary to the data dictionary

            if operation not in results: 
                results[operation]   = {}
                variables[operation] = {}

            # add operation number dictionary to the operation type dictionary

            if number not in results[operation]: 
                results[operation][number]   = {}
                variables[operation][number] = {}

            # add the operation module to the operation number dictionary

            if section not in results[operation][number]: 
                results[operation][number][section]   = {}
                variables[operation][number][section] = []

            # rectype = 0 mean a list of the variables

            if rectype == 0:

                # read the length of the record

                s = data[i-8:i-4]
                r1, r2, r3, r4 = struct.unpack('4B', s)

                # calculate the length

                reclen = r1 // 4 + r2 * 2**6 + r3 * 2**14 + r4 * 2**22

                # loop through the variable names for the section

                j = i + 20

                while j < i + 20 + reclen - 28:

                    # get the length of the variable name

                    s = data[j:j+4]
                    l = struct.unpack('I', s)[0]

                    # read the variable name

                    s = data[j + 4:j + 4 + l]
                    v = struct.unpack('{}s'.format(l), s)[0].decode()

                    # add the variable name in the data structures

                    variables[operation][number][section].append(v)
                    results[operation][number][section][v] = []

                    j += 4 + l

            # rectype = 1 means data values for the variables

            if rectype == 1:

                # read the date

                s = data[i + 20:i + 48]
                units, l, yr, mo, da, hr, mi = struct.unpack('7I', s)

                # Data record

                n = len(variables[operation][number][section])

                # adjust the HSPF time to real time

                if hr == 24:
                    t = (datetime.datetime(yr, mo, da, 0, mi) +
                         datetime.timedelta(days = 1))
                else:
                    t = datetime.datetime(yr, mo, da, hr, mi)

                # read the data

                s = data[i + 48:i + 48 + 4 * n]
                values = struct.unpack('{}f'.format(n), s)

                # package up the data in the output dictionary

                for n, v in zip(variables[operation][number][section], values):

                    results[operation][number][section][n].append((t, v))

        return results

if __name__ == '__main__':

    # the base.hbn file is distributed with BASINS

    filename = 'base.hbn'

    if not os.path.isfile(filename):
        print('\nError: hbn file {} does not exist!\n'.format(filename))
        raise

    # create an instance of the HBN reader

    reader = HBNReader()

    # read it

    results = reader.read(filename)

    # the data are packaged in a series of dictionaries in the following order:
    #
    #     -operation type (e.g., PERLND, IMPLND, RCHRES)
    #     -operation number (e.g., 101, 102)
    #     -operation section (e.g., PWATER, SEDMNT, HYDR)
    #     -section variable name (e.g., PERO, SURO, SURS)
    #     -list of (date, value) pairs (e.g., 10/01/2001, 3.4)
    #
    # so for example, to get the values of PERO from PERLND 101:

    var  = 'PERO'
    data = results['PERLND'][101]['PWATER'][var]

    # the data are packaged as (time, value) pairs

    for t, v in data: 

        i = t.year, t.month, t.day, var, v
        print('On {:04d}-{:02d}-{:02d}, the value of {} was {:.2f}'.format(*i))

    # the data can easily be regrouped into lists of dates and values using zip

    times, values = zip(*data)

    # get the average

    ave = sum(values) / len(values)

    print('the average value of {} was {:.2f}'.format(var, ave))
