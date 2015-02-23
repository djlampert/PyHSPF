# hbnexample.py
#
# David J. Lampert
#
# This example shows how to read an HBN output file from HSPF using PyHSPF's
# HBNReader class, which is (very loosely) adapted from hspfbintoolbox

import os

# import the reader

from pyhspf import HBNReader

reader = HBNReader()

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

# for example, get and display the average value across the simulation

ave = sum(values) / len(values)

print('the average value of {} was {:.2f}'.format(var, ave))
