# test03.py
#
# David Lampert (djlampert@gmail.com)
#
# runs the TEST03.UCI simulation

# Import the hspf library and WDM utility modules

from pyhspf import hspf, WDMUtil

import os

os.chdir('data/tests')

# this is the path to the message file in PyHSPF (hspfmsg.wdm)

pyhspfdirectory = os.path.dirname(hspf.__file__)
messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(pyhspfdirectory)

# the 3rd simulation adds more data to "test.wdm," so we need to create the
# datasets as before.

wdm = WDMUtil(verbose = True)

wdm.open('test.wdm', 'rw')

attributes = {'TCODE ': 4,      # hourly units 
              'TSSTEP': 1,      # one unit (hourly) time step
              'TSFORM': 1       # cloud cover is an average across the step
              }

attributes['TSTYPE'] = 'CLND'

wdm.create_dataset('test.wdm', 140, attributes)

attributes['TSTYPE'] = 'CLDC'

wdm.create_dataset('test.wdm', 135, attributes)

wdm.close('test.wdm')

# run it

hspf.hsppy('test03.uci', messagepath)


