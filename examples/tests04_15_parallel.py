# test04_15.py
#
# David Lampert (djlampert@gmail.com)
#
# this is a repeat of tests 4 to 15 but shows how you can run them in parallel!
# running parallel simulations can speed up the calibration process

# Import the hspf library et al

from pyhspf          import hspf
from multiprocessing import Pool, cpu_count

import os, time

# this is the path to the message file in PyHSPF (hspfmsg.wdm)

pyhspfdirectory = os.path.dirname(hspf.__file__)
messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(pyhspfdirectory)

# there is really nothing to do but just run these--but if you feel creative
# and wish to contribute it would be nice if someone would pull out the results
# of these and make plots to illustrate what the simulations do.

# create a function for mapping the processes

def run(ucifile):

    print('performing HSPF simulation {}'.format(ucifile))
    hspf.hsppy(ucifile, messagepath)
    print('successfully completed simulation\n')

# setup the main routine

def main():    

    os.chdir('data/tests')
    ucis = ['TEST{:02d}.UCI'.format(i) for i in range(4,16)]

    # keep track of the time

    start = time.time()

    # serial run

    for f in ucis: run(f)

    # parallel run

    pstart = time.time()
    pool = Pool(cpu_count())
    pool.map(run, ucis)

    end = time.time()
    
    print('\nserial run time = {:.2f} seconds\n'.format(pstart - start))
    print('parallel run time = {:.2f} seconds\n'.format(end - pstart))

if __name__ == '__main__': main()
