# test04_15.py
#
# David J. Lampert (djlampert@gmail.com)
#
# this is a repeat of tests 04 to 15 but shows how you can run them in parallel!
# running parallel simulations can speed up the calibration process
# of course the run time will not be faster if you don't have multiple cores
# on your computer

# Import the hspf library et al

from pyhspf          import hspf
from multiprocessing import Pool, cpu_count

import os, time

# parallel or serial flag

parallel = True

# path to the message file in PyHSPF (hspfmsg.wdm)

pyhspfdirectory = os.path.dirname(hspf.__file__)
messagepath = '{}/pyhspf/core/hspfmsg.wdm'.format(pyhspfdirectory)

# create a function for mapping the processes

def run(ucifile):

    print('performing HSPF simulation {}'.format(ucifile))
    hspf.hsppy(ucifile, messagepath)
    print('successfully completed simulation\n')

# setup the main routine

def main():    

    if os.path.isdir('data'):

        os.chdir('data')

    else:
        print('you appear to be missing the data files in the "data"')
        print('directory that are needed for this simulation')
        raise

    ucis = ['test{:02d}.uci'.format(i) for i in range(4,16)]

    # keep track of the time

    start = time.time()

    # serial or parallel run

    if parallel:

        pool = Pool(cpu_count())
        pool.map(run, ucis)

    else:
        
        for f in ucis: run(f)

    end = time.time()
    
    print('run time = {:.2f} seconds\n'.format(end - start))

if __name__ == '__main__': main()
