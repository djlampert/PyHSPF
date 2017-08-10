# setup.py
#
# David J. Lampert (djlampert@gmail.com)
#
# the setup file for PyHSPF

_version   = '0.2.4'

# check for the required and optional dependencies

try:
    import numpy
except:
    print('error: required package "NumPy" is not installed')
    raise

try:
    import scipy
except:
    print('error: required package "SciPy" is not installed')
    raise

try:
    import matplotlib
except:
    print('error: required package "Matplotlib" is not installed')
    raise

try:
    import gdal
except:
    print('warning: preprocessing dependency "GDAL" is not installed')

try:
    import shapefile
except:
    print('warning: preprocessing dependency "PyShp" is not installed')

try:
    import PIL
except:
    print('warning: preprocessing dependency "Pillow" is not installed')
    
import os, sys

from numpy.distutils.core import Extension, setup
from distutils            import sysconfig

_directory = '{}/pyhspf'.format(sysconfig.get_python_lib())

_d = (
"""
PyHSPF contains a library of subroutines to run the Hydrological 
Simulation Program in Fortran (HSPF), Python extensions to the HSPF 
library, and a series of classes for building HSPF input files, 
performing simulations, and postprocessing simulation results.  

HSPF requires flowline and catchment data for a stream network, land use 
data for the stream reach subbasins, time series of climate and hydrology
data. A series of preprocessing classes were developed to extract data 
from the following publically-available databases on the World Wide Web:

- National Hydrography Dataset Plus Version 2 (NHDPlus)
- National Water Information System (NWIS)
- National Inventory of Dams (NID)
- Cropland Data Layer (CDL)
- National Solar Radiation Database (NSRDB)
- Global Historical Climate Network Daily (GHCND)
- Global Summary of the Day (GSOD)
- Hourly Precipitation Database (DSI-3240)

The "core" module requires NumPy, SciPy, and Matplotlib, and can be used to
generate the HSPF input files. The preprocessing routines require GDAL, 
PyShp, and Pillow.

PyHSPF can be used to assimilate the data into an HSPF model, build the 
HSPF input files, simulate the model over a period of time, and then 
provide statistics and plots of the simulation output. A series 
of examples is provided to illustrate PyHSPF usage.
"""
)

_s = """Python Extensions for utilizing the Hydrological Simulation Program in Fortran (HSPF)"""

_l = (
"""
PyHSPF, Version {}

Copyright (c) 2014, UChicago Argonne, LLC
All rights reserved.
Copyright 2014. UChicago Argonne, LLC. This software was produced under U.S. 
Government contract DE-AC02-06CH11357 for Argonne National Laboratory (ANL), 
which is operated by UChicago Argonne, LLC for the U.S. Department of Energy. 
The U.S. Government has rights to use, reproduce, and distribute this software.
NEITHER THE GOVERNMENT NOR UCHICAGO ARGONNE, LLC MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.  If 
software is modified to produce derivative works, such modified software 
should be clearly marked, so as not to confuse it with the version available 
from ANL.

Additionally, redistribution and use in source and binary forms, with or 
without modification, are permitted provided that the following conditions 
are met:

1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution. 
3. Neither the name of UChicago Argonne, LLC, Argonne National Laboratory, 
   ANL, the U.S. Government, nor the names of its contributors may be used 
   to endorse or promote products derived from this software without specific 
   prior written permission. 

THIS SOFTWARE IS PROVIDED BY UCHICAGO ARGONNE, LLC AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL UCHICAGO ARGONNE, LLC OR CONTRIBUTORS BE 
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.
""".format(_version))

# link flags

if os.name == 'nt': lflags = ['-static']
else:               lflags = []

# any additional files that are needed (blank for now)

data_files = []

data_directory = sysconfig.get_python_lib()

# if the source files exists, install

package_data = ['hspfmsg.wdm', 'attributes']

# find all the fortran and c files

files = ['hspf13/{}'.format(f) 
         for f in os.listdir('hspf13')
         if f[-1] == 'c' or f[-1] == 'f']

fflags = ['-O3', '-fno-automatic', '-fno-align-commons']
requires = ['numpy', 'scipy', 'matplotlib']

setup(
    name = 'pyhspf',
    version = _version,
    description = _s,
    author = 'David Lampert',
    author_email = 'david.lampert@okstate.edu',
    url = 'https://github.com/djlampert/PyHSPF',
    license = _l,
    long_description = _d,
    keywords = ['hydrology', 
                'watershed modeling', 
                'GIS',
                ],
    platforms = ['Windows', 'Linux'],
    classifiers = [
        'Programming Language :: Python',
        'Programming Language :: Python :: 3',
        'Development Status :: 4 - Beta',
        'Intended Audience :: Science/Research',
        ],
    packages = ['pyhspf', 
                'pyhspf.core', 
                'pyhspf.preprocessing', 
                'pyhspf.calibration',
                'pyhspf.forecasting',
                ],
    package_dir = {'pyhspf':        'pyhspf',
                   'core':          'pyhspf/core', 
                   'preprocessing': 'pyhspf/preprocessing', 
                   'calibration':   'pyhspf/calibration',
                   'forecasting':   'pyhspf/forecasting',
                   },
    package_data = {'pyhspf': ['HSPF13.zip'],
                    'pyhspf.core': package_data
                    },
    install_requires = requires,
    data_files = [(data_directory, data_files)],
    ext_modules=[Extension(name = 'hspf', 
                           sources = files, 
                           extra_link_args = lflags,
                           extra_f77_compile_args = fflags
                           )]
    )
