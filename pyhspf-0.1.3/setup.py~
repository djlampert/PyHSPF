#!/usr/bin/env python
"""
This is the setup file.
"""

import os, sys, io, zipfile, shutil, pickle

from numpy.distutils.core import Extension, setup
from distutils            import sysconfig
from urllib               import request

_directory = '{}/pyhspf'.format(sysconfig.get_python_lib())

_d = (
"""PyHSPF contains a library of subroutines to run the Hydrological 
Simulation Program in Fortran (HSPF), Version 12.2, Python extensions to 
the HSPF library, and a series of classes for building HSPF input files, 
performing simulations, and postprocessing simulation results.  

HSPF requires flowline and catchment data for a stream network, land use 
data for the stream reach subbasins, time series data of climate 
parameters, and hydrology parameters for each land use category/subbasin.  
These data sources (with the exception of the hydrology parameters) can be 
supplied externally as needed (e.g., using Python extensions for 
geographic information systems (GIS) software). Alternatively, a 
series of preprocessing classes and routines were developed based on 
flowline and catchment data from the National Hydrolography Dataset 
Version 2 (NHDPlus), climate data from the National Climate Data Center, 
and landuse data from the National Agricultural Statitistics Service (NASS)
Cropland Data Layer (CDL). The "core" module requires NumPy, SciPy, and
Matplotlib, and can be used for generating input files. The preprocessing 
routines require GDAL, PyShp, and Pillow and make a series of specific
assumptions about the location and availability of data sources on the 
computer. For more info contact me.

PyHSPF can be used to assimilate the data into an HSPF model, build the 
HSPF input files, simulate the model over a period of time, and then 
provide statistics and plots of the simulation output. A series 
of examples are provided (independently) to illustrate PyHSPF usage."""
)

_s = """Python Extensions for utilizing the Hydrological 
Simulation Program in Fortran (HSPF) Version 12.2"""

_l = """
PyHSPF, Version 0.1.2

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
"""

# this section will download the source code for hspf, and change the file
# names (basically gets the source code in the right place for Python)

# minor issue with some windows

if os.name == 'nt':

    python = sys.executable[:-10]
    gnu = python + 'lib/site-packages/numpy/distutils/fcompiler/gnu.py'
    with open(gnu, 'r') as f: s = f.read()
    i = s.index('raise NotImplementedError')
    if s[i-5:i] != 'pass#': 
        s = s[:i] + 'pass#' + s[i:]
        with open(gnu, 'w') as f: f.write(s)

# directory for the source files

destination = '{}/hspf13'.format(os.getcwd())

if not os.path.isdir(destination): 
    
    os.mkdir(destination)

    # url to HSPF source zip file data

    url = 'http://hspf.com/pub/hspf/HSPF12.2SourceForBASINS4.0.zip'

    # zip file with modifications to source code

    modified = 'pyhspf/HSPF13.zip'

    # source and destination working directories

    source = '{}/hspf12'.format(os.getcwd())

    # subdirectory path to source files in lib3.0

    src = 'lib3.0/src'

    # subdirectory "UTIL" c files

    utilc = ['DIRLIS_.C',
             'GETDIR_.C',
             'MALLO~BA.C',
             'FILEN~AI.C',
             ]

    # subdirectory "UTIL" fortran files

    utilf = ['UTCHAR.FOR', 
             'Utdate.FOR', 
             'UTGNRL.FOR', 
             'UTNUMB.FOR', 
             'UTSORT.FOR', 
             'UTCPGN.FOR', 
             'CKFSDG.FOR', 
             'USCNVT.FOR', 
             'DATSYS77.FOR',
             ]

    # subdirectory "ADWDM" files

    adwdm = ['UTWDMD.FOR',
             'UTWDMF.FOR',
             'utwdt1.FOR',
             'WDMESS.FOR',
             'WDATM1.FOR',
             'MSIMPT.FOR',
             'MSEXPT.FOR',
             'MSOPEN.FOR',
             'PRTFIL.FOR',
             'ZTWDMF.FOR',
             'ADUTIL.FOR',
             'USYSUX.FOR',
             'WDOPUX.FOR',
             ]

    # subdirectory "WDM" files

    wdm = ['WDBTCH.FOR',
           'WDATRB.FOR',
           'WDATM2.FOR',
           'WDSPTM.FOR',
           'WDIMPT.FOR',
           'WDEXPT.FOR',
           'WDTMS2.FOR',
           'WDTMS1.FOR',
           'WDTBLE.FOR',
           'WDTBL2.FOR',
           'WDDLG.FOR',
           'WDATRU.FOR',
           'TSBUFR.FOR',
           'WDTMS3.FOR',
           'WDMID.FOR',
           ]

    # subdirectory "HSPNODSS" files

    hspnodss = ['HDSSX.FOR']

    # subdirectory "HSPF" files

    hspf122 = ['HOSUPER.FOR',
               'HDATUT.FOR',
               'HGENUT.FOR',
               'HIMP.FOR',
               'himpgas.FOR',
               'HIMPQUA.FOR',
               'HIMPSLD.FOR',
               'HIMPWAT.FOR',
               'Hioosup.FOR',
               'HIOOSV.FOR',
               'HIOTSIN.FOR',
               'HIOUCI.FOR',
               'HIOWRK.FOR',
               'HPER.FOR',
               'HPERAGUT.FOR',
               'HPERAIR.FOR',
               'HPERGAS.FOR',
               'HPERMST.FOR',
               'HPERPES.FOR',
               'HPERPHO.FOR',
               'HPERQUA.FOR',
               'HPERSED.FOR',
               'HPERTMP.FOR',
               'HPERTRA.FOR',
               'HPERWAT.FOR',
               'HPERNIT.FOR',
               'HPERSNO.FOR',
               'HPRBUT.FOR',
               'HRCH.FOR',
               'HRCHACI.FOR',
               'HRCHCON.FOR',
               'HRCHGQU.FOR',
               'HRCHHTR.FOR',
               'HRCHNUT.FOR',
               'HRCHOXR.FOR',
               'HRCHPHC.FOR',
               'HRCHPLK.FOR',
               'HRCHRQ.FOR',
               'HRCHUT.FOR',
               'HRCHHYD.FOR',
               'HRCHSED.FOR',
               'HRINGEN.FOR',
               'HRINGEUT.FOR',
               'HRINOPUT.FOR',
               'HRINSEQ.FOR',
               'HRINTS.FOR',
               'HRUNUT.FOR',
               'HRUNTSGP.FOR',
               'HRUNTSGQ.FOR',
               'HRUNTSGT.FOR',
               'HRUNTSGW.FOR',
               'HRUNTSUT.FOR',
               'HRINTSS.FOR',
               'HRINWDM.FOR',
               'HRUNTSPT.FOR',
               'HRUNTSPW.FOR',
               'HTSINSI.FOR',
               'HTSINSZ.FOR',
               'HTSSUT.FOR',
               'HUTOP.FOR',
               'HUTOPINP.FOR',
               'HWDMUT.FOR',
               'HUTDURA.FOR',
               'Specact.FOR',
               'HSPF.FOR',
               'HSPFEC.FOR',
               'HSPFITAB.FOR',
               'HRINOPN.FOR',
               'HEXTUTIL.FOR',
               'HBMPRAC.FOR',
               'HREPORT.FOR',
               'HIOSTA.FOR',
               'HIRRIG.FOR',
               'HRCHSHD.FOR',
               'HPESTUT.FOR',
               ]

    # modifications to source files in the "modified" zip archive

    modifiedfiles = ['djl.f', 
                     'HFILES.FOR',
                     'hspf_f2py_module.f',
                     ]

    # download the source files from the web

    if not os.path.isdir(source):

        r = request.Request(url)

        print('\ndownloading the HSPF source code...\n')

        with io.BytesIO(request.urlopen(r).read()) as b:

            with zipfile.ZipFile(b) as zf: zf.extractall(source)

    if not os.path.isfile(modified):
        print('error, you seem to be missing the modified files "HSPF13.zip"')
        exit()

    if not os.path.isdir('{}/{}/djl'.format(source, src)):

        # bring in the modified files

        with zipfile.ZipFile(modified) as zf:
 
            zf.extractall('{}/{}'.format(source, src))

    print('moving source files to a new directory...\n')

    subs = ['UTIL', 'ADWDM', 'WDM', 'hspf122', 'djl']
    incs = {sub: [f for f in os.listdir('{}/{}/{}'.format(source, src, sub))
                  if f[-3:] == 'inc' or f[-3:] == 'INC'] for sub in subs}

    directories = {'UTIL':     utilc + utilf + incs['UTIL'], 
                   'ADWDM':    adwdm         + incs['ADWDM'],
                   'WDM':      wdm           + incs['WDM'],
                   'HSPNODSS': hspnodss,
                   'hspf122':  hspf122       + incs['hspf122'],
                   'djl':      modifiedfiles + incs['djl'],
                   }

    for sub, files in directories.items():

        for f in files:

            s = '{}/{}/{}/{}'.format(source, src, sub, f)

            # lower the case of the files and change "for" files to "f" files

            g = f.lower()
            if g[-3:] == 'for': g = g[:-2]

            d = '{}/{}'.format(destination, g) 

            shutil.copy(s, d)

# if the source files exists, install

datafiles = ['hspfmsg.wdm', 'attributes']

# files

files = ['hspf13/{}'.format(f) 
         for f in os.listdir('hspf13') if f[-1] == 'c' or f[-1] == 'f']

gflags = ['-O3', '-fno-automatic', '-fno-align-commons']
setup(
    name = 'pyhspf',
    version = '0.1.2',
    description = _s,
    author = 'David Lampert',
    author_email = 'djlampert@gmail.com',
    url = 'http://www.anl.gov',
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
        'Intended Audience :: Watershed modelers familiar with Python',
        ],
    packages = ['pyhspf', 
                'pyhspf.core', 
                'pyhspf.preprocessing', 
                'pyhspf.calibration',
                'pyhspf.forecasting',
                ],
    package_dir = {'pyhspf': 'pyhspf',
                   'core': 'pyhspf/core', 
                   'preprocessing': 'pyhspf/preprocessing', 
                   'calibration': 'pyhspf/calibration',
                   'forecasting': 'pyhspf/forecasting',
                   },
    package_data = {'pyhspf': ['HSPF13.zip'],
                    'pyhspf.core': datafiles
                    },
    ext_modules=[Extension(name = 'hspf', sources = files, 
                           extra_f77_compile_args = gflags)]
    )

