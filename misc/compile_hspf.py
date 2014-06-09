#!/usr/bin/python
#
# compile_hspf.py
# author: David J. Lampert
# email: djlampert@gmail.com
# date: 01/06/2014 
# purpose: the setup file to install pyhspf from the source code.
# dependencies: mingw, python3, numpy
# instructions: pretty simple. if the source files change, this may need to 
# be modified. otherwise, just point to the url of the source code and the
# modified files, and make sure you have the gnu compilers as environment
# variables, the right python3 executable, and numpy installed. then run this
# file, and it should generate the HSPF library, the python extensions,
# the message file, and the attribute file (everything for pyhspf).

from urllib import request

import os, sys, io, zipfile, shutil, pickle

# remove temporary files or not

clean = True

# url to HSPF source zip file data

url = 'http://hspf.com/pub/hspf/HSPF12.2SourceForBASINS4.0.zip'

# directory path to zip file with modifications to source code

modified = 'HSPF13.zip'

# path to python directory

if os.name == 'nt':      python = 'C:/python32'
elif os.name == 'posix': python = '/usr/bin/python3.2'
else: raise

########### you should not need to modify anything below this line #############

# source and destination working directories

source      = '{}/hspf12'.format(os.getcwd())
destination = '{}/hspf13'.format(os.getcwd())

# source files from HSPF 12.2 listed by directory in lib3.0

src = 'lib3.0/src'

# UTIL c files

utilc = ['DIRLIS_.C',
         'GETDIR_.C',
         'MALLO~BA.C',
         'FILEN~AI.C',
         ]

# UTIL fortran files

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

# ADWDM files

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

# WDM files

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

# HSPNODSS files

hspnodss = ['HDSSX.FOR']

# HSPF files

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

# names of object files after compiling

ofiles = [f[:f.index('.')] + '.o' for f in utilc + utilf + adwdm + wdm + 
          hspf122 + ['HDSSX.FOR', 'djl.f', 'HFILES.FOR']]

# fortran files

ffiles = utilf + adwdm + wdm + hspf122 + ['HDSSX.FOR', 'djl.f', 'HFILES.FOR']

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

# make a new directory to work in and bring all the files together

if not os.path.isdir(destination): 
    
    os.mkdir(destination)

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

            # lower the case of the c and inc files

            if f[-3:] == 'INC' or f[-3:] == 'inc': f = f.lower()
            if f[-2:] == '.C': f = f[:-2] + '.c'

            d = '{}/{}'.format(destination, f)

            shutil.copy(s, d)

# go into the directory and build the HSPF library

os.chdir(destination)

# minor fixes for f2py on windows

if os.name == 'nt':

    distfile = '{}/Lib/distutils/distutils.cfg'.format(python)
    if not os.path.isfile(distfile):
        print('adding a configuration file to the Python library...\n')
        with open(distfile, 'w') as f:
            f.write('[build]\ncompiler=mingw32')

    # 64 bit has minor issue

    if sys.maxsize > 2**32:
        gnu = python + '/lib/site-packages/numpy/distutils/fcompiler/gnu.py'
        with open(gnu, 'r') as f: s = f.read()
        i = s.index('raise NotImplementedError')
        if s[i-5:i] != 'pass#': 
            s = s[:i-1] + 'pass#' + s[i-1:]
            with open(gnu, 'w') as f: f.write(s)

    # gfortran flags

    gflags = '-O3 -fno-automatic -fno-align-commons'

    commands = ['gcc -ansi -O3 -c ' + ' '.join([c[:-1] + 'c' for c in utilc]),
                'gfortran -c ' + ' '.join(ffiles) + ' ' + gflags,
                'gfortran -shared -mrtd -O3 -o hspflib.dll ' + ' '.join(ofiles),
                ('{0}/python.exe {0}/Scripts/f2py.py '.format(python) +
                 '-m hspf -c hspf_f2py_module.f -L. -lhspflib'),
                ]

    if not os.path.isfile('hspflib.dll') or not os.path.isfile('hspf.pyd'):
        print('compiling the source files...\n')
        for command in commands: os.system(command)

# linux

elif os.name == 'posix':

    # gfortran flags

    gflags = '-O3 -fPIC -fno-automatic -fno-align-commons'

    commands = [('gcc -ansi -O3 -c -fPIC ' + 
                 ' '.join([c[:-1] + 'c' for c in utilc])),
                'gfortran -c ' + ' '.join(ffiles) + ' ' + gflags,
                'gfortran -shared -O3 -o libhspf.so ' + ' '.join(ofiles),
                'f2py3 -m hspf -c hspf_f2py_module.f -L/usr/local/lib -lhspf',
                'sudo ldconfig',
                ]

    if not os.path.isfile('libhspf.so'):
        print('compiling the source files...\n')
        for command in commands: os.system(command)

# make the message file used by HSPF for error reporting.

class WDMattribute:
    """A class to store WDM attributes from the USGS library."""

    def __init__(self, attribute, index):
        """Defines the attribute and its index."""

        self.attribute = attribute
        self.index = index

        self.desc    = None
        self.type    = None
        self.length  = None
        self.min     = None
        self.max     = None
        self.default = None
        self.value   = None
        self.help    = None

    def add_type(self,fortran_type):
        """adds the type of variable FORTRAN uses."""

        self.type = fortran_type

    def add_length(self, length):
        """Adds the length of the attribute."""

        self.length = int(length.strip())

    def add_description(self, desc):
        """Adds the description of the attribute."""

        self.desc = desc.strip()

    def add_range(self, line):
        """Adds the min and max values if applicable."""

        colon = line.index(':')
        minimum = line[10:colon]
        maximum = line[colon+1:]

        if self.type == 'INTEGER':
            self.min = int(minimum.strip())
            self.max = int(maximum.strip())
        if self.type == 'REAL':
            self.min = float(minimum.strip())
            self.max = float(maximum.strip())

    def add_default(self, default):
        """Adds the default value if applicable."""

        if self.type == 'INTEGER': self.default = int(default.strip())
        if self.type == 'REAL':    self.default = float(default.strip())

    def create_help(self):
        "Creates a place for the help info (can be multiple lines)."""

        self.help = ''

    def add_help(self, info):
        """Adds helpful information about the attribute."""

        self.help = self.help + info.strip() + '\n'

    def add_value(self, value):
        """Adds value to the parameter."""

        self.value = value

    def print_info(self):
        """Prints out all the attributes."""

        print('ATTRIBUTE:', self.attribute)
        print('INDEX:', self.index)
        if self.desc is not None: print('DESCRIPTION:', self.desc)
        if self.type is not None: print('FORTRAN VARIABLE TYPE:', self.type)
        if self.length is not None: print('ARRAY LENGTH:', self.length)
        if self.max is not None: 
            print('RANGE OF VALUES:', self.min, 'TO', self.max)
        if self.default is not None: print('DEFAULT VALUE:', self.default)
        if self.value is not None: print('VALUE:', self.value)
        if self.help is not None: print(self.help)

def make_messagefile(lib, name = 'hspfmsg.wdm', verbose = True):
    """builds the HSPF message file from a list of the paths to the source
    sequential Fortran files (.SEQ)."""

    sys.path.append(os.getcwd())

    import hspf

    if verbose: print('building the HSPF message file')

    # SEQ files needed from the various subdirectories in LIB3.2\MSG

    adwdmseqs  = ['ATTR001', 'ATTR051', 'ATTR101', 'ATTR151', 'ATTR201', 
                  'ATTR301', 'ATTR351', 'ATTR401']
    aideseqs   = ['MESSAGE']
    waideseqs  = ['AWFEB', 'TSLIST', 'AGPLOT']
    awstatseqs = ['TSCMPR', 'A193', 'PROFDR']
    annseqs    = ['PGENER', 'QTPRNT']
    #hspfseqs   = ['hiouci', 'hprbut', 'hruntspt',  'himpqua', 
    #              'hspfitab', 'hrch', 'hdatut', 'hringeut', 'htsinsi', 
    #              'hringen', 'hspf', 'hspfec', 'hpersno', 'hperpho', 
    #              'hpernit', 'hperpes', 'hrchnut', 'hperwat', 'hrchaci', 
    #              'hrchgqu', 'hrchhyd', 'hrchphc', 'hrchsed', 'hrchhtr', 
    #              'hrchplk', 'hwdmut', 'hdssut', 'hdssx', 'hutop', 'hutopinp', 
    #              'hrunut', 'hrinseq', 'hrinwdm', 'hrindss', 'hruntsgw', 
    #              'himp', 'himpwat', 'hperagut', 'hruntsgq', 'hperqua', 'hper',
    #              'hruntsgp', 'hruntsgt', 'hruntspw', 'hutdura', 'hruntsut', 
    #              'hruntsgd', 'hruntspd', 'hrints', 'hrintss', 'htssut', 
    #              'specact', 'perlndts', 'implndts', 'rchrests', 'copyts', 
    #              'pltgents', 'displyts', 'duranlts', 'generts', 'mutsints', 
    #              'perlnd', 'implnd', 'rchres', 'copy', 'pltgen', 'disply', 
    #              'gener', 'duranl', 'mutsin']

    #newaqtseqs = ['sgtabl', 'agmap', 'prwplt', 'ucimod', 'ucirea', 'wsgsim',
    #              'wsgutl', 'dspeci', 'wsgsys', 'tsplot', 'durani', 'tsfreq',
    #              'sturbn']

    hspfseqs   = os.listdir('{}/hspf122'.format(lib))
    newaqtseqs = os.listdir('{}/NEWAQT12'.format(lib))

    newaqtseqs.remove('DSNSEL.SEQ')

    # build a list of all the seq files from LIB3.0

    seqfiles = []

    for seq in aideseqs:   seqfiles.append('{}/AIDE/{}.SEQ'.format(lib, seq))
    for seq in waideseqs:  seqfiles.append('{}/WAIDE/{}.SEQ'.format(lib, seq))
    for seq in awstatseqs: seqfiles.append('{}/AWSTAT/{}.SEQ'.format(lib, seq))
    for seq in annseqs:    seqfiles.append('{}/ANN/{}.SEQ'.format(lib, seq))
    for seq in hspfseqs:   seqfiles.append('{}/hspf122/{}'.format(lib, seq))
    for seq in newaqtseqs: seqfiles.append('{}/NEWAQT12/{}'.format(lib, seq))

    n = 35  # Fortran file number for the message file
    E = 99  # Fortran file number for the error file
    m = 36  # Fortran file number for the seq file
    
    if os.path.exists(name):
        if verbose: print('warning: message file exists, deleting')
        os.remove(name)

    # open up the new file and the error file

    hspf.wdbopnpy(n, name, 2)
    hspf.erroropen(E)

    # make the attribute file

    attributes = {}
    for seq in adwdmseqs:
        if verbose: print('adding attributes to the message file')
        p = lib + '/ADWDM/{}.SEQ'.format(seq)
        hspf.seqopen(p, m)
        hspf.seqimport(n, m, 0)
        hspf.seqclose(m)

        # make an independent Python file of the ADWDM attributes

        with open(p, 'r') as f:
            lines = f.readlines()
            for line in lines:
                if line[1:10] == 'ATTRIBUTE':
                    attributes[int(line[30:33].strip())] = \
                        WDMattribute(line[12:18], int(line[30:33].strip()))
                    current = attributes[int(line[30:33].strip())]
                if line[1:5] == 'TYPE': 
                    current.add_type(line[10:19].strip())
                if line[1:7] == 'LENGTH': 
                    current.add_length(line[9:11].strip())
                if line[1:5] == 'DESC': 
                    current.add_description(line[10:])
                if line[1:6] == 'RANGE': 
                    current.add_range(line)
                if line[1:8] == 'DEFAULT': 
                    current.add_default(line[10:19])
                if line[1:5] == 'HELP': 
                    current.create_help() 
                if line[:3] == '   ': 
                    current.add_help(line[3:])

    # work around for inconistency in lib3.0 (thanks Aquaterra)

    hspf.seqopen(lib + '/ADWDM/attr251.seq', m)
    hspf.seqimport(n, m, 0)
    hspf.seqclose(m)

    with open(lib + '/ADWDM/attr251.seq', 'r') as f:
        lines = f.readlines()
        for line in lines:
            if line[1:10] == 'ATTRIBUTE':
                attributes[int(line[30:33].strip())] = \
                    WDMattribute(line[12:18], int(line[30:33].strip()))
                current = attributes[int(line[30:33].strip())]
            if line[1:5] == 'TYPE': 
                current.add_type(line[10:19].strip())
            if line[1:7] == 'LENGTH': 
                current.add_length(line[9:11].strip())
            if line[1:5] == 'DESC': 
                current.add_description(line[10:])
            if line[1:6] == 'RANGE': 
                current.add_range(line)
            if line[1:8] == 'DEFAULT': 
                current.add_default(line[10:19])
            if line[1:5] == 'HELP': 
                current.create_help() 
            if line[:3] == '   ': 
                current.add_help(line[3:])

    wdmattributes = {v.attribute: {'index':     k,
                                   'desc':      v.desc,
                                   'type':      v.type,
                                   'length':    v.length,
                                   'min':       v.min,
                                   'max':       v.max,
                                   'default':   v.default,
                                   'value':     v.value,
                                   'help':      v.help
                                   }
                     for k, v in attributes.items()}

    with open('attributes', 'wb') as f: pickle.dump(wdmattributes, f)

    # now import the data sets

    for f in seqfiles:
        if verbose: print('importing data from {}'.format(f))
        hspf.seqopen(f, m)
        hspf.seqimport(n, m, n)
        hspf.seqclose(m)

    # close up the wdm file and the error file

    hspf.seqclose(E)
    hspf.wdflclpy(n)

if os.name == 'nt':    
    distributables = ['hspflib.dll', 'hspf.pyd']
elif os.name == 'posix': 
    distributables = [f for f in os.listdir(os.getcwd()) if f[-3:] == '.so']

for f in distributables: shutil.copy(f, '..')

os.chdir('..')

# path to LIB3.0/MSG

lib = '{}/lib3.0/msg'.format(source)

make_messagefile(lib)

if clean: 
    os.remove('error.fil')
    if os.path.isdir(destination): shutil.rmtree(destination)
    if os.path.isdir(source): shutil.rmtree(source)

