# make_messagefile.py
#
# David J. Lampert
# djlampert@gmail.com
#
# Purpose: makes the message file used by HSPF for error reporting.

import os, hspf

def make_messagefile(lib, name = 'hspfmsg.wdm', verbose = True):
    """builds the HSPF message file from a list of the paths to the source
    sequential Fortran files (.SEQ)."""

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

    hspfseqs   = os.listdir(lib + '/hspf122')
    newaqtseqs = os.listdir(lib + '/NEWAQT12')

    print(newaqtseqs)
    newaqtseqs.remove('DSNSEL.SEQ')

    # build a list of all the seq files from LIB3.2

    seqfiles = []

    for seq in aideseqs:   seqfiles.append(lib + '/AIDE/%s.SEQ' % seq)
    for seq in waideseqs:  seqfiles.append(lib + '/WAIDE/%s.SEQ' % seq)
    for seq in awstatseqs: seqfiles.append(lib + '/AWSTAT/%s.SEQ' % seq)
    for seq in annseqs:    seqfiles.append(lib + '/ANN/%s.SEQ' % seq)
    for seq in hspfseqs:   seqfiles.append(lib + '/hspf122/%s' % seq)
    for seq in newaqtseqs: seqfiles.append(lib + '/NEWAQT12/%s' % seq)

    n = 35  # Fortran file number for the message file
    E = 99  # Fortran file number for the error file
    m = 36  # Fortran file number for the seq file
    
    if os.path.exists(name):
        if verbose: print('warning: message file exists, deleting')
        os.remove(name)

    # open up the new file and the error file

    hspf.wdbopnpy(n, name, 2)
    hspf.erroropen(E)

    # add the attributes from adwdm first

    for seq in adwdmseqs:
        if verbose: print('adding attributes to the message file')
        f = lib + '/ADWDM/%s.SEQ' % seq
        hspf.seqopen(f, m)
        hspf.seqimport(n, m, 0)
        hspf.seqclose(m)

    # work around for inconistency in lib3.0 (thanks Aquaterra)

    hspf.seqopen(lib + '/ADWDM/attr251.seq', m)
    hspf.seqimport(n, m, 0)
    hspf.seqclose(m)

    # now import the data sets

    for f in seqfiles:
        if verbose: print('importing data from %s' % f)
        hspf.seqopen(f, m)
        hspf.seqimport(n, m, n)
        hspf.seqclose(m)

    # close up the wdm file and the error file

    hspf.wdflclpy(n)
    hspf.wdflclpy(E)

# path to LIB3.0\MSG

lib = '../../msg'

make_messagefile(lib)

