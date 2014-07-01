# checks the differences between HSPF v11 and v12 using Python

import filecmp, os

UTILf = 'utchar.for utdate.for utgnrl.for utnumb.for utsort.for utcpgn.for ckfsdg.for uscnvt.for'

UTILc  = 'dirlis_.c getdir_.c MALLO~BA.C FileName.c'

UTILincs = 'color.inc const.inc fversn.inc'

ADWDM  = 'utwdmd.for utwdmf.for utwdt1.for wdmess.for wdatm1.for msimpt.for msexpt.for msopen.for prtfil.for ztwdmf.for adutil.for usysux.for wdopux.for'

ADWDMincs = ''.join([f + ' ' for f in os.listdir('c:/hspf12/lib3.0/src/adwdm')
                     if f[-3:].lower() == 'inc'])

WDM    = 'wdbtch.for wdatrb.for wdatm2.for wdsptm.for wdimpt.for wdexpt.for wdtms2.for wdtms1.for wdtble.for wdtbl2.for wddlg.for wdatru.for tsbufr.for wdtms3.for wdmid.for'

WDMincs = ''.join([f + ' ' for f in os.listdir('c:/hspf12/lib3.0/src/wdm')
                   if f[-3:].lower() == 'inc'])

HSPF   = 'hosuper.for hfiles.for hdatut.for hgenut.for himp.for himpgas.for himpqua.for himpsld.for himpwat.for hioosup.for hioosv.for hiotsin.for hiouci.for hiowrk.for hper.for hperagut.for hperair.for hpergas.for hpermst.for hperpes.for hperpho.for hperqua.for hpersed.for hpertmp.for hpertra.for hperwat.for hpernit.for hpersno.for hprbut.for hrch.for hrchaci.for hrchcon.for hrchgqu.for hrchhtr.for hrchnut.for hrchoxr.for hrchphc.for hrchplk.for hrchrq.for hrchut.for hrchhyd.for hrchsed.for hringen.for hringeut.for hrinoput.for hrinseq.for hrints.for hrunut.for hruntsgp.for hruntsgq.for hruntsgt.for hruntsgw.for hruntsut.for hrintss.for hrinwdm.for hruntspt.for hruntspw.for htsinsi.for htsinsz.for htssut.for hutop.for hutopinp.for hwdmut.for hutdura.for specact.for hspf.for hspfec.for hspfitab.for hrinopn.for hextutil.for'

HSPFincs = ''.join([f + ' ' for f in os.listdir('c:/hspf12/lib3.0/src/hspf122')
                    if f[-3:].lower() == 'inc'])

NEWAQT = 'sgdate.for utclas.for utdir.for utmap.for utwin.for'

NEWAQTincs = ''.join([f + ' ' for f in os.listdir('c:/hspf12/lib3.0/src/newaqt12')
                     if f[-3:].lower() == 'inc'])

path = 'c:/HSPF12/lib3.0/src/'

def compare_versions(testfolder, dir):

    # split the string

    testfolder = testfolder.split()
    testfolder = [f for f in testfolder if f[0].lower() == 'p']

    # make list of the relative paths

    #dir1 = 'hspf'
    #dir2 = 'hspf122'
    dir1 = dir
    dir2 = dir

    v11 = ['c:/lib3.2/src/%s/' % dir1 + f        for f in testfolder]
    v12 = ['c:/hspf12/lib3.0/src/%s/' % dir2 + f for f in testfolder]

    # iterate through the files and test similarities

    for f11, f12 in zip(v11, v12):

        try:
            with open(f11, 'r') as f: l11 = f.readlines()
            with open(f12, 'r') as f: l12 = f.readlines()

            same = True
            for l1, l2 in zip(l11, l12):
                if l1 != l2:
                    same = False
                    #break
                    #print(l1)
                    #print(l2)

            if not same: print('%s files are different' % f12)

        except: print(f11, 'not found')
        #else: print('%s files are same' % f12)

        #input()

testfolder = HSPFincs
dir = 'parameters'

compare_versions(testfolder, dir)

# findings:

# UTIL: 
#    utdate.for (updates to date/time routines)
#
# ADWDM: 
#    utwdmd.for 
#        -updates to error logging for WDM files
#        -allow to close message file (???? existing subroutine for this)
#        -comments involving printing variables
#    utwdt1.for (2**21=2097152 max record value ???)
#    wdmess.for
#        -extraneous comments
#        -added subroutine WDGTAT that isn't called by LIB3.0
#        -added search attribute "creation date" to dataset
#    ztwdmf.for
#        -calls routine WMSBCX for DSN of -1
#        -some routines related to ANNIE and HSPFEXP (???)
#        -subroutine WMSGFD related to GenScen
#
#    zcntrl.inc
#*************** changed MXSCBF from 300 to 3000 ********************
#
# WDM:
#    wdbtch.for, wdsptm.for (added creation date attribute)
#    wdimpt.for (something related to shortening blocks)
#    wdexpt.for (something related to CURNOV???)
#    wdtms1.for
#        -comments
#        -creation date attribute
#        -compression of missing data
#    wdtms2.for 
#        -first record number TDSFRC = zero check
#        -missing data compression
#        -creation date attribute
#    wdtble.for (creation date attribute)
#    wddlg.for (creation date attribute)
#    wdmid.for (checks to see that WDM ID is not assigned)
#
# Summary: None of these changes seem particularly important; if anything they
# should help with error checking.
#
# HSPF:
#
#    hextutil (added IHM flag)
#    hspfitab 
#        -error checking for strings
#        -PEST file input added to FILES array (optional)
#    hspfec.for (changes involve error checking in reading the UCI file)
#
#    hruntsgt.for (comments re: Keywords)
#    hruntspt.for (subroutine IFRAME renamed to HFRAME ???; comments Keywords)
#
# Parameter include files:
#
#    cosupm.inc (increased size of in memory OSUP file MAXOSP from 221 to 521)
#    crin1.inc  (doubled parameters)
#    pmxexi.inc (fixed typo)
#    pmxftb.inc (max ftables increased from 200 to 500)
#    pmxopn.inc (increased max operations MAXOPN from 200 to 500)
#    pmxosv.inc (max osv increased from 18000 to 500000)
#    pmxpst.inc (PEST supplement file array sizes; not in lib3.2)
#    pmxrow.inc (max inpad rows increased from 1800 to 80000)
#    pmxsta.inc (max status variables; not in lib3.2)
#    pmxvnm.inc (max special action variables increased from 1500 to 1800)
#    posvm.inc  (size of in memory OSV file increased from 6000 to 15000)
#    pspcnd.inc (parameters for special action conditions changed)
#        -MXSPCN 2000 to 5000
#        -MXSPBK=1000 to 5000
#        -MXSPCH=2000 to 5000
#        -MXSPCR=4000 to 15000
#    pspuvr.inc (dimension size of special actions)
#        -MXSPUV 2000 to 3000
#        -MXSPUX 4000 to 5000
#    pspvqd.inc (special action user variable quantities)
#        -MXSPVQ 2000 to 7500
#        -MXPIPE same (20000)
# NEWAQT12: 
