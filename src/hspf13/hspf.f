C
C
C
      SUBROUTINE   HSPF
     I                   (FILES,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Main routine for HSPF without file management or system dep stuff.
C     Copies Data Set from User's Control Input to memory, finds start
C     and end of the Data Set and hands control to appropriate part
C     of HSPF software for execution of user's instructions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FILES(15),RETCOD
C
C     + + + ARGUMENT DEFINTIONS + + +
C     FILES  - unit numbers of files from files block in uci or application
C     RETCOD - return code - 0:run completed
C                            1:interp only
C                            2:errors in interp
C                            3:no run keyword found
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,SCLU,SGRP,MESSU,MSGFL,INITFG,CLEN,CONT,KCNT,
     $             ECOUNT,KWDDIM(1),KWDTYP(1),MKFILS,EMFG,IOPT,ITIM
      CHARACTER*1  CHSTR1(20),KWDLIB(12)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  PMXTFT, KEYUCI, DMPKEY, INTERP, OSUPER, WMSGTT
      EXTERNAL  INIKEY, HDMEST, HDMES3, SDELAY, FILCLO
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + END SPECIFICATIONS + + +
C
C     WRITE(*,*) 'HSPF:HSPF:entry'
      IOPT= 10
      CALL HDMES3(IOPT,'HSPF')
      I1= 1
      I0= 0
C     no errors yet
      ECOUNT= 0
C
      MESSU= FILES(1)
      MSGFL= FILES(15)
C
      SCLU= 201
C
C     title block to output file
      SGRP= 1
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     start of job message
      SGRP= 2
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     first pre-process input for a data set message
      SGRP= 3
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     get major keywords (RUN)
      SGRP= 21
      INITFG= 1
      CLEN= 20
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) KWDLIB,KWDDIM,KWDTYP
C
C     look for a fresh set of keywords
      CALL INIKEY
C
C     look for major keywords
      CLEN= 4
      CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M             ECOUNT,
     O             KCNT)
      CALL DMPKEY
C
      IF (ECOUNT .EQ. 0) THEN
C       a clean run data set was found, interpret it
        IOPT= 1
        SGRP= 58
        CALL HDMEST (IOPT,MSGFL,SCLU,SGRP)
        MKFILS= 1
        CALL INTERP (I1,MKFILS,
     M               FILES,
     O               EMFG,RETCOD)
C
        IF (RETCOD .EQ. 0) THEN
C         run data set interpreted without error - run it
          IOPT= 1
          SGRP= 59
          CALL HDMEST (IOPT,MSGFL,SCLU,SGRP)
          ITIM= 20
          CALL SDELAY (ITIM)
          CALL OSUPER (FILES,RETCOD)
C         all done - end of job
          SGRP= 4
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          ITIM= 40
          CALL SDELAY (ITIM)
        END IF
      ELSE
C       no run keyword found
        RETCOD= 3
      END IF
C     close all files but wdm
      CALL FILCLO
C
C     get rid of status display window
      IOPT= 10
      CALL HDMES3(IOPT,'HSPF Done')
      IOPT= 99
      CALL HDMES3(IOPT,'HSPF Done')
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTERP
     I                    (UCLOFG,MKFILS,
     M                     FILES,
     O                     EMFG,RETCOD)
C
C     + + + PURPOSE + + +
C     Read and process a run data set in the user's control input
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    UCLOFG,MKFILS,FILES(15),EMFG,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCLOFG - uci may be closed flag - 0 - leave open
C     MKFILS - flag to indicate if instruction files should be written,
C                0-no,1-yes
C     FILES  - array of file unit numbers
C     EMFG   - english/metric units flag (english-1,metric-2)
C     RETCOD - return code 0:interp ok
C                          1:no execution desired
C                          2:errors found
C                         -1:user interrupt
C
C     + + + PARAMETERS + + +
      INTEGER      MAXBLK,MAXMLK
      PARAMETER    (MAXBLK=30)
      PARAMETER    (MAXMLK=50)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION INTERP1 + + +
      INCLUDE    'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RUNWID,I,I1,I0,KEYND,KEYST,CONDFG,
     $             MESSU,MSGFL,STFIL,LKWOPR,NKWOPR,
     $             NDAMON(12),RUNMIN,RUNFG,SEDAT(10),IOPT,
     $             SCLU,SGRP,MSLINX(MAXMLK,3),NMLTBS,KTYP,SPOUT,IHMFG
      CHARACTER*80 CHSTR,DUMCHR
      CHARACTER*12 BLNK12
      CHARACTER*1  KWDOPR(8,MAXBLK)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(SEDAT,SDATIM),(SEDAT(6),EDATIM)
      CHARACTER*1  CHSTR1(80)
      INTEGER      SDATIM(5),EDATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TABBLK,MSLKBK,OPNBLK,DUMPER,HDMES3
      EXTERNAL   TIMSER,OSUP,PSPECL,ZIPI,MDATBK
      EXTERNAL   PTHBLK,GETSE,PMXTFT,UCIGEN,CLOUCI
      EXTERNAL   SETPST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAMON/31, 0,31,30,31,30,31,31,30,31,30,31/
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C
C     + + + END SPECIFICATIONS + + +
C
C     WRITE(*,*) 'HSPF:INTERP:entry'
      RETCOD= 0
C
      I0= 0
      I1= 1
C
      MESSU= FILES(1)
      MSGFL= FILES(15)
      STFIL= FILES(9)
C
      SCLU= 201
C
C     put file unit numbers in common block
      DO 10 I= 1, 15
        FILE(I)= FILES(I)
 10   CONTINUE
C
C     interp message to output file
      SGRP= 5
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     warning info
      I= 10
      CALL ZIPI (I,I0,
     O           WCOUNT)
C
C     error info
      ECOUNT= 0
C
      CALL UCIGEN (MSGFL,MESSU,FILES(8),MKFILS,MAXOPN,MAXBLK,
     M             ECOUNT,WCOUNT,
     O             SEDAT,SDATIM,EDATIM,RUNMIN,OUTLEV,
     O             RESMFG,RUNFG,EMFG,IHMFG,DUMCHR,NIVLS,IVLLIB,
     O             EXGTAB,GRPTAB,OPNTAB,NXGRPS,NGRPS,
     O             NOPNS,LKWOPR,NKWOPR,KWDOPR,SPOUT)
C
      IF (FILE(7) .GT. 0) THEN
C       pest supplemental file
        CALL SETPST (MESSU, MSGFL, FILE(7), OUTLEV,
     M               ECOUNT)
      END IF
C
C     ftables block
      KTYP= 4
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL TABBLK (KEYST,KEYND,OUTLEV,MESSU,
     I             MSGFL,MAXFTB,
     M             ECOUNT,
     O             TABINX,NFTABS)
C
C     mass-link block
      CALL MSLKBK (OUTLEV,MESSU,
     I             MSGFL,MAXMLK,
     M             ECOUNT,
     O             MSLINX,NMLTBS)
C
C     month-data block
      KTYP= 14
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL MDATBK (KEYST,KEYND,OUTLEV,MESSU,
     I             MSGFL,MAXMDT,
     M             ECOUNT,
     O             MDTINX,NMDATS)
C
C     pathnames block
      KTYP= 15
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL PTHBLK (KEYST,KEYND,OUTLEV,MESSU,MSGFL,
     M             ECOUNT)
C
C     process the operation-type blocks
      CALL OPNBLK (SDATIM,EDATIM,NDAMON,EMFG,SCLU,
     I             LKWOPR,NKWOPR,KWDOPR,
     O             RUNWID,RETCOD)
C     black out table name on screen
      BLNK12 = '            '
      IOPT   = 3
      CALL HDMES3 (IOPT,BLNK12)
      IOPT   = 4
      CALL HDMES3 (IOPT,BLNK12)
C
C     process any special action instructions
      KTYP= 9
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL PSPECL (KEYST,KEYND,NDAMON,SDATIM,EDATIM,SPOUT,
     I             STFIL,
     M             RUNWID,
     O             CONDFG)
cthj  added next 7 lines so special action variable names still in 
cthj  common block crintp when status file is read - moved from osuper
C
C     status file read
C      WRITE(99,*) 'HSPF:status file unit:',STFIL
      IF ( (MKFILS .EQ. 1) .AND. (STFIL .NE. 0) ) THEN
C        WRITE(99,*) 'HSPF:read status file'
        CALL GSTVFL (MESSU,MSGFL,STFIL,SDATIM)
      END IF
C
C     look for formats block
      KTYP= 6
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      IF ( (KEYST .NE. 0) .AND. (OUTLEV .GT. 2) ) THEN
C       formats block is present, processing message
        SGRP= 10
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C       dump formats block
        CALL DUMPER (KEYST,KEYND,MESSU)
C       end processing message
        SGRP= 11
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
      END IF
C
C     process blocks dealing with time series linkages -
C     ext sources, network, and ext targets
      CALL TIMSER (SDATIM,EDATIM,MAXMLK,MSLINX,NMLTBS,RUNWID,IHMFG)
C
      IF (MKFILS .EQ. 1) THEN
C       write the operations supervisor instruction file
        CALL OSUP (SDATIM,EDATIM,RUNMIN,RUNWID)
      END IF
C
C     interp complete message
      SGRP= 12
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
      IF (RUNFG .EQ. 0) THEN
C       user did not want to execute
        SGRP= 14
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        RETCOD= 1
      END IF
C
      IF (ECOUNT .GT. 0) THEN
C       errors found in uci
        SGRP= 13
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        RETCOD= 2
      END IF
C
      IF (CONDFG.EQ.0 .AND. UCLOFG.NE.0) THEN
C       free up space for keywords and uci recs
        CALL CLOUCI
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNBLK
     I                   (SDATIM,EDATIM,NDAMON,EMFG,SCLU,
     I                    LKWOPR,NKWOPR,KWDOPR,
     O                    RUNWID,RETCOD)
C
C     + + + PURPOSE + + +
C     Process each <operation - type> block in the run data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SDATIM(5),EDATIM(5),NDAMON(12),EMFG,SCLU,
     $            LKWOPR,NKWOPR,RUNWID,RETCOD
      CHARACTER*1 KWDOPR(LKWOPR,NKWOPR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     SCLU   - cluster containing general info
C     LKWOPR - length of operation name keywords
C     NKWOPR - number of operation name keywords
C     KWDOPR - operation name keywords
C     RUNWID - maximum run span width allowed - 0 if no restrictions
C     RETCOD - return code -1 for user interupt
C
C     + + + PARAMETERS + + +
      INCLUDE    'pmxosv.inc'
C
C     + + + COMMON BLOCKS- INTERP2 + + +
      INCLUDE    'crin2.inc'
      INCLUDE    'crin2c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      GRP,I,I0,I1,I12,I2,IX,KTYP,KCNT,
     $             J,K,KEYND,KEYST,LTABT,NDELT,NTABT,
     $             OMCODE,OMCNT,OSVKEY,START,CNUM,CLEN(1),
     $             MESSU,MSGFL,LCLU,SGRP,INITFG,CONT,
     $             TABBAS,TKEYST,TKEYND,TABXXX(MAXTTP),IOPT
      CHARACTER*12 OPTNAM,BLNK12
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(OPTNAM,OPTNM1)
      CHARACTER*1  CHSTR1(80),OPTNM1(12)
C
C     + + + FUNCTIONS + + +
      INTEGER    OPNNO, CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   DUMPER,PPERLN,PIMPLN,PRCHRE,PCOPY,PPLTGN,PIRGTF,PIRSTF
      EXTERNAL   OPNNO,CHKSTR,PDISPL,PDURAN,PGENER,PMUTSN,WMSGTT,PPIRRG
      EXTERNAL   PBMPRC,PREPRT
      EXTERNAL   GETKNM,GETSE,PMXTFC,KEYUCI,ZIPI,HDMES2,HDMES3,HDMESN
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (3X,I3,2X,A12)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I12= 12
      IX= 32767
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      OSVKEY= 0
C
C     initialize global reach irrigation flag
      CALL PIRSTF (I0)
C
C     loop thru all operations by type
      OMCNT= 0
 10   CONTINUE
C       put blank operation number to screen
        BLNK12 = '            '
        IOPT   = 3
        CALL HDMES3 (IOPT,BLNK12)
        IOPT   = 4
        CALL HDMES3 (IOPT,BLNK12)
        OMCNT= OMCNT+ 1
        KTYP= 100
        CALL GETSE (KTYP,OMCNT,
     O              KEYST,KEYND)
C
        IF (KEYST .EQ. 0) THEN
C         we are done
          OPTNAM= ' '
          IOPT = 3
          CALL HDMES3 (IOPT,OPTNAM)
        ELSE
          IOPT = 2
          CALL HDMES2 (IOPT,KTYP,OMCNT)
C         block for what operation-type was supplied?
          CALL GETKNM (KTYP,OMCNT,
     O                 OPTNAM)
C         WRITE(*,*) 'HSPF:OPNBLK:proc',OPTNAM
          OPTYP= OPTNAM(1:8)
          OMCODE= CHKSTR(LKWOPR,NKWOPR,OPTNM1,KWDOPR)
C         get keyword info about tables
          LCLU= OMCODE+ 120
          LTABT= 3
          SGRP= 1
          INITFG= 1
          NTABT= 1
          TABBAS= OMCODE*1000
C
 20       CONTINUE
            CLEN(1)= 80
            CALL WMSGTT (MSGFL,LCLU,SGRP,INITFG,
     M                   CLEN,
     O                   CHSTR1,CONT)
            READ (CHSTR,1020) TABDIM(NTABT),TABNAM(NTABT)
            TABXXX(NTABT)= TABBAS+ NTABT
            NTABT= NTABT+ 1
            INITFG= 0
          IF (CONT .EQ. 1) GO TO 20
C
          IF (OUTLEV .GT. 0) THEN
C           doing this type
            CNUM= 1
            CLEN(1)= 8
            SGRP= 41
            CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,CNUM,CLEN,OPTNM1)
            IF (OUTLEV .GT. 5) THEN
C             dump user's control input
              CALL DUMPER (KEYST,KEYND,MESSU)
            END IF
          END IF
C
C         find and validate the keys to each table of information
C         contained in this operation-type block
C
          CALL KEYUCI (NTABT,I12,I2,KTYP,OMCNT,
     I                 TABNM1,TABDIM,TABXXX,
     M                 ECOUNT,
     O                 KCNT)
          KYST(1)= 1
          IF (NTABT .GT. 1) THEN
            DO 30 J= 2, NTABT
              KYST(J)= KYST(J- 1)+ TABDIM(J- 1)
  30         CONTINUE
          END IF
C
          CALL ZIPI (MAXTBL,I0,
     O               TABKST)
          CALL ZIPI (MAXTBL,I0,
     O               TABKND)
C
C         loop thru all tables
          I= 0
  40      CONTINUE
C           loop thru all occurances
            I= I+ 1
            K= 0
  50        CONTINUE
              K= K+ 1
              CALL GETSE (TABXXX(I),K,
     O                    TKEYST,TKEYND)
              IF (TKEYST .GT. 0) THEN
C               save it
                J= KYST(I)+ K- 1
                TABKST(J)= TKEYST
                TABKND(J)= TKEYND
              END IF
            IF (TKEYST .GT. 0) GO TO 50
          IF (I .LT. NTABT) GO TO 40
C
          LTABTS= LTABT
          NTABTS= NTABT
C
C         find the first operation of this type, in opntab
          OPNO= OPNNO (OPTYP,I0,IX,MAXOPN,OPNTAB,I1,NOPNS)
C
C         whiledo opno> 0
 60       CONTINUE
            IF (OPNO .NE. 0) THEN
C             process the input to an operation of this type
              GRP= OPNTAB(6,OPNO)
              NDELT= GRPTAB(3,GRP)
              OPTNO= OPNTAB(3,OPNO)
C             put operation number to screen
              IOPT = 3
              CALL HDMESN (IOPT,OPTNO)
C
              IF (OMCODE .EQ. 1) THEN
C               perlnd module
                CALL PPERLN (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 2) THEN
C               implnd module
                CALL PIMPLN (OUTLEV,MESSU,MSGFL,RESMFG,
     I                       NDELT,SDATIM,NDAMON,OPNO,EMFG,MAXOPN,
     I                       MAXOSV,
     M                       OSVKEY,OPNTAB,ECOUNT)
C
              ELSE IF (OMCODE .EQ. 3) THEN
C               rchres module
                CALL PRCHRE (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 4) THEN
C               copy module
                CALL PCOPY (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                      OSVKEY)
C
              ELSE IF (OMCODE .EQ. 5) THEN
C               pltgen module
                 CALL PPLTGN (NDELT,SDATIM,EDATIM,NDAMON,EMFG,MAXOSV,
     M                        OSVKEY)
C
              ELSE IF (OMCODE .EQ. 6) THEN
C               disply module
                CALL PDISPL (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 7) THEN
C               duranl module
                CALL PDURAN (NDELT,SDATIM,EDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 8) THEN
C               gener module
                CALL PGENER (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 9) THEN
C               mutsin module
                CALL PMUTSN (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 10) THEN
C               bmprac module
                CALL PBMPRC (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 11) THEN
C               report module
                CALL PREPRT (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              END IF
C
C             find the next operation of this type
              START= OPNO+ 1
              OPNO= OPNNO (OPTYP,I0,IX,MAXOPN,OPNTAB,START,NOPNS)
            END IF
          IF (OPNO.GT.0 .AND. RETCOD.EQ.0) GO TO 60
C         end whiledo
C
          IF (OUTLEV .GT. 0) THEN
C           done this type
            CNUM= 1
            CLEN(1)= 8
            SGRP= 42
            CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,CNUM,CLEN,OPTNM1)
          END IF
        END IF
      IF (OPTNAM.NE.' ' .AND. RETCOD.EQ.0) GO TO 10
C
C     set run width equal to global reach irrigation withdrawal flag
      CALL PIRGTF
     O            (RUNWID)
      IF (RUNWID .EQ. 1) THEN
C       reach irrigation withdrawals needed - initialize keys and addresses
        CALL PPIRRG (MSGFL,NOPNS,OPNTAB)
      END IF
C
      RETURN
      END
