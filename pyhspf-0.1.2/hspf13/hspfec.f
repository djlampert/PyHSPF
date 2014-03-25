C
C
C
      SUBROUTINE   TIMSER
     I                    (SDATIM,EDATIM,MAXMLK,MSLINX,NMLTBS,RUNWID,
     I                     IHMFG)
C
C     + + + PURPOSE + + +
C     Process the blocks which deal with time series
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    EDATIM(5),SDATIM(5),MAXMLK,MSLINX(MAXMLK,3),NMLTBS,
     $           RUNWID,IHMFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     MAXMLK - ???
C     MSLINX - ???
C     NMLTBS - ???
C     RUNWID - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE 'crin3.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   FMTKND,FMTKST,GPKEY,GRPND,GRPST,
     $          I1,I2,I3,I4,I5,IDUM1,IDUM2,ITYP,J,MSGFL,
     $          MESSU,NETKND,NETKST,OPND,OPST,
     $          OPTNO,OPTYPI(2),PGPKEY,SRCKND,SRCKST,
     $          TARKND,TARKST,TSSFL,WDMSFL(4),
     $          WKEY,WKND,WKNDES,WKNDNT,WKND1,WKSTX,WKNDX,
     $          WKND2,WKST,WKSTES,WKSTNT,WKST1,WKST2,XGRP,
     $          SCHKST,SCHKND,MXKY,I0,DREC(1),IOPT
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKTSS,CHKWDM,GPFINT,DUMPER,CHKDSS,HDMES2
      EXTERNAL  WORKIO,SRCBLK,SCHBLK,NETBLK,CHAIN,WKDMP1
      EXTERNAL  TARBLK,SAME,ALLOC,OSVFPT,WKDMP2,TINSTR,HRITSI,GETSE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('='),
     $        /,' PROCESSING BLOCKS CONTAINING TIME SERIES LINKAGES')
 2010 FORMAT (/,' PROCESSING ENTRIES FOR EXGROUP NO.',I4)
 2020 FORMAT (/,' SITUATION AFTER OCCURRENCES OF IDENTICAL SOURCES ',
     $          'AND TARGETS HAVE BEEN CHAINED:')
 2030 FORMAT (/,' SITUATION AFTER ALLOCATION OF INPAD ROWS:')
 2040 FORMAT (/,' DUMPING PRIMITIVE TSGET/TSPUT INSTRUCTIONS')
 2050 FORMAT (/,' OPNTAB for ',2A4,I4)
 2055 FORMAT ('  OSVF',I4,' L',I4,
     $         ' TSGF',I4,' L',I4,
     $         ' TSPF',I4,' L',I4)
 2060 FORMAT (/,' TSGET INSTRUCTIONS')
 2070 FORMAT (/,' TSPUT INSTRUCTIONS')
 2080 FORMAT (/,' FINISHED PROCESSING ENTRIES FOR EXGROUP NO.',I4,
     $          '                MAXIMUM WRKSPA RECORD = ',I5)
 2090 FORMAT (/,' FINISHED PROCESSING BLOCKS CONTAINING TIME ',
     $          'SERIES LINKAGES',
     $        /,' ',132('='))
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
C
      MESSU = FILE(1)
      TSSFL = FILE(10)
      DO 5 J= 1, 4
        WDMSFL(J)= FILE(10+J)
 5    CONTINUE
      MSGFL = FILE(15)
C
      IF (OUTLEV .GT. 0) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize keyworks, etc for timeseries
      CALL HRITSI (MSGFL)
C
      IF (TSSFL .GT. 0) THEN
C       tssfl is being used in this run, look at tss descriptor
        CALL CHKTSS (TSSFL,MESSU,MSGFL,
     M               ECOUNT,
     O               TDFREC,TDDS,TOTDS,RECLT,TDSIZE)
      END IF
C
      DO 10 J= 1, 4
        IF (WDMSFL(J) .GT. 0) THEN
C         wdmsfl is being used in this run, initialize wdms file
          CALL CHKWDM (WDMSFL(J),MESSU,MSGFL,
     M                 ECOUNT)
        END IF
 10   CONTINUE
C
C     check open dss files
      CALL CHKDSS (MESSU,MSGFL,
     M             ECOUNT)
C
C     initialize the tsget/tsput files
      CALL GPFINT(RECLT,FILE,GPKEY)
      PGPKEY= GPKEY
C
C     get keys to users ext sour block
      ITYP= 5
      CALL GETSE(ITYP,I1,
     O           SRCKST,SRCKND)
      IF (SRCKST .GT. 0 .AND. OUTLEV .GT. 3) THEN
C       echo user's control input
        CALL DUMPER (SRCKST,SRCKND,MESSU)
      END IF
C
C     get keys to users formats block
      ITYP= 6
      CALL GETSE(ITYP,I1,
     O           FMTKST,FMTKND)
      IF (FMTKST .GT. 0 .AND. OUTLEV .GT. 3) THEN
C       echo user's control input
        CALL DUMPER (FMTKST,FMTKND,MESSU)
      END IF
C
C     get keys to users schematic block
      ITYP= 10
      CALL GETSE(ITYP,I1,
     O           SCHKST,SCHKND)
      IF (SCHKST .GT. 0 .AND. OUTLEV .GT. 3) THEN
C       echo user's control input
        CALL DUMPER (SCHKST,SCHKND,MESSU)
      END IF
C
C     get keys to users network block
      ITYP= 7
      CALL GETSE(ITYP,I1,
     O           NETKST,NETKND)
      IF (NETKST .GT. 0 .AND. OUTLEV .GT. 3) THEN
C       echo user's control input
        CALL DUMPER (NETKST,NETKND,MESSU)
      END IF
C
C     get keys to users external targets block
      ITYP= 8
      CALL GETSE(ITYP,I1,
     O           TARKST,TARKND)
      IF (TARKST .GT. 0 .AND. OUTLEV .GT. 3) THEN
C       echo user's control input
        CALL DUMPER (TARKST,TARKND,MESSU)
      END IF
C
C     exgroup loop
      DO 100 XGRP= 1,NXGRPS
C       find the starting and ending operations
        GRPST= EXGTAB(1,XGRP)
        OPST = GRPTAB(1,GRPST)
        GRPND= EXGTAB(2,XGRP)
        OPND = GRPTAB(2,GRPND)
C
C       call workio to initialize max key
        MXKY= -1
        DREC(1)= 0
        CALL WORKIO (I0,I1,I1,
     M               DREC,MXKY)
C
        IF (OUTLEV .GT. 3) THEN
C         processing xgrp message
          WRITE (MESSU,2010) XGRP
        END IF
C
C       initialize keys to entries which may be written to workfl
        WKEY= 0
        DO 60 OPNO= OPST,OPND
          DO 50 J= 9,20
            OPNTAB(J,OPNO)= 0
 50       CONTINUE
 60     CONTINUE
C
        WKSTES= 0
        WKNDES= 0
        WKSTNT= 0
        WKNDNT= 0
C
        IF (SRCKST .GT. 0) THEN
C         ext sources block is present - process it
          IOPT= 2
          ITYP= 5
          CALL HDMES2(IOPT,ITYP,I1)
          CALL SRCBLK (OPST,OPND,SRCKST,SRCKND,SDATIM,EDATIM,
     I                 FMTKST,FMTKND,IHMFG,
     M                 WKEY,WKSTES,WKNDES)
        END IF
C
        IF (SCHKST .GT. 0 .OR. NETKST .GT. 0) THEN
C         schematic block or network block is present
          WKSTX = WKEY + 1
C
          IF (SCHKST .GT. 0) THEN
C           schematics block is present - process it
            IOPT= 2
            ITYP= 10
            CALL HDMES2(IOPT,ITYP,I1)
            CALL SCHBLK
     I                  (OPST,OPND,SCHKST,SCHKND,MAXMLK,
     I                   MSLINX,NMLTBS,
     M                   WKEY)
          END IF
C
          IF (NETKST .GT. 0) THEN
C           network block is present - process it
            IOPT= 2
            ITYP= 7
            CALL HDMES2(IOPT,ITYP,I1)
            CALL NETBLK (OPST,OPND,NETKST,NETKND,
     M                   WKEY)
          END IF
C
C         the following chaining has been moved up to timser from netblk
C         because this process needs to be performed for all entries
C         originating from both schematic and network blocks
          IF (WKEY .GE. WKSTX) THEN
C           network/schematic entries were written to workfl
            WKNDX= WKEY
C
C           chain them in source opn sequence order and keep workfl
C           keys in opntab
            I1= 3
            I2= 15
            I3= 11
            I4= 12
            CALL CHAIN (OPST,OPND,WKSTX,WKNDX,I1,I2,I3,I4,MAXOPN,
     M                  OPNTAB,
     O                  IDUM1,IDUM2)
C
C           chain in target opn sequence order
            I1= 29
            I2= 41
            I3= 13
            I4= 14
            CALL CHAIN (OPST,OPND,WKSTX,WKNDX,I1,I2,I3,I4,MAXOPN,
     M                  OPNTAB,
     O                  WKSTNT,WKNDNT)
C
            IF (OUTLEV .GT. 5) THEN
C             dump records written to workfl
              CALL WKDMP1 (WKSTX,WKNDX,MESSU)
            END IF
          END IF
        END IF
C
        IF (TARKST .GT. 0) THEN
C         ext targets block is present - process it
          IOPT= 2
          ITYP= 8
          I1  = 1
          CALL HDMES2(IOPT,ITYP,I1)
          CALL TARBLK (OPST,OPND,TARKST,TARKND,SDATIM,EDATIM,
     I                 FMTKST,FMTKND,IHMFG,
     M                 WKEY)
        END IF
C
C       chain all occurrences of the same source/stkind/sttran
C       combination, in ext sources block, considering entries in
C       target opn sequence order
        WKST1= WKSTES
        WKND1= WKNDES
C
        WKST2= 0
        WKND2= 0
C
        I1= 1
        I2= 41
        I3= 16
        I4= 3
        I5= 11
        CALL SAME (I1,I2,I3,I4,I5,WKST1,WKND1,WKST2,WKND2,
     I             MESSU,MSGFL,
     M             ECOUNT)
C
C       chain all occurrences of the same source in the network
C       block, considering entries in target opn sequence order
        WKST1= WKSTNT
        WKND1= WKNDNT
C
        WKST2= 0
        WKND2= 0
C
        I1= 0
        I2= 41
        I3= 16
        I4= 3
        I5= 11
        CALL SAME (I1,I2,I3,I4,I5,WKST1,WKND1,WKST2,WKND2,
     I             MESSU,MSGFL,
     M             ECOUNT)
C
        DO 70 OPNO= OPST,OPND
C         chain all occurrences of the same source in ext targets
C         and network blocks, considering entries in source opn'
C         sequence order
          WKST1= OPNTAB(15,OPNO)
          WKND1= OPNTAB(16,OPNO)
C
          WKST2= OPNTAB(11,OPNO)
          WKND2= OPNTAB(12,OPNO)
C
          I1= 0
          I2= 15
          I3= 17
          I4= 3
          I5= 11
          CALL SAME (I1,I2,I3,I4,I5,WKST1,WKND1,WKST2,WKND2,
     I               MESSU,MSGFL,
     M               ECOUNT)
C
C         chain all occurrences of the same internal target in
C         network and ext sources blocks, considering entries in
C         target opn sequence order
          WKST1= OPNTAB(13,OPNO)
          WKND1= OPNTAB(14,OPNO)
C
          WKST2= OPNTAB(9,OPNO)
          WKND2= OPNTAB(10,OPNO)
C
          I1= -1
          I2= 41
          I3= 42
          I4= 29
          I5= 37
          CALL SAME (I1,I2,I3,I4,I5,WKST1,WKND1,WKST2,WKND2,
     I               MESSU,MSGFL,
     M               ECOUNT)
C
 70     CONTINUE
C
        IF (OUTLEV .GT. 5) THEN
C         dump contents of workfl
          WRITE (MESSU,2020)
          I1= 1
          CALL WKDMP1 (I1,WKEY,MESSU)
        END IF
C
C       allocate rows in the inpad and generate primitive
C       tsget/tsput instructions
        CALL ALLOC (PGPKEY,GRPST,GRPND,RUNWID)
C
        IF (OUTLEV .GT. 5) THEN
C         dump workfl
          WRITE (MESSU,2030)
          I1=1
          CALL WKDMP1 (I1,WKEY,MESSU)
        END IF
C
C       assign values to the flag pointers in each osv for this
C       exgroup
        CALL OSVFPT (GRPST,GRPND)
C
        IF (OUTLEV .GT. 5) THEN
C         dump primitive tsget and tsput instructions
          WRITE (MESSU,2040)
          DO 90 OPNO= OPST,OPND
            DO 80 J= 1,2
              OPTYPI(J)= OPNTAB(J,OPNO)
 80         CONTINUE
            OPTNO= OPNTAB(3,OPNO)
            WRITE (MESSU,2050)  OPTYPI, OPTNO
C
C           opntab for this operation
            WRITE (MESSU,2055) OPNTAB(7,OPNO),OPNTAB(8,OPNO),
     $                         (OPNTAB(J,OPNO),J=17,20)
C
C           tsget instructions
            WRITE (MESSU,2060)
            WKST= OPNTAB(17,OPNO)
            WKND= OPNTAB(18,OPNO)
            IF (WKST .GT. 0) THEN
C             dump instructions
              CALL WKDMP2 (WKST,WKND,MESSU)
            END IF
C
C           tsput instructions
            WRITE (MESSU,2070)
            WKST= OPNTAB(19,OPNO)
            WKND= OPNTAB(20,OPNO)
            IF (WKST .GT. 0) THEN
C             dump instructions
              CALL WKDMP2 (WKST,WKND,MESSU)
            END IF
 90       CONTINUE
        END IF
C
C       generate final tsget/tsput instructions from primitive
C       instructions
        CALL TINSTR(SDATIM,EDATIM,OPST,OPND,GPKEY)
C
C       call workio to get max key
        I0  = 0
        I1  = 1
        MXKY= 0
        DREC(1)= 0
        CALL WORKIO (I0,I1,I1,
     M               DREC,MXKY)
C
        IF (OUTLEV .GT. 3) THEN
C         end processing xgrp message
          WRITE (MESSU,2080)  XGRP,MXKY
        END IF
 100  CONTINUE
C
      IF (OUTLEV .GT. 0) THEN
C       end processing timeseries message
        WRITE (MESSU,2090)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTTMRC
     I                   (TSBKCD,KEYND,MESSU,MSGFL,SCLU,BGRP,
     M                    KEY,ECOUNT,
     O                    SVOL,SVOLNO,SGRPN,SMEMN,SMEMSB,SSYST,SGAPST,
     O                    MFACTR,TRAN,TVOL,TVOLNO,TOPFST,TOPLST,TGRPN,
     O                    TMEMN,TMEMSB,TSYST,AGG,AMDST,GTCOD)
C
C     + + + PURPOSE + + +
C     Get an entry from time series blocks data structure.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TSBKCD,KEYND,MESSU,MSGFL,SCLU,BGRP,KEY,ECOUNT,SVOLNO,
     $            SMEMSB(2),TVOLNO,TOPFST,TOPLST,TMEMSB(2),GTCOD
      REAL        MFACTR
      CHARACTER*4 SSYST,SGAPST,TRAN,TSYST,AGG,AMDST
      CHARACTER*6 SVOL,SGRPN,SMEMN,TVOL,TGRPN,TMEMN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TSBKCD - time-series type code,
C              1 - Ext Sources
C              2 - Network
C              3 - Ext Targets
C              4 - Schematic
C     KEYND  - UCI key of last line in time-series block
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of message file
C     SCLU   -
C     BGRP   -
C     KEY    - UCI key of current line in time-series block
C     ECOUNT -
C     SVOL   - source volume
C     SVOLNO - source volume number
C     SGRPN  - 
C     SMEMN  - 
C     SMEMSB - ???
C     SSYST  - ???
C     SGAPST - ???
C     MFACTR - ???
C     TRAN   - ???
C     TVOL   - ???
C     TVOLNO - ???
C     TOPFST - ???
C     TOPLST - ???
C     TGRPN  - ???
C     TMEMN  - ???
C     TMEMSB - ???
C     TSYST  - ???
C     AGG    - ???
C     AMDST  - ???
C     GTCOD  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I80,TCLU,SGRP
      CHARACTER*2  CSMSUB(2),CTMSUB(2)
      CHARACTER*10 CMFACT
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UCIBF,UCIBF1)
      CHARACTER*1  UCIBF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER   CHRDIG
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETUCI,CHRDIG,TAGVAL,RDMFAC,OMSG,OMSTC
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (A6,I4,1X,A6,A2,1X,2A4,A10,A4,1X,A6,2(1X,I3),
     $        2(1X,A6),2A2)
 1020 FORMAT (A6,I4,2(1X,A6),2A2,A10,A4,1X,A6,2(1X,I3),
     $        2(1X,A6),2A2)
 1030 FORMAT (A6,I4,2(1X,A6),2A2,A10,A4,1X,A6,I4,1X,
     $        A6,A2,3(1X,A4))
 1040 FORMAT (A6,I4,18X,A10,5X,A6,I4,3X,I4,11X,2A2)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I80= 80
C
      TCLU= 202
C
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
C
      IF (KEY .NE. KEYND) THEN
C       not done yet
        IF (TSBKCD .EQ. 1) THEN
C         external sources
          READ (UCIBF,1010,ERR=10)  SVOL, SVOLNO, SMEMN, CSMSUB(1),
     $              SSYST, SGAPST, CMFACT, TRAN, TVOL, TOPFST,
     $              TOPLST, TGRPN, TMEMN, CTMSUB
            GO TO 15
 10       CONTINUE
C           error - cannot read ext sources line
            CALL OMSTC (I80,UCIBF1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,TCLU,SGRP,
     M                 ECOUNT)
 15       CONTINUE
          CALL RDMFAC (CMFACT,MESSU,MSGFL,TCLU,
     M                 ECOUNT,
     O                 MFACTR)
          CALL TAGVAL (CSMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 SMEMSB(1))
          CALL TAGVAL (CTMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(1))
          CALL TAGVAL (CTMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(2))
          IF (SVOL(1:3) .EQ. 'WDM') THEN
C           do extra processing for wdm files
            IF (UCIBF(18:19) .EQ. '  ') THEN
C             quality flag defaults to 31
              SMEMSB(1)= 31
            END IF
            SMEMSB(2)= CHRDIG (SVOL(4:4))
            IF ( (SMEMSB(2) .LT. 1) .OR. (SMEMSB(2) .GT. 4) ) THEN
C             blank or invalid
              SMEMSB(2)= 1
              SVOL(4:4)= '1'
            END IF
          ELSE
C           dummy second subscript for non-wdm
            SMEMSB(2) = -999
          END IF
          GTCOD= 1
        ELSE IF (TSBKCD .EQ. 2) THEN
C         network
          READ (UCIBF,1020,ERR=20)  SVOL, SVOLNO, SGRPN, SMEMN,
     $          CSMSUB, CMFACT, TRAN, TVOL, TOPFST, TOPLST,
     $          TGRPN, TMEMN, CTMSUB
            GO TO 25
 20       CONTINUE
C           error - cannot read network line
            CALL OMSTC (I80,UCIBF1)
            SGRP= 22
            CALL OMSG (MESSU,MSGFL,TCLU,SGRP,
     M                 ECOUNT)
 25       CONTINUE
          CALL RDMFAC (CMFACT,MESSU,MSGFL,TCLU,
     M                 ECOUNT,
     O                 MFACTR)
          CALL TAGVAL (CSMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 SMEMSB(1))
          CALL TAGVAL (CSMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 SMEMSB(2))
          CALL TAGVAL (CTMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(1))
          CALL TAGVAL (CTMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(2))
          GTCOD= 2
        ELSE IF (TSBKCD .EQ. 3) THEN
C         external targets
          READ (UCIBF,1030,ERR=30)  SVOL, SVOLNO, SGRPN, SMEMN,
     $          CSMSUB, CMFACT, TRAN, TVOL, TVOLNO, TMEMN,
     $          CTMSUB(1), TSYST, AGG, AMDST
            GO TO 35
 30       CONTINUE
C           error - cannot read ext targets line
            CALL OMSTC (I80,UCIBF1)
            SGRP= 23
            CALL OMSG (MESSU,MSGFL,TCLU,SGRP,
     M                 ECOUNT)
 35       CONTINUE
          CALL RDMFAC (CMFACT,MESSU,MSGFL,TCLU,
     M                 ECOUNT,
     O                 MFACTR)
          CALL TAGVAL (CSMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 SMEMSB(1))
          CALL TAGVAL (CSMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 SMEMSB(2))
          CALL TAGVAL (CTMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(1))
          IF (TVOL(1:3) .EQ. 'WDM') THEN
C           do extra processing for wdm files
            TMEMSB(2)= CHRDIG (TVOL(4:4))
            IF ( (TMEMSB(2) .LT. 1) .OR. (TMEMSB(2) .GT. 4) ) THEN
C             blank or invalid
              TMEMSB(2)= 1
              TVOL(4:4)= '1'
            END IF
          ELSE
C           dummy second subscript for non-wdm
            TMEMSB(2) = -999
          END IF
          GTCOD= 3
        ELSE IF (TSBKCD .EQ. 4) THEN
C         schematic
          READ (UCIBF,1040,ERR=40) SVOL,SVOLNO,CMFACT,TVOL,TVOLNO,
     $          TOPFST,CTMSUB
            GO TO 45
 40       CONTINUE
C           error - cannot read ext sources line
            CALL OMSTC (I80,UCIBF1)
            SGRP= 24
            CALL OMSG (MESSU,MSGFL,TCLU,SGRP,
     M                 ECOUNT)
 45       CONTINUE
          CALL RDMFAC (CMFACT,MESSU,MSGFL,TCLU,
     M                 ECOUNT,
     O                 MFACTR)
          CALL TAGVAL (CTMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(1))
          CALL TAGVAL (CTMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 TMEMSB(2))
          GTCOD= 4
        END IF
      ELSE
C       at end of block
        GTCOD= -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSCHEM
     I                   (MSGFL,MESSU,MAXOPN,OPNTAB,OPST,OPND,SVOL,
     I                    SVOLNO,TVOL,TVOLNO,AREA,MAXMLK,MSLINX,NMLTBS,
     I                    MSLKTB,MAXCNX,
     M                    ECOUNT,
     O                    SGPNAM,SMMNAM,SMMSUB,MFCT,TRANSF,
     O                    TGPNAM,TMMNAM,TMMSUB,NUMCON)
C
C     + + + PURPOSE + + +
C     Expand an entry in the schematic block by using either default
C     time series connections from the message file or
C     user-defined connections from a mass link block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MAXOPN,MAXMLK,MAXCNX,MSGFL,MESSU,OPNTAB(20,MAXOPN),
     #            OPST,OPND,SVOLNO,TVOLNO,ECOUNT,
     #            SMMSUB(2,MAXCNX),
     #            TMMSUB(2,MAXCNX),NUMCON,MSLINX(MAXMLK,3),NMLTBS,MSLKTB
      REAL        MFCT(MAXCNX),AREA
      CHARACTER*6 SVOL,SGPNAM(MAXCNX),SMMNAM(MAXCNX),
     #            TVOL,TGPNAM(MAXCNX),TMMNAM(MAXCNX)
      CHARACTER*4 TRANSF(MAXCNX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     MAXOPN - ???
C     OPNTAB - ???
C     OPST   - ???
C     OPND   - ???
C     SVOL   - ???
C     SVOLNO - ???
C     TVOL   - ???
C     TVOLNO - ???
C     AREA   - ???
C     MAXMLK - ???
C     MSLINX - ???
C     NMLTBS - ???
C     MSLKTB - ???
C     MAXCNX - ???
C     ECOUNT - count(s) of specific errors
C     SGPNAM - ???
C     SMMNAM - ???
C     SMMSUB - ???
C     MFCT   - ???
C     TRANSF - ???
C     TGPNAM - ???
C     TMMNAM - ???
C     TMMSUB - ???
C     NUMCON - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,KEY,ERR,STTYP,SNUM,TNUM,I0,I6,
     $             CNNXN,SOPCOD,TOPCOD,MSLKFG,
     $             KEYND,SCLU,SGRP,BGRP,WCOUNT
      REAL         CONV
      CHARACTER*2  CSMSUB(2),CTMSUB(2)
      CHARACTER*6  CHSTR,TSVOL,TTVOL
      CHARACTER*10 CCONV
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(UCIBF,UCIBF1)
      CHARACTER*1  CHSTR1(6),UCIBF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER   OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL  OPNNO,OMSG,OMSTI,OMSTC,HRIMSI,GETUCI,TAGVAL,RDMFAC
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (11X,A6,1X,A6,2A2,A10,20X,A6,1X,A6,2A2)
 1010 FORMAT (A6,37X,A6,26X)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 202
      BGRP= 10
      I0= 0
      I6= 6
      MSLKFG= 0
      IF (NMLTBS .GT. 0) THEN
        DO 10 I= 1, NMLTBS
          IF (MSLINX(I,1) .EQ. MSLKTB) THEN
            MSLKFG = I
          END IF
 10     CONTINUE
      END IF
C
      NUMCON= 0
      IF (MSLKFG .GE. 1) THEN
C       schematic entry refers to table in mass-link block, process it
        KEY= MSLINX(MSLKFG,2)
        KEYND= MSLINX(MSLKFG,3)
C       read entries in table and process
        CNNXN= 0
 20     CONTINUE
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IF (KEY .NE. KEYND) THEN
C           process entry
            CNNXN= CNNXN+ 1
            READ (UCIBF,1000,ERR=30) SGPNAM(CNNXN),SMMNAM(CNNXN),
     $                       (CSMSUB(I),I=1,2),CCONV,
     $                        TGPNAM(CNNXN),TMMNAM(CNNXN),
     $                       (CTMSUB(I),I=1,2)
              GO TO 40
 30         CONTINUE
C             error - cannot read mass-link line
              CALL OMSTI (MSLKTB)
              I= 80
              CALL OMSTC (I,UCIBF1)
              SGRP= 25
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
 40         CONTINUE
C             check to see if implict mass-link
              IF (SMMNAM(CNNXN).EQ.'      ' .AND. 
     1            TMMNAM(CNNXN).EQ.'      ') THEN
C               implicit mass-link, make sure source and target vols are the same
                READ (UCIBF,1010,ERR=50) TSVOL,TTVOL
                IF (TSVOL.NE.TTVOL) THEN
C                 report warning -- this can produce unintended results
                  CALL OMSTI (MSLKTB)
                  SGRP= 26
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       WCOUNT)
                END IF
              END IF
 50         CONTINUE
            CALL TAGVAL (CSMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   SMMSUB(1,CNNXN))
            CALL TAGVAL (CSMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   SMMSUB(2,CNNXN))
            CALL TAGVAL (CTMSUB(1),I0,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   TMMSUB(1,CNNXN))
            CALL TAGVAL (CTMSUB(2),I0,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   TMMSUB(2,CNNXN))
C
            CALL RDMFAC (CCONV,MESSU,MSGFL,SCLU,
     M                   ECOUNT,
     O                   CONV)
            MFCT(CNNXN)= AREA * CONV
            TRANSF(CNNXN)= '    '
            NUMCON= CNNXN
          END IF
        IF (KEY .NE. KEYND) GO TO 20
      ELSE
C       no mass-link block present or current schematic entry
C       does not refer to any tables in the block, so will use
C       default table in message file if source-target relation is ok
        SOPCOD = 0
        TOPCOD = 0
        SNUM= OPNNO(SVOL,SVOLNO,SVOLNO,MAXOPN,OPNTAB,OPST,OPND)
        TNUM= OPNNO(TVOL,TVOLNO,TVOLNO,MAXOPN,OPNTAB,OPST,OPND)
        IF (SNUM .GT. 0 .AND. TNUM .GT. 0) THEN
          SOPCOD= OPNTAB(4,SNUM)
          TOPCOD= OPNTAB(4,TNUM)
        END IF
C
        ERR = 0
        IF (SOPCOD .EQ. 1 .AND. TOPCOD .EQ. 3) THEN
C         perlnd to rchres
          STTYP = 1
        ELSE IF (SOPCOD .EQ. 2 .AND. TOPCOD .EQ. 3) THEN
C         implnd to rchres
          STTYP = 2
        ELSE
C         invalid entry
          ERR   = 1
        END IF
C
        IF (ERR .EQ. 0) THEN
C         generate default connections implied by this schematic entry
C         from message file
          CALL HRIMSI (MSGFL,AREA,STTYP,MAXCNX,
     O                 SGPNAM,SMMNAM,SMMSUB,
     O                 TRANSF,NUMCON,MFCT,
     O                 TGPNAM,TMMNAM,TMMSUB)
        ELSE
C         error - invalid entry in schematic block
          CHSTR= SVOL
          CALL OMSTC (I6,CHSTR1)
          CALL OMSTI (SVOLNO)
          CHSTR= TVOL
          CALL OMSTC (I6,CHSTR1)
          CALL OMSTI (TVOLNO)
          SGRP = 1
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
C
          NUMCON = 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   EXTTS
     I                  (MEMN,MEMSB,SYST,AMDST,GAPST,SDATIM,EDATIM,
     I                   VOL,VOLNO,UKEYST,UKEYND,TRFLAG,MXTSTB,IHMFG,
     O                   NUM,DELT,UNT,GRPN,NTS,AMDCD,FRC,GAPCD,
     O                   TABL,TABLR)
C
C     + + + PURPOSE + + +
C     check and expand a reference to a group of time series or a
C     single time series to be obtained from an external source/target
C     trflag=1 if external target is being processed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MEMSB(2),SDATIM(5),
     #            EDATIM(5),VOLNO,UKEYST,UKEYND,TRFLAG,NUM,DELT,
     #            UNT,NTS,AMDCD,FRC,GAPCD,MXTSTB,IHMFG,TABL(10,MXTSTB)
      REAL        TABLR(10,MXTSTB)
      CHARACTER*6 VOL,MEMN,GRPN
      CHARACTER*4 SYST,AMDST,GAPST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MEMN   - ???
C     MEMSB  - ???
C     SYST   - ???
C     AMDST  - ???
C     GAPST  - ???
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     VOL    - volume name - SEQ,TSS,WDMn,DSS
C     VOLNO  - volume number
C     UKEYST - ???
C     UKEYND - ???
C     TRFLAG - ???
C     MXTSTB - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     NUM    - ???
C     DELT   - simulation time interval in minutes
C     UNT    - ???
C     GRPN   - ???
C     NTS    - ???
C     AMDCD  - ???
C     FRC    - ???
C     GAPCD  - ???
C     TABL   - ???
C     TABLR  - ???
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE   'crin3.inc'
      INCLUDE   'crin3c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     MESSU,TSSFL,WDMSFL(4),I4,N,SCLU,SGRP,MSGFL
      CHARACTER*4 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHKSTR,TSSDS,OMSG,SEQDS,WDMDS,OMSTI,DSSDS
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT(' BEGIN CHECKING/EXPANDING TIME SERIES REFERENCE')
 2020 FORMAT(' END CHECKING/EXPANDING TIME SERIES REFERENCE')
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C
C     + + + END SPECIFICATIONS + + +
C
      I4    = 4
C
      MESSU = FILE(1)
      TSSFL = FILE(10)
      DO 5 N= 1, 4
        WDMSFL(N)= FILE(10+N)
 5    CONTINUE
      MSGFL = FILE(15)
      SCLU  = 202
C
      IF (OUTLEV .GT. 5) THEN
        WRITE(MESSU,2010)
      END IF
C
      GRPN= '      '
C
C     set access mode, frc and gap code to zero to represent null
C     values. tssds, seqds, wdmds, and dssds do not always set
C     meaningful values
      AMDCD= 0
      GAPCD= 0
      FRC  = 0
C
C     search for source/target volume name
      CHSTR(1:3)= VOL(1:3)
      CHSTR1(4)= ' '
      N    = CHKSTR(I4,I4,CHSTR1,EXTKW1)
C
      IF (N .EQ. 1) THEN
C       tss
        IF (TSSFL .NE. 0) THEN
C         tssfl available - process entries normally
          CALL TSSDS (MSGFL,MESSU,TSSFL,MEMN,MEMSB(1),
     I                AMDST,SDATIM,EDATIM,VOLNO,
     I                TRFLAG,MXTSTB,
     O                NUM,DELT,UNT,NTS,AMDCD,FRC,
     O                TABL,TABLR)
        ELSE
C         error - tss file not available, but active reference to tss
C         dataset encountered in user control input
          SGRP= 2
          CALL OMSTI(VOLNO)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (N .EQ. 2) THEN
C       seq
        IF (TRFLAG .EQ. 0) THEN
C         sequential input - process it
          CALL SEQDS (MSGFL,MESSU,MEMN,MEMSB(1),SYST,
     I                GAPST,VOLNO,
     I                UKEYST,UKEYND,MXTSTB,
     O                NUM,DELT,UNT,NTS,GAPCD,
     O                TABL,TABLR)
        ELSE
C         sequential output not allowed
          SGRP= 5
          CALL OMSTI(VOLNO)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (N .EQ. 3) THEN
C       wdm
        IF (WDMSFL(MEMSB(2)) .NE. 0) THEN
C         wdmsfl is available - process entries
          CALL WDMDS (MSGFL,MESSU,WDMSFL(MEMSB(2)),VOLNO,MEMN,MEMSB,
     I                GAPST,GAPKW1,SYST,SYSKW1,AMDKW1,AMDST,SDATIM,
     I                EDATIM,TRFLAG,OUTLEV,MXTSTB,IHMFG,
     M                ECOUNT,
     O                NUM,DELT,UNT,NTS,GAPCD,AMDCD,TABL,TABLR)
        ELSE
C         wdmsfl not available, but active references to wdms
C         datsets encountered in user control input
          SGRP= 4
          CALL OMSTI(VOLNO)
          CALL OMSTI(MEMSB(2))
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (N .EQ. 4) THEN
C       dss
        CALL DSSDS (MSGFL,MESSU,VOLNO,GAPST,GAPKW1,SYST,SYSKW1,AMDKW1,
     I              AMDST,SDATIM,EDATIM,TRFLAG,OUTLEV,MAXTTB,
     M              ECOUNT,
     O              NUM,DELT,UNT,NTS,GAPCD,AMDCD,TABL,TABLR)
      ELSE
C       unknown keyword
        SGRP= 3
        CALL OMSTI(VOLNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (OUTLEV .GT. 5) THEN
        WRITE(MESSU,2020)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RDMFAC
     I                    (CMFACT,MESSU,MSGFL,SCLU,
     M                     ECOUNT,
     O                     MFACTR)
C
C     + + + PURPOSE + + +
C     Look at a supplied character value.  If it is blank, return
C     default, else return supplied real value.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,MSGFL,SCLU,ECOUNT
      REAL         MFACTR
      CHARACTER*10 CMFACT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CMFACT - input multiplication factor as a character string
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of hspf message file
C     SCLU   - 
C     ECOUNT -
C     MFACTR - time series multiplication factor
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phcat.inc'
      INCLUDE 'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,I10,SGRP,VALSET
      CHARACTER*10 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(10)
C
C     + + + FUNCTIONS + + +
      INTEGER  LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL LENSTR,OMSTC,OMSG,GETVEC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I10/0,1,10/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (F10.0)
C
C     + + + END SPECIFICATIONS + + +
C
      CHSTR= CMFACT
C
      IF (LENSTR (I10,CHSTR1) .LE. 0) THEN
C       default multiplication factor
        MFACTR= 1.0
        VALSET= 1
      ELSE IF (INDEX(CHSTR,'~') .GT. 0) THEN
C       process pest supplemental value
        CALL GETVEC (MESSU, MSGFL, I1, I0, I1,
     M               CHSTR, MFACTR, ECOUNT,
     O               VALSET)          
      ELSE 
C       no supplemental value
        VALSET = 0
      END IF        
C
      IF (VALSET .EQ. 0) THEN
C       read uci value
        READ (CMFACT,1000,ERR= 10) MFACTR
        GO TO 20
 10     CONTINUE
C         error - read error in mult factor
          CALL OMSTC (I10,CHSTR1)
          SGRP= 6
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
          MFACTR= 1.0
 20     CONTINUE
      END IF
C
      RETURN
      END
