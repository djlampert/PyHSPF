C
C
C
      SUBROUTINE   HRITSI
     I                   (MSGFL)
C
C     + + + PURPOSE + + +
C     read in general timseries data from message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MSGFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - message file unit number
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE 'crin3.inc'
      INCLUDE 'crin3c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      N,J,CASE,STKIND,SCLU,SGRP,INITFG,CONT,CLEN
      CHARACTER*1  INBUF1(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (INBUF1,INBUFF)
      CHARACTER*80 INBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (8(A4,6X))
 1040 FORMAT (A6,4X,2I10)
 1050 FORMAT (11I5)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 215
C
C     external sources/targets keyword library
      SGRP  = 12
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1010) EXTKWL
C
C     access mode keyword library
      SGRP  = 13
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1010) AMDKWL
C
C     unit system keyword library
      SGRP  = 14
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1010) SYSKWL
C
C     gap code keyword library
      SGRP  = 15
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1010) GAPKWL
C
C     format class keyword library
      SGRP  = 16
      INITFG= 1
      N     = 0
 10   CONTINUE
        CLEN  = 80
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               INBUF1,CONT)
        INITFG= 0
        N     = N+ 1
        READ (INBUFF,1040) FMTKWL(N),(FMTINF(J,N),J=1,2)
      IF (CONT .EQ. 1) GO TO 10
C
C     transformation keyword library
      SGRP  = 17
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1010) TRNKWL
C
C     transformation code table
      SGRP  = 18
      INITFG= 1
 20   CONTINUE
        CLEN  = 80
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               INBUF1,CONT)
        INITFG= 0
        READ(INBUFF,1050) CASE,STKIND,(TRNTAB(J,STKIND,CASE),J=1,9)
      IF (CONT .EQ. 1) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE GPFINT
     I                  (RECLT,FILE,
     O                   GPKEY)
C
C     + + + PURPOSE + + +
C     Initialize the instruction files for tsget/tsput by
C     placing the appropriate error offsets plus other data
C     in the initial record of each instruction file.
C     note: several scalars and EOFFST share storage with INSTR.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RECLT,FILE(15),GPKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RECLT  - record length of the time series store
C     FILE   - array of file unit numbers
C     GPKEY  - key in tsget file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      INSTR(3),K,MSGFL,I3,SCLU,SGRP,INITFG,CONT,CLEN
      CHARACTER*1  INBUF1(80)
C
C     + + + EQIVALENCES + + +
      EQUIVALENCE (INSTR(1),TESTFG),(INSTR(2),RECL),
     #            (INSTR(3),UNDEF)
      INTEGER      TESTFG,RECL
      REAL         UNDEF
      EQUIVALENCE (INBUF1,INBUFF)
      CHARACTER*80 INBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT,PUTTSI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (20X,I10)
C
C     + + + END SPECIFICATIONS + + +
C
      I3    = 3
      MSGFL = FILE(15)
      SCLU  = 215
C     define record length and undefined value.
      RECL  = RECLT
      UNDEF = -1.E30
C
      DO 100 K= 1,2
C       read info for tsget and tsput errors (temp)
        SGRP  = K+ 30
        INITFG= 1
        CLEN  = 80
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               INBUF1,CONT)
        READ(INBUFF,1000) TESTFG
C
        CALL PUTTSI (I3,K,INSTR)
C
 100  CONTINUE
C
C     set key for tsgetf/tsputf
      GPKEY= 2
C
      RETURN
      END
C
C
C
      SUBROUTINE   HRIMSI
     I                   (MSGFL,AREA,STTYP,MAXCNX,
     O                    SGPNAM,SMMNAM,SMMSUB,
     O                    TRANSF,NUMCON,MFCT,
     O                    TGPNAM,TMMNAM,TMMSUB)
C
C     + + + PURPOSE + + +
C     read in mass link default data from information file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,STTYP,MAXCNX,
     #            SMMSUB(2,MAXCNX),
     #            NUMCON,TMMSUB(2,MAXCNX)
      REAL        AREA,MFCT(MAXCNX)
      CHARACTER*6 SGPNAM(MAXCNX),SMMNAM(MAXCNX),
     #            TGPNAM(MAXCNX),TMMNAM(MAXCNX)
      CHARACTER*4 TRANSF(MAXCNX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - information file
C     AREA   - ???
C     STTYP  - source to target type, 1-per to rch, 2-imp to rch
C     SGPNAM - ???
C     SMNNAM - ???
C     SMNSUB - ???
C     TRANSF - ???
C     NUMCON - ???
C     MFACT  - ???
C     TGPNAM - ???
C     TMNNAM - ???
C     TMNSUB - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SCLU,SGRP,CONT,INITFG,CLEN
      REAL         CONV
      CHARACTER*1  INBUF1(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (INBUF1,INBUFF)
      CHARACTER*80 INBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A6,2X,A6,2I2,4X,F10.0,4X,A6,2X,A6,2I2,26X,I4)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 215
      SGRP= STTYP+ 40
C
      INITFG= 1
      NUMCON= 0
 10   CONTINUE
        CLEN  = 80
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               INBUF1,CONT)
        INITFG= 0
        NUMCON= NUMCON+ 1
        READ (INBUFF,1000) SGPNAM(NUMCON),SMMNAM(NUMCON),
     $                    (SMMSUB(I,NUMCON),I=1,2),CONV,
     $                     TGPNAM(NUMCON),TMMNAM(NUMCON),
     $                    (TMMSUB(I,NUMCON),I=1,2)
C
        MFCT(NUMCON)  = AREA * CONV
        TRANSF(NUMCON)= '    '
      IF (CONT.EQ.1 .AND. NUMCON.LT.MAXCNX) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   FINSTR
     I                   (SKY,EKY,GETF,VOPAD,
     I                    WIDTH,START,ENDR,STIME,FMIN,DELT,
     M                    INKEY)
C
C     + + + PURPOSE + + +
C     Finish the tsget/tsput instruction
C
C     + + + HISTORY + + +
C     12/6/2004 - jlk&pbd - changed I200 to I220 for new
C       TSGET/TSPUT file length, updated to use enhanced 
C       primitive instruction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SKY,EKY,GETF,VOPAD,WIDTH,START(5),
     $          ENDR(5),STIME,FMIN,DELT,INKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SKY    - ???
C     EKY    - ???
C     GETF   - get(1)/put(0) flag
C     VOPAD  - ???
C     WIDTH  - inpad width
C     START  - ???
C     ENDR   - ???
C     STIME  - ???
C     FMIN   - ???
C     DELT   - simulation time interval in minutes
C     INKEY  - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
      INCLUDE   'cpthnm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FIFLG,I,J,KEY,NTS,TREC,VECA(35),VECB(35),I2,SCLU,
     #             SGRP,CONT,INITFG,CLEN,I35,I220,I0,BLNK,
     #             VECT(35),TKEY,IMATCH
      REAL         INITPV(8)
      CHARACTER*1  INBUF1(80)
      CHARACTER*80 UCIBF
      CHARACTER*4  CBLNK
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (VECA(4),ROW),(VECA(9),AT),(VECA(10),BT),
     $            (VECA(15),FRC),(VECA(25),AORIGT)
      INTEGER      ROW,FRC
      REAL         AT,BT,AORIGT
      EQUIVALENCE (INBUF1,INBUFF)
      CHARACTER*80 INBUFF
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    OMSG,RBUFF,GINIT,PINIT,TIMHTW,TIMBAK,TINOUT,WMSGTT,
     #            OMSTI,GETTSI,PUTTSI,GETUCI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        INITPV/-999.,-999.,0.0,0.0,-1.0E30,1.0E30,-999.,0.0/
      DATA        CBLNK/'    '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(4X,19A4)
 1010 FORMAT(19A4)
 1020 FORMAT(A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' BEGIN FINISHING TSGET/TSPUT INSTRUCTION')
 2020 FORMAT(' END FINISHING TSGET/TSPUT INSTRUCTION')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 215
      I0   = 0
      I2   = 2
      I35  = 35
      I220 = 220
C
      READ(CBLNK,'(A4)') BLNK
C
      IF (OUTLEV .GT. 5) THEN
C       finishing message
        WRITE(MESSU,2000)
      END IF
C
      IF (SKY .NE. 0) THEN
C       something to finish
        CALL GETTSI (I35,SKY,
     O               VECA)
        NTS    = 0
        LTRNFG = 0
        KEY    = SKY
C       dountil key > eky
 10     CONTINUE
C         process the component
          NTS         = NTS+ 1
          VOPADR(NTS) = VOPAD+ (ROW-1)*(WIDTH+1)
          OFFSET(NTS) = VECA(5)
          STKIND(NTS) = VECA(6)
          STTRAN(NTS) = VECA(7)
          INMODE(NTS) = VECA(8)
          A(NTS)      = AT
          B(NTS)      = BT
C         if (at .ne. 1.0 .or. bt .ne. 0.0) then
          IF ((ABS(AT-1.0)) .GT. 1.0E-5 .OR. (ABS(BT)) .GT. 0.0) THEN
            LTRNFG = 1
          END IF
C
          KEY   = KEY + 1
          FIFLG = 0
          IF (KEY .GT. EKY) THEN
C           done with this operation - therefore finish instruction
            FIFLG = 1
          ELSE
C           get next primitive instruction
            CALL GETTSI (I35,KEY,
     O                   VECB)
            IF (NTS .EQ. 20) THEN
C             instruction is filled - set finish flag
              FIFLG= 1
            ELSE
              IF (VECA(1) .NE. 4 .OR. VECB(1) .NE. 4.
     $            OR.VECA(2) .NE. VECB(2)) THEN
C               new group of components encountered - set flag
                FIFLG=1
              END IF
            END IF
          END IF
C
          IF (FIFLG .EQ. 1) THEN
C           finish the instruction, process information from veca
C           some values not needed by tsget but supplied anyway
            VOLCOD = VECA(1)
            FILE   = VECA(2)
            DELTAT = VECA(3)
            AMODE  = VECA(13)  
C
C           check subsequent primitive instructions for this same source/target pair
            TKEY = KEY
C           loop thru timser instrctions for this operation
 15         CONTINUE
C             assume a match exists
              IMATCH = 1
C             get a temp copy of later instruction
              CALL GETTSI (I35,TKEY,
     O                     VECT)
              TKEY = TKEY + 1
C             check for matching source
              DO 17 I = 16,24
                IF (VECA(I).NE.VECT(I)) THEN
                  IMATCH = 0
                END IF
 17           CONTINUE
C             check for matching target
              DO 18 I = 26,34
                IF (VECA(I).NE.VECT(I)) THEN
                  IMATCH = 0
                END IF
 18           CONTINUE
C
              IF (IMATCH.EQ.1) THEN
C               found another occurance of this source/target pair
C               force exit
                TKEY = EKY + 1
              END IF
            IF (TKEY .LE. EKY) GOTO 15
C            
            IF (IMATCH.EQ.0) THEN
C             variables in veca below needed for ability to modify mfactr during execution
              SVOLX(1)  = VECA(16)  
              SVOLX(2)  = VECA(17)  
              SNUMX     = VECA(18)  
              SGRPNX(1) = VECA(19)  
              SGRPNX(2) = VECA(20)  
              SMEMN(1)  = VECA(21)  
              SMEMN(2)  = VECA(22)  
              SMEMSB(1) = VECA(23)  
              SMEMSB(2) = VECA(24)  
              ARATIO    = AT/AORIGT
              TVOLX(1)  = VECA(26)  
              TVOLX(2)  = VECA(27)  
              TNUMX     = VECA(28)  
              TGRPNX(1) = VECA(29)  
              TGRPNX(2) = VECA(30)  
              TMEMN(1)  = VECA(31)  
              TMEMN(2)  = VECA(32)  
              TMEMSB(1) = VECA(33)  
              TMEMSB(2) = VECA(34)
            ELSE   
C             not the last occurance of this source/target pair,
C             instruction modify not allowed
              SVOLX(1)  = BLNK
              SVOLX(2)  = BLNK  
              SNUMX     = 0  
              SGRPNX(1) = BLNK  
              SGRPNX(2) = BLNK  
              SMEMN(1)  = BLNK  
              SMEMN(2)  = BLNK  
              SMEMSB(1) = 0  
              SMEMSB(2) = 0  
              ARATIO    = 0
              TVOLX(1)  = BLNK
              TVOLX(2)  = BLNK  
              TNUMX     = 0  
              TGRPNX(1) = BLNK  
              TGRPNX(2) = BLNK  
              TMEMN(1)  = BLNK  
              TMEMN(2)  = BLNK  
              TMEMSB(1) = 0  
              TMEMSB(2) = 0  
            END IF
C 
            NCOMPS = NTS
            ENDF   = 0
C
            IF (VOLCOD .EQ. 1) THEN
C             sequential file - read format from proper source
              FMTCLS = VECA(11)
              IF (VECA(12) .NE. 0) THEN
C               get format from ucifl
                TREC= -VECA(12)
                CALL GETUCI (I0,
     M                       TREC,
     O                       UCIBF)
                READ (UCIBF,1000) (PVAR(J),J=1,19)
                READ (CBLNK,1020) PVAR(20)
              ELSE
C               get format from message file using fmtcls
                SGRP  = FMTCLS+ 20
                INITFG= 1
                CLEN  = 80
                CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                       CLEN,
     O                       INBUF1,CONT)
                READ (INBUFF,1010) (PVAR(J),J=1,19)
                READ (CBLNK,1020) PVAR(20)
              END IF
              IF (VECA(14) .EQ. 0) THEN
                GAPVAL = 0.0
              ELSE
                GAPVAL = -1.E30
              END IF
C
C             initialize card sequence information
              CRDSEQ(1) = BEGYR- 1
              CRDSEQ(2) = 12
              CRDSEQ(3) = 31
              CRDNO     = 1
              OLDOFF    = -1440
C
            ELSE IF (VOLCOD .EQ. 2) THEN
C             expad - not yet implemented
C             program bug - report error
              SGRP = 123
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
C
            ELSE IF (VOLCOD .EQ. 3) THEN
C             inpad
C             convert row number in file to virtual origin of inpad row
              FILE = VOPAD+ (FILE-1)*(WIDTH+1)
            ELSE IF (VOLCOD .EQ. 4) THEN
C             tss
C             input the dataset label
              CALL RBUFF (FRC,RECLT,TSSFL,
     O                    TBUFF)
              TRCNO  = FRC
              VOBUFF = 0
              BMTFLG = 0
              BADR   = 1
C             set up values dependent on label info only
              DELTAT = DSDELT
              FREC   = DSFREC
              LREC   = DSLREC
              COMPR  = DSCMPR
              TOTCOM = 0
              DO 20 I=1,NMEMS
                TOTCOM = TOTCOM + MSUB(I)
 20           CONTINUE
C             set up values already known
              UCNT = 0
              ZCNT = 0
              IF (GETF .EQ. 1) THEN
C               processing tsget instructions
                CALL GINIT (START)
              ELSE
C               processing tsput instructions
                CALL PINIT (START,ENDR,STIME,FMIN,DELT)
                DO 30 I= 1, TOTCOM
                  PVAR(I)= 0.0
 30             CONTINUE
              END IF
            ELSE IF (VOLCOD .LT. 0) THEN
C             wdms
C             set gap value for get
              IF (GETF .EQ. 1) THEN
                IF (VECA(14) .EQ. 0) THEN
                  GAPVAL= 0.0
                ELSE
                  GAPVAL= -1.E30
                END IF
              ELSE
C               set aggregation flag for put
                AGGRFG= VECA(14)
              END IF
C
C             set data quality code
              QLFG= VECA(11)
C
C             initialize nrem and pvar for put
              NREM=    0
              PVAR(1)= INITPV(STTRAN(1))
C
C             set time at start of run, convert to wdms time,
              DO 40 I= 1, 5
                INPDAT(I)= START(I)
 40           CONTINUE
              INPDAT(6)= 0
C
              CALL TIMHTW (MESSU,MSGFL,
     M                     INPDAT)
C
              IF (GETF .EQ. 1) THEN
                DO 50 I= 1, DELT
                  CALL TIMBAK (I2,
     M                         INPDAT)
 50             CONTINUE
              END IF
            ELSE IF (VOLCOD .EQ. 6) THEN
C             dss
C             set gap value for get
              IF (GETF .EQ. 1) THEN
                IF (VECA(14) .EQ. 0) THEN
                  GAPVAL= 0.0
                ELSE
                  GAPVAL= -1.E30
                END IF
              END IF
C
C             get info from pathname block
              I= 0
C             dountil found (i= -1)
 60           CONTINUE
                I= I+ 1
                IF (I .GT. NPATH) THEN
C                 error - program bug - missing dss record id number
C                 should be checked and stopped in DSSDS
                  CALL OMSTI (VECA(2))
                  SGRP= 124
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                ELSE
C                 check for dss record id number
                  IF (DSSDSN(I) .EQ. VECA(2)) THEN
                    FILE= DSSFL(I)
                    READ (CTYPE(I),1010) (CTYPI(J),J= 1, 2)
                    READ (CPATH(I),1010) (PVAR(J),J= 1, 16)
                    DO 70 J= 17, 20
                      READ(CBLNK,1020) PVAR(J)
 70                 CONTINUE
                    I= -1
                  END IF
                END IF
              IF (I .GT. 0) GO TO 60
C
              IF (GETF .NE. 1) THEN
C               set nrem and aggrfg to indicate pvar(20) is empty
C               initialize accumulator for aggregation on put
                NREM=     0
                AGGRFG=   0
                PVAR(20)= INITPV(STTRAN(1))
              END IF
C
C             set time at start of run
              DO 80 I= 1, 5
                INPDAT(I)= START(I)
 80           CONTINUE
              INPDAT(6)= 0
C
            END IF
C
            INKEY= INKEY +1
            CALL PUTTSI (I220,INKEY,INSTR)
            IF (OUTLEV .GT. 5) THEN
              CALL TINOUT (INKEY,GETF)
            END IF
C           set up for next instruction
            NTS    = 0
            LTRNFG = 0
          END IF
          DO 100 I= 1, 35
            VECA(I)= VECB(I)
 100      CONTINUE
        IF (KEY .LE. EKY) GO TO 10
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
      SUBROUTINE   NETBLK
     I                   (OPST,OPND,KEYST,KEYND,
     M                    WKEY)
C
C     + + + PURPOSE + + +
C     Expand and check any entries for a given exgrp, in the network
C     block.  chain entries by source and target opn sequence
C     order.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYND,KEYST,OPND,OPST,WKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPST   - ???
C     OPND   - ???
C     KEYST  - starting record number
C     KEYND  - ending record number
C     WKEY   - ???
C
C     + + + COMMON BLOCKS- INTERP3, OSV + + +
      INCLUDE    'crin3.inc'
      INCLUDE    'crin3c.inc'
      INCLUDE    'cmosv.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     EUNITS,MESSU,MSGFL,SCLU,BGRP,GRP,I6,
     $            RWFG,SGRP,SMEMSB(2),START,SXGRP,TGRP,
     $            TOPFST,TOPLST,TMEMSB(2),TXGRP,
     $            TSBKCD,ITSR,GTCOD,DUMTVN
      REAL        MFACTR
      CHARACTER*6 SMEMN,TMEMN
      CHARACTER*4 TRAN,SSYST,SGAPST,DUMTSY,DUMTGP,DUMAMD
C
C     + + + FUNCTIONS + + +
      INTEGER    OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL   GTTMRC,OPNNO,OPNTS,TOPTNO,PAIRS,OMSG,OMSTI,OMSTC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING ANY ENTRIES IN NETWORK BLOCK')
 2010 FORMAT (/,' THE FOLLOWING TARGET OPN-ID SPECIFICATION ',
     $        'IMPLIES NO OPERATION WHICH IS IN THE')
 2020 FORMAT (' SAME EXGRP AS THE SOURCE OPN - ENTRY IGNORED    ',
     $        A6,2I4)
 2060 FORMAT (/,' FINISHED PROCESSING ANY ENTRIES IN NETWORK BLOCK')
C
C     + + + END SPECIFICATIONS + + +
C
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      SCLU  = 215
C
      I6    = 6
C
      IF (OUTLEV .GT. 4) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      TSBKCD= 2
      ITSR= KEYST
      BGRP= 52
C
10    CONTINUE
C       get a network entry (first network entry if first time through)
        CALL GTTMRC
     I              (TSBKCD,KEYND,MESSU,MSGFL,SCLU,BGRP,
     M               ITSR,ECOUNT,
     O               SVOLC,SVOLNO,SGRPN,SMEMN,SMEMSB,SSYST,SGAPST,
     O               MFACTR,TRAN,TVOLC,DUMTVN,TOPFST,TOPLST,TGRPN,
     O               TMEMN,TMEMSB,DUMTSY,DUMTGP,DUMAMD,GTCOD)
C
        IF (GTCOD .EQ. 2) THEN
C         network entry, process, check the source opn-id
          SNUM= OPNNO(SVOLC,SVOLNO,SVOLNO,MAXOPN,OPNTAB,OPST,OPND)
C
          IF (SNUM .GT. 0) THEN
C           something in this exgrp matches the spec
            SXGRP = OPNTAB(5,SNUM)
            SGRP  = OPNTAB(6,SNUM)
            SDELT = GRPTAB(3,SGRP)
C           dummy values
            SAMDCD= 0
            SGAPCD= 0
            SFRC  = 0
C           check and expand the supplied group and member-ids
            EUNITS= 0
C           reading timseries
            RWFG  = 2
            CALL OPNTS
     I                 (SNUM,MAXOPN,OPNTAB,MSGFL,MESSU,SGRPN,
     I                  RWFG,SMEMN,SMEMSB,MAXOSV,MAXTTB,
     M                  ECOUNT,EUNITS,
     O                  OSV,SNTS,STABL,STABLR)
C
C           check the supplied target opn-ids
C           check that the opn-type numbers are valid
            CALL TOPTNO
     I                  (MESSU,MSGFL,
     M                   TOPFST,TOPLST,ECOUNT)
C
C           look for 1st operation which fits supplied target opn-id spec
            TNUM= OPNNO(TVOLC,TOPFST,TOPLST,MAXOPN,OPNTAB,OPST,OPND)
C
            IF (TNUM .EQ. 0) THEN
C             nothing in this exgroup matches spec
              IF (OUTLEV .GE. 3) THEN
C               skip info message
                WRITE (MESSU,2010)
                WRITE (MESSU,2020)  TVOLC, TOPFST, TOPLST
              END IF
            ELSE
C             process each target
C             whiledo tnum not=0
              IF (TNUM .NE. 0) THEN
40              CONTINUE
                  TVOLNO= OPNTAB(3,TNUM)
                  TGRP  = OPNTAB(6,TNUM)
                  TDELT = GRPTAB(3,TGRP)
C                 dummy values
                  TAMDCD= 0
                  TGAPCD= 0
                  TFRC  = 0
C                 writing timeseries
                  RWFG  = 1
C                 check and expand supplied group and member ids
                  CALL OPNTS
     I                       (TNUM,MAXOPN,OPNTAB,MSGFL,MESSU,
     I                        TGRPN,RWFG,TMEMN,TMEMSB,MAXOSV,
     I                        MAXTTB,
     M                        ECOUNT,EUNITS,
     O                        OSV,TNTS,TTABL,TTABLR)
C
C                 check that source/target relationship is valid
                  TXGRP= OPNTAB(6,TNUM)
                  IF (SNUM .GE. TNUM .OR. SXGRP .NE. TXGRP) THEN
C                   error - source/target operations are incompatible;
C                   will be ignored
                    CALL OMSTC (I6,SVOLC1)
                    CALL OMSTI (SVOLNO)
                    CALL OMSTC (I6,TVOLC1)
                    CALL OMSTI (TVOLNO)
                    GRP = 119
                    CALL OMSG (MESSU,MSGFL,SCLU,GRP,
     M                         ECOUNT)
                  ELSE
C                   match individual time series now situated in
C                   source and target tables.  write matched entries
C                   to workfl
                    CALL PAIRS
     I                         (TRAN,-999.0,
     M                          MFACTR,WKEY)
                  END IF
C
C                 find next target operation for this entry
                  START= TNUM+ 1
                  TNUM = OPNNO(TVOLC,TOPFST,TOPLST,MAXOPN,OPNTAB,
     1                         START,OPND)
                IF (TNUM .NE. 0) GO TO 40
              END IF
            END IF
          END IF
        END IF
      IF (GTCOD .EQ. 2) GO TO 10
C
      IF (OUTLEV .GT. 4) THEN
C       done processing message
        WRITE (MESSU,2060)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNTS
     I                  (OPNO,MAXOPN,OPNTAB,MSGFL,MESSU,GRPN,
     I                   RWFG,MEMN,MEMSB,MAXOSV,MXTTB,
     M                   ECOUNT,EUNITS,
     O                   OSV,NTS,TABL,TABLR)
C
C     + + + PURPOSE + + +
C     Process a reference to time series pertaining to operation opno,
C     which were specified in, or implied by, a user's instruction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECOUNT,EUNITS,MAXOPN,MAXOSV,MXTTB,
     $            MSGFL,MEMSB(2),
     $            MESSU,NTS,OPNO,OPNTAB(20,MAXOPN),OSV(MAXOSV),
     $            RWFG,TABL(10,MXTTB)
      REAL        TABLR(10,MXTTB)
      CHARACTER*6 GRPN,MEMN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPNO   - ???
C     MAXOPN - ???
C     OPNTAB - ???
C     MSGFL  - fortran unit number of message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     GRPN   - ???
C     RWFG   - read/write flag, 1-write, 2-read
C     MEMN   - ???
C     MEMSB  - ???
C     MAXOSV - ???
C     MXTTB -  ???
C     ECOUNT - count(s) of specific errors
C     EUNITS - ???
C     OSV    - operation status vector for current operation
C     NTS    - ???
C     TABL   - ???
C     TABLR  - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxttb.inc'
C
C     + + + SAVE VARIABLES + + +
      INTEGER      GCUR,MCUR,MMAX,GMAX,GGRP(MAXTTB),SCLU
      CHARACTER*6  GKYWDS(MAXTTB),MKYWDS(MAXTTB)
      SAVE         GCUR,MCUR,MMAX,GMAX,GGRP,SCLU,
     $             GKYWDS,MKYWDS
C
C     + + + EQUIVALENCES + + +
      INTEGER      MAXTT6
      PARAMETER   (MAXTT6=MAXTTB*6)
      EQUIVALENCE (GKYWDS,GKYWD1),(MKYWDS,MKYWD1)
      CHARACTER*1  GKYWD1(MAXTT6),MKYWD1(MAXTT6)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      GRPNO,I6,J,
     $             N,OMCODE,OPTNO,
     $             OSVKND,OSVKST,UUNITS,
     $             CLU,SGRP,CONT,INITFG,CLEN
      CHARACTER*80 INBUFF
      CHARACTER*6  CHSTR,OPTYP
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (INBUFF,INBUF1),(CHSTR,CHSTR1)
      CHARACTER*1  INBUF1(80),CHSTR1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHKSTR,OMSG,OMSTI,OMSTC,GETOSV,MEMTS,WMSGTT
C
C     + + + INITIALIZATIONS + + +
      DATA GCUR/0/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A6,I4)
C
C     + + + OUTPUT FORMATS + + +
 2070 FORMAT (A4,A2)
C
C     + + + END SPECIFICATIONS + + +
C
      I6= 6
C
C     initialize number of time series in result table
      NTS= 0
C
      WRITE(OPTYP,2070) (OPNTAB(J,OPNO),J=1,2)
      OPTNO = OPNTAB(3,OPNO)
      OMCODE= OPNTAB(4,OPNO)
C
C     read in the osv
      OSVKST= OPNTAB(7,OPNO)
      OSVKND= OPNTAB(8,OPNO)
      CALL GETOSV (OSVKST,OSVKND,MAXOSV,
     O             OSV)
C
C     get internal unit system
      IF (OMCODE .GE. 4) THEN
C       utility operation
        UUNITS= OSV(29)
      ELSE IF (OMCODE .EQ. 3) THEN
C       rchres operation
        UUNITS= OSV(43)
      ELSE
C       perlnd or implnd operation
        UUNITS= OSV(44)
      END IF
C
C     determine the external unit system appropriate to this reference
      IF (EUNITS .EQ. 0) THEN
C       use internal units of this opn as external units if the
C       opn is a utility opn; use ounits if the opn is a process opn
        IF (OMCODE .GE. 4) THEN
C         utility operation
          EUNITS= UUNITS
        ELSE IF (OMCODE .EQ. 3) THEN
C         rchres operation
          EUNITS= OSV(45)
        ELSE
C         perlnd or implnd operation
          EUNITS= OSV(46)
        END IF
      ELSE
C       valid value was supplied in argument list
      END IF
C
      IF (OMCODE .NE. GCUR) THEN
C       need new group keys, get ready to read msgfl
        SCLU  = 140+ OMCODE
        SGRP  = 1
        INITFG= 1
        GMAX  = 0
 20     CONTINUE
          CLEN  = 80
          CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                 CLEN,
     O                 INBUF1,CONT)
          INITFG= 0
          GMAX  = GMAX+ 1
          READ (INBUFF,1000) GKYWDS(GMAX),GGRP(GMAX)
        IF (CONT.EQ.1 .AND. GMAX.LT.50) GO TO 20
        GCUR= OMCODE
        MCUR= 0
      END IF
C
C     check the group name supplied by the user
      CHSTR= GRPN
      GRPNO= CHKSTR(I6,GMAX,CHSTR1,GKYWD1)
C
      IF (GRPNO .EQ. 0) THEN
C       error - group name is invalid
        CHSTR= GRPN
        CALL OMSTC (I6,CHSTR1)
        CHSTR= OPTYP
        CALL OMSTC (I6,CHSTR1)
        CALL OMSTI (OPTNO)
        CLU  = 215
        SGRP = 111
        CALL OMSG (MESSU,MSGFL,CLU,SGRP,
     M             ECOUNT)
      ELSE
C       group valid, check the member name/subscript reference
        IF (MCUR .NE. GRPNO) THEN
C         need to know whats valid and how many there are
          SGRP  = GRPNO+ 1
          INITFG= 1
          MMAX  = 0
 30       CONTINUE
            CLEN  = 80
            CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                   CLEN,
     O                   INBUF1,CONT)
            INITFG= 0
            MMAX  = MMAX+ 1
            READ (INBUFF,1000) MKYWDS(MMAX)
          IF (CONT.EQ.1 .AND. MMAX.LT.50) GO TO 30
          MCUR= GRPNO
        END IF
C
        IF (MEMN .EQ.'      ') THEN
C         all members are implied
          DO 70 N= 1,MMAX
            SGRP= N+ GGRP(GRPNO)
            CALL MEMTS (MESSU,MSGFL,SCLU,SGRP,MEMSB,OPTYP,OPTNO,GRPN,
     I                  EUNITS,UUNITS,MAXOSV,OSV,RWFG,MXTTB,
     M                  NTS,ECOUNT,
     O                  TABL,TABLR)
 70       CONTINUE
        ELSE
C         only one member is involved, check the name
          CHSTR= MEMN
          N    = CHKSTR(I6,MMAX,CHSTR1,MKYWD1)
C
          IF (N .EQ. 0) THEN
C           error - invalid member name
            CHSTR= OPTYP
            CALL OMSTC (I6,CHSTR1)
            CALL OMSTI (OPTNO)
            CHSTR= GRPN
            CALL OMSTC (I6,CHSTR1)
            CHSTR= MEMN
            CALL OMSTC (I6,CHSTR1)
            CLU  = 215
            SGRP = 112
            CALL OMSG (MESSU,MSGFL,CLU,SGRP,
     M                 ECOUNT)
          ELSE
C           ok - process this member
            SGRP= N+ GGRP(GRPNO)
            CALL MEMTS (MESSU,MSGFL,SCLU,SGRP,MEMSB,OPTYP,OPTNO,GRPN,
     I                  EUNITS,UUNITS,MAXOSV,OSV,RWFG,MXTTB,
     M                  NTS,ECOUNT,
     O                  TABL,TABLR)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MEMTS
     I                  (MESSU,MSGFL,SCLU,SGRP,MEMSB,OPTYP,OPTNO,GRPN,
     I                   EUNITS,UUNITS,MAXOSV,OSV,RWFG,MAXTTB,
     M                   NTS,ECOUNT,
     O                   TABL,TABLR)
C
C     + + + PURPOSE + + +
C     Process a single member (n)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECOUNT,EUNITS,MAXTTB,MAXOSV,
     $            MSGFL,SCLU,MEMSB(2),MESSU,SGRP,NTS,OPTNO,
     $            OSV(MAXOSV),RWFG,TABL(10,MAXTTB),UUNITS
      REAL        TABLR(10,MAXTTB)
      CHARACTER*6 GRPN,OPTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of message file
C     SCLU   - current cluster in message file
C     SGRP   - current group in message file
C     MEMSB  - ???
C     OPTYP  - operation type
C     OPTNO  - ???
C     GRPN   - ???
C     EUNITS - ???
C     UUNITS - system of units   1-english, 2-metric
C     OSV    - ???
C     RWFG   - read write flag, 1-write, 2-read
C     NTS    - ???
C     ECOUNT - count(s) of specific errors
C     TABL   - ???
C     TABLR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ERRFG,J,MAXSB1,MAXSB2,MDIM1,MIO,MKIND,MSECT,
     #             OSVBAS,OSVOFF,PROCFG,SPTRN,SUB1,SUB1HI,SUB1LO,SUB2,
     #             SUB2HI,SUB2LO,K1,OFONFG,PROARR(3,2,2),L,K,CONT,
     #             INITFG,CLEN,CLU,GRP,I6
      REAL         ADD,LTVAL(2,2,2),MULT
      CHARACTER*1  INBUF1(80)
      CHARACTER*6  CHSTR,MEMN
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (INBUF1,INBUFF)
      CHARACTER*80 INBUFF
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER      IVALUE
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTI,OMSTC,IVALUE,WMSGTT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA         PROARR/0,1,0,3,0,1,0,0,0,1,1,2/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A6,I3,3X,2I6,4I2,I5,I6,13X,2(F4.0,F8.0))
 1005 FORMAT (56X,2(F4.0,F8.0))
 1010 FORMAT (A4,A2)
C
C     + + + END SPECIFICATIONS + + +
C
      I6= 6
C
C     get info about this member from message file
      INITFG= 1
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1000) MEMN,MDIM1,MAXSB1,MAXSB2,MKIND,SPTRN,MSECT,
     #                   MIO,OSVBAS,OSVOFF,((LTVAL(K,L,1),K=1,2),L=1,2)
      INITFG= 0
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             INBUF1,CONT)
      READ (INBUFF,1005) ((LTVAL(K,L,2),K=1,2),L=1,2)
C
C     section off(1) or on(2)
C
      IF (MSECT .GT. 0) THEN
C       timseries associated with module section,
C       section may not be active - check active sections vector
        OFONFG= OSV(MSECT)+ 1
      ELSE
C       section always on
        OFONFG= 2
      END IF
C
      K1= MIO+ 1
      PROCFG= PROARR(K1,OFONFG,RWFG)
C
      IF (PROCFG .EQ. 0) THEN
C       dont process, not active
      ELSE IF (PROCFG .EQ. 1) THEN
C       check subscripts to make sure this member is used
        ERRFG= 0
C       some data which have come from message file may be pointers to
C       osv locations containing the actual information - process these
C       max values for subscripts
        MAXSB1= IVALUE (MAXSB1,MAXOSV,OSV)
        MAXSB2= IVALUE (MAXSB2,MAXOSV,OSV)
C
        IF ( (MAXSB1 .LE. 0) .OR. (MAXSB2 .LE. 0) ) THEN
C         dont process, maximum subscript is zero - treat like procfg=0
        ELSE
C         process the member, assume no problem with subscripts
C
          IF ( (MEMSB(2) .LT. 0) .OR. (MEMSB(2) .GT. MAXSB2) ) THEN
C           error - subscript out of range
            ERRFG= 1
            CHSTR= OPTYP
            CALL OMSTC (I6,CHSTR1)
            CALL OMSTI (OPTNO)
            CHSTR= GRPN
            CALL OMSTC (I6,CHSTR1)
            CHSTR= MEMN
            CALL OMSTC (I6,CHSTR1)
            CALL OMSTI (MEMSB(2))
            CLU= 215
            GRP= 115
            CALL OMSG (MESSU,MSGFL,CLU,GRP,
     M                 ECOUNT)
            MEMSB(2)= 1
          END IF
C
          IF (MEMSB(2) .EQ. 0) THEN
C           whole possible range
            SUB2LO= 1
            SUB2HI= MAXSB2
          ELSE
C           just specified value
            SUB2LO= MEMSB(2)
            SUB2HI= MEMSB(2)
          END IF
C
C         look at first subscript
          IF ( (MEMSB(1) .LT. 0) .OR. (MEMSB(1) .GT. MAXSB1) ) THEN
C           error - subscript out of range
            ERRFG= 1
            CHSTR= OPTYP
            CALL OMSTC (I6,CHSTR1)
            CALL OMSTI (OPTNO)
            CHSTR= GRPN
            CALL OMSTC (I6,CHSTR1)
            CHSTR= MEMN
            CALL OMSTC (I6,CHSTR1)
            CALL OMSTI (MEMSB(1))
            CLU= 215
            GRP= 115
            CALL OMSG (MESSU,MSGFL,CLU,GRP,
     M                 ECOUNT)
            MEMSB(1)= 1
          ELSE IF (MEMSB(1) .EQ. 0) THEN
C           whole possible range
            SUB1LO= 1
            SUB1HI= MAXSB1
          ELSE
C           just specified value
            SUB1LO= MEMSB(1)
            SUB1HI= MEMSB(1)
          END IF
C
          IF (ERRFG .EQ. 0) THEN
C           unit system conversion constants
            IF ( (EUNITS .EQ. 1) .OR. (EUNITS .EQ. 2) ) THEN
              ADD= LTVAL(1,EUNITS,UUNITS)
              MULT= LTVAL(2,EUNITS,UUNITS)
            ELSE
              ADD= 0.
              MULT= 1.
            END IF
C           built output table
            DO 130 SUB2= SUB2LO,SUB2HI
              DO 120 SUB1= SUB1LO,SUB1HI
C               enter this time series in the results table
                NTS= NTS+ 1
C               member name
                READ (MEMN,1010) (TABL(J,NTS),J=1,2)
C               subscripts
                TABL(3,NTS)= SUB1
                TABL(4,NTS)= SUB2
C
C               osv offset
                TABL(5,NTS)= OSVBAS+ OSVOFF+ (SUB2- 1)*MDIM1+ SUB1- 1
                TABL(6,NTS)= MKIND
                TABL(7,NTS)= SPTRN
                TABLR(8,NTS)= ADD
                TABLR(9,NTS)= MULT
 120          CONTINUE
 130        CONTINUE
          END IF
        END IF
      ELSE IF (PROCFG .EQ. 2) THEN
C       trying to output an input only timeseries
        ERRFG= 1
        CHSTR= OPTYP
        CALL OMSTC (I6,CHSTR1)
        CALL OMSTI (OPTNO)
        CHSTR= GRPN
        CALL OMSTC (I6,CHSTR1)
        CHSTR= MEMN
        CALL OMSTC (I6,CHSTR1)
        CLU= 215
        GRP= 96
        CALL OMSG (MESSU,MSGFL,CLU,GRP,
     M             ECOUNT)
      ELSE IF (PROCFG .EQ. 3) THEN
C       trying to input an output only timeseries
        ERRFG= 1
        CHSTR= OPTYP
        CALL OMSTC (I6,CHSTR1)
        CALL OMSTI (OPTNO)
        CHSTR= GRPN
        CALL OMSTC (I6,CHSTR1)
        CHSTR= MEMN
        CALL OMSTC (I6,CHSTR1)
        CLU= 215
        GRP= 97
        CALL OMSG (MESSU,MSGFL,CLU,GRP,
     M             ECOUNT)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION IVALUE
     I                          (IVAL,MAXOSV,OSV)
C
C     + + + PURPOSE + + +
C     Look at a supplied integer value.  if it's positive, return the
C     value, if negative return the value in the osv with a virtual
C     origin equal to its absolute value.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IVAL,MAXOSV,OSV(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - value supplied
C     MAXOSV - maximum size of osv
C     OSV    - operation status vector
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ADDR
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IVAL .GE. 0) THEN
C       positive, return the value
        IVALUE= IVAL
      ELSE
C       negative, return value from osv at position value
        ADDR  = IABS(IVAL)
        IVALUE= OSV(ADDR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PAIRS
     I                  (TRAN,AREA,
     M                   MFACTR,WKEY)
C
C     + + + PURPOSE + + +
C     Check pairs of entries in source and target tables and write
C     paired entries to workfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WKEY
      REAL        AREA,MFACTR
      CHARACTER*4 TRAN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRAN   - ???
C     AREA   - area term for pair, -999 for ignore
C     MFACTR - ???
C     WKEY   - ???
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE     'crin3.inc'
      INCLUDE     'crin3c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CASE,DIFF,IDUM1(8),IDUM2(10),ERRFG,I0,I8,J,
     $            MESSU,N,NUMBR,SKIND,SPTRN,STKIND,STTRAN,MSGFL,
     $            TKIND,RWFG,LEN,REC(50),MXKY,I6,I4,SCLU,SGRP
      REAL        A,B,SADD,SMULT,TADD,TMULT
      CHARACTER*6 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(6)
      EQUIVALENCE (REC(1),RREC(1))
      REAL         RREC(50)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD,ABS
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHKSTR,WORKIO,OMSG,ZIPI,OMSTC,OMSTI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2070 FORMAT (A4,A2)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I4  = 4
      I6  = 6
      I8  = 8
      SCLU= 215
C
      CALL ZIPI(I8,I0,IDUM1)
      J= 10
      CALL ZIPI(J,I0,IDUM2)
C
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      DIFF  = SNTS- TNTS
      IF (DIFF .GT. 0) THEN
        NUMBR= TNTS
      ELSE
        NUMBR= SNTS
      END IF
C
      IF (SNTS.EQ.0 .OR. TNTS.EQ.0) THEN
        NUMBR= 0
      END IF
C
      IF (NUMBR .GT. 0) THEN
C       there are 1 or more pairs to process
        ERRFG= 0
C
C       check time step relationship for aggregation to wdm dataset;
C       set wdm target time step to inpad time step of source if
C       aggregation not being performed
        IF (TVOLC(1:3) .EQ. EXTKWL(3)(1:3)) THEN
C         target is wdm dataset
          IF (TDELT .GT. SDELT .AND.
     $       (TGAPCD .EQ. 1 .OR. TGAPCD .EQ. 3)) THEN
C           wdm dataset timestep > opn timestep and aggregation is on
            IF (MOD(TDELT,SDELT) .NE. 0) THEN
C             error - wdm time step not even multiple of opn timestep
              SGRP = 150
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
            IF (TGAPCD .EQ. 3) THEN
              TGAPCD= 1
            END IF
          ELSE
C           aggregation not being performed
            IF (TGAPCD .EQ. 2 .OR. TGAPCD .EQ. 3) THEN
C             vbtime is 1, check time step
              IF (SDELT .NE. TDELT) THEN
C               error, time step of dataset and run dont match
                SGRP = 150
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
            END IF
            TGAPCD= 0
            TDELT = SDELT
          END IF
        END IF
C
C       now check for compatibility of time intervals
        IF (SDELT .EQ. TDELT) THEN
          CASE= 1
        ELSE
          IF ( MOD(SDELT,TDELT) .NE. 0) THEN
            IF ( MOD(TDELT,SDELT) .NE. 0) THEN
C             error - source and target time intervals not
C             compatible
              CALL OMSTC (I6,SVOLC1)
              CALL OMSTI (SVOLNO)
              CALL OMSTC (I6,TVOLC1)
              CALL OMSTI (TVOLNO)
              SGRP = 116
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFG= 1
            END IF
          END IF
C
          IF (SDELT .GT. TDELT) THEN
            CASE= 2
          ELSE
            CASE= 3
          END IF
        END IF
C
        IF (ERRFG .EQ. 0) THEN
C         process the entries
          DO 100 N= 1,NUMBR
C           check the next source/target pair
            SKIND= STABL(6,N)
            TKIND= TTABL(6,N)
C
            IF (SKIND .EQ. 1) THEN
C             source timeseries is point
              IF (TKIND .EQ. 1 .OR. TKIND .EQ. 3) THEN
C               point to point (since target is point or undefined)
                STKIND= 1
              ELSE
C               point to mean
                STKIND= 3
              END IF
            ELSE IF (SKIND .EQ. 2) THEN
C             source timeseries is mean
              IF (TKIND .EQ. 1) THEN
C               mean to point - error
                ERRFG = 1
                STKIND= 0
                CALL OMSTC (I6,SVOLC1)
                CALL OMSTI (SVOLNO)
                CHSTR= SGRPN
                CALL OMSTC (I6,CHSTR1)
                WRITE (CHSTR,2070) (STABL(J,N),J=1,2)
                CALL OMSTC (I6,CHSTR1)
                CALL OMSTI (STABL(3,N))
                CALL OMSTI (STABL(4,N))
                CALL OMSTC (I6,TVOLC1)
                CALL OMSTI (TVOLNO)
                CHSTR= TGRPN
                CALL OMSTC (I6,CHSTR1)
                WRITE (CHSTR,2070) (TTABL(J,N),J=1,2)
                CALL OMSTC (I6,CHSTR1)
                CALL OMSTI (TTABL(3,N))
                CALL OMSTI (TTABL(4,N))
                SGRP = 117
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
C
              ELSE
C               mean to mean  (target is either mean or undefined)
                STKIND= 2
              END IF
            ELSE IF (SKIND .EQ. 3) THEN
C             skind is undefined so base stkind on tkind
C             this is for sequential input
              IF (TKIND .EQ. 1) THEN
                STKIND= 1
              ELSE IF (TKIND .EQ. 2) THEN
                STKIND= 2
              END IF
            END IF
C
            IF (ERRFG .NE. 1) THEN
C             ok - continue
              IF (TRAN .EQ. '    ') THEN
C               use default source/target transformation functional
                SPTRN= TTABL(7,N)
                IF (SPTRN .GT. 0 .AND. SDELT .EQ. 1440) THEN
C                 a special disaggregation functional is to be used
                  STTRAN= SPTRN
                ELSE
C                 default functional to be used
                  STTRAN= TRNTAB(9,STKIND,CASE)
C                 change default intp or last to aver if wdm or dss
C                 source, or dss target
                  IF ( (SVOLC(1:3) .EQ. EXTKWL(3)(1:3)) .OR.
     #                 (SVOLC(1:4) .EQ. EXTKWL(4)) .OR.
     #                 (TVOLC(1:4) .EQ. EXTKWL(4)) ) THEN
                    IF (STTRAN .EQ. 7 .OR. STTRAN .EQ. 8) THEN
                      STTRAN= 3
                    END IF
                  END IF
                END IF
              ELSE
C               check supplied transformation functional
                CHSTR(1:4)= TRAN
                STTRAN    = CHKSTR(I4,I8,CHSTR1,TRNKW1)
                IF (STTRAN .EQ. 0) THEN
C                 invalid functional keyword
                  ERRFG= 1
                ELSE
C                 valid keyword so far, but must check wdm source
C                 and other sources separately
                  ERRFG = 0
C                 sources other than wdm - regular functionals are used;
C                 this is ok since disagg to a wdm target is not allowed,
C                 and intp is not implemented for wdm put;
C                 sdelt <= tdelt for wdm put
                  IF (TRNTAB(STTRAN,STKIND,CASE) .EQ. 0) THEN
C                   transformation not allowed
                    ERRFG =1
                  END IF
C                 check wdm and dss source, and dss target separately
                  IF ( (SVOLC(1:3) .EQ. EXTKWL(3)(1:3)) .OR.
     #                 (SVOLC(1:4) .EQ. EXTKWL(4)) .OR.
     #                 (TVOLC(1:4) .EQ. EXTKWL(4)) ) THEN
                    IF ( (STTRAN .EQ. 7) .OR. (STTRAN .EQ. 8) ) THEN
C                     intp/last not valid
                      ERRFG= 1
                    ELSE IF ((STTRAN .EQ. 1) .OR. (STTRAN .EQ. 3)) THEN
C                     same/aver is valid
                      ERRFG= 0
                    END IF
                  END IF
                END IF
C
                IF (ERRFG .EQ. 1) THEN
C                 error - invalid source/target transformation
C                 functional
                  CALL OMSTC (I6,SVOLC1)
                  CALL OMSTI (SVOLNO)
                  CHSTR= SGRPN
                  CALL OMSTC (I6,CHSTR1)
                  WRITE (CHSTR,2070) (STABL(J,N),J=1,2)
                  CALL OMSTC (I6,CHSTR1)
                  CALL OMSTI (STABL(3,N))
                  CALL OMSTI (STABL(4,N))
                  CHSTR(1:4)= TRAN
                  CALL OMSTC (I4,CHSTR1)
                  CALL OMSTI (STKIND)
                  CALL OMSTI (CASE)
                  CALL OMSTC (I6,TVOLC1)
                  CALL OMSTI (TVOLNO)
                  CHSTR= TGRPN
                  CALL OMSTC (I6,CHSTR1)
                  WRITE (CHSTR,2070) (TTABL(J,N),J=1,2)
                  CALL OMSTC (I6,CHSTR1)
                  CALL OMSTI (TTABL(3,N))
                  CALL OMSTI (TTABL(4,N))
                  SGRP = 118
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
              END IF
C
              IF (ERRFG .EQ. 0) THEN
C               derive composite pair of constants for linear transform
                SMULT= STABLR(9,N)
                SADD = STABLR(8,N)
                TMULT= TTABLR(9,N)
                TADD = TTABLR(8,N)
cC
cC               mfactr is user-supplied factor, check for default value
c                IF ((ABS(MFACTR)) .LE. 0.0) THEN
c                  MFACTR= 1.0
c                END IF
C
                A= (TMULT/SMULT)*MFACTR
                IF (ABS(A-1.0) .LT. .001) THEN
                  A= 1.0
                END IF
C
                B= (TADD- MFACTR* SADD)*TMULT
                IF (ABS(B-0.0) .LT. .0001) THEN
                  B= 0.0
                END IF
C
C               put this pair in workfl
                WKEY  = WKEY+ 1
C               make up rec
                READ(SVOLC,1000) (REC(J),J=1,2)
                REC(3)= SNUM
                REC(4)= SDELT
                READ(SGRPN,1000) (REC(J),J=5,6)
                DO 70 J= 1,5
                  REC(J+6) = STABL(J,N)
                  REC(J+32)= TTABL(J,N)
 70             CONTINUE
                REC(12)= SAMDCD
                REC(13)= SGAPCD
                REC(14)= SFRC
                DO 80 J= 1,8
                 REC(J+14)=IDUM1(J)
 80             CONTINUE
C               addition for enhanced timser output
                REC(19) = SDELT
                REC(20) = STTRAN
                IF (ABS(AREA+999.0).GT.1.0E-5) THEN
C                 area term specified, primary for update during run
                  RREC(21)= AREA
                  RREC(22)= MFACTR/AREA
                ELSE
                  RREC(21)= MFACTR
                  RREC(22)= 1.0
                END IF
C               end addition
                REC(23) = STKIND
                REC(24) = STTRAN
                RREC(25)= A
                RREC(26)= B
                READ(TVOLC,1000) (REC(J),J=27,28)
                REC(29) = TNUM
                REC(30) = TDELT
                READ(TGRPN,1000) (REC(J),J=31,32)
                REC(38) = TAMDCD
                REC(39) = TGAPCD
                REC(40) = TFRC
                DO 90 J= 1,10
                  REC(J+40)= IDUM2(1)
 90             CONTINUE
C
                RWFG= 1
                LEN = 50
                MXKY= 0
                CALL WORKIO (RWFG,LEN,WKEY,
     M                       REC,MXKY)
              END IF
            END IF
 100      CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCHBLK
     I                   (OPST,OPND,KEYST,KEYND,MAXMLK,
     I                    MSLINX,NMLTBS,
     M                    WKEY)
C
C     + + + PURPOSE + + +
C     Expand and check any entries for a given exgrp, in the schematics
C     block.
C
C     + + + HISTORY + + +
C     12/7/2004  jlk&pbd added area
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   KEYND,KEYST,OPND,OPST,WKEY,MAXMLK,MSLINX(MAXMLK,3),
     #          NMLTBS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPST   - ???
C     OPND   - ???
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXMLK - maximum number of mass-link tables
C     MSLINX - ???
C     NMLTBS - ???
C     WKEY   - ???
C
C     + + + PARAMETERS + + +
      INTEGER    MAXCNX
      PARAMETER (MAXCNX=150)
C
C     + + + COMMON BLOCKS- INTERP3, OSV + + +
      INCLUDE    'crin3.inc'
      INCLUDE    'crin3c.inc'
      INCLUDE    'cmosv.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     EUNITS,MSGFL,MESSU,BGRP,RWFG,SGRP,SMEMSB(2),
     $            SXGRP,TGRP,TMEMSB(2),TXGRP,I,MSLKTB,
     $            SMMSUB(2,MAXCNX),
     #            TMMSUB(2,MAXCNX),NUMCON,CNNXN,
     #            DUMSMS(2),DTMSUB(2),
     #            TSBKCD,ITSR,GTCOD,DUMTPL,SCLU,GRP,I6
      REAL        MFACTR,MFCT(MAXCNX),AREA
      CHARACTER*6 SMEMN,TMEMN,SGPNAM(MAXCNX),SMMNAM(MAXCNX),
     #            TGPNAM(MAXCNX),TMMNAM(MAXCNX),
     #            DUMSGN,DUMTGN,DUMSMB,DUMTMB
      CHARACTER*4 TRAN,TRANSF(MAXCNX),DUMTSY,DUMTGP,DUMAMD,
     #            DUMSSY,DUMTRN,DUMSGP
C
C     + + + FUNCTIONS + + +
      INTEGER     OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL    OPNNO,PSCHEM,OPNTS,TOPTNO,OMSG,OMSTI,OMSTC
      EXTERNAL    PAIRS,GTTMRC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING ANY ENTRIES IN SCHEMATICS BLOCK')
 2010 FORMAT (/,' THE FOLLOWING TARGET OPN-ID SPECIFICATION ',
     $          'IS NOT IN THE')
 2020 FORMAT (' SAME INGRP AS THE SOURCE OPN - ENTRY IGNORED    ',
     $        A6,I4)
 2060 FORMAT (/,' FINISHED PROCESSING ANY ENTRIES IN SCHEMATICS BLOCK')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 215
      MESSU = FILE(1)
      MSGFL = FILE(15)
      I6    = 6
C
      IF (OUTLEV .GT. 4) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      TSBKCD= 4
      ITSR= KEYST
      BGRP= 54
C
10    CONTINUE
C       get a schematic entry (first schematic entry if first time through)
        CALL GTTMRC
     I              (TSBKCD,KEYND,MESSU,MSGFL,SCLU,BGRP,
     M               ITSR,ECOUNT,
     O               SVOLC,SVOLNO,DUMSGN,DUMSMB,DUMSMS,DUMSSY,DUMSGP,
     O               AREA,DUMTRN,TVOLC,TVOLNO,MSLKTB,DUMTPL,DUMTGN,
     O               DUMTMB,DTMSUB,DUMTSY,DUMTGP,DUMAMD,GTCOD)
C
        IF (GTCOD .EQ. 4) THEN
C         schematic entry, expand it
          CALL PSCHEM
     I                (MSGFL,MESSU,MAXOPN,OPNTAB,OPST,OPND,SVOLC,SVOLNO,
     I                 TVOLC,TVOLNO,AREA,MAXMLK,MSLINX,NMLTBS,MSLKTB,
     I                 MAXCNX,
     M                 ECOUNT,
     O                 SGPNAM,SMMNAM,SMMSUB,MFCT,TRANSF,
     O                 TGPNAM,TMMNAM,TMMSUB,NUMCON)
          IF (NUMCON .GT. 0) THEN
C           process each connection implied by the schematic entry
            DO 50 CNNXN = 1, NUMCON
C             assign necessary variables for subroutines opnts and pairs
              MFACTR= MFCT(CNNXN)
              TRAN  = TRANSF(CNNXN)
              SGRPN = SGPNAM(CNNXN)
              SMEMN = SMMNAM(CNNXN)
              TGRPN = TGPNAM(CNNXN)
              TMEMN = TMMNAM(CNNXN)
              DO 20 I= 1,2
                SMEMSB(I)= SMMSUB(I,CNNXN)
                IF (TMMSUB(I,CNNXN) .GE. 1) THEN
C                 mass-link specified subscript overrides schematic default
                  TMEMSB(I)= TMMSUB(I,CNNXN)
                ELSE
C                 use default
                  TMEMSB(I)= DTMSUB(I)
                END IF
 20           CONTINUE
C
C             check the source opn-id
              SNUM= OPNNO(SVOLC,SVOLNO,SVOLNO,MAXOPN,OPNTAB,OPST,OPND)
C
              IF (SNUM .GT. 0) THEN
C               entry in this exgrp matches the source spec
                SXGRP = OPNTAB(5,SNUM)
                SGRP  = OPNTAB(6,SNUM)
                SDELT = GRPTAB(3,SGRP)
C               dummy values
                SAMDCD= 0
                SGAPCD= 0
                SFRC  = 0
C               check the supplied group and member-ids
                EUNITS= 0
C               reading timseries
                RWFG  = 2
                CALL OPNTS (SNUM,MAXOPN,OPNTAB,MSGFL,MESSU,
     I                      SGRPN,RWFG,SMEMN,SMEMSB,MAXOSV,MAXTTB,
     M                      ECOUNT,EUNITS,
     O                      OSV,SNTS,STABL,STABLR)
C
C               check the supplied target opn-id
C               check that the opn-type number is valid
                CALL TOPTNO (MESSU,MSGFL,
     M                       TVOLNO,TVOLNO,ECOUNT)
C
C               check the target opn-id
                TNUM= OPNNO(TVOLC,TVOLNO,TVOLNO,MAXOPN,OPNTAB,OPST,OPND)
C
                IF (TNUM .EQ. 0) THEN
C                 nothing in this exin group matches target
                  IF (OUTLEV .GT. 4) THEN
                    WRITE (MESSU,2010)
                    WRITE (MESSU,2020)  TVOLC, TVOLNO
                  END IF
                ELSE
C                 process the target
                  TGRP  = OPNTAB(6,TNUM)
                  TDELT = GRPTAB(3,TGRP)
C                 dummy values
                  TAMDCD= 0
                  TGAPCD= 0
                  TFRC  = 0
C                 writing timseries
                  RWFG  = 1
C                 check and expand supplied group and member ids
                  CALL OPNTS (TNUM,MAXOPN,OPNTAB,MSGFL,MESSU,
     I                        TGRPN,RWFG,TMEMN,TMEMSB,MAXOSV,
     I                        MAXTTB,
     M                        ECOUNT,EUNITS,
     O                        OSV,TNTS,TTABL,TTABLR)
C
C                 check that source/target relationship is valid
                  TXGRP= OPNTAB(6,TNUM)
                  IF (SNUM .GE. TNUM .OR. SXGRP .NE. TXGRP) THEN
C                   error - source/target operations are incompatible;
C                   will be ignored
                    CALL OMSTC (I6,SVOLC1)
                    CALL OMSTI (SVOLNO)
                    CALL OMSTC (I6,TVOLC1)
                    CALL OMSTI (TVOLNO)
                    GRP = 119
                    CALL OMSG (MESSU,MSGFL,SCLU,GRP,
     M                         ECOUNT)
                  ELSE
C                   match individual time series now situated in
C                   source and target tables.  write matched entries
C                   to workfl
                    CALL PAIRS (TRAN,AREA,
     M                          MFACTR,WKEY)
                  END IF
                END IF
              END IF
 50         CONTINUE
          END IF
        END IF
C
      IF (GTCOD .EQ. 4) GO TO 10
C
      IF (OUTLEV .GT. 4) THEN
C       end processing message
        WRITE (MESSU,2060)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SRCBLK
     I                    (OPST,OPND,KEYST,KEYND,SDATIM,EDATIM,
     I                     SFTKST,SFTKND,IHMFG,
     M                     WKEY,WKSTES,WKNDES)
C
C     + + + PURPOSE + + +
C     expand and check entries in ext sources block, for a given range
C     of operations, and chain by target opn sequence order
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EDATIM(5),OPND,OPST,SDATIM(5),SFTKND,SFTKST,WKEY,
     #          WKNDES,WKSTES,KEYST,KEYND,IHMFG
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C     12/7/2004   jlk&pbd added dummy area
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPST   - ???
C     OPND   - ???
C     KEYST  - starting record number
C     KEYND  - ending record number
C     SDATIM - ???
C     EDATIM - ???
C     SFTKST - ???
C     SFTKND - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     WKEY   - ???
C     WKSTES - ???
C     WKNDES - ???
C
C     + + + COMMON BLOCKS- INTERP3, OSV + + +
      INCLUDE 'crin3.inc'
      INCLUDE 'crin3c.inc'
      INCLUDE 'cmosv.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     MESSU,SMEMSB(2),TMEMSB(2),SCLU,BGRP,
     $            START,TGRP,TOPFST,TOPLST,
     $            MSGFL,WKND,WKST,I1,I2,I3,I4,RWFG,
     $            TSBKCD,ITSR,GTCOD,DUMTVN
      REAL        MFACTR 
      CHARACTER*6 SMEMN,TMEMN
      CHARACTER*4 TRAN,SSYST,AMDST,SGAPST,DUMTSY,DUMAMD,DUMTGP
C
C     + + + FUNCTIONS + + +
      INTEGER   OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTTMRC,TOPTNO,EXTTS,OPNTS,PAIRS,CHAIN,WKDMP1,OPNNO
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (/,' PROCESSING ANY ENTRIES IN EXT SOURCES BLOCK')
2010  FORMAT (/,' FINISHED PROCESSING ANY ENTRIES IN EXT SOURCES BLOCK')
C
C     + + + END SPECIFICATIONS + + +
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      SCLU= 215
C
C     wkst is key at which first record will be written to workfl,
C     if any are written
      WKST= WKEY+ 1
C
      IF (OUTLEV .GT. 4) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      TSBKCD= 1
      ITSR= KEYST
      BGRP= 50
C
10    CONTINUE
C       get an ext sources entry (first entry if first time through)
        CALL GTTMRC
     I              (TSBKCD,KEYND,MESSU,MSGFL,SCLU,BGRP,
     M               ITSR,ECOUNT,
     O               SVOLC,SVOLNO,SGRPN,SMEMN,SMEMSB,SSYST,SGAPST,
     O               MFACTR,TRAN,TVOLC,DUMTVN,TOPFST,TOPLST,TGRPN,
     O               TMEMN,TMEMSB,DUMTSY,DUMTGP,DUMAMD,GTCOD)
        IF (GTCOD .EQ. 1) THEN
C         ext sources entry, process
C
C         check whether target reference is valid and if it covers
C         any time series in this exgroup
          CALL TOPTNO
     I                (MESSU,MSGFL,
     M                 TOPFST,TOPLST,ECOUNT)
          TNUM= OPNNO(TVOLC,TOPFST,TOPLST,MAXOPN,OPNTAB,OPST,OPND)
C
          IF (TNUM .GT. 0) THEN
C           at least one operation in this exgroup is included in this
C           entry - check entry in detail and expand it if necessary
C
C           source reference
C           for ext sources, access mode is not meaningful
            AMDST= '    '
            I1   = 0
            CALL EXTTS
     I                 (SMEMN,SMEMSB,SSYST,AMDST,SGAPST,SDATIM,
     I                  EDATIM,SVOLC,SVOLNO,SFTKST,SFTKND,I1,MAXTTB,
     I                  IHMFG,
     O                  SNUM,SDELT,SUNITS,SGRPN,SNTS,SAMDCD,SFRC,
     O                  SGAPCD,STABL,STABLR)
C
C           whiledo tnum not= 0
30          CONTINUE
              IF (TNUM .NE. 0) THEN
                TVOLNO= OPNTAB(3,TNUM)
                TGRP  = OPNTAB(6,TNUM)
                TDELT = GRPTAB(3,TGRP)
C               dummy values
                TAMDCD= 0
                TGAPCD= 0
                TFRC  = 0
C               writing timeseries
                RWFG = 1
C               check target reference
                CALL OPNTS
     I                     (TNUM,MAXOPN,OPNTAB,MSGFL,MESSU,
     I                      TGRPN,RWFG,TMEMN,TMEMSB,MAXOSV,MAXTTB,
     M                      ECOUNT,SUNITS,
     O                      OSV,TNTS,TTABL,TTABLR)
C
C               match source and target entries,
C               write matched pairs to workfl
                CALL PAIRS
     I                     (TRAN,-999.0,
     M                      MFACTR,WKEY)
C
C               find the next target opn for this entry
                START= TNUM+ 1
                TNUM = OPNNO(TVOLC,TOPFST,TOPLST,MAXOPN,OPNTAB,START,
     I                       OPND)
              END IF
            IF (TNUM .NE. 0) GO TO 30
          END IF
        END IF
C       loop back if more entries
      IF (GTCOD .EQ. 1) GO TO 10
C
      IF (WKEY .GE. WKST) THEN
C       something was written to workfl
        WKND= WKEY
C
C       sort, using pointers, in target opn seq order and keep workfl
C       keys in opntab
        I1= 29
        I2= 41
        I3= 9
        I4= 10
        CALL CHAIN
     I             (OPST,OPND,WKST,WKND,I1,I2,I3,I4,MAXOPN,
     O              OPNTAB,WKSTES,WKNDES)
C
        IF (OUTLEV .GT. 5) THEN
C         dump records written to workfl
          CALL WKDMP1 (WKST,WKND,MESSU)
        END IF
      END IF
C
      IF (OUTLEV .GT. 4) THEN
C       done processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TARBLK
     I                   (OPST,OPND,KEYST,KEYND,SDATIM,EDATIM,TFTKST,
     I                    TFTKND,IHMFG,
     M                    WKEY)
C
C     + + + PURPOSE + + +
C     Expand and check any entries in the ext targets block for this
C     exgrp.  chain entries by source opn sequence order.
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C     12/7/2004   jlk&pbd added dummy area
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EDATIM(5),KEYND,KEYST,OPND,OPST,SDATIM(5),
     $          TFTKND,TFTKST,WKEY,IHMFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPST   - ???
C     OPND   - ???
C     KEYST  - starting record number
C     KEYND  - ending record number
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TFTKST - ???
C     TFTKND - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     WKEY   - ???
C
C     + + + COMMON BLOCKS- INTERP3, OSV + + +
      INCLUDE    'crin3.inc'
      INCLUDE    'crin3c.inc'
      INCLUDE    'cmosv.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDUM1,IDUM2,MESSU,MSGFL,SGRP,SCLU,BGRP,
     $            SMEMSB(2),TMEMSB(2),
     $            WKND,WKST,I1,I2,I3,I4,TSBKCD,
     $            ITSR,GTCOD,RWFG,TOPFST,TOPLST
      REAL        MFACTR
      CHARACTER*6 SMEMN,TMEMN
      CHARACTER*4 TRAN,TSYST,SSYST,TGAPST,AMDST,SGAPST
C
C     + + + FUNCTIONS + + +
      INTEGER   OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTTMRC,OPNNO,EXTTS,OPNTS,PAIRS,CHAIN,WKDMP1
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING ANY ENTRIES IN EXT TARGETS BLOCK')
 2010 FORMAT (/,' FINISHED PROCESSING ANY ENTRIES IN EXT TARGETS BLOCK')
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      SCLU= 215
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
C     wkst is key at which first record will be written to
C     workfl, if any are written
      WKST= WKEY+ 1
C
      IF (OUTLEV .GT. 4) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      TSBKCD= 3
      ITSR= KEYST
      BGRP= 56
C
10    CONTINUE
C       get an ext targets entry (first entry if first time through)
        CALL GTTMRC
     I              (TSBKCD,KEYND,MESSU,MSGFL,SCLU,BGRP,
     M               ITSR,ECOUNT,
     O               SVOLC,SVOLNO,SGRPN,SMEMN,SMEMSB,SSYST,SGAPST,
     O               MFACTR,TRAN,TVOLC,TVOLNO,TOPFST,TOPLST,TGRPN,
     O               TMEMN,TMEMSB,TSYST,TGAPST,AMDST,GTCOD)
C
        IF (GTCOD .EQ. 3) THEN
C         ext targets entry, process
C
C         check the source opn-id
          SNUM= OPNNO(SVOLC,SVOLNO,SVOLNO,MAXOPN,OPNTAB,OPST,OPND)
C
          IF (SNUM .GT. 0) THEN
C           check and expand the supplied target reference
            CALL EXTTS
     I                (TMEMN,TMEMSB,TSYST,AMDST,TGAPST,SDATIM,
     I                 EDATIM,TVOLC,TVOLNO,TFTKST,TFTKND,I1,MAXTTB,
     I                 IHMFG,
     O                 TNUM,TDELT,TUNITS,TGRPN,TNTS,TAMDCD,TFRC,
     O                 TGAPCD,TTABL,TTABLR)
C
C           check and expand the supplied source reference
            SGRP  = OPNTAB(6,SNUM)
            SDELT = GRPTAB(3,SGRP)
C           dummy values
            SAMDCD= 0
            SGAPCD= 0
            SFRC  = 0
C           reading timeseries
            RWFG  = 2
            CALL OPNTS
     I                (SNUM,MAXOPN,OPNTAB,MSGFL,MESSU,SGRPN,
     I                 RWFG,SMEMN,SMEMSB,MAXOSV,MAXTTB,
     M                 ECOUNT,TUNITS,
     O                 OSV,SNTS,STABL,STABLR)
C
C           match any individual time series references situated
C           in the source and target tables.  write matched
C           references to workfl.
            CALL PAIRS
     I                 (TRAN,-999.0,
     M                  MFACTR,WKEY)
          END IF
        END IF
      IF (GTCOD .EQ. 3) GO TO 10
C
      IF (WKEY .GE. WKST) THEN
C       something was written to workfl
        WKND= WKEY
C
C       chain in source opn sequence order, and keep keys in opntab
        I1= 3
        I2= 15
        I3= 15
        I4= 16
        CALL CHAIN
     I             (OPST,OPND,WKST,WKND,I1,I2,I3,I4,MAXOPN,
     O              OPNTAB,IDUM1,IDUM2)
C
        IF (OUTLEV .GT. 5) THEN
C         dump records written to workfl
          CALL WKDMP1 (WKST,WKND,MESSU)
        END IF
      END IF
C
      IF (OUTLEV .GT. 4) THEN
C       done processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
