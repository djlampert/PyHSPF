C
C     4.1
C
      SUBROUTINE TSGET
     I                 (FILEAR,TSGKST,TSGKND,DELT,STIVL,WID,FSTCAL,
     I                  EXTFG)
C
C     + + + PURPOSE + + +
C     Get a time series from tss, sequential file, dss, wdm file,
C     expad or inpad and place it in the inpad
C
C     + + + HISTORY + + +
C     12/6/2004 - jlk&pbd - changed I200 to I220 for new
C       TSGET/TSPUT file length
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,FILEAR(15),TSGKND,TSGKST,STIVL,WID,FSTCAL,EXTFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILEAR - ???
C     TSGKST - ???
C     TSGKND - ???
C     DELT   - simulation time interval in minutes
C     STIVL  - index of first interval in INSPAN
C     WID    - number of intervals being simulated
C     FSTCAL - flag indicating first interval of run
C     EXTFG  - flag indicating which instructions to get:
C              0 - all
C              1 - external only
C              2 - internal only
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLAG,I,WIDTH,WORDI,WDMSFL(4),SCLU,SGRP,DATIM(5),I220,
     #          DUMIN(3),I1,I3,SKIPFG,WDMIDX,VOLUME
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (DUMIN,RDUMIN)
      REAL         RDUMIN(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETSEQ,MOVROW,GETTSS,GETWDM,GPDUMP,OMSG,OMSTI,OMSTD
      EXTERNAL  FDATIM,GETDSS,GETTSI,PUTTSI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 231
      I1   =   1
      I3   =   3
      I220 = 220
      BLEN = 545
      KNT  =   0
      WIDTH= WID +1
C
      MESSU = FILEAR(1)
      TSSFL = FILEAR(10)
      DO 5 I= 1, 4
        WDMSFL(I)= FILEAR(10+I)
 5    CONTINUE
      MSGFL = FILEAR(15)
C
      CALL GETTSI (I3,I1,
     O             DUMIN)
      TESTFG= DUMIN(1)
      RECLT= DUMIN(2)
      UNDEF= RDUMIN(3)
C
C     initialize suitability flag on for no undefined values
      SUITFG= 1
C
      DO 80 I= TSGKST,TSGKND
        CALL GETTSI (I220,I,
     O               INSTR)
C
        IF (TESTFG .NE. 0) THEN
          GPFLAG = 1
          CALL GPDUMP(I,GPFLAG,WIDTH,DELT)
        END IF
C
        IF ( (EXTFG .EQ. 1) .AND. (VOLCOD .EQ. 3) ) THEN
C         external only - skip movrow
          SKIPFG= 1
        ELSE IF ( (EXTFG .EQ. 2) .AND. (VOLCOD .NE. 3) ) THEN
C         internal only - skip all but movrow
          SKIPFG= 1
        ELSE
C         do instruction
          SKIPFG= 0
        END IF
C
        IF (SKIPFG .EQ. 0) THEN
C         get a t.s.
C
          IF (VOLCOD .LT. 0) THEN
C           wdm file
            VOLUME= 5
            WDMIDX= -VOLCOD
          ELSE
C           volume code is unchanged
            VOLUME= VOLCOD
          END IF
C
C         case entry
          GO TO (10,60,20,30,40,50,60), VOLUME
C
C         case sequential
 10       CONTINUE
            CALL GETSEQ (DELT,WIDTH)
            GO TO 70
C
C         case inpad
 20       CONTINUE
            CALL MOVROW (FILE,VOPADR(1),STIVL,WIDTH)
            GO TO 70
C
C         case tss
 30       CONTINUE
            CALL GETTSS (DELT,WIDTH)
            GO TO 70
C
C         case wdms
 40       CONTINUE
            CALL GETWDM (DELT,WIDTH,WDMSFL(WDMIDX),FSTCAL)
            GO TO 70
C
C         case dss
 50       CONTINUE
            CALL GETDSS (DELT,WIDTH,FSTCAL)
            GO TO 70
C
C         case error
 60       CONTINUE
            CALL FDATIM (INPSTR,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (VOLCOD)
            CALL OMSTI (I)
            SGRP = 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
C
C         endcase
 70       CONTINUE
C
          WORDI  = DELT
          INPSTR = INPSTR + WORDI*(WIDTH-1)
          IF (TESTFG .NE. 0) THEN
            GPFLAG = 1
            CALL GPDUMP(I,GPFLAG,WIDTH,DELT)
          END IF
C
          CALL PUTTSI (I220,I,INSTR)
C
        END IF
C
 80   CONTINUE
C
      RETURN
      END
C
C     4.3
C
      SUBROUTINE   TSPUT
     I                   (FILES,TSPKST,TSPKND,DELT,STIVL,WID,FSTCAL,
     I                    LSTCAL,EXTFG)
C
C     + + + PURPOSE + + +
C     Put a time series segment from the inpad
C     onto tss, sequential file, dss, wdm file, expad or inpad
C
C     + + + HISTORY + + +
C     12/6/2004 - jlk&pbd - changed I200 to I220 for new
C       TSGET/TSPUT file length
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FILES(15),TSPKST,TSPKND,DELT,STIVL,WID,FSTCAL,LSTCAL,
     #          EXTFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILES  - ???
C     TSPKST - ???
C     TSPKND - ???
C     DELT   - simulation time interval in minutes
C     STIVL  - index of first interval in INSPAN
C     WID    - number of intervals being simulated
C     FSTCAL - flag indicating first interval of run
C     LSTCAL - flag indicating last interval of run
C     EXTFG  - flag indicating which instructions to put:
C              0 - all
C              1 - external only
C              2 - internal only
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLAG,I,WIDTH,WORDI,WDMSFL(4),SCLU,SGRP,DATIM(5),I220,
     #          I3,I2,DUMIN(3),SKIPFG,WDMIDX
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (DUMIN,RDUMIN)
      REAL         RDUMIN(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  GPDUMP,MOVROW,PUTTSS,PUTDSS,GETTSI,PUTTSI
      EXTERNAL  PUTWDM,OMSG,OMSTD,OMSTI,FDATIM
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU   = 231
      BLEN   = 545
      KNT    = 0
      WIDTH= WID +1
      I2     = 2
      I3     = 3
      I220   = 220
C
C     set the local file numbers
      MESSU  = FILES(1)
      TSSFL  = FILES(10)
      DO 5 I= 1, 4
        WDMSFL(I)= FILES(10+I)
 5    CONTINUE
      MSGFL  = FILES(15)
C
C     read the undefined value and the error offsets
C     from the second record of the instruction file
C
      CALL GETTSI (I3,I2,
     O             DUMIN)
      TESTFG= DUMIN(1)
      RECLT= DUMIN(2)
      UNDEF= RDUMIN(3)
C
C     process the instructions
      DO 10 I = TSPKST,TSPKND
        CALL GETTSI (I220,I,
     O               INSTR)
        IF (TESTFG.NE.0) THEN
          GPFLAG = 2
          CALL GPDUMP (I,GPFLAG,WIDTH,DELT)
        END IF
C
        IF ( (EXTFG .EQ. 1) .AND. (VOLCOD .EQ. 3) ) THEN
C         external only - skip movrow
          SKIPFG= 1
        ELSE IF ( (EXTFG .EQ. 2) .AND. (VOLCOD .NE. 3) ) THEN
C         internal only - skip all but movrow
          SKIPFG= 1
        ELSE
C         do instruction
          SKIPFG= 0
        END IF
C
        IF (SKIPFG .EQ. 0) THEN
C         put a t.s. entry
          IF (VOLCOD .EQ. 3) THEN
C           pad to pad
            CALL MOVROW (FILE,VOPADR(1),STIVL,WIDTH)
          ELSE IF (VOLCOD .EQ. 4) THEN
C           put in tss file
            CALL PUTTSS (FSTCAL,LSTCAL,DELT,WIDTH)
          ELSE IF (VOLCOD .LT. 0) THEN
C           put in wdm file
            WDMIDX= -VOLCOD
            CALL PUTWDM (DELT,WIDTH,WDMSFL(WDMIDX),FSTCAL,LSTCAL)
          ELSE IF (VOLCOD .EQ. 6) THEN
C           put in dss file
            CALL PUTDSS (DELT,WIDTH,FSTCAL)
          ELSE
C           program bug
C           invalid option - not yet supported
            CALL FDATIM (FRMTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            SGRP = 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
          END IF
C
C         update inpad start time for next inpad
          WORDI  = DELT
          INPSTR = INPSTR + WORDI*(WIDTH-1)
C
          IF (TESTFG.NE.0) THEN
            GPFLAG = 2
            CALL GPDUMP(I,GPFLAG,WIDTH,DELT)
          END IF
C
C         output the instruction
          CALL PUTTSI (I220,I,INSTR)
C
        END IF
C
 10   CONTINUE
C
      RETURN
      END
