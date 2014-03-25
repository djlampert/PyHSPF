C
C
C
      SUBROUTINE CRTP
     I                (START,ENDR,MESSU,OUTLEV,
     O                 BEGYR,FMIN,LPYRFG,INPSTR,STIME,TYREND,TENDR)
C
C     + + + PURPOSE + + +
C     Compute critical time values for the tsget/tsput
C     instructions - these time points are a function of
C     start and endr and do not depend on the source/target
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   START(5),ENDR(5),MESSU,OUTLEV,BEGYR,LPYRFG,
     $          FMIN,INPSTR,STIME,TYREND,TENDR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     START  - ???
C     ENDR   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     OUTLEV - run interpreter output level
C     BEGYR  - year at start of run
C     FMIN   - ???
C     LPYRFG - ???
C     INPSTR - ???
C     STIME  - ???
C     TYREND - ???
C     TENDR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DIFF,I1,TEMP(5),YEAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   TDIF,YROFF,FIRINT,LPYEAR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' BEGIN COMPUTATION OF CRITICAL TIME VALUES')
 2010 FORMAT(' CRITICAL TIME VALUES:',8I10)
 2020 FORMAT(' END COMPUTATION OF CRITICAL TIME VALUES')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (OUTLEV .GT. 5) THEN
C       begin message
        WRITE(MESSU,2000)
      END IF
C
C     compute date/time of the first minute of the run
      CALL FIRINT(I1,START,
     O            TEMP)
C
C     compute time of first minute relative to start of
C     year containing the first minute
      CALL YROFF(TEMP,
     O           FMIN)
C
C     compute start time of run relative to year containing
C     first minute of the run
      STIME = FMIN- 1
      INPSTR= STIME
C     year containing first minute of the run
      BEGYR = TEMP(1)
      YEAR  = BEGYR
C     set the leap year flag
      CALL LPYEAR(YEAR,
     O            LPYRFG)
C     set time of year end
      IF (LPYRFG .EQ. 1) THEN
C       leap year
        TYREND = 527040
      ELSE
C       not leap year
        TYREND = 525600
      END IF
C
C     compute time of end of run
      CALL TDIF(BEGYR,ENDR(1),ENDR(2),ENDR(3),      DIFF)
C     add in the time offset within the last day of the run
      TENDR = DIFF+ (ENDR(4)-1)*60 + ENDR(5)
C
      IF (OUTLEV .GT. 5) THEN
C       end message
        WRITE(MESSU,2010) BEGYR,FMIN,LPYRFG,INPSTR,STIME,
     $    TYREND,TENDR
        WRITE(MESSU,2020)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TINOUT
     I                   (INKEY,GETF)
C
C     + + + PURPOSE + + +
C     Output a tsget/tsput instruction to echo file
C
C     + + + HISTORY + + +
C     jlk&pbd 12/7/2004 - added new info for changing an
C                         mfact during a run
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INKEY,GETF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INKEY  - ???
C     GETF   - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J 
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' TSGET INSTRUCTION AT KEY=',I5)
 2010 FORMAT (' TSPUT INSTRUCTION AT KEY=',I5)
 2015 FORMAT (1X,5I10,2X,2A4,1PE10.3,I10)
 2020 FORMAT (1X,10I10,/1X,4I10,1PE10.3,5I10,/1X,10I10,
     #       /1X,3I10)
 2030 FORMAT (' VOPADR',20I6)
 2040 FORMAT (' OFFSET',20I6)
 2050 FORMAT(' STKIND',20I6)
 2060 FORMAT(' STTRAN',20I6)
 2070 FORMAT(' INMODE',20I6)
 2080 FORMAT(' A     ',10(1PE12.5),/1X,6X,10(1PE12.5))
 2090 FORMAT(' B     ',10(1PE12.5),/1X,6X,10(1PE12.5))
 2100 FORMAT(' PVAR  ',10(1PE12.5),/1X,6X,10(1PE12.5))
 2105 FORMAT(' ARATIO',1PE12.5)
 2108 FORMAT(1X,A4,1X,A4,A2,I5,2(1X,A4,A2),2I5)
 2110 FORMAT(' FORMAT',19A4)
 2120 FORMAT(' PATH  ',16A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (GETF .EQ. 1) THEN
C       get instruction
        WRITE(MESSU,2000) INKEY
      ELSE
C       put instruction
        WRITE(MESSU,2010) INKEY
      END IF
C
      IF (VOLCOD .EQ. 6) THEN
        WRITE (MESSU,2015)  AMODE,NCOMPS,VOLCOD,FILE,DELTAT,
     #                     (CTYPI(J),J= 1, 2),GAPVAL,LTRNFG
      ELSE
        WRITE (MESSU,2020) AMODE,NCOMPS,VOLCOD,FILE,DELTAT,
     #                     FREC,LREC,COMPR,TOTCOM,FMTCLS,BEGYR,
     #                     TENDR,LGAP,TGAP,GAPVAL,LTRNFG,VOTSB,
     #                     VOLFRM,VOTFRM,BCWBTI,BCWNOV,INPSTR,
     #                     YEAR,MO,DAY,LPYRFG,NREM,TYREND,
     #                     FRMTIM,TZERO,TLAST,UCNT,ZCNT
      END IF
C
      WRITE(MESSU,2030) (VOPADR(J),J=1,NCOMPS)
      WRITE(MESSU,2040) (OFFSET(J),J=1,NCOMPS)
      WRITE(MESSU,2050) (STKIND(J),J=1,NCOMPS)
      WRITE(MESSU,2060) (STTRAN(J),J=1,NCOMPS)
      WRITE(MESSU,2070) (INMODE(J),J=1,NCOMPS)
      WRITE(MESSU,2080) (A(J),J=1,NCOMPS)
      WRITE(MESSU,2090) (B(J),J=1,NCOMPS)
C
      IF (VOLCOD .EQ. 1) THEN
        WRITE(MESSU,2110) (PVAR(J),J=1,19)
      ELSE IF (VOLCOD .EQ. 6) THEN
        WRITE(MESSU,2120) (PVAR(J),J=1,16)
      ELSE
        WRITE(MESSU,2100) (PVAR(J),J=1,NCOMPS)
      END IF
C
      IF (SNUMX .NE. 0 .OR. TNUMX .NE. 0) THEN
C       supplemental info available
        WRITE(MESSU,2105) ARATIO
        WRITE(MESSU,2108) "SRC:",SVOLX,SNUMX,SGRPNX,SMEMN,SMEMSB
        WRITE(MESSU,2108) "TAR:",TVOLX,TNUMX,TGRPNX,TMEMN,TMEMSB
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TINSTR
     I                   (START,ENDR,OPST,OPND,
     M                    GPKEY)
C
C     + + + PURPOSE + + +
C     Generate the tsget/tsput instructions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   START(5),ENDR(5),OPST,OPND,GPKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     START  - ???
C     ENDR   - ???
C     OPST   - ???
C     OPND   - ???
C     GPKEY  - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DELT,FMIN,GETF,I,IGRP,SKY,STIME,EKY,VOPAD,WIDTH
C
C     + + + EXTERNALS + + +
      EXTERNAL  CRTP,FINSTR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' BEGIN GENERATION OF TSGET/TSPUT INSTRUCTIONS',
     $       ' FOR OPERATIONS ',I3,' THROUGH ',I3)
 2010 FORMAT(' END GENERATION OF TSGET/TSPUT INSTRUCTIONS',
     $       ' FOR OPERATIONS ',I3,' THROUGH ',I3)
C
C     + + + END SPECIFICATIONS + + +
C
C     set file numbers. filet is used to avoid conflict with
C     file in the instruction
      MESSU = FILET(1)
      TSSFL = FILET(10)
      MSGFL = FILET(15)
C
C     set max length for buffer--tbuff
      BLEN  = 545
C
      IF (OUTLEV .GT. 4) THEN
C       begin generation message
        WRITE(MESSU,2000) OPST,OPND
      END IF
C
C
C     initialize instr to avoid undefined values during debug
      DO 5 I= 1, 200
        INSTR(I)= 0
 5    CONTINUE
C
C     compute critical time points
      CALL CRTP(START,ENDR,MESSU,OUTLEV,
     O          BEGYR,FMIN,LPYRFG,INPSTR,STIME,TYREND,TENDR)
      YEAR= BEGYR
C
      DO 10 I= OPST,OPND
C       get the inpad characteristics for this operation
        IGRP = OPNTAB(6,I)
        DELT = GRPTAB(3,IGRP)
        VOPAD= GRPTAB(4,IGRP)
        WIDTH= GRPTAB(5,IGRP)
C
C       get starting and ending keys in the tsget for
C       the primitive tsget instructions for this operation
        SKY  = OPNTAB(17,I)
        EKY  = OPNTAB(18,I)
        GETF = 1
        IF (SKY .GT. 0) THEN
          OPNTAB(17,I) = GPKEY+ 1
          CALL FINSTR(SKY,EKY,GETF,VOPAD,
     I                WIDTH,START,ENDR,STIME,FMIN,DELT,
     M                GPKEY)
          OPNTAB(18,I) = GPKEY
        END IF
C
C       get starting and ending keys in the tsput for
C       the primitive tsput instructions for this operation
        SKY  = OPNTAB(19,I)
        EKY  = OPNTAB(20,I)
        GETF = 0
        IF (SKY .GT. 0) THEN
          OPNTAB(19,I) = GPKEY+ 1
          CALL FINSTR(SKY,EKY,GETF,VOPAD,
     I                WIDTH,START,ENDR,STIME,FMIN,DELT,
     M                GPKEY)
          OPNTAB(20,I) = GPKEY
        END IF
 10   CONTINUE
C
      IF (OUTLEV .GT. 4) THEN
C       done message
        WRITE(MESSU,2010) OPST,OPND
      END IF
C
      RETURN
      END
