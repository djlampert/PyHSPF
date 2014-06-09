C
C     4.1.06
C
      SUBROUTINE GPDUMP
     I                 (KY,GPFLAG,WIDTH,DELT)
C
C     + + + PURPOSE + + +
C     Dump instruction for debugging
C
C     + + + HISTORY + + +
C     jlk&pbd 12/7/2004 - added new info for changing an
C                         mfact during a run
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,GPFLAG,KY,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KY     - ???
C     GPFLAG - ???
C     WIDTH  - ???
C     DELT   - simulation time interval in minutes
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
      INTEGER   J
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(1X,'TSGET INSTRUCTION AT KEY=',3I5)
 2010 FORMAT(1X,'TSPUT INSTRUCTION AT KEY=',3I5)
 2020 FORMAT(1X,10I10,/,1X,4I10,1PE10.3,5I10,/,1X,10I10,
     $       /1X,3I10)
 2030 FORMAT(1X,'VOPADR',20I6)
 2040 FORMAT(1X,'OFFSET',20I6)
 2050 FORMAT(1X,'STKIND',20I6)
 2060 FORMAT(1X,'STTRAN',20I6)
 2070 FORMAT(1X,'INMODE',20I6)
 2080 FORMAT(1X,'A     ',10(1PE12.5),/,1X,6X,10(1PE12.5))
 2090 FORMAT(1X,'B     ',10(1PE12.5),/,1X,6X,10(1PE12.5))
 2100 FORMAT(1X,'PVAR  ',10(1PE12.5),/,1X,6X,10(1PE12.5))
 2105 FORMAT(1X,'ARATIO',1PE12.5)
 2108 FORMAT(1X,A4,1X,A4,A2,I5,2(1X,A4,A2),2I5)
 2110 FORMAT(1X,'FORMAT',19A4)
 2120 FORMAT(1X,'PATH',19A4,/,1X,'AGGVAL',1PE12.5)
C
C     + + + END SPECIFICATIONS + + +
C
C     Write headings
C
      IF (GPFLAG.NE.1) GO TO 10
        WRITE(MESSU,2000) KY,WIDTH,DELT
        GO TO 20
 10   CONTINUE
        WRITE(MESSU,2010) KY,WIDTH,DELT
 20   CONTINUE
C
C     write the instruction
C
      WRITE(MESSU,2020) AMODE,NCOMPS,VOLCOD,FILE,DELTAT,
     $      FREC,LREC,COMPR,TOTCOM,FMTCLS,BEGYR,
     $      TENDR,LGAP,TGAP,GAPVAL,LTRNFG,VOTSB,
     $      VOLFRM,VOTFRM,BCWBTI,BCWNOV,INPSTR,
     $      YEAR,MO,DAY,LPYRFG,NREM,TYREND,
     $      FRMTIM,TZERO,TLAST,UCNT,ZCNT
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
C       seq
        WRITE(MESSU,2110) (PVAR(J),J=1,19)
      ELSE IF (VOLCOD .EQ. 4) THEN
C       dss
        WRITE(MESSU,2120) (PVAR(J),J=1,20)
      ELSE
C       tss or wdm
        WRITE(MESSU,2100) (PVAR(J),J=1,NCOMPS)
      END IF
C
      WRITE(MESSU,2105) ARATIO
      WRITE(MESSU,2108) "SRC:",SVOL,SNUM,SGRPN,SMEMN,SMEMSB
      WRITE(MESSU,2108) "TAR:",TVOL,TNUM,TGRPN,TMEMN,TMEMSB
C
      RETURN
      END
C
C     4.1.05
C
      SUBROUTINE INMOD
     I                 (PLACE)
C
C     + + + PURPOSE + + +
C     Place workspace values at xvar onto pad according to the
C     proper mode
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PLACE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLACE  - ???
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
      INTEGER   I,BRBFG,BRBPLA
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING INMOD')
 2010 FORMAT (3X, 'PLACE ACROSS ROW=',I12)
 2020 FORMAT (7(3X,E16.7),/)
 2030 FORMAT (' TSS PLACE, PAD(PLACE):  ',I10,E12.4)
C
C     + + + END SPECIFICATIONS + + +
C
C     Inmode off = place new values on pad
C     Inmode on  = add new values to current pad values
C
C     Place is interval across pad row
C
      BRBFG= 0
      IF (TESTFG .LT.2) GO TO 20
        WRITE (MESSU,2000)
        WRITE (MESSU,2010) PLACE
 20   CONTINUE
C
      DO 80 I= 1,NCOMPS
        IF (INMODE(I) .EQ. 1) GO TO 40
C         put values on pad
          PAD(VOPADR(I)+ PLACE)= XVAR(I)
          GO TO 60
 40     CONTINUE
C         add values to current pad values
          PAD(VOPADR(I)+ PLACE)= XVAR(I)+
     $    PAD(VOPADR(I)+ PLACE)
 60     CONTINUE
         BRBPLA= VOPADR(I) + PLACE
       IF (BRBFG.GE.2) WRITE (MESSU,2030) BRBPLA,PAD(BRBPLA)
 80   CONTINUE
C
      IF (TESTFG .GE. 3) WRITE (MESSU,2020) (PAD(VOPADR(I)+
     $        PLACE),I=1,NCOMPS)
C
      RETURN
      END
C
C     4.1.04
C
      SUBROUTINE LTRAN
     I                (N,A,B,XVAR)
C
C     + + + PURPOSE + + +
C     Linear transformation of xvar using a and b
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N
      REAL      A(N),B(N),XVAR(N)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - ???
C     A      - multiplying factor for the linear transform A*X + B
C     B      - shift value for the linear transform
C     XVAR   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      FAC
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      DO 30 I=1,N
        FAC=A(I)
C       if (fac .ne. 1.0) go to 10
        IF ((ABS(FAC-1.0)).GT.1.0E-5) GO TO 10
          XVAR(I)= XVAR(I) +B(I)
          GO TO 20
   10   CONTINUE
          XVAR(I)=FAC*XVAR(I) +B(I)
   20   CONTINUE
   30 CONTINUE
      RETURN
      END
C
C     4.1.08
C
      SUBROUTINE MOVROW
     I                  (SROW,TROW,STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     Move a row of the pad to a target row on the pad.
C     kind of data may be changed and the elements of the source
C     row may be added to the elements of the target row.
C     Note: statements with numbers from 800-999 are machine
C     dependent and may have to be altered.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SROW,TROW,STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SROW   - ???
C     TROW   - ???
C     STIVL  - ???
C     WIDTH  - ???
C
C     + + + COMMON BLOCKS- GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ENDROW,I,I4,STROW,SCLU,SGRP,DATIM,START
      REAL      Y
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSG,OMSTI,OMSTD,FDATIM
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING MOVROW')
 2010 FORMAT(3X, 'SOURCE/TARGET AND TARGET/SOURCE ADDRESSES=',2I14,
     $         3X,'STKIND=',I6,3X,'STTRAN=',I6,/,3X,'A + B=',2E16.7,3X,
     $         'INMODE (0=OFF,1=ADD TO INPAD)=',I6)
 2020 FORMAT(3X,'INPAD SOURCE ROW:',(/ ,10(1X,1PE11.5)))
 2030 FORMAT(3X,'INPAD TARGET ROW:',(/ ,10(1X,1PE11.5)))
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 237
      IF (TESTFG .LT. 3) GO TO 50
        WRITE (MESSU,2000)
        WRITE (MESSU,2010) SROW,TROW,STKIND(1),STTRAN(1),A(1),B(1),
     $                 INMODE(1)
C
        STROW = SROW+ STIVL
        ENDROW= SROW+ WIDTH
        WRITE (MESSU,2020) (PAD(I),I= STROW,ENDROW)
C
        STROW = TROW+ STIVL
        ENDROW= TROW+ WIDTH
        WRITE (MESSU,2030) (PAD(I),I= STROW,ENDROW)
 50   CONTINUE
C
C     Case entry stkind
      I4=STKIND(1)
      GO TO (60,140,220,390), I4
C
C     Case 1
 60   CONTINUE
C       * point to point data *
C
        IF (STIVL .EQ. 1) THEN
C         need to move initial value
          START= STIVL
        ELSE
C         no initial value
          START= STIVL+ 1
        END IF
C
        DO 120 I= START, STIVL+ WIDTH- 1
          Y= PAD(SROW+ I)
C
C         perform linear transformation
C         if (a(1) .eq. 1.0) go to 70
          IF ((ABS(A(1)-1.0)).LT.1.0E-5) GO TO 70
            Y=A(1)*Y
 70       CONTINUE
          Y= Y+B(1)
C
          IF (INMODE(1) .NE. 1) GO TO 80
            PAD(TROW+I)= Y+ PAD(TROW+I)
            GO TO 100
 80       CONTINUE
            PAD(TROW+I)= Y
 100      CONTINUE
 120    CONTINUE
        GO TO 400
C
C     Case 2
 140  CONTINUE
C       * mean to mean data *
C
        IF (STIVL .EQ. 1) THEN
C         put undefined value on inpad
          PAD(TROW+ 1)= UNDEF
        END IF
C
        DO 200 I=STIVL+ 1, STIVL+ WIDTH- 1
          Y= PAD(SROW+ I)
C
C         perform linear transformation
C         if (a(1) .eq. 1.0) go to 150
          IF ((ABS(A(1)-1.0)).LT.1.0E-5) GO TO 150
            Y=A(1)*Y
 150      CONTINUE
          Y= Y+B(1)
C
          IF (INMODE(1) .NE. 1) GO TO 160
            PAD(TROW+I)= Y+ PAD(TROW+I)
            GO TO 180
 160      CONTINUE
            PAD(TROW+I)= Y
 180      CONTINUE
 200    CONTINUE
        GO TO 400
C
C     Case 3
 220  CONTINUE
C       * point to mean data *
C
C       srow and trow may be identical
C
        IF (STIVL .EQ. 1) THEN
C         get first point
          PVAR(1)= PAD(SROW+STIVL)
C         put undefined value on inpad
          PAD(TROW+ 1)= UNDEF
        END IF
C
        DO 380 I= STIVL+ 1, STIVL+ WIDTH- 1
C
C         calculate average
          Y= (PVAR(1)+ PAD(SROW+I))/2.0
          PVAR(1)= PAD(SROW+ I)
C
C         perform linear transformation
C         if (a(1) .eq. 1.0) go to 355
          IF ((ABS(A(1)-1.0)).LT.1.0E-5) GO TO 355
            Y=A(1)*Y
 355      CONTINUE
          Y= Y+B(1)
C
          IF (INMODE(1) .NE. 1) GO TO 360
            PAD(TROW+I)= Y+ PAD(TROW+I)
            GO TO 370
 360      CONTINUE
            PAD(TROW+I)= Y
 370      CONTINUE
C
 380    CONTINUE
        GO TO 400
C
C     * error case *
 390  CONTINUE
        CALL FDATIM (INPSTR,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (STKIND(1))
        CALL OMSTI (I)
        CALL OMSTI (FILE)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
C
 400  CONTINUE
C     Endcase stkind
C
C
      IF (TESTFG .GE. 3) WRITE (MESSU,2030) (PAD(I),I=STROW,ENDROW)
C
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   VYDELT
     I                          (YEAR)
C
C     + + + PURPOSE + + +
C     Calculate number of minutes in a year, accounting for leap year
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  YEAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - year
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  LPYRFG,MIN365,MINDAY,MINS
C
C     + + + EXTERNALS + + +
      EXTERNAL LPYEAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MINDAY,MIN365/1440,525600/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL LPYEAR (YEAR,
     O             LPYRFG)
C
      MINS= MIN365
      IF (LPYRFG .EQ. 1) THEN
C       add leap day
        MINS= MINS+ MINDAY
      END IF
C
      VYDELT= MINS
C
      RETURN
      END
