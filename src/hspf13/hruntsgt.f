C
C
C
      SUBROUTINE   BCWUPD
     I                    (INPEND)
C
C     + + + PURPOSE + + +
C     Update the bcw and interpret the bcwbti (block type indicator)
C     and nov (number of values).  called from fillws when last time
C     frame in an tsb encountered, except when at end of inpad.
C     Note: statements numbered 800-999 are installation dependent
C     And may need to be modified
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     INPEND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPEND - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ADDR,WORD,WORDI,YR,YRI,SCLU,SGRP,DATIM(5)
      REAL        WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDI,WORDR,YRI)
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTSSRC,LPYEAR,BCWSPL,OMSTD,FDATIM,OMSG,OMSTI
C
C     + + + INTRINSICS + + +
      INTRINSIC   IABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING BCWUPD')
 2010 FORMAT(3X,'INPAD END TIME AND CURRENT FRAME TIME=',2I10)
 2020 FORMAT (3X,'NEW YEAR BEGUN:',I6)
 2030 FORMAT (3X,'NEW FRAME START AND END TIMES=',2I10)
 2040 FORMAT (3X, 'CURRENT YEAR BEING PROCESSED=',I6,
     $        3X, 'YEAR READ FROM TSS=',I6)
 2050 FORMAT (3X, 'WORD IN TBUFF=',I6)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 233
      IF (TESTFG .LT. 2) GO TO 20
        WRITE (MESSU,2000)
        WRITE (MESSU,2010) INPEND,FRMTIM
 20   CONTINUE
C
C     Determine address of new bcw
      VOTSB= VOTFRM+ TOTCOM
C
C     Get bcw
      ADDR  = VOTSB+ 1
      VOTFRM= ADDR
      IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 30
        CALL GTSSRC (ADDR)
 30   CONTINUE
      WORD= ADDR- VOBUFF
      IF (TESTFG .GE. 2) WRITE (MESSU,2050) WORD
C
      WORDR= TBUFF(WORD)
      BCW  = WORDI
C     Interpret new bcw
      IF (BCW .NE. 0) GO TO 60
C       end of chronological data
        IF ((INPEND .EQ. FRMTIM) .AND. (TENDR .EQ. FRMTIM)) GO TO 40
          CALL FDATIM (FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSTI (BCW)
          CALL OMSTI (INPEND)
          CALL OMSTI (FRMTIM)
          CALL OMSTI (TENDR)
          CALL OMSTI (FILE)
          SGRP = 1
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
 60   CONTINUE
C
      IF (BCW .GE. 0) GO TO 100
C       end of year
        VOTFRM=IABS(BCW)
        YEAR  = YEAR+ 1
        CALL LPYEAR(YEAR,  LPYRFG)
C
        IF (TESTFG .GE. 1) WRITE (MESSU,2020) YEAR
C
C       read first word of year
C       get year at votfrm+ 1
        ADDR= VOTFRM+ 1
        IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 70
          CALL GTSSRC (ADDR)
 70     CONTINUE
        WORD= ADDR- VOBUFF
        IF (TESTFG .GE. 2) WRITE (MESSU,2050) WORD
C
        WORDR= TBUFF(WORD)
        YR   = YRI
C       check agreement between year and year in first word
        IF (YEAR .EQ. YR) GO TO 80
          IF (TESTFG .GE. 2) WRITE(MESSU,2040) YEAR,YR
          CALL FDATIM (FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSTI (YEAR)
          CALL OMSTI (YR)
          CALL OMSTI (FILE)
          SGRP = 2
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 80     CONTINUE
C
C       get new bcw
C       bcw should be > 0
C       increment from virtual origin of year to bcw
        VOTFRM= VOTFRM+ 2
        VOTSB = VOTFRM- 1
C       get bcw at votfrm
        ADDR  = VOTFRM
        IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 90
          CALL GTSSRC (ADDR)
 90     CONTINUE
        WORD= ADDR- VOBUFF
        IF (TESTFG .GE. 2) WRITE (MESSU,2050) WORD
C
        WORDR= TBUFF(WORD)
        BCW  = WORDI
 100  CONTINUE
C
      IF (BCW .LE. 0) GO TO 110
C
C       split up bcw
        CALL BCWSPL(WORDI,     BCWBTI,BCWNOV)
        TZERO = TLAST
        WORDI = BCWNOV
        TLAST = TZERO+ (WORDI-1)*DELTAT
C
        IF (TESTFG .GE. 2) WRITE (MESSU,2030) TZERO,TLAST
C
        FRMTIM= TZERO
 110  CONTINUE
C
      RETURN
      END
C
C     4.1.1.2
C
      SUBROUTINE FILLWS
     I                  (INPEND)
C
C     + + + PURPOSE + + +
C     Fill workspace with one time frame by moving data values from
C     tss buffer for each component.
C
C     Workspace is an area consisting of six vectors: xold, xvar,
C     xnew, xtrn, xzero, and xlast.
C       xold is the source point value immediately preceding the
C     time point of interest
C       xnew is the source value at or immediately following the
C     time point of interest
C       xvar is the current state from agg/disagg of the target
C     value being constructed
C       xtrn is used for linear transformation input or output
C       xzero contains the xo values from a tsb for linear compressed
C     data
C       xlast contains the last values from a tsb for compressed
C     data
C
C     There will be a time field for each of xold,xnew, xzero, and
C     xlast.  this field contains the minute of the
C     calendar year.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INPEND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPEND - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
 
C     + + + LOCAL VARIABLES + + +
      INTEGER   ADDR,I,WORD,SCLU,SGRP,DATIM(5)
      REAL      OFFST,WID
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTSSRC,OMSTD,FDATIM,OMSG,OMSTI,BCWUPD
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING FILLWS')
 2020 FORMAT (3X, 'TXNEW (TIME OF CURRENT VALUE IN WORKSPACE=',
     $         I10,3X, 'FRMTIM (TIME OF CURRENT FRAME)=',I10)
 2030 FORMAT (3X,'LINEAR VARIATION COMPRESSION:     XZERO VALUES',/,
     $         6X,(8E16.7))
 2040 FORMAT (6X, 'XLAST VALUES:',/,6X, (8E16.7))
 2050 FORMAT (3X, 'XNEW (CURRENT VALUES FROM TSS):',/,
     $         6X, (8E16.7))
 2060 FORMAT(3X,'WORD IN TBUFF=',I6)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 233
      IF (TESTFG .LT. 2) GO TO 20
        WRITE (MESSU,2000)
        WRITE (MESSU,2020) TXNEW,FRMTIM
 20   CONTINUE
C
      UNDEF= -1.0E30
C
C     Move xnew values to xold in workspace
      DO 30 I= 1,NCOMPS
        XOLD(I)= XNEW(I)
 30   CONTINUE
C     Following line commented out to fix interpolate bug
CBRB  Txold= txnew
C     Following line added to fix interpolate bug
      IF (TXOLD.LT.0) TXOLD= 0
C
C     Case entry bcwbti
      GO TO (40,140,140,300,420),BCWBTI
C
C     Case uncompressed
 40   CONTINUE
        IF (TXNEW .LE. FRMTIM) GO TO 60
          VOTFRM= VOTFRM+ TOTCOM
          FRMTIM= FRMTIM+ DELTAT
          GO TO 100
 60     CONTINUE
          IF (TXNEW .GE. FRMTIM) GO TO 80
            VOTFRM= VOTFRM- TOTCOM
            FRMTIM= FRMTIM- DELTAT
 80       CONTINUE
 100    CONTINUE
C
        DO 130 I= 1,NCOMPS
C         get value at votfrm+offset(i)
          ADDR= VOTFRM+ OFFSET(I)
          IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 110
            CALL GTSSRC(ADDR)
 110      CONTINUE
          WORD= ADDR- VOBUFF
C
          IF (TESTFG .GE. 2) WRITE(MESSU,2060) WORD
C
C         put value in workspace at xnew(i)
          XNEW(I)= TBUFF(WORD)
C
C         check suitability undefined values not allowed
C         if (xnew(i) .ne. undef) go to 120
          IF ((ABS(XNEW(I)-UNDEF)).GT.1.0E-5) GO TO 120
C           set flag off when undefined value encountered
C           except for first mean value on inpad
            IF (I .EQ. 1 .AND. STKIND(I) .EQ. 2) GO TO 115
              SUITFG= 0
 115        CONTINUE
 120      CONTINUE
 130    CONTINUE
C
        GO TO 440
C
C     Case compressed
 140  CONTINUE
C       votfrm is correct. no update needed.
        FRMTIM= TXNEW
C
C       transfer zeroes or undefined ncomps times unless at tlast
        IF (TXNEW .NE. TLAST) GO TO 200
          DO 180 I= 1,NCOMPS
C           get value at votfrm+offset(i)
            ADDR= VOTFRM+ OFFSET(I)
            IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 150
              CALL GTSSRC(ADDR)
 150        CONTINUE
            WORD= ADDR- VOBUFF
C
            IF (TESTFG .GE. 2) WRITE(MESSU,2060) WORD
C
C           put value in workspace at xnew(i)
            XNEW(I)= TBUFF(WORD)
C
C           check suitability
C           if (xnew(i) .ne. undef) go to 160
            IF ((ABS(XNEW(I)-UNDEF)).GT.1.0E-5) GO TO 160
C             set flag off when undefined value encountered
C             except for first mean value on inpad
              IF (I .EQ. 1 .AND. STKIND(I) .EQ. 2) GO TO 155
                SUITFG= 0
 155          CONTINUE
 160        CONTINUE
 180      CONTINUE
C
          GO TO 280
C
 200    CONTINUE
C
          DO 260 I=1,NCOMPS
C           put undefined value or zero in workspace at xnew(i)
            IF (BCWBTI .EQ. 3) GO TO 220
C             zero compression
              XNEW(I)= 0.0
              GO TO 240
 220        CONTINUE
C             undefined compression
              XNEW(I)= UNDEF
C             set flag off when undefined value encountered
C             except for first mean value on inpad
              IF (I .EQ. 1 .AND. STKIND(I) .EQ. 2) GO TO 230
                SUITFG= 0
 230          CONTINUE
 240        CONTINUE
C
 260      CONTINUE
 280    CONTINUE
C
        GO TO 440
C
C     Case linear variation
 300  CONTINUE
        FRMTIM= TXNEW
C
        DO 340 I=1,NCOMPS
C         get value at frame votfrm+offset(i)
          ADDR= VOTFRM+ OFFSET(I)
          IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 310
            CALL GTSSRC(ADDR)
 310      CONTINUE
          WORD= ADDR- VOBUFF
C
          IF (TESTFG .GE. 2) WRITE(MESSU,2060) WORD
C
C         put value into xzero(i)
          XZERO(I)= TBUFF(WORD)
C
C         check suitability
C         if (xzero(i) .ne. undef) go to 320
          IF ((ABS(XZERO(I)-UNDEF)).GT.1.0E-5) GO TO 320
C           set flag off when undefined value encountered
C           except for first mean value on inpad
            IF (I .EQ. 1 .AND. STKIND(I) .EQ. 2) GO TO 315
              SUITFG= 0
 315        CONTINUE
 320      CONTINUE
 340    CONTINUE
C
        DO 380 I= 1,NCOMPS
          VOTFRM= VOTFRM+ TOTCOM
C         get values at frame votfrm+offset(i)
          ADDR= VOTFRM+ OFFSET(I)
          IF ((ADDR .GT. VOBUFF) .AND. (ADDR .LE. ENDBUF)) GO TO 350
            CALL GTSSRC(ADDR)
 350      CONTINUE
          WORD= ADDR- VOBUFF
C
          IF (TESTFG .GE. 2) WRITE(MESSU,2060) WORD
C
C         put value into xlast(i)
          XLAST(I)= TBUFF(WORD)
C
C         check suitability
C         if (xlast(i) .ne. undef) go to 360
          IF ((ABS(XLAST(I)-UNDEF)).GT.1.0E-5) GO TO 360
C           set flag off when undefined value encountered
C           except for first mean value on inpad
            IF (I .EQ. 1 .AND. STKIND(I) .EQ. 2) GO TO 355
              SUITFG= 0
 355        CONTINUE
 360      CONTINUE
 380    CONTINUE
C
        IF (TESTFG .LT. 2) GO TO 390
          WRITE(MESSU,2030) (XZERO(I),I= 1,NCOMPS)
          WRITE (MESSU,2040) (XLAST(I),I= 1,NCOMPS)
 390    CONTINUE
C
        DO 400 I= 1,NCOMPS
C         interpolate for value using tzero and tlast
          OFFST  = FRMTIM- TZERO
          WID    = TLAST- TZERO
          XNEW(I)= (XZERO(I)*(WID-OFFST) + XLAST(I)*OFFST)/WID
 400    CONTINUE
C
        GO TO 440
C
C     Case error
 420  CONTINUE
        CALL FDATIM (FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSTI (BCWBTI)
        CALL OMSTI (FILE)
        SGRP = 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
C
C     Endcase
 440  CONTINUE
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2050) (XNEW(I),I=1,NCOMPS)
C
C     Interpret next bcw as needed
      IF ((FRMTIM .NE. TLAST) .OR. (FRMTIM .GT. INPEND))
     $        GO TO 460
        CALL BCWUPD (INPEND)
 460  CONTINUE
C
      RETURN
      END
C
C     4.1.1
C
      SUBROUTINE GETTSS
     I                  (DELT, WIDTH)
C
C     + + + PURPOSE + + +
C     Get values from tss. transform them and fill a row on
C     the inpad for each component of this dataset
C
C     NOTE: Statements with numbers 800-900 are installation
C     dependent and may need to be modified
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DELT,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - ???
C     WIDTH  - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ADDR,I,I4,INPEND,INPTIM,SPLIT(1)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (SPLIT,WORDI),(SPLIT,WORDR)
      REAL         WORDR
      INTEGER      WORDI
C
C     + + + EXTERNALS + + +
      EXTERNAL     GTSSRC,FILLWS,TFUNE,LTRAN,INMOD,TFUNG,XVINIT,TFUNL
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING GETTSS')
 2010 FORMAT (3X,'INPAD DELTA TIME=',I6,3X, 'TSS DELTAT TIME=',I6,
     $         3X, 'NO. OF COMPONENTS=',I6,3X, 'WIDTH=',I12,3X,
     $         'INPAD START TIME=',I10,/,3X,
     $         'TIME ZERO AND LAST (BEGINNING ',
     $         'AND END OF TIME FRAME)=',2I10)
 2030 FORMAT (3X,'INPAD END TIME=',F10.2,3X,
     $         'TXNEW (TIME OF CURRENT VALUE IN WORK SPACE)=',F10.2)
 2040 FORMAT (2X, 'COMPONENT',5X, 'VOPADR  OFFSET  STKIND  STTRAN  '
     $         ,'INMODE',17X,'A',17X,'B')
 2050 FORMAT (3X,I6,2X,I12,4(2X,I6),2E18.7)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .LT. 1) GO TO 20
        WRITE (MESSU,2000)
        WRITE (MESSU,2010) DELT,DELTAT,NCOMPS,WIDTH,
     $                     INPSTR,TZERO,TLAST
        WRITE (MESSU,2040)
        DO 10 I= 1,NCOMPS
          WRITE (MESSU,2050) I,VOPADR(I),OFFSET(I), STKIND(I),
     $                       STTRAN(I),INMODE(I), A(I),B(I)
 10     CONTINUE
 20   CONTINUE
C
C     Read current bcw
      ADDR= VOTSB+ 1
      CALL GTSSRC (ADDR)
C
C     Initialize inpad time to start time for inpad
      INPTIM= INPSTR
C     Calculate end time of inpad
      WORDI = WIDTH
      INPEND= (WORDI-1)*DELT+ INPSTR
C
C     Initialize workspace
      IF (INPSTR .NE. FRMTIM) GO TO 60
C       set xnew to zero to prevent computational problems
C       with point to mean transforms
        DO 40 I= 1,NCOMPS
          XNEW(I)= 0.0
 40     CONTINUE
        TXNEW= INPSTR- DELTAT
 60   CONTINUE
C
      IF (INPSTR .GE. FRMTIM) GO TO 80
C       only when deltat > delt case
        TXNEW= FRMTIM- DELTAT
C
C       fill xnew with values from previous frame
C       following line added to fix interpolate bug
        TXOLD= TXNEW
        CALL FILLWS (INPEND)
 80   CONTINUE
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2030) INPEND,TXNEW
C
      IF (DELTAT .NE. DELT) GO TO 120
C       tss time step=inpad time step
C
        DO 100 I= 1,WIDTH
          TXOLD= TXNEW
          TXNEW= INPTIM
C
          CALL FILLWS (INPEND)
          CALL TFUNE
C
C         perform linear transformation
          IF (LTRNFG .NE. 1) GO TO 90
            CALL LTRAN(NCOMPS,A,B,   XVAR)
 90       CONTINUE
C
C         insert value on inpad
          CALL INMOD (I)
C
          INPTIM= INPTIM+ DELT
 100    CONTINUE
 120  CONTINUE
C
      IF (DELTAT .LE. DELT) GO TO 220
C       tss time step>inpad time step
C
        RATIO = DELTAT/DELT
        I4    =RATIO
        RRATIO= FLOAT (I4)
        DO 200 I= 1,WIDTH
C         make sure proper values are in workspace
          IF (INPTIM .LE.TXNEW) GO TO 140
            TXOLD= TXNEW
            TXNEW= TXNEW+ DELTAT
            CALL FILLWS (INPEND)
 140      CONTINUE
C
          CALL TFUNG(INPTIM,DELTAT)
C
C         perform linear transformation
          IF (LTRNFG .NE. 1) GO TO 160
            CALL LTRAN(NCOMPS,A,B,   XVAR)
 160      CONTINUE
C
C         insert value on inpad
          CALL INMOD (I)
C
          INPTIM= INPTIM+ DELT
 200    CONTINUE
 220  CONTINUE
C
      IF (DELTAT .GE.  DELT) GO TO 300
C       tss time step< inpad time step
C
        RATIO = DELT/DELTAT
        I4    =RATIO
        RRATIO= FLOAT (I4)
C       initialize the xvar values
        CALL XVINIT (NCOMPS,FILE,FRMTIM,MESSU,
     $               STTRAN,TYREND,YEAR,MSGFL,TESTFG,KNT,XVAR)
C
        DO 280 I= 1,WIDTH
C         dountil txnew=inptim
 240      CONTINUE
          IF (TXNEW .EQ. INPTIM) GO TO 260
            TXOLD= TXNEW
            TXNEW= TXNEW+ DELTAT
C
            CALL FILLWS (INPEND)
            CALL TFUNL(INPTIM)
            GO TO 240
 260      CONTINUE
C
C         linear transformation
          IF (LTRNFG .NE. 1) GO TO 270
            CALL LTRAN(NCOMPS,A,B,   XVAR)
 270      CONTINUE
C
C         place values in xvar into inpad
          CALL INMOD (I)
C
          INPTIM= INPTIM+ DELT
C
C         reset the xvar values
          CALL XVINIT (NCOMPS,FILE,FRMTIM,MESSU,
     $                 STTRAN,TYREND,YEAR,MSGFL,TESTFG,KNT,XVAR)
C
 280    CONTINUE
 300  CONTINUE
C
C
      RETURN
      END
C
C     4.1.1.1
C
      SUBROUTINE GTSSRC
     I                  (ADDR)
C
C     + + + PURPOSE + + +
C     Get a tssfl record as needed and place in buffer.
C
C     Note: statements numbered 800-999 are installation dependent
C     And may need to be altered
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ADDR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADDR   - ???
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
      INTEGER   RECNO,SCLU,SGRP,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,FDATIM,OMSG,OMSTI,RBUFF
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING GTSSRC')
 2010 FORMAT (1X,'*', 'ADDRESS WANTED=',I12,3X, 'RECORD NO.=',I12,
     $        29X,'*')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 233
      IF (TESTFG .GE. 2) WRITE (MESSU,2000)
C
C     Vobuff is virtual origin of present contents of tbuff
C     Trcno is record number in tbuff
C
C     Addr is the desired address (not virtual origin)
C     Calculate new record number
      RECNO= (ADDR- 1)/RECLT+ FREC
C
C     Check record number in dataset
      IF ((RECNO .GE. FREC) .AND. (RECNO .LE. LREC)) GO TO 40
        CALL FDATIM (FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSTI (RECNO)
        CALL OMSTI (FREC)
        CALL OMSTI (LREC)
        CALL OMSTI (FILE)
        SGRP = 4
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
        WRITE (MESSU,2010) RECNO,FREC,LREC,FILE
 40   CONTINUE
C
      CALL RBUFF(RECNO,RECLT,TSSFL,     TBUFF)
      TRCNO = RECNO
      VOBUFF= (TRCNO- FREC)*RECLT
      ENDBUF= VOBUFF+ RECLT
C
      IF (TESTFG .GE. 3) WRITE (MESSU,2010) ADDR,TRCNO
C
      RETURN
      END
C
C     1.2.17
C
      SUBROUTINE   WBUFF
     I                 (REC,RECL,TSSFL,BUFF)
C
C     + + + PURPOSE + + +
C     Write contents of buff to rec-th record of file tssfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    REC,RECL,TSSFL
      REAL       BUFF(RECL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REC    - ???
C     RECL   - ???
C     TSSFL  - fortran unit number of time series store file
C     BUFF   - ???
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (TSSFL,REC=REC) BUFF
C
      RETURN
      END
