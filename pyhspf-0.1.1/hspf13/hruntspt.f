C
C     4.3.1.3.1.1
C
      SUBROUTINE   GTWORD
     I                    (ADDR,      WORD)
C     + + + PURPOSE + + +
C     Get a word from the tss dataset at the given address.
C     current buffer must be valid and available for reuse
C     this subroutine is closely related to gword.
C     any bugs detected here may be present there.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ADDR,WORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADDR   - ???
C     WORD   - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     RECNO,WORDI,SCLU,SGRP,DATIM(5)
      REAL        WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDI,WORDR)
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTD,OMSTI,FDATIM,OMSG,RBUFF
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      IF (ADDR .GE. 1) GO TO 10
C       program bug
C       buffer underflow
        SGRP = 24
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 10   CONTINUE
C
      RECNO= (ADDR-1)/RECLT +FREC
      IF (RECNO .LE. LREC) GO TO 20
C       error
C       attempt to obtain data beyond upper limit of dataset
        SGRP = 25
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 20   CONTINUE
C
      IF (RECNO .EQ. TRCNO) GO TO 30
        CALL RBUFF(RECNO,RECLT,TSSFL,      TBUFF)
        TRCNO =RECNO
        VOBUFF=(TRCNO-FREC)*RECLT
        ENDBUF=VOBUFF +RECLT
        BMTFLG=0
        BADR  =1
 30   CONTINUE
C
C     get the word from the buffer
      WORDR=TBUFF(ADDR-VOBUFF)
      WORD =WORDI
C
      RETURN
      END
C
C     4.3.1.2
C
      SUBROUTINE   BUFINT
     I                    (VTSB,VTFRM)
C
C     + + + PURPOSE + + +
C     Initialize the buffer/extension from the tss
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   VTSB,VTFRM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VTSB   - ???
C     VTFRM  - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,LWORD,RECBCW,RECFRM,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  RBUFF,OMSTD,OMSTI,FDATIM,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
C     check the state of the buffer
C
      SCLU = 235
      IF (BMTFLG .NE.  1 ) GO TO 10
        BMTFLG= 0
        GO TO 20
 10   CONTINUE
C       program bug
C       invalid buffer state
        SGRP = 17
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 20   CONTINUE
C
      RECBCW=FREC +VTSB/RECLT
      RECFRM=FREC +VTFRM/RECLT
C
      IF (RECBCW .GE. FREC) GO TO 30
C       error
C       dataset underflow
        SGRP = 18
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
        GO TO 50
 30   CONTINUE
        IF (RECBCW .LE. LREC) GO TO 40
C         error
C         dataset overflow
          SGRP = 19
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
 50   CONTINUE
      IF (RECFRM .GE. FREC) GO TO 60
C       error
C       dataset underflow
        SGRP = 18
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
        GO TO 80
 60   CONTINUE
        IF (RECFRM .LE. LREC) GO TO 70
C         error
C         dataset overflow
          SGRP = 19
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 70     CONTINUE
 80   CONTINUE
C
C     finally fill the buffer/extension
C
      IF (RECBCW .NE. RECFRM) GO TO 90
        CALL RBUFF(RECBCW,RECLT,TSSFL,      TBUFF)
        VOBUFF=(RECBCW-FREC)*RECLT
        TRCNO =RECBCW
        BADR  =VTFRM-VOBUFF
        EXTF  = 0
        GO TO 150
 90   CONTINUE
        IF (RECBCW+1 .NE. RECFRM) GO TO 130
C         read extension
          CALL RBUFF(RECFRM,RECLT,TSSFL,      TBUFF)
          VOBUFF=(RECFRM-FREC)*RECLT
          TRCNO =RECFRM
C         transfer extension to proper point in tbuff
C
          LWORD=VTFRM -VOBUFF
          IF ((LWORD+RECLT+TOTCOM) .LE. BLEN) GO TO 100
C           program bug
C           buffer overflow
            SGRP = 20
            CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 100      CONTINUE
C     whiledo lword>0
 110      IF ((LWORD .LE. 0)) GO TO 120
            TBUFF(LWORD+RECLT)=TBUFF(LWORD)
            LWORD             =LWORD -1
            GO TO 110
 120      CONTINUE
C         read the buffer
          CALL RBUFF(RECBCW,RECLT,TSSFL,      TBUFF)
          VOBUFF=(RECBCW-FREC)*RECLT
          TRCNO =RECBCW
          BADR  =VTFRM -VOBUFF
          EXTF  = 1
          GO TO 140
 130    CONTINUE
C         program bug
C         buffer/extension does not contain the tsb
          SGRP = 21
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 140    CONTINUE
 150  CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.3.1
C
      SUBROUTINE   CLSTSB
C
C     + + + PURPOSE + + +
C     Close a tsb.
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     N,OFF(1),WORDI
      REAL        WORDR(1)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDI,WORDR(1))
C
C     + + + FUNCTIONS + + +
      INTEGER     BCWCAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    BCWCAL,PTVAL
C
C     + + + END SPECIFICATIONS + + +
C
      OFF(1) = 0
      FORFLG = 0
C
      IF (BCWBTI .NE. 2 .AND. BCWBTI .NE. 3) GO TO 10
C
C       write terminating frame for compressed block
C       this frame is already included in bcwnov
C
        CWF = 0
C
        CALL PTVAL(TOTCOM,OFF,XVAR,VOTFRM)
        VOLFRM = VOTFRM
C
        VOTFRM = VOTFRM + TOTCOM
C
 10   CONTINUE
C     write bcw for the tsb
C
C
      CWF   = 1
      N     = 1
      WORDI = BCWCAL(BCWBTI,BCWNOV)
C
      CALL PTVAL(N,OFF,WORDR,VOTSB)
C
      RETURN
      END
C
C     4.3.1.3
C
      SUBROUTINE   FILTSS
     I                    (COMP,NTS)
C
C     + + + PURPOSE + + +
C     Put xvar into the tss with all its ramifications.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   COMP,NTS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     COMP   - ???
C     NTS    - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BTI,DUMR4,SCLU,SGRP,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,OMSTI,FDATIM,OMSG,REPLC,XVEXP,FINIS,XVPUT,SQUISH
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      IF (TXVAR .EQ. FRMTIM) GO TO 10
C       program bug
C       txvar not= frmtim
        SGRP = 1
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 10   CONTINUE
C
      IF (AMODE .NE. 3) GO TO 20
C
        CALL REPLC(OFFSET,NTS)
C
        GO TO 190
 20   CONTINUE
C
C       add or insert amode
C
        IF (NTS .EQ. TOTCOM) GO TO 30
          CALL XVEXP(NTS,TOTCOM,GAPVAL,XVAR,OFFSET)
 30     CONTINUE
C
        GO TO (40,70,180), COMP
C
C         uncompressed
 40       CONTINUE
C           check for forced change in compression mode
C           forced change used to enable compressed
C            leading or trailing gaps in an otherwise
C            uncompressed dataset
            IF (BCWBTI .EQ. 1) GO TO 50
C             we can close the tsb because bcwbti not= 1
C              occurs iff bcwnov >= 2
              ZFLAG=0
              UFLAG=0
C             add values to the compressed tsb
C             finis writes the full frame when
C             it calls clstsb
              BCWNOV=BCWNOV+1
              TLAST =TLAST +DELTAT
              FRMTIM=FRMTIM +DELTAT
              CALL FINIS
              GO TO 60
 50         CONTINUE
              CALL XVPUT
              CALL FINIS
 60         CONTINUE
          GO TO 190
C
C         compressed
 70       CONTINUE
C
            GO TO (80,150,160,170), BCWBTI
C
C             uncompressed tsb
 80           CONTINUE
                CALL XVPUT
                IF (ZFLAG .NE.1) GO TO 100
                  UCNT  = 0
                  ZCNT  = ZCNT + 1
                  DUMR4 = ZCNT - 2
                  IF (DUMR4*TOTCOM - 2 .LE. 0) GO TO 90
C                   compress the tsb because we have
C                   enough consecutive zero frames
                    BTI = 2
                    CALL SQUISH(ZCNT,BTI)
 90               CONTINUE
                  GO TO 140
 100            CONTINUE
                  ZCNT = 0
                  IF (UFLAG .NE.1) GO TO 120
                    UCNT  = UCNT + 1
                    DUMR4 = UCNT - 2
                    IF (DUMR4*TOTCOM - 2 .LE. 0) GO TO 110
C                     compress the tsb because we have
C                     enough consecutive undefined frames
                      BTI = 3
                      CALL SQUISH(UCNT,BTI)
 110                CONTINUE
                    GO TO 130
 120              CONTINUE
                    UCNT = 0
 130              CONTINUE
 140            CONTINUE
                CALL FINIS
              GO TO 175
C
C             zero compressed tsb
 150          CONTINUE
C               add the value to the compressed tsb
                BCWNOV=BCWNOV +1
                TLAST =TLAST +DELTAT
                FRMTIM=FRMTIM +DELTAT
C               force closing of current tsb in finis
                IF (UFLAG .EQ. 1) ZFLAG = -1
                CALL FINIS
              GO TO 175
C
C             undefined compressed tsb
 160          CONTINUE
C               add the value to the compressed tsb
                BCWNOV=BCWNOV +1
                TLAST =TLAST +DELTAT
                FRMTIM=FRMTIM +DELTAT
C               force closing of current tsb in finis
                IF (ZFLAG .EQ. 1) UFLAG = -1
                CALL FINIS
              GO TO 175
C
C             linear variation
 170          CONTINUE
C               program bug
C               linear variation bti encountered when
C               such bti's are not yet supported
                SGRP = 2
                CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
                CALL OMSTD (DATIM)
                CALL OMSTI (FILE)
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     KNT)
C
 175          CONTINUE
            GO TO 190
C
C         linear variation
 180      CONTINUE
C           program bug
C           linear variation compression selected when
C           such compression is not yet supported
            SGRP = 3
            CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
        GO TO 190
 190  CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.3
C
      SUBROUTINE   FINIS
C
C     + + + PURPOSE + + +
C     Close tsbs as needed and update labels and control words.
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     N,OFF(1),SPLIT(1),WORDI,VONXYR
      REAL        WORDR(1)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDI,WORDR(1)), (WORDI,SPLIT(1))
C
C     + + + EXTERNALS + + +
      EXTERNAL    CLSTSB,PTVAL,UPDLAB,BUFINT,LPYEAR,NEWTSB
C
C     + + + INTRINSICS + + +
      INTRINSIC   IABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TLAST .NE. TYREND) GO TO 30
C       end of calendar year
C
        CALL CLSTSB
C
        IF (TLAST .LT. TENDR) GO TO 10
C         end of processing -- close year, update label
C
          BCW    = 0
          WORDI  = BCW
          N      = 1
          OFF(1) = 0
          FORFLG = 1
          VOTSB  = VOTFRM
          CALL PTVAL(N,OFF,WORDR,VOTSB)
          BCW    = VOTSB + 1
          CALL UPDLAB(BCW,RECLT,FREC,TSSFL,YEAR)
C
          GO TO 20
 10     CONTINUE
C
C         close the year, update label, and initiate new year
C
C         vo of final word for the year
          N=1
C
          OFF(1) = 0
          VOTSB  = VOTFRM
C         virtural origin of the next year
          BCW    = -(VOTSB+1)
          WORDI  = BCW
          FORFLG = 1
C
          CALL PTVAL(N,OFF,WORDR,VOTSB)
C
C         update the dataset label for the year just completed
C
          CALL UPDLAB(BCW,RECLT,FREC,TSSFL,YEAR)
C
C         reinitialize buffer -- compute virtual origin of the
C          next year
C
          VONXYR = IABS(BCW)
C
          CALL BUFINT(VONXYR,VONXYR)
C
C         update year and tyrend
C
          YEAR   = YEAR + 1
          TYREND = TYREND + 525600
C
          CALL LPYEAR(YEAR,   LPYRFG)
          IF (LPYRFG.EQ.1) TYREND = TYREND + 1440
C
C         initiate the new year -- note split(1) overlays bcw
C
          SPLIT(1) = YEAR
          FORFLG   = 0
          CWF      = 0
          N        = 1
          OFF(1)   = 0
C
          CALL PTVAL(N,OFF,WORDR,VONXYR)
C
C         initiate new tsb -- get votfrm for use by newtsb
C
          VOTFRM = VONXYR + 1
C
          CALL NEWTSB
C
C
 20     CONTINUE
C
        GO TO 80
 30   CONTINUE
C
C       not end of calendar year
C
        IF (BCWNOV .NE. 32767) GO TO 40
          CALL CLSTSB
          CALL NEWTSB
          GO TO 70
 40     CONTINUE
          IF (BCWNOV .LE. 1) GO TO 60
            IF (EXTF.NE.1) GO TO 50
              CALL CLSTSB
              CALL NEWTSB
              GO TO 59
 50         CONTINUE
              IF (BCWBTI.EQ.1) GO TO 57
                IF ((ZFLAG+UFLAG).NE.0) GO TO 55
                  CALL CLSTSB
                  CALL NEWTSB
 55             CONTINUE
 57           CONTINUE
 59          CONTINUE
 60       CONTINUE
 70     CONTINUE
C
 80   CONTINUE
C
      RETURN
      END
C
C     4.3.1.4
C
      SUBROUTINE   HFRAME
     I                    (DELT)
C
C     + + + PURPOSE + + +
C     Compute initial frame from inpad for puttss.
C     called if inpad start time matches a time point
C     represented in the dataset
C
C     + + + DUMMY ARGUMENT + + +
      INTEGER   DELT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- PUTCOM + + +
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
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
C
      TXVAR = INPSTR
C
      IF (DELT .NE. DELTAT) GO TO 30
C
        DO 20 J=1,NCOMPS
          XVAR(J) = GAPVAL
          IF (STKIND(J) .NE. 1) GO TO 10
C           point kinds -- same is only valid functional
            XVAR(J) = PAD(VOPADR(J)+1)
 10       CONTINUE
 20     CONTINUE
C
        GO TO 110
 30   CONTINUE
C
        IF (DELT .LE. DELTAT) GO TO 60
          DO 50 J=1,NCOMPS
            XVAR(J) = GAPVAL
            IF (STKIND(J) .NE. 1) GO TO 40
C             point kinds and interpolate is only valid functional
              XVAR(J) = PAD(VOPADR(J)+1)
 40         CONTINUE
 50       CONTINUE
C
          GO TO 100
 60     CONTINUE
C
          IF (BCWNOV .NE. 0) GO TO 90
            DO 80 J=1,NCOMPS
              XVAR(J) = GAPVAL
              IF (STKIND(J) .NE. 1) GO TO 70
C               point kinds -- only valid functional is last
                XVAR(J) = PAD(VOPADR(J)+1)
 70           CONTINUE
 80         CONTINUE
 90       CONTINUE
C
 100    CONTINUE
C
 110  CONTINUE
C
C     check for zero/undefined frame
      ZFLAG= 1
      UFLAG= 1
      DO 120 J=1,NCOMPS
        IF ((ABS(XVAR(J))) .GT. 0.0) ZFLAG= 0
        IF (XVAR(J) .GT. -1.E15) UFLAG= 0
 120  CONTINUE
      RETURN
      END
C
C     4.3.1.3.3.3
C
      SUBROUTINE   NEWTSB
C
C     + + + PURPOSE + + +
C     Create the bcw and update control values for a new
C     uncompressed tsb.
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
      INTEGER   OFF(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    PTVAL
C
C     + + + END SPECIFICATIONS + + +
C
      BCWNOV = 1
      BCWBTI = 1
      VOTSB  = VOTFRM
      TZERO  = TLAST
      FRMTIM = TZERO + DELTAT
      VOTFRM = VOTFRM + 1
      OFF(1) = 0
      CWF    = 0
      FORFLG = 0
C
C     write initial frame of tsb-bcw is written when tsb
C     is closed.
C     we cannot set mean values to undefined because we do not
C     have information on the location of the mean values in the
C     frame.  the instruction contains only information on the
C     values being transfered.
C
      CALL PTVAL(TOTCOM,OFF,XVAR,VOTFRM)
C
      VOTFRM = VOTFRM + TOTCOM
C     set counters for potential compressed frames
C
      UCNT = 0
      ZCNT = 0
C
      IF (COMPR .NE. 2) GO TO 10
C
C
        IF (ZFLAG .EQ. 1) ZCNT = 1
        IF (UFLAG .EQ. 1) UCNT = 1
C
 10   CONTINUE
C
C     clean up possible negative flags
      IF (ZFLAG .LT. 0) ZFLAG = 0
      IF (UFLAG .LT. 0) UFLAG = 0
C
      RETURN
      END
C
C     4.3.1.1
C
      SUBROUTINE   PTFRAM
     I                    (VOFRM)
C
C     + + + PURPOSE + + +
C     Update any point values in the frame given by vofrm taking
C     values from xvar.
C
C     + + + DUMMY ARGUMENT + + +
      INTEGER   VOFRM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOFRM  - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,N,OFF(20)
C
C     + + + EXTERNALS + + +
      EXTERNAL  BUFINT,RPUT,WBUFF
C
C     + + + END SPECIFICATIONS + + +
C
C     select the point kind series
C
      N = 0
      DO 20 J=1,NCOMPS
        IF (STKIND(J) .NE. 1) GO TO 10
          N      = N + 1
          OFF(N) = OFFSET(J)
 10     CONTINUE
 20   CONTINUE
C
      IF (N .LE. 0) GO TO 30
        CALL BUFINT(VOFRM,VOFRM)
        CALL RPUT(N,OFF,XVAR,VOFRM)
        CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
C
        BMTFLG = 1
 30   CONTINUE
C
      RETURN
      END
C
C     4.3.1.03
C
      SUBROUTINE   PTVAL
     I                   (N,OFF,VAL,VO)
C
C     + + + PURPOSE + + +
C     Put one or more values into the buffer/extension and write
C     to the tss as necessary
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N,OFF(20),VO
      REAL      VAL(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - ???
C     OFF    - ???
C     VAL    - ???
C     VO     - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,J,NVO,I,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,OMSTI,FDATIM,OMSG,WBUFF,MEXT
C
C     + + + END SPECIFICATIONS + + +
C
C     check buffer state
C
      SCLU = 235
      IF (BMTFLG .NE.  1) GO TO 10
C       program bug
C       invalid buffer state
        SGRP = 13
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 10   CONTINUE
C
C     compute virtual origin for buffer operations
C
      NVO=VO -VOBUFF
      IF (NVO .GE. 0) GO TO 20
C       program bug
C       buffer underflow
        SGRP = 14
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 20   CONTINUE
      IF (OFF(1) .LE. 0) GO TO 40
        IF (NVO+OFF(N) .LE. BLEN) GO TO 30
C         program bug
C         buffer/extension overflow
          SGRP = 15
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 30     CONTINUE
        GO TO 60
 40   CONTINUE
        IF (NVO+N .LE. BLEN) GO TO 50
C         program bug
C         buffer/extension overflow
          SGRP = 15
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 50     CONTINUE
 60   CONTINUE
C
C     put values into the buffer/extension
C
      IF (OFF(1) .NE. 0) GO TO 80
        DO 70 I=1,N
          J = NVO +I
          TBUFF(J) = VAL(I)
 70     CONTINUE
        GO TO 100
 80   CONTINUE
        DO 90 I=1,N
          J = NVO +OFF(I)
          TBUFF(J) = VAL(I)
 90     CONTINUE
 100  CONTINUE
C
C     update buffer overflow detector and set extf as needed
C
      IF (J .LE. BADR) GO TO 110
        BADR=J
 110  CONTINUE
      IF (BADR .LE. RECLT) GO TO 120
        EXTF= 1
 120  CONTINUE
C
      IF (FORFLG.NE. 1) GO TO 150
C       write buffer/extension
        BMTFLG= 1
        CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
        VOBUFF=VOBUFF +RECLT
C
C       move extension into buffer
C
        CALL MEXT(RECLT,BLEN,  BADR,TBUFF)
C
C       write buffer again
C
        IF (BADR .LE. 0) GO TO 140
          TRCNO=TRCNO +1
          IF (TRCNO .LE. LREC) GO TO 130
C           error
C           dataset overflow
            SGRP = 16
            CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 130      CONTINUE
          CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
 140    CONTINUE
        GO TO 190
 150  CONTINUE
C       write buffer and move extension if buffer is full and
C       tsb is complete
C
        IF (EXTF .NE.  1) GO TO 180
          IF (CWF.NE.  1) GO TO 170
            EXTF = 0
            CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
            CALL MEXT(RECLT,BLEN,  BADR,TBUFF)
            TRCNO=TRCNO +1
            IF (TRCNO .LE. LREC) GO TO 160
C             error
C             dataset overflow
              SGRP = 16
              CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
              CALL OMSTD (DATIM)
              CALL OMSTI (FILE)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   KNT)
 160        CONTINUE
            VOBUFF=VOBUFF +RECLT
 170      CONTINUE
 180    CONTINUE
 190  CONTINUE
C
      RETURN
      END
C
C     4.3.1
C
      SUBROUTINE   PUTTSS
     I                    (FSTCAL,LSTCAL,DELT,WIDTH)
C
C     + + + PURPOSE + + +
C     Take time frames from the pad, perform functional
C     operations and place resulting time frame into the tss
C
C     + + + DUMMY ARGUMENT + + +
      INTEGER   FSTCAL,LSTCAL,DELT,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FSTCAL - flag indicating first interval of run
C     LSTCAL - flag indicating last interval of run
C     DELT   - simulation time interval in minutes
C     WIDTH  - inpad width
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
      INTEGER   COMPRS,I,I4,ILIM,INPTIM,J,N,OFF(20)
      REAL      XV
C
C     + + + EXTERNALS + + +
      EXTERNAL  PTFRAM,BUFINT,FILTSS,HFRAME,REPLC
      EXTERNAL  XVINIT,TFUNE,LTRAN,TFUNG,TFUNL,WFBUFF
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS,ABS,FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
C     set buffer empty flag to 'on'
      BMTFLG = 1
C
      IF (FSTCAL.NE.1) GO TO 170
C       first call
C
        IF (IABS(LGAP).LE.0) GO TO 60
C         initialize xvar with user selected value for gaps
C         set flags describing contents of xvar
          IF ((ABS(GAPVAL)).GT.0.0) GO TO 4
            ZFLAG=1
            UFLAG=0
            GO TO 8
 4        CONTINUE
            ZFLAG=0
            UFLAG=1
 8        CONTINUE
          DO 10 I=1,TOTCOM
            XVAR(I)=GAPVAL
 10       CONTINUE
C
          IF (VOLFRM.LE.0) GO TO 20
C           update point values in previous frame
            CALL PTFRAM(VOLFRM)
 20       CONTINUE
C
C         fill the gap at start of calendar year
C         replace access mode does not permit gaps
C
          CALL BUFINT(VOTSB,VOTFRM)
C
C         select compression mode for the gap
C
          COMPRS=COMPR
          IF (COMPR.NE.1 .OR. LGAP.GE.0) GO TO 30
            COMPRS=2
 30       CONTINUE
C
C         leading gaps can only exist in the year at
C         start of run
          TXVAR = 0
          ILIM  =IABS(LGAP)
          DO 40 I=1,ILIM
            CALL FILTSS(COMPRS,TOTCOM)
            TXVAR = TXVAR + DELTAT
 40       CONTINUE
C
C         compute initial frame and write to tss if times match
C
          IF (NREM.NE.DELTAT/DELT) GO TO 50
C           times match
            CALL HFRAME(DELT)
            CALL FILTSS(COMPR,NCOMPS)
 50       CONTINUE
C
          GO TO 150
 60     CONTINUE
          IF (NREM.NE.DELTAT/DELT) GO TO 130
C           times match
            CALL HFRAME(DELT)
C           update point values in previous tsb
            IF (VOLFRM.LE.0) GO TO 70
              CALL PTFRAM(VOLFRM)
 70         CONTINUE
C           initialize the buffer
            CALL BUFINT(VOTSB,VOTFRM)
C
C           write the initial frame
            IF (AMODE.NE.3) GO TO 110
C             write point values only
              N=0
              DO 90 J=1,NCOMPS
                IF (STKIND(J).NE.1) GO TO 80
                  N     =N+1
                  OFF(N)=OFFSET(J)
 80             CONTINUE
 90           CONTINUE
              IF (N.LE.0) GO TO 100
                CALL REPLC(OFF,N)
                GO TO 102
 100          CONTINUE
                VOTFRM= VOTFRM+ TOTCOM
                FRMTIM= FRMTIM+ DELTAT
 102          CONTINUE
              GO TO 120
 110        CONTINUE
C             write all values
              CALL FILTSS(COMPR,NCOMPS)
 120        CONTINUE
            GO TO 140
 130      CONTINUE
            CALL BUFINT(VOTSB,VOTFRM)
 140      CONTINUE
 150    CONTINUE
C
        IF (DELT.GE.DELTAT) GO TO 160
          CALL XVINIT(NCOMPS,FILE,FRMTIM,MESSU,
     I                STTRAN,TYREND,YEAR,MSGFL,TESTFG,
     M                KNT,
     O                XVAR  )
 160    CONTINUE
        GO TO 180
 170  CONTINUE
C
C       not first call
C       initialize the buffer
        CALL BUFINT(VOTSB,VOTFRM)
 180  CONTINUE
C
C
C     initialize xold from first column of pad
C
      DO 190 J=1,NCOMPS
        XOLD(J)=PAD(VOPADR(J)+1)
 190  CONTINUE
C
C
C
      IF (DELT.NE.DELTAT) GO TO 230
        TXVAR = INPSTR
        DO 220 I=2,WIDTH
          DO 200 J=1,NCOMPS
            XNEW(J)=PAD(VOPADR(J)+I)
 200      CONTINUE
          CALL TFUNE
          IF (LTRNFG.EQ.1) CALL LTRAN(NCOMPS,A,B,
     M                                XVAR)
          TXVAR = TXVAR + DELTAT
          CALL FILTSS(COMPR,NCOMPS)
          DO 210 J=1,NCOMPS
            XOLD(J)=XNEW(J)
 210      CONTINUE
 220    CONTINUE
        GO TO 370
 230  CONTINUE
        IF (DELT.LE.DELTAT) GO TO 280
          TXOLD =INPSTR
          RATIO =DELT/DELTAT
          I4    =RATIO
          RRATIO=FLOAT (I4)
          INPTIM=INPSTR
          TXVAR =INPSTR
          DO 270 I=2,WIDTH
            DO 240 J=1,NCOMPS
              XNEW(J)=PAD(VOPADR(J)+I)
 240        CONTINUE
            INPTIM=INPTIM +DELT
            TXNEW =INPTIM
            DO 250 J=1,RATIO
              TXVAR=TXVAR+DELTAT
              CALL TFUNG(TXVAR,DELT)
              IF (LTRNFG.EQ.1) CALL LTRAN(NCOMPS,A,B,
     M                                    XVAR)
              CALL FILTSS(COMPR,NCOMPS)
 250        CONTINUE
            DO 260 J=1,NCOMPS
              XOLD(J)=XNEW(J)
 260        CONTINUE
            TXOLD=TXNEW
 270      CONTINUE
          GO TO 360
 280    CONTINUE
C         delt < deltat
C         nrem is never zero here
C
C         initialize xvar
          DO 290 I=1,NCOMPS
            XVAR(I)=PVAR(I)
 290      CONTINUE
C
C         note that txvar in this section gives time at which next
C         frame will be written whereas in the other two cases
C         (equality and disaggregation) txvar gives the time at
C         which the last frame was written.
C
          TXVAR =INPSTR +NREM*DELT
          TXOLD =INPSTR
          INPTIM=INPSTR
          RATIO =DELTAT/DELT
          I4    =RATIO
          RRATIO=FLOAT (I4)
          DO 340 I=2,WIDTH
            DO 300 J=1,NCOMPS
              XNEW(J)=PAD(VOPADR(J)+I)
 300        CONTINUE
            INPTIM=INPTIM +DELT
            TXNEW =INPTIM
            CALL TFUNL(TXVAR)
            NREM  =NREM -1
            IF (NREM.NE.0) GO TO 320
              NREM =RATIO
C             check for zero/undefined frames
C             set flags for zero/undefined
              ZFLAG= 1
              UFLAG= 1
              DO 310 J=1,NCOMPS
                XV=XVAR(J)
                IF ((ABS(XV)).GT.0.0) ZFLAG= 0
                IF (XV.GT.-1.E15) UFLAG= 0
 310          CONTINUE
              IF (LTRNFG.EQ.1) CALL LTRAN(NCOMPS,A,B,
     M                                    XVAR)
              CALL FILTSS(COMPR,NCOMPS)
              TXVAR=TXVAR +DELTAT
              CALL XVINIT(NCOMPS,FILE,FRMTIM,MESSU,
     I                    STTRAN,TYREND,YEAR,MSGFL,TESTFG,
     M                    KNT,
     O                    XVAR  )
 320        CONTINUE
            DO 330 J=1,NCOMPS
              XOLD(J)=XNEW(J)
 330        CONTINUE
            TXOLD=TXNEW
 340      CONTINUE
C         save xvar
          DO 350 I=1,NCOMPS
            PVAR(I)=XVAR(I)
 350      CONTINUE
C
 360    CONTINUE
 370  CONTINUE
C
      IF (LSTCAL.NE.1) GO TO 420
C       last call
        IF (IABS(TGAP).LE.0) GO TO 410
C         set flags describing contents of xvar
          IF ((ABS(GAPVAL)).GT.0.0) GO TO 374
            ZFLAG=1
            UFLAG=0
            GO TO 378
 374      CONTINUE
            ZFLAG=0
            UFLAG=1
 378      CONTINUE
          DO 380 J=1,TOTCOM
            XVAR(J)=GAPVAL
 380      CONTINUE
C
          COMPRS=COMPR
          IF (COMPR.NE.1 .OR. TGAP.GE.0) GO TO 390
            COMPRS=2
 390      CONTINUE
C
C         adjust txvar for aggregation case
          IF (DELT.LT.DELTAT) TXVAR= TXVAR- DELTAT
          ILIM=IABS(TGAP)
          DO 400 I=1,ILIM
            TXVAR = TXVAR + DELTAT
            CALL FILTSS(COMPRS,TOTCOM)
 400      CONTINUE
 410    CONTINUE
 420  CONTINUE
C     write out the buffer/extension if needed
      IF (BMTFLG.NE.0) GO TO 430
        CALL WFBUFF
 430  CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.1
C
      SUBROUTINE   REPLC
     I                   (OFF,NTS)
C
C     + + + PURPOSE + + +
C     Store values for replace access mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NTS,OFF(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OFF    - ???
C     NTS    - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BCWADR,SCLU,SGRP,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,OMSTI,FDATIM,OMSG,RPUT,RGET,GTWORD,RINIT
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      IF (BMTFLG .NE. 1) GO TO 10
C       program bug.  invalid buffer state.
        SGRP = 22
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 10   CONTINUE
C
C
      CALL RPUT(NTS,OFF,XVAR,VOTFRM)
C
      VOTFRM = VOTFRM + TOTCOM
C
C
C     check for end of a tsb not at end of run
C
      IF (FRMTIM .NE. TLAST) GO TO 80
        IF (FRMTIM .EQ. TENDR) GO TO 70
C         end of tsb -- get next bcw
C
          VOTSB = VOTFRM
C
          CALL RGET(VOTSB,      BCW)
          IF (BCW .GE. 0) GO TO 30
C           end of year
            BCW    = IABS(BCW)
            BCWADR = BCW + 1
            CALL GTWORD(BCWADR,      BCW)
C           use gtword because the next year could be more than one
C            tss record distant from trcno
C
            IF (YEAR .EQ. BCWBTI) GO TO 20
C             program bug
C             year not= value of year obtained in dataset
              SGRP = 11
              CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
              CALL OMSTD (DATIM)
              CALL OMSTI (FILE)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   KNT)
 20         CONTINUE
C
            VOTSB = BCWADR
            CALL RGET(VOTSB,      BCW)
            CALL RINIT
C
            GO TO 60
 30       CONTINUE
C
C
            IF (BCW .LE. 0) GO TO 40
C             new tsb
C
              CALL RINIT
C
              GO TO 50
 40         CONTINUE
C
C             bcw = 0 -- end of chronological data
C             error
C             end of chronological data encountered
C             before end of run
              SGRP = 12
              CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
              CALL OMSTD (DATIM)
              CALL OMSTI (FILE)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   KNT)
C
 50         CONTINUE
 60       CONTINUE
 70     CONTINUE
C
        GO TO 90
 80   CONTINUE
C
        FRMTIM = FRMTIM + DELTAT
C
 90   CONTINUE
C
      RETURN
      END
C
C     4.3.1.01
C
      SUBROUTINE   RGET
     I                  (VO,
     O                   WORD)
C
C     + + + PURPOSE + + +
C     Get a word from the dataset at vo.  word should be in current
C      buffer or in the tss record after the tss record in the buffer.
C     Used for replace access.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     VO,WORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VO     - ???
C     WORD   - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ADR,SGRP,SCLU,WORDI,DATIM(5)
      REAL        WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDI,WORDR)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,OMSTI,FDATIM,OMSG,WBUFF,RBUFF
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      ADR = VO - VOBUFF + 1
      IF (ADR .GE. 1) GO TO 10
C       program bug
C       buffer underflow
        SGRP = 4
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 10   CONTINUE
C
      IF (ADR .LE. RECLT) GO TO 30
        CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
C
        TRCNO = TRCNO + 1
        IF (TRCNO .LE. LREC) GO TO 20
C         program bug
C         dataset overflow
          SGRP = 5
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 20     CONTINUE
C
        CALL RBUFF(TRCNO,RECLT,TSSFL,      TBUFF)
C
        VOBUFF = VOBUFF + RECLT
        ADR    = ADR - RECLT
 30   CONTINUE
C
      IF (ADR .LE. RECLT) GO TO 40
C       program bug
C       buffer overflow
        SGRP = 6
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 40   CONTINUE
C
      WORDR = TBUFF(ADR)
      WORD  = WORDI
C
      RETURN
      END
C
C     4.3.1.3.1.2
C
      SUBROUTINE   RINIT
C
C     + + + PURPOSE + + +
C     Initialize a tsb for replace processing.
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,WORDI,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  BCWSPL,OMSTD,OMSTI,FDATIM,OMSG,RPUT
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      WORDI= BCW
      CALL BCWSPL(WORDI,    BCWBTI,BCWNOV)
      IF (BCWBTI .EQ. 1) GO TO 10
C       error
C       invalid time series block encountered during
C       replace access. block must be type uncompressed.
        SGRP = 7
        CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
 10   CONTINUE
C
      TZERO  = TLAST
      TLAST  = TZERO + (BCWNOV-1)*DELTAT
      FRMTIM = TZERO
      VOTFRM = VOTSB + 1
C
C     write the initial frame
C
      CALL RPUT(NCOMPS,OFFSET,XVAR,VOTFRM)
C
      VOTFRM = VOTFRM + TOTCOM
      FRMTIM = FRMTIM + DELTAT
C
      RETURN
      END
C
C     4.3.1.02
C
      SUBROUTINE   RPUT
     I                  (N,OFF,VAL,VO)
C
C     + + + PURPOSE + + +
C     Put n values in val into the buffer at offsets in off from
C     virtual origin in vo.  write and read records as needed, but
C     no backtracking allowed.  advancement must be one record at a
C     time.  offsets must be in ascending order in off.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N,OFF(20),VO
      REAL      VAL(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - ???
C     OFF    - ???
C     VAL    - ???
C     VO     - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ADR,SCLU,SGRP,I,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,OMSTI,FDATIM,OMSG,WBUFF,RBUFF
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      DO 50 I=1,N
        ADR = VO - VOBUFF + OFF(I)
C
        IF (ADR .GE. 1) GO TO 10
C         program bug
C         buffer underflow
          SGRP = 8
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 10     CONTINUE
C
        IF (ADR .LE. RECLT) GO TO 30
C
          CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
          TRCNO = TRCNO + 1
          IF (TRCNO .LE. LREC) GO TO 20
C           error
C           dataset overflow
            SGRP = 9
            CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 20       CONTINUE
          CALL RBUFF(TRCNO,RECLT,TSSFL,TBUFF)
          VOBUFF = VOBUFF + RECLT
          ADR    = ADR - RECLT
C
 30     CONTINUE
C
        IF (ADR .LE. RECLT) GO TO 40
C         program bug
C         buffer overflow
          SGRP = 10
          CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
C
        TBUFF(ADR) = VAL(I)
C
 50   CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.5
C
      SUBROUTINE   SQUISH
     I                    (CNT,BTI)
C
C     + + + PURPOSE + + +
C     Compress an uncompressed tsb.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNT,BTI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNT    - ???
C     BTI    - ???
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     N,OFF(1),TIME,VO,WORDI
      REAL        WORDR(1)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDI,WORDR(1))
C
C     + + + FUNCTIONS + + +
      INTEGER     BCWCAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    BCWCAL,PTVAL
C
C     + + + END SPECIFICATIONS + + +
C
C     find time and vo of the first compressible frame in the
C      current series of uncompressed values.
C
      TIME = FRMTIM - CNT*DELTAT
      VO   = VOTFRM - CNT*TOTCOM
C
      IF (TIME .NE. TZERO) GO TO 20
C
C       reset existing tsb to be of type bti compressed
C
        BCWBTI = BTI
        BCWNOV = CNT
        VOTFRM = VOTSB + 1
        VOLFRM = VOTSB - TOTCOM
        WORDI  = CNT
        BADR   = BADR - WORDI*TOTCOM
        IF (BADR .GT. RECLT) GO TO 10
          EXTF =0
 10     CONTINUE
C
        GO TO 40
 20   CONTINUE
C
C       terminate current tsb and initiate a bti compressed tsb
C
        BCWNOV = BCWNOV - CNT + 1
        BADR   = BADR - (CNT-1)*TOTCOM
        IF (BADR .GT. RECLT) GO TO 30
          EXTF =0
 30     CONTINUE
        CWF    = 1
        OFF(1) = 0
        FORFLG = 0
        N      = 1
        WORDI  = BCWCAL(BCWBTI,BCWNOV)
C
        CALL PTVAL(N,OFF,WORDR,VOTSB)
C
        WORDI  = BCWNOV
        TZERO  = TZERO +(WORDI-1)*DELTAT
        VOTSB  = VO + TOTCOM
        VOTFRM = VOTSB + 1
        VOLFRM = VO
        BCWBTI = BTI
        BCWNOV = CNT
 40   CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.3.2
C
      SUBROUTINE   UPDLAB
     I                    (BCW,RECLT,FREC,TSSFL,YEAR)
C
C     + + + PURPOSE + + +
C     Label data structure declared overlaid on tbuff.
C     update keying information in dataset label.
C     bring label into buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BCW,FREC,RECLT,TSSFL,YEAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCW    - ???
C     RECLT  - record length of the time series store
C     FREC   - ???
C     TSSFL  - fortran unit number of time series store file
C     YEAR   - ???
C
C     + + + COMMON BLOCKS- PUTCOM1 + + +
      INCLUDE   'ctslx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL    RBUFF,WBUFF
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS
C
C     + + + END SPECIFICATIONS + + +
C
      CALL RBUFF(FREC,RECLT,TSSFL,      TBUFF)
C
      KEYS(YEAR-BASEYR) = VOYEAR
C
      IF (YEAR .LE. LASTYR) GO TO 10
        LASTYR = YEAR
 10   CONTINUE
C
      VOFRWD = IABS(BCW)
      VOYEAR = VOFRWD
C
      CALL WBUFF(FREC,RECLT,TSSFL,TBUFF)
C
      RETURN
      END
C
C     4.3.1.5
C
      SUBROUTINE   WFBUFF
C
C     + + + PURPOSE + + +
C     Write final buffer/extension
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WBUFF,MEXT,OMSTD,OMSTI,FDATIM,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 235
      CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
      IF (AMODE .EQ. 3) GO TO 30
C       check on extension
        CALL MEXT(RECLT,BLEN,  BADR,TBUFF)
        IF (BADR .LE. 0) GO TO 20
          TRCNO = TRCNO + 1
          IF (TRCNO .LE. LREC) GO TO 10
            SGRP = 23
            CALL FDATIM(FRMTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 10       CONTINUE
          CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
 20     CONTINUE
 30   CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.2
C
      SUBROUTINE   XVEXP
     I                   (NCOMPS,TOTCOM,GAPVAL,XVAR,OFFSET)
C
C     + + + PURPOSE + + +
C     Expand xvar so that all totcom components have proper values
C     in xvar.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NCOMPS,TOTCOM,OFFSET(20)
      REAL      GAPVAL,XVAR(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCOMPS - ???
C     TOTCOM - ???
C     GAPVAL - ???
C     XVAR   - ???
C     OFFSET - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I
C
C     + + + END SPECIFICATIONS + + +
C
C     set unused portion to user selected value.
C
      I1 = NCOMPS + 1
      DO 10 I=I1,TOTCOM
        XVAR(I) = GAPVAL
 10   CONTINUE
C
      DO 30 I1=1,NCOMPS
        I=NCOMPS+1-I1
        IF (OFFSET(I) .EQ. I) GO TO 20
          XVAR(OFFSET(I)) = XVAR(I)
          XVAR(I)         = GAPVAL
 20     CONTINUE
C
 30   CONTINUE
C
      RETURN
      END
C
C     4.3.1.3.4
C
      SUBROUTINE   XVPUT
C
C     + + + PURPOSE + + +
C     Place xvar in the current tsb and update control values.
C
C     + + + COMMON BLOCKS- PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OFF(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    PTVAL
C
C     + + + END SPECIFICATIONS + + +
C
      CWF    = 0
      OFF(1) = 0
      FORFLG = 0
C
      CALL PTVAL(TOTCOM,OFF,XVAR,VOTFRM)
C
      BCWNOV = BCWNOV + 1
      FRMTIM = FRMTIM + DELTAT
      VOLFRM = VOTFRM
      VOTFRM = VOTFRM + TOTCOM
      TLAST  = TLAST + DELTAT
C
      RETURN
      END
