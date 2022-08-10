C
C
C
      SUBROUTINE ADJLAB
     I                 (YR)
C     + + + PURPOSE + + +
C     Adjust label of dataset as needed and transfer the
C      updated label to the tss
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ISTART
C
C     + + + EXTERNALS + + +
      EXTERNAL  WBUFF,RBUFF
C
C     + + + END SPECIFICATIONS + + +
C
C     Input label-it may have been overwritten in tbuff
C     By previous operations.
C
      CALL RBUFF(FREC,RECLT,TSSFL,TBUFF)
      IF (AMODE .NE. 1) GO TO 60
        IF (KEYS(YR-BASEYR) .NE. 0) GO TO 30
          IF (YR .GE. LASTYR) GO TO 10
C           add used to place data before any pre-existing data in
C           the dataset. initialize as if dataset were empty
            LASTYR= 0
            VOYEAR= LBLSZ
            GO TO 20
 10       CONTINUE
C           add used to place data immediately after last year
C           of data
            VOYEAR= VOFRWD
 20       CONTINUE
          GO TO 40
 30     CONTINUE
C         add used to place data within pre-existing data
          VOYEAR= KEYS(YR-BASEYR)
 40     CONTINUE
C       erase keys information for overwritten pre-existing data
        ISTART= YR- BASEYR+ 1
        IF (ISTART .GT. 100) ISTART= 100
        DO 50 I= ISTART,100
          KEYS(I)= 0
 50     CONTINUE
        GO TO 70
 60   CONTINUE
        VOYEAR=VOFRWD
 70   CONTINUE
C
C     write updated label to tss
      CALL WBUFF(FREC,RECLT,TSSFL,TBUFF)
C
      RETURN
      END
C
C     3.5.8.2.3.2
C
      SUBROUTINE ADJTSB
     I                 (STIME,SINT,DELT)
C     + + + PURPOSE + + +
C     Adjust existing tsb to be ready for writing from puttss.
C     the tss record containing the bcw for the tsb is assumed
C     to be in tbuff as left by fitsb.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,STIME,SINT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIME  - ???
C     SINT   - ???
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL  RPUNC,AIUNC,AICOMP,OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 216
C     compute time step ratio.  ratio = 0 if deltat < delt
      RATIO = DELTAT/DELT
      IF (AMODE .NE.  3 ) GO TO 10
        CALL RPUNC(STIME,SINT,DELT)
        GO TO 60
 10   CONTINUE
        GO TO (20,30,40,50), BCWBTI
 20       CONTINUE
            CALL AIUNC(STIME,SINT,DELT)
            GO TO 55
 30       CONTINUE
            CALL AICOMP(STIME,SINT,DELT)
            GO TO 55
 40       CONTINUE
            CALL AICOMP(STIME,SINT,DELT)
            GO TO 55
 50       CONTINUE
C           program bug
C           linear variation bti encountered when such bti's
C           are not yet supported
            SGRP = 137
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
 55     CONTINUE
 60   CONTINUE
C
      RETURN
      END
C
C     3.5.8.2.3.2.3
C
      SUBROUTINE AICOMP
     I                 (STIME,SINT,DELT)
C
C     + + + PURPOSE + + +
C     Adjust existing compressed tsb for add access
C     Needs 22 real words of workspace in common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DELT,STIME,SINT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIME  - ???
C     SINT   - ???
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE     'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,N,OFF(20),REM,WORDI,VO,SCLU,SGRP
      REAL        DUMMY,WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDR,WORDI)
C
C     + + + FUNCTIONS + + +
      INTEGER    BCWCAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   BCWCAL,WBUFF,PVAL,OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 216
      IF (STIME .NE. TZERO) GO TO 10
C       starting time at initial point of tsb
C       restart tsb as uncompressed
        NREM   = RATIO
        BCWBTI = 1
        BCWNOV = 0
        VOTFRM = VOTSB +1
        TLAST  = TZERO-DELTAT
        FRMTIM = STIME
        WORDI  =BCWCAL(BCWBTI,BCWNOV)
        TBUFF(VOTSB-VOBUFF+1) = WORDR
        CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
        BMTFLG = 1
        GO TO 100
 10   CONTINUE
        N    = (STIME-TZERO)/DELTAT
        REM  = (STIME-TZERO) -N*DELTAT
        NREM = RATIO -REM/DELT
C       set workspace to zero or undefined
        IF (BCWBTI .NE. 2) GO TO 20
          DUMMY = 0
          GO TO 30
 20     CONTINUE
          DUMMY = -1.E30
 30     CONTINUE
          DO 40 I=1,TOTCOM
            WS(I+1) = DUMMY
 40       CONTINUE
        IF (REM .NE. 0) GO TO  60
C         starting point at a point represented in tsb,
C         but not initial nor final point
C         revise tsb and initiate a new tsb
          BCWNOV = N +1
          VOLFRM = VOTSB +1
          VO     = VOTSB
          WORDI  =BCWCAL(BCWBTI,BCWNOV)
          WS(1)  = WORDR
C         develop new bcw
          BCWBTI = 1
          BCWNOV = 0
          VOTSB  = VOTSB +TOTCOM +1
          VOTFRM = VOTSB +1
          TZERO  = SINT-DELTAT
          IF (TZERO .EQ. STIME) GO TO 50
C           program bug
            SGRP = 136
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
 50       CONTINUE
          TLAST  = TZERO -DELTAT
          FRMTIM = STIME
          WORDI  =BCWCAL(BCWBTI,BCWNOV)
          WS(TOTCOM+2) = WORDR
          N      = TOTCOM +2
          OFF(1) =  0
          CWF    = 1
          FORFLG = 1
          CALL PVAL(N,OFF,WS,VO)
          GO TO 90
 60     CONTINUE
C         starting point between points stored in the tsb
          VOLFRM = 0
          BCWNOV = N +1
          CWF    = 1
          OFF(1) = 0
          N      = TOTCOM +1
          IF (BCWNOV .NE. 1) GO TO 70
C           restart tsb as uncompressed-first frame is zero or
C            undefined
            BCWBTI = 1
            WORDI  =BCWCAL(BCWBTI,BCWNOV)
            WS(1)  = WORDR
            FORFLG = 1
            VOTFRM = VOTSB +TOTCOM +1
            CALL PVAL(N,OFF,WS,VOTSB)
            GO TO  80
 70       CONTINUE
C           revise the tsb and initiate a new uncompressed tsb.
C            the last frame of the current tsb and the first frame
C            of the new tsb are zero or undefined
C
            WORDI=BCWCAL(BCWBTI,BCWNOV)
            WS(1)  = WORDR
            FORFLG = 0
            CALL PVAL(N,OFF,WS,VOTSB)
            BCWBTI = 1
            BCWNOV = 1
            VOTSB  = VOTSB +TOTCOM +1
            VOTFRM = VOTSB +TOTCOM +1
            WORDI  =BCWCAL(BCWBTI,BCWNOV)
            WS(1)  = WORDR
            FORFLG = 1
            CALL PVAL(N,OFF,WS,VOTSB)
 80       CONTINUE
          TZERO  = SINT-DELTAT
          TLAST  = TZERO
          FRMTIM = SINT
 90     CONTINUE
 100  CONTINUE
C
      RETURN
      END
C
C     3.5.8.2.3.2.2
C
      SUBROUTINE AIUNC
     I                (STIME,SINT,DELT)
C
C     + + + PURPOSE + + +
C     Adjust existing uncompressed tsb for add access mode
C     Needs 21 real words of workspace in common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DELT,STIME,SINT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIME  - ???
C     SINT   - ???
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE     'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ADR,ADRBCW,I,N,OFF(20),REM,WORDI,SCLU,SGRP
      REAL        WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDR,WORDI)
C
C     + + + FUNCTIONS + + +
      INTEGER    BCWCAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   BCWCAL,WBUFF,GWORD,PVAL,OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 216
      IF (STIME .NE. TZERO) GO TO 10
C       starting time is at initial point of tsb
C       restart tsb
        NREM  =RATIO
        BCWNOV=0
        VOTFRM=VOTSB +1
        FRMTIM=STIME
        TLAST =TZERO -DELTAT
C       write the bcw for the tsb
        WORDI =BCWCAL(BCWBTI,BCWNOV)
        TBUFF(VOTSB-VOBUFF+1)=WORDR
        CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
        BMTFLG= 1
        GO TO 80
 10   CONTINUE
        N   =(STIME-TZERO)/DELTAT
        REM =(STIME-TZERO) -N*DELTAT
        NREM=RATIO -REM/DELT
        IF (REM .NE. 0) GO TO 30
C         starting time at a point stored in tsb
C         but not initial nor last point
C         revise tsb and initiate new tsb
          BCWNOV=N+1
          WORDI =BCWCAL(BCWBTI,BCWNOV)
          TBUFF(VOTSB-VOBUFF+1)=WORDR
          CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
          BMTFLG= 1
C         set up new bcw
          VOTSB=VOTSB +BCWNOV*TOTCOM +1
          BCWBTI=1
          BCWNOV=0
          VOTFRM=VOTSB +1
          VOLFRM=VOTSB -TOTCOM
          TZERO =SINT-DELTAT
          IF (TZERO .EQ. STIME) GO TO 20
C           program bug
C           timing error : stime not= tzero
            SGRP = 135
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
 20       CONTINUE
          TLAST =TZERO -DELTAT
          FRMTIM=STIME
          ADRBCW=VOTSB +1
C         make sure correct record is in buffer
          CALL GWORD(ADRBCW,      WORDR)
          WORDI =BCWCAL(BCWBTI,BCWNOV)
          TBUFF(ADRBCW-VOBUFF)=WORDR
          CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
          BMTFLG= 1
          GO TO 70
 30     CONTINUE
C         starting time not at any point stored in tsb
          VOLFRM=0
          BCWNOV=N+1
          IF (BCWNOV .NE. 1) GO TO  40
C           revise current tsb
            VOTFRM=VOTSB +1 +TOTCOM
            WORDI =BCWCAL(BCWBTI,BCWNOV)
            TBUFF(VOTSB-VOBUFF+1) = WORDR
            CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
            BMTFLG= 1
            GO TO  60
 40       CONTINUE
C           revise current tsb and create a new tsb
            WORDI=BCWCAL(BCWBTI,BCWNOV)
            TBUFF(VOTSB-VOBUFF+1) = WORDR
            CALL WBUFF(TRCNO,RECLT,TSSFL,TBUFF)
            BMTFLG= 1
C           read last frame of the revised tsb
            VOTFRM=VOTSB +1 +N*TOTCOM
              DO 50 I=1,TOTCOM
                ADR=VOTFRM +I
                CALL GWORD(ADR,WS(I+1))
 50           CONTINUE
            VOTSB=VOTSB +BCWNOV*TOTCOM +1
            BCWNOV=1
            BCWBTI=1
            WORDI =BCWCAL(BCWBTI,BCWNOV)
            WS(1) =WORDR
            VOTFRM=VOTSB +1 +TOTCOM
            ADRBCW=VOTSB +1
C           make sure correct record is in buffer
            CALL GWORD(ADRBCW,WORDR)
            CWF   = 1
            FORFLG= 1
            N     =TOTCOM +1
            OFF(1)=0
            CALL PVAL(N,OFF,WS,VOTSB)
 60       CONTINUE
          TZERO =SINT -DELTAT
          TLAST =TZERO
          FRMTIM=SINT
 70     CONTINUE
 80   CONTINUE
C
      RETURN
      END
C
C     3.5.1.1
C
      SUBROUTINE CHKTSS
     I                  (TSSFL,MESSU,MSGFL,
     M                   ECOUNT,
     O                   TDFREC,TDDS,TOTDS,RECLT,TDSIZE)
C
C     + + + PURPOSE + + +
C     Read tss descriptor from first record of the tss, check validity
C     and return selected descriptor values
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,MESSU,RECLT,
     $           TDDS,TDFREC,TDSIZE,TOTDS,TSSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TSSFL  - fortran unit number of time series store file
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ECOUNT - count(s) of specific errors
C     TDFREC - ???
C     TDDS   - ???
C     TOTDS  - ???
C     RECLT  - record length of the time series store
C     TDSIZE - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TSSTYP,SCLU,SGRP,REC(11)
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 216
      READ (TSSFL,REC=1)  REC
      TDFREC= REC(2)
      TDDS= REC(5)
      TOTDS= REC(6)
      RECLT= REC(8)
      TDSIZE= REC(10)
      TSSTYP= REC(11)
C
C     check validity of tss
      IF (TSSTYP .NE. 1) THEN
C       error-incompatible tss-run must terminate at this point
        SGRP = 74
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      RETURN
      END
C
C     3.5.2.2.1.1
C
      INTEGER FUNCTION   DSCHK
     I                         (MESSU,MSGFL,TSSFL,DSN)
C
C     + + + PURPOSE + + +
C     Check validity of the tss dataset and input the label if valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,MSGFL,TSSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     TSSFL  - fortran unit number of time series store file
C     DSN    - ???
C
C     + + + COMMON BLOCKS- INTERP3  + + +
      INCLUDE     'crin3.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REC,SCLU,SGRP,DS(2),VOBUFF,VO,I,ADDRT,RECT,DSFRLO
      REAL        RFREC,RDSEC
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RFREC,DSFRLO), (RDSEC,DS(1))
C
C     + + + EXTERNALS + + +
      EXTERNAL   RBUFF,OMSG,OMSTI
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT(1X,'BEGIN CHECKING DATASET VALIDITY')
 2020 FORMAT(1X,'END CHECKING DATASET VALIDITY')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 216
      IF (OUTLEV .GT. 7) WRITE(MESSU,2010)
C
      IF (DSN .LE. 0 .OR. DSN .GT. TOTDS) GO TO 60
        IF (DSN .EQ. TDDS) GO TO 40
C         dataset number is valid-see if dataset exists in tss
C         compute address of the dataset entry in the tss
C          directory.
          VO =(DSN-1)*TDSIZE
          REC=TDFREC +VO/RECLT
          CALL RBUFF(REC,RECLT,TSSFL,      TBUFF)
          VOBUFF=RECLT*(REC-TDFREC)
C         special overlays needed here-must read in part of directory
C         from next record if needed
          I= VO- VOBUFF+ 3
          IF(I .LT. RECLT) GO TO 5
            RECT = REC+ 1
            ADDRT= RECLT+ 1
            CALL RBUFF(RECT,TDSIZE,TSSFL,        TBUFF(ADDRT))
 5        CONTINUE
C
          RFREC=TBUFF(VO-VOBUFF+3)
          RDSEC=TBUFF(VO-VOBUFF+1)
C
          IF (DSFRLO .LE. 0) GO TO 20
C           dataset exists
            IF (DSN .EQ. DS(1)) GO TO 10
C             program bug-error in tss directory number.
              SGRP = 87
              CALL OMSTI (DSN)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
 10         CONTINUE
C
            DSCHK=1
C
C           input the label.
C
            CALL RBUFF(DSFRLO,RECLT,TSSFL,      TBUFF)
            GO TO 30
 20       CONTINUE
C           dataset does not exist.
            DSCHK=0
 30       CONTINUE
C
          GO TO 50
 40     CONTINUE
C
          DSCHK=0
C         error-time series dataset reference made to the
C          tss directory dataset.
          SGRP = 88
          CALL OMSTI (DSN)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 50     CONTINUE
        GO TO 70
 60   CONTINUE
        DSCHK=0
C       error-dataset number outside valid range for this tss.
        SGRP = 89
        CALL OMSTI (DSN)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 70   CONTINUE
C
      IF (OUTLEV .GT. 7) WRITE(MESSU,2020)
C
      RETURN
      END
C
C     3.5.8.2.2.1
C
      SUBROUTINE FITSB
     I                 (VOYR,SINT)
C
C     + + + PURPOSE + + +
C     Find the tsb which contains the interval given by
C     sint.  the virtual origin of the year is given by voyr
C     Note that stime < tlast for the tsb found by this
C     subroutine because sint > stime.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   VOYR,SINT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOYR   - ???
C     SINT   - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE     'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ADRBCW,SCLU,SGRP,WORDI,DADR
      REAL        WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDR,WORDI)
C
C     + + + EXTERNALS + + +
      EXTERNAL    GWORD,OMSTI,OMSG,BCWSPL
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 216
      TLAST=0
C
C     compute address of first bcw in the year
      ADRBCW=VOYR +2
C
C     dountil(sint <=tlast or bcwnov <= 1)
 5    CONTINUE
        CALL GWORD(ADRBCW,      WORDR)
        CALL BCWSPL(WORDI,   BCWBTI,BCWNOV)
C
        IF (BCW .GE. 0) GO TO 10
C         program bug
C         end of year encountered when such encounter should
C         be impossible
          SGRP = 132
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 10     CONTINUE
        IF (BCW .NE. 0) GO TO   20
C         program bug
C         end of data encountered when such encounter should
C         be impossible
          SGRP = 133
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 20     CONTINUE
C
        TZERO=TLAST
        TLAST=TZERO +(BCWNOV-1)*DELTAT
C
C       point to next bcw
        IF (BCWBTI .NE. 1) GO TO 30
          DADR=TOTCOM*BCWNOV
          GO TO  60
 30     CONTINUE
          IF (BCWBTI .NE. 2 .AND. BCWBTI .NE. 3) GO TO 40
            DADR=TOTCOM
            GO TO  50
 40       CONTINUE
            DADR=2*TOTCOM
 50       CONTINUE
 60     CONTINUE
C
        VOTSB =ADRBCW-1
        ADRBCW=ADRBCW +DADR +1
      IF(SINT .GT. TLAST .AND. BCWNOV .GT. 1) GO TO 5
C
      RETURN
      END
C
C     3.5.8.2.3.1
C
      SUBROUTINE FLFRM
     I                 (YR,BASEYR,KEYS,TOTCOM,VOFRWD,
     O                  VOLFRM)
C
C     + + + PURPOSE + + +
C     Find virtual origin of previous years last frame,volfrm.  the
C     previous year is known to exist.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BASEYR,KEYS(100),TOTCOM,VOFRWD,VOLFRM,YR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - ???
C     BASEYR - base year for the keying system
C     KEYS   - ???
C     TOTCOM - ???
C     VOFRWD - ???
C     VOLFRM - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,VOCRYR,VOMIN,VOPYR
C
C     + + + END SPECIFICATIONS + + +
C
C     find smallest key which excedes the previous years key.  if no
C      such year exists then the frame is the last one in the
C      dataset.
C
      VOPYR=KEYS(YR-1-BASEYR)
      VOMIN=VOFRWD
      DO 20 I=1,100
        VOCRYR=KEYS(I)
        IF (VOCRYR .LE. VOPYR .OR. VOCRYR .GT. VOMIN) GO TO 10
          VOMIN=VOCRYR
 10     CONTINUE
 20   CONTINUE
      VOLFRM=VOMIN -TOTCOM -1
      RETURN
      END
C
C     3.5.8.2.3.5
C
      SUBROUTINE FNDGAP
     I                 (ENDR,DELTAT,TGAP)
C
C     + + + PURPOSE + + +
C     Compute the number of intervals in any gap at end of
C     a year
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ENDR(5),DELTAT,TGAP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDR   - ???
C     DELTAT - time step in minutes for source/target
C     TGAP   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   REND,TEMP(5),YEND
C
C     + + + EXTERNALS + + +
      EXTERNAL  YROFF
C
C     + + + END SPECIFICATIONS + + +
C
C     compute minute for date time in endr
C
      CALL YROFF(ENDR,REND)
C
C     compute minute of end of year
C
      TEMP(1) = ENDR(1)
      TEMP(2) = 12
      TEMP(3) = 31
      TEMP(4) = 24
      TEMP(5) = 60
      CALL YROFF(TEMP,YEND)
C
      TGAP = (YEND-REND+DELTAT-1)/DELTAT
C
      RETURN
      END
C
C     3.5.8.2.2
C
      SUBROUTINE GINIT
     I                 (START)
C
C     + + + PURPOSE + + +
C     Compute control values for the instruction file
C     for tsget for references to the tss.  no changes
C     are needed in the datasets.  the dataset label
C     is assumed to be present in the common area.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   START(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     START  - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SINT,TEMP(5),VOYR
C
C     + + + EXTERNALS + + +
      EXTERNAL    FIRINT,YROFF,FITSB
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(1X,'BEGIN COMPUTING CONTROL VALUES FOR TSGET')
 2010 FORMAT(1X,'END COMPUTING CONTROL VALUES FOR TSGET')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV .LE. 5) GO TO  10
        WRITE(MESSU,2000)
 10   CONTINUE
C
C     find the time at end of the starting interval
C      for the dataset.
      CALL FIRINT(DELTAT,START,      TEMP)
      CALL YROFF(TEMP,      SINT)
C     find the tsb which contains the first interval
      VOYR = KEYS(BEGYR-BASEYR)
      CALL FITSB(VOYR,SINT)
C     tlast,tzero,bcwnov,bcwbti,votsb are defined by fitsb
C     we know that tzero <= inpstr < tlast
C
C     compute frmtim and votfrm.  frmtim should be the
C      ceiling of inpstr using increments of deltat
      FRMTIM = ((INPSTR+ DELTAT- 1)/DELTAT)*DELTAT
C
      GO TO (20,30,40,50), BCWBTI
 20     CONTINUE
C         uncompressed tsb
          VOTFRM = VOTSB+ 1+ ((FRMTIM-TZERO)/DELTAT)*TOTCOM
          GO TO 55
 30     CONTINUE
C         zero compressed
          VOTFRM = VOTSB+ 1
          GO TO 55
 40     CONTINUE
C         undefined compressed
          VOTFRM = VOTSB+ 1
          GO TO 55
 50     CONTINUE
C         linear variation
          VOTFRM = VOTSB+ 1+ TOTCOM
 55   CONTINUE
      IF (OUTLEV .LE. 6) GO TO 60
        WRITE(MESSU,2010)
 60   CONTINUE
C
      RETURN
      END
C
C     3.5.8.01
C
      SUBROUTINE GWORD
     I                 (ADDR,
     O                  WORD)
C     + + + PURPOSE + + +
C     Get a word from the tss dataset at the given address.
C     current buffer must be valid and available for reuse
C     This subroutine is closely related to gtword.
C      any bugs detected here may be present there.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ADDR
      REAL      WORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADDR   - ???
C     WORD   - ???
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,RECNO
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,RBUFF,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 216
      IF (ADDR .GE. 1) GO TO 10
C       program bug
C       buffer underflow
        SGRP = 130
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 10   CONTINUE
C
      RECNO= (ADDR-1)/RECLT +FREC
      IF (RECNO .LE. LREC) GO TO 20
C       error
C       attempt to obtain data beyond upper limit of dataset
        SGRP = 131
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 20   CONTINUE
C
      IF (RECNO .EQ. TRCNO) GO TO 30
        CALL RBUFF (RECNO,RECLT,TSSFL,      TBUFF)
        TRCNO  = RECNO
        VOBUFF = (TRCNO-FREC)*RECLT
        ENDBUF = VOBUFF +RECLT
        BMTFLG = 0
        BADR   = 1
 30   CONTINUE
C
C     get the word from the buffer
      WORD = TBUFF(ADDR-VOBUFF)
C
      RETURN
      END
C
C     3.5.8.2.3.3
C
      SUBROUTINE NEWYR
     I                (YR,PYR,DELT)
C
C     + + + PURPOSE + + +
C     Set up initial bcw's for a new year. keys is not updated until
C     the end of the calendar year or the end of the run
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     YR,PYR,DELT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - ???
C     PYR    - ???
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- VERSION INTERP4 + + +
      INCLUDE     'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     N,OFF(20),SPLIT(2),SCLU,SGRP,
     $            ADR,VO,VOFREE,WORDI
      REAL        VAL(20),WORDR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WORDR,WORDI),(VAL(1),SPLIT(1))
C
C     + + + EXTERNALS + + +
      EXTERNAL  WBUFF,GWORD,OMSTI,OMSG,PVAL
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 216
C     wordr and wordi occupy the same space in the data structure,
C      as do val(1) and split(1)
C
C     save the value of vofrwd because call to gword destroys
C       the contents of the buffer.
      VOFREE=VOFRWD
      IF (PYR .LE. 0) GO TO 20
C       update linking bcw in previous year
        VO = VOLFRM+TOTCOM
C       get linking bcw
        ADR = VO+1
        CALL GWORD(ADR,      WORDR)
        BCW = WORDI
        IF (BCW .EQ. 0) GO TO 10
C         program bug
          SGRP = 138
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 10     CONTINUE
        WORDI = -VOFREE
        TBUFF(VO-VOBUFF+1) = WORDR
        CALL WBUFF (TRCNO,RECLT,TSSFL,TBUFF)
        BMTFLG = 1
        TRCNO  = 0
 20   CONTINUE
C
C     set up the year word and the initial bcw for the new year
C     tsb is initiated as uncompressed
      SPLIT(1) = YR
      SPLIT(2) = 0
      BCWBTI   = 1
      BCWNOV   = 0
      VOTSB    = VOFREE+1
      VOTFRM   = VOTSB+1
      TZERO    = 0
      TLAST    = -DELTAT
      FRMTIM   = TZERO
      NREM     = DELTAT/DELT
      ADR      = VOFREE+1
C     make sure correct record is in buffer
      CALL GWORD(ADR,      WORDR)
      CWF      = 1
      FORFLG   = 1
      OFF(1)   = 0
      N        = 2
      CALL PVAL(N,OFF,VAL,VOFREE)
C
      RETURN
      END
C
C     3.5.8.2.3
C
      SUBROUTINE PINIT
     I                 (START,ENDR,STIME,FMIN,DELT)
C
C     + + + PURPOSE + + +
C     Compute control values for the instruction file for tsput
C     for references to the tss and adjust the datasets in the tss
C     so that they are consistent with the instruction file
C     We assume that the entire dataset label, entire instruction and
C     workspace are in common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   START(5),ENDR(5),DELT,STIME,FMIN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     START  - ???
C     ENDR   - ???
C     STIME  - ???
C     FMIN   - ???
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TEMP(5),YR,SCLU,SGRP,PYR,SKIPFG,SINT,VOYR
C
C     + + + INTRINSICS + + +
      INTRINSIC  IABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  FIRINT,YROFF,OMSG,FLFRM,FITSB,ADJTSB,NEWYR
      EXTERNAL  ADJLAB,FNDGAP,OMSTI
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(1X,'BEGIN COMPUTING CONTROL VALUES FOR TSPUT')
 2020 FORMAT(1X,'END COMPUTING CONTROL VALUES FOR TSPUT')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 216
      SKIPFG= 0
C
      IF (OUTLEV .LE. 6) GO TO 10
        WRITE(MESSU,2000)
 10   CONTINUE
C
C     compute date/time and minute of the year for the starting interv
C      in the dataset.  note that sint > stime at all times.
      CALL FIRINT(DELTAT,START,      TEMP)
      CALL YROFF(TEMP,      SINT)
C     establish year at end of run
C
      YR=TEMP(1)
C
C     check for invalid access method
      IF (YEAROR .NE. 1) GO TO 50
        IF (AMODE .NE. 1 .AND. AMODE .NE. 2 ) GO TO  40
          IF (LASTYR .LE. 0) GO TO 30
            IF (YR .LE. LASTYR+1) GO TO 20
C             error
C             gap of one or more years using add or insert for a data-
C             set which requires data to be in chronological order
              SGRP = 124
              CALL OMSTI (FILE)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
C             set flag to suppress a subsequent program bug message
              SKIPFG= 1
 20         CONTINUE
 30       CONTINUE
 40     CONTINUE
        GO TO  70
 50   CONTINUE
        IF (AMODE .NE. 1 ) GO TO 60
C         error
C         attempt to use add access on a dataset in which data is not
C         in chronological order
          SGRP = 125
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 60     CONTINUE
 70   CONTINUE
C
C     search for the starting interval in the dataset
      IF (YR .GT. BASEYR) GO TO 80
C       error
C       starting year is earlier than first year permitted
C       in the keying system
        SGRP = 126
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 80   CONTINUE
      PYR=YR-1
      IF (PYR .NE. BASEYR) GO TO  90
        PYR=0
        GO TO 110
 90   CONTINUE
        IF (KEYS(PYR-BASEYR) .NE. 0) GO TO 100
          PYR=0
 100    CONTINUE
 110  CONTINUE
      VOYR=KEYS(YR-BASEYR)
      IF (VOYR .LE. 0) GO TO 190
C       year exists in the dataset
        LGAP=0
        IF (AMODE .NE.  2 ) GO TO  120
C         error
C         attempt to access dataset with insert for a year
C         which already contains data
          SGRP = 127
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 120    CONTINUE
        IF (FMIN .NE. 1) GO TO 150
C         start time is on year boundary
          IF (PYR .LE. 0) GO TO  130
C           previous year exists in the dataset
            CALL FLFRM(YR,BASEYR,KEYS,TOTCOM,VOFRWD,      VOLFRM)
            CALL FITSB(VOYR,SINT)
            CALL ADJTSB(STIME,SINT,DELT)
            GO TO 140
 130      CONTINUE
C           previous year does not exist
            VOLFRM=0
            CALL FITSB(VOYR,SINT)
            CALL ADJTSB(STIME,SINT,DELT)
 140      CONTINUE
          GO TO    180
 150    CONTINUE
C         start time is not on a year boundary
          CALL FITSB(VOYR,SINT)
          IF (STIME .NE. TZERO) GO TO  160
C           compute address of preceding frame
            VOLFRM=VOTSB-TOTCOM
            GO TO 170
 160      CONTINUE
            VOLFRM=0
 170      CONTINUE
          CALL ADJTSB(STIME,SINT,DELT)
 180    CONTINUE
        GO TO 270
 190  CONTINUE
C       year does not exist in the dataset
        IF (AMODE .NE. 3 ) GO TO 200
C         error
C         attempt to use replace for a year without data
          SGRP = 128
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 200    CONTINUE
        LGAP=(STIME+DELTAT-1)/DELTAT
        IF (PYR .LE. 0) GO TO  210
C         previous year exists
          CALL FLFRM(YR,BASEYR,KEYS,TOTCOM,VOFRWD,      VOLFRM)
          CALL NEWYR(YR,PYR,DELT)
          GO TO 260
 210    CONTINUE
          IF (LASTYR .LE. 0) GO TO 240
C           some data exists
            IF (AMODE .NE. 2 ) GO TO 220
              VOLFRM=0
              CALL NEWYR(YR,PYR,DELT)
              GO TO 230
 220        CONTINUE
              IF (YR .GE. LASTYR) GO TO 225
C               add is being used to place data before any pre-
C               existing data in the dataset. initialize as for
C               an empty dataset
                VOFRWD= LBLSZ
                VOLFRM= 0
                CALL NEWYR(YR,PYR,DELT)
                GO TO 228
 225          CONTINUE
C               error - we should not be here at all. if yr= lastyr
C               then keys(yr-baseyr) should be >0 and we should
C               have taken a different path. if yr> lastyr then
C               either data exists for the previous year or an
C               error should be detected earlier. however that
C               error does not stop processing so that we must add
C               a flag to prevent spurious program bug reports
                IF (SKIPFG .EQ. 1) GO TO 226
C                 program bug or invalid dataset label - improper
C                 branching
                  SGRP = 129
                  CALL OMSTI (FILE)
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
 226            CONTINUE
 228          CONTINUE
 230        CONTINUE
            GO TO 250
 240      CONTINUE
C           no data exists
            VOLFRM=0
            CALL NEWYR(YR,PYR,DELT)
 250      CONTINUE
 260    CONTINUE
 270  CONTINUE
      IF (AMODE .EQ.  3 ) GO TO 280
        CALL ADJLAB(YR)
 280  CONTINUE
      IF (AMODE .NE.  3 ) GO TO 290
        TGAP=0
        GO TO 300
 290  CONTINUE
        CALL FNDGAP(ENDR,DELTAT,      TGAP)
 300  CONTINUE
C     set nature of leading and trailing gaps
      IF (GAPCOD .LE. 0) GO TO 310
        GAPVAL=0.0
        GO TO 320
 310  CONTINUE
        GAPVAL=-1.E30
 320  CONTINUE
      IF (COMPR .NE. 1) GO TO 370
        IF (LGAP .LE. 0) GO TO 340
          IF (IABS(GAPCOD) .LE. 2) GO TO 330
            LGAP=-LGAP
 330      CONTINUE
 340    CONTINUE
        IF (TGAP .LE. 0) GO TO 360
          IF (IABS(GAPCOD) .NE. 2 .AND. IABS(GAPCOD) .NE. 4) GO TO 350
            TGAP=-TGAP
 350      CONTINUE
 360    CONTINUE
 370  CONTINUE
      IF (OUTLEV .LE. 6) GO TO 380
        WRITE(MESSU,2020)
 380  CONTINUE
C
C
      RETURN
      END
C
C     3.5.2.2.1.2
C
      SUBROUTINE PROMEM
     I                  (MESSU,MSGFL,MNUM,VOMEM,SUB,NSUB,
     I                   MEMNAM,MKIND,SVOLNO,OUTLEV,MAXTTB,
     M                   NTS,ECOUNT,
     O                   TABL,TABLR)
C
C     + + + PURPOSE + + +
C     Process a member
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,MSGFL,MNUM,VOMEM,SUB,NSUB,
     $            MKIND(20),SVOLNO,MAXTTB,
     $            OUTLEV,NTS,ECOUNT,TABL(10,MAXTTB)
      REAL        TABLR(10,MAXTTB)
      CHARACTER*6 MEMNAM(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     MNUM   - ???
C     VOMEM  - ???
C     SUB    - ???
C     NSUB   - ???
C     MEMNAM - ???
C     MKIND  - ???
C     SVOLNO - ???
C     OUTLEV - run interpreter output level
C     MAXTTB - ???
C     NTS    - ???
C     ECOUNT - count(s) of specific errors
C     TABL   - ???
C     TABLR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,K,OFFS,I
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT(' BEGIN PROCESSING MEMBER/MEMBERS')
 2020 FORMAT(' END PROCESSING MEMBER/MEMBERS')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 216
      IF (OUTLEV .GT. 7) WRITE(MESSU,2010)
C
      IF (SUB .NE. 0) GO TO 30
C       all components implied
        DO 20 I=1,NSUB
          NTS =NTS+1
          OFFS=VOMEM+I
          READ (MEMNAM(MNUM),1000) (TABL(K,NTS),K=1,2)
          TABL(3,NTS) =I
          TABL(4,NTS) =0
          TABL(5,NTS) =OFFS
          TABL(6,NTS) =MKIND(MNUM)
          TABLR(8,NTS)=0.0
          TABLR(9,NTS)=1.0
 20     CONTINUE
C
        GO TO 70
 30   CONTINUE
C
C       single subscript implied
        IF (SUB .GE. 1 .AND. SUB .LE. NSUB) GO TO 40
C         error-member subscript out of range
          SGRP = 90
          CALL OMSTI (SVOLNO)
          CALL OMSTI (SUB)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
C
          GO TO 60
 40     CONTINUE
C
          NTS =NTS+1
          OFFS=VOMEM+SUB
          READ (MEMNAM(MNUM),1000) (TABL(K,NTS),K=1,2)
          TABL(3,NTS) =SUB
          TABL(4,NTS) =0
          TABL(5,NTS) =OFFS
          TABL(6,NTS) =MKIND(MNUM)
          TABLR(8,NTS)=0.0
          TABLR(9,NTS)=1.0
 60     CONTINUE
 70   CONTINUE
C
      IF (OUTLEV .GT. 7) WRITE(MESSU,2020)
C
      RETURN
      END
C
C     3.5.8.02
C
      SUBROUTINE PVAL
     I                (N,OFF,VAL,VO)
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
C     + + + COMMON BLOCKS- INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,I,J,NVO
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTI,OMSG,WBUFF,MEXT
C
C     + + + END SPECIFICATIONS + + +
C
C     check buffer state
C
      SCLU = 216
      IF (BMTFLG .NE.  1) GO TO 10
C       program bug
C       invalid buffer state
        SGRP = 139
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 10   CONTINUE
C
C     compute virtual origin for buffer operations
C
      NVO=VO -VOBUFF
      IF (NVO .GE. 0) GO TO 20
C       program bug
C       buffer underflow
        SGRP = 140
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 20   CONTINUE
      IF (OFF(1) .LE. 0) GO TO 40
        IF (NVO+OFF(N) .LE. BLEN) GO TO 30
C         program bug
C         buffer/extension overflow
          SGRP = 141
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
 30     CONTINUE
        GO TO 60
 40   CONTINUE
        IF (NVO+N .LE. BLEN) GO TO 50
C         program bug
C         buffer/extension overflow
          SGRP = 141
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
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
      IF (FORFLG .NE. 1) GO TO 150
C       write buffer/extension
        BMTFLG= 1
        CALL WBUFF (TRCNO,RECLT,TSSFL,TBUFF)
        VOBUFF=VOBUFF +RECLT
C
C       move extension into buffer
C
        CALL MEXT (RECLT,BLEN,BADR,TBUFF)
C
C       write buffer again
C
        IF (BADR .LE. 0) GO TO 140
          TRCNO=TRCNO +1
          IF (TRCNO .LE. LREC) GO TO 130
C           error
C           dataset overflow
            SGRP = 142
            CALL OMSTI (FILE)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
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
            EXTF= 0
            CALL WBUFF (TRCNO,RECLT,TSSFL,TBUFF)
            CALL MEXT  (RECLT,BLEN,  BADR,TBUFF)
            TRCNO=TRCNO +1
            IF (TRCNO .LE. LREC) GO TO 160
C             error
C             dataset overflow
              SGRP = 142
              CALL OMSTI (FILE)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
 160        CONTINUE
            VOBUFF=VOBUFF +RECLT
 170      CONTINUE
 180    CONTINUE
 190  CONTINUE
C
      RETURN
      END
C
C     3.5.8.2.3.2.1
C
      SUBROUTINE RPUNC
     I                (STIME,SINT,DELT)
C
C     + + + PURPOSE + + +
C     Compute control words for replace access mode
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,STIME,SINT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIME  - ???
C     SINT   - ???
C     DELT   - simulation time interval in minutes
C
C     + + + COMMON BLOCKS- VERSION INTERP4 + + +
      INCLUDE   'crin4.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,N,REM
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
C     check for invalid tsb
      SCLU = 216
      IF (BCWBTI .EQ. 1) GO TO 10
C       error
C       invalid time series block encountered during setup
C       for replace access.  block must be type uncompressed.
        SGRP = 134
        CALL OMSTI (FILE)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 10   CONTINUE
C
      IF (STIME .NE. TZERO) GO TO 20
C       starting time is at initial point of tsb
        NREM  =RATIO
        VOTFRM=VOTSB +1
        FRMTIM=STIME
        GO TO 50
 20   CONTINUE
        N   =(STIME-TZERO)/DELTAT
        REM =(STIME-TZERO) -N*DELTAT
        NREM=RATIO -REM/DELT
C
        IF (REM .NE. 0) GO TO 30
C         starting point at point stored in tsb but
C         not initial point nor last point
          VOTFRM=VOTSB +TOTCOM*N +1
          FRMTIM=STIME
          GO TO 40
 30     CONTINUE
C         starting point not at any point stored in the tsb
          VOTFRM=VOTSB +(N+1)*TOTCOM +1
          FRMTIM=SINT
 40     CONTINUE
C
 50   CONTINUE
C
      RETURN
      END
C
C     3.5.2.2.1
C
      SUBROUTINE TSSDS
     I                 (MSGFL,MESSU,TSSFL,MEMN,MEMSB,
     I                  AMDST,SDATIM,EDATIM,VOLNO,TRFLAG,MXTSTB,
     O                  NUM,DELT,UNT,NTS,AMDCD,FRC,
     O                  TABL,TABLR)
C
C     + + + PURPOSE + + +
C     Check and if necessary expand a users reference to a tss
C      dataset
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,TSSFL,MEMSB,MXTSTB,
     $            SDATIM(5),EDATIM(5),VOLNO,TRFLAG,
     $            NUM,DELT,UNT,NTS,AMDCD,FRC,TABL(10,MXTSTB)
      REAL        TABLR(10,MXTSTB)
      CHARACTER*6 MEMN
      CHARACTER*4 AMDST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     TSSFL  - fortran unit number of time series store file
C     MEMN   - ???
C     MEMSB  - ???
C     AMDST  - ???
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     VOLNO  - ???
C     TRFLAG - ???
C     MXTSTB - ???
C     NUM    - ???
C     DELT   - simulation time interval in minutes
C     UNT    - ???
C     NTS    - ???
C     AMDCD  - ???
C     FRC    - ???
C     TABL   - ???
C     TABLR  - ???
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE   'crin3.inc'
      INCLUDE   'crin3c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      EYR,SYR,SCLU,SGRP,I,I3,I4,I6,OK,VOMEM,N,
     $             MNUM,NSUB
      CHARACTER*6  CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER    DSCHK,CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   DSCHK,CHKSTR,OMSG,OMSTC,OMSTI,PROMEM
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,A2)
 2010 FORMAT (' BEGIN CHECKING/EXPANDING TSS REFERENCE')
 2020 FORMAT (' END CHECKING/EXPANDING TSS REFERENCE')
C
C     + + + END SPECIFICATIONS + + +
C
      I3  = 3
      I4  = 4
      I6  = 6
      SCLU= 216
      IF (OUTLEV .GT. 6) THEN
        WRITE(MESSU,2010)
      END IF
C
      NTS= 0
C     initialize unt in case dataset not found in tss
C     subr memts needs a valid value of eunits
      UNT= 0
      IF (DSCHK(MESSU,MSGFL,TSSFL,VOLNO) .GT. 0) THEN
C       dataset exists and label is already present in common
C       check for data being present for all years of the run
        EYR= EDATIM(1)
        SYR= SDATIM(1)
        IF (SDATIM(2) .EQ. 12) THEN
          IF (SDATIM(3) .EQ. 31) THEN
            IF (SDATIM(4) .EQ. 24) THEN
              IF (SDATIM(5) .EQ. 60) THEN
                SYR= SYR +1
              END IF
            END IF
          END IF
        END IF
C
        SYR= SYR -BASEYR
        EYR= EYR -BASEYR
        IF (SYR .LT. 1 .OR. SYR .GT. 100) THEN
C         error-year containing initial interval of the run is
C         outside the valid range for the dataset.
C         valid range is from baseyr+1 through baseyr+100.
          SGRP = 80
          CALL OMSTI (VOLNO)
          CALL OMSTI (BASEYR)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (TRFLAG .EQ. 1) THEN
          IF (DSSEC .EQ. 1) THEN
C           error - dataset is write protected -
C           unable to output data to the given
C           external target
            SGRP = 81
            CALL OMSTI (VOLNO)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
        IF (EYR .LT. 1 .OR. EYR .GT. 100) THEN
C         error-year containing final interval of the run
C         lies outside the valid range for the dataset.
C         the valid range is from baseyr+1 through baseyr+100.
          SGRP = 82
          CALL OMSTI (VOLNO)
          CALL OMSTI (BASEYR)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (TRFLAG .EQ. 0) THEN
          OK= 1
          DO 65 I=SYR,EYR
            IF (KEYS(I) .EQ. 0) OK= 0
 65       CONTINUE
          IF (OK .EQ. 0) THEN
C           error-one or more years of data missing in the dataset
            SGRP = 83
            CALL OMSTI (VOLNO)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       set general data
        NUM = VOLNO
        DELT= DSDELT
        FRC = DSFREC
        UNT = UNITS
C       process access mode
        IF (TRFLAG .EQ. 1) THEN
          CHSTR(1:4)= AMDST
          AMDCD     = CHKSTR(I4,I3,CHSTR1,AMDKW1)
          IF (AMDCD .EQ. 0) THEN
C           error - unknown access mode given
            SGRP = 84
            CALL OMSTI (VOLNO)
            CHSTR= AMDST
            CALL OMSTC (I4,CHSTR1)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       copy member name from label to character common
        DO 100 I= 1, NMEMS
          WRITE (MEMNAM(I),2000) (IMEMNM(N,I),N=1,2)
 100    CONTINUE
C
C       process member/subscript information
        IF (MEMN .EQ. '      ') THEN
C         member name is blank. all members implied
          VOMEM=0
          DO 120 I=1,NMEMS
            NSUB = MSUB(I)
            CALL PROMEM (MESSU,MSGFL,I,VOMEM,MEMSB,NSUB,
     I                   MEMNAM,MKIND,SVOLNO,OUTLEV,MXTSTB,
     M                   NTS,ECOUNT,
     O                   TABL,TABLR)
            VOMEM= VOMEM+NSUB
 120      CONTINUE
        ELSE
C         user has specified a single member. find it.
          CHSTR= MEMN
          MNUM = CHKSTR(I4,NMEMS,CHSTR1,MEMNA1)
C
          IF (MNUM .GT. 0) THEN
C           match found
            VOMEM= 0
            N    = MNUM -1
            IF (N .GT. 0) THEN
              DO 140 I= 1,N
                VOMEM= VOMEM +MSUB(I)
 140          CONTINUE
            END IF
C
            NSUB=MSUB(MNUM)
            CALL PROMEM (MESSU,MSGFL,MNUM,VOMEM,MEMSB,NSUB,
     I                   MEMNAM,MKIND,SVOLNO,OUTLEV,MXTSTB,
     M                   NTS,ECOUNT,
     O                   TABL,TABLR)
C
          ELSE
C           error-given member name not found
            SGRP = 85
            CALL OMSTI (VOLNO)
            CHSTR= MEMN
            CALL OMSTC (I6,CHSTR1)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
      ELSE
C       error-dataset not found in tss
        SGRP = 86
        CALL OMSTI (VOLNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (OUTLEV .GT. 6) THEN
        WRITE(MESSU,2020)
      END IF
C
      RETURN
      END
