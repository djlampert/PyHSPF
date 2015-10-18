C
C
C
      SUBROUTINE PROWDS
     I                  (MSGFL,MESSU,WDMSFL,VOLNO,MEMN,MEMSB,SYST,
     I                   GAPST,AMDST,SDATIM,EDATIM,TRFLAG,
     I                   SYSKW1,AMDKW1,GAPKW1,IHMFG,
     M                   ECOUNT,
     O                   DELT,UNT,KIND,GAPCD,AMDCD,QLFG)
C
C     + + + PURPOSE + + +
C     check attributes for a reference to a time series dataset
C     in a wdm file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,WDMSFL,VOLNO,MEMSB(2),
     $            SDATIM(5),EDATIM(5),TRFLAG,IHMFG,
     $            ECOUNT,DELT,UNT,KIND,GAPCD,AMDCD,QLFG
      CHARACTER*6 MEMN
      CHARACTER*4 GAPST,SYST,AMDST
      CHARACTER*1 AMDKW1(12),SYSKW1(8),GAPKW1(8)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     WDMSFL - watershed data management file unit number
C     VOLNO  - ???
C     MEMN   - ???
C     MEMSB  - ???
C     SYST   - ???
C     GAPST  - ???
C     AMDST  - ???
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TRFLAG - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     SYSKW1 - ???
C     AMDKW1 - access mode keyword library
C     GAPKW1 - ???
C     ECOUNT - count(s) of specific errors
C     DELT   - dataset time interval in minutes
C     UNT    - ???
C     KIND   - ???
C     GAPCD  - ???
C     AMDCD  - ???
C     QLFG   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SAIND,RNSTRT(6),RNEND(6),RETCOD,RETCD1,
     $            RETCD2,DSSTRT(6),DSEND(6),TCODE,TSSTEP,GPFLG,TDSFRC,
     $            RWFLAG,STRTFG,ENDFG,UNTDS,TSFORM,SCLU,SGRP,
     $            I,I1,I2,I3,I4,OK,ERRCOD,ITMP(1),
     $            UNTUSR,TIMFCT(5),AGGRCD,CONV(7),VBTIME
      REAL        TSFILL,RTMP(1)
      CHARACTER*4 TSTYP
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TSTYP,TSTYP1)
      CHARACTER*1 TSTYP1(4)
      CHARACTER*4  CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C     + + + EXTERNALS + + +
      EXTERNAL    TIMHTW,WDBSGC,OMSG,OMSTI,OMSTC,WDBSGI,WTFNDT,TIMADD
      EXTERNAL    CHKSTR,CKDATE,WDBSGR
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        CONV/60,60,24,999,12,100,999/
      DATA        TIMFCT/0,1,60,1440,43200/
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 217
      I1  = 1
      I2  = 2
      I3  = 3
      I4  = 4
C     convert run times from hspf internal format to wdm
      DO 10 I = 1,5
        RNSTRT(I) = SDATIM(I)
        RNEND(I)  = EDATIM(I)
 10   CONTINUE
      RNSTRT(6) = 0
      RNEND(6)  = 0
      CALL TIMHTW (MESSU,MSGFL,
     M             RNSTRT)
      CALL TIMHTW (MESSU,MSGFL,
     M             RNEND)
C
C     check memn (wdm attribute = tstyp)
      SAIND = 1
      CALL WDBSGC (WDMSFL,VOLNO,SAIND,I4,
     O             TSTYP1,RETCOD)
C
      IF (RETCOD .EQ. 0) THEN
C       attribute tstyp found - check it
        IF (TSTYP .NE. MEMN(1:4)) THEN
C         error - dataset tstyp does not match input memn
          CALL OMSTI (VOLNO)
          CALL OMSTI (MEMSB(2))
          CHSTR= MEMN(1:4)
          CALL OMSTC (I4,CHSTR1)
          CHSTR= TSTYP
          CALL OMSTC (I4,CHSTR1)
          SGRP = 99
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE
C       error - attribute tstyp not found
        CALL OMSTI (VOLNO)
        CALL OMSTI (MEMSB(2))
        SGRP = 100
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C
C     check delt (wdm attributes = tcode and tsstep)
      SAIND = 17
      CALL WDBSGI (WDMSFL,VOLNO,SAIND,I1,
     O             ITMP,RETCD1)
      TCODE = ITMP(1)
C
      SAIND = 33
C
      CALL WDBSGI (WDMSFL,VOLNO,SAIND,I1,
     O             ITMP,RETCD2)
      TSSTEP= ITMP(1)
C
      IF (RETCD1 .EQ. 0 .AND. RETCD2 .EQ. 0) THEN
C       dataset is a timeseries dataset - determine delt
        IF (TCODE .GE. 2 .AND. TCODE .LE. 5) THEN
C         valid tcode
          DELT = TSSTEP * TIMFCT(TCODE)
        ELSE
C         error - invalid tcode for hspf; must be min., hr., or day
C         month ok for source
C         assign dummy time step for subsequent checks
          TCODE  = 2
          TSSTEP = 60
          CALL OMSTI (VOLNO)
          CALL OMSTI (MEMSB(2))
          SGRP = 101
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE
C
C       error - time step not valid, so dataset is not timeseries;
C       assign dummy time step for subsequent checks
        TCODE  = 2
        TSSTEP = 60
        CALL OMSTI (VOLNO)
        CALL OMSTI (MEMSB(2))
        SGRP = 102
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C
C     check time span
      GPFLG = 1
      CALL WTFNDT (WDMSFL,VOLNO,GPFLG,
     O             TDSFRC,DSSTRT,DSEND,RETCOD)
C
C
      IF (TRFLAG .EQ. 0) THEN
C       source dataset - compare time span of dataset to time span of run
        OK = 0
        IF (RETCOD .EQ. 0) THEN
C         move run start time forward 1 'data' interval
C          write(99,*) 'rnstrt',rnstrt
          CALL TIMADD (RNSTRT,TCODE,TSSTEP,I1,
     O                 RNSTRT)
          CALL CKDATE (DSSTRT,RNSTRT,
     O                 STRTFG)
          CALL CKDATE (RNEND,DSEND,
     O                 ENDFG)
          IF (STRTFG .LE. 0 .AND. ENDFG .LE. 0) OK = 1
        END IF
        IF (OK .EQ. 0) THEN
C         error - run time span not within dataset time span
C          write(99,*) dsstrt
C          write(99,*) rnstrt
C          write(99,*) strtfg
C          write(99,*) rnend
C          write(99,*) dsend
C          write(99,*) endfg
          CALL OMSTI (VOLNO)
          CALL OMSTI (MEMSB(2))
          SGRP = 103
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
C         missing data not fatal for ihm operation
          IF (IHMFG .EQ. 1) THEN
C            WRITE(99,*) 'PROWDS:missing data IGNORE!',WDMSFL,VOLNO
            ECOUNT = ECOUNT-1
          END IF
        END IF
      ELSE
C       target dataset - check access mode, write flag, and aggregation
C       access mode
        CHSTR= AMDST
        AMDCD= CHKSTR(I4,I3,CHSTR1,AMDKW1)
C
        IF (AMDCD .NE. 1 .AND. AMDCD .NE. 3) THEN
C         access mode is invalid for wdm
          CALL OMSTI (VOLNO)
          CALL OMSTI (MEMSB(2))
          SGRP = 106
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (RETCOD .EQ. 0 .AND. AMDCD .EQ. 1) THEN
          CALL CKDATE (DSEND,RNSTRT,
     O                 ERRCOD)
          IF (ERRCOD .GT. 0) THEN
C           error - overwrite of data not allowed, but data are
C           already present in dataset after run start time
            CALL OMSTI (VOLNO)
            CALL OMSTI (MEMSB(2))
            SGRP = 109
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       check write flag
        SAIND = 35
        CALL WDBSGI (WDMSFL,VOLNO,SAIND,I1,
     O               ITMP,RETCOD)
        RWFLAG= ITMP(1)
        IF (RETCOD .EQ. 0) THEN
          IF (RWFLAG .EQ. 1) THEN
C           error - write not allowed to target dataset
            CALL OMSTI (VOLNO)
            CALL OMSTI (MEMSB(2))
            SGRP = 104
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       aggregation
        IF (GAPST .EQ. '    ') THEN
C         default: aggregation turned off
          AGGRCD = 0
        ELSE IF (GAPST .EQ. 'AGGR') THEN
C         aggregate output data; will check for compatibility of
C         time steps in subroutine pairs
          AGGRCD = 1
        ELSE
C         error in user-supplied input for aggregation keyword
          AGGRCD = 0
          CALL OMSTI (VOLNO)
          CALL OMSTI (MEMSB(2))
          SGRP = 149
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C       gapcd is used to 'carry' aggrcd since gapcd not needed for target
        GAPCD = AGGRCD
C
C       check vbtime
        SAIND = 85
        CALL WDBSGI (WDMSFL,VOLNO,SAIND,I1,
     O               ITMP,RETCOD)
        VBTIME= ITMP(1)
C
        IF (VBTIME .EQ. 1) THEN
C         constant time step, check tsstep, tcode
C         and modify gapcd for use in pairs
          GAPCD= GAPCD + 2
          IF (MOD(TSSTEP,CONV(TCODE)) .EQ. 0) THEN
C           error
            CALL OMSTI (VOLNO)
            CALL OMSTI (MEMSB(2))
            SGRP = 150
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       dont allow month timestep for target wdm dataset
        IF (TCODE .GT. 4) THEN
C         error - invalid tcode for wdm target; must be min., hr., or day
C         assign dummy time step for subsequent checks
          TCODE  = 2
          TSSTEP = 60
          CALL OMSTI (VOLNO)
          CALL OMSTI (MEMSB(2))
          SGRP = 101
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C
C     check units system of dataset and the user-supplied system
C thj this check is currently a dummy - no attribute 999 defined
      SAIND = 999
      CALL WDBSGI (WDMSFL,VOLNO,SAIND,I1,
     O             ITMP,RETCOD)
      UNTDS = ITMP(1)
C
      CHSTR = SYST
      UNTUSR= CHKSTR(I4,I2,CHSTR1,SYSKW1)
C
      ERRCOD = 0
      IF (RETCOD .EQ. 0 .AND. UNTUSR .NE. 0) THEN
C       dataset units valid & user units valid; error if different
        UNT = UNTDS
        IF (UNTDS .NE. UNTUSR) ERRCOD = 1
      ELSE IF (RETCOD .EQ. 0 .AND. UNTUSR .EQ. 0) THEN
C       dataset units valid & user units invalid; error if user not blank
        UNT = UNTDS
        IF (SYST .NE. '    ') THEN
C         user string was invalid and not blank
          ERRCOD = 2
        END IF
      ELSE IF (RETCOD .NE. 0 .AND. UNTUSR .NE. 0) THEN
C       dataset units invalid & user units valid; no error
        UNT = UNTUSR
      ELSE IF (RETCOD .NE. 0 .AND. UNTUSR .EQ. 0) THEN
C       dataset units invalid & user units invalid; error if user not blank
        UNT    = 1
        IF (SYST .NE. '    ') THEN
C         user string was invalid and not blank
          ERRCOD = 3
        END IF
      END IF
      IF (ERRCOD .NE. 0) THEN
C       unit system error
        CALL OMSTI (VOLNO)
        CALL OMSTI (MEMSB(2))
        CALL OMSTI (ERRCOD)
        SGRP = 105
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C
C     check kind (tsform) of dataset - if attribute not found, set
C     kind = undefined
      SAIND = 84
      CALL WDBSGI (WDMSFL,VOLNO,SAIND,I1,
     O             ITMP,RETCOD)
      TSFORM= ITMP(1)
C
      IF (RETCOD .NE. 0 .OR. TSFORM .LT. 1 .OR. TSFORM .GT. 3) THEN
C       error, tsform is not found or not valid for hspf
        CALL OMSTI (VOLNO)
        CALL OMSTI (MEMSB(2))
        SGRP = 151
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        KIND= 3
      ELSE IF (TSFORM .EQ. 3) THEN
C       point
        KIND= 1
      ELSE
C       mean
        KIND= 2
      END IF
C
C
      IF (TRFLAG .EQ. 0) THEN
C       check user-input for gapst; set gapcd
        IF (GAPST .EQ. '    ') THEN
C         default to gap value of undefined
          GAPCD = 1
        ELSE
          CHSTR= GAPST
          GAPCD= CHKSTR(I4,I2,CHSTR1,GAPKW1)
          IF (GAPCD .EQ. 0) THEN
C           error - invalid gap value requested - undefined assumed
            CALL OMSTI (VOLNO)
            CALL OMSTI (MEMSB(2))
            SGRP = 107
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          ELSE
            GAPCD = GAPCD - 1
          END IF
        END IF
C
C       check attribute tsfill; if it exists, use it, otherwise
C       use value determined above from user-input
        SAIND = 32
        CALL WDBSGR (WDMSFL,VOLNO,SAIND,I1,
     O               RTMP,RETCOD)
        TSFILL= RTMP(1)
C
        IF (RETCOD .EQ. 0) THEN
C         tsfill exists - compare tsfill to 0
          IF (TSFILL .LT. 0.001 .AND. TSFILL .GT. -0.001) THEN
            GAPCD = 0
          ELSE
            GAPCD = 1
          END IF
        END IF
      END IF
C
C
C     check user-input 'data quality code'
      IF (MEMSB(1) .GE. 0 .AND. MEMSB(1) .LE. 31) THEN
C       valid quality code found
        QLFG = MEMSB(1)
      ELSE
C       error - invalid quality code
        QLFG = 0
        CALL OMSTI (VOLNO)
        CALL OMSTI (MEMSB(2))
        SGRP = 108
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C
      RETURN
      END
C
C     3.5.2.2.3
C
      SUBROUTINE   WDMDS
     I                  (MSGFL,MESSU,WDMSFL,VOLNO,MEMN,MEMSB,GAPST,
     I                   GAPKW1,SYST,SYSKW1,AMDKW1,AMDST,SDATIM,
     I                   EDATIM,TRFLAG,OUTLEV,MAXTTB,IHMFG,
     M                   ECOUNT,
     O                   NUM,DELT,UNT,NTS,GAPCD,AMDCD,
     O                   TABL,TABLR)
C
C     + + + PURPOSE + + +
C     process a reference to a time series dataset in a wdm file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,WDMSFL,VOLNO,MEMSB(2),
     $            SDATIM(5),EDATIM(5),TRFLAG,OUTLEV,MAXTTB,IHMFG,
     $            ECOUNT,NUM,DELT,UNT,NTS,GAPCD,AMDCD,TABL(10,MAXTTB)
      REAL        TABLR(10,MAXTTB)
      CHARACTER*6 MEMN
      CHARACTER*4 GAPST,SYST,AMDST
      CHARACTER*1 AMDKW1(12),SYSKW1(8),GAPKW1(8)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     WDMSFL - watershed data management file unit number
C     VOLNO  - ???
C     MEMN   - ???
C     MEMSB  - ???
C     GAPST  - ???
C     GAPKW1 - ???
C     SYST   - ???
C     SYSKW1 - ???
C     AMDKW1 - access mode keyword library
C     AMDST  - ???
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TRFLAG - ???
C     OUTLEV - run interpreter output level
C     ECOUNT - count(s) of specific errors
C     MAXTTB - ???
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     NUM    - ???
C     DELT   - simulation time interval in minutes
C     UNT    - ???
C     NTS    - ???
C     GAPCD  - ???
C     AMDCD  - ???
C     TABL   - ???
C     TABLR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,KIND,QLFG,LREC,RETCOD,SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDDSCK,PROWDS,OMSG,OMSTI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT(' BEGIN CHECKING/EXPANDING A WDMS FILE REFERENCE')
 2020 FORMAT(' END CHECKING/EXPANDING A WDMS FILE REFERENCE')
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 217
      IF (OUTLEV .GT. 6) WRITE(MESSU,2010)
C
      NTS = 0
C     initialize unt in case dataset not found in wdm file
C     subroutine memts requires a valid value
      UNT = 0
C     determine whether dataset exists in wdmsfl
      CALL WDDSCK(WDMSFL,VOLNO,
     O            LREC,RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       dataset exists - process it
C
        CALL PROWDS (MSGFL,MESSU,WDMSFL,VOLNO,MEMN,MEMSB,SYST,GAPST,
     I               AMDST,SDATIM,EDATIM,TRFLAG,
     I               SYSKW1,AMDKW1,GAPKW1,IHMFG,
     M               ECOUNT,
     O               DELT,UNT,KIND,GAPCD,AMDCD,QLFG)
C
        NUM = VOLNO
        NTS = 1
        READ(MEMN,1000) (TABL(I,NTS),I=1,2)
        TABL(3,NTS)  = QLFG
        TABL(4,NTS)  = MEMSB(2)
        TABL(5,NTS)  = 0
        TABL(6,NTS)  = KIND
        TABLR(8,NTS) = 0.0
        TABLR(9,NTS) = 1.0
      ELSE
C       error - dataset does not exist
        CALL OMSTI (VOLNO)
        CALL OMSTI (MEMSB(2))
        SGRP = 98
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (OUTLEV .GT. 6) WRITE(MESSU,2020)
C
C
      RETURN
      END
