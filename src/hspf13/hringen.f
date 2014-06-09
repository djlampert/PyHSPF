C
C
C
      SUBROUTINE   ENDEXG
     I                   (INFG,XCOUNT,XGRPNO,GRPNO,XDELT,
     I                    MESSU,MSGFL,SCLU,
     M                    ECOUNT,WCOUNT,EXGTAB,
     O                    EXFG)
C
C     + + + PURPOSE + + +
C     User has explicitly or implicitly specified end exgroup - check
C     whether this is legal and make entry in exgtab
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,SCLU,EXFG,
     $           EXGTAB(5,10),GRPNO,INFG,MESSU,
     $           WCOUNT(10),XCOUNT,XDELT,XGRPNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFG   - ???
C     XCOUNT - ???
C     XGRPNO - ???
C     GRPNO  - ???
C     XDELT  - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing messages for this routine
C     ECOUNT - count(s) of specific errors
C     WCOUNT - ???
C     EXGTAB - ???
C     EXFG   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG
C
C     + + + END SPECIFICATIONS + + +
C
C     exgroup no longer active
      EXFG= 0
C
      IF (INFG.EQ.1) THEN
C       error - end exgroup found before end of current ingroup
        SGRP = 36
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (XCOUNT.EQ.0) THEN
C       warning - there are no operations in this exgroup -
C       instruction ignored
        SGRP = 4
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             WCOUNT(4))
      ELSE
C       write closing entries for this exgroup
        IF (XGRPNO.LE.10) THEN
          EXGTAB(2,XGRPNO)= GRPNO
          EXGTAB(3,XGRPNO)= XDELT
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ENDING
     I                   (INFG,EXFG,ICOUNT,XCOUNT,GRPNO,XGRPNO,OPNO,
     I                    NDELT,XDELT,MESSU,MSGFL,SCLU,OUTLEV,
     M                    ECOUNT,WCOUNT,
     O                    GRPTAB,EXGTAB)
C
C     + + + PURPOSE + + +
C     User has specified end ingroup - check whether this is legal and
C     make appropriate closing entries
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,EXFG,EXGTAB(5,10),GRPNO,GRPTAB(5,10),
     #           ICOUNT,INFG,MESSU,NDELT,OPNO,OUTLEV,WCOUNT(10),XCOUNT,
     #           XDELT,XGRPNO,SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFG   - ???
C     EXFG   - ???
C     ICOUNT - ???
C     XCOUNT - ???
C     GRPNO  - ???
C     XGRPNO - ???
C     OPNO   - ???
C     NDELT  - simulation time interval in minutes
C     XDELT  - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing messages for this routine
C     OUTLEV - run interpreter output level
C     ECOUNT - count(s) of specific errors
C     WCOUNT - ???
C     GRPTAB - ???
C     EXGTAB - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,ENDEXG,PMXTFT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INFG.EQ.0) THEN
C       error - end ingroup found, but corresponding valid ingroup
C       heading didn't precede it - command ignored
        SGRP = 35
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      ELSE
        INFG= 0
        IF (ICOUNT.EQ.0) THEN
C         warning - there are no operations in this ingroup -
C         instruction ignored
          SGRP = 3
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               WCOUNT(3))
        ELSE
          IF (GRPNO.LE.10) THEN
C           write closing entry for this ingroup
            GRPTAB(2,GRPNO)= OPNO
            GRPTAB(3,GRPNO)= NDELT
          END IF
        END IF
C
        IF (EXFG.EQ.0) THEN
C         not in a user specified exgroup.  close the generated
C         exgroup
          IF (OUTLEV.GE.5) THEN
            SGRP= 37
            CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
          END IF
          CALL ENDEXG (INFG,XCOUNT,XGRPNO,GRPNO,XDELT,
     I                 MESSU,MSGFL,SCLU,
     M                 ECOUNT,WCOUNT,EXGTAB,
     O                 EXFG)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GLOBLK
     I                    (MESSU,MSGFL,UPDFIL,
     M                     ECOUNT,
     O                     SEDAT,SDATIM,EDATIM,RUNMIN,OUTLEV,SPOUT,
     O                     RESMFG,RUNFG,EMFG,RNINFO,IHMFG)
C
C     + + + PURPOSE + + +
C     process the global block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MSGFL,MESSU,UPDFIL,ECOUNT,
     $             SEDAT(10),SDATIM(5),EDATIM(5),
     $             OUTLEV,SPOUT,RESMFG,RUNFG,RUNMIN,EMFG,IHMFG
      CHARACTER*80 RNINFO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     UPDFIL - update date file unit number
C     ECOUNT - count(s) of specific errors
C     SEDAT  - array containing both start and end dates
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     RUNMIN - span of run in minutes
C     OUTLEV - run interpreter output level
C     SPOUT  - runtime Special Action output level
C     RESMFG - resume flag - 0:standard mode, 1:resume mode
C     RUNFG  - run flag - 1:run with no errors
C                         0:just interp
C     EMFG   - english/metric units flag (english-1,metric-2)
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     RNINFO - character string of run information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      J,KEY,SCLU,SGRP,UNIT,I0,I1,I5,ITMP(1),INUM,ITYP,
     $             KEYST,KEYND,IOPT
      CHARACTER*80 UCIBF
C
C     + + + SAVE VARIABLES + + +
      INTEGER      NDAMON(12)
      SAVE         NDAMON
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UCIBF,UCIBF1)
      CHARACTER*1  UCIBF1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSG,OMSTI,OMSTC,STDATE,ENDATE,DIFTIM,GETUCI,COPYI
      EXTERNAL  PMXTFT,PMXTFI,PMXTFC,ZIPI,GETSE,HDMES2,LENSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAMON/31, 0,31,30,31,30,31,31,30,31,30,31/
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (2(I5,4I3,3X))
 1030 FORMAT (10X,I8,4(1X,I2),5X,I8,4(1X,I2))
 1040 FORMAT (25X,2I5)
 1050 FORMAT (9X,I5,5X,I5,31X,I5,5X,I5)
C
C     + + + OUTPUT FORMATS + ++
 2000 FORMAT (' UPSTART      ',I5,4I3,'  END   ',I5,4I3)
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C                 also added error messages for IHMFG, EMFG, and RUNFG
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 211
      I0= 0
      I1= 1
      I5= 5
C     default english/metric units to english
      EMFG= 1
C     find table in uci (type 2 from hspf.seq, grp 22, col 3
      ITYP= 2
      CALL GETSE (ITYP,I1,
     O            KEYST,KEYND)
      IF (KEYST .EQ. 0) THEN
C       error - global block must be present
        IF (MESSU .NE. 0) THEN
          SGRP= 108
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C       give variables dummy values to permit interpretation to continue
        J= 10
        CALL ZIPI (J,I0,
     O             SEDAT)
        RUNMIN= 0
        OUTLEV= 1
        SPOUT= 1
        RESMFG= 0
        RUNFG= 0
      ELSE
C       processing global block
        IOPT= 2
        CALL HDMES2 (IOPT,ITYP,I1)
        IF (MESSU .NE. 0) THEN
          SGRP= 109
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
        KEY= KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        IF (MESSU .NE. 0) THEN
          ITMP(1)= 80
          SGRP= 110
          CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,I1,ITMP,UCIBF1)
        END IF
C       put run info into output argument
        RNINFO= UCIBF
C       read dates and times, as user supplied them
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        IF (MESSU .NE. 0) THEN
          ITMP(1)= 80
          SGRP= 111
          CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,I1,ITMP,UCIBF1)
        END IF
        READ (UCIBF,1030,ERR=10) SEDAT
          GO TO 20
 10     CONTINUE
C         error - cannot read start and end dates
          ITMP(1)= 55
          CALL OMSTC (ITMP,UCIBF1)
          SGRP= 123
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ITMP(1)= 10
          CALL ZIPI (ITMP,I0,
     O               SEDAT)
 20     CONTINUE
        IF (UPDFIL .GT. 0) THEN
C         update start and end dates
          READ(UPDFIL,1010,ERR=25,END=25) SEDAT
            IF (MESSU .GT. 0) THEN
              WRITE(MESSU,2000) SEDAT
            END IF
            GOTO 28
 25       CONTINUE
C            WRITE(99,*) 'UCIGEN:failed to upd dates'
 28       CONTINUE
        END IF
        CALL COPYI (I5,SEDAT(1),
     O              SDATIM)
        CALL COPYI (I5,SEDAT(6),
     O              EDATIM)
C
C       check starting date/time for validity, convert to internal form
        CALL STDATE (NDAMON,MESSU,MSGFL,
     M               ECOUNT,SDATIM)
C
C       check ending date/time for validity, convert to internal format
        CALL ENDATE (NDAMON,MESSU,MSGFL,
     M               ECOUNT,EDATIM)
C
C       echo interp date
        IF (MESSU .NE. 0) THEN
          INUM= 10
          SGRP= 112
          CALL PMXTFI (MSGFL,MESSU,SCLU,SGRP,INUM,SEDAT)
        END IF
C
C       find and check the difference, in minutes, between starting
C       and ending date/times
        CALL DIFTIM (SDATIM,EDATIM,NDAMON,
     O               RUNMIN)
C
        IF (RUNMIN .LT. 0) THEN
C         error - ending date/time is earlier than starting date/time
          IF (MESSU .NE. 0) THEN
            DO 30 J= 1, 10
              CALL OMSTI (SEDAT(J))
 30         CONTINUE
            SGRP= 113
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       set the "level" for quantity of run interpreter output and runtime
C       Special Action output
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        READ (UCIBF,1040,ERR=40)  OUTLEV,SPOUT
          GO TO 50
 40     CONTINUE
C         error - cannot read output levels
          ITMP(1)= 35
          CALL OMSTC (ITMP,UCIBF1)
          SGRP= 124
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          OUTLEV= 1
          SPOUT= 2
 50     CONTINUE
        IF ( (OUTLEV .LT. 0) .OR. (OUTLEV .GT. 10) ) THEN
C         error - invalid value - will be reset to 1
          IF (MESSU .NE. 0) THEN
            CALL OMSTI (OUTLEV)
            SGRP= 114
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
          OUTLEV= 1
        ELSE
C         valid output level
          IF (MESSU .NE. 0) THEN
            INUM= 1
            SGRP= 115
            ITMP(1)= OUTLEV
            CALL PMXTFI (MSGFL,MESSU,SCLU,SGRP,INUM,ITMP)
          END IF
        END IF
        IF (SPOUT .EQ. 0) THEN
C         check to see if zero is entered or defaulted
          J= LENSTR (I5,UCIBF1(31))
          IF (J .EQ. 0) THEN
C           default is two
            SPOUT= 2
          END IF
        END IF
        IF ( (SPOUT .LT. 0) .OR. (SPOUT .GT. 10) ) THEN
C         error - invalid value - will be reset to 2
          IF (MESSU .NE. 0) THEN
            CALL OMSTI (SPOUT)
            SGRP= 121
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
          SPOUT= 2
        ELSE
C         valid special action output level
          IF (MESSU .NE. 0) THEN
            INUM= 1
            SGRP= 122
            ITMP(1)= SPOUT
            CALL PMXTFI (MSGFL,MESSU,SCLU,SGRP,INUM,ITMP)
          END IF
        END IF
C
C       ascertain functions to be performed in this run
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        READ (UCIBF,1050,ERR=60)  RESMFG,RUNFG,UNIT,IHMFG
          GO TO 70
 60     CONTINUE
C         error - cannot read flags
          ITMP(1)= 60
          CALL OMSTC (ITMP,UCIBF1)
          SGRP= 125
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          RESMFG= 0
          RUNFG= 0
          UNIT= 0
          IHMFG= 0
 70     CONTINUE
C
        IF ( (RUNFG .EQ. 0) .OR. (RUNFG .EQ. 1) ) THEN
C         valid RUNFG specified
        ELSE
C         error - RUNFG must be 0 or 1
          IF (MESSU .NE. 0) THEN
            CALL OMSTI (RUNFG)
            SGRP= 138
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
          RUNFG= 0
        END IF
C
        IF ( (UNIT .EQ. 1) .OR. (UNIT .EQ. 2) ) THEN
C         valid english(1)/metric(2) units specified
          EMFG= UNIT
        ELSE
          IF (UNIT .NE. 0) THEN
C           error - EMFG must be 1 or 2
            IF (MESSU .NE. 0) THEN
              CALL OMSTI (EMFG)
              SGRP= 139
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          END IF
          EMFG= 1
        END IF
C
        IF ( (IHMFG .EQ. 0) .OR. (IHMFG .EQ. 1) ) THEN
C         valid IHMFG specified
        ELSE
C         error - IHMFG must be 0 or 1
          IF (MESSU .NE. 0) THEN
            CALL OMSTI (IHMFG)
            SGRP= 140
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
          IHMFG= 0
        END IF
C
        IF (RESMFG .NE. 0) THEN
C         error - resume mode not implemented in this release
          IF (MESSU .NE. 0) THEN
            SGRP= 116
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
          RESMFG= 0
        ELSE
C         no message here for compat
        END IF
C
C       echo run flag and units flag and IHMFG
        IF (MESSU .NE. 0) THEN
          SGRP= RUNFG+ 117
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          SGRP= EMFG+ 133
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          IF (IHMFG .EQ. 1) THEN
            SGRP= IHMFG+ 136
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
        END IF
C
C       tssfl and wdmsfl are no longer included in global block.
C
C       be sure we are at end of global block
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        IF (KEY .NE. KEYND) THEN
C         error - global block didn't contain 6 non-comment lines
          IF (MESSU .NE. 0) THEN
            SGRP= 119
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
C       finished processing
        IF (MESSU .NE. 0) THEN
          SGRP= 120
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MSLKBK
     I                   (OUTLEV,MESSU,MSGFL,
     I                    MAXMLK,
     M                    ECOUNT,
     O                    MSLINX,NMLTBS)
C
C     + + + PURPOSE + + +
C     Locate all the tables in the mass links block and place
C     them in the mass links index
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ECOUNT,MSGFL,MESSU,NMLTBS,OUTLEV,
     $             MAXMLK,MSLINX(MAXMLK,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interpreter output level
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     MAXMLK - maximum number of mass-link tables
C     ECOUNT - count(s) of specific errors
C     MSLINX - mass link table index; subscript 1 = internal table number
C              subscript 2: 1 = user's id number, 2 and 3 = starting and
C              ending record in ucifl
C     NMLTBS - number of mass link tables in uci
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,I0,I3,I12,J,KEY,
     $             MTABS,N,NUMBR,OPENFG,OPENNO,
     $             TABNO,ERRINT,
     $             SCLU,SGRP,INITFG,CLEN,CONT,KTYP,KEYST,KEYND,IOPT
      CHARACTER*4  CEND
      CHARACTER*12 CHSTR,MASKWD,TXT
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(UCIBF,UCIBF1)
      CHARACTER*1  CHSTR1(12),UCIBF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER     VALNO,CRINTE
C
C     + + + EXTERNALS + + +
      EXTERNAL    VALNO,CRINTE,DUMPER,OMSTC,OMSTI,OMSG,WMSGTT
      EXTERNAL    GETUCI,GETSE,GETEND,HDMES2,PMXTFT
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (/,' FOUND ',A12,I3)
 2060 FORMAT (  ' FOUND ',A12,I3)
 2100 FORMAT (/,' ',A12,' INDEX')
 2110 FORMAT (/,'      TABNO     TABID    TABKST    TABKND',/)
 2120 FORMAT (  ' ',4I10)
 2130 FORMAT (/,' ',A12,' INDEX IS EMPTY')
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I3    = 3
      I12   = 12
      SCLU  = 211
      MTABS = MAXMLK
      ERRINT= -999
C
C     end delimeter
      CALL GETEND(CEND)
C
      KTYP  = 11
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      KEY = KEYST
      IF (KEYST .GT. 0) THEN
C       get first record
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
      END IF
C
      IF (KEY .NE. KEYND) THEN
C       block has been supplied and is not empty
C       read data from message file
        SGRP  = 50
        INITFG= 1
        CLEN  = 12
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        MASKWD= CHSTR
C
        IOPT= 2
        CALL HDMES2(IOPT,KTYP,I1)
        IF (OUTLEV.GE.1) THEN
          SGRP = 58
          CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
        END IF
C       locate the start and end of each mass link table in the
C       mass link table block
        IF (OUTLEV .GT. 2) THEN
C         dump user's control input
          CALL DUMPER (KEYST,KEYND,MESSU)
        END IF
C
C       initialize
        TABNO = 0
        OPENFG= 0
C
C       zero contents of mslinx
        DO 40 I= 1,MTABS
          DO 30 J= 1,3
            MSLINX(I,J)= 0
 30       CONTINUE
 40     CONTINUE
C
C       whiledo not to keynd
 50     CONTINUE
C         check whether this is the start of a new mass link table
          TXT   = UCIBF(3:14)
          NUMBR = CRINTE(ERRINT,I3,UCIBF1(18))
C
          IF (TXT.EQ.MASKWD .AND. NUMBR.NE.ERRINT) THEN
C           mass link table heading
            IF (OUTLEV .GT. 3) THEN
              WRITE (MESSU,2010) TXT,NUMBR
            END IF
            IF (OPENFG .GT. 0) THEN
C             error - expecting a delimiter, not a mass link table heading
              CHSTR= TXT
              CALL OMSTC (I12,CHSTR1)
              CALL OMSTI (NUMBR)
              CHSTR= MASKWD
              CALL OMSTC (I12,CHSTR1)
              CALL OMSTI (MSLINX(OPENFG,1))
              SGRP = 51
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
C
C           check that mass link table no. is unique
            IF (TABNO .GT. 0) THEN
              IF (VALNO(TABNO,MSLINX,NUMBR) .GT. 0) THEN
C               error - duplicate mass link table id
                CHSTR= TXT
                CALL OMSTC (I12,CHSTR1)
                CALL OMSTI (NUMBR)
                SGRP = 52
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
            END IF
C
            TABNO = TABNO+ 1
            OPENFG= TABNO
            IF (TABNO .GT. MTABS) THEN
C             error - too many mass link tables for table index
              SGRP = 53
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             record this table
              MSLINX(TABNO,1)= NUMBR
              MSLINX(TABNO,2)= KEY
            END IF
          ELSE
C           not a mass link table heading
            IF (UCIBF(3:6).EQ.CEND .AND. NUMBR.NE.ERRINT) THEN
C             found a delimiter
              IF (OUTLEV.GT.4) THEN
                WRITE (MESSU,2060) TXT, NUMBR
              END IF
              IF (TXT(5:12) .NE. MASKWD(1:8)) THEN
C               error - unrecognized delimiter
                CHSTR= TXT
                CALL OMSTC (I12,CHSTR1)
                CALL OMSTI (NUMBR)
                SGRP = 54
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE IF (OPENFG .EQ. 0) THEN
C               error - not expecting a delimiter because
C               there is no previous mass link table heading
                CHSTR= TXT
                CALL OMSTC (I12,CHSTR1)
                CALL OMSTI (NUMBR)
                SGRP = 55
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE
                OPENNO= MSLINX(OPENFG,1)
                IF (OPENNO .NE. NUMBR) THEN
C                 error - delimiter doesn't match heading
                  CHSTR= TXT
                  CALL OMSTC (I12,CHSTR1)
                  CALL OMSTI (NUMBR)
                  CHSTR= MASKWD
                  CALL OMSTC (I12,CHSTR1)
                  CALL OMSTI (OPENNO)
                  SGRP = 56
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                ELSE
C                 ok - record the delimiter
                  MSLINX(OPENFG,3)= KEY
                END IF
              END IF
C             mass link table, whichever it was, will be marked "closed"
              OPENFG= 0
            ELSE
C             not a mass link table heading or delimiter - keep going
C
            END IF
C
          END IF
C         read next record
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY .NE. KEYND) GO TO 50
C       end whiledo
C
        NMLTBS= TABNO
        IF (OPENFG .GT. 0) THEN
C         error - end of block encountered before end of last mass link table
          CHSTR= MASKWD
          CALL OMSTC (I12,CHSTR1)
          CALL OMSTI (MSLINX(OPENFG,1))
          SGRP = 57
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (OUTLEV .GT. 3) THEN
          IF (NMLTBS .GT. 0) THEN
C           write out contents of table index
C           heading
            WRITE (MESSU,2100) MASKWD
            WRITE (MESSU,2110)
            DO 230 N= 1,NMLTBS
              WRITE (MESSU,2120)  N, MSLINX(N,1), (MSLINX(N,J)-1,J=2,3)
 230        CONTINUE
          ELSE
            WRITE (MESSU,2130) MASKWD
          END IF
        END IF
C
        IF (OUTLEV .GT. 0)  THEN
C         finished mass link message
          SGRP = 59
          CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
        END IF
      ELSE
C       block was not supplied
        NMLTBS= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MDATBK
     I                   (KEYST,KEYND,OUTLEV,MESSU,MSGFL,
     I                    MAXMDT,
     M                    ECOUNT,
     O                    MDTINX,NMDATS)
C
C     + + + PURPOSE + + +
C     Locate all the tables in the month-data block and place
C     them in the month-data index
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ECOUNT,MSGFL,KEYND,KEYST,MESSU,NMDATS,OUTLEV,
     $             MAXMDT,MDTINX(MAXMDT,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number of this block in the ucifl
C     KEYND  - ending record number of this block in the ucifl
C     OUTLEV - run interpreter output level
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     MAXMDT - maximum number of month-data tables
C     ECOUNT - count(s) of specific errors
C     MDTINX - month-data table index; subscript 1 = internal table number
C              subscript 2: 1 = user's id number, 2 and 3 = starting and
C              ending record in ucifl
C     NMDATS - number of month-data tables in uci
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,J,KEY,ITYP,
     $             MTABS,N,NUMBR,OPENFG,OPENNO,
     $             TABNO,I3,ERRINT,I12,I1,
     $             SCLU,SGRP,INITFG,CLEN,CONT,IOPT
      CHARACTER*3  CHRNUM
      CHARACTER*4  CEND
      CHARACTER*12 CHSTR,KWDLIB,TXT
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(CHRNUM,CHRNU1)
      CHARACTER*1  CHSTR1(12),CHRNU1(3)
C
C     + + + FUNCTIONS + + +
      INTEGER     VALNO,CRINTE
C
C     + + + EXTERNALS + + +
      EXTERNAL    VALNO,CRINTE,DUMPER,OMSTC,OMSTI,OMSG,WMSGTT
      EXTERNAL    GETUCI,GETEND,HDMES2
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('='),/,
     $          ' PROCESSING MONTH-DATA BLOCK')
 2010 FORMAT (/,' FOUND ',A12,I3)
 2060 FORMAT (  ' FOUND ',A12,I3)
 2100 FORMAT (/,' ',A12,' INDEX')
 2110 FORMAT (/,'      TABNO     TABID    TABKST    TABKND',/)
 2120 FORMAT (  ' ',4I10)
 2130 FORMAT (/,' ',A12,' INDEX IS EMPTY')
 2140 FORMAT (/,' FINISHED PROCESSING MONTH-DATA BLOCK',
     $        /,' ',132('='))
C
C     + + + END SPECIFICATIONS + + +
C
      I0     = 0
      I1     = 1
      I3     = 3
      I12    = 12
      SCLU   = 211
C
      MTABS  = MAXMDT
      ERRINT = -999
C
C     end delimeter
      CALL GETEND(CEND)
C
      IF (KEYST .GT. 0) THEN
C       block has been supplied, read data from message file
        SGRP  = 70
        INITFG= 1
        CLEN  = 12
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        KWDLIB= CHSTR
C
        IOPT = 2
        ITYP = 14
        CALL HDMES2(IOPT,ITYP,I1)
        IF (OUTLEV.GT.0) THEN
          WRITE (MESSU,2000)
          IF (OUTLEV .GT. 2) THEN
C           dump user's control input
            CALL DUMPER (KEYST,KEYND,MESSU)
          END IF
        END IF
C       locate the start and end of each month-data table in the
C       month-data table block
C
C       initialize
        TABNO = 0
        OPENFG= 0
C
C       zero contents of mdtinx
        DO 40 I= 1,MTABS
          DO 30 J= 1,3
            MDTINX(I,J)= 0
 30       CONTINUE
 40     CONTINUE
C
        KEY= KEYST
C       whiledo key< keynd
 50     CONTINUE
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IF (KEY .NE. KEYND) THEN
C           check whether this is the start of a new month-data table
            TXT   = UCIBF(3:14)
            CHRNUM= UCIBF(18:20)
            NUMBR = CRINTE (ERRINT,I3,CHRNUM)
            IF (TXT.EQ.KWDLIB .AND. NUMBR.NE.ERRINT) THEN
C             month-data table heading
              IF (OUTLEV .GT. 3) THEN
                WRITE (MESSU,2010) TXT,NUMBR
              END IF
              IF (OPENFG .GT. 0) THEN
C               error - expecting a delimiter, not a month data table heading
                CHSTR= TXT
                CALL OMSTC (I12,CHSTR1)
                CALL OMSTI (NUMBR)
                CHSTR= KWDLIB
                CALL OMSTC (I12,CHSTR1)
                CALL OMSTI (MDTINX(OPENFG,1))
                SGRP = 71
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             check that month-data table no. is unique
              IF (TABNO .GT. 0) THEN
                IF (VALNO(TABNO,MDTINX,NUMBR) .GT. 0) THEN
C                 error - duplicate month-data table id
                  CHSTR= TXT
                  CALL OMSTC (I12,CHSTR1)
                  CALL OMSTI (NUMBR)
                  SGRP = 72
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
              END IF
C
              TABNO = TABNO+ 1
              OPENFG= TABNO
              IF (TABNO .GT. MTABS) THEN
C               error - too many month-data tables for table index
                SGRP = 73
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE
C               record this table
                MDTINX(TABNO,1)= NUMBR
                MDTINX(TABNO,2)= KEY
              END IF
            ELSE
C             not a month-data table heading
              IF (UCIBF(3:6).EQ.CEND .AND. NUMBR.NE.ERRINT) THEN
C               found a delimiter
                IF (OUTLEV.GT.4) THEN
                  WRITE (MESSU,2060) TXT,NUMBR
                END IF
                IF (TXT(5:12) .NE. KWDLIB(1:8)) THEN
C                 error - unrecognized delimiter
                  CHSTR= TXT
                  CALL OMSTC (I12,CHSTR1)
                  CALL OMSTI (NUMBR)
                  SGRP = 74
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                ELSE IF (OPENFG .EQ. 0) THEN
C                 error - not expecting a delimiter because
C                 there is no previous month-data table heading
                  CHSTR= TXT
                  CALL OMSTC (I12,CHSTR1)
                  CALL OMSTI (NUMBR)
                  SGRP = 75
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                ELSE
                  OPENNO= MDTINX(OPENFG,1)
                  IF (OPENNO .NE. NUMBR) THEN
C                   error - delimiter doesn't match heading
                    CHSTR= TXT
                    CALL OMSTC (I12,CHSTR1)
                    CALL OMSTI (NUMBR)
                    CHSTR= KWDLIB
                    CALL OMSTC (I12,CHSTR1)
                    CALL OMSTI (OPENNO)
                    SGRP = 76
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  ELSE
C                   ok - record the delimiter
                    MDTINX(OPENFG,3)= KEY
                  END IF
                END IF
C               month-data table, whichever it was, will be marked "closed"
                OPENFG= 0
              ELSE
C               not a month-data table heading or delimiter - keep going
C
              END IF
            END IF
          END IF
        IF (KEY .NE. KEYND) GO TO 50
C       end whiledo
C
        NMDATS= TABNO
        IF (OPENFG .GT. 0) THEN
C         error - end of block encountered before end of last month-data table
          CHSTR= KWDLIB
          CALL OMSTC (I12,CHSTR1)
          CALL OMSTI (MDTINX(OPENFG,1))
          SGRP = 77
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (OUTLEV .GT. 3) THEN
          IF (NMDATS .GT. 0) THEN
C           write out contents of table index
C           heading
            WRITE (MESSU,2100)  KWDLIB
            WRITE (MESSU,2110)
            DO 230 N= 1,NMDATS
              WRITE (MESSU,2120)  N, (MDTINX(N,J),J=1,3)
 230        CONTINUE
          ELSE
            WRITE (MESSU,2130) KWDLIB
          END IF
        END IF
C
        IF (OUTLEV .GT. 0)  THEN
C         finished month-data message
          WRITE (MESSU,2140)
        END IF
      ELSE
C       block was not supplied
        NMDATS= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWEXG
     I                   (EXFG,MESSU,MSGFL,SCLU,XGRPNO,
     $                    INFG,GRPNO,HRMIN,NIVLS,IVLLIB,
     $                    RUNMIN,
     M                    ECOUNT,
     O                    XCOUNT,XDELT)
C
C     + + + PURPOSE + + +
C     New exgroup - check whether this is legal, check xdelt and
C     remember the details of this exgroup
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,SCLU,EXFG,GRPNO,
     $           HRMIN(2),INFG,IVLLIB(20),MESSU,NIVLS,RUNMIN,
     $           XCOUNT,XDELT,XGRPNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXFG   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing messages for this routine
C     XGRPNO - ???
C     INFG   - ???
C     GRPNO  - ???
C     HRMIN  - ???
C     NIVLS  - ???
C     IVLLIB - ???
C     RUNMIN - ???
C     ECOUNT - count(s) of specific errors
C     XCOUNT - ???
C     XDELT  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP
C
C     + + + FUNCTIONS + + +
      INTEGER    VALNO
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   VALNO,OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 211
C     xcount is the no. of operations encountered so far in the
C     current exgroup
      XCOUNT= 0
C
      IF (EXFG .GT. 0) THEN
C       error - exgroup was specified before end of existing exgroup
C       was encountered - end exgroup generated
        CALL OMSTI (XGRPNO)
        SGRP = 32
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (INFG .GT. 0) THEN
C       error - exgroup was explicitly or implicitly specified before
C       end of existing ingroup was encountered
        CALL OMSTI (GRPNO)
        SGRP = 33
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     check the time interval for this expad
      XDELT= HRMIN(1)*60+ HRMIN(2)
C
      IF (VALNO(NIVLS,IVLLIB,XDELT) .EQ. 0) THEN
C       error - hrmin(*) is not a valid interval
        CALL OMSTI (HRMIN(1))
        CALL OMSTI (HRMIN(2))
        SGRP = 30
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      ELSE
C       ok - valid interval
C       check whether the run span is a multiple of xdelt
        IF (MOD(RUNMIN,XDELT) .NE. 0) THEN
C         error - run span is not a multiple of the xdelt
          CALL OMSTI (RUNMIN)
          CALL OMSTI (XDELT)
          SGRP = 34
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWING
     I                   (INFG,EXFG,MESSU,MSGFL,SCLU,
     $                    GRPNO,XGRPNO,OUTLEV,HRMIN,NIVLS,
     $                    IVLLIB,
     $                    RUNMIN,
     M                    ECOUNT,
     O                    ICOUNT,NDELT,XCOUNT,XDELT)
C
C     + + + PURPOSE + + +
C     New ingroup - check whether this is legal, check ndelt and
C     remember the details of this ingroup
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,SCLU,EXFG,GRPNO,
     $           HRMIN(2),ICOUNT,INFG,IVLLIB(20),MESSU,NDELT,NIVLS,
     $           OUTLEV,RUNMIN,XCOUNT,XDELT,XGRPNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFG   - ???
C     EXFG   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing messages for this routine
C     GRPNO  - ???
C     XGRPNO - ???
C     OUTLEV - run interpreter output level
C     HRMIN  - ???
C     NIVLS  - ???
C     IVLLIB - ???
C     RUNMIN - ???
C     ECOUNT - count(s) of specific errors
C     ICOUNT - ???
C     NDELT  - simulation time interval in minutes
C     XCOUNT - ???
C     XDELT  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP
C
C     + + + FUNCTION + + +
      INTEGER    VALNO
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   VALNO,OMSG,OMSTI,NEWEXG,PMXTFT
C
C     + + + END SPECIFICATIONS + + +
C
C     icount is the no. of opns encountered so far in the current
C     ingroup
      ICOUNT= 0
C
      IF (INFG.GT.0) THEN
C       error - ingroup was specified before end of existing ingroup
C       was encountered
        CALL OMSTI (GRPNO)
        SGRP = 29
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (EXFG.EQ.0) THEN
C       not in a user specified exgroup - generate one
        IF (OUTLEV .GT. 4)  THEN
          SGRP= 38
          CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
        END IF
C       check properties of this exgroup
        CALL NEWEXG (EXFG,MESSU,MSGFL,SCLU,XGRPNO,
     $               INFG,GRPNO,HRMIN,NIVLS,IVLLIB,RUNMIN,
     M               ECOUNT,
     O               XCOUNT,XDELT)
      END IF
C
C     check the time interval supplied for this ingroup
      NDELT= HRMIN(1)*60+ HRMIN(2)
C
      IF (VALNO(NIVLS,IVLLIB,NDELT).EQ.0) THEN
C       error - ndelt is not a valid interval
        CALL OMSTI (HRMIN(1))
        CALL OMSTI (HRMIN(2))
        SGRP = 30
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
C
      ELSE
C       ok - check that it is compatible with xdelt
        IF (MOD(XDELT,NDELT) .NE. 0) THEN
          IF (MOD(NDELT,XDELT) .NE. 0) THEN
C           error - xdelt and ndelt are not compatible
            CALL OMSTI (XDELT)
            CALL OMSTI (NDELT)
            SGRP = 31
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWOPN
     I                   (OPTSTR,OPTNO,MESSU,MSGFL,SCLU,
     I                    INFG,EXFG,OUTLEV,HRMIN,NIVLS,
     I                    IVLLIB,RUNMIN,KWDNO,
     I                    MENTRY,MAXOPN,
     M                    OPNO,ECOUNT,GRPNO,XGRPNO,ICOUNT,XCOUNT,
     M                    NDELT,XDELT,WCOUNT,
     O                    OPNTAB,GRPTAB,EXGTAB)
C
C     + + + PURPOSE + + +
C     User has specified a new operation.  check that values given are
C     legal, that the operation-id is unique and generate exgroup or
C     ingroup info, if required
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECOUNT,MSGFL,SCLU,EXFG,MAXOPN,
     $            EXGTAB(5,10),GRPNO,GRPTAB(5,10),HRMIN(2),ICOUNT,INFG,
     $            IVLLIB(20),KWDNO,MENTRY,MESSU,NDELT,NIVLS,OPNO,
     $            OPNTAB(20,MAXOPN),OPTNO,OUTLEV,RUNMIN,
     $            WCOUNT(10),XCOUNT,XDELT,
     $            XGRPNO
      CHARACTER*6 OPTSTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTSTR - operation type
C     OPTNO  - operation number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing messages for this routine
C     INFG   - ???
C     EXFG   - ???
C     OUTLEV - run interpreter output level
C     HRMIN  - ???
C     NIVLS  - ???
C     IVLLIB - ???
C     RUNMIN - ???
C     KWDNO  - ???
C     MENTRY - ???
C     MAXOPN - maximum number of operations
C     OPNO   - ???
C     ECOUNT - count(s) of specific errors
C     GRPNO  - ???
C     XGRPNO - ???
C     ICOUNT - ???
C     XCOUNT - ???
C     NDELT  - simulation time interval in minutes
C     XDELT  - ???
C     WCOUNT - ???
C     OPNTAB - ???
C     GRPTAB - ???
C     EXGTAB - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,I6,SGRP,J
      CHARACTER*6  CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER    OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL   OPNNO,NEWING,OMSG,OMSTC,OMSTI,PMXTFT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4,A2)
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I6  = 6
C
C     check that opn-id has not occurred before
      IF (OPNNO (OPTSTR,OPTNO,OPTNO,MAXOPN,OPNTAB,I1,OPNO).GT.0) THEN
C       error - duplicate operation-id
        CHSTR= OPTSTR
        CALL OMSTC (I6,CHSTR1)
        CALL OMSTI (OPTNO)
        SGRP = 25
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (INFG.EQ.0) THEN
C       operation is not in a user-specified ingroup - generate one
        IF (OUTLEV.GT.4) THEN
C         generated ingroup
          SGRP= 10
          CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
        END IF
        CALL NEWING (INFG,EXFG,MESSU,MSGFL,SCLU,GRPNO,
     $               XGRPNO,OUTLEV,HRMIN,NIVLS,IVLLIB,RUNMIN,
     M               ECOUNT,
     O               ICOUNT,NDELT,XCOUNT,XDELT)
      ELSE
C       operation is in a user-specified ingroup
        IF (HRMIN(1).NE.0 .OR. HRMIN(2).NE.0) THEN
C         warning - because opn is in a user specified ingroup, hspf
C         expects hrmin field to be blank, but it was not.  value
C         ignored
          CALL OMSTI (HRMIN(1))
          CALL OMSTI (HRMIN(2))
          SGRP = 2
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               WCOUNT(2))
        END IF
      END IF
C
      OPNO= OPNO+ 1
      IF (OPNO .GT. MENTRY) THEN
C       error - user has more operations in his run than can be
C       accomodated in opntab - operation not entered in table
        SGRP = 26
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      ELSE
C       process this operation
        ICOUNT= ICOUNT+ 1
        XCOUNT= XCOUNT+ 1
        IF (ICOUNT.EQ.1) THEN
C         first opn is an ingroup
          GRPNO= GRPNO+ 1
          IF (GRPNO .GT. 10) THEN
C           error - user has more ingroup's in his run than can be
C           accomodated in grptab.  group not entered in table
            SGRP = 27
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          ELSE
C           process the ingroup
            GRPTAB(1,GRPNO)= OPNO
            IF (INFG .EQ. 0) THEN
              IF (OUTLEV .GT. 4) THEN
C               generated end ingroup
                SGRP= 11
                CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
              END IF
              GRPTAB(2,GRPNO)= OPNO
              GRPTAB(3,GRPNO)= NDELT
            END IF
          END IF
C
          IF (XCOUNT.EQ.1) THEN
C           first opn in an exgroup
            XGRPNO= XGRPNO+ 1
            IF (XGRPNO .GT. 10) THEN
C             error - user has more exgroup's in his run than can be
C             accomodated in exgtab.  group not entered in table
              SGRP = 28
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             process the exgroup
              EXGTAB(1,XGRPNO)= GRPNO
              IF (INFG.EQ.0 .AND. EXFG.EQ.0) THEN
C               not in a user-specified ingroup or exgroup
                EXGTAB(2,XGRPNO)= GRPNO
                EXGTAB(3,XGRPNO)= XDELT
                IF (OUTLEV.GT.4) THEN
C                 generated end exgrp
                  SGRP= 12
                  CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
                END IF
              END IF
            END IF
          END IF
        END IF
C
C       record particulars of this operation in opntab
        READ(OPTSTR,1000) (OPNTAB(J,OPNO),J=1,2)
        OPNTAB(3,OPNO)= OPTNO
        OPNTAB(4,OPNO)= KWDNO
        OPNTAB(5,OPNO)= XGRPNO
        OPNTAB(6,OPNO)= GRPNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEQBLK
     I                   (MESSU,MSGFL,NIVLS,IVLLIB,
     I                    OUTLEV,RUNMIN,MAXOPN,LKWOPR,NKWOPR,KWDOPR,
     M                    WCOUNT,ECOUNT,
     O                    EXGTAB,GRPTAB,OPNTAB,NXGRPS,NGRPS,
     O                    NOPNS)
C
C     + + + PURPOSE + + +
C     Read and process the opn sequence block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECOUNT,MSGFL,
     $            EXGTAB(5,10),GRPTAB(5,10),MAXOPN,LKWOPR,NKWOPR,
     $            IVLLIB(20),MESSU,NGRPS,NIVLS,NOPNS,
     $            NXGRPS,OPNTAB(20,MAXOPN),OUTLEV,RUNMIN,
     $            WCOUNT(10)
      CHARACTER*1 KWDOPR(LKWOPR,NKWOPR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     NIVLS  - ???
C     IVLLIB - ???
C     OUTLEV - run interpreter output level
C     RUNMIN - span of run in minutes
C     MAXOPN - maximum number of operations
C     LKWOPR - length of operation name keywords
C     NKWOPR - number of operation name keywords
C     KWDOPR - operation name keyword library
C     WCOUNT - ???
C     ECOUNT - count(s) of specific errors
C     EXGTAB - ???
C     GRPTAB - ???
C     OPNTAB - ???
C     NXGRPS - ???
C     NGRPS  - ???
C     NOPNS  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      EXFG,GRPNO,HRMIN(2),I,I0,I1,ITYP,I20,
     $             ICOUNT,INFG,J,KEY,I12,KEYST,KEYND,
     $             KWDNO,MENTRY,NDELT,OPNO,OPTNO,
     $             XCOUNT,XDELT,XGRPNO,SCLU,SGRP,
     $             INITFG,CLEN,CONT,ITMP(1),ERRINT,IOPT
      CHARACTER*4  CEND
      CHARACTER*6  CBLNK6
      CHARACTER*8  CEXG,CING
      CHARACTER*20 CHSTR
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(UCIBF,UCIBF1)
      CHARACTER*1  CHSTR1(20),UCIBF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER    CHKSTR,CRINTE
C
C     + + + EXTERNALS + + +
      EXTERNAL   DUMPER,NEWEXG,NEWING,NEWOPN,OMSG,ENDEXG,CRINTE
      EXTERNAL   ENDING,OMSTC,WMSGTT,GETUCI,GETSE,GETEND,CHKSTR
      EXTERNAL   PMXTFT,PMXTFC,HDMES2
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4,A2)
 1010 FORMAT (30X,I2,1X,I2)
C
C     + + + OUTPUT FORMATS + + +
 2050 FORMAT (/,' FOUND END EXGROUP')
 2070 FORMAT (/,' FOUND END INGROUP')
 2082 FORMAT (  ' GENERATED END INGROUP')
 2084 FORMAT (  ' GENERATED END EXGROUP')
 2090 FORMAT (/,' CONTENTS OF TABLES GENERATED BY SUBROUTINE SEQBLK',/)
 2100 FORMAT (/,' EXGROUP TABLE')
 2110 FORMAT (/,'    EXGROUP     FIRST      LAST      TIME')
 2120 FORMAT (  '        NO.   INGROUP   INGROUP  INTERVAL')
 2130 FORMAT (  '                                   (MINS)')
 2140 FORMAT (  ' ',4I10)
 2150 FORMAT (/,' INGROUP TABLE')
 2160 FORMAT (/,'    INGROUP     FIRST      LAST      TIME')
 2170 FORMAT (  '        NO. OPERATION OPERATION  INTERVAL')
 2180 FORMAT (/,' OPERATION TABLE')
 2190 FORMAT (/,'  OPERATION       OPN   OP-TYPE             EXGROUP',
     $        '   INGROUP')
 2200 FORMAT (' ','       NO.      TYPE       NO.    OMCODE       NO.',
     $        '       NO.',/)
 2210 FORMAT (' ',I10,4X,A4,A2,4I10)
 2220 FORMAT (/,' NO OPERATIONS PROCESSED')
 2230 FORMAT (/,' FINISHED PROCESSING OPN SEQUENCE BLOCK',
     $        /,' ',132('='))

C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I12   = 12
      I20   = 20
      SCLU  = 211
      ERRINT= -999
      CBLNK6='      '
C
C     read opn specific keyword info from msg file
      SGRP  = 5
      INITFG= 1
      CLEN  = 16
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      CEXG = CHSTR(1:8)
      CING = CHSTR(9:16)
C
      CALL GETEND(CEND)
C
C     find table in uci (type 3 from hspf.seq, grp 22, col 3
      ITYP= 3
      CALL GETSE(ITYP,I1,
     O           KEYST,KEYND)
      KEY= KEYST
      IF (KEYST .GT. 0) THEN
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
      END IF
      IF (KEY .NE. KEYND) THEN
C       opn sequence block has been supplied and is not empty
        IOPT= 2
        CALL HDMES2(IOPT,ITYP,I1)
        IF (OUTLEV.GT.0) THEN
C         processing message
          SGRP= 130
          CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
          IF (OUTLEV.GT.2) THEN
C           dump user's control input
            CALL DUMPER (KEYST,KEYND,MESSU)
          END IF
        END IF
C
C       initialize
        XGRPNO= 0
        GRPNO = 0
        OPNO  = 0
C
C       max no. of entries in tables
        MENTRY= MAXOPN
        EXFG  = 0
        INFG  = 0
C
C       zero tables
        DO 40 I= 1,10
          DO 30 J= 1,5
            EXGTAB(J,I)= 0
            GRPTAB(J,I)= 0
 30       CONTINUE
 40     CONTINUE
C
        DO 45 I= 1,MAXOPN
          READ(CBLNK6,1000) (OPNTAB(J,I),J=1,2)
          OPNTAB(3,I)= 0
          OPNTAB(4,I)= 0
          OPNTAB(5,I)= 0
          OPNTAB(6,I)= 0
 45     CONTINUE
C
C       whiledo key< keynd
 50     CONTINUE
          READ (UCIBF,1010,ERR=60)  HRMIN
            GO TO 70
 60       CONTINUE
C           error - cannot read time step of run
            I= 12
            CALL OMSTC (I,UCIBF1(24))
            SGRP= 126
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            HRMIN(1)= 24
            HRMIN(2)= 0
 70       CONTINUE
C         see if we have a keyword - test each of 3 possible fields
          IF (UCIBF(1:8) .EQ. CEXG) THEN
C           exgroup
            IF (OUTLEV.GT.4) THEN
C             exgrp
              SGRP= 131
              CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
            END IF
C           warning - external scratch pad is not implemented in
C           this release of hspf
            SGRP = 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 WCOUNT(1))
C           check and remember details for this exgroup
            CALL NEWEXG (EXFG,MESSU,MSGFL,SCLU,XGRPNO,
     $                   INFG,GRPNO,HRMIN,NIVLS,IVLLIB,
     $                   RUNMIN,
     M                   ECOUNT,
     O                   XCOUNT,XDELT)
            EXFG= 1
          ELSE IF (UCIBF(5:12) .EQ. CING) THEN
C           ingroup
            IF (OUTLEV.GT.4) THEN
C             ingrp
              SGRP= 132
              CALL PMXTFT(MSGFL,MESSU,SCLU,SGRP)
            END IF
            CALL NEWING (INFG,EXFG,MESSU,MSGFL,SCLU,
     $                   GRPNO,XGRPNO,OUTLEV,HRMIN,NIVLS,
     $                   IVLLIB,RUNMIN,
     M                   ECOUNT,
     O                   ICOUNT,NDELT,XCOUNT,XDELT)
            INFG=1
          ELSE
C           test third (operation) field
            CHSTR= UCIBF(7:12)
            KWDNO= CHKSTR (LKWOPR,NKWOPR,CHSTR1,KWDOPR)
            J= 5
            OPTNO = CRINTE(ERRINT,J,UCIBF1(16))
            IF ( (KWDNO.GT.0) .AND. (OPTNO .NE. ERRINT) ) THEN
C             operation
              IF (OUTLEV.GT.4) THEN
                ITMP(1)= 14
                SGRP   = 133
                CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,I1,ITMP,UCIBF1(7))
              END IF
              CALL NEWOPN (CHSTR,OPTNO,MESSU,MSGFL,SCLU,
     I                     INFG,EXFG,OUTLEV,HRMIN,
     I                     NIVLS,IVLLIB,RUNMIN,
     I                     KWDNO,MENTRY,MAXOPN,
     M                     OPNO,ECOUNT,GRPNO,XGRPNO,
     M                     ICOUNT,XCOUNT,NDELT,XDELT,WCOUNT,
     O                     OPNTAB,GRPTAB,EXGTAB)
            ELSE
C             no keyword was recognized - check whether we have a
C             delimiter
C             first (exgroup) field
              IF (UCIBF(1:4).EQ.CEND) THEN
C               found a delimiter
                IF (UCIBF(5:12) .EQ. CEXG) THEN
C                 end exgroup
                  IF (OUTLEV.GT.4) THEN
                    WRITE (MESSU,2050)
                  END IF
                  IF (EXFG.EQ.0) THEN
C                   error - end exgroup found but corresponding
C                   valid exgroup heading didn't precede it -
C                   command ignored
                    SGRP = 21
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  ELSE
                    CALL ENDEXG (INFG,XCOUNT,XGRPNO,GRPNO,
     I                           XDELT,MESSU,MSGFL,SCLU,
     M                           ECOUNT,WCOUNT,EXGTAB,
     O                           EXFG)
                  END IF
                ELSE
C                 error - delimiter is invalid - ignored
                  CHSTR= UCIBF(1:12)
                  CALL OMSTC (I12,CHSTR1)
                  SGRP = 22
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
              ELSE IF (UCIBF(5:8).EQ.CEND) THEN
C               check second (ingroup) field
                IF (UCIBF(9:16).EQ.CING) THEN
C                 end ingroup
                  IF (OUTLEV.GT.4) THEN
                    WRITE (MESSU,2070)
                  END IF
                  CALL ENDING (INFG,EXFG,ICOUNT,XCOUNT,GRPNO,XGRPNO,
     I                         OPNO,NDELT,XDELT,MESSU,MSGFL,SCLU,OUTLEV,
     M                         ECOUNT,WCOUNT,
     O                         GRPTAB,EXGTAB)
                ELSE
C                 error - delimiter is invalid - ignored
                  CHSTR= UCIBF(1:12)
                  CALL OMSTC (I12,CHSTR1)
                  SGRP = 22
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
              ELSE
C               error - text not recognized as valid
C               heading or delimiter - ignored
                CHSTR= UCIBF(1:20)
                CALL OMSTC (I20,CHSTR1)
                SGRP = 23
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
            END IF
          END IF
C         get the next record
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY .NE. KEYND) GO TO 50
C       end whiledo
C
        IF (INFG.NE.0) THEN
          IF (OUTLEV.GT.4) THEN
            WRITE (MESSU,2082)
          END IF
          CALL ENDING (INFG,EXFG,ICOUNT,XCOUNT,GRPNO,XGRPNO,OPNO,NDELT,
     I                 XDELT,MESSU,MSGFL,SCLU,OUTLEV,
     M                 ECOUNT,WCOUNT,
     O                 GRPTAB,EXGTAB)
        END IF
        IF (EXFG.NE.0) THEN
          IF (OUTLEV.GT.4) THEN
            WRITE (MESSU,2084)
          END IF
          CALL ENDEXG (INFG,XCOUNT,XGRPNO,GRPNO,XDELT,MESSU,MSGFL,SCLU,
     M                 ECOUNT,WCOUNT,EXGTAB,
     O                 EXFG)
        END IF
        NXGRPS= XGRPNO
        NGRPS = GRPNO
        NOPNS = OPNO
C
        IF (OUTLEV .GT. 3) THEN
C         dump the tables
          IF (NOPNS .GT. 0) THEN
            WRITE (MESSU,2090)
C           exgroup table
            WRITE (MESSU,2100)
            WRITE (MESSU,2110)
            WRITE (MESSU,2120)
            WRITE (MESSU,2130)
            DO 240 I= 1,NXGRPS
              WRITE (MESSU,2140)  I, (EXGTAB(J,I),J=1,3)
 240        CONTINUE
C
C           ingroup table
            WRITE (MESSU,2150)
            WRITE (MESSU,2160)
            WRITE (MESSU,2170)
            WRITE (MESSU,2130)
            DO 250 I= 1,NGRPS
              WRITE (MESSU,2140)  I, (GRPTAB(J,I),J=1,3)
 250        CONTINUE
C
C           operation table
            WRITE (MESSU,2180)
            WRITE (MESSU,2190)
            WRITE (MESSU,2200)
            DO 260 I= 1,NOPNS
              WRITE (MESSU,2210)  I, (OPNTAB(J,I),J=1,6)
 260        CONTINUE
          ELSE
C           no operations processed
            WRITE (MESSU,2220)
          END IF
        END IF
C
        IF (OUTLEV.GT.0)  THEN
          WRITE (MESSU,2230)
        END IF
      ELSE
C       no operation sequence block was in user's control input
C       error - because "resume" mode is not supported in this
C       release of hspf
        SGRP = 24
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TABBLK
     I                    (KEYST,KEYND,OUTLEV,MESSU,
     I                     MSGFL,MAXFTB,
     M                     ECOUNT,
     O                     TABINX,NFTABS)
C
C     + + + PURPOSE + + +
C     Locate all the tables in the ftables block and place
C     them in the ftables index
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,KEYND,KEYST,MESSU,NFTABS,OUTLEV,
     $           MAXFTB,TABINX(MAXFTB,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     OUTLEV - run interpreter output level
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     MAXFTB - maximum number of ftables
C     ECOUNT - count(s) of specific errors
C     TABINX - ???
C     NFTABS - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,J,KEY,ITYP,I1,
     $             MTABS,N,NUMBR,OPENFG,OPENNO,
     $             TABNO,SGRP,SCLU,I6,
     $             INITFG,CONT,CLEN,IOPT
      CHARACTER*4  CEND
      CHARACTER*6  KWDLIB
      CHARACTER*15 CHSTR
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(15)
C
C     + + + FUNCTIONS + + +
      INTEGER      VALNO
C
C     + + + EXTERNALS + + +
      EXTERNAL     VALNO,DUMPER,OMSG,OMSTI,OMSTC,WMSGTT
      EXTERNAL     GETUCI,GETEND,HDMES2
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (A6)
 1025 FORMAT (12X,I3)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('='),/,' ','PROCESSING FTABLES BLOCK')
 2010 FORMAT (/,' FOUND ',A6,2X,I3)
 2060 FORMAT (  ' FOUND ',A6,2X,I3)
 2100 FORMAT (/,' ',A6,'   INDEX')
 2110 FORMAT (/,'      TABNO     TABID    TABKST    TABKND',/)
 2120 FORMAT (  ' ',4I10)
 2130 FORMAT (/,' ',A6,'   INDEX IS EMPTY')
 2140 FORMAT (/,' ','FINISHED PROCESSING FTABLES BLOCK',
     $        /,' ',132('='))
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I6   = 6
C
      MTABS= MAXFTB
      SCLU = 211
C     end delimeter
      CALL GETEND(CEND)
C
      IF (KEYST .GT. 0) THEN
C       block has been supplied, read keyword
        SGRP  = 40
        CLEN  = 8
        INITFG= 1
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ(CHSTR,1010) KWDLIB
C
        IOPT = 2
        ITYP = 4
        CALL HDMES2(IOPT,ITYP,I1)
        IF (OUTLEV .GT. 0) THEN
C         write processing message
          WRITE (MESSU,2000)
          IF (OUTLEV .GT. 2) THEN
C           dump user's control input
            CALL DUMPER (KEYST,KEYND,MESSU)
          END IF
        END IF
C
C       initialize
        TABNO = 0
        OPENFG= 0
C
C       zero contents of tabinx
        DO 20 I= 1,MTABS
          DO 10 J= 1,3
            TABINX(I,J)= 0
 10       CONTINUE
 20     CONTINUE
C
        KEY= KEYST
C       locate the start and end of each ftable in the ftables block
C       whiledo key <> keynd
 30     CONTINUE
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          IF (KEY .NE. KEYND) THEN
C           process record
C
C           check whether this is the start of a new ftable
            IF (UCIBF(3:8) .EQ. KWDLIB) THEN
C             ftable heading
              READ (UCIBF,1025,ERR=40)  NUMBR
                GO TO 50
 40           CONTINUE
C               error - cannot read ftable number
                CHSTR(1:15)= UCIBF(1:15)
                I= 15
                CALL OMSTC (I,CHSTR1)
                SGRP= 127
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                NUMBR= -999
 50           CONTINUE
              IF (OUTLEV .GT. 3) THEN
C               echo ftable heading
                WRITE (MESSU,2010) UCIBF(3:8),NUMBR
              END IF
              IF (OPENFG .GT. 0) THEN
C               error - expecting a delimiter, not an ftable heading
                CHSTR(1:6)= UCIBF(3:8)
                CALL OMSTC (I6,CHSTR1)
                CALL OMSTI (NUMBR)
                CHSTR(1:6)= KWDLIB
                CALL OMSTC (I6,CHSTR1)
                CALL OMSTI (TABINX(OPENFG,1))
                SGRP = 41
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             check that ftable no. is unique
              IF (TABNO .GT. 0) THEN
                IF (VALNO(TABNO,TABINX,NUMBR) .GT. 0) THEN
C                 error - duplicate ftable id
                  CHSTR(1:6)= UCIBF(3:8)
                  CALL OMSTC (I6,CHSTR1)
                  CALL OMSTI (NUMBR)
                  SGRP = 42
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
              END IF
C
              TABNO = TABNO+ 1
              OPENFG= TABNO
              IF (TABNO .GT. MTABS) THEN
C               error - too many ftables for table index
                SGRP = 43
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE
C               record this table
                TABINX(TABNO,1)= NUMBR
                TABINX(TABNO,2)= KEY
              END IF
            ELSE
C             not an ftable heading
              IF (UCIBF(3:6) .EQ. CEND) THEN
C               found a delimiter
                READ (UCIBF,1025,ERR=60)  NUMBR
                  GO TO 70
 60             CONTINUE
C                 error - cannot read ftable number
                  CHSTR(1:15)= UCIBF(1:15)
                  I= 15
                  CALL OMSTC (I,CHSTR1)
                  SGRP= 128
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                  NUMBR= -999
 70             CONTINUE
                IF (OUTLEV .GT. 4) THEN
C                 echo delimiter
                  WRITE (MESSU,2060)  KWDLIB,NUMBR
                END IF
                IF (UCIBF(7:10) .NE. KWDLIB(1:4)) THEN
C                 error - unrecognized delimiter
                  CHSTR(1:6)= UCIBF(7:12)
                  CALL OMSTC (I6,CHSTR1)
                  CALL OMSTI (NUMBR)
                  SGRP = 44
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                ELSE
                  IF (OPENFG .EQ. 0) THEN
C                   error - not expecting a delimiter because
C                   there is no previous ftable heading
                    CHSTR(1:6)= UCIBF(7:12)
                    CALL OMSTC (I6,CHSTR1)
                    CALL OMSTI (NUMBR)
                    SGRP = 45
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  ELSE
                    OPENNO= TABINX(OPENFG,1)
                    IF (OPENNO .NE. NUMBR) THEN
C                     error - delimiter doesn't match heading
                      CHSTR(1:6)= UCIBF(7:12)
                      CALL OMSTC (I6,CHSTR1)
                      CALL OMSTI (NUMBR)
                      CHSTR(1:6)= KWDLIB
                      CALL OMSTC (I6,CHSTR1)
                      CALL OMSTI (OPENNO)
                      SGRP = 46
                      CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                           ECOUNT)
                    ELSE
C                     ok - record the delimiter
                      TABINX(OPENFG,3)= KEY
                    END IF
                  END IF
                END IF
C               ftable, whichever it was, will be marked "closed"
                OPENFG= 0
              ELSE
C               not an ftable heading or delimiter - keep going
C
              END IF
            END IF
          END IF
        IF (KEY .NE. KEYND) GO TO 30
C       end whiledo
C
        NFTABS= TABNO
        IF (OPENFG .GT. 0) THEN
C         error - end of block encountered before end of last ftable
          CHSTR(1:6)= KWDLIB
          CALL OMSTC (I6,CHSTR1)
          CALL OMSTI (TABINX(OPENFG,1))
          SGRP = 47
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (OUTLEV .GT. 3) THEN
          IF (NFTABS .GT. 0) THEN
C           write out contents of table index
C           heading
            WRITE (MESSU,2100) KWDLIB
            WRITE (MESSU,2110)
            DO 230 N= 1,NFTABS
              WRITE (MESSU,2120)  N, TABINX(N,1), (TABINX(N,J)-1,J=2,3)
 230        CONTINUE
          ELSE
            WRITE (MESSU,2130) KWDLIB
          END IF
        END IF
C
        IF (OUTLEV.GT.0) THEN
C         finished message
          WRITE (MESSU,2140)
        END IF
      ELSE
C       block was not supplied
        NFTABS= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CATBLK
     I                   (MESSU,OUTLEV,MSGFL,
     M                    ECOUNT)
C
C     + + + PURPOSE + + +
C     process the category block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,OUTLEV,MSGFL,ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     OUTLEV - run interpreter output level
C     MSGFL  - fortran unit number of HSPF message file
C     ECOUNT - count of errors
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phcat.inc'
      INCLUDE 'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      KEY,SCLU,SGRP,I,KEYST,KEYND,I1,I0,ITYP,
     #             ERRFLG,ICH,I2,IOPT
      CHARACTER*2  CATID
      CHARACTER*16 LCATNM
      CHARACTER*22 TBUFF
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TBUFF,TBUF1),(CATID,CATID1)
      CHARACTER*1  TBUF1(22),CATID1(2)
C
C     + + + FUNCTIONS + + +
      INTEGER      CHKSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSTC,OMSG,GETUCI,GETSE,PMXTFT,HDMES2,CHKSTR
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (3X,A2,1X,A16)
C
C     + + + OUTPUT FORMATS + + +
 2030 FORMAT (4X,A2,4X,A16)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
C
C     cluster containing error message text
      SCLU= 211
C
C     initialize common block
      NCAT= 0
      DO 10 I= 1, MXCAT
        CATNAM(I)= '<unknown>       '
 10   CONTINUE
C
C     find table in uci (type 13 from hspf.seq, grp 22, col 3
      ITYP= 13
      CALL GETSE (ITYP,I1,
     O            KEYST,KEYND)
C
      IF (KEYST .EQ. 0) THEN
C       no category block
        IF (OUTLEV .GT. 0) THEN
C         message to that effect
          SGRP= 64
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
      ELSE
        IOPT= 2
        CALL HDMES2 (IOPT,ITYP,I1)
        IF (OUTLEV .GT. 0) THEN
C         processing message
          SGRP= 65
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
C       read past the uci heading
        KEY= KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        IF (KEY .NE. KEYND) THEN
          IF (OUTLEV .GT. 2) THEN
C           header
            SGRP= 66
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
C         loop to read categories
 20       CONTINUE
            ERRFLG= 0
C           process this category
            READ (UCIBF,1020,ERR=30) CATID,LCATNM
            IF (OUTLEV .GT. 2) THEN
C             echo this category
              WRITE(MESSU,2030) CATID,LCATNM
            END IF
C
            IF (NCAT .GT. 0) THEN
C             check for duplicate category
              I= CHKSTR (I2,NCAT,CATID1,CATTG1)
              IF (I .GT. 0) THEN
C               dup category id
                CALL OMSTC (I2,CATID1)
                SGRP= 62
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                ERRFLG= 1
              END IF
            END IF
C
C           check that first character is a letter
            ICH= ICHAR (CATID1(1))
            IF ( ( (ICH .LT. 97) .OR. (ICH .GT. 122) ) .AND.
     $           ( (ICH .LT. 65) .OR. (ICH .GT.  90) ) ) THEN
C             invalid id
              CALL OMSTC (I2,CATID1)
              SGRP= 62
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
            END IF
            IF (ERRFLG .EQ. 0) THEN
C             good new category
              NCAT= NCAT+ 1
              CATNAM(NCAT)= LCATNM
              CATTAG(NCAT)= CATID
            END IF
C           skip format error
            GO TO 40
 30         CONTINUE
C             format error
              I= 22
              CALL OMSTC (I,TBUF1)
              SGRP= 63
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
 40         CONTINUE
C           read the next record
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY .NE. KEYND) GO TO 20
        ELSE
C         no categories in block
          IF (OUTLEV .GT. 0) THEN
C           message to that effect
            SGRP= 67
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
        END IF
      END IF
C
      IF (OUTLEV .GT. 0) THEN
C       end processing message
        SGRP = 68
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PTHBLK
     I                   (KEYST,KEYND,OUTLEV,MESSU,MSGFL,
     M                    ECOUNT)
C
C     + + + PURPOSE + + +
C     Read and store pathnames and their associated dsns, files, and types
C     Check pathnames for validity, but not for presence in file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  KEYST,KEYND,OUTLEV,MESSU,MSGFL,ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  -
C     KEYND  -
C     OUTLEV -
C     MESSU  -
C     MSGFL  -
C     ECOUNT -
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cpthnm.inc'
      INCLUDE 'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RETCOD,I,I0,I8,I64,SCLU,SGRP,NKWDS,I1,
     #             CLEN,KEY,INITFG,CONT,I4,LEN,ITYP,IOPT
      CHARACTER*1  BLANK
      CHARACTER*64 CHSTR
      CHARACTER*80 UCIBUF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(64)
C
C     + + + FUNCTIONS + + +
      INTEGER LENSTR,CHKSTR
C     
C     + + + EXTERNALS + + +
      EXTERNAL ZCHKPN,LENSTR,ZIPC,ZIPI,DUMPER,GETUCI
      EXTERNAL WMSGTT,OMSTI,OMSTC,OMSG,CHKSTR,HDMES2
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I4,I8,I64/0,4,8,64/
      DATA BLANK/' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4A8)
 1020 FORMAT (I4,I2,1X,A8,1X,A64)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('='),/,' ','PROCESSING PATHNAMES BLOCK')
 2020 FORMAT (A64)
 2030 FORMAT (/,' FINISHED PROCESSING PATHNAMES BLOCK',
     #        /,' ',132('='))
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 211
C
C     initialize common variables
      DO 10 I= 1, MAXPTH
        CALL ZIPC (I64,BLANK,
     O             CPATH(I))
        CALL ZIPC (I8,BLANK,
     O             CTYPE(I))
 10   CONTINUE
C
      CALL ZIPI (MAXPTH,I0,
     O           DSSDSN)
      CALL ZIPI (MAXPTH,I0,
     O           DSSFL)
C
      NPATH= 0
C
C     begin reading block
C
      IF (KEYST .GT. 0) THEN
C       block has been supplied, read from message file the keyword
C       library for dss data types: per-aver,per-cum,inst-val,inst-cum
        NKWDS = 4
        SGRP  = 80
        INITFG= 1
        CLEN  = 32
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ (CHSTR,1000) (DTPKWL(I),I=1,NKWDS)
C
        IOPT= 2
        ITYP= 15
        I1  = 1
        CALL HDMES2(IOPT,ITYP,I1)
        IF (OUTLEV .GT. 0) THEN
          WRITE (MESSU,2000)
        END IF
C
        IF (OUTLEV .GT. 2) THEN
C         dump user's control input
          CALL DUMPER (KEYST,KEYND,MESSU)
        END IF
C
        KEY= KEYST
C       whiledo key <> keynd
 20     CONTINUE
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBUF)
          IF (KEY .NE. KEYND) THEN
C           read a pathname
            NPATH= NPATH+ 1
            IF (NPATH .LE. MAXPTH) THEN
C             process name
              READ (UCIBUF,1020,ERR=30) DSSDSN(NPATH), DSSFL(NPATH),
     #                           CTYPE(NPATH), CPATH(NPATH)
                GO TO 40
 30           CONTINUE
C               error - cannot read integers correctly
                CHSTR(1:6)= UCIBUF(1:6)
                I= 6
                CALL OMSTC (I,CHSTR1(1))
                SGRP= 129
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                DSSDSN(NPATH)= 99999
                DSSFL(NPATH)= 0
 40           CONTINUE
C
C             check if dss file number is legal
              IF ( (DSSFL(NPATH) .LE. 0) .OR.
     #             (DSSFL(NPATH) .GT. MAXDSS) ) THEN
C               error - dss file number is out of range
                CALL OMSTI (DSSDSN(NPATH))
                WRITE (CHSTR,2020) CPATH(NPATH)
                CALL OMSTC (I64,CHSTR1)
                CALL OMSTI (DSSFL(NPATH))
                CALL OMSTI (MAXDSS)
                SGRP= 81
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)              
              END IF
C
C             check type for validity
              CHSTR(1:8)= CTYPE(NPATH)
              DTYPI(NPATH)= CHKSTR (I8,I4,CHSTR1,DTPKW1)
              IF ( (DTYPI(NPATH) .EQ. 0) .OR.
     #             (DTYPI(NPATH) .EQ. 4) ) THEN
C               error - unrecognized dss data type
                CALL OMSTI (DSSDSN(NPATH))
                CHSTR(1:8)= CTYPE(NPATH)
                CALL OMSTC (I8,CHSTR1)
                CHSTR(1:8)= DTPKWL(1)
                CALL OMSTC (I8,CHSTR1)
                CHSTR(1:8)= DTPKWL(2)
                CALL OMSTC (I8,CHSTR1)
                CHSTR(1:8)= DTPKWL(3)
                CALL OMSTC (I8,CHSTR1)
                CHSTR(1:8)= DTPKWL(4)
                CALL OMSTC (I8,CHSTR1)
                SGRP= 82
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             check pathname for validity
              LEN= LENSTR (I64,CPATH(NPATH))
              CALL ZCHKPN
     M                    (CPATH(NPATH),
     I                     LEN,
     O                     RETCOD)
              IF (RETCOD .NE. 0) THEN
C               error - invalid pathname
                CALL OMSTI (DSSDSN(NPATH))
                WRITE (CHSTR,2020) CPATH(NPATH)
                CALL OMSTC (I64,CHSTR1)
                SGRP= 83
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             check for repetition
              RETCOD= 0
              DO 50 I= 1, NPATH- 1
                IF (DSSDSN(NPATH) .EQ. DSSDSN(I)) THEN
                  RETCOD= I
                END IF
                IF ( (CPATH(I) .EQ. CPATH(NPATH)) .AND.
     #               (DSSFL(I) .EQ. DSSFL(NPATH)) ) THEN
C                 error - data record referred by multiple dsns
                  CALL OMSTI (DSSFL(NPATH))
                  WRITE (CHSTR,2020) CPATH(NPATH)
                  CALL OMSTC (I64,CHSTR1)
                  CALL OMSTI (DSSDSN(I))
                  CALL OMSTI (DSSDSN(NPATH))
                  SGRP= 84
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
 50           CONTINUE
              IF (RETCOD .GT. 0) THEN
C               error - dsn refers to multiple data records
                CALL OMSTI (DSSDSN(RETCOD))
                SGRP= 85
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
            ELSE
C             error - too many pathnames
              CALL OMSTI (MAXPTH)
              SGRP= 86
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          END IF
        IF (KEY .NE. KEYND) GO TO 20
C       end whiledo
C
        IF (OUTLEV .GT. 0)  THEN
C         finished pathnames block
          WRITE (MESSU,2030)
        END IF
      END IF
C
      RETURN
      END
