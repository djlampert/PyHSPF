C
C     3.5.2.2.2
C
      SUBROUTINE SEQDS
     I                 (MSGFL,MESSU,MEMN,MEMSB,SYST,GAPST,VOLNO,
     I                  UKEYST,UKEYND,MXTSTB,
     O                  NUM,DELT,UNT,NTS,GAPCD,
     O                  TABL,TABLR)
C
C     + + + PURPOSE + + +
C     Process a reference to a time series in a sequential file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MSGFL,MESSU,MEMSB,MXTSTB,
     #            VOLNO,UKEYST,UKEYND,
     #            NUM,DELT,UNT,NTS,TABL(10,MXTSTB),GAPCD
      REAL        TABLR(10,MXTSTB)
      CHARACTER*6 MEMN
      CHARACTER*4 GAPST,SYST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     MEMN   - ???
C     MEMSB  - ???
C     SYST   - ???
C     GAPST  - ???
C     VOLNO  - ???
C     UKEYST - ???
C     UKEYND - ???
C     MXTSTB - ???
C     NUM    - ???
C     DELT   - simulation time interval in minutes
C     UNT    - ???
C     NTS    - ???
C     GAPCD  - ???
C     TABL   - ???
C     TABLR  - ???
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE   'crin3.inc'
      INCLUDE   'crin3c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      CLASS,I0,I2,I4,I6,REC,FMTCOD,K,SFREC,SCLU,SGRP
      CHARACTER*4  BLNK
      CHARACTER*6  CHSTR
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER    CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTI,OMSG,OMSTC,HSCKFL,GETUCI,CHKSTR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I4)
 1010 FORMAT (A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (' BEGIN CHECKING/EXPANDING A SEQUENTIAL FILE REFERENCE')
 2020 FORMAT (' END CHECKING/EXPANDING A SEQUENTIAL FILE REFERENCE')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 214
C
      BLNK= '    '
      I0  = 0
      I2  = 2
      I4  = 4
      I6  = 6
      IF (OUTLEV .GT. 6) THEN
        WRITE(MESSU,2010)
      END IF
C
      NTS = 0
      IF (VOLNO .GE. 14) THEN
C       unit number for the file is valid.
C
C       check output file - if not open,
C       then open it with a standard name
        CALL HSCKFL
     I              (VOLNO)
C
C       look up format class.
        CHSTR= MEMN
        CLASS= CHKSTR(I6,I6,CHSTR1,FMTKW1)
C
        IF (CLASS .EQ. 0) THEN
C         error-invalid format class.
          CALL OMSTI (VOLNO)
          CHSTR= MEMN
          CALL OMSTC (I6,CHSTR1)
          SGRP = 1
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         check for user supplied format
          IF (MEMSB .NE. 0) THEN
C           make sure format is really in the sformats block
            SFREC= 0
C           see if block is present
            IF (UKEYST .GT. 0) THEN
              REC= UKEYST
 20           CONTINUE
                CALL GETUCI (I0,
     M                       REC,
     O                       UCIBF)
                IF (REC .NE. UKEYND) THEN
C                 check this format code
                  READ(UCIBF,1000,ERR=30) FMTCOD
                    GO TO 40
 30               CONTINUE
C                   error - invalid numeric input
                    CHSTR(1:4)= UCIBF(1:4)
                    CALL OMSTC (I4,CHSTR1)
                    SGRP = 6
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                    FMTCOD= -999
 40               CONTINUE
                  IF (FMTCOD .EQ. MEMSB) THEN
C                   a match
                    SFREC= REC
                  END IF
                END IF
              IF (SFREC.EQ.0 .AND. REC.NE.UKEYND) GO TO 20
            END IF
C
            IF (SFREC .EQ. 0) THEN
C             error-format not found in formats block or no formats block
              CALL OMSTI (VOLNO)
              SGRP = 2
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          ELSE
C           take the default for the format.
            SFREC= 0
          END IF
C         check for gap value
          IF (GAPST .EQ. BLNK) THEN
C           default to gap value of zero
            GAPCD=0
          ELSE
            CHSTR(1:4)= GAPST
            GAPCD     = CHKSTR(I4,I2,CHSTR1,GAPKW1)
            IF (GAPCD .EQ. 0) THEN
C             error-invalid gap value requested. zero assumed.
              CALL OMSTI (VOLNO)
              SGRP = 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
              GAPCD=GAPCD-1
            END IF
          END IF
C
C         check for user supplied units.
          CHSTR(1:4)= SYST
          UNT= CHKSTR(I4,I2,CHSTR1,SYSKW1)
C
          IF (SYST .EQ. BLNK) THEN
C           blank - default to english
            UNT= 1
          END IF
          IF (UNT .EQ. 0) THEN
C           error-invalid source units given.
            CALL OMSTI (VOLNO)
            CALL OMSTC (I4,CHSTR1)
            SGRP= 4
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          ELSE
            NTS= 1
            NUM= -VOLNO
            DELT= FMTINF(1,CLASS)
            TABL(6,NTS)= FMTINF(2,CLASS)
C
            READ(MEMN,1010) (TABL(K,NTS),K=1,2)
            TABL(3,NTS)= CLASS
            TABL(4,NTS)= SFREC
            TABL(5,NTS)= 0
            TABLR(8,NTS)= 0.0
            TABLR(9,NTS)= 1.0
          END IF
        END IF
      ELSE
C       error-invalid unit number. unit number for sequential files
C       must be >= 14.
        CALL OMSTI (VOLNO)
        SGRP = 5
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
