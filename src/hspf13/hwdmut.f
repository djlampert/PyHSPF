C
C     3.5.1.3
C
      SUBROUTINE CHKWDM
     I                  (WDMSFL,MESSU,MSGFL,
     M                   ECOUNT)
C
C     + + + PURPOSE + + +
C     Initialize arrays needed for wdmsfl routines
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,MESSU,
     $           WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     ECOUNT - count(s) of specific errors
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SCLU,SGRP,RETCOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDFLCK,OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 206
C     initialize array for wdms routines
      CALL WDFLCK (WDMSFL,
     O             RETCOD)
C
      IF (RETCOD .NE. 0) THEN
C       wdms routines not successfully initialized, must terminate
C       run at this point
        CALL OMSTI (RETCOD)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      RETURN
      END
