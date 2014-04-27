C
C
C
      SUBROUTINE   GETTSI
     I                  (ILEN,KEY,
     O                   INSTR)
C
C     + + + PURPOSE + + +
C     Retrieve a time series instruction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ILEN,KEY,INSTR(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ILEN   - length of instruction - max 200
C     KEY    - record number
C     INSTR  - time series instruction
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsgpm.inc'
C
C     + + + EXTERNAL + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (ILEN,TSGPM(1,KEY),
     O            INSTR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTTSI
     I                   (ILEN,KEY,INSTR)
C
C     + + + PURPOSE + + +
C     Store a time series instruction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ILEN,KEY,INSTR(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ILEN   - length of record - max 200
C     KEY    - record number
C     INSTR  - time series instruction
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsgpm.inc'
C
C     + + + EXTERNAL + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (ILEN,INSTR,
     O            TSGPM(1,KEY))
C
      RETURN
      END

