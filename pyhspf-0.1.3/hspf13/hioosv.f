C
C
C
      SUBROUTINE   GETOSV
     I                    (KEYST,KEYND,MAXOSV,
     O                     OSV)
C
C     + + + PURPOSE + + +
C     Move an osv from osvfl into memory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   KEYND,KEYST,MAXOSV,OSV(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXOSV - maximum size of osv
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KEY,START,ILEN
C
C     + + + EXTERNAL + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     start at first position
      START= 1
C     how much to read
      ILEN = 500
C
      DO 10 KEY= KEYST,KEYND
C       loop thru records
C       read a record from memory
        CALL COPYI (ILEN,OSVM(1,KEY),
     O              OSV(START))
C       next position in memory
        START= START+ ILEN
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTOSV
     I                    (KEYST,KEYND,MAXOSV,OSV)
C
C     + + + PURPOSE + + +
C     Move an osv from memory to osvfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYND,KEYST,MAXOSV,OSV(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXOSV - maximum size of osv
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KEY,ILEN,START
C
C     + + + EXTERNAL + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     start at first position
      START= 1
C     how much to read
      ILEN = 500
C
      DO 10 KEY= KEYST,KEYND
C       loop thru records
C       write a record to memory
        CALL COPYI(ILEN,OSV(START),
     O             OSVM(1,KEY))
C       next position in memory
        START= START+ ILEN
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTOSVI
     I                    (RECORD,OFFSET,
     O                     IVAL)
C
C     + + + PURPOSE + + +
C     Retrieve a single integer value directly from the OSV file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RECORD,OFFSET,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RECORD - index of OSV block
C     OFFSET - address within OSV block
C     IVAL   - value being fetched
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL= OSVM(OFFSET,RECORD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PTOSVI
     I                    (RECORD,OFFSET,IVAL)
C
C     + + + PURPOSE + + +
C     Reset a single integer value directly into the OSV file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RECORD,OFFSET,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RECORD - index of OSV block
C     OFFSET - address within OSV block
C     IVAL   - value being replaced
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      OSVM(OFFSET,RECORD)= IVAL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTOSVR
     I                    (RECORD,OFFSET,
     O                     RVAL)
C
C     + + + PURPOSE + + +
C     Retrieve a single real value directly from the OSV file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RECORD,OFFSET
      REAL      RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RECORD - index of OSV block
C     OFFSET - address within OSV block
C     RVAL   - value being fetched
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL    R
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (I,R)
      INTEGER I
C
C     + + + END SPECIFICATIONS + + +
C
      I= OSVM(OFFSET,RECORD)
      RVAL= R
C
      RETURN
      END
C
C
C
      SUBROUTINE   PTOSVR
     I                    (RECORD,OFFSET,RVAL)
C
C     + + + PURPOSE + + +
C     Reset a single integer value directly into the OSV file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RECORD,OFFSET
      REAL      RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RECORD - index of OSV block
C     OFFSET - address within OSV block
C     RVAL   - value being replaced
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL    R
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (I,R)
      INTEGER I
C
C     + + + END SPECIFICATIONS + + +
C
      R= RVAL
      OSVM(OFFSET,RECORD)= I
C
      RETURN
      END
