C
C
C
      INTEGER FUNCTION   OPNNO
     I                         (OPTYP,LONO,HINO,MAXOPN,OPNTAB,OPST,
     I                          OPND)
C
C     + + + PURPOSE + + +
C     Search opntab from opst thru opnd for operation of type optyp and
C     op-type number in range lono thru hino.  return opnno of first
C     operation found, zero if none found
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     HINO,LONO,OPND,MAXOPN,OPNTAB(20,MAXOPN),OPST
      CHARACTER*6 OPTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTYP  - type of operation (PERNLD, IMPLND, etc)
C     LONO   - ???
C     HINO   - ???
C     MAXOPN - ???
C     OPNTAB - ???
C     OPST   - ???
C     OPND   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     OP
      CHARACTER*6 OPNTBC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(A4,A2)
C
C     + + + END SPECIFICATIONS + + +
C
      OPNNO= 0
C
C     whiledo op<= opnd and opnno= 0
      OP= OPST
 10   CONTINUE
C       check name
        WRITE(OPNTBC,2000) OPNTAB(1,OP),OPNTAB(2,OP)
        IF (OPNTBC .EQ. OPTYP) THEN
C         name agrees
          IF (LONO.LE.OPNTAB(3,OP) .AND. HINO.GE.OPNTAB(3,OP)) THEN
C           number agrees
            OPNNO= OP
          END IF
        END IF
        OP= OP+ 1
      IF (OP.LE.OPND .AND. OPNNO.EQ.0) GO TO 10
C     end whiledo
C
      RETURN
      END
C
C
C
      SUBROUTINE   TOPTNO
     I                    (MESSU,MSGFL,
     M                     TOPFST,TOPLST,ECOUNT)
C
C     + + + PURPOSE + + +
C     Check that the supplied target operation-type numbers, in an
C     entry in the ext sources or network block, are valid
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,MESSU,TOPFST,TOPLST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of hspf message file
C     TOPFST - ???
C     TOPLST - ???
C     ECOUNT - count(s) of specific errors
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP,SCLU
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 212
      IF (TOPLST.EQ.0) THEN
C       handle blank oplst field
        TOPLST= TOPFST
      END IF
C
C     check validity of opfst, oplst range
      IF (TOPFST.LE.0) THEN
C       error - topfst must be positive
        CALL OMSTI (TOPFST)
        SGRP = 8
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        TOPFST= 1
      END IF
C
      IF (TOPLST.LT.TOPFST) THEN
C       error - toplst must be >= topfst
        CALL OMSTI (TOPLST)
        CALL OMSTI (TOPFST)
        SGRP = 9
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        TOPLST= TOPFST
      END IF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   VALNO
     I                         (NVALS,VALLIB,VAL)
C
C     + + + PURPOSE + + +
C     Check whether a given integer value val is equal to a value in
C     a library vallib.  return the value no., zero if not found
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NVALS
      INTEGER    VAL,VALLIB(NVALS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVALS  - ???
C     VALLIB - ???
C     VAL    - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      VALNO= 0
C
C     whiledo i<= nvals and valno= 0
      I = 0
 10   CONTINUE
        I = I + 1
        IF (VAL .EQ. VALLIB(I)) THEN
C         a match
          VALNO= I
        END IF
C       loop back if needed
      IF (I .LT. NVALS .AND. VALNO .EQ. 0) GO TO 10
C     end whiledo
C
      RETURN
      END
C
C
C
      SUBROUTINE   TAGVAL
     I                    (CTAGIN,NEGFG,MESSU,MSGFL,SCLU,BGRP,
     M                     ECOUNT,
     O                     VAL)
C
C     + + + PURPOSE + + +
C     Look at a supplied character value.  If it's an integer, return the
C     value, if character return the index in the table of tags.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NEGFG,MESSU,MSGFL,SCLU,BGRP,ECOUNT,VAL
      CHARACTER*2 CTAGIN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CTAGIN - input subscript in variable form
C     NEGFG  - if 1, then return value is negative if CTAGIN is integer
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of hspf message file
C     SCLU   - 
C     BGRP   - base screen-group number of error messages
C     ECOUNT -
C     VAL    -
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phcat.inc'
      INCLUDE 'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I2,CHARFG,SGRP
      CHARACTER*2  LTAG
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LTAG1,LTAG)
      CHARACTER*1  LTAG1(2)
C
C     + + + FUNCTIONS + + +
      INTEGER CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL CHKSTR,OMSTC,OMSG
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I2)
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      CHARFG= 0
C
      LTAG= CTAGIN
      READ (LTAG,1000,ERR=10) VAL
      GO TO 20
 10   CONTINUE
C
C     value is not integer, so try reading as a tag
      CHARFG= 1
      IF (NCAT .GT. 0) THEN
C       there are tags to try
        VAL= CHKSTR (I2,NCAT,LTAG1,CATTG1)
        IF (VAL .EQ. 0) THEN
C         error - invalid tag
          SGRP= BGRP
          CALL OMSTC (I2,LTAG1)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          VAL= -999
        END IF
      ELSE
C       error - there are no tags
        SGRP= BGRP+ 1
        CALL OMSTC (I2,LTAG1)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        VAL= -999
      END IF
 20   CONTINUE
C
      IF ( (NEGFG .EQ. 1) .AND. (CHARFG .EQ. 0) ) THEN
C       negate
        VAL= -VAL
      END IF
C
      RETURN
      END
