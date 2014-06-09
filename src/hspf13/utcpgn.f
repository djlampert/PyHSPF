C
C
C
      SUBROUTINE   COPYD
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the double precision array ZIP of size LEN
C     to the double precision array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      DOUBLE PRECISION   ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
        X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPYI
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the integer array ZIP of size LEN to
C     the integer array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      INTEGER     ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPYR
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the real array ZIP of size LEN to the
C     real array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      REAL        ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
