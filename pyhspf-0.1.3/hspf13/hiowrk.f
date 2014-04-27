C
C     3.5.01.3
C
      SUBROUTINE WORKIO
     I                  (RWFG,LEN,KEY,
     M                   REC,MXKY)
C
C     + + + PURPOSE + + +
C     Perform i/o to workfl core equivalent
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEN
      INTEGER   RWFG,KEY,REC(LEN),MXKY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RWFG   - ???
C     LEN    - ???
C     KEY    - ???
C     REC    - ???
C     MXKY   - ???
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'cwork.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,LMXKY
      SAVE      LMXKY
C
C     + + + INTRINSICS + + +
      INTRINSIC  MAX0
C
C     + + + OUTPUT FORMATS + + +
C 1000 FORMAT(' IN WORKIO',3I10,/,2X,2A4,2I5,2(2X,2A4),16I5,
C     $       2F5.1,/,2X,2A4,2I5,2(2X,2A4),16I5)
C
C     + + + END SPECIFICATIONS + + +
C
C     check for key out of range? report error?
C
C     keep track of max record in WORKSPA
      IF (MXKY .EQ. -1) THEN
C       first call
        LMXKY= 1
      ELSE
C       other calls
        LMXKY= MAX0(LMXKY,KEY)
      END IF
      MXKY= LMXKY
C
      IF (RWFG .EQ. 0) THEN
C       read operation
        DO 10 I=1,LEN
          REC(I)=WRKSPA(I,KEY)
 10     CONTINUE
      ELSE
C       write operation
        DO 20 I=1,LEN
          WRKSPA(I,KEY)=REC(I)
 20     CONTINUE
      END IF
C
C     IF (LEN .GT. 30)
C    $                WRITE(*,1000) RWFG,LEN,KEY,REC
C
      RETURN
      END
