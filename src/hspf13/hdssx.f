C
C
C
      SUBROUTINE   ATTEND
      RETURN
      END
C
C
C
      SUBROUTINE   DSSCLO
      RETURN
      END
C
C
C
      SUBROUTINE   ZSET
     I                 (CH1,CH2,INT1)
      CHARACTER*(*)     CH1,CH2
      INTEGER           INT1
      RETURN
      END
C
C
C
      SUBROUTINE   CHKDSS
     I                   (MESSU,MSGFL,
     M                    ECOUNT)
      INTEGER MESSU,MSGFL,ECOUNT
      RETURN
      END
C
C
C
      SUBROUTINE   GETDSS
     I                   (DELT,WIDTH,FSTCAL)
      INTEGER   DELT,WIDTH,FSTCAL
      RETURN
      END
C
C
C
      SUBROUTINE   PUTDSS
     I                   (DELT,WIDTH,FSTCAL)
      INTEGER   DELT,WIDTH,FSTCAL
      RETURN
      END
C
C
C
      SUBROUTINE   ZOPEN
     I                   (IDUM1,CDUM1,
     O                    ISTAT)
      INTEGER IDUM1, ISTAT
      CHARACTER*(*) CDUM1
      ISTAT= -999
      RETURN
      END
C
C
C
      SUBROUTINE   ATTACH
     I                    (IDUM1,CDUM1,CDFALT,CDUM2,
     O                     CNAME,ISTAT)
      INTEGER IDUM1,ISTAT
      CHARACTER*(*) CDUM1,CDUM2,CDFALT,CNAME
      CNAME= CDFALT
      ISTAT= 0
      RETURN
      END
C
C
C
      SUBROUTINE   ZCHKPN
     I                   (CDUM1,IDUM1,
     O                    ISTAT)
      INTEGER IDUM1, ISTAT
      CHARACTER*(*) CDUM1
      ISTAT= 0
      RETURN
      END
C
C
C
      SUBROUTINE   DSSDS
     I                   (MSGFL,MESSU,VOLNO,D1,D2,D3,D4,D5,D6,D7,D8,D9,
     I                    D10,D11,
     M                    ECOUNT,
     O                    D12,D13,D14,D15,D16,D17,D18,D19)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MSGFL,MESSU,VOLNO,ECOUNT,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,
     #        D11,D12,D13,D14,D15,D16,D17,D18,D19
C
C     + + + LOCAL VARIABLES + + +
      INTEGER SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL OMSTI,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA SCLU,SGRP/299,99/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL OMSTI (VOLNO)
      CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M           ECOUNT)
C
      RETURN
      END
