C
C     common block to store variables to be output with next message
C
      COMMON /CHMSG/ICNT,IMSVL,RCNT,RMSVL,DATIM,CCNT
      INTEGER       ICNT,IMSVL(MXMSI),RCNT,DATIM(5),CCNT
      REAL          RMSVL(MXMSR)
C
      COMMON /CHMSGC/CMSVL
      CHARACTER*1    CMSVL(MXMSC)
C
