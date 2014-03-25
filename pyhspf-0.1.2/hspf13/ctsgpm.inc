C
C     parameter for size of in memory timeseries instruction file
      INTEGER   MAXTSF
      PARAMETER(MAXTSF=40000)
C
C     in memory common block for timeseries instruction file
      COMMON/CTSGPM/TSGPM
      INTEGER       TSGPM(220,MAXTSF)
C
C     updated 12/6/2004 jlk&pbd
C       instruction size changed from 200 to 220 to 
C       store information needed to allow MFACT changes 
C       during execution of run