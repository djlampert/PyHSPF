C     from CMPAD.INC
C     scratch pad is the entire common block
      INTEGER   SCRSIZ,SCRSZ2
      PARAMETER (SCRSIZ=518000)
      PARAMETER (SCRSZ2=259000)
C
      INTEGER IPAD(SCRSIZ)
      REAL    PAD(SCRSIZ)
      DOUBLE PRECISION DPPAD(SCRSZ2)
C
      EQUIVALENCE (ASVEC(1),IPAD(1)),(ASVEC(1),PAD(1)),
     1            (ASVEC(1),DPPAD(1))
