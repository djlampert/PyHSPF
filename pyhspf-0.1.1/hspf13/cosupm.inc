C
C     parameter for size of in memory OSUP file
C     calculated as MAXOPN+ 10 (max exgroups)+ 10 (max ingroups)+ 1 (runinfo)
      INTEGER   MAXOSP
      PARAMETER(MAXOSP=521)
C
C     in memory common block for OSUP file
      COMMON/COSUPM/OSUPM
      INTEGER       OSUPM(11,MAXOSP)
C
