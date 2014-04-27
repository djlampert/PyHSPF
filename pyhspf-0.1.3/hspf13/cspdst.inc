C
C     include file CSPDST.INC - common block to store special action
C     distributions
      COMMON /CSPDST/ SPDCNT,SPDTST,SPDTCD,SPDDFG,SPDFRC
C
      INTEGER   SPDCNT(MXSPDS),SPDTST(MXSPDS),SPDTCD(MXSPDS),
     $          SPDDFG(MXSPDS)
      REAL      SPDFRC(10,MXSPDS)
C
C     SPDCNT - number of distribution intervals
C     SPDTST - time step between distribution intervals
C     SPDTCD - time code between distribution intervals: 2-minute; 3-hour;
C              4-day; 5-month; 6-year
C     SPDFFG - deferral flag: 1-skip; 2-shift; 3-accum
C     SPDFRF - fraction of total to apply this interval
C
