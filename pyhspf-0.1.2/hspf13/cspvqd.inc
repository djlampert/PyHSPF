C
C     include file CSPVQD.INC - common block to store special action
C     conditions
      COMMON /CSPVQD/ NVQD,UVQOPN,UVQOPX,UVQSUB,UVQADD,UVQTYP,UVQMUL,
     $                UVQLAG,UVQAGG,UVQAFG,UVQPOS,UVQLEN,UVQPIP
C
      INTEGER         NVQD,UVQOPN(2,MXSPVQ),UVQOPX(MXSPVQ),
     $                UVQSUB(3,MXSPVQ),UVQADD(MXSPVQ),
     $                UVQAGG(MXSPVQ),UVQAFG(MXSPVQ),UVQPOS(MXSPVQ),
     $                UVQTYP(MXSPVQ),UVQLAG(MXSPVQ),UVQLEN(MXSPVQ)
      REAL            UVQMUL(MXSPVQ),UVQPIP(MXPIPE)
C
      EQUIVALENCE    (IVQPIP,UVQPIP)
      INTEGER         IVQPIP(MXPIPE)
C
      COMMON /SPUVQC/ UVQNAM,UVQBNM
      CHARACTER*6     UVQNAM(MXSPVQ),UVQBNM(MXSPVQ)
C
      EQUIVALENCE    (UVQNAM,UVQNM1),(UVQBNM,UVQBN1)
      CHARACTER*1     UVQNM1(MXSPVQ*6),UVQBN1(MXSPVQ*6)
C
C     NVQD   - number of UVQUANs
C     UVQOPN - OMCODE of base operation
C     UVQOPX - index of base operation in OPN-SEQUENCE block
C     UVQSUB - subscripts of base variable
C     UVQADD - address of base variable
C     UVQTYP - type of UVQUAN: 2-integer; 3-real; 4-double precision
C     UVQMUL - multiplier on base variable
C     UVQLAG - number of intervals to lag value(s) of base variable
C     UVQAGG - number of intervals to aggregate value(s) of base variable
C     UVQAFG - aggregation flag: 1-sum; 2-aver (can't be integer); 3-max; 4-min;
C     UVQPOS - starting position of values in pipe
C     UVQLEN - length of (i.e. number of values in) pipe
C     UVQPIP - pipe containing base values
C     UVQNAM - name of UVQUAN
C     UVQBNM - name of base variable
C
