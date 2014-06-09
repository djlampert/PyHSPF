C
C     include file CSPCND.INC - common block to store special action
C     conditions
      COMMON /CSPCND/ NCOND,CNDCOD,CNDVAL,CNDUVQ,CNDLNK,CNDLOG,CNDPOP,
     $                NBLOCK,BLKCNT,BLKPOS,BLKCHN,BLKLVL,NCHAIN,
     $                CHNCND,CHNUVQ,CHNKEY
C
      INTEGER         NCOND,CNDCOD(MXSPCN),CNDUVQ(2,MXSPCN),
     $                CNDLNK(MXSPCN),CNDLOG(MXSPCN),CNDPOP(MXSPCN),
     $                NBLOCK,BLKCNT(MXSPBK),BLKPOS(MXSPBK),
     $                BLKCHN(MXSPCR),BLKLVL(MXSPBK),NCHAIN,
     $                CHNCND(MXSPCH),CHNUVQ(MXSPCH),CHNKEY(MXSPCH)
      REAL            CNDVAL(2,MXSPCN)
C
C     NCOND  - number of logical conditions
C     CNDCOD - comparison operator:  1   2   3   4   5   6
C                                    =  /=   >  >=   <  <=
C     CNDVAL - constant comparand
C     CNDUVQ - index of UVQUAN for variable comparand
C     CNDLNK - pointer to next condition in chain, or zero if last
C     CNDLOG - logic operator to next condition in chain: 1-and; 2-or
C     CNDPOP - number of logic operators to pop off stack after evaluating
C              this condition
C     NBLOCK - number of logical blocks
C     BLKCNT - number of chains in BLKCHN defining logical block
C     BLKPOS - position in BLKCHN of first chain reference for logical block
C     BLKCHN - absolute value is index of chain in CHNCND - sign is desired
C              truth-value
C     BLKLVL - nesting level of logical block (max is MXBKLV in PSPUCI)
C     CHNCND - index of first logical condition in chain
C     CHNUVQ - index of internally-generated UVQUAN containing logical
C              value of chain each interval
C
