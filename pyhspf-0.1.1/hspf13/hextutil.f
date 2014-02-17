C
C
C
      SUBROUTINE   UCIGET
     I                   (UCIFL,HMSGFL,OUTPFL,
     M                    ECOUNT)
C
C     + + + PURPOSE + + +
C     read uci from file, find major keywords
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UCIFL,HMSGFL,OUTPFL,ECOUNT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     UCIFL  - users control input unit number
C     HMSGFL - hspf message file unit number
C     OUTPFL - status/error message unit number
C     ECOUNT - count of errors found
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,INITFG,CLEN,CONT,KCNT,
     #             KWDDIM(1),KWDTYP(1),I0,I1
      CHARACTER*1  CHSTR1(20),KWDLIB(12)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     UCIINP,WMSGTT,KEYUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
      CALL UCIINP (UCIFL,HMSGFL,OUTPFL)
C     get major keywords (RUN)
      SCLU  = 201
      SGRP  = 21
      INITFG= 1
      CLEN  = 20
      CALL WMSGTT (HMSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) KWDLIB,KWDDIM,KWDTYP
C     look for major keywords
      CLEN  = 4
      CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M             ECOUNT,
     O             KCNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCIGEN
     I                   (HMSGFL,OUTPFL,UPDFIL,MKFILS,MAXOPN,MAXBLK,
     M                    ECOUNT,WCOUNT,
     O                    SEDAT,SDATIM,EDATIM,RUNMIN,OUTLEV,
     O                    RESMFG,RUNFG,EMFG,IHMFG,RNINFO,
     O                    NIVLS,IVLLIB,
     O                    EXGTAB,GRPTAB,OPNTAB,NXGRPS,NGRPS,
     O                    NOPNS,LKWOPR,NKWOPR,KWDOPR,SPOUT)
C
C     + + + PURPOSE + + +
C     process general run sections
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      HMSGFL,OUTPFL,UPDFIL,MKFILS,MAXOPN,
     #             ECOUNT,WCOUNT(10),
     #             SEDAT(10),SDATIM(5),EDATIM(5),MAXBLK,
     #             RUNMIN,OUTLEV,RESMFG,RUNFG,EMFG,NIVLS,IVLLIB(*),
     #             EXGTAB(5,10),GRPTAB(5,10),NOPNS,NGRPS,
     #             NXGRPS,OPNTAB(20,MAXOPN),LKWOPR,NKWOPR,SPOUT,IHMFG
      CHARACTER*80 RNINFO
      CHARACTER*1  KWDOPR(8,MAXBLK)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     HMSGFL - hspf message file unit number
C     OUTPFL - status/error message unit number
C     UPDFIL - update date file unit number
C     MKFILS - flag to indicate if instruction files should be written,
C                0-no,1-yes
C     MAXOPN - maximum number of operations allowed
C     ECOUNT - count of errors found
C     WCOUNT - count of warnings found
C     SEDAT  - start/end dates spec by user
C     SDATIM - start date in hspf format
C     EDATIM - end date in hspf format
C     RUNMIN - span of run in minutes
C     OUTLEV - run interpreter output level
C     RESMFG - resume flag - 0:standard mode, 1:resume mode
C     RUNFG  - run flag - 1:run with no errors
C                         0:just interp
C     EMFG   - english/metric units flag (english-1,metric-2)
C     IHMFG  - IHM flag (normal-0,IHM control-1)
C     RNINFO - character string of run information
C     NIVLS  - number of intervals allowed
C     IVLLIB - intervals allowed
C     EXGTAB - ???
C     GRPTAB - ???
C     OPNTAB - ???
C     NXGRPS - ???
C     NGRPS  - ???
C     NOPNS  - ???
C     SPOUT  - special action output level
C
C     + + + PARAMTERS + + +
      INTEGER    MAXBK
      PARAMETER (MAXBK=30)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,INITFG,CLEN,CONT,KCNT,
     #             KWDDIM(MAXBK),KWDTYP(MAXBK),J,NKWDS,
     #             I,I0,I1,LKWDS,KTYP
      CHARACTER*1  CHSTR1(80),KWDLIB(12,MAXBK)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*80 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT,CHRCHR,KEYUCI,GLOBLK,SEQBLK,PUTOLV,CATBLK
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
 1040 FORMAT (20I4)
C
C     + + + HISTORY + + +
C     05/06/2004  BRB added IHMFG to allow no data range checking for WDM datasets
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
C     run block keyword info
      SCLU  = 201
      SGRP  = 22
      INITFG= 1
      CLEN  = 80
      LKWDS = 12
      LKWOPR= 8
C
      I     = 0
      NKWOPR= 0
 20   CONTINUE
        I= I+ 1
        CALL WMSGTT (HMSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ(CHSTR,1000) (KWDLIB(J,I),J=1,LKWDS),KWDDIM(I),KWDTYP(I)
        IF (KWDTYP(I) .EQ. 100) THEN
C         operation type keyword, save for use in opn seq block proc
          NKWOPR= NKWOPR+ 1
          CALL CHRCHR(LKWOPR,KWDLIB(1,I),KWDOPR(1,NKWOPR))
        END IF
        INITFG= 0
        CLEN  = 80
      IF (CONT .EQ. 1) GO TO 20
      NKWDS= I
C      WRITE(99,*) '   got keywords:',NKWDS
C
C     locate the boundaries of each block of user's control input
      KTYP= 1
      CALL KEYUCI (NKWDS,LKWDS,I0,KTYP,I1,
     I             KWDLIB,KWDDIM,KWDTYP,
     M             ECOUNT,
     O             KCNT)
C
C     interval information
      NIVLS = 19
      SGRP  = 24
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (HMSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1040) (IVLLIB(J),J=1,NIVLS)
C
C     save the output level
      IF (MKFILS.EQ.0) THEN
C       force output level to zero
        OUTLEV = 0
      END IF
      CALL PUTOLV (OUTLEV)
C
      IF (ECOUNT .EQ. 0) THEN
        CALL GLOBLK (OUTPFL,HMSGFL,UPDFIL,
     M               ECOUNT,
     O               SEDAT,SDATIM,EDATIM,RUNMIN,OUTLEV,SPOUT,
     O               RESMFG,RUNFG,EMFG,RNINFO,IHMFG)
C
C       process the category block
        CALL CATBLK (OUTPFL,OUTLEV,HMSGFL,
     M               ECOUNT)
C
C       operation sequence block
        CALL SEQBLK (OUTPFL,HMSGFL,NIVLS,IVLLIB,
     I               OUTLEV,RUNMIN,MAXOPN,LKWOPR,NKWOPR,KWDOPR,
     M               WCOUNT,ECOUNT,
     O               EXGTAB,GRPTAB,OPNTAB,NXGRPS,NGRPS,
     O               NOPNS)
      END IF
C
      RETURN
      END
