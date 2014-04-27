C     from CRIN3.INC   
C     run interpreter common
C       third version
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxopn.inc'
      INCLUDE 'pmxttb.inc'
      INCLUDE 'pmxrow.inc'
C
C     + + + HISTORY + + +
C     12/6/2004 - jlk&pbd - reformatted to better show TSGET/TSPUTinfo 
C                           for MFACT changes during execution, 
C                           increased size of DUMTSR to reflect longer
C                           TSGET/TSPUT instruction
C
      COMMON/CRINTP/FILE,DUMT1,DUMT2,DUMT3,WCOUNT,ECOUNT,OUTLEV,
     $              RESMFG,NIVLS,IVLLIB,NXGRPS,EXGTAB,NGRPS,GRPTAB,
     $              NOPNS,OPNO,OPNTAB,DUMS,SVOLNO,SNUM,SDELT,SUNITS,
     $              DUMG1,SAMDCD,SGAPCD,SFRC,SNTS,STABL,DUMT,TVOLNO,
     $              TNUM,TDELT,TUNITS,DUMG2,TAMDCD,TGAPCD,TFRC,TNTS,
     $              TTABL,DUMK,FMTINF,TDFREC,TDDS,TOTDS,RECLT,TDSIZE,
     $              DUML,TRNTAB,AVFG,RELFG,
     $              DUMTSR,
     $              DSDSNO,LBLSZ,DSFREC,
     $              DSLREC,VOFRWD,VOYEAR,LASTYR,DSSEC,BASEYR,VOKEY,
     $              KEYS,NAME,UNITS,DSCMPR,YEAROR,OBSTIM,STA,LOCATN,
     $              GAPCOD,DSDELT,NMEMS,IMEMNM,MSUB,MKIND,FMT,DUMLAB,
     $              DUM
      INTEGER       FILE(15),DUMT1,DUMT2,DUMT3,WCOUNT(10),ECOUNT,
     $              OUTLEV,RESMFG,NIVLS,IVLLIB(20),NXGRPS,
     $              EXGTAB(5,10),NGRPS,GRPTAB(5,10),NOPNS,OPNO,
     $              OPNTAB(20,MAXOPN),DUMS(2),SVOLNO,SNUM,SDELT,
     $              SUNITS,DUMG1(2),SAMDCD,SGAPCD,SFRC,SNTS,
     $              STABL(10,MAXTTB),DUMT(2),TVOLNO,TNUM,TDELT,TUNITS,
     $              DUMG2(2),TAMDCD,TGAPCD,TFRC,TNTS,TTABL(10,MAXTTB),
     $              DUMK(23),FMTINF(2,6),TDFREC,TDDS,TOTDS,RECLT,
     $              TDSIZE,DUML(8),TRNTAB(9,3,3),AVFG(MXROW),
     $              RELFG(MXROW),
     $              DUMTSR(280),
     $              DSDSNO,LBLSZ,DSFREC,
     $              DSLREC,VOFRWD,VOYEAR,LASTYR,DSSEC,BASEYR,VOKEY,
     $              KEYS(100),NAME(2),UNITS,DSCMPR,YEAROR,OBSTIM,
     $              STA(2),LOCATN(10),GAPCOD,DSDELT,NMEMS,
     $              IMEMNM(2,20),MSUB(20),MKIND(20),FMT(20),
     $              DUMLAB(294)
      INTEGER       DUM(886)
C
      EQUIVALENCE   (STABLR,STABL),(TTABLR,TTABL),(DSLABL,DSDSNO),
     $              (TBUFF,DSDSNO)
      REAL          STABLR(10,MAXTTB),TTABLR(10,MAXTTB),DSLABL(256),
     $              TBUFF(545)
C     end byte:  88000 word:22000
