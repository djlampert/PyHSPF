C     from CRIN4.INC   
C     run interpreter common
C       fourth version
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxopn.inc'
      INCLUDE 'pmxttb.inc'
      INCLUDE 'pmxrow.inc'
C
C     + + + HISTORY + + +
C     12/6/2004 - jlk&pbd - reformatted to better show TSGET/TSPUT
C                           for MFACT changes during execution,
C                           added variables for enhanced TSGET/TSPUT 
C                           instructions
C
      COMMON/CRINTP/FILET,DUMT1,DUMT2,DUMT3,WCOUNT,ECOUNT,OUTLEV,
     $              RESMFG,NIVLS,IVLLIB,NXGRPS,EXGTAB,NGRPS,GRPTAB,
     $              NOPNS,OPNO,OPNTAB,SVOL,SVOLNO,SNUM,SDELT,SUNITS,
     $              SGRPN,SAMDCD,SGAPCD,SFRC,SNTS,STABL,TVOL,TVOLNO,
     $              TNUM,TDELT,TUNITS,TGRPN,TAMDCD,TGAPCD,TFRC,TNTS,
     $              TTABL,DUMK,FMTINF,TDFREC,TDDS,TOTDS,RECLT,TDSIZE,
     $              DUML,TRNTAB,AVFG,RELFG,
     $              AMODE,NCOMPS,VOLCOD,FILE,
     $              DELTAT,FREC,LREC,COMPR,TOTCOM,FMTCLS,BEGYR,TENDR,
     $              LGAP,TGAP,GAPVAL,LTRNFG,VOTSB,VOLFRM,VOTFRM,
     $              BCWBTI,BCWNOV,INPSTR,ENDF,YEAR,MO,DAY,LPYRFG,NREM,
     $              TYREND,FRMTIM,TZERO,TLAST,UCNT,ZCNT,VOPADR,OFFSET,
     $              STKIND,STTRAN,INMODE,A,B,PVAR,INPDAT,
     $              SVOLX,SNUMX,SGRPNX,SMEMN,SMEMSB,ARATIO,
     $              TVOLX,TNUMX,TGRPNX,TMEMN,TMEMSB,DUMMF,
     $              MESSU,UCIFL,
     $              TSGETF,TSPUTF,WORKFL,MSGFL,RATIO,RRATIO,XVAR,WS,
     $              TSSFL,BADR,BLEN,CWF,FORFLG,BMTFLG,EXTF,TRCNO,
     $              VOBUFF,ENDBUF,
     $              DSDSNO,LBLSZ,DSFREC,DSLREC,VOFRWD,
     $              VOYEAR,LASTYR,DSSEC,BASEYR,VOKEY,KEYS,NAME,UNITS,
     $              DSCMPR,YEAROR,OBSTIM,STA,LOCATN,GAPCOD,DSDELT,
     $              NMEMS,IMEMNM,MSUB,MKIND,FMT,DUMLAB,DUM
      INTEGER       FILET(15),DUMT1,DUMT2,DUMT3,WCOUNT(10),ECOUNT,
     $              OUTLEV,RESMFG,NIVLS,IVLLIB(20),NXGRPS,
     $              EXGTAB(5,10),NGRPS,GRPTAB(5,10),NOPNS,OPNO,
     $              OPNTAB(20,MAXOPN),SVOL(2),SVOLNO,SNUM,SDELT,
     $              SUNITS,SGRPN(2),SAMDCD,SGAPCD,SFRC,SNTS,
     $              STABL(10,MAXTTB),TVOL(2),TVOLNO,TNUM,TDELT,TUNITS,
     $              TGRPN(2),TAMDCD,TGAPCD,TFRC,TNTS,TTABL(10,MAXTTB),
     $              DUMK(23),FMTINF(2,6),TDFREC,TDDS,TOTDS,RECLT,
     $              TDSIZE,DUML(8),TRNTAB(9,3,3),AVFG(MXROW),
     $              RELFG(MXROW),
     $              AMODE,NCOMPS,VOLCOD,FILE,DELTAT,FREC,
     $              LREC,COMPR,TOTCOM,FMTCLS,BEGYR,TENDR,LGAP,TGAP,
     $              LTRNFG,VOTSB,VOLFRM,VOTFRM,BCWBTI,BCWNOV,INPSTR,
     $              ENDF,YEAR,MO,DAY,LPYRFG,NREM,TYREND,FRMTIM,TZERO,
     $              TLAST,UCNT,ZCNT,VOPADR(20),OFFSET(20),STKIND(20),
     $              STTRAN(20),INMODE(20),INPDAT(6),
     $              SVOLX(2),SNUMX,SGRPNX(2),SMEMN(2),SMEMSB(2),
     $              TVOLX(2),TNUMX,TGRPNX(2),TMEMN(2),TMEMSB(2),DUMMF,
     $              MESSU,UCIFL,
     $              TSGETF,TSPUTF,WORKFL,MSGFL,RATIO,TSSFL,BADR,BLEN,
     $              CWF,FORFLG,BMTFLG,EXTF,TRCNO,VOBUFF,ENDBUF,DSDSNO,
     $              LBLSZ,DSFREC,DSLREC,VOFRWD,VOYEAR,LASTYR,DSSEC
      INTEGER       BASEYR,VOKEY,KEYS(100),NAME(2),UNITS,DSCMPR,
     $              YEAROR,OBSTIM,STA(2),LOCATN(10),GAPCOD,DSDELT,
     $              NMEMS,IMEMNM(2,20),MSUB(20),MKIND(20),FMT(20),
     $              DUMLAB(294)
      REAL          GAPVAL,A(20),B(20),PVAR(20),RRATIO,XVAR(20),
     $              WS(22),ARATIO
      INTEGER       DUM(886)
C
      EQUIVALENCE   (STABLR,STABL),(TTABLR,TTABL),(INSTR,AMODE),
     $              (CTYPI,FREC),(QLFG,COMPR),(CRDSEQ,VOTSB),
     $              (BCW,BCWBTI),(AGGRFG,LPYRFG),(OLDOFF,TZERO),
     $              (CRDNO,UCNT),(DSLABL,DSDSNO),(TBUFF,DSDSNO)
      INTEGER       INSTR(200),CTYPI(2),QLFG,CRDSEQ(3),BCW,AGGRFG,
     $              OLDOFF,CRDNO
      REAL          STABLR(10,MAXTTB),TTABLR(10,MAXTTB),DSLABL(256),
     $              TBUFF(545)
C     end byte:  88000 word:22000
