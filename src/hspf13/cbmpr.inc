C     from CBMPR.INC   
C     BMPRAC module common
C
C     + + + PARAMETERS + + +
      INCLUDE 'phcat.inc'
C
      COMMON/SCRTCH/BMPID,BMPTYP,NCONS,NGQUAL,NCATS,NACID,PFLAG,PIVL,
     $              PYREND,BMPPFG,MESSU,MSGFL,BMPNO,DUMA,UUNITS,
     $              IUNITS,OUNITS,PUNIT,NDAY,DELT,NDAYS,HRFG,DAYFG,
     $              STFG,EDAYFG,EMONFG,EPYRFG,YR,MON,DAY,HR,MIN,IVL,
     $              IVL1,PIVLNO,NXTMON,SPAFP,SPAKND,SPIVL,SPOPNO,
     $              SPOUT,SPNUND,HYIF,IVOLFP,RCVOLX,HYIFC,CIVLFP,
     $              RCCVFP,VOLFFG,VOLFRM,VOLFRC,HYOF,ROVFP,HYOFC,
     $              CROVFP,HYRF,RMVFP,HYRFC,CRMVFP,CNIF,ICNFP,RCCNFP,
     $              CONFFG,CONFRM,CONID,CQTYID,CONFRC,CNOF,ROCNFP,
     $              CNRF,RMCNFP,HTIF,IHTFP,RCHTFP,HTFFG,HTFRM,HTFRC,
     $              HTOF,ROHTFP,HTRF,RMHTFP,SDIF,ISEDFP,RCSDFP,SEDFFG,
     $              SEDFRM,SEDFRC,SDOF,ROSDFP,SDRF,RMSDFP,GQDIF,IDQFP,
     $              RCDQFP,GQSIF,ISQFP,RCSQFP,GQFFG,GQDFRM,GQSFRM,
     $              GQID,GQTYID,GQDFRC,GQSFRC,GQDOF,RODQFP,GQSOF,
     $              ROSQFP,GQDRF,RMDQFP,GQSRF,RMSQFP,OXIF,IOXFP,
     $              RCOXFP,OXFFG,OXFRM,OXFRC,OXOF,ROOXFP,OXRF,RMOXFP,
     $              DNUIF,IDNUTX,RCDNUX,SNUIF,ISNUTX,RCSNUX,NUTFFG,
     $              DNUFRM,SNUFRM,DNUFRC,SNUFRC,DNUOF,RODNUX,SNUOF,
     $              ROSNUX,DNURF,RMDNUX,SNURF,RMSNUX,PKIF,IPLKFP
      COMMON/SCRTCH/RCPKFP,PLKFFG,PLKFRM,PLKFRC,PKOF,ROPLKX,PKRF,
     $              RMPLKX,PHIF,IPHFP,RCPHFP,PHFFG,PHFRM,PHFRC,PHOF,
     $              ROPHFP,PHRF,RMPHFP,ACIF,IACIDX,RCACIX,ACIFFG,
     $              ACIFRM,ACIFRC,ACOF,ROACIX,ACRF,RMACIX,DUMBMP
      INTEGER       BMPID(5),BMPTYP,NCONS,NGQUAL,NCATS,NACID,
     $              PFLAG(10),PIVL,PYREND,BMPPFG,MESSU,MSGFL,BMPNO,
     $              DUMA(2),UUNITS,IUNITS,OUNITS,PUNIT(2),NDAY(12),
     $              NDAYS,HRFG,DAYFG,STFG,EDAYFG,EMONFG,EPYRFG,YR,MON,
     $              DAY,HR,MIN,IVL,IVL1,PIVLNO,NXTMON,SPAFP,SPAKND,
     $              SPIVL,SPOPNO,SPOUT,SPNUND,IVOLFP,RCVOLX,
     $              CIVLFP(MXCAT),RCCVFP(MXCAT),VOLFFG,ROVFP,
     $              CROVFP(MXCAT),RMVFP,CRMVFP(MXCAT),ICNFP(10),
     $              RCCNFP(10),CONFFG(10),ROCNFP(10),RMCNFP(10),IHTFP,
     $              RCHTFP,HTFFG,ROHTFP,RMHTFP,ISEDFP(3),RCSDFP(3),
     $              SEDFFG(3),ROSDFP(3),RMSDFP(3),IDQFP(3),RCDQFP(3),
     $              ISQFP(3,3),RCSQFP(3,3),GQFFG(12),RODQFP(3),
     $              ROSQFP(3,3),RMDQFP(3),RMSQFP(3,3),IOXFP(2),
     $              RCOXFP(2),OXFFG(2),ROOXFP(2),RMOXFP(2),IDNUTX(4),
     $              RCDNUX(4),ISNUTX(3,2),RCSNUX(3,2),NUTFFG(10),
     $              RODNUX(4),ROSNUX(3,2),RMDNUX(4),RMSNUX(3,2),
     $              IPLKFP(5),RCPKFP(5),PLKFFG(5),ROPLKX(5),RMPLKX(5),
     $              IPHFP(2),RCPHFP(2),PHFFG(2),ROPHFP(2),RMPHFP(2)
      INTEGER       IACIDX(7),RCACIX(7),ACIFFG(7),ROACIX(7),RMACIX(7)
      REAL          DELT,HYIF(5),HYIFC(MXCAT,5),VOLFRM(12),VOLFRC,
     $              HYOF(5),HYOFC(MXCAT,5),HYRF(5),HYRFC(MXCAT,5),
     $              CNIF(10,5),CONFRM(12,10),CONID(5,10),CQTYID(2,10),
     $              CONFRC(10),CNOF(10,5),CNRF(10,5),HTIF(5),
     $              HTFRM(12),HTFRC,HTOF(5),HTRF(5),SDIF(3,5),
     $              SEDFRM(12,3),SEDFRC(3),SDOF(3,5),SDRF(3,5),
     $              GQDIF(3,5),GQSIF(3,3,5),GQDFRM(12,3),
     $              GQSFRM(12,3,3),GQID(5,3),GQTYID(2,3),GQDFRC(3),
     $              GQSFRC(3,3),GQDOF(3,5),GQSOF(3,3,5),GQDRF(3,5),
     $              GQSRF(3,3,5),OXIF(2,5),OXFRM(12,2),OXFRC(2),
     $              OXOF(2,5),OXRF(2,5),DNUIF(4,5),SNUIF(3,2,5),
     $              DNUFRM(12,4),SNUFRM(12,3,2),DNUFRC(4),SNUFRC(3,2),
     $              DNUOF(4,5),SNUOF(3,2,5),DNURF(4,5),SNURF(3,2,5),
     $              PKIF(5,5),PLKFRM(12,5),PLKFRC(5),PKOF(5,5),
     $              PKRF(5,5),PHIF(2,5),PHFRM(12,2),PHFRC(2),
     $              PHOF(2,5),PHRF(2,5),ACIF(7,5),ACIFRM(12,7),
     $              ACIFRC(7),ACOF(7,5),ACRF(7,5)
      INTEGER       DUMBMP(14192)
C
      EQUIVALENCE   (ASVEC,BMPID(1)),(UNIT,UUNITS),(TIMFG,HRFG),
     $              (DATIM,YR),(IVOL,HYIF(1)),(CIVOL,HYIFC(1,1)),
     $              (ROVOL,HYOF(1)),(CROVOL,HYOFC(1,1)),
     $              (RMVOL,HYRF(1)),(CRMVOL,HYRFC(1,1)),
     $              (ICON,CNIF(1,1)),(ROCON,CNOF(1,1)),
     $              (RMCON,CNRF(1,1)),(IHEAT,HTIF(1)),
     $              (ROHEAT,HTOF(1)),(RMHEAT,HTRF(1)),
     $              (ISED,SDIF(1,1)),(ROSED,SDOF(1,1)),
     $              (RMSED,SDRF(1,1)),(IDQAL,GQDIF(1,1)),
     $              (ISQAL,GQSIF(1,1,1)),(RODQAL,GQDOF(1,1))
      EQUIVALENCE   (ROSQAL,GQSOF(1,1,1)),(RMDQAL,GQDRF(1,1)),
     $              (RMSQAL,GQSRF(1,1,1)),(IOX,OXIF(1,1)),
     $              (ROOX,OXOF(1,1)),(RMOX,OXRF(1,1)),
     $              (IDNUT,DNUIF(1,1)),(ISNUT,SNUIF(1,1,1)),
     $              (RODNUT,DNUOF(1,1)),(ROSNUT,SNUOF(1,1,1)),
     $              (RMDNUT,DNURF(1,1)),(RMSNUT,SNURF(1,1,1)),
     $              (IPLK,PKIF(1,1)),(ROPLK,PKOF(1,1)),
     $              (RMPLK,PKRF(1,1)),(IPH,PHIF(1,1)),
     $              (ROPH,PHOF(1,1)),(RMPH,PHRF(1,1)),
     $              (IACID,ACIF(1,1)),(ROACID,ACOF(1,1))
      EQUIVALENCE   (RMACID,ACRF(1,1))
      INTEGER       ASVEC(1),UNIT(5),TIMFG(6),DATIM(5)
      REAL          IVOL,CIVOL(MXCAT),ROVOL,CROVOL(MXCAT),RMVOL,
     $              CRMVOL(MXCAT),ICON(10),ROCON(10),RMCON(10),IHEAT,
     $              ROHEAT,RMHEAT,ISED(3),ROSED(3),RMSED(3),IDQAL(3),
     $              ISQAL(3,3),RODQAL(3),ROSQAL(3,3),RMDQAL(3),
     $              RMSQAL(3,3),IOX(2),ROOX(2),RMOX(2),IDNUT(4),
     $              ISNUT(3,2),RODNUT(4),ROSNUT(3,2),RMDNUT(4),
     $              RMSNUT(3,2),IPLK(5),ROPLK(5),RMPLK(5),IPH(2),
     $              ROPH(2),RMPH(2),IACID(7),ROACID(7),RMACID(7)
C     end byte:  72000 word:18000
