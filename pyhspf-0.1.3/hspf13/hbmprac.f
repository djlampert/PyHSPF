C
C
C
      SUBROUTINE   PBMPRC
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the BMPRAC module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes ???
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - key to where we are in OSV file
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR1 + + +
      INCLUDE    'crin2.inc'
      INCLUDE    'cbmpr.inc'
      INCLUDE    'cmpad.inc'
      INCLUDE    'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,N,IVAL(12),TBNO,TBSB,NVAL,RETCOD,LEV,OSVREC,
     $            OSVKST,OSVKND
      REAL        RVAL(11)
C
C     + + + FUNCTIONS + + +
      INTEGER     DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYMNH,ITABLE,RTABLE,MDATBL,BMPRST,PUTOSV
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING BMP GENERATOR NO:',I4,
     $            '     TIME STEP(DELT):',I5,'  MINS')
 2010 FORMAT (/,' FINISHED PROCESSING BMP GENERATOR NO. ',I4,
     $        /,' ',132('+'))
 2020 FORMAT (  ' ',132('-'))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RESMFG.EQ.1) THEN
C       read the general part of the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area
        DO 10 I= 1,MAXOSV
          IPAD(I)= -999
 10     CONTINUE
      END IF
C
C     file units in use by this opertation
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPNTAB(3,OPNO), NDELT
      END IF
C
C     id for this bmp
      BMPNO= OPNTAB(3,OPNO)
C     minimum size of osv for this operation
      OSVREC= 8
C
C     put english/metric units flag into implnd common
      UUNITS = EMFG
C
      DELT  = NDELT
C
      DO 20 I= 1,5
        DATIM(I)= SDATIM(I)
 20   CONTINUE
C
      NDAYS= DAYMNH (YR,MON,NDAMON)
      DO 30 I= 1,12
        NDAY(I)= NDAMON(I)
 30   CONTINUE
C
      HRFG  = 1
      DAYFG = 1
      PIVLNO= 0
      STFG  = 1
      SPIVL=  0
      SPOPNO= OPNO
C
      IF (MON.LT.12) THEN
        NXTMON= MON+ 1
      ELSE
        NXTMON= 1
      END IF
C
C     process table-type print-info
      TBNO= 1
      TBSB= 1
      NVAL= 11
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      DO 40 I= 1, 9
        PFLAG(I)= IVAL(I)
 40   CONTINUE
C     printout flag
      PFLAG(10)= PFLAG(9)
      PIVL= IVAL(10)
      PYREND= IVAL(11)
C     dummy value of overall print flag until run starts
      BMPPFG= 6
C
C     process table-type gen-info
      TBNO= 2
      TBSB= 1
      NVAL= 12
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      DO 50 I= 1, 5
        BMPID(I)= IVAL(I)
 50   CONTINUE
      BMPTYP= IVAL(6)
      NCONS=  IVAL(7)
      NGQUAL= IVAL(8)
      IUNITS= IVAL(9)
      OUNITS= IVAL(10)
      PUNIT(1)= IVAL(11)
      PUNIT(2)= IVAL(12)
C
C     check output files - if not open,
C     then open them with a standard name
      DO 55 I= 1, 2
        IF (PUNIT(I) .GT. 0) THEN
          CALL HSCKFL
     I                (PUNIT(I))
        END IF
 55   CONTINUE
C
C     process table-type flow-flag
      TBNO= 3
      TBSB= 1
      NVAL= 1
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             VOLFFG)
C
C     process table-type flow-frac
      TBNO= 4
      TBSB= 1
      NVAL= 1
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             VOLFRC)
C
C     check if monthly values needed
      IF (VOLFFG .GE. 1) THEN
C       monthly fraction must be read
        CALL MDATBL (VOLFFG,
     O               VOLFRM,RETCOD)
      END IF
C
      IF (NCAT .GE. 1) THEN
C       there are categories in category block - place in osv
        NCATS= NCAT
      ELSE
C       no categories
        NCATS= 0
      END IF
C
      IF (NCONS .GE. 1) THEN
C       handle conseratives
C
C       process table-type cons-flag
        TBNO= 5
        TBSB= 1
        NVAL= NCONS
        CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M               CONFFG)
C     
C       process table-type cons-frac
        TBNO= 6
        NVAL= 8
        DO 70 TBSB= 1, NCONS
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL)
          DO 60 I= 1, 5
            CONID(I,TBSB)= RVAL(I)
 60       CONTINUE
          CQTYID(1,TBSB)= RVAL(6)
          CQTYID(2,TBSB)= RVAL(7)
          CONFRC(TBSB)= RVAL(8)
C
C         check if monthly values needed
          IF (CONFFG(TBSB) .GE. 1) THEN
C           monthly fraction must be read
            CALL MDATBL (CONFFG(TBSB),
     O                   CONFRM(1,TBSB),RETCOD)
          END IF
 70     CONTINUE
      END IF
C
C     process table-type heat-flag
      TBNO= 7
      TBSB= 1
      NVAL= 1
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             HTFFG)
C
C     process table-type heat-frac
      TBNO= 8
      TBSB= 1
      NVAL= 1
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             HTFRC)
C
C     check if monthly values needed
      IF (HTFFG .GE. 1) THEN
C       monthly fraction must be read
        CALL MDATBL (HTFFG,
     O               HTFRM,RETCOD)
      END IF
C
C     process table-type sed-flag
      TBNO= 9
      TBSB= 1
      NVAL= 3
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             SEDFFG)
C
C     process table-type sed-frac
      TBNO= 10
      TBSB= 1
      NVAL= 3
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             SEDFRC)
C
C     check if monthly values needed
      DO 80 I= 1, 3
        IF (SEDFFG(I) .GE. 1) THEN
C         monthly fraction must be read
          CALL MDATBL (SEDFFG(I),
     O                 SEDFRM(1,I),RETCOD)
        END IF
 80   CONTINUE
C
      IF (NGQUAL .GE. 1) THEN
C       handle quals
C
C       process table-type gq-flag
        TBNO= 11
        TBSB= 1
        NVAL= NGQUAL*4
        CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M               GQFFG)
C
C       process table-type gq-frac
        TBNO= 12
        NVAL= 11
        DO 110 TBSB= 1, NGQUAL
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL)
          DO 90 I= 1, 5
            GQID(I,TBSB)= RVAL(I)
 90       CONTINUE
          GQTYID(1,TBSB)= RVAL(6)
          GQTYID(2,TBSB)= RVAL(7)
          GQDFRC(TBSB)= RVAL(8)
C
C         check if monthly values needed
          N= (TBSB- 1)*4+ 1
          IF (GQFFG(N) .GE. 1) THEN
C           monthly fraction must be read
            CALL MDATBL (GQFFG(N),
     O                   GQDFRM(1,TBSB),RETCOD)
          END IF
          DO 100 I= 1, 3
            GQSFRC(I,TBSB)= RVAL(I+ 8)
C
C           check if monthly values needed
            N= (TBSB- 1)*4+ I+ 1
            IF (GQFFG(N) .GE. 1) THEN
C             monthly fraction must be read
              CALL MDATBL (GQFFG(N),
     O                     GQSFRM(1,I,TBSB),RETCOD)
            END IF
 100      CONTINUE
 110    CONTINUE
      END IF
C
C     process table-type oxy-flag
      TBNO= 13
      TBSB= 1
      NVAL= 2
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             OXFFG)
C
C     process table-type oxy-frac
      TBNO= 14
      TBSB= 1
      NVAL= 2
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             OXFRC)
C
C     check if monthly values needed
      DO 120 I= 1, 2
        IF (OXFFG(I) .GE. 1) THEN
C         monthly fraction must be read
          CALL MDATBL (OXFFG(I),
     O                 OXFRM(1,I),RETCOD)
        END IF
 120  CONTINUE
C
C     process table-type nut-flag
      TBNO= 15
      TBSB= 1
      NVAL= 10
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             NUTFFG)
C
C     process table-type dnut-frac
      TBNO= 16
      TBSB= 1
      NVAL= 4
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             DNUFRC)
C
C     check if monthly values needed
      DO 130 I= 1, 4
        IF (NUTFFG(I) .GE. 1) THEN
C         monthly fraction must be read
          CALL MDATBL (NUTFFG(I),
     O                 DNUFRM(1,I),RETCOD)
        END IF
 130  CONTINUE
C
C     process table-type adsnut-frac
      TBNO= 17
      TBSB= 1
      NVAL= 6
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             SNUFRC)
      DO 150 I= 1, 2
        DO 140 J= 1, 3
C
C         check if monthly values needed
          N= (I- 1)*3+ J+ 4
          IF (NUTFFG(N) .GE. 1) THEN
C           monthly fraction must be read
            CALL MDATBL (NUTFFG(N),
     O                   SNUFRM(1,J,I),RETCOD)
          END IF
 140    CONTINUE
 150  CONTINUE
C
C     process table-type plank-flag
      TBNO= 18
      TBSB= 1
      NVAL= 5
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PLKFFG)
C
C     process table-type plank-frac
      TBNO= 19
      TBSB= 1
      NVAL= 5
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PLKFRC)
C
C     check if monthly values needed
      DO 160 I= 1, 5
        IF (PLKFFG(I) .GE. 1) THEN
C         monthly fraction must be read
          CALL MDATBL (PLKFFG(I),
     O                 PLKFRM(1,I),RETCOD)
        END IF
 160  CONTINUE
C
C     process table-type ph-flag
      TBNO= 20
      TBSB= 1
      NVAL= 2
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PHFFG)
C
C     process table-type ph-frac
      TBNO= 21
      TBSB= 1
      NVAL= 2
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PHFRC)
C
C     check if monthly values needed
      DO 170 I= 1, 2
        IF (PHFFG(I) .GE. 1) THEN
C         monthly fraction must be read
          CALL MDATBL (PHFFG(I),
     O                 PHFRM(1,I),RETCOD)
        END IF
 170  CONTINUE
C
C     process table-type acid-flag
      TBNO= 22
      TBSB= 1
      NVAL= 8
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      NACID= IVAL(1)
      DO 180 I= 1, NACID
        ACIFFG(I)= IVAL(I+ 1)
 180  CONTINUE
C
C     process table-type acid-frac
      TBNO= 23
      TBSB= 1
      NVAL= NACID
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             ACIFRC)
C
C     check if monthly values needed
      DO 190 I= 1, NACID
        IF (ACIFFG(I) .GE. 1) THEN
C         monthly fraction must be read
          CALL MDATBL (ACIFFG(I),
     O                 ACIFRM(1,I),RETCOD)
        END IF
 190  CONTINUE
C
C     set flux accumulators to zero
      DO 200 LEV= 2, 5
        CALL BMPRST (LEV)
 200  CONTINUE
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
C       done processing message
        WRITE (MESSU,2010) BMPNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BMPRAC
     I                   (STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     Perform reductions in fluxes due to implementation of BMPs
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - of inpad
C     WIDTH  - inpad width
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR2 + + +
      INCLUDE    'cbmpr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT,I,J,N,ACTFG
      CHARACTER*6 OPTYP
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC   MIN0
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,SPECL,DAYVAL,BBAROT,BPRINT,UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'BMPRAC'/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL=  STIVL- 1
      IVL1= STIVL
      IDELT= DELT
      PIVLNO= 0
C
C     time loop
      DO 200 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
        SPIVL= SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,NDAY,PIVL,PYREND,
     M               DATIM,PIVLNO,
     O               NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,EMONFG,EPYRFG)
        IF (STFG .EQ. 1) THEN
C         first interval of run
          STFG = 0
          HRFG = 1
          DAYFG= 1
C
C         set overall print flag according to sections with input t.s.
C
C         initialize to no output
          BMPPFG= 6
C
          IF (NCATS .LE. 0) THEN
C           check flow without categories
            IF (IVOLFP .GE. 1) THEN
              BMPPFG= MIN0 (BMPPFG, PFLAG(1))
            ELSE
              PFLAG(1)= 6
            END IF
          ELSE
C           check flow with categories
            ACTFG= 0
            DO 2 I= 1, NCATS
              IF (CIVLFP(I) .GE. 1) THEN
                BMPPFG= MIN0 (BMPPFG, PFLAG(1))
                ACTFG= 1
              END IF
 2          CONTINUE
            IF (ACTFG .EQ. 0) THEN
              PFLAG(1)= 6
            END IF
          END IF
C
C         check conservatives
          ACTFG= 0
          DO 3 I= 1, NCONS
            IF (ICNFP(1) .GE. 1) THEN
              BMPPFG= MIN0 (BMPPFG, PFLAG(2))
              ACTFG= 1
            END IF
 3        CONTINUE
          IF (ACTFG .EQ. 0) THEN
            PFLAG(2)= 6
          END IF
C
C         check heat
          IF (IHTFP .GE. 1) THEN
            BMPPFG= MIN0 (BMPPFG, PFLAG(3))
          ELSE
            PFLAG(3)= 6
          END IF
C
C         check sediment fractions
          IF ( (ISEDFP(1) .GE. 1) .OR. (ISEDFP(2) .GE. 1) .OR.
     $         (ISEDFP(3) .GE. 1) ) THEN
            BMPPFG= MIN0 (BMPPFG, PFLAG(4))
          ELSE
            PFLAG(4)= 6
          END IF
C
C         check each qual, dissolved and adsorbed to sediment fractions
          ACTFG= 0
          DO 5 I= 1, NGQUAL
            IF ( (ISQFP(1,I) .GE. 1) .OR. (ISQFP(2,I) .GE. 1) .OR.
     $           (ISQFP(3,I) .GE. 1) .OR. (IDQFP(I) .GE. 1) ) THEN
              BMPPFG= MIN0 (BMPPFG, PFLAG(5))
              ACTFG= 1
            END IF
 5        CONTINUE
          IF (ACTFG .EQ. 0) THEN
            PFLAG(5)= 6
          END IF
C
C         check oxygen components
          IF ( (IOXFP(1) .GE. 1) .OR. (IOXFP(2) .GE. 1) ) THEN
            BMPPFG= MIN0 (BMPPFG, PFLAG(6))
          ELSE
            PFLAG(6)= 6
          END IF
C
C         check nutrients
          IF ( (IDNUTX(1) .GE. 1) .OR. (IDNUTX(2) .GE. 1) .OR.
     $         (IDNUTX(3) .GE. 1) .OR. (IDNUTX(4) .GE. 1) .OR.
     $         (ISNUTX(1,1) .GE. 1) .OR. (ISNUTX(1,2) .GE. 1) .OR.
     $         (ISNUTX(2,1) .GE. 1) .OR. (ISNUTX(2,2) .GE. 1) .OR.
     $         (ISNUTX(3,1) .GE. 1) .OR. (ISNUTX(3,2) .GE. 1) ) THEN
            BMPPFG= MIN0 (BMPPFG, PFLAG(7))
          ELSE
            PFLAG(7)= 6
          END IF
C
C         check plankton components
          IF ( (IPLKFP(1) .GE. 1) .OR. (IPLKFP(2) .GE. 1) .OR.
     $         (IPLKFP(3) .GE. 1) .OR. (IPLKFP(4) .GE. 1) .OR.
     $         (IPLKFP(5) .GE. 1) ) THEN
            BMPPFG= MIN0 (BMPPFG, PFLAG(8))
          ELSE
            PFLAG(8)= 6
          END IF
C
C         check inorganic carbon components
          IF ( (IPHFP(1) .GE. 1) .OR. (IPHFP(2) .GE. 1) ) THEN
            BMPPFG= MIN0 (BMPPFG, PFLAG(9))
          ELSE
            PFLAG(9)= 6
          END IF
C
C         check each acid mine drainage component
          ACTFG= 0
          DO 7 I= 1, NACID
            IF (IACIDX(1) .GE. 1) THEN
              BMPPFG= MIN0 (BMPPFG, PFLAG(10))
              ACTFG= 1
            END IF
 7        CONTINUE
          IF (ACTFG .EQ. 0) THEN
            PFLAG(10)= 6
          END IF
        END IF
C
        IF (SPAFP .GT. 0 .AND. SPAFP .LE. SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,BMPNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     I                SPOUT,SPNUND,
     M                SPAFP)
        END IF
C
C       perform removals
C
        IF ( (DAYFG .EQ. 1) .AND. (VOLFFG .GE. 1) ) THEN
C         it is the first interval of the day and
C         the fraction is allowed to vary throughout the year
C         interpolate for the daily value
          VOLFRC= DAYVAL (VOLFRM(MON),VOLFRM(NXTMON),DAY,NDAYS)
        END IF
        IF (IVOLFP .GE. 1) THEN
C         flow
          IVOL= PAD(IVOLFP+ IVL1)
          RMVOL= IVOL*VOLFRC
          ROVOL= IVOL- RMVOL
        ELSE
          IVOL= 0.0
          RMVOL= 0.0
          ROVOL= 0.0
        END IF
C
        DO 10 I= 1, NCATS
          IF (CIVLFP(I) .GE. 1) THEN
C           this category
            CIVOL(I)= PAD(CIVLFP(I)+ IVL1)
            CRMVOL(I)= CIVOL(I)*VOLFRC
            CROVOL(I)= CIVOL(I)- CRMVOL(I)
          ELSE
            CIVOL(I)= 0.0
            CRMVOL(I)= 0.0
            CROVOL(I)= 0.0
          END IF
          IVOL= IVOL+ CIVOL(I)
          ROVOL= ROVOL+ CROVOL(I)
          RMVOL= RMVOL+ CRMVOL(I)
 10     CONTINUE
C
        DO 20 I= 1, NCONS
          IF (ICNFP(I) .GE. 1) THEN
C           this cons
            ICON(I)= PAD(ICNFP(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (CONFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              CONFRC(I)= DAYVAL (CONFRM(MON,I),CONFRM(NXTMON,I),DAY,
     I                           NDAYS)
            END IF
            RMCON(I)= ICON(I)*CONFRC(I)
            ROCON(I)= ICON(I)- RMCON(I)
          ELSE
            ICON(I)= 0.0
            RMCON(I)= 0.0
            ROCON(I)= 0.0
          END IF
 20     CONTINUE
C
        IF (IHTFP .GE. 1) THEN
C         heat
          IHEAT= PAD(IHTFP+ IVL1)
          IF ( (DAYFG .EQ. 1) .AND. (HTFFG .GE. 1) ) THEN
C           it is the first interval of the day and
C           the fraction is allowed to vary throughout the year
C           interpolate for the daily value
            HTFRC= DAYVAL (HTFRM(MON),HTFRM(NXTMON),DAY,NDAYS)
          END IF
          RMHEAT= IHEAT*HTFRC
          ROHEAT= IHEAT- RMHEAT
        ELSE
          IHEAT= 0.0
          RMHEAT= 0.0
          ROHEAT= 0.0
        END IF
C
        DO 30 I= 1, 3
          IF (ISEDFP(I) .GE. 1) THEN
C           this sediment fraction
            ISED(I)= PAD(ISEDFP(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (SEDFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              SEDFRC(I)= DAYVAL (SEDFRM(MON,I),SEDFRM(NXTMON,I),DAY,
     I                           NDAYS)
            END IF
            RMSED(I)= ISED(I)*SEDFRC(I)
            ROSED(I)= ISED(I)- RMSED(I)
          ELSE
            ISED(I)= 0.0
            RMSED(I)= 0.0
            ROSED(I)= 0.0
          END IF
 30     CONTINUE
C
        DO 50 I= 1, NGQUAL
          IF (IDQFP(I) .GE. 1) THEN
C           this qual
            IDQAL(I)= PAD(IDQFP(I)+ IVL1)
            N= (I- 1)*4+ I
            IF ( (DAYFG .EQ. 1) .AND. (GQFFG(N) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              GQDFRC(I)= DAYVAL (GQDFRM(MON,I),GQDFRM(NXTMON,I),DAY,
     I                           NDAYS)
            END IF
            RMDQAL(I)= IDQAL(I)*GQDFRC(I)
            RODQAL(I)= IDQAL(I)- RMDQAL(I)
          ELSE
            IDQAL(I)= 0.0
            RMDQAL(I)= 0.0
            RODQAL(I)= 0.0
          END IF
          DO 40 J= 1, 3
            IF (ISQFP(J,I) .GE. 1) THEN
C             this qual on this fraction
              ISQAL(J,I)= PAD(ISQFP(J,I)+ IVL1)
              N= (I- 1)*4+ J+ 1
              IF ( (DAYFG .EQ. 1) .AND. (GQFFG(N) .GE. 1) ) THEN
C               it is the first interval of the day and
C               the fraction is allowed to vary throughout the year
C               interpolate for the daily value
                GQSFRC(J,I)= DAYVAL (GQSFRM(MON,J,I),
     I                               GQSFRM(NXTMON,J,I),DAY,NDAYS)
              END IF
              RMSQAL(J,I)= ISQAL(J,I)*GQSFRC(J,I)
              ROSQAL(J,I)= ISQAL(J,I)- RMSQAL(J,I)
            ELSE
              ISQAL(J,I)= 0.0
              RMSQAL(J,I)= 0.0
              ROSQAL(J,I)= 0.0
            END IF
 40       CONTINUE
 50     CONTINUE
C
        DO 60 I= 1, 2
          IF (IOXFP(I) .GE. 1) THEN
C           this oxygen component
            IOX(I)= PAD(IOXFP(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (OXFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              OXFRC(I)= DAYVAL (OXFRM(MON,I),OXFRM(NXTMON,I),DAY,NDAYS)
            END IF
            RMOX(I)= IOX(I)*OXFRC(I)
            ROOX(I)= IOX(I)- RMOX(I)
          ELSE
            IOX(I)= 0.0
            RMOX(I)= 0.0
            ROOX(I)= 0.0
          END IF
 60     CONTINUE
C
        DO 70 I= 1, 4
          IF (IDNUTX(I) .GE. 1) THEN
C           this nutrient
            IDNUT(I)= PAD(IDNUTX(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (NUTFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              DNUFRC(I)= DAYVAL (DNUFRM(MON,I),DNUFRM(NXTMON,I),DAY,
     I                           NDAYS)
            END IF
            RMDNUT(I)= IDNUT(I)*DNUFRC(I)
            RODNUT(I)= IDNUT(I)- RMDNUT(I)
          ELSE
            IDNUT(I)= 0.0
            RMDNUT(I)= 0.0
            RODNUT(I)= 0.0
          END IF
 70     CONTINUE
C
        DO 90 I= 1, 2
          DO 80 J= 1, 3
            IF (ISNUTX(J,I) .GE. 1) THEN
C             this nutrient on this sediment fraction
              ISNUT(J,I)= PAD(ISNUTX(J,I)+ IVL1)
              RMSNUT(J,I)= ISNUT(J,I)*SNUFRC(J,I)
              ROSNUT(J,I)= ISNUT(J,I)- RMSNUT(J,I)
            ELSE
              ISNUT(J,I)= 0.0
              RMSNUT(J,I)= 0.0
              ROSNUT(J,I)= 0.0
            END IF
 80       CONTINUE
 90     CONTINUE
C
        DO 100 I= 1, 5
          IF (IPLKFP(I) .GE. 1) THEN
C           this plankton component
            IPLK(I)= PAD(IPLKFP(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (PLKFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              PLKFRC(I)= DAYVAL (PLKFRM(MON,I),PLKFRM(NXTMON,I),DAY,
     I                           NDAYS)
            END IF
            RMPLK(I)= IPLK(I)*PLKFRC(I)
            ROPLK(I)= IPLK(I)- RMPLK(I)
          ELSE
            IPLK(I)= 0.0
            RMPLK(I)= 0.0
            ROPLK(I)= 0.0
          END IF
 100    CONTINUE
C
        DO 110 I= 1, 2
          IF (IPHFP(I) .GE. 1) THEN
C           this inorganic carbon component
            IPH(I)= PAD(IPHFP(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (PHFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              PHFRC(I)= DAYVAL (PHFRM(MON,I),PHFRM(NXTMON,I),DAY,NDAYS)
            END IF
            RMPH(I)= IPH(I)*PHFRC(I)
            ROPH(I)= IPH(I)- RMPH(I)
          ELSE
            IPH(I)= 0.0
            RMPH(I)= 0.0
            ROPH(I)= 0.0
          END IF
 110    CONTINUE
C
        DO 120 I= 1, NACID
          IF (IACIDX(I) .GE. 1) THEN
C           this acid component
            IACID(I)= PAD(IACIDX(I)+ IVL1)
            IF ( (DAYFG .EQ. 1) .AND. (ACIFFG(I) .GE. 1) ) THEN
C             it is the first interval of the day and
C             the fraction is allowed to vary throughout the year
C             interpolate for the daily value
              ACIFRC(I)= DAYVAL (ACIFRM(MON,I),ACIFRM(NXTMON,I),DAY,
     I                           NDAYS)
            END IF
            RMACID(I)= IACID(I)*ACIFRC(I)
            ROACID(I)= IACID(I)- RMACID(I)
          ELSE
            IACID(I)= 0.0
            RMACID(I)= 0.0
            ROACID(I)= 0.0
          END IF
 120    CONTINUE
C
C       output time series
        CALL BBAROT
C
C       handle flux accumulation, printout
        IF (BMPPFG .LT. 6) THEN
          CALL BPRINT
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   BMPRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux accumulators and state variables used for printout.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR2 + + +
      INCLUDE    'cbmpr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I2,I3,I4,I5,I6,I7,I9,I10
      REAL        R0
C
C     + + + EXTERNALS + + +
      EXTERNAL    SETVEC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R0,I2,I3,I4,I5,I6,I7,I9,I10/0.0,2,3,4,5,6,7,9,10/
C
C     + + + END SPECIFICATIONS + + +
C
C     inflow fluxes
      HYIF(LEV)= R0
      CALL SETVEC (MXCAT,R0,
     O             HYIFC(1,LEV))
      CALL SETVEC (I10,0.0,
     O             CNIF(1,LEV))
      HTIF(LEV)= 0.0
      CALL SETVEC (I3,0.0,
     O             SDIF(1,LEV))
      CALL SETVEC (I3,0.0,
     O             GQDIF(1,LEV))
      CALL SETVEC (I9,0.0,
     O             GQSIF(1,1,LEV))
      CALL SETVEC (I2,0.0,
     O             OXIF(1,LEV))
      CALL SETVEC (I4,0.0,
     O             DNUIF(1,LEV))
      CALL SETVEC (I6,0.0,
     O             SNUIF(1,1,LEV))
      CALL SETVEC (I5,0.0,
     O             PKIF(1,LEV))
      CALL SETVEC (I2,0.0,
     O             PHIF(1,LEV))
      CALL SETVEC (I7,0.0,
     O             ACIF(1,LEV))
C
C     outflow fluxes
      HYOF(LEV)= R0
      CALL SETVEC (MXCAT,R0,
     O             HYOFC(1,LEV))
      CALL SETVEC (I10,0.0,
     O             CNOF(1,LEV))
      HTOF(LEV)= 0.0
      CALL SETVEC (I3,0.0,
     O             SDOF(1,LEV))
      CALL SETVEC (I3,0.0,
     O             GQDOF(1,LEV))
      CALL SETVEC (I9,0.0,
     O             GQSOF(1,1,LEV))
      CALL SETVEC (I2,0.0,
     O             OXOF(1,LEV))
      CALL SETVEC (I4,0.0,
     O             DNUOF(1,LEV))
      CALL SETVEC (I6,0.0,
     O             SNUOF(1,1,LEV))
      CALL SETVEC (I5,0.0,
     O             PKOF(1,LEV))
      CALL SETVEC (I2,0.0,
     O             PHOF(1,LEV))
      CALL SETVEC (I7,0.0,
     O             ACOF(1,LEV))
C
C     removal fluxes
      HYRF(LEV)= R0
      CALL SETVEC (MXCAT,R0,
     O             HYRFC(1,LEV))
      CALL SETVEC (I10,0.0,
     O             CNRF(1,LEV))
      HTRF(LEV)= 0.0
      CALL SETVEC (I3,0.0,
     O             SDRF(1,LEV))
      CALL SETVEC (I3,0.0,
     O             GQDRF(1,LEV))
      CALL SETVEC (I9,0.0,
     O             GQSRF(1,1,LEV))
      CALL SETVEC (I2,0.0,
     O             OXRF(1,LEV))
      CALL SETVEC (I4,0.0,
     O             DNURF(1,LEV))
      CALL SETVEC (I6,0.0,
     O             SNURF(1,1,LEV))
      CALL SETVEC (I5,0.0,
     O             PKRF(1,LEV))
      CALL SETVEC (I2,0.0,
     O             PHRF(1,LEV))
      CALL SETVEC (I7,0.0,
     O             ACRF(1,LEV))
C
      RETURN
      END
C
C
C
      SUBROUTINE   BBAROT
C
C     + + + PURPOSE + + +
C     Place the current values of all bar-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR2 + + +
      INCLUDE  'cbmpr.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
C     outflow fluxes
      IF (ROVFP .GE. 1) THEN
        PAD(ROVFP+ IVL1)= ROVOL
      END IF
      DO 10 I= 1, NCATS
        IF (CROVFP(I) .GE. 1) THEN
          PAD(CROVFP(I)+ IVL1)= CROVOL(I)
        END IF
 10   CONTINUE
      DO 20 I= 1, NCONS
        IF (ROCNFP(I) .GE. 1) THEN
          PAD(ROCNFP(I)+ IVL1)= ROCON(I)
        END IF
 20   CONTINUE
      IF (ROHTFP .GE. 1) THEN
        PAD(ROHTFP+ IVL1)= ROHEAT
      END IF
      DO 30 I= 1, 3
        IF (ROSDFP(I) .GE. 1) THEN
          PAD(ROSDFP(I)+ IVL1)= ROSED(I)
        END IF
 30   CONTINUE
      DO 50 I= 1, NGQUAL
        IF (RODQFP(I) .GE. 1) THEN
          PAD(RODQFP(I)+ IVL1)= RODQAL(I)
        END IF
        DO 40 J= 1, 3
          IF (ROSQFP(J,I) .GE. 1) THEN
            PAD(ROSQFP(J,I)+ IVL1)= ROSQAL(J,I)
          END IF
 40     CONTINUE
 50   CONTINUE
      DO 60 I= 1, 2
        IF (ROOXFP(I) .GE. 1) THEN
          PAD(ROOXFP(I)+ IVL1)= ROOX(I)
        END IF
 60   CONTINUE
      DO 70 I= 1, 4
        IF (RODNUX(I) .GE. 1) THEN
          PAD(RODNUX(I)+ IVL1)= RODNUT(I)
        END IF
 70   CONTINUE
      DO 90 I= 1, 2
        DO 80 J= 1, 3
          IF (ROSNUX(J,I) .GE. 1) THEN
            PAD(ROSNUX(J,I)+ IVL1)= ROSNUT(J,I)
          END IF
 80     CONTINUE
 90   CONTINUE
      DO 100 I= 1, 5
        IF (ROPLKX(I) .GE. 1) THEN
          PAD(ROPLKX(I)+ IVL1)= ROPLK(I)
        END IF
 100  CONTINUE
      DO 110 I= 1, 2
        IF (ROPHFP(I) .GE. 1) THEN
          PAD(ROPHFP(I)+ IVL1)= ROPH(I)
        END IF
 110  CONTINUE
      DO 120 I= 1, NACID
        IF (ROACIX(I) .GE. 1) THEN
          PAD(ROACIX(I)+ IVL1)= ROACID(I)
        END IF
 120  CONTINUE
C
C     removal fluxes
      IF (RMVFP .GE. 1) THEN
        PAD(RMVFP+ IVL1)= RMVOL
      END IF
      DO 130 I= 1, NCATS
        IF (CRMVFP(I) .GE. 1) THEN
          PAD(CRMVFP(I)+ IVL1)= CRMVOL(I)
        END IF
 130  CONTINUE
      DO 140 I= 1, NCONS
        IF (RMCNFP(I) .GE. 1) THEN
          PAD(RMCNFP(I)+ IVL1)= RMCON(I)
        END IF
 140  CONTINUE
      IF (RMHTFP .GE. 1) THEN
        PAD(RMHTFP+ IVL1)= RMHEAT
      END IF
      DO 150 I= 1, 3
        IF (RMSDFP(I) .GE. 1) THEN
          PAD(RMSDFP(I)+ IVL1)= RMSED(I)
        END IF
 150  CONTINUE
      DO 170 I= 1, NGQUAL
        IF (RMDQFP(I) .GE. 1) THEN
          PAD(RMDQFP(I)+ IVL1)= RMDQAL(I)
        END IF
        DO 160 J= 1, 3
          IF (RMSQFP(J,I) .GE. 1) THEN
            PAD(RMSQFP(J,I)+ IVL1)= RMSQAL(J,I)
          END IF
 160    CONTINUE
 170  CONTINUE
      DO 180 I= 1, 2
        IF (RMOXFP(I) .GE. 1) THEN
          PAD(RMOXFP(I)+ IVL1)= RMOX(I)
        END IF
 180  CONTINUE
      DO 190 I= 1, 4
        IF (RMDNUX(I) .GE. 1) THEN
          PAD(RMDNUX(I)+ IVL1)= RMDNUT(I)
        END IF
 190  CONTINUE
      DO 210 I= 1, 2
        DO 200 J= 1, 3
          IF (RMSNUX(J,I) .GE. 1) THEN
            PAD(RMSNUX(J,I)+ IVL1)= RMSNUT(J,I)
          END IF
 200    CONTINUE
 210  CONTINUE
      DO 220 I= 1, 5
        IF (RMPLKX(I) .GE. 1) THEN
          PAD(RMPLKX(I)+ IVL1)= RMPLK(I)
        END IF
 220  CONTINUE
      DO 230 I= 1, 2
        IF (RMPHFP(I) .GE. 1) THEN
          PAD(RMPHFP(I)+ IVL1)= RMPH(I)
        END IF
 230  CONTINUE
      DO 240 I= 1, NACID
        IF (RMACIX(I) .GE. 1) THEN
          PAD(RMACIX(I)+ IVL1)= RMACID(I)
        END IF
 240  CONTINUE
C
C     received fluxes
      IF (RCVOLX .GE. 1) THEN
        PAD(RCVOLX+ IVL1)= IVOL
      END IF
      DO 250 I= 1, NCATS
        IF (RCCVFP(I) .GE. 1) THEN
          PAD(RCCVFP(I)+ IVL1)= CIVOL(I)
        END IF
 250  CONTINUE
      DO 260 I= 1, NCONS
        IF (RCCNFP(I) .GE. 1) THEN
          PAD(RCCNFP(I)+ IVL1)= ICON(I)
        END IF
 260  CONTINUE
      IF (RCHTFP .GE. 1) THEN
        PAD(RCHTFP+ IVL1)= IHEAT
      END IF
      DO 270 I= 1, 3
        IF (RCSDFP(I) .GE. 1) THEN
          PAD(RCSDFP(I)+ IVL1)= ISED(I)
        END IF
 270  CONTINUE
      DO 290 I= 1, NGQUAL
        IF (RCDQFP(I) .GE. 1) THEN
          PAD(RCDQFP(I)+ IVL1)= IDQAL(I)
        END IF
        DO 280 J= 1, 3
          IF (RCSQFP(J,I) .GE. 1) THEN
            PAD(RCSQFP(J,I)+ IVL1)= ISQAL(J,I)
          END IF
 280    CONTINUE
 290  CONTINUE
      DO 300 I= 1, 2
        IF (RCOXFP(I) .GE. 1) THEN
          PAD(RCOXFP(I)+ IVL1)= IOX(I)
        END IF
 300  CONTINUE
      DO 310 I= 1, 4
        IF (RCDNUX(I) .GE. 1) THEN
          PAD(RCDNUX(I)+ IVL1)= IDNUT(I)
        END IF
 310  CONTINUE
      DO 330 I= 1, 2
        DO 320 J= 1, 3
          IF (RCSNUX(J,I) .GE. 1) THEN
            PAD(RCSNUX(J,I)+ IVL1)= ISNUT(J,I)
          END IF
 320    CONTINUE
 330  CONTINUE
      DO 340 I= 1, 5
        IF (RCPKFP(I) .GE. 1) THEN
          PAD(RCPKFP(I)+ IVL1)= IPLK(I)
        END IF
 340  CONTINUE
      DO 350 I= 1, 2
        IF (RCPHFP(I) .GE. 1) THEN
          PAD(RCPHFP(I)+ IVL1)= IPH(I)
        END IF
 350  CONTINUE
      DO 360 I= 1, NACID
        IF (RCACIX(I) .GE. 1) THEN
          PAD(RCACIX(I)+ IVL1)= IACID(I)
        END IF
 360  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   BMPACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for printout.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR2 + + +
      INCLUDE    'cbmpr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I2,I3,I4,I5
C
C     + + + EXTERNALS + + +
      EXTERNAL    ACCVEC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I2,I3,I4,I5/2,3,4,5/
C
C     + + + END SPECIFICATIONS + + +
C
C     inflow fluxes
      HYIF(TOROW)= HYIF(TOROW) + HYIF(FRMROW)
      IF (NCATS .GT. 0) THEN
C       category inflow
        CALL ACCVEC (MXCAT,HYIFC(1,FRMROW),
     M               HYIFC(1,TOROW))
      END IF
      CALL ACCVEC (NCONS,CNIF(1,FRMROW),
     M             CNIF(1,TOROW))
      HTIF(TOROW)= HTIF(TOROW) + HTIF(FRMROW)
      CALL ACCVEC (I3,SDIF(1,FRMROW),
     M             SDIF(1,TOROW))
      IF (NGQUAL .GE. 1) THEN
        CALL ACCVEC (NGQUAL,GQDIF(1,FRMROW),
     M               GQDIF(1,TOROW))
        DO 10 I= 1, NGQUAL
          CALL ACCVEC (I3,GQSIF(1,I,FRMROW),
     O                 GQSIF(1,I,TOROW))
 10     CONTINUE
      END IF
      CALL ACCVEC (I2,OXIF(1,FRMROW),
     M             OXIF(1,TOROW))
      CALL ACCVEC (I4,DNUIF(1,FRMROW),
     O             DNUIF(1,TOROW))
      DO 20 I= 1, 2
        CALL ACCVEC (I3,SNUIF(1,I,FRMROW),
     O               SNUIF(1,I,TOROW))
 20   CONTINUE
      CALL ACCVEC (I5,PKIF(1,FRMROW),
     O             PKIF(1,TOROW))
      CALL ACCVEC (I2,PHIF(1,FRMROW),
     O             PHIF(1,TOROW))
      IF (NACID .GE. 1) THEN
        CALL ACCVEC (NACID,ACIF(1,FRMROW),
     O               ACIF(1,TOROW))
      END IF
C
C     outflow fluxes
      HYOF(TOROW)= HYOF(TOROW) + HYOF(FRMROW)
      IF (NCATS .GT. 0) THEN
C       category inflow
        CALL ACCVEC (MXCAT,HYOFC(1,FRMROW),
     M               HYOFC(1,TOROW))
      END IF
      CALL ACCVEC (NCONS,CNOF(1,FRMROW),
     M             CNOF(1,TOROW))
      HTOF(TOROW)= HTOF(TOROW) + HTOF(FRMROW)
      CALL ACCVEC (I3,SDOF(1,FRMROW),
     M             SDOF(1,TOROW))
      IF (NGQUAL .GE. 1) THEN
        CALL ACCVEC (NGQUAL,GQDOF(1,FRMROW),
     M               GQDOF(1,TOROW))
        DO 30 I= 1, NGQUAL
          CALL ACCVEC (I3,GQSOF(1,I,FRMROW),
     O                 GQSOF(1,I,TOROW))
 30     CONTINUE
      END IF
      CALL ACCVEC (I2,OXOF(1,FRMROW),
     M             OXOF(1,TOROW))
      CALL ACCVEC (I4,DNUOF(1,FRMROW),
     O             DNUOF(1,TOROW))
      DO 40 I= 1, 2
        CALL ACCVEC (I3,SNUOF(1,I,FRMROW),
     O               SNUOF(1,I,TOROW))
 40   CONTINUE
      CALL ACCVEC (I5,PKOF(1,FRMROW),
     O             PKOF(1,TOROW))
      CALL ACCVEC (I2,PHOF(1,FRMROW),
     O             PHOF(1,TOROW))
      IF (NACID .GE. 1) THEN
        CALL ACCVEC (NACID,ACOF(1,FRMROW),
     O               ACOF(1,TOROW))
      END IF
C
C     removal fluxes
      HYRF(TOROW)= HYRF(TOROW) + HYRF(FRMROW)
      IF (NCATS .GT. 0) THEN
C       category inflow
        CALL ACCVEC (MXCAT,HYRFC(1,FRMROW),
     M               HYRFC(1,TOROW))
      END IF
      CALL ACCVEC (NCONS,CNRF(1,FRMROW),
     M             CNRF(1,TOROW))
      HTRF(TOROW)= HTRF(TOROW) + HTRF(FRMROW)
      CALL ACCVEC (I3,SDRF(1,FRMROW),
     M             SDRF(1,TOROW))
      IF (NGQUAL .GE. 1) THEN
        CALL ACCVEC (NGQUAL,GQDRF(1,FRMROW),
     M               GQDRF(1,TOROW))
        DO 50 I= 1, NGQUAL
          CALL ACCVEC (I3,GQSRF(1,I,FRMROW),
     O                 GQSRF(1,I,TOROW))
 50     CONTINUE
      END IF
      CALL ACCVEC (I2,OXRF(1,FRMROW),
     M             OXRF(1,TOROW))
      CALL ACCVEC (I4,DNURF(1,FRMROW),
     O             DNURF(1,TOROW))
      DO 60 I= 1, 2
        CALL ACCVEC (I3,SNURF(1,I,FRMROW),
     O               SNURF(1,I,TOROW))
 60   CONTINUE
      CALL ACCVEC (I5,PKRF(1,FRMROW),
     O             PKRF(1,TOROW))
      CALL ACCVEC (I2,PHRF(1,FRMROW),
     O             PHRF(1,TOROW))
      IF (NACID .GE. 1) THEN
        CALL ACCVEC (NACID,ACRF(1,FRMROW),
     O               ACRF(1,TOROW))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BMPPRT
     I                    (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Perform printout for module BMPRAC.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV,PRINTU,UNITFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR2 + + +
      INCLUDE   'cbmpr.inc'
      INCLUDE   'cmpad.inc'
      INCLUDE   'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J
      REAL        VFACT(2,2),HFACT(2,2),SFACT(2,2),MFACT(2,2),
     $            PIFLUX,POFLUX,PRFLUX
      CHARACTER*4 SDSIZE(3)
      CHARACTER*5 NUTNAM(4)
      CHARACTER*8 VUNIT(2),HUNIT(2),SUNIT(2),MUNIT(2)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA VFACT/ 1. , 8.1071E+2 , 1.2335E-3 , 1. /
      DATA HFACT/ 1. , 3.969     , 0.2520    , 1. /
      DATA SFACT/ 1. , 1.1025    , 0.9070    , 1. /
      DATA MFACT/ 1. , 2.205     , 0.4536    , 1. /
      DATA SDSIZE/'SAND','SILT','CLAY'/
      DATA NUTNAM/'NO3-N','TAM-N','NO2-N','PO4-P'/
      DATA VUNIT/'   AC-FT','    MM^3'/
      DATA HUNIT/'     BTU','    KCAL'/
      DATA SUNIT/'    TONS','  TONNES'/
      DATA MUNIT/'     LBS','      KG'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,/,' CONSTITUENT',34X,'INFLOW',5X,'OUTFLOW',5X,'REMOVAL')
 2005 FORMAT (/,2X,'FLOW (',A8,')',23X,3(1PG12.3))
 2010 FORMAT (/,2X,'FLOW (',A8,')')
 2020 FORMAT (4X,'TOTAL',31X,3(1PG12.3))
 2030 FORMAT (6X,A16,18X,3(1PG12.3))
 2040 FORMAT (/,2X,'CONSERVATIVES')
 2050 FORMAT (4X,5A4,' (',2A4,')',5X,3(1PG12.3))
 2060 FORMAT (/,2X,'HEAT (',A8,')',23X,3(1PG12.3))
 2070 FORMAT (/,2X,'SEDIMENT (',A8,')')
 2080 FORMAT (4X,A4,32X,3(1PG12.3))
 2090 FORMAT (/,2X,'GENERAL QUALITY')
 2100 FORMAT (4X,5A4,' (',2A4,')')
 2110 FORMAT (6X,'DISSOLVED',25X,3(1PG12.3))
 2120 FORMAT (6X,'ADSORBED TO ',A4,18X,3(1PG12.3))
 2130 FORMAT (/,2X,'DISSOLVED OXYGEN (',A8,')',11X,3(1PG12.3))
 2140 FORMAT (/,2X,'BIOCHEMICAL OXYGEN DEMAND (',A8,')',2X,3(1PG12.3))
 2150 FORMAT (/,2X,'NUTRIENTS (',A8,')')
 2160 FORMAT (4X,'DISSOLVED ',A5,21X,3(1PG12.3))
 2170 FORMAT (4X,A5,' ADSORBED TO ',A4,14X,3(1PG12.3))
 2180 FORMAT (/,2X,'PHYTOPLANKTON (',A8,')',14X,3(1PG12.3))
 2190 FORMAT (/,2X,'ZOOPLANKTON (',A8,')',16X,3(1PG12.3))
 2200 FORMAT (/,2X,'ORGANIC N (',A8,')',18X,3(1PG12.3))
 2210 FORMAT (/,2X,'ORGANIC P (',A8,')',18X,3(1PG12.3))
 2220 FORMAT (/,2X,'ORGANIC C (',A8,')',18X,3(1PG12.3))
 2230 FORMAT (/,2X,'TOTAL INORGANIC C (',A8,')',10X,3(1PG12.3))
 2240 FORMAT (/,2X,'DISSOLVED CO2 (',A8,')',14X,3(1PG12.3))
 2250 FORMAT (/,2X,'ACID MINE DRAINAGE CHEMICALS (',A8,')')
 2260 FORMAT (4X,'CHEMICAL NO.',I3,21X,3(1PG12.3))
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (PRINTU,2000)
C
      IF (PFLAG(1) .LE. LEV) THEN
C       printout flow results
C
C       total flow
        PIFLUX= HYIF(LEV)*VFACT(UUNITS,UNITFG)
        POFLUX= HYOF(LEV)*VFACT(UUNITS,UNITFG)
        PRFLUX= HYRF(LEV)*VFACT(UUNITS,UNITFG)
        IF (NCATS .LE. 0) THEN
          WRITE (PRINTU,2005) VUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        ELSE
          WRITE (PRINTU,2010) VUNIT(UNITFG)
          WRITE (PRINTU,2020) PIFLUX,POFLUX,PRFLUX
        END IF
C
C       by category
        DO 10 I= 1, NCATS
          PIFLUX= HYIFC(I,LEV)*VFACT(UUNITS,UNITFG)
          POFLUX= HYOFC(I,LEV)*VFACT(UUNITS,UNITFG)
          PRFLUX= HYRFC(I,LEV)*VFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2030) CATNAM(I),PIFLUX,POFLUX,PRFLUX
 10     CONTINUE
      END IF
C
      IF (PFLAG(2) .LE. LEV) THEN
C       printout conservative results
        WRITE (PRINTU,2040)
        DO 20 I= 1, NCONS
          IF (ICNFP(I) .GE. 1) THEN
            PIFLUX= CNIF(I,LEV)
            POFLUX= CNOF(I,LEV)
            PRFLUX= CNRF(I,LEV)
            WRITE (PRINTU,2050) (CONID(J,I),J=1,5),(CQTYID(J,I),J=1,2),
     $                           PIFLUX,POFLUX,PRFLUX
          END IF
 20     CONTINUE
      END IF
C
      IF (PFLAG(3) .LE. LEV) THEN
C       printout heat results
        PIFLUX= HTIF(LEV)*HFACT(UUNITS,UNITFG)
        POFLUX= HTOF(LEV)*HFACT(UUNITS,UNITFG)
        PRFLUX= HTRF(LEV)*HFACT(UUNITS,UNITFG)
        WRITE (PRINTU,2060) HUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
      END IF
C
      IF (PFLAG(4) .LE. LEV) THEN
C       printout sediment results
        WRITE (PRINTU,2070) SUNIT(UNITFG)
        DO 30 I= 1, 3
          PIFLUX= SDIF(I,LEV)*SFACT(UUNITS,UNITFG)
          POFLUX= SDOF(I,LEV)*SFACT(UUNITS,UNITFG)
          PRFLUX= SDRF(I,LEV)*SFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2080) SDSIZE(I),PIFLUX,POFLUX,PRFLUX
 30     CONTINUE
      END IF
C
      IF (PFLAG(5) .LE. LEV) THEN
C       printout general qual results
        WRITE (PRINTU,2090)
C
C       dissolved
        DO 50 I= 1, NGQUAL
          IF ( (ISQFP(1,I) .GE. 1) .OR. (ISQFP(2,I) .GE. 1) .OR.
     $         (ISQFP(3,I) .GE. 1) .OR. (IDQFP(I) .GE. 1) ) THEN
            WRITE (PRINTU,2100) (GQID(J,I),J=1,5),(GQTYID(J,I),J=1,2)
            PIFLUX= GQDIF(I,LEV)
            POFLUX= GQDOF(I,LEV)
            PRFLUX= GQDRF(I,LEV)
            WRITE (PRINTU,2110) PIFLUX,POFLUX,PRFLUX
            DO 40 J= 1, 3
              PIFLUX= GQSIF(J,I,LEV)
              POFLUX= GQSOF(J,I,LEV)
              PRFLUX= GQSRF(J,I,LEV)
              WRITE (PRINTU,2120) SDSIZE(J),PIFLUX,POFLUX,PRFLUX
 40         CONTINUE
          END IF
 50     CONTINUE
      END IF
C
      IF (PFLAG(6) .LE. LEV) THEN
C       printout oxygen results
C
        IF (IOXFP(1) .GE. 1) THEN
C         dissolved oxygen
          PIFLUX= OXIF(1,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= OXOF(1,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= OXRF(1,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2130) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
C
        IF (IOXFP(2) .GE. 1) THEN
C         bod
          PIFLUX= OXIF(2,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= OXOF(2,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= OXRF(2,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2140) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
      END IF
C
      IF (PFLAG(7) .LE. LEV) THEN
C       printout nutrient results
        WRITE (PRINTU,2150) MUNIT(UNITFG)
C
C       dissolved
        DO 60 I= 1, 4
          IF (IDNUTX(I) .GE. 1) THEN
            PIFLUX= DNUIF(I,LEV)*MFACT(UUNITS,UNITFG)
            POFLUX= DNUOF(I,LEV)*MFACT(UUNITS,UNITFG)
            PRFLUX= DNURF(I,LEV)*MFACT(UUNITS,UNITFG)
            WRITE (PRINTU,2160) NUTNAM(I),PIFLUX,POFLUX,PRFLUX
          END IF
 60     CONTINUE
        DO 80 I= 1, 2
          IF ( (ISNUTX(1,I) .GE. 1) .OR. (ISNUTX(2,I) .GE. 1) .OR.
     $         (ISNUTX(3,I) .GE. 1) ) THEN
            DO 70 J= 1, 3
              PIFLUX= SNUIF(J,I,LEV)*MFACT(UUNITS,UNITFG)
              POFLUX= SNUOF(J,I,LEV)*MFACT(UUNITS,UNITFG)
              PRFLUX= SNURF(J,I,LEV)*MFACT(UUNITS,UNITFG)
              WRITE (PRINTU,2170) NUTNAM(2*I),SDSIZE(J),PIFLUX,POFLUX,
     $                            PRFLUX
 70         CONTINUE
          END IF
 80     CONTINUE
      END IF
C
      IF (PFLAG(8) .LE. LEV) THEN
C       printout plankton results
C
        IF (IPLKFP(1) .GE. 1) THEN
C         phytoplankton
          PIFLUX= PKIF(1,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PKOF(1,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PKRF(1,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2180) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
C
        IF (IPLKFP(2) .GE. 1) THEN
C         zooplankton
          PIFLUX= PKIF(2,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PKOF(2,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PKRF(2,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2190) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
C
        IF (IPLKFP(3) .GE. 1) THEN
C         organic n
          PIFLUX= PKIF(3,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PKOF(3,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PKRF(3,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2200) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
C
        IF (IPLKFP(4) .GE. 1) THEN
C         organic p
          PIFLUX= PKIF(4,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PKOF(4,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PKRF(4,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2210) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
C
        IF (IPLKFP(5) .GE. 1) THEN
C         organic c
          PIFLUX= PKIF(5,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PKOF(5,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PKRF(5,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2220) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
      END IF
C
      IF (PFLAG(9) .LE. LEV) THEN
C       printout inorganic carbon results
C
        IF (IPHFP(1) .GE. 1) THEN
C         total inorganic carbon
          PIFLUX= PHIF(1,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PHOF(1,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PHRF(1,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2230) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
C
        IF (IPHFP(2) .GE. 1) THEN
C         dissolved co2
          PIFLUX= PHIF(2,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= PHOF(2,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= PHRF(2,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2240) MUNIT(UNITFG),PIFLUX,POFLUX,PRFLUX
        END IF
      END IF
C
      IF (PFLAG(10) .LE. LEV) THEN
C       printout acid mine results
        WRITE (PRINTU,2250) MUNIT(UNITFG)
        DO 90 I= 1, NACID
          PIFLUX= ACIF(I,LEV)*MFACT(UUNITS,UNITFG)
          POFLUX= ACOF(I,LEV)*MFACT(UUNITS,UNITFG)
          PRFLUX= ACRF(I,LEV)*MFACT(UUNITS,UNITFG)
          WRITE (PRINTU,2260) I,PIFLUX,POFLUX,PRFLUX
 90     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BPRINT
C
C     + + + PURPOSE + + +
C     Accumulate fluxes and produce printed output for BMPRAC module.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION BMPR2 + + +
      INCLUDE    'cbmpr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    EXDAT(5),I1,I2,I3,I4,I5,J,PRINTU,UNITFG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I2,I3,I4,I5/1,2,3,4,5/
C
C     + + + EXTERNALS + + +
      EXTERNAL   BMPACC,EXDATE,BMPPRT,BMPRST
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('1',//,' BMP SCENARIO NO.',I4,15X,20X,5A4,6X,
     $        'REPORT FOR',I4,' INTERVALS ENDING ',I4,'/',I2,'/',
     $        I2,' ',I2,':',I2)
 2010 FORMAT ('1',//,' BMP SCENARIO NO.',I4,15X,20X,5A4,30X,
     $        'REPORT FOR DAY ',I4,'/',I2,'/',I2)
 2020 FORMAT ('1',//,' BMP SCENARIO NO.',I4,15X,20X,5A4,31X,
     $        'REPORT FOR MONTH ',I4,'/',I2)
 2030 FORMAT ('1',//,' BMP SCENARIO NO.',I4,15X,20X,5A4,16X,
     $        'REPORT FOR PRINTOUT YEAR ENDING ',I4,'/',I2)
C
C     + + + END SPECIFICATIONS + + +
C
C     flux accumulation
      IF (BMPPFG .EQ. 2) THEN
C       printout at the pivl level is being produced, so
C       accumulate fluxes to this level
        CALL BMPACC
     I             (I1,I2)
      END IF
C
C     always accumulate to the daily level
      CALL BMPACC
     I           (I1,I3)
C
      IF (EDAYFG .EQ. 1) THEN
C       it's the last interval of the day - accumulate daily
C       fluxes to the month level
        CALL BMPACC (I3,I4)
C
        IF (EMONFG .EQ. 1) THEN
C         it's the last interval of the month - accumulate
C         monthly fluxes to the year level
          CALL BMPACC (I4,I5)
        END IF
      END IF
C
C     printout
      DO 110 UNITFG= 1,2
C
        PRINTU= PUNIT(UNITFG)
        IF (PRINTU .NE. 0) THEN
C         printout is required in this set of external units.
C         unitfg= 1 for english, 2 for metric.  printu is the
C         fortran logical unit no. to be used for printout
          IF ( (PIVLNO .EQ. PIVL) .AND. (BMPPFG .EQ. 2) ) THEN
C           it's time to handle any pivl level printout, and some
C           is required
C           convert hour and minute fields in date/time
C           to external format
            CALL EXDATE (DATIM,
     O                   EXDAT)
            WRITE (PRINTU,2000)  BMPNO, BMPID, PIVL, EXDAT
            CALL BMPPRT (UNITFG,I2,PRINTU)
          END IF
C
          IF (EDAYFG .EQ. 1) THEN
            IF (BMPPFG .LE. 3) THEN
C             it's time to handle daily printout
              WRITE (PRINTU,2010)  BMPNO, BMPID, (DATIM(J),J=1,3)
              CALL BMPPRT (UNITFG,I3,PRINTU)
            END IF
C
            IF (EMONFG .EQ. 1) THEN
              IF (BMPPFG .LE. 4) THEN
C               it's time to handle monthly printout
                WRITE (PRINTU,2020)  BMPNO, BMPID, (DATIM(J),J=1,2)
                CALL BMPPRT (UNITFG,I4,PRINTU)
              END IF
C
              IF (EPYRFG .EQ. 1) THEN
C               it's time to handle yearly printout
                WRITE (PRINTU,2030)  BMPNO, BMPID, (DATIM(J),J=1,2)
                CALL BMPPRT (UNITFG,I5,PRINTU)
              END IF
            END IF
          END IF
        END IF
 110  CONTINUE
C
C     reset flux accumulators and state variables used in material
C     balance checks
      IF ( (PIVLNO .EQ. PIVL) .AND. (BMPPFG .EQ. 2) ) THEN
C       reset any pivl level variables in use
        CALL BMPRST (I2)
      END IF
C
      IF (EDAYFG .EQ. 1) THEN
C       reset any daily variables in use
        CALL BMPRST (I3)
C
        IF (EMONFG .EQ. 1) THEN
C         reset any monthly variables in use
          CALL BMPRST (I4)
C
          IF (EPYRFG .EQ. 1) THEN
C           reset any yearly variables in use
            CALL BMPRST (I5)
          END IF
        END IF
      END IF
C
      RETURN
      END
