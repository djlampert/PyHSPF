C
C
C
      SUBROUTINE   PPWATR
     I                    (SDATIM,NDAMON,
     M                     OSVREC)
C
C     + + + PURPOSE + + +
C     Process input for pwater section of module perlnd
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SDATIM(5),NDAMON(12),OSVREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - start date of run
C     NDAMON - number of days in each month
C     OSVREC - number of OSV records needed for PWATER
C              3 if IFFCFG= 1
C              3 if IFFCFG= 2
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER1 + + +
      INCLUDE    'cplpw.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TBSB,TBNO,NVAL,SCLU,SGRP,I,J,RDFLAG,STABNO,IRRPRI(3),
     $           ANNFLG,N
      REAL       AGWRC,RVAL(140),UELVM,AGWSMX,IRRFRC(3),IRRTOT(3),R,
     $           PELEV(6),PFAC
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  INT,MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,RTABLE,DAYVAL,SUTABL,PIRSTF,ZIPR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PWATER')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PWATER')
 2020 FORMAT (/,' CRITICAL GW ELEVATIONS AND STORAGES')
 2030 FORMAT (/,'      SELV      UELV      LELV      BELV    GWDATM',
     $          '      GWEL')
 2040 FORMAT (6(8X,'FT'))
 2050 FORMAT (6(9X,'M'))
 2060 FORMAT (6(F10.3))
 2070 FORMAT (/,'     ULGWS     LLGWS      BGWS')
 2080 FORMAT (3(8X,'IN'))
 2090 FORMAT (3(8X,'MM'))
C
C     + + + HISTORY + + +
C     2002  THJ added optional LZET method for IHM; selected with VLEFG = 2,3
C     5/2004 brb added optional infiltration redistribution method to LZONE
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 303
C
      IF (OUTLEV .GT. 1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     warning message counter initialization
      PWWCNT(1)= 0
C
C     error message counter initialization
      PWECNT(1)= 0
      PWECNT(2)= 0
C
C     table of coordinates for function used to
C     evaluate upper zone behavior
      UZRA(1)= 0.0
      UZRA(2)= 1.25
      UZRA(3)= 1.50
      UZRA(4)= 1.75
      UZRA(5)= 2.00
      UZRA(6)= 2.10
      UZRA(7)= 2.20
      UZRA(8)= 2.25
      UZRA(9)= 2.5
      UZRA(10)= 4.0
C
      INTGRL(1)= 0.0
      INTGRL(2)= 1.29
      INTGRL(3)= 1.58
      INTGRL(4)= 1.92
      INTGRL(5)= 2.36
      INTGRL(6)= 2.81
      INTGRL(7)= 3.41
      INTGRL(8)= 3.8
      INTGRL(9)= 7.1
      INTGRL(10)= 3478.
C
C     initialize flag state variables
      SMSFG= 0
      FSMSFG= 0
C
C     initialize some other variables
      RLZRAT= -1.0E30
      LZFRAC= -1.0E30
      BGWS= 0.0
C
C     initialize fluxes that may not be used
      SURET= 0.0
C
C     process values in table - type pwat-parm1
      TBNO= 13
      TBSB= 1
      NVAL= 13
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PWPM1)
C
C     check to see if we need to read pwat-parm4
      RDFLAG= 0
      DO 10 I= 4, 9
        IF (PWPM1(I) .EQ. 0) THEN
C         parm is constant - need table
          RDFLAG= 1
        END IF
 10   CONTINUE
C
C     set OSVREC based on IFFCFG
      IF (IFFCFG .EQ. 1) THEN
C       pstemp not needed
        OSVREC= MAX(OSVREC,3)
      ELSE
C       need pstemp
        OSVREC= MAX(OSVREC,3)
      END IF
C
C     check IFRDFG to ensure it isn't 1 or 2
      IF (IFRDFG .EQ. 1 .OR. IFRDFG .EQ. 2) THEN 
C       error
        CALL OMSTI (IFRDFG)
        SGRP= 18
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     process values in table - type pwat-parm2
      TBNO= 14
      TBSB= 1
      NVAL= 7
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PWPM2)
C
C     convert to internal units
      INFILT= INFILT*DELT60
C
C     groundwater recession parameters
      AGWRC= PWPM2(7)
      KGW= 1.0- AGWRC**(DELT60/24.0)
C
C     process values in table - type pwat-parm3
      TBNO= 15
      TBSB= 1
      NVAL= 7
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PWPM3)
C
      IF (RDFLAG .EQ. 1) THEN
C       process values in table - type pwat-parm4
        TBNO= 16
        TBSB= 1
        NVAL= 6
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               PWST5)
      ELSE
C       dummy values until dayval is called on first interval
        DO 20 I= 1, 6
          PWST5(I)= 0.0
 20     CONTINUE
      END IF
C
C     process values in table - type pwat-parm5
C     fzg and fzgl - frozen ground parameters for
C     corps of engineers - chicago district 10/93
      TBNO= 17
      TBSB= 1
      NVAL= 2
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PWPM5)
C
      IF (HWTFG .EQ. 1) THEN
C       process values in table - type pwat-parm6
C       high water table parameters for sfwmd - 7/95
        TBNO= 18
        TBSB= 1
        NVAL= 6
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               RVAL)
        IF (SNOWFG .EQ. 0) THEN
C         snow is inactive - use this value
          MELEV= RVAL(1)
        ELSE
C         already using value from snow-parm1
        END IF
        BELV=   RVAL(2)
        GWDATM= RVAL(3)
        PCW=    RVAL(4)
        PGW=    RVAL(5)
        UPGW=   RVAL(6)
C
        SELV= MELEV*12- GWDATM
        BELV= BELV- GWDATM
C
C       process values in table - type pwat-parm7
        TBNO= 19
        TBSB= 1
        NVAL= 7
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               RVAL)
C
        STABNO= RVAL(1)
        SRRC=   RVAL(2)
        SREXP=  RVAL(3)
        IFWSC=  RVAL(4)
        DELTA=  RVAL(5)
        UELFAC= RVAL(6)
        LELFAC= RVAL(7)
C
C       convert src to internal units (/ivl)
        SRRC= SRRC**DELT60
C
        IF (RTOPFG .EQ. 3) THEN
C         process ftable
          CALL SUTABL (STABNO,DELT60,
     O                 NSROWS,NSCOLS,SURTAB)
        END IF
      ELSE
C       no high water table
        GWEL= -1.0E30
      END IF
C
      IF (VCSFG .EQ. 1) THEN
C       get monthly interception storage capacity - table-type mon-intercep
        TBNO= 21
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               CEPSCM)
      END IF
C
      IF (VUZFG .EQ. 1) THEN
C       get monthly values of uzsn - table-type mon-uzsn
        TBNO= 22
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               UZSNM)
      END IF
C
      IF (VNNFG .EQ. 1) THEN
C       get monthly values of manning's n - table-type mon-manning
        TBNO= 23
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               NSURM)
      END IF
C
      IF (VIFWFG .EQ. 1) THEN
C       get monthly interflow inflow params - table-type mon-interflw
        TBNO= 24
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               INTFWM)
      END IF
C
      IF (VIRCFG .EQ. 1) THEN
C       get monthly interflow recession constant - table-type mon-irc
        TBNO= 25
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               IRCM)
      END IF
C
C     VLEFG= 2,3 = optional lzet method for IHM
      IF ( (VLEFG .EQ. 1) .OR. (VLEFG .EQ. 3) ) THEN
C       get monthly lower zone e-t parm - table-type mon-lzetparm
        TBNO= 26
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               LZETPM)
      END IF
C
      IF (IRRGFG .GE. 1) THEN
C       need irrigation parameters
C
        IF ( (IRRGFG .EQ. 2) .OR. (IRRGFG .EQ. 3) ) THEN
C         process values in table - type irrig-parm1
          TBNO= 33
          TBSB= 1
          NVAL= 5
          CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 IRPM1)
        END IF
C
C       process values in table - type irrig-parm2
        TBNO= 34
        TBSB= 1
        NVAL= 8
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               IRPM2)
C
C       process values in table - type irrig-source
        TBNO= 35
        TBSB= 1
        NVAL= 7
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               RVAL)
C
C       initialize source array
        I= 9
        R= 0.0
        CALL ZIPR (I,R,
     O             IRRSRC)
C
C       retrieve user-input source priority fraction
        DO 30 I= 1, 3
          N= 2*(I-1)+ 1
          IRRPRI(I)= RVAL(N)
          IRRFRC(I)= RVAL(N+1)
C         initialize total fraction for each priority level
          IRRTOT(I)= 0.0
 30     CONTINUE
        IRCHNO= INT (RVAL(7)+ 0.01)
C
        IF (IRRPRI(3) .GE. 1) THEN
C         irrigation withdrawals are being made from a rchres
C         turn on global irrigation flag
          I= 1
          CALL PIRSTF (I)
        END IF
C
C       fill source array
        DO 40 I= 1, 3
          IF (IRRPRI(I) .GE. 1) THEN
C           this source is being used
            IRRSRC(I,IRRPRI(I))= IRRFRC(I)
C           also track total fraction for each priority level
            IRRTOT(IRRPRI(I))= IRRTOT(IRRPRI(I))+ IRRFRC(I)
          END IF
 40     CONTINUE
C
C       normalize fractions for each priority level
        DO 60 I= 1, 3
          IF ( (IRRTOT(I) .GT. 0.0) .AND. (IRRTOT(I) .NE. 1.0) ) THEN
C           normalize this level
            DO 50 J= 1, 3
              IRRSRC(I,J)= IRRSRC(I,J)/IRRTOT(I)
 50         CONTINUE
          END IF
 60     CONTINUE
C
C       process values in table - type irrig-target
        TBNO= 36
        TBSB= 1
        NVAL= 5
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               IRRTGT)
C
C       check sum of fractions
        R= 0.0
        DO 65 I= 1, 5
          R= R+ IRRTGT(I)
 65     CONTINUE
        IF (ABS (R- 1.0) .GT. 1.0E-5) THEN
C           error - fractions do not sum to one
            SGRP= 17
            CALL OMSTR (IRRTGT(1))
            CALL OMSTR (IRRTGT(2))
            CALL OMSTR (IRRTGT(3))
            CALL OMSTR (IRRTGT(4))
            CALL OMSTR (IRRTGT(5))
            CALL OMSTR (R)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
        END IF
C
        IF ( (IRRGFG .EQ. 3) .AND. (NSKED .GE. 1) ) THEN
C         process values in table - type irrig-sched
          TBNO= 37
          TBSB= 1
          NVAL= NSKED*7
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL)
C
C         place values into proper arrays
          DO 70 I= 1, NSKED
            N= (I-1)*7+ 1
C
C           date of beginning of application
            IRDATE(1,I)= INT (RVAL(N)+ .01)
            IRDATE(2,I)= INT (RVAL(N+ 1)+ .01)
            IRDATE(3,I)= INT (RVAL(N+ 2)+ .01)
            IRDATE(4,I)= INT (RVAL(N+ 3)+ .01)
            IRDATE(5,I)= INT (RVAL(N+ 4)+ .01)
C
C           check date
            IF (IRDATE(1,I) .EQ. 0) THEN
C             annual application - dummy year
              IRDATE(1,I)= SDATIM(1)
              ANNFLG= 1
            END IF
            CALL STDATE (NDAMON,MESSU,MSGFL,
     M                   ECOUNT,IRDATE(1,I))
            IF (ANNFLG .EQ. 1) THEN
C             reset year for annual application
              IRDATE(1,I)= 0
            END IF
C
C           duration in minutes of application
            IRDURA(I)= INT (RVAL(N+ 5)+ .0001)
C
C           convert hourly rate to internal units
            IF (IRDURA(I) .GE. DELT) THEN
C             application duration is at least a complete interval
              IRRATE(I)= RVAL(N+ 6)*DELT60
            ELSE
C             application is only part of interval - use full amount
              IRRATE(I)= RVAL(N+ 6)*IRDURA(I)/60.0
            END IF
 70       CONTINUE
        END IF
C
        IF (IRRGFG .EQ. 2) THEN
C         using crop demand method
C
C         get soil layer depths and masses - table-type soil-data
          CALL SOLDAT (UUNITS,
     O                 SOILM,SOILD)
C
          IF (SZONFG .EQ. 1) THEN
C           field capacity and wilting point differ by layers
C
C           get wilting point for each layer - table-type soil-data2
            CALL SOLDA2 (UUNITS,
     O                   WILTPT)
C           convert from in/in or in/cm to inches
            DO 75 I= 1, 4
              WILTPT(I)= WILTPT(I)* SOILD(I)
 75         CONTINUE
C
C           get field capacity for each layer - table-type soil-data3
            CALL SOLDA3 (UUNITS,
     O                   FDCAP)
C
            DO 80 I= 1, 4
              AWC(I)= FDCAP(I)- WILTPT(I)/SOILD(I)
80          CONTINUE
          ELSE
C           field capacity and wilting point same for all layers - already
C           input in table-type irrig-parm2
            DO 90 I= 1, 4
              AWC(I)= FLDCAP- WILTP
              WILTPT(I)= WILTP*SOILD(I)
              FDCAP(I)= FLDCAP
 90         CONTINUE
          END IF
C
C         get crop season dates - table-type crop-dates
          CALL CROPDT (MESSU,MSGFL,UUNITS,
     M                 ECOUNT,
     O                 NCRP,CRPDAT)
C
          IF ( (VCRDFG .EQ. 2) .OR. (VAWDFG .EQ. 2) ) THEN
C           using stages of crop seasons for crdep and irawd
C
            DO 130 I= 1, NCRP
C             get stage fractions for each season
              TBNO= 31
              TBSB= I
              NVAL= 4
              CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                     RVAL)
              DO 100 J= 1, 4
                CRPSTG(J,I)= RVAL(J)
 100          CONTINUE
C
C             get seasonal parameters for each crop
              TBNO= 32
              TBSB= I
              NVAL= 6
              CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                     RVAL)
              DO 110 J= 1, 4
                CRPAWD(J,I)= RVAL(J)
 110          CONTINUE
              DO 120 J= 1, 2
                CRPRDP(J,I)= RVAL(J+4)
 120          CONTINUE
 130        CONTINUE
C
          END IF
C
          IF (VCRDFG .EQ. 1) THEN
C           get monthly values of crdep - table-type mon-irr-crdp
            TBNO= 38
            TBSB= 1
            NVAL= 12
            CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                   CRDEPM)
          END IF
C
          IF (VAWDFG .EQ. 1) THEN
C           get monthly values of irawd - table-type mon-irr-awd
            TBNO= 39
            TBSB= 1
            NVAL= 12
            CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                   IRAWDM)
          END IF
C
        END IF
C
      ELSE
C       initialize unused fluxes to zero
        I= 2
        R= 0.0
        CALL ZIPR (I,R,
     O             IRRCF1)
        I= 3
        CALL ZIPR (I,R,
     O             IRRCF2)
        I= 6
        CALL ZIPR (I,R,
     O             IRRCF3)
      END IF
C
C     initial conditions
C
C     process values in table-type pwat-state1
      TBNO= 20
      TBSB= 1
      NVAL= 7
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PWST1)
      TGWS= AGWS
      RZWS= 0.0
      RZWSC= 0.0
C
      IF (HWTFG .EQ. 1) THEN
C       compute initial value of groundwater elevation
C
        IF (VUZFG .EQ. 1) THEN
C         upper zone nominal storage is variable
          UZSN= DAYVAL (UZSNM(MON),UZSNM(NXTMON),DAY,NDAYS)
        ENDIF
C
C       compute elevations
        LELV= SELV- LELFAC*LZSN/PCW
        UELV= SELV- (UELFAC*UZSN+ IFWSC)/UPGW
C
C       compute equivalent groundwater storages
        LLGWS= LELV*(PCW+ PGW)
        ULGWS= LLGWS+ (UELV- LELV)*PGW
        IF (BELV .GT. UELV) THEN
C         base elevation in upper region
          BGWS= ULGWS+ (BELV-UELV)*UPGW
        ELSE IF (BELV .GT. LELV) THEN
C         base elevation in lower region
          BGWS= LLGWS+ (BELV-LELV)*PGW
        ELSE
C         base elevation in groundwater region
          BGWS= BELV*(PCW+ PGW)
        END IF
        TGWS= AGWS+ BGWS
C
C       compute groundwater elevation relative to datum
        IF (TGWS .LE. LLGWS) THEN 
C         below lower influence level
          GWEL= TGWS/(PCW+ PGW)
        ELSE IF (TGWS .LE. ULGWS) THEN 
C         between lower and upper influence levels
          GWEL= LELV+ (TGWS- LLGWS)/PGW
        ELSE
C         above upper influence level
          GWEL= UELV+ ((TGWS- ULGWS)+ IFWS+ UZS)/UPGW
        END IF
        GWEL= GWEL
C
C       write out elevations and critical gw storages
C
C       write header
        WRITE (MESSU,2020)
C
C       write elevations
        WRITE (MESSU,2030)
        IF (UUNITS .EQ. 1) THEN
C         english units
          WRITE (MESSU,2040)
          PFAC= 12.0
        ELSE
C         metric units
          WRITE (MESSU,2050)
          PFAC= 39.4
        END IF
        PELEV(1)= (SELV+ GWDATM)/PFAC
        PELEV(2)= (UELV+ GWDATM)/PFAC
        PELEV(3)= (LELV+ GWDATM)/PFAC
        PELEV(4)= (BELV+ GWDATM)/PFAC
        PELEV(5)= GWDATM/PFAC
        PELEV(6)= (GWEL+ GWDATM)/PFAC
        WRITE (MESSU,2060) (PELEV(N),N=1,6)
C
C       write critical storages
        WRITE (MESSU,2070)
        IF (UUNITS .EQ. 1) THEN
C         english units
          WRITE (MESSU,2080)
        ELSE
C         metric units
          WRITE (MESSU,2090)
        END IF
        WRITE (MESSU,2060) ULGWS,LLGWS,BGWS
C
C       check elevations for proper order
        IF (GWEL .GT. SELV) THEN
C         error - active groundwater storage too high
C                 compute max for AGWS
          AGWSMX= ULGWS+ (UELFAC*UZSN- UZS)+ (IFWSC- IFWS)- BGWS
          CALL OMSTI (LSNO)
          CALL OMSTR (GWEL)
          CALL OMSTR (SELV)
          CALL OMSTR (MELEV)
          CALL OMSTR (AGWS)
          CALL OMSTR (AGWSMX)
          CALL OMSTR (ULGWS)
          CALL OMSTR (UELFAC)
          CALL OMSTR (UZSN)
          CALL OMSTR (UZS)
          CALL OMSTR (IFWSC)
          CALL OMSTR (IFWS)
          CALL OMSTR (BGWS)
          SGRP= 4
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
cccc        IF (LELV .LE. BELV) THEN
ccccC         error - lower elevation less than base elevation
cccc          CALL OMSTI (LSNO)
cccc          CALL OMSTR (BELV)
cccc          CALL OMSTR (LELV)
cccc          CALL OMSTR (SELV)
cccc          CALL OMSTR (LELFAC)
cccc          CALL OMSTR (LZSN)
cccc          CALL OMSTR (PCW)
cccc          SGRP= 5
cccc          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
cccc     M               ECOUNT)
cccc        END IF
        IF (VUZFG .EQ. 1) THEN
C         check all monthlies
          DO 150 I= 1, 12
            UELVM= SELV- (UELFAC*UZSNM(I)+ IFWSC)/UPGW
            IF (UELVM .LE. LELV) THEN
C             error - a monthly upper elevation less than
C                     lower elevation
              CALL OMSTI (LSNO)
              CALL OMSTI (I)
              CALL OMSTR (UELVM)
              CALL OMSTR (SELV)
              CALL OMSTR (UELFAC)
              CALL OMSTR (UZSN)
              CALL OMSTR (IFWSC)
              CALL OMSTR (UPGW)
              CALL OMSTR (LELV)
              CALL OMSTR (LELFAC)
              CALL OMSTR (LZSN)
              CALL OMSTR (PCW)
              SGRP= 6
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
 150      CONTINUE
        ELSE IF (UELV .LE. LELV) THEN
C         error - upper elevation less than lower elevation
          CALL OMSTI (LSNO)
          CALL OMSTR (UELV)
          CALL OMSTR (SELV)
          CALL OMSTR (UELFAC)
          CALL OMSTR (UZSN)
          CALL OMSTR (IFWSC)
          CALL OMSTR (UPGW)
          CALL OMSTR (LELV)
          CALL OMSTR (LELFAC)
          CALL OMSTR (LZSN)
          CALL OMSTR (PCW)
          SGRP= 7
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
      IF (AGWS .LT. 0.0) THEN
C       no gw storage is active
        AGWS= 0.0
        IF (TGWS .LT. 0.0) THEN
C         error - gw is below datum - cannot track
          TGWS= 0.0
        END IF
      END IF
C
C     total storage in the pls
      PERS= CEPS+ SURS+ IFWS+ UZS+ LZS+ TGWS
C
      IF (OUTLEV .GT. 1) THEN
C       end processing section message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SUTABL
     I                    (STABNO,DELT60,
     O                     NSROWS,NSCOLS,SURTAB)
C
C     + + + PURPOSE + + +
C     Process an FTABLE referred to in the input to the PWATER section
C     of the PERLND module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    STABNO,NSCOLS,NSROWS
      REAL       DELT60,SURTAB(100)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STABNO - ftable number
C     DELT60 - simulation time interval in hours
C     NSROWS - number of rows in ftable
C     NSCOLS - number of columns in ftable
C     SURTAB - ftable stored as an array
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION INTERP1 + + +
      INCLUDE    'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I0,NO,IBASE,SCLU,SGRP,MESSU,MSGFL,KEYST,KEYND,
     $             KEY,ERRFG
      REAL         PDEPTH,RVAL(8)
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER    VALNO
C
C     + + + EXTERNALS + + +
      EXTERNAL   VALNO,OMSG,OMSTI,DUMPER,GETUCI
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (2I5)
 1020 FORMAT (8F10.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING FTABLE NO. ',I3)
 2010 FORMAT (/,' FINISHED PROCESSING FTABLE NO. ',I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
      SCLU= 303
C
      IF (OUTLEV.GT.3) THEN
C       processing message
        WRITE (MESSU,2000)  STABNO
      END IF
C
C     check that the reference is valid
      NO= VALNO (NFTABS,TABINX,STABNO)
      IF (NO .EQ. 0) THEN
C       error - ftable referred to not found in ftable index
        CALL OMSTI (STABNO)
        SGRP= 10
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      ELSE
C       ftable found - process it
        KEYST= TABINX(NO,2)
        KEYND= TABINX(NO,3)
        IF (OUTLEV .GT. 5) THEN
C         dump ftable records
          CALL DUMPER (KEYST,KEYND,MESSU)
        END IF
C
        KEY= KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        READ (UCIBF,1010,ERR=10)  NSROWS, NSCOLS
 10     CONTINUE
C       check values from heading
        IF (NSROWS .LE. 0) THEN
C         no rows
          ERRFG= 1
        ELSE IF (NSCOLS .NE. 2) THEN
C         not enough or too many columns
          ERRFG= 1
        ELSE
          I= NSCOLS*NSROWS
          IF (I .GT. 100) THEN
C           too much space required
            ERRFG= 1
          ELSE
C           heading values ok
            ERRFG= 0
          END IF
        END IF
C
        IF (ERRFG .EQ. 1) THEN
C         error - ftable must have one or more rows, two columns,
C         and not more than 100 values
          CALL OMSTI (STABNO)
          CALL OMSTI (NSROWS)
          CALL OMSTI (NSCOLS)
          SGRP= 11
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         process the ftable
          ERRFG= 0
          PDEPTH= 0.0
C
C         dountil nr= nrows or errfg= 1
          I= 0
 30       CONTINUE
            I= I+ 1
            IF (KEY .EQ. KEYND) THEN
C             error - no. of rows spec'd for ftable puts it beyond
C             its end delimiter
              ERRFG= 1
              CALL OMSTI (STABNO)
              CALL OMSTI (NSROWS)
              CALL OMSTI (KEYND)
              SGRP= 12
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             process the row
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              READ (UCIBF,1020,ERR=40)  RVAL
 40           CONTINUE
C
              IF (I .EQ. 1) THEN
C               check that first row is zeroes
                ERRFG= 0
                DO 45 J= 1, NSCOLS
                  IF (RVAL(J) .GT. 0.0) THEN
C                   error - nonzero value
                    ERRFG= 1
                  END IF
 45             CONTINUE
                IF (ERRFG .EQ. 1) THEN
C                 error - ftable must have first row with all zeroes
                  CALL OMSTI (STABNO)
                  SGRP= 13
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
              END IF
C
C             check values are not negative
              ERRFG= 0
              DO 50 J= 1, NSCOLS
                IF (RVAL(J).LT.0.0) THEN
C                 negative value
                  ERRFG= 1
                END IF
 50           CONTINUE
C
              IF (ERRFG .EQ. 1) THEN
C               error - negative value
                CALL OMSTI (STABNO)
                CALL OMSTI (I)
                SGRP= 14
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             check that depth is not decreasing
              IF (RVAL(1).LT.PDEPTH) THEN
C               error - depth must not decrease
                CALL OMSTI (I)
                CALL OMSTI (STABNO)
                SGRP= 15
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             check that outflow fraction is not greater than unity
              IF (RVAL(2) .GT. 1.0) THEN
C               error - outflow fraction too big
                CALL OMSTI (I)
                CALL OMSTI (STABNO)
                SGRP= 16
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             remember depth for processing next row
              PDEPTH= RVAL(1)
C
C             convert values to internal units
C             outflow rate
              RVAL(2)= RVAL(2)*DELT60
C
C             store values in array
              IBASE= (I- 1)*NSCOLS
              DO 90 J= 1, NSCOLS
                SURTAB(IBASE+ J)= RVAL(J)
 90           CONTINUE
            END IF
C           loop back for more rows
          IF ( (I .NE. NSROWS) .AND. (ERRFG .NE. 1) ) GO TO 30
        END IF
      END IF
C
      IF (OUTLEV.GT.3) THEN
        WRITE (MESSU,2010)  STABNO
      END IF
C
      RETURN
      END
C
C     4.2(1).3
C
      SUBROUTINE   PWATER
C
C     + + + PURPOSE + + +
C     Simulate the water budget for a pervious land segment.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE     'cplpw.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      REAL        PREC
      CHARACTER*6 TSNAM,OPTYP,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    HREQTS,DAYVAL,PIRRIG,PWAHWT,PWATRX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'PERLND','PWATER'/
C
C     + + + END SPECIFICATIONS + + +
C
C     get input PET
Cthj      PETINP= PAD(PETIFP+ IVL1)
      REQFG= 2   
      TSNAM= 'PETINP'
      CALL HREQTS (PETIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PETINP)
C
      IF (CSNOFG .EQ. 1) THEN
C       snow is being considered - allow for it
C       find the moisture supplied to interception storage
C       rainf is rainfall in inches/ivl. adjust for fraction of land
C       segment covered by snow. WYIELD is the water yielded by the
C       snowpack in inches/ivl. it has already been adjusted to an
C       effective yield over the entire land segment
C
C       get input time series
        IF (AIRTFG .EQ. 0) THEN
C         get air temperature data
Cthj          AIRTMP= PAD(AIRTFP+ IVL1)
            REQFG= 5
            MSECNM= 'ATEMP '
            OPFGNM= 'CSNOFG'
            CALL HREQTS (AIRTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                   AIRTMP)

        ELSE
C         air temperatures, in degrees f, are available from atemp
        END IF
C
        IF (SNOWFG .EQ. 0) THEN
C         get snow time series
Cthj          RAINF= PAD(RNFFP+ IVL1)
          REQFG= 5
          MSECNM= 'SNOW  '
          OPFGNM= 'CSNOFG'
          TSNAM= 'RAINF '
          CALL HREQTS (RNFFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 RAINF)
Cthj          SNOCOV= PAD(SNOCFP+ IVL1)
          TSNAM= 'SNOCOV'
          CALL HREQTS (SNOCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 SNOCOV)
Cthj        WYIELD= PAD(WYFP+ IVL1)
          TSNAM= 'WYIELD'
          CALL HREQTS (WYFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 WYIELD)
          IF (ICEFG .EQ. 1) THEN
C           effects of frozen ground are considered
Cthj            PACKI= PAD(PIFP+ IVL1)
            TSNAM= 'PACKI '
            OPFGNM= 'ICEFG '
            CALL HREQTS (PIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                   TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,ICEFG,
     O                   PACKI)
          END IF
        ELSE
C         the above time series are available from snow
        END IF
C
        SUPY= RAINF*(1.0- SNOCOV)+ WYIELD
C
        IF (HRFG .EQ. 1) THEN
C         it is time to recalculate intermittently computed numbers
C
C         adjustment factor for input pet to account for forest and
C         snowcover
          PETADJ= (1.0- FOREST)*(1.0- SNOCOV)+ FOREST
C
          IF (AIRTMP .LT. PETMAX) THEN
C           adjustment factor may be reduced
            IF (AIRTMP .LT. PETMIN) THEN
C             pet is completely shut off
              PETADJ= 0.0
            ELSE
              IF (PETADJ .GT. 0.5) THEN
                PETADJ= 0.5
              END IF
            END IF
          END IF
C
          IF (ICEFG .EQ. 1) THEN
C           calculate factor to reduce infiltration and percolation
C           to account for frozen ground
            IF (IFFCFG .EQ. 1) THEN
C             enhanced version of the original method -
C             this algorithm defaults to the original result if
C             the parameters (fzg and fzgl) have the default values
              INFFAC= 1.0- FZG*PACKI
              IF (INFFAC .LT. FZGL) THEN
                INFFAC= FZGL
              END IF
            END IF
          ELSE
            INFFAC= 1.0
          END IF
        ELSE
C         petadj and inffac remain unchanged
        END IF
C
C       adjust input pet
        PET= PETINP*PETADJ
      ELSE
C       snow is not being considered
C       all precipitation is assumed to be rain
Cthj        PREC= PAD(PRECFP+ IVL1)
        REQFG= 4
        TSNAM= 'PREC  '
        OPFGNM= 'CSNOFG'
        CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O               PREC)
        SUPY= PREC
        PET= PETINP
        INFFAC= 1.0
      END IF
C
C     adjust INFFAC based on soil temperature
      IF (HRFG .EQ. 1) THEN
        IF (IFFCFG .EQ. 2) THEN
C         optional method - based on lower soil temperature and fzgl
          IF (PSTFG .EQ. 0) THEN
C           get soil temperatures from the inpad
CTHJ            LGTMP= PAD(LGTFP+ IVL1)
            REQFG= 5
            TSNAM= 'PREC  '
            MSECNM= 'PSTEMP'
            OPFGNM= 'IFFCFG'
            CALL HREQTS (LGTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                   TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,IFFCFG,
     O                   LGTMP)
          ELSE
C           soil temperatures have been calculated in pstemp
          END IF
          IF (LGTMP .LE. 0.0) THEN
C           frozen ground
            INFFAC= FZGL
          ELSE
C           not frozen
            INFFAC= 1.0
          END IF
        END IF
      END IF
C         
C     Uzsn may be a varying parameter - the code to interpolate its
C     current value is placed here because UZSN is used in several
C     places in the code below this point
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VUZFG .EQ. 1) THEN
C         uzsn is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate uzsn between two values from the
C         monthly array uzsnm(12)
          UZSN= DAYVAL(UZSNM(MON),UZSNM(NXTMON),DAY,NDAYS)
        ELSE
C         uzsn does not vary throughout the year and
C         has been supplied by the run interpreter
        END IF
      END IF
C
C     get lateral inflow timeseries
C
      IF (SLIFP .GT. 0) THEN
C       surface lateral inflow timseries
        SURLI= PAD(SLIFP+ IVL1)
      ELSE
C       surface lateral inflow not considered
        SURLI= 0.0
      END IF
      IF (ILIFP .GT. 0) THEN
C       interflow lateral inflow timseries
        IFWLI= PAD(ILIFP+ IVL1)
      ELSE
C       interflow lateral not being considered
        IFWLI= 0.0
      END IF
      IF (ULIFP .GE. 1) THEN
C       there is lateral inflow to upper zone
        UZLI= PAD(ULIFP+IVL1)
      ELSE
C       no lateral inflow
        UZLI= 0.0
      END IF
      IF (LLIFP .GE. 1) THEN
C       there is lateral inflow to lower zone
        LZLI= PAD(LLIFP+IVL1)
      ELSE
C       no lateral inflow
        LZLI= 0.0
      END IF
      IF (ALIFP .GT. 0) THEN
C       groundwater lateral inflow timeseries
        AGWLI= PAD(ALIFP+ IVL1)
      ELSE
C       groundwater lateral inflow not considered
        AGWLI= 0.0
      END IF
C
      IF (IRRGFG .GE. 1) THEN
C       perform irrigation routine
C
        IF (IRRGFG .EQ. 1) THEN
C         input demand timeseries
          IF (IRRIFP .GE. 1) THEN
C           timeseries provided
            IRRINP= PAD(IRRIFP+ IVL1)
          ELSE
C           no demand
            IRRINP= 0.0
          END IF
        END IF
C
        CALL PIRRIG
      ELSE
C       no irrigation demand
        IRRDEM= 0.0
        RZWS= 0.0
        RZWSC= 0.0
      END IF
C
C     perform simulation 
      IF (HWTFG .EQ. 1) THEN
C       pwater modified for high water table conditions
        CALL PWAHWT
      ELSE
C       pwater (unmodified)
        CALL PWATRX
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWATRX
C
C     + + + PURPOSE + + +
C     Simulate the water budget for a pervious land segment.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE     'cplpw.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL        GWI,IPERC,LZRAT,MSUPY,REMPET
C
C     + + + EXTERNALS + + +
      EXTERNAL    ICEPT,SURFAC,INTFLW,UZONE,LZONE,GWATER,EVAPT
C
C     + + + HISTORY + + +
C     5/2004  (brb) IFRDFG added to call to subr LZONE for IHM infilt redistrib method
C
C     + + + END SPECIFICATIONS + + +
C
C     simulate interception
      CALL ICEPT (VCSFG,DAYFG,CEPSCM,MON,NXTMON,DAY,NDAYS,SUPY,
     I            IRRAPP(1),
     M            CEPSC,CEPS,
     O            CEPO)
C
C     surface inflow is the sum of interception outflow and surface
C     lateral inflow (if any)
      SURI= CEPO+ SURLI
C
C     Msupy is used below and again in subroutine group surfac
C     it includes irrigation application not subject
C     to interception (if any)
C     Surss is the value of surs at the start of the ivl -
C     it is used by module section MSTLAY
      MSUPY= SURI+ SURS+ IRRAPP(2)
      SURSS= SURS
C
C     set flags which indicate whether or not there is anything to
C     infiltrate and whether or not this is the first in a series of
C     wet intervals
C
      IF (MSUPY .GT. 0.0) THEN
C       there is surface moisture supply
        IF (SMSFG .EQ. 0) THEN
C         this is the first interval with surface moisture supply
C         after one or more intervals with none
          FSMSFG= 1
        ELSE
C         this is not the first wet interval
          FSMSFG= 0
        END IF
C       there is surface moisture supply
        SMSFG= 1
      ELSE
C       there is no surface moisture supply
        SMSFG= 0
        FSMSFG= 0
      END IF
C
C     determine the current value of the lower zone storage ratio
      LZRAT= LZS/LZSN
C
      IF (SMSFG .EQ. 1) THEN
C       simulate behavior of water on the land surface - surface
C       detention, infiltration, interflow input, surface outflow
        CALL SURFAC (LZRAT,INFILT,INFEXP,INFFAC,INFILD,FSMSFG,DAYFG,
     I               VNNFG,NSURM,LSUR,SLSUR,VIFWFG,INTFWM,MSUPY,UZSN,
     I               UZS,DELT60,UZRA,INTGRL,RTOPFG,UZFG,MON,NXTMON,DAY,
     I               NDAYS,LSNO,MESSU,MSGFL,DATIM,
     M               DEC,SRC,NSUR,INTFW,SURS,PWECNT(1),PWECNT(2),
     O               INFIL,UZI,IFWI,SURO)
C
      ELSE
C       storage on the surface and outputs from it will be zero
        SURS= 0.0
        SURO= 0.0
        IFWI= 0.0
        INFIL= 0.0
        UZI= 0.0
      END IF
C
C     simulate interflow
      CALL INTFLW (DAYFG,VIRCFG,IRCM,MON,NXTMON,DAY,NDAYS,DELT60,IFWI,
     I             IFWLI,
     M             IRC,IFWK1,IFWK2,IFWS,UZS,
     O             IFWO)
C
C     simulate upper zone behavior
      CALL UZONE (UZSN,UZI,UZLI,IRRAPP(3),INFILT,INFFAC,LZRAT,
     M            UZS,
     O            PERC)
C
C     collect inflows to lower zone and groundwater
      IPERC= PERC+ INFIL+ LZLI
C
C     simulate lower zone behavior
      CALL LZONE (IPERC,IRRAPP(4),LZRAT,IFRDFG,
     M            LZFRAC,LZS,RLZRAT,
     O            LZI)
C
C     simulate groundwater behavior - first account for the fact that
C     iperc doesn't include lzirr
      GWI= (IPERC+ IRRAPP(4))- LZI
      CALL GWATER (DEEPFR,GWI,KVARY,DAYFG,KGW,AGWLI,IRRAPP(5),
     M             AGWS,GWVS,
     O             IGWI,AGWI,AGWO)
      TGWS= AGWS
C
C     simulate ET
      CALL EVAPT (PET,BASETP,UZSN,AGWETP,KVARY,DAYFG,VLEFG,LZETPM,MON,
     I            NXTMON,DAY,NDAYS,LZSN,DELT60,
     M            AGWO,CEPS,UZS,AGWS,GWVS,LZETP,RPARM,LZS,
     O            REMPET,TAET,BASET,CEPE,UZET,AGWET,LZET)
      TGWS= AGWS
C
C     find total outflow
      PERO= SURO+ IFWO+ AGWO
C
C     total input of water to the pervious land segment
      WATIN= SUPY+ SURLI+ UZLI+ IFWLI+ LZLI+ AGWLI+ IRRAPP(6)
C
C     net input of water to the pervious land segment
      WATDIF= WATIN- (PERO+ IGWI+ TAET+ IRDRAW(2))
C
C     total moisture storage
      PERS= CEPS+ SURS+ IFWS+ UZS+ LZS+ TGWS
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWAHWT
C
C     + + + PURPOSE + + +
C     Simulate the water budget for a pervious land segment
C     with high water table conditions.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE     'cplpw.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL        GWI,IPERC,LZRAT,MSUPY,REMPET,UZCA,AGWTMP,IFWI0,TGWS0,
     $            GWEL0,UZLIM,LZLIM,PSUR
      LOGICAL     UPRGN,LOWRGN
C
C     + + + EXTERNALS + + +
      EXTERNAL    ICEPT,SURFHW,INTFHW,SUROHW,UZONE,LZONE,GWATER,
     $            EVAPHW,ADJGWE
C
C     + + + HISTORY + + +
C     5/2004  (brb) IFRDFG added to call to subr LZONE for IHM infilt redistrib method
C
C     + + + END SPECIFICATIONS + + +
C
C     store initial values of ground water storage and elevation
      TGWS0= TGWS
      GWEL0= GWEL
C
      IF ( (VUZFG .EQ. 1) .AND. (DAYFG .EQ. 1) ) THEN
C       recompute upper influence elevation and equiv storage
        UELV= SELV- (UELFAC*UZSN+ IFWSC)/UPGW
        ULGWS= LLGWS+ (UELV- LELV)*PGW
      END IF
C
C     recalculate ground water elevation, using storages, so that it does not 
C     deviate
      IF (TGWS .LE. LLGWS) THEN 
C       below lower influence level
        GWEL= TGWS/(PCW+ PGW)
      ELSE IF (TGWS .LE. ULGWS) THEN 
C       between lower and upper influence levels
        GWEL= LELV+ (TGWS- LLGWS)/PGW
      ELSE
C       above upper influence level
        GWEL= UELV+ ((TGWS- ULGWS)+ IFWS+ UZS)/UPGW
      END IF
C
C     adjustments to avoid jumping around influence elevations
C
C     determine ground water storage thresholds for the current time step,
C     including tolerance levels. Also determine if ground water elevation
C     adjustments were made in the previous time step
C
      UPRGN= .FALSE. 
      LOWRGN= .FALSE.
C
      IF (ABS (GWEL- GWEL0) .GT. 0.001) THEN
C       an adjustment for tolerance was made in a previous hour;
C       continue comparing
        IF (GWEL0 .GT. GWEL) THEN
C         coming from above;
C         flag to indicate that in the previous hour the 
C         ground storage water fell to a lower region, but the elevation
C         has not been updated yet
          UPRGN= .TRUE.
          UZLIM= ULGWS- DELTA
          LZLIM= LLGWS- DELTA
        ELSE
C         flag to indicate that in the previous hour the 
C         ground storage water reaches an upper region, but the elevation
C         has not been updated yet
          LOWRGN= .TRUE.
          UZLIM= ULGWS+ DELTA
          LZLIM= LLGWS+ DELTA
        END IF
      ELSE
        IF (TGWS0 .LT. LLGWS) THEN 
C         currently below LELV
          LZLIM= LLGWS+ DELTA
        ELSE 
C         currently above LELV
          LZLIM= LLGWS- DELTA
        END IF
        IF (TGWS0 .LT. ULGWS) THEN
C         currently below UELV
          UZLIM= ULGWS+ DELTA
        ELSE 
C         currently above UELV
          UZLIM= ULGWS- DELTA
        END IF
      END IF
C
C     simulate interception 
      CALL ICEPT (VCSFG,DAYFG,CEPSCM,MON,NXTMON,DAY,NDAYS,SUPY,
     I            IRRAPP(1),
     M            CEPSC,CEPS,
     O            CEPO)
C
C     surface inflow is the sum of interception outflow and surface
C     lateral inflow (if any)
      SURI= CEPO+ SURLI
C
C     Msupy is used below and again in subroutine group surfac
C     it includes irrigation application not subject
C     to interception (if any)
C     Surss is the value of surs at the start of the ivl -
C     it is used by module section MSTLAY
      MSUPY= SURI+ SURS+ IRRAPP(2)
      SURSS= SURS
C
C     set flags which indicate whether or not there is anything to
C     infiltrate and whether or not this is the first in a series of
C     wet intervals
      IF (MSUPY .GT. 0.0) THEN
C       there is surface moisture supply
        IF (SMSFG .EQ. 0) THEN
C         this is the first interval with surface moisture supply
C         after one or more intervals with none
          FSMSFG= 1
        ELSE
C         this is not the first wet interval
          FSMSFG= 0
        END IF
C       there is surface moisture supply
        SMSFG= 1
      ELSE
C       there is no surface moisture supply
        SMSFG= 0
        FSMSFG= 0
      END IF
C
C     determine the current value of the lower zone storage ratio
      LZRAT= LZS/LZSN

      IF (SMSFG .EQ. 1) THEN
C       distribute water for infiltration and surface runoff
C       simulate behavior of water on the land surface (except
C       surface runoff) - detention, infiltration, interflow input
        CALL SURFHW (LZRAT,INFILT,INFEXP,INFFAC,INFILD,FSMSFG,DAYFG,
     I               VIFWFG,INTFWM,MSUPY,UZSN,UZS,UZRA,INTGRL,UZFG,MON,
     I               NXTMON,DAY,NDAYS,LSNO,MESSU,MSGFL,DATIM,
     M               INTFW,PWECNT(1),
     O               INFIL,UZI,IFWI,PSUR,UZCA)
C
      ELSE
C       storage on the surface and outputs from it will be zero
        IFWI= 0.0
        INFIL= 0.0
        UZI= 0.0
        PSUR= 0.0
C?!     what should uzca be in this case ? I assumed 0.0, But ?
        UZCA= 0.0
      END IF
      IFWI0= IFWI
C
C     simulate interflow
      CALL INTFHW (DAYFG,VIRCFG,IRCM,MON,NXTMON,DAY,NDAYS,DELT60,IFWLI,
     I             ULGWS,LLGWS,IFWSC,IFWI0,
     M             IRC,IFWK1,IFWK2,IFWI,IFWS,UZS,TGWS,PSUR,
     O             IFWO)
C     recompute AGWS
      AGWS= TGWS- BGWS
      IF (AGWS .LT. 0) THEN
C       no gw above base elevation
        AGWS= 0.0
      END IF
C
C     simulate surface runoff
      CALL SUROHW (RTOPFG,FSMSFG,DAYFG,VNNFG,MON,NXTMON,DAY,NDAYS,SRRC,
     I             SREXP,NSROWS,NSCOLS,SURTAB,NSURM,LSUR,SLSUR,PSUR,
     I             DELT60,MESSU,MSGFL,LSNO,
     M             PWECNT(2),NSUR,DEC,SRC,SURS,
     O             SURO)
C
C     simulate upper zone behavior
      CALL UZONE (UZSN,UZI,UZLI,IRRAPP(3),INFILT,INFFAC,LZRAT,
     M            UZS,
     O            PERC)
C
C     collect inflows to lower zone and groundwater
      IPERC= PERC+ INFIL+ LZLI
C
C     simulate lower zone behavior
      CALL LZONE (IPERC,IRRAPP(4),LZRAT,IFRDFG,
     M            LZFRAC,LZS,RLZRAT,
     O            LZI)
C
C     simulate groundwater behavior - first account for the fact that
C     iperc doesn't include lzirr
      GWI= (IPERC+ IRRAPP(4))- LZI
      AGWTMP= TGWS- BGWS
      CALL GWATER (DEEPFR,GWI,KVARY,DAYFG,KGW,AGWLI,IRRAPP(5),
     M             AGWTMP,GWVS,
     O             IGWI,AGWI,AGWO)
      TGWS= TGWS+ ((AGWLI+ AGWI+ IRRAPP(5))- AGWO)
      AGWS= TGWS- BGWS
      IF (AGWS .LT. 0.0) THEN
C       no active groundwater storage
        AGWS= 0.0
      END IF
C
C     simulate ET
      CALL EVAPHW (PET,BASETP,UZSN,AGWETP,KVARY,DAYFG,VLEFG,LZETPM,MON,
     I             NXTMON,DAY,NDAYS,LZSN,DELT60,ULGWS,LLGWS,SELV,UELV,
     I             LELV,UPGW,PGW,IFWS,
     M             AGWO,CEPS,UZS,AGWS,TGWS,GWVS,LZETP,RPARM,LZS,SURS,
     M             GWEL,
     O             REMPET,TAET,BASET,CEPE,UZET,AGWET,LZET,SURET)
C
C
C     do adjustments of gw elevation, etc
      CALL ADJGWE (DATIM,LSNO,MESSU,MSGFL,PCW,PGW,UPGW,ULGWS,LLGWS,
     I             UZLIM,LZLIM,SELV,UELV,LELV,IFWSC,UZSN,UZCA,LZSN,
     I             TGWS0,UPRGN,LOWRGN,UELFAC,LELFAC,
     M             SURS,IFWS,UZS,LZS,AGWS,TGWS,GWEL)
C
C     find total outflow
      PERO= SURO+ IFWO+ AGWO
C
C     total input of water to the pervious land segment
      WATIN= SUPY+ SURLI+ UZLI+ IFWLI+ LZLI+ AGWLI+ IRRAPP(6)
C
C     net input of water to the pervious land segment
      WATDIF= WATIN- (PERO+ IGWI+ TAET+ IRDRAW(2))
C
C     total moisture storage
      PERS= CEPS+ SURS+ IFWS+ UZS+ LZS+ TGWS
C
      RETURN
      END
C
C?! questions
C
C1. What about lateral inflow?  I have included it, but it wasnt easy
C
C2. When/how do I compute deepgwi, etc?
C   and 
C   What is difference between deepgwi deepperc?
C
C
C
      SUBROUTINE   DSPHWT
     I                    (IMIN,IMAX,RATIO,MSUPY,UZSN,UZS,UZRA,INTGRL,
     I                     UZFG,LSNO,MESSU,MSGFL,DATIM,
     M                     ECNT1,
     O                     INFIL,UZI,IFWI,PSUR,UZFRAC)
C
C     + + + PURPOSE + + +
C     Dispose of moisture supply on the land segment.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECNT1,MSGFL,LSNO,MESSU,UZFG,DATIM(5)
      REAL       IFWI,IMAX,IMIN,INFIL,INTGRL(10),MSUPY,RATIO,UZI,
     $           UZRA(10),UZS,UZSN,PSUR,UZFRAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMIN   - ???
C     IMAX   - ???
C     RATIO  - ???
C     MSUPY  - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     UZRA   - ???
C     INTGRL - ???
C     UZFG   - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     ECNT1  - ???
C     INFIL  - ???
C     UZI    - ???
C     IFWI   - ???
C     DATIM  - date and time of day
C     PSUR   - ???
C     UZFRAC - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       IIMAX,IIMIN,OVERI,OVERII,PDRO,PIFWI,
     $           UNDRI,UNDRII
C
C     + + + EXTERNALS + + +
      EXTERNAL   DIVISN, UZINF, UZINF2
C
C     + + + END SPECIFICATIONS + + +
C
C     determine how much of the MSUPY falls above and below the
C     infiltration line in the infiltration/interflow/surface runoff
C     figure
      CALL DIVISN (IMIN,IMAX,MSUPY,
     O             OVERI,UNDRI)
C
C     the quantity under I is infiltrated moisture
      INFIL= UNDRI
      IF (OVERI .GT. 0.0) THEN
C       there is some potential interflow inflow and maybe surface
C       detention/outflow -- the sum of these is potential direct
C       runoff
        PDRO= OVERI
C       determine how much of this potential direct runoff will
C       be taken by the upper zone
        IF (UZFG .EQ. 0) THEN
          CALL UZINF (PDRO,UZSN,UZS,UZRA,INTGRL,LSNO,MESSU,MSGFL,DATIM,
     M                ECNT1,
     O                UZI)
        ELSE
          CALL UZINF2 (PDRO,UZSN,UZS,
     O                 UZI)
        END IF
        IF (UZI .GT. PDRO) THEN
          UZI= PDRO
        END IF
        UZFRAC= UZI/PDRO
C
C       determine how much of the msupy falls above and below the
C       "infiltration + interflow" line in the infiltration/
C       interflow/surface runoff figure. the prefix "ii" is used on
C       variables associated with this line
        IIMIN= IMIN*RATIO
        IIMAX= IMAX*RATIO
        CALL DIVISN (IIMIN,IIMAX,MSUPY,
     O               OVERII,UNDRII)
C
C       psur is potential surface detention/runoff
        PSUR= OVERII
C       pifwi is potential interflow inflow
        PIFWI= PDRO- PSUR
        IFWI= PIFWI*(1.0- UZFRAC)
C
        IF (PSUR .GT. 0.0) THEN
C         there will be something on or running off the surface
C         reduce it to account for the upper zone's share
          PSUR= PSUR*(1.0- UZFRAC)
        END IF
      ELSE
C       there is no potential direct runoff or contribution to the
C       upper zone
        PSUR= 0.0
        IFWI= 0.0
        UZI= 0.0
C?!     what should uzfrac be in this case ? I assumed 0.0, But ?
        UZFRAC= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVAPHW
     I                    (PET,BASETP,UZSN,AGWETP,KVARY,DAYFG,VLEFG,
     I                     LZETPM,MON,NXTMON,DAY,NDAYS,LZSN,DELT60,
     I                     ULGWS,LLGWS,SELV,UELV,LELV,UPGW,PGW,IFWS,
     M                     AGWO,CEPS,UZS,AGWS,TGWS,GWVS,LZETP,RPARM,
     M                     LZS,SURS,GWEL,
     O                     REMPET,TAET,BASET,CEPE,UZET,AGWET,LZET,
     O                     SURET)
C
C     + + + PURPOSE + + +
C     Simulate evapotranspiration (ET) for high water table conditions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAYFG,VLEFG,MON,NXTMON,DAY,NDAYS
      REAL       PET,BASETP,UZSN,AGWETP,KVARY,LZETPM(12),LZSN,DELT60,
     $           ULGWS,LLGWS,SELV,UELV,LELV,UPGW,PGW,IFWS,AGWO,CEPS,
     $           UZS,AGWS,TGWS,GWVS,LZETP,RPARM,LZS,SURS,GWEL,REMPET,
     $           TAET,BASET,CEPE,UZET,AGWET,LZET,SURET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PET    - potential evaporation
C     BASETP - et from baseflow
C     UZSN   - upper zone nominal storage
C     AGWETP - et from active gw
C     KVARY  - ???
C     DAYFG  - flag for first day or day change
C     VLEFG  - flag for variable lower zone et parameter 
C     LZETPM - monthly lower zone et parameter 
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     LZSN   - lower zone nominal storage parameter
C     DELT60 - simulation time interval in hours
C     ULGWS  - ???
C     LLGWS  - ???
C     SELV   - ???
C     UELV   - ???
C     LELV   - ???
C     UPGW   - ???
C     PGW    - ???
C     IFWS   - ???
C     AGWO   - active gw outflow
C     CEPS   - interception storage
C     UZS    - initial upper zone storage
C     AGWS   - active gw storage
C     TGWS   - total gw storage above datum
C     GWVS   - ???
C     LZETP  - lower zone et parameter
C     RPARM  - ???
C     LZS    - lower zone storage
C     SURS   - surface storage
C     GWEL   - groundwater elevation
C     REMPET - remaining unsatisfied pet
C     TAET   - total actual et
C     BASET  - et from baseflow
C     CEPE   - et from interception storage
C     UZET   - et from upper zone
C     AGWET  - et from active gw
C     LZET   - et from lower zone
C     SURET  - et from surface storage
C
C     + + + EXTERNALS + + +
      EXTERNAL   ETBASE,EVICEP,EVSUR,ETUZON,ETLZON,ETAGW
C
C     + + + END SPECIFICATIONS + + +
C
C     Taet is total actual et - inches/ivl
      TAET= 0.0
C     Rempet is remaining potential et - inches/ivl
      REMPET= PET
      IF (REMPET .GT. 0.0) THEN
C       simulate et from baseflow
        CALL ETBASE (BASETP,
     M               AGWO,REMPET,TAET,
     O               BASET)
C
        IF (REMPET .GT. 0.0) THEN
C         simulate evaporation from interception
          CALL EVICEP
     M                (CEPS,REMPET,TAET,
     O                 CEPE)
C
          IF (REMPET .GT. 0.0) THEN
C           simulate evaporation from surface detention
            CALL EVSUR
     M                 (SURS,REMPET,TAET,
     O                  SURET)
C
            IF (REMPET .GT. 0.0) THEN
C             simulate et from the upper zone
              CALL ETUZON (UZSN,
     M                     REMPET,UZS,TAET,
     O                     UZET)
C
              IF ( (REMPET .GT. 0.0) .AND. (TGWS .LE. LLGWS) ) THEN
C               simulate et from active groundwater -
C               use standard method only if the ground water is 
C               below the lower influence level
                CALL ETAGW (AGWETP,KVARY,
     M                      REMPET,TGWS,TAET,GWVS,
     O                      AGWET)
C
                AGWS= AGWS- AGWET
                IF (AGWS .LT. 0.0) THEN
C                 below base elevation
                  AGWS= 0.0
                END IF
              ELSE
C               no gw et
                AGWET= 0.0
              END IF
            ELSE
C             no upper zone et
              UZET= 0.0
              AGWET= 0.0
            END IF
          ELSE
C           no surface et
            SURET= 0.0
            UZET= 0.0
            AGWET= 0.0
          END IF
        ELSE
C         no interception et
          CEPE= 0.0
          SURET= 0.0
          UZET= 0.0
          AGWET= 0.0
        END IF
      ELSE
C       no baseflow et
        BASET= 0.0
        CEPE= 0.0
        SURET= 0.0
        UZET= 0.0
        AGWET= 0.0
      END IF
C
C     Et from lower zone is handled here because it must be called
C     every interval to make sure that seasonal variation in
C     parameter LZETP and recalculation of RPARM are correctly done
C     simulate ET from the lower zone
C@@@
      CALL ETLZON (DAYFG,VLEFG,MON,DAY,LZETPM,NXTMON,NDAYS,LZSN,DELT60,
     M             LZETP,REMPET,RPARM,LZS,TAET,
     O             LZET)
C
C     first take water from agws above lelv if gwel above lelv
C     (also, note that only one computation of agwet is done, 
C     depending on gwel (or tgws))
C
      IF (TGWS .GT. LLGWS) THEN  
C       gwel above lelv
C       first take water from tgws above lelv;
C       The fraction of rempet that can be sought from agws is
C       proportional to the groundwater elevation above the lower
C       influence level;
C       recalculate gw elevation in case something has changed
        IF (TGWS .GT. ULGWS) THEN
C         above upper influence level
          GWEL= UELV+ (IFWS+ UZS+ TGWS- ULGWS)/UPGW
        ELSE 
C         between upper and lower influence level
          GWEL= LELV+ (TGWS- LLGWS)/PGW
        END IF
C
        IF (GWEL .GT. SELV) THEN 
C         cut off gw elevation at surface
          GWEL= SELV
        END IF
C
        AGWET= REMPET*(AGWETP+
     $         ((1.0- AGWETP)*(GWEL- LELV)/(SELV- LELV)))
C
C       limit aet from ground water to ground water storage above 
C       lower influence level 
        IF (AGWET .GT. (TGWS- LLGWS)) THEN 
C         empty storage above lower influence level
          AGWET= (TGWS- LLGWS)
          GWEL= LELV
        END IF
        TGWS= TGWS- AGWET
        AGWS= AGWS- AGWET
        IF (AGWS .LT. 0.0) THEN
C         groundwater has fallen below base elevation
          AGWS= 0.0
        END IF
        REMPET= REMPET- AGWET
        TAET= TAET+ AGWET
      END IF
C     finally, do lower zone et
c@@@@
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVSUR
     M                   (SURS,REMPET,TAET,
     O                    SURET)
C
C     + + + PURPOSE + + +
C     Simulate evaporation from surface detention storage
C     for high water table conditions.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       SURET,SURS,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURS   - surface storage
C     REMPET - remaining potential et
C     TAET   - total actual et
C     SURET  - et from surface detention storage
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SURS .GT. 0.0) THEN
C       there is something in surface detention storage to evaporate
        IF (REMPET .GT. SURS) THEN
C         evaporation from detention storage is limited
C         by quantity available
          SURET= SURS
          SURS= 0.0
        ELSE
C         surface detention evaporation will not exhaust storage, so
C         empty at potential
          SURET= REMPET
          SURS= SURS- SURET
        END IF
C
C       update totals
        TAET= TAET+ SURET
        REMPET= REMPET- SURET
      ELSE
C       there is no evaporation from surface detention storage
        SURET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTFHW
     I                    (DAYFG,VIRCFG,IRCM,MON,NXTMON,DAY,NDAYS,
     I                     DELT60,IFWLI,ULGWS,LLGWS,IFWSC,IFWI0,IRC,
     M                     IFWK1,IFWK2,IFWI,IFWS,UZS,TGWS,
     M                     PSUR,
     O                     IFWO)
C
C     + + + PURPOSE + + +
C     Simulate interflow in high water table conditions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAYFG,VIRCFG,MON,NXTMON,DAY,NDAYS
      REAL       IRCM(12),DELT60,IFWLI,ULGWS,LLGWS,IFWSC,IFWI0,IRC,
     $           IFWK1,IFWK2,IFWI,IFWS,UZS,TGWS,PSUR,IFWO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VIRCFG - flag for variable interflow recession constant parm
C     IRCM   - variable interflow recession constant
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     DELT60 - simulation time interval in hours
C     IFWLI  - lateral inflow of interflow
C     ULGWS  - active gw storage when water is at uelv
C     LLGWS  - active gw storage when water is at lelv
C     IFWSC  - maximum interflow storage capacity
C     IFWI0  - initial value of interflow inflow
C     IRC    - interflow recession constant parameter
C     IFWK1  - ???
C     IFWK2  - ???
C     IFWI   - interflow inflow
C     IFWS   - interflow storage
C     UZS    - initial upper zone storage
C     TGWS   - total gw storage
C     PSUR   - ???
C     IFWO   - interflow outflow
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY,INFLO,KIFW,VALUE,IFWS0,LIFWS,DIFWS
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VIRCFG .EQ. 1) THEN
C         interflow recession constant is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate irc between two values from the
C         monthly array ircm(12)
          IRC= DAYVAL (IRCM(MON),IRCM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow recession constant does not vary throughout the
C         year - irc value has been supplied by the run interpreter
        END IF
C
C       derive parameters used in routing
        DUMMY= 24.0/DELT60
        KIFW= -ALOG (IRC)/DUMMY
        IFWK2= 1.0- EXP (-KIFW)
        IFWK1= 1.0- (IFWK2/KIFW)
      END IF
C
C?!   what is the "slight difference with hspf"?
      IFWS0= IFWS
      IF ((TGWS .GT. LLGWS) .AND. (TGWS .LE. ULGWS) .AND. 
     $    (IFWS0 .GT. IFWSC)) THEN
C       check that agws has not reached ifws
C       ifws below uelv is:
        LIFWS= IFWS0- IFWSC
C       total gravity porosity available there is ulgws-llgws
        IF ((LIFWS+ TGWS) .GT. ULGWS) THEN
C         transfer ifws to agws
          DIFWS= LIFWS+ TGWS- ULGWS
          IFWS= IFWS- DIFWS
          IFWS0= IFWS
          TGWS= TGWS+ DIFWS
        END IF
      END IF
C
      INFLO= IFWI+ IFWLI
      VALUE= INFLO+ IFWS
C     check if gw is above upper influence level
      IF (TGWS .GT. ULGWS) THEN
C       above upper zone influence level
C       make sure ifws does not exceed ifwsc
        IF (IFWS0 .LT. IFWSC) THEN
          IFWO= IFWK2*IFWS+ IFWK1*(INFLO)
          IFWS= IFWS0 + INFLO- IFWO
          IF (IFWS .GT. IFWSC) THEN
            IFWO= IFWK2*(IFWSC)
            IFWS= IFWSC
            IFWI= IFWSC- IFWS0+ IFWO
            PSUR= PSUR+ (IFWI0+ IFWLI- IFWI)
            IF (PSUR .LT. 0.0) THEN
              PSUR= 0.0
            END IF
          END IF
        ELSE
          IFWO= IFWK2*(IFWS0)
          IFWS= IFWS0- IFWO
          IFWI= 0.0
          PSUR= PSUR+ IFWI0+ IFWLI
        END IF
      ELSE
C       gw is below upper influence level
        IF (VALUE .GT. 0.00002) THEN
C         there is something worth routing
          IFWO= IFWK2*IFWS+ IFWK1*(INFLO)
          IFWS= IFWS0+ IFWI+ IFWLI- IFWO
        ELSE
C         nothing worth routing-dump back to uzs
          IFWO= 0.0
          IFWS= 0.0
          UZS = UZS+ VALUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SURFHW
     I                    (LZRAT,INFILT,INFEXP,INFFAC,INFILD,FSMSFG,
     I                     DAYFG,VIFWFG,INTFWM,MSUPY,UZSN,UZS,UZRA,
     I                     INTGRL,UZFG,MON,NXTMON,DAY,NDAYS,LSNO,MESSU,
     I                     MSGFL,DATIM,
     M                     INTFW,ECNT1,
     O                     INFIL,UZI,IFWI,PSUR,UZFRAC)
C
C     + + + PURPOSE + + +
C     Distribute the water available for infiltration and runoff -
C     units of fluxes are in./ivl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,FSMSFG,MON,NDAYS,NXTMON,VIFWFG,LSNO,MESSU,
     $           DATIM(5),ECNT1,MSGFL,UZFG
      REAL       IFWI,INFEXP,INFIL,INFILD,INFILT,INFFAC,INTFW,
     $           INTFWM(12),INTGRL(10),LZRAT,MSUPY,UZI,UZRA(10),UZS,
     $           UZSN,PSUR,UZFRAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LZRAT  - ???
C     INFILT - infiltration rate parameter
C     INFEXP - ???
C     INFFAC - infiltration adjustment factor
C     INFILD - ???
C     FSMSFG - ???
C     DAYFG  - flag for first day or day change
C     VIFWFG - ???
C     INTFWM - ???
C     MSUPY  - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     UZRA   - ???
C     INTGRL - ???
C     UZFG   - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     INTFW  - ???
C     ECNT1  - ???
C     INFIL  - computed infiltration
C     UZI    - upper zone inflow
C     IFWI   - interflow inflow
C     DATIM  - date and time of day
C     PSUR   - ???
C     UZFRAC - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       IBAR,IMAX,IMIN,RATIO
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,DSPHWT
C
C     + + + END SPECIFICATIONS + + +
C
C     Establish locations of sloping lines on infiltration/inflow/sur
C     runoff figure.  Prefix "I" refers to "infiltration" line
C     Ibar is the mean infiltration capacity over the segment
C     internal units of INFILT are inches/ivl
      IBAR= INFILT/(LZRAT**INFEXP)
C
      IF (INFFAC .LT. 1.0) THEN
C       adjust ibar to account for frozen ground
        IBAR= IBAR*INFFAC
      END IF
C
C     find the maximum and minimum infiltration capacities
      IMAX= IBAR*INFILD
C     Infild is an input parameter - ratio of maximum to mean
C     infiltration capacity
      IMIN= IBAR- (IMAX- IBAR)
C
      IF (FSMSFG .EQ. 1 .OR. DAYFG .EQ. 1) THEN
C       it is time to recompute any varying parameters
        IF (VIFWFG .EQ. 1) THEN
C         interflow parmeters are allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate intfw between two values from the
C         monthly array intfwm(12)
          INTFW= DAYVAL (INTFWM(MON),INTFWM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow parameter does not vary throughout the year.
C         intfw value has been supplied by the run interpreter
        END IF
      END IF
C
C     Ratio is the ratio of the ordinates of the "infiltration +
C     interflow" line to those of the "infiltration" line
      RATIO= INTFW*(2.0**LZRAT)
      IF (RATIO .LE. 1.0) THEN
        RATIO= 1.0001
      END IF
C
C     determine what happens to the moisture supply
      CALL DSPHWT (IMIN,IMAX,RATIO,MSUPY,UZSN,UZS,UZRA,INTGRL,UZFG,
     I             LSNO,MESSU,MSGFL,DATIM,
     M             ECNT1,
     O             INFIL,UZI,IFWI,PSUR,UZFRAC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SUROHW
     I                    (RTOPFG,FSMSFG,DAYFG,VNNFG,MON,NXTMON,DAY,
     I                     NDAYS,SRRC,SREXP,NSROWS,NSCOLS,SURTAB,NSURM,
     I                     LSUR,SLSUR,PSUR,DELT60,MESSU,MSGFL,LSNO,
     M                     ECNT2,NSUR,DEC,SRC,SURS,
     O                     SURO)
C
C     + + + PURPOSE + + +
C     Simulate surface runoff in high water table conditions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    RTOPFG,FSMSFG,DAYFG,VNNFG,MON,NXTMON,DAY,NDAYS,NSROWS,
     $           NSCOLS,MESSU,MSGFL,LSNO,ECNT2
      REAL       SRRC,SREXP,SURTAB(100),NSURM(12),LSUR,SLSUR,PSUR,
     $           DELT60,NSUR,DEC,SRC,SURS,SURO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RTOPFG - surface routinge option flag
C     FSMSFG - flag indicating if interval is first with surface
C              moisture supply after one or more intervals with none
C     DAYFG  - flag for first day or day change
C     VNNFG  - flag for variable interflow recession constant parm
C     NSURM  - variable interflow recession constant
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     MESSU  - ???
C     MSGFL  - ???
C     LSNO   - ???
C     DELT60 - simulation time interval in hours
C     ECNT2  - ???
C     SRRC   - ???              
C     SREXP  - ???
C     NSUR   - ???
C     LSUR   - ???
C     SLSUR  - ???
C     SURS   - ???
C     SURO   - ???
C     PSUR   - ???
C     DEC    - ???
C     SRC    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       KSRRC
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   SUROFT,DAYVAL,PROUTE
C
C     + + + END SPECIFICATIONS + + +
C
C     user can choose one of original methods or the new
C     power function method; we may put power function in
C     proute at a later time, but for now, leave it as a
C     stand-alone method
      IF (RTOPFG .EQ. 2) THEN
C       power function method
C?!is this correct (should surs=psur), and if so, should next option be same?
        SURS= PSUR  
        KSRRC = -ALOG (SRRC)*DELT60/24.0
        SURO= (SURS**(SREXP))*(1.0- EXP (-KSRRC))
        SURS= SURS- SURO
      ELSE IF (RTOPFG .EQ. 3) THEN
C       ftable method
        SURS= PSUR
        CALL SUROFT (NSROWS,NSCOLS,SURTAB,
     M               SURS,
     O               SURO)
      ELSE
C       use one of original methods
        IF ( (FSMSFG .EQ. 1) .OR. (DAYFG .EQ. 1) ) THEN
C         it is time to recompute any varying parameters
          IF (VNNFG .EQ. 1) THEN
C           mannings n is allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate nsur between two values from the
C           monthly array nsurm(12)
            NSUR= DAYVAL (NSURM(MON),NSURM(NXTMON),DAY,NDAYS)
          ELSE
C           mannings n does not vary throughout the year.
C           nsur value has been supplied by the run interpreter
          END IF
C
C         calculate parameters for routing surface runoff
          DEC= 0.00982*(NSUR*LSUR/SQRT (SLSUR))**0.6
          SRC= 1020.0*(SQRT (SLSUR)/(NSUR*LSUR))
        END IF
C
        CALL PROUTE (PSUR,RTOPFG,DELT60,DEC,SRC,MESSU,MSGFL,LSNO,
     M               SURS,ECNT2,
     O               SURO)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SUROFT
     I                    (NSROWS,NSCOLS,SURTAB,
     M                     SURS,
     O                     SURO)
C
C     + + + PURPOSE + + +
C     Compute surface runoff using ftable method.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NSROWS,NSCOLS
      REAL    SURTAB(100),SURS,SURO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSROWS - ???
C     NSCOLS - ???
C     SURTAB - ???
C     SURS   - ???
C     SURO   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,J,ROWPTR,FRCPTR
      REAL    SFRAC,FRAC1,FRAC2,DEP1,DEP2
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize row pointer
      ROWPTR= 1
C
C     begin do-until loop to find row in ftable
      DO 10 I= 1, NSROWS
        J= NSCOLS*(I- 1)+ 1
        IF (SURS .GE. SURTAB(J)) THEN
C         found row
          ROWPTR= I
        END IF
 10   CONTINUE
C
      FRCPTR= NSCOLS*(ROWPTR- 1)+ 1
C
      IF (ROWPTR .EQ. NSROWS) THEN
C       past top of table - use last row
        SFRAC= SURTAB(FRCPTR+ 1)
      ELSE
C       interpolate between two rows
        DEP1= SURTAB(FRCPTR)
        FRAC1= SURTAB(FRCPTR+ 1)
        DEP2= SURTAB(FRCPTR+ NSCOLS)
        FRAC2= SURTAB(FRCPTR+ NSCOLS+ 1)
C
        SFRAC= FRAC1+ (FRAC2- FRAC1)*((SURS- DEP1)/(DEP2- DEP1))
      END IF
C
      IF (SFRAC .GE. 1.0) THEN
C       all runs off
        SURO= SURS
        SURS= 0.0
      ELSE
        SURO= SFRAC*SURS
        SURS= SURS- SURO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADJGWE
     I                    (DATIM,LSNO,MESSU,MSGFL,PCW,PGW,UPGW,ULGWS,
     I                     LLGWS,UZLIM,LZLIM,SELV,UELV,LELV,IFWSC,
     I                     UZSN,UZCA,LZSN,TGWS0,UPRGN,LOWRGN,UELFAC,
     I                     LELFAC,
     M                     SURS,IFWS,UZS,LZS,AGWS,TGWS,GWEL)
C
C     + + + PURPOSE + + +
C     Adjust gw elevation based on updated storages, etc.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DATIM,LSNO,MESSU,MSGFL
      REAL       PCW,PGW,UPGW,ULGWS,LLGWS,UZLIM,LZLIM,SELV,UELV,LELV,
     $           IFWSC,UZSN,UZCA,LZSN,TGWS0,UELFAC,LELFAC,SURS,IFWS,
     $           UZS,LZS,AGWS,TGWS,GWEL
      LOGICAL    UPRGN,LOWRGN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PCW    - ???
C     PGW    - ???
C     UPGW   - ???
C     ULGWS  - ???
C     LLGWS  - ???
C     UZLIM  - ???
C     LZLIM  - ???
C     SELV   - ???
C     UELV   - ???
C     LELV   - ???
C     IFWSC  - ???
C     UZSN   - ???
C     UZCA   - ???
C     LZSN   - ???
C     TGWS0  - ???
C     UPRGN  - ???
C     LOWRGN - ???
C     UELFAC - ???
C     LELFAC - ???
C     SURS   - ???
C     IFWS   - ???
C     UZS    - ???
C     LZS    - ???
C     AGWS   - ???
C     TGWS   - ???
C     GWEL   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SCLU,SGRP,DONEFG,ECNT
      REAL       NWLZS,NWTGWS,NWAGWS,DSUS,DIFWS,DUZS,DLZS,DGWS,DGWSX,
     $           GWFRAC,NWGWEL,UPPER,SLICE
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    UZGWDR,UZGWRS
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 303
C
      NWLZS= LZS
      NWAGWS= AGWS
      NWTGWS= TGWS
C
      IF (TGWS .LE. LZLIM) THEN
C       below lower zone influence; all the storages are as calculated}
        GWEL= TGWS/(PCW+ PGW)
      ELSE
C       interaction between saturated and unsaturated zones
        IF (TGWS .LT. ULGWS) THEN 
C         between lelv and uelv
C         Determine the change in agws that should be re-distributed:
C         if agws was below influence level at the beginning of the 
C         interval, then only the portion of the increment above llgws
C         should be re-distributed
          IF (TGWS0 .LE. LLGWS) THEN
C           rising gw level crossed over lelv in this time step: 
C           redistribute with lzs
C           the portion to be redistributed with lzs is:
            DGWS= NWTGWS- LLGWS
            DLZS= (1.0- NWLZS/(LELFAC*LZSN))*PCW*(NWTGWS- TGWS0)
            LZS= NWLZS+ DLZS
C           there are no changes in interflow or uzs
            DIFWS= 0.0
            DUZS= 0.0
            AGWS= NWAGWS- DLZS
            TGWS= NWTGWS- DLZS
            GWEL= LELV+ (TGWS- LLGWS)/PGW
          ELSE IF (TGWS0 .GT. ULGWS) THEN  
C           dropping gw level fell below uelv in this time step:
C           part of the original change should come from ifws and uzs
C           negative change
            DGWS= ULGWS- TGWS0
            CALL UZGWDR (DGWS,IFWSC,UPGW,UZCA,UZSN,UELFAC,
     M                   UZS,IFWS,AGWS,TGWS)
C
C           check tolerance to see if gelev should jump to lower region
            IF (TGWS .LT. UZLIM) THEN
C             dropped to lower region
              GWEL= LELV+ (TGWS- LLGWS)/PGW
            ELSE
C             stayed in upper region
              GWEL= UELV+ (IFWS+ UZS)/UPGW
            END IF
          ELSE
C           No change of region in this time step, gw storage was and
C           remains between lelv and uelv
C           However, there may be a pending change;
            DLZS= (1.0- NWLZS/(LELFAC*LZSN))*PCW*(NWTGWS- TGWS0)
            IF (DLZS .LT. 0.0) THEN 
C             cohesion water cannot percolate
              DLZS= 0.0
            END IF
            LZS= NWLZS+ DLZS
            AGWS= NWAGWS- DLZS
            TGWS= NWTGWS- DLZS
            IF (UPRGN .AND. (TGWS. GT. UZLIM)) THEN 
C             gelev is still above uelv
              GWEL= UELV+ (IFWS+ UZS)/UPGW
            ELSE IF (LOWRGN .AND. (TGWS .LT. LZLIM)) THEN
              GWEL= TGWS/(PCW+ PGW)
            ELSE
              GWEL= LELV+ (TGWS- LLGWS)/PGW
            END IF
          END IF
        ELSE
C         agws > ulgws :  ground water above upper influence level
C         distribute changes between all storages;
          DLZS= (1.0- NWLZS/(LELFAC*LZSN))*PCW*(NWTGWS- TGWS0)
          IF (DLZS .LT. 0) THEN 
            DLZS= 0.0
          END IF
          LZS= NWLZS+ DLZS
          AGWS= NWAGWS- DLZS
          TGWS= NWTGWS- DLZS
          IF (TGWS .LT. UZLIM) THEN
C           addition to gw was really not enough to cross uelv
C           it went to lzs; no changes in uzs or ifws
            GWEL= LELV+ (TGWS- LLGWS)/PGW
            DGWS= 0.0
          ELSE
C           still in region 3
            DSUS= 0.0
            IF (TGWS0 .LT. ULGWS) THEN
C             rising gw crossed upper influence level in this time step;
C             portion of change in agws that should be distributed is:
              DGWS= TGWS- ULGWS
            ELSE
C             no change of region in this time step; 
C             However, there may be a pending change
C             lzs was taken care of
              DGWS= TGWS- TGWS0
            END IF
            NWGWEL= UELV+ (IFWS+ UZS+ (TGWS- ULGWS))/UPGW
C
            UPPER= UZS+ IFWS
            SLICE= TGWS- ULGWS
            IF (UPPER .LE. SLICE) THEN
              GWFRAC= 1.0
            ELSE
              GWFRAC= (TGWS- ULGWS)/(UZS+ IFWS)
            END IF
            DONEFG= 0
            IF (DGWS .GT. 0.0) THEN
C             first determine if gw has reached the land surface: in 
C             this case, part should be assigned to surs
              IF ((UZS+ IFWS+ (TGWS- ULGWS)) .GE. 
     $            (UELFAC*UZSN+ IFWSC)) THEN
                DSUS= (UZS+ IFWS+ (TGWS- ULGWS))- (UELFAC*UZSN+ IFWSC)
                SURS= SURS+ DSUS
                IF (DSUS .GT. DGWS) THEN
C                 transfer to surface storage was larger than
C                 original increase in ground water, because
C                 uzs, ifws and tgws were so large; take the water
C                 out of ifws and uzs and tgws according to how
C                 full they are. tgws cannot fall below uzinfl.
                  DGWS= DGWS- DSUS
                  CALL UZGWDR (DGWS,IFWSC,UPGW,UZCA,UZSN,UELFAC,
     M                         UZS,IFWS,AGWS,TGWS)
                  AGWS= AGWS- DSUS
                  TGWS= TGWS- DSUS
                  IF ((TGWS- ULGWS) .LT. 0.001) THEN
C                    WRITE (99,*)'AVOID CHANGE OF REGION'
                    DUZS= TGWS- (ULGWS+ 0.001)
C                   algebraically, duzs must be negative
C                   so agws goes up, and ifws and uzs go down
C
C                   algebraically, the first following line is same as
C                   tgws= tgws- duzs, parallel to agws line just after
C
                    TGWS= ULGWS+ 0.001
                    AGWS= AGWS- DUZS
                    DIFWS= DUZS*(UZCA)
                    IF (ABS (DIFWS) .GE. IFWS) THEN
C                     don't force ifws negative
                      DIFWS= -IFWS
                    END IF
                    IFWS= IFWS+ DIFWS
                    DUZS= DUZS- DIFWS
                    IF (ABS (DUZS) .GE. UZS) THEN
C                     don't force uzs negative
                      DUZS= DUZS+ UZS
                      UZS= 0.0
                      IF (ABS (DUZS) .GE. IFWS) THEN
C                       take all of ifws and rest from gw
                        DUZS= DUZS+ IFWS
                        IFWS= 0.0
                        TGWS= TGWS+ DUZS
                        AGWS= AGWS+ DUZS
                      ELSE
C                       take from ifws only
                        IFWS= IFWS+ DUZS
C                        WRITE(99,*)' avoid negative uzs'
                      END IF
                    ELSE
C                     just take out of upper zone
                      UZS= UZS+ DUZS
                    END IF
                    IF ((UZS .LT. 0.0) .OR. (IFWS .LT. 0.0)) THEN
C                     error - program bug - negative uzs or ifws
                      CALL OMSTD (DATIM)
                      CALL OMSTI (LSNO)
                      CALL OMSTR (UZS)
                      CALL OMSTR (IFWS)
                      SGRP= 21
                      CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                           ECNT)
C                     WRITE (99,*)'ERROR, UZS OR IFWS < 0'
                    END IF
                  END IF
                  DONEFG= 1
                END IF
                IF (DONEFG .EQ. 0) THEN
C                 this is unreachable
                  DGWS= DGWS- DSUS
                  NWGWEL= SELV
                END IF
              END IF
              IF (DONEFG .EQ. 0) THEN
C               else
                DGWSX= DGWS*GWFRAC
                CALL UZGWRS (DGWSX,IFWSC,UPGW,UZCA,UZSN,UELFAC,
     M                       UZS,IFWS,AGWS,TGWS)
                AGWS= AGWS- DSUS
                TGWS= TGWS- DSUS
              END IF
            ELSE IF (DGWS .LT. 0.0) THEN
C             gw falling
              DGWSX= DGWS*(1.0- GWFRAC)
              CALL UZGWDR (DGWSX,IFWSC,UPGW,UZCA,UZSN,UELFAC,
     M                     UZS,IFWS,AGWS,TGWS)
            END IF
C
            NWGWEL= UELV+ (IFWS+ UZS+ (TGWS- ULGWS))/UPGW
C
            IF ((NWGWEL- SELV) .GT. 0.0001) THEN
C             excess agws should go to surface storage
              DGWS= TGWS- (IFWSC+ UELFAC*UZSN)+ (IFWS+ UZS- ULGWS)
              IF (ABS(DGWS) .GT. 0.0001) THEN
C                WRITE (99,*)'still excess water, DGWS:',DGWS
                AGWS= AGWS- DGWS
                TGWS= TGWS- DGWS
                SURS= SURS+ DGWS
                GWEL= SELV
              END IF
            ELSE
              GWEL= NWGWEL
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZGWDR
     I                    (DGWS,IFWSC,UPGW,UZCA,UZSN,UELFAC,
     M                     UZS,IFWS,AGWS,TGWS)
C
C     + + + PURPOSE + + +
C     Distribute a negative change in agws between uzs, ifws and agws.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL  DGWS,IFWSC,UPGW,UZCA,UZSN,UELFAC,UZS,IFWS,AGWS,TGWS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DGWS   - ???
C     IFWSC  - ???
C     UPGW   - ???
C     UZCA   - ???
C     UZSN   - ???
C     UELFAC - ???
C     UZS    - ???
C     IFWS   - ???
C     AGWS   - ???
C     TGWS   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL  DIFWS,DUZS
C
C     + + + END SPECIFICATIONS + + +
C
C     reduction in uzs and ifws should be proportional to how
C     saturated they are
      DUZS= (UZS+ IFWS)/(UELFAC*UZSN+ IFWSC)*UPGW*DGWS
C     reduction in uzs should be inversely proportional to its capacity
      DIFWS= DUZS*UZCA
      IF (IFWS .GT. 0.0) THEN
        IFWS= IFWS + DIFWS
        IF (IFWS .LT. 0.0) THEN
          DIFWS= DIFWS- IFWS
          IFWS= 0.0
        END IF
      ELSE
        DIFWS= 0.0
      END IF
      DUZS= DUZS- DIFWS
      UZS= UZS+ DUZS
      IF (UZS .LT. 0.0) THEN
C       uzs cannot go negative
        DUZS= -UZS
        UZS= 0.0
      END IF
      AGWS= AGWS- DUZS- DIFWS
      TGWS= TGWS- DUZS- DIFWS
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZGWRS
     I                    (DGWS,IFWSC,UPGW,UZCA,UZSN,UELFAC,
     M                     UZS,IFWS,AGWS,TGWS)
C
C     + + + PURPOSE + + +
C     Distribute increase in agws between uzs and ifws.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL  DGWS,IFWSC,UPGW,UZCA,UZSN,UELFAC,UZS,IFWS,AGWS,TGWS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DGWS   - ???
C     IFWSC  - ???
C     UPGW   - ???
C     UZCA   - ???
C     UZSN   - ???
C     UELFAC - ???
C     UZS    - ???
C     IFWS   - ???
C     AGWS   - ???
C     TGWS   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL  DIFWS,DUZS
C
C     + + + END SPECIFICATIONS + + +
C
      DUZS= (1.0- (UZS+ IFWS)/(UELFAC*UZSN+ IFWSC))*UPGW*DGWS
      DIFWS= DUZS*(1.0- UZCA)
      IF (IFWS .LT. IFWSC) THEN
C       add water to interflow storage
        IFWS= IFWS+ DIFWS
        IF (IFWS .GT. IFWSC) THEN
C         excess overflows to active groundwater
cthj          DIFWS= IFWS- IFWSC
          DIFWS= DIFWS- (IFWS- IFWSC)
          IFWS= IFWSC
        END IF
      ELSE
C       water cannot be added to ifws
        DIFWS= 0.0
      END IF
      DUZS= DUZS- DIFWS
      UZS= UZS+ DUZS
      AGWS= AGWS- DUZS- DIFWS
      TGWS= TGWS- DUZS- DIFWS
C 
      RETURN
      END  
C
C
C
      SUBROUTINE   DISPOS
     I                    (IMIN,IMAX,RATIO,MSUPY,UZSN,UZS,DELT60,DEC,
     I                     SRC,UZRA,INTGRL,RTOPFG,UZFG,LSNO,MESSU,MSGFL,
     I                     DATIM,
     M                     SURS,ECNT1,ECNT2,
     O                     INFIL,UZI,IFWI,SURO)
C
C     + + + PURPOSE + + +
C     Dispose of moisture supply on either an individual block of
C     the land segment or on an entire land segment.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECNT1,ECNT2,MSGFL,LSNO,MESSU,RTOPFG,UZFG,DATIM(5)
      REAL       DEC,DELT60,IFWI,IMAX,IMIN,INFIL,INTGRL(10),MSUPY,
     $           RATIO,SRC,SURO,SURS,UZI,UZRA(10),UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMIN   - ???
C     IMAX   - ???
C     RATIO  - ???
C     MSUPY  - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     DELT60 - simulation time interval in hours
C     DEC    - ???
C     SRC    - ???
C     UZRA   - ???
C     INTGRL - ???
C     RTOPFG - ???
C     UZFG   - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     SURS   - ???
C     ECNT1  - ???
C     ECNT2  - ???
C     INFIL  - ???
C     UZI    - ???
C     IFWI   - ???
C     SURO   - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       IIMAX,IIMIN,OVERI,OVERII,PDRO,PIFWI,PSUR,UNDRI,UNDRII,
     $           UZFRAC
C
C     + + + EXTERNALS + + +
      EXTERNAL   DIVISN,UZINF,UZINF2,PROUTE
C
C     + + + END SPECIFICATIONS + + +
C
C     determine how much of the MSUPY falls above and below the
C     infiltration line in the infiltration/interflow/surface runoff
C     figure
      CALL DIVISN (IMIN,IMAX,MSUPY,
     O             OVERI,UNDRI)
C
C     the quantity under I is infiltrated moisture
      INFIL= UNDRI
      IF (OVERI .GT. 0.0) THEN
C       there is some potential interflow inflow and maybe surface
C       detention/outflow -- the sum of these is potential direct
C       runoff
        PDRO= OVERI
C       determine how much of this potential direct runoff will
C       be taken by the upper zone
        IF (UZFG .EQ. 0) THEN
          CALL UZINF (PDRO,UZSN,UZS,UZRA,INTGRL,LSNO,MESSU,MSGFL,DATIM,
     M                ECNT1,
     O                UZI)
        ELSE
          CALL UZINF2 (PDRO,UZSN,UZS,
     O                 UZI)
        END IF
        IF (UZI .GT. PDRO) THEN
          UZI= PDRO
        END IF
        UZFRAC= UZI/PDRO
C
C       determine how much of the msupy falls above and below the
C       "infiltration + interflow" line in the infiltration/
C       interflow/surface runoff figure. the prefix "ii" is used on
C       variables associated with this line
        IIMIN= IMIN*RATIO
        IIMAX= IMAX*RATIO
        CALL DIVISN (IIMIN,IIMAX,MSUPY,
     O               OVERII,UNDRII)
C
C       psur is potential surface detention/runoff
        PSUR= OVERII
C       pifwi is potential interflow inflow
        PIFWI= PDRO- PSUR
        IFWI= PIFWI*(1.0- UZFRAC)
C
        IF (PSUR .GT. 0.0) THEN
C         there will be something on or running off the surface
C         reduce it to account for the upper zone's share
          PSUR= PSUR*(1.0- UZFRAC)
C
C         determine how much of this potential surface detention/
C         outflow will run off in this time interval
          CALL PROUTE (PSUR,RTOPFG,DELT60,DEC,SRC,MESSU,MSGFL,LSNO,
     M                 SURS,ECNT2,
     O                 SURO)
C
        ELSE
C         there is nothing to store on the surface or to run off
          SURS= 0.0
          SURO= 0.0
        END IF
      ELSE
C       there is no potential direct runoff or contribution to the
C       upper zone
        SURS= 0.0
        SURO= 0.0
        IFWI= 0.0
        UZI= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DIVISN
     I                    (MIN,MAX,MSUPY,
     O                     OVER,UNDER)
C
C     + + + PURPOSE + + +
C     Determine the divisions of the quantities of the moisture supply
C     above and below the "infiltration" line or the "infiltration +
C     interflow" line in the infiltration/interflow/surface runoff
C     figure.  This routine is used either to simulate the behavior of
C     an individual block of the land segment or the entire land
C     segment.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       MAX,MIN,MSUPY,OVER,UNDER
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MIN    - ???
C     MAX    - ???
C     MSUPY  - ???
C     OVER   - ???
C     UNDER  - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MSUPY .LE. MIN) THEN
C       msupy line is entirely below other line
        UNDER= MSUPY
        OVER= 0.0
      ELSE
        IF (MSUPY .GT. MAX) THEN
C         msupy line is entirely above other line
          UNDER= (MIN+ MAX)*0.5
          OVER=MSUPY- UNDER
        ELSE
C         msupy line crosses the other line
          OVER= ((MSUPY- MIN)**2)*0.5/(MAX- MIN)
          UNDER= MSUPY- OVER
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETAGW
     I                   (AGWETP,KVARY,
     M                    REMPET,AGWS,TAET,GWVS,
     O                    AGWET)
C
C     + + + PURPOSE + + +
C     Simulate ET from active groundwater storage.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AGWET,AGWETP,AGWS,GWVS,KVARY,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AGWETP - ???
C     KVARY  - ???
C     REMPET - ???
C     AGWS   - ???
C     TAET   - ???
C     GWVS   - ???
C     AGWET  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       GWPET
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     Agwetp is the input parameter governing et from active groundwater
      IF (AGWETP .GT. 0.0) THEN
C       there is et from groundwater
C       determine remaining capacity
        GWPET= REMPET*AGWETP
C
        IF (GWPET .GT. AGWS) THEN
C         groundwater et is limited by quantity available
          AGWET= AGWS
          AGWS= 0.0
        ELSE
C         groundwater et will not exhaust storage, so empty at
C         potential
          AGWET= GWPET
          AGWS= AGWS- AGWET
        END IF
C
C       update variables
        IF ((ABS (KVARY)) .GT. 0.0) THEN
C         update variable storage
          GWVS= GWVS- AGWET
C
C         This is a check added to version 12 to ensure that GWVS
C         does not become negative due to evaporation.  However, in earlier
C         versions, when this check was not present, GWVS sometimes
C         strayed just below zero without causing noticeable problems.
C         In order to avoid changing existing results, a small tolerance
C         is allowed.  If this code receives extensive revamping where
C         small changes in results are expected, the tolerance should
C         be removed.  A similar tolerance is found in subroutine GWATER.
C
          IF (GWVS .LT. -0.02) THEN
C           no remaining wedge storage
C            write (99,*) 'ETAGW: reset GWVS from',GWVS,' to zero'
C            GWVS= 0.0
          END IF          
        END IF
        TAET= TAET+ AGWET
        REMPET= REMPET- AGWET
      ELSE
C       there is no et from groundwater
        AGWET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETBASE
     I                    (BASETP,
     M                     AGWO,REMPET,TAET,
     O                     BASET)
C
C     + + + PURPOSE + + +
C     Simulate ET from baseflow.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AGWO,BASET,BASETP,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BASETP - ???
C     AGWO   - ???
C     REMPET - ???
C     TAET   - ???
C     BASET  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       BASPET
C
C     + + + END SPECIFICATIONS + + +
C
      IF (BASETP .GT. 0.0) THEN
C       there is et from baseflow
        BASPET= BASETP*REMPET
        IF (BASPET .GT. AGWO) THEN
C         baseflow et is limited by quantity available
          BASET= AGWO
          AGWO= 0.0
        ELSE
C         baseflow et will not exhaust storage, so empty at potential
          BASET= BASPET
          AGWO= AGWO- BASET
        END IF
C
C       update totals
        TAET= TAET+ BASET
        REMPET= REMPET- BASET
      ELSE
C       no et from baseflow
        BASET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETLZON
     I                    (DAYFG,VLEFG,MON,DAY,LZETPM,NXTMON,
     I                     NDAYS,LZSN,DELT60,
     M                     LZETP,REMPET,RPARM,LZS,TAET,
     O                     LZET)
C
C     + + + PURPOSE + + +
C     Simulate ET from the lower zone.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VLEFG
      REAL       DELT60,LZET,LZETP,LZETPM(12),LZS,LZSN,REMPET,RPARM,
     $           TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VLEFG  - ???
C     MON    - calendar month
C     DAY    - day of month
C     LZETPM - ???
C     NXTMON - next calendar month
C     NDAYS  - no. of days in this month
C     LZSN   - ???
C     DELT60 - simulation time interval in hours
C     LZETP  - ???
C     REMPET - ???
C     RPARM  - ???
C     LZS    - ???
C     TAET   - ???
C     LZET   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       LZPET,LZRAT
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
CTHJ      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + HISTORY + + +
C     2002  THJ added optional LZET method for IHM; selected with VLEFG = 2,3
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
CTHJ-IHM begin change
        IF ( (VLEFG .EQ. 1) .OR. (VLEFG .EQ. 3) ) THEN
CTHJ-IHM end change
C         lower zone et parameter is allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate lzetp between two values from the
C         monthly array lzetpm(12)
          LZETP= DAYVAL (LZETPM(MON),LZETPM(NXTMON),DAY,NDAYS)
        ELSE
C         lower zone et parameter does not vary throughout the year.
C         lzetp value has been supplied by the run interpreter
        END IF
C
        LZRAT= LZS/LZSN
C
C       it is time to recalculate et opportunity parameter
C       rparm is max et opportunity - inches/ivl
        IF (LZETP .LE. 0.99999) THEN
CTHJ      old case
          RPARM= 0.25/(1.0- LZETP)*LZRAT*DELT60/24.0
        ELSE
CTHJ      new case - dummy value for rparm
          RPARM= 1.0E10
        END IF
      END IF
C
      IF (REMPET .GT. 0.0) THEN
C       there remains an et demand
        IF (LZS .GT. 0.02) THEN
C         assume et can take place
CTHJ          IF (ABS (LZETP- 1.0) .LE. 1.0E-5) THEN
          IF (LZETP .GE. 0.99999) THEN
C           special case - will try to draw et from whole land
C           segment at remaining potential rate
CTHJ            LZPET= REMPET
            LZPET= REMPET*LZETP
          ELSE
CTHJ-IHM begin change
C           lzet depends on et opportunity parameter
            IF (VLEFG .LE. 1) THEN
C             usual case - desired et will vary over the whole land seg
CTHJ-IHM end change
              IF (REMPET .GT. RPARM) THEN
C               potential exceeds opportunity
                LZPET= 0.5*RPARM
              ELSE
C               potential exceeds opportunity over only part of the
C               land segment
                LZPET= REMPET*(1.0- REMPET/(2.0*RPARM))
              END IF
C
              IF (LZETP .LT. 0.5) THEN
C               reduce the et to account for area devoid of vegetation
                LZPET= LZPET*2.0*LZETP
              END IF
CTHJ-IHM begin change
            ELSE IF (VLEFG .GE. 2) THEN
C             et constant over whole land seg
              IF (LZRAT .LT. 1.0) THEN
C               soil moisture below field capacity so et reduced
                LZPET= LZETP*LZRAT*REMPET
              ELSE
C               maximum et
                LZPET= LZETP*REMPET
              END IF
            END IF
CTHJ-IHM end change
          END IF
C
          IF (LZPET .LT. (LZS- 0.02) ) THEN
C           lower zone et will not exhaust storage, so empty at
C           potential
            LZET= LZPET
          ELSE
C           lower zone et is limited by quantity available
            LZET= LZS- 0.02
          END IF
C
C         update variables
          LZS= LZS- LZET
          TAET= TAET+ LZET
          REMPET= REMPET- LZET
        ELSE
C         assume no et can take place
          LZET= 0.0
        END IF
      ELSE
C       no more et demand
        LZET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETUZON
     I                    (UZSN,
     M                     REMPET,UZS,TAET,
     O                     UZET)
C
C     + + + PURPOSE + + +
C     Simulate ET from the upper zone
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       REMPET,TAET,UZET,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UZSN   - upper zone nominal storage
C     REMPET - ???
C     UZS    - initial upper zone storage
C     TAET   - ???
C     UZET   - ???
C
C     + + + EXTERNALS + + +
      EXTERNAL   ETUZS
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ETUZS (UZSN,REMPET,
     M            UZS,
     O            UZET)
C
C     update totals
      TAET= TAET+ UZET
      REMPET= REMPET- UZET
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETUZS
     I                   (UZSN,REMPET,
     M                    UZS,
     O                    UZET)
C
C     + + + PURPOSE + + +
C     This is a subsidiary subroutine for computing upper zone ET.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       REMPET,UZET,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UZSN   - upper zone nominal storage
C     REMPET - ???
C     UZS    - initial upper zone storage
C     UZET   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       UZPET,UZRAT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (UZS .GT. 0.001) THEN
C       there is et from the upper zone
C       estimate the uzet opportunity
        UZRAT= UZS/UZSN
        IF (UZRAT .GT. 2.0) THEN
          UZPET= REMPET
        ELSE
          UZPET= 0.5*UZRAT*REMPET
        END IF
C
C       calculate the actual et
        IF (UZPET .GT. UZS) THEN
C         upper zone et is limited by quantity available
          UZET= UZS
          UZS= 0.0
        ELSE
C         upper zone et will not exhaust storage, so empty at
C         potential
          UZET= UZPET
          UZS= UZS- UZET
        END IF
      ELSE
C       there is no et from upper zone
        UZET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVAPT
     I                   (PET,BASETP,UZSN,AGWETP,KVARY,DAYFG,VLEFG,
     I                    LZETPM,MON,NXTMON,DAY,NDAYS,LZSN,DELT60,
     M                    AGWO,CEPS,UZS,AGWS,GWVS,LZETP,RPARM,LZS,
     O                    REMPET,TAET,BASET,CEPE,UZET,AGWET,LZET)
C
C     + + + PURPOSE + + +
C     Simulate evapotranspiration (ET)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VLEFG
      REAL       AGWET,AGWETP,AGWO,AGWS,BASET,BASETP,CEPE,CEPS,DELT60,
     $           GWVS,KVARY,LZET,LZETP,LZETPM(12),LZS,LZSN,PET,REMPET,
     $           RPARM,TAET,UZET,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PET    - ???
C     BASETP - ???
C     UZSN   - upper zone nominal storage
C     AGWETP - ???
C     KVARY  - ???
C     DAYFG  - flag for first day or day change
C     VLEFG  - ???
C     LZETPM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     LZSN   - ???
C     DELT60 - simulation time interval in hours
C     AGWO   - ???
C     CEPS   - ???
C     UZS    - initial upper zone storage
C     AGWS   - ???
C     GWVS   - ???
C     LZETP  - ???
C     RPARM  - ???
C     LZS    - ???
C     REMPET - ???
C     TAET   - ???
C     BASET  - ???
C     CEPE   - ???
C     UZET   - ???
C     AGWET  - ???
C     LZET   - ???
C
C     + + + EXTERNALS + + +
      EXTERNAL   ETBASE,EVICEP,ETUZON,ETLZON,ETAGW
C
C     + + + END SPECIFICATIONS + + +
C
C     Taet is total actual et - inches/ivl
      TAET= 0.0
C     Rempet is remaining potential et - inches/ivl
      REMPET= PET
      IF (REMPET .GT. 0.0) THEN
C       simulate et from baseflow
        CALL ETBASE (BASETP,
     M               AGWO,REMPET,TAET,
     O               BASET)
C
        IF (REMPET .GT. 0.0) THEN
C         simulate evaporation from interception
          CALL EVICEP
     M               (CEPS,REMPET,TAET,
     O                CEPE)
C
          IF (REMPET .GT. 0.0) THEN
C           simulate et from the upper zone
            CALL ETUZON (UZSN,
     M                   REMPET,UZS,TAET,
     O                   UZET)
     C
            IF (REMPET .GT. 0.0) THEN
C               simulate et from active groundwater
              CALL ETAGW (AGWETP,KVARY,
     M                    REMPET,AGWS,TAET,GWVS,
     O                    AGWET)
            ELSE
              AGWET= 0.0
            END IF
          ELSE
            UZET= 0.0
            AGWET= 0.0
          END IF
        ELSE
          CEPE= 0.0
          UZET= 0.0
          AGWET= 0.0
        END IF
      ELSE
        BASET= 0.0
        CEPE= 0.0
        UZET= 0.0
        AGWET= 0.0
      END IF
C
C     Et from lower zone is handled here because it must be called
C     every interval to make sure that seasonal variation in
C     parameter LZETP and recalculation of RPARM are correctly done
C     simulate ET from the lower zone
      CALL ETLZON (DAYFG,VLEFG,MON,DAY,LZETPM,NXTMON,
     I             NDAYS,LZSN,DELT60,
     M             LZETP,REMPET,RPARM,LZS,TAET,
     O             LZET)
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVICEP
     M                    (CEPS,REMPET,TAET,
     O                     CEPE)
C
C     + + + PURPOSE + + +
C     Simulate evaporation from interception storage.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       CEPE,CEPS,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CEPS   - ???
C     REMPET - ???
C     TAET   - ???
C     CEPE   - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CEPS .GT. 0.0) THEN
C       there is something in interception storage to evaporate
        IF (REMPET .GT. CEPS) THEN
C         evaporation from interception storage is limited
C         by quantity available
          CEPE= CEPS
          CEPS= 0.0
        ELSE
C         interception evaporation will not exhaust storage, so
C         empty at potential
          CEPE= REMPET
          CEPS= CEPS- CEPE
        END IF
C
C       update totals
        TAET= TAET+ CEPE
        REMPET= REMPET- CEPE
      ELSE
C       there is no evaporation from interception storage
        CEPE= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GWATER
     I                    (DEEPFR,GWI,KVARY,DAYFG,KGW,AGWLI,GWIRR,
     M                     AGWS,GWVS,
     O                     IGWI,AGWI,AGWO)
C
C     + + + PURPOSE + + +
C     Simulate groundwater behavior.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAYFG
      REAL       AGWI,AGWLI,GWIRR,AGWO,AGWS,DEEPFR,GWI,GWVS,IGWI,
     $           KGW,KVARY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEEPFR - ???
C     GWI    - ???
C     KVARY  - ???
C     DAYFG  - flag for first day or day change
C     KGW    - ???
C     AGWLI  - ???
C     GWIRR  - ???
C     AGWS   - ???
C     GWVS   - ???
C     IGWI   - ???
C     AGWI   - ???
C     AGWO   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       AINFLO,AVAIL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (GWI .GT. 0.0) THEN
C       groundwater inflow components
        IGWI= DEEPFR*GWI
        AGWI= GWI- IGWI
      ELSE
        IGWI= 0.0
        AGWI= 0.0
      END IF
C
C     active groundwater
C     total inflow includes lateral inflow
      AINFLO= AGWI+ AGWLI+ GWIRR
      AGWO= 0.0
C     evaluate groundwater recharge parameter
      IF ((ABS (KVARY)) .GT. 0.0) THEN
C       update the index to variable groundwater slope
        GWVS= GWVS+ AINFLO
        IF (DAYFG .EQ. 1) THEN
C         cut it back
          IF (GWVS .GT. 0.0001) THEN
            GWVS= GWVS*0.97
          ELSE
            GWVS= 0.0
          END IF
        END IF
C
C       groundwater outflow(baseflow)
        IF (AGWS .GT. 1.0E-20) THEN
C         enough water to have outflow
          AGWO= KGW*(1.0+ KVARY*GWVS)*AGWS
C
C         the following check was added to ensure that AGWS never
C         goes negative because of excess AGWO.  This can happen
C         when KVARY*AINFLO*KGW > AGWS, that is when the inflow
C         is on the order of one thousand times larger than the
C         starting storage.  Later below, AGWS is checked again
C         in case AGWO=AVAIL, but numerical roundoff causes a
C         slightly negative AGWS.
C
          AVAIL= AINFLO+ AGWS
          IF (AGWO .GT. AVAIL) THEN
C           all flows out
C            write (99,*) 'GWATER: reduced AGWO',AGWO,' to AVAIL',AVAIL
            AGWO= AVAIL
          END IF
C         end check
        END IF
      ELSE
        IF (AGWS .GT. 1.0E-20) THEN
C         enough water to have outflow
          AGWO= KGW*AGWS
        END IF
      END IF
C
      AGWS= AGWS+ (AINFLO- AGWO)
C
C     This is the second part of the check on AGWS to ensure that it
C     remains non-negative.
      IF (AGWS .LT. 0.0) THEN
C       no remaining water - this should happen only with HWTFG=1
C       it may happen from lateral inflows, which is a bug, in
C       which case negative values for AGWS should show up in
C       the output timeseries
C        write (99,*) 'GWATER: reset AGWS',AGWS,' to zero'
        AGWS= 0.0
      END IF
C
C     This is another check added to version 12 to ensure that GWVS
C     does not exceed AGWS. Without it, if AGWLI is large (from a
C     large area draining through a narrow border strip, for example),
C     then GWVS can blow up and cause problems.  However, in earlier
C     versions, when this check was not present, GWVS sometimes
C     strayed just above AGWS without causing noticeable problems.
C     In order to avoid changing existing results, a small tolerance
C     is allowed.  If this code receives extensive revamping where
C     small changes in results are expected, the tolerance should
C     be removed.  A similar tolerance is found in subroutine ETAGW.
      IF (ABS (KVARY) .GT. 0.0) THEN
C       check variable storage
        IF (GWVS .GT. (AGWS+0.0)) THEN
C         reduce variable storage
C         write(99,*)'GWATER: reduced GWVS',GWVS,' to AGWS',AGWS,GWVS-AGWS
C          GWVS= AGWS
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ICEPT
     I                   (VCSFG,DAYFG,CEPSCM,MON,NXTMON,DAY,NDAYS,SUPY,
     I                    IRRCEP,
     M                    CEPSC,CEPS,
     O                    CEPO)
C
C     + + + PURPOSE + + +
C     Simulate the interception of moisture by vegetal or other
C     ground cover.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    VCSFG,DAYFG,MON,NXTMON,DAY,NDAYS
      REAL       CEPSCM(12),SUPY,IRRCEP,CEPSC,CEPS,CEPO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VCSFG  - ???
C     DAYFG  - flag for first day or day change
C     CEPSCM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     SUPY   - ???
C     IRRCEP - irrigation application subject to interception
C     CEPSC  - ???
C     CEPS   - ???
C     CEPO   - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VCSFG .EQ. 1) THEN
C         interception capacity allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate cepsc between two values from the
C         monthly array cepscm(12)
          CEPSC= DAYVAL (CEPSCM(MON),CEPSCM(NXTMON),DAY,NDAYS)
        ELSE
C         interception capacity does not vary throughout the year
C         cepsc value has been supplied by the run interpreter
        END IF
      END IF
C
C     add to interception storage
      CEPS= CEPS+ SUPY+ IRRCEP
      IF (CEPS .GT. CEPSC) THEN
C       there is outflow from interception storage
        CEPO= CEPS- CEPSC
        CEPS= CEPSC
      ELSE
        CEPO= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTFLW
     I                    (DAYFG,VIRCFG,IRCM,MON,NXTMON,DAY,NDAYS,
     I                     DELT60,IFWI,IFWLI,
     M                     IRC,IFWK1,IFWK2,IFWS,UZS,
     O                     IFWO)
C
C     + + + PURPOSE + + +
C     Simulate interflow.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VIRCFG
      REAL       DELT60,IFWI,IFWK1,IFWK2,IFWLI,IFWO,
     $           IFWS,IRC,IRCM(12),UZS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VIRCFG - ???
C     IRCM   - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     DELT60 - simulation time interval in hours
C     IFWI   - ???
C     IFWLI  - ???
C     IRC    - ???
C     IFWK1  - ???
C     IFWK2  - ???
C     IFWS   - ???
C     UZS    - initial upper zone storage
C     IFWO   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY,INFLO,KIFW,VALUE
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG,EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VIRCFG .EQ. 1) THEN
C         interflow recession constant is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate irc between two values from the
C         monthly array ircm(12)
          IRC= DAYVAL (IRCM(MON),IRCM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow recession constant does not vary throughout the
C         year.  irc value has been supplied by the run interpreter
        END IF
C
C       derive parameters used in routing
        DUMMY= 24.0/DELT60
        KIFW= -ALOG (IRC)/DUMMY
        IFWK2= 1.0- EXP (-KIFW)
        IFWK1= 1.0- (IFWK2/KIFW)
      END IF
C
C     surface and near-surface zones of the land segment have not
C     been subdivided into blocks
      INFLO= IFWI+ IFWLI
      VALUE= INFLO+ IFWS
      IF (VALUE .GT. 0.00002) THEN
C       there is something worth routing
        IFWO= (IFWK1*INFLO)+ (IFWK2*IFWS)
        IFWS= VALUE- IFWO
      ELSE
C       nothing worth routing-dump back to uzs
        IFWO= 0.0
        IFWS= 0.0
        UZS= UZS+ VALUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LZONE
     I                   (IPERC,LZIRR,LZRAT,IFRDFG,
     M                    LZFRAC,LZS,RLZRAT,
     O                    LZI)
C
C     + + + PURPOSE + + +
C     Simulate lower zone behavior.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IFRDFG
      REAL       IPERC,LZIRR,LZFRAC,LZI,LZRAT,LZS,RLZRAT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPERC  - ???
C     LZIRR  - ???
C     LZRAT  - ???
C     IFRDFG - selects IHM infiltration redistribution (optional method)
C     LZFRAC - ???
C     LZS    - ???
C     RLZRAT - ???
C     LZI    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       INDX,LPERC,EXFACT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + HISTORY + + +
C     5/2004  brb added optional infiltration distribution method for IHM; selected with IFRDFG > 0
C
C     + + + END SPECIFICATIONS + + +
C
      LPERC= IPERC+ LZIRR
      IF (LPERC .GT. 0.0) THEN
C       if necessary, recalculate the fraction of infiltration plus
C       percolation which will be taken by lower zone
        IF (ABS (LZRAT- RLZRAT) .GT. 0.02 .OR. IFRDFG .NE. 0) THEN
C         it is time to recalculate
          RLZRAT= LZRAT
          IF (LZRAT .LE. 1.0) THEN
            IF (IFRDFG .EQ. 0) THEN
C             standard method
              INDX= 2.5- 1.5*LZRAT
              LZFRAC= 1.0- LZRAT*(1.0/(1.0+ INDX))**INDX
            ELSE
C             IHM method
              LZFRAC= 1.
            END IF  
          ELSE
            IF (IFRDFG .EQ. 0) THEN
C             standard method
              INDX= 1.5*LZRAT- 0.5
              LZFRAC= (1.0/(1.0+ INDX))**INDX
            ELSE
C             IHM method
              EXFACT= -1.0*REAL(IFRDFG)
              LZFRAC= EXP(EXFACT*(LZRAT-1.0))
            END IF
          END IF
        ELSE
C         keep the old value of lzfrac
        END IF
C
C       lower zone inflow
        LZI= LZFRAC*LPERC
        LZS= LZS+ LZI
      ELSE
C       no inflow
        LZI= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PROUTE
     I                    (PSUR,RTOPFG,DELT60,DEC,SRC,MESSU,MSGFL,LSNO,
     M                     SURS,ECNT2,
     O                     SURO)
C
C     + + + PURPOSE + + +
C     Determine how much potential surface detention (PSUR) runs
C     off in one simulation interval.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    RTOPFG,MESSU,MSGFL,LSNO,ECNT2
      REAL       DEC,DELT60,PSUR,SRC,SURO,SURS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PSUR   - ???
C     RTOPFG - ???
C     DELT60 - simulation time interval in hours
C     DEC    - ???
C     SRC    - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     LSNO   - line number in the opn sequence block of uci
C     ECNT2  - ???
C     SURS   - ???
C     SURO   - surface output
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    COUNT,SGRP,SCLU
      REAL       A1,CHANGE,DFSURO,DSURO,DUMMY,FACT,FSURO,RATIO,SSUPR,
     $           DTERM,STERM,SURSE,SURSM,SURSNW,TSURO,FFACT,DFACT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTI,OMSTR
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 303
C
      IF (PSUR .GT. 0.0002) THEN
C       something is worth routing on the surface
        IF (RTOPFG .NE. 1) THEN
C         do routing the new way
C         estimate the rate of supply to the overland flow surface -
C         inches/hour
          SSUPR= (PSUR- SURS)/DELT60
C         determine equilibrium depth for this supply rate
          SURSE= 0.0
          IF (SSUPR .GT. 0.0) THEN
            SURSE= DEC*SSUPR**0.6
          END IF
C         determine runoff by iteration - newton's method
C         estimate the new surface storage
          SURSNW= PSUR
          SURO= 0.0
          COUNT= 0
C         dountil relative error is small
 10       CONTINUE
            IF (SSUPR .GT. 0.0) THEN
              RATIO= SURSNW/SURSE
              IF (RATIO .LE. 1.0) THEN
C               flow is increasing
                FACT= 1.0+ 0.6*RATIO**3
              ELSE
                FACT= 1.6
              END IF
            ELSE
C             ratio is arbitrarily large for supply rate <= 0
              RATIO= 1.0E30
              FACT= 1.6
            END IF
C
C           coefficient in outflow equation
            A1= DELT60*SRC*FACT**1.667
            STERM= SURSNW**1.667
            COUNT= COUNT+ 1
            FFACT= A1*STERM
            FSURO= FFACT- SURO
            DFACT= -1.667*FFACT
            DFSURO= DFACT/SURSNW- 1.0
            IF (RATIO .LE. 1.0) THEN
C             additional term required in derivative wrt suro
              DTERM=DFACT/(FACT*SURSE)*1.8*RATIO**2
              DFSURO= DFSURO+ DTERM
            END IF
            DSURO= FSURO/DFSURO
C
            IF (COUNT .GT. 100) THEN
C             error message -- didn't converge
              CALL OMSTI (LSNO)
              CALL OMSTR (SURSE)
              CALL OMSTR (SURO)
              CALL OMSTR (SURSNW)
              CALL OMSTR (FSURO)
              CALL OMSTR (DFSURO)
              CALL OMSTR (DSURO)
              SGRP= 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECNT2)
            END IF
C
            SURO= SURO- DSURO
C           boundary condition- don't let suro go negative
            IF (SURO .LE. 1.0E-10) THEN
              SURO= 0.0
            END IF
            SURSNW= PSUR- SURO
            CHANGE= 0.0
C
            IF ((ABS (SURO)) .GT. 0.0) THEN
              CHANGE= ABS (DSURO/SURO)
            END IF
C
          IF (CHANGE .GE. 0.01) GO TO 10
C         enddo
          SURS= SURSNW
        ELSE
C         do routing the way it is done in arm, nps, and hspx
C         estimate the rate of supply to the overland flow surface -
C         inches/ivl
          SSUPR= PSUR- SURS
C         estimate the mean surface detention storage over the
C         interval
          SURSM= (SURS+ PSUR)*0.5
C         estimate the equilibrium detention depth for this supply
C         rate - surse
          IF (SSUPR .GT. 0.0) THEN
C           preliminary estimate of surse
            DUMMY= DEC*SSUPR**0.6
            IF (DUMMY .GT. SURSM) THEN
C             flow is increasing
              SURSE= DUMMY
              DUMMY= SURSM*(1.0+ 0.6*(SURSM/SURSE)**3)
            ELSE
C             flow on surface is at equilibrium or receding
              DUMMY= SURSM*1.6
            END IF
          ELSE
C           flow on the surface is receding - equilibrium detention is
C           assumed equal to actual detention
            DUMMY= SURSM*1.6
          END IF
C
          TSURO= DELT60*SRC*DUMMY**1.667
C
C         check the temporary calculation of surface outflow
          IF (TSURO .GT. PSUR) THEN
C           too much surface runoff is estimated
            SURO= PSUR
            SURS= 0.0
          ELSE
            SURO= TSURO
            SURS= PSUR- SURO
          END IF
        END IF
      ELSE
C       send what is on the overland flow plane straight to the
C       channel
        SURO= PSUR
        SURS= 0.0
      END IF
C
      IF (SURO .LE. 1.0E-10) THEN
C       fix bug in on PC - underflow leads to "not a number"
        SURO= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SURFAC
     I                    (LZRAT,INFILT,INFEXP,INFFAC,INFILD,FSMSFG,
     I                     DAYFG,VNNFG,NSURM,LSUR,SLSUR,VIFWFG,INTFWM,
     I                     MSUPY,UZSN,UZS,DELT60,UZRA,INTGRL,RTOPFG,
     I                     UZFG,MON,NXTMON,DAY,NDAYS,LSNO,MESSU,MSGFL,
     I                     DATIM,
     M                     DEC,SRC,NSUR,INTFW,SURS,ECNT1,ECNT2,
     O                     INFIL,UZI,IFWI,SURO)
C
C     + + + PURPOSE + + +
C     Distribute the water available for infiltration and runoff -
C     units of fluxes are in./ivl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,FSMSFG,MON,NDAYS,NXTMON,RTOPFG,VIFWFG,VNNFG,
     $           LSNO,MESSU,DATIM(5),ECNT1,ECNT2,MSGFL,UZFG
      REAL       DEC,DELT60,IFWI,INFEXP,INFIL,
     $           INFILD,INFILT,INFFAC,INTFW,INTFWM(12),INTGRL(10),
     $           LSUR,LZRAT,MSUPY,NSUR,NSURM(12),SLSUR,SRC,SURO,SURS,
     $           UZI,UZRA(10),UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LZRAT  - ???
C     INFILT - ???
C     INFEXP - ???
C     INFFAC - ???
C     INFILD - ???
C     FSMSFG - ???
C     DAYFG  - flag for first day or day change
C     VNNFG  - ???
C     NSURM  - ???
C     LSUR   - ???
C     SLSUR  - ???
C     VIFWFG - ???
C     INTFWM - ???
C     MSUPY  - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     DELT60 - simulation time interval in hours
C     UZRA   - ???
C     INTGRL - ???
C     RTOPFG - ???
C     UZFG   - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     DEC    - ???
C     SRC    - ???
C     NSUR   - ???
C     INTFW  - ???
C     SURS   - ???
C     ECNT1  - ???
C     ECNT2  - ???
C     INFIL  - ???
C     UZI    - ???
C     IFWI   - ???
C     SURO   - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       IBAR,IMAX,IMIN,RATIO
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INRINSICS + + +
      INTRINSIC  SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,DISPOS
C
C     + + + END SPECIFICATIONS + + +
C
C     Establish locations of sloping lines on infiltration/inflow/sur
C     runoff figure.  Prefix "I" refers to "infiltration" line
C     Ibar is the mean infiltration capacity over the segment
C     internal units of INFILT are inches/ivl
      IBAR= INFILT/(LZRAT**INFEXP)
C
      IF (INFFAC .LT. 1.0) THEN
C       adjust ibar to account for frozen ground
        IBAR= IBAR*INFFAC
      END IF
C
C     find the maximum and minimum infiltration capacities
      IMAX= IBAR*INFILD
C     Infild is an input parameter - ratio of maximum to mean
C     infiltration capacity
      IMIN= IBAR- (IMAX- IBAR)
C
      IF ( (FSMSFG .EQ. 1) .OR. (DAYFG .EQ. 1) ) THEN
C       it is time to recompute any varying parameters
        IF (VNNFG .EQ. 1) THEN
C         mannings n is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate nsur between two values from the
C         monthly array nsurm(12)
          NSUR= DAYVAL (NSURM(MON),NSURM(NXTMON),DAY,NDAYS)
        ELSE
C         mannings n does not vary throughout the year.
C         nsur value has been supplied by the run interpreter
        END IF
C
C       calculate parameters for routing surface runoff
        DEC= 0.00982*(NSUR*LSUR/SQRT (SLSUR))**0.6
        SRC= 1020.0*(SQRT (SLSUR)/(NSUR*LSUR))
C
        IF (VIFWFG .EQ. 1) THEN
C         interflow parmeters are allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate intfw between two values from the
C         monthly array intfwm(12)
          INTFW= DAYVAL (INTFWM(MON),INTFWM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow parameter does not vary throughout the year.
C         intfw value has been supplied by the run interpreter
        END IF
      END IF
C
C     Ratio is the ratio of the ordinates of the "infiltration +
C     interflow" line to those of the "infiltration" line
      RATIO= INTFW*(2.0**LZRAT)
      IF (RATIO .LE. 1.0) THEN
        RATIO= 1.0001
      END IF
C
C     now allocate the water among the alternative destinations
C     surface and near-surface zones of the land-segment have
C     not been subdivided into blocks
C     determine what happens to the moisture supply
      CALL DISPOS (IMIN,IMAX,RATIO,MSUPY,UZSN,UZS,DELT60,DEC,SRC,
     I             UZRA,INTGRL,RTOPFG,UZFG,LSNO,MESSU,MSGFL,DATIM,
     M             SURS,ECNT1,ECNT2,
     O             INFIL,UZI,IFWI,SURO)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZINF
     I                   (PDRO,UZSN,UZS,UZRA,INTGRL,LSNO,MESSU,MSGFL,
     I                    DATIM,
     M                    ECNT1,
     O                    UZI)
C
C     + + + PURPOSE + + +
C     Compute the inflow to the upper zone during this time interval.
C     Do this using a table look-up to handle the non-analytic integral
C     given in supporting documentation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECNT1,MSGFL,LSNO,MESSU,DATIM(5)
      REAL       INTGRL(10),PDRO,UZI,UZRA(10),UZS,UZSN
C
C     + + + ARGUMENT DEFINTIONS + + +
C     PDRO   - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     UZRA   - ???
C     INTGRL - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ECNT1  - ???
C     UZI    - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,SCLU,SGRP
      REAL       INTGA,INTGB,INTG1,INTG2,UZRAA,UZRAB,UZR1,UZR2
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTR,OMSG,OMSTI,OMSTD
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 303
C     find the value of the integral at initial UZRA
      UZRAA= UZS/UZSN
C     in FORTRAN do not use an ordinary do loop, since the I counter
C     is needed following the loop
C     dountil
      I= 0
 10   CONTINUE
        I= I+ 1
        UZR1= UZRA(I)
        UZR2= UZRA(I+ 1)
      IF ( (UZR2 .LT. UZRAA) .AND. (I .NE. 9) )  GO TO 10
C     enddo
C
      IF (UZR2 .LT. UZRAA) THEN
C       say that the solution could not be found
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (UZR1)
        CALL OMSTR (UZR2)
        CALL OMSTR (UZRAA)
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT1)
      END IF
C
C     do linear interpolation
      INTGA= INTGRL(I)+ (UZRAA- UZR1)/(UZR2- UZR1)*(INTGRL(I+ 1)-
     $       INTGRL(I))
C
C     now find the value of the integral at the end of the time step
      INTGB= INTGA+ PDRO/UZSN
C
C     find the value of UZRA after inflow is admitted to the upper
C     zone - reverse the table look-up process
C     start with same I just used
C     in FORTRAN do not use an ordinary do loop, since the I counter
C     is needed following the loop
C     dountil
      I= 0
 20   CONTINUE
        I= I+ 1
        INTG1= INTGRL(I)
        INTG2= INTGRL(I+ 1)
      IF ( (INTG2 .LT. INTGB) .AND. (I .NE. 9) )  GO TO 20
C     enddo
C
      IF (INTG2 .LT. INTGB) THEN
C       say that the solution could not be found
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (INTG1)
        CALL OMSTR (INTG2)
        CALL OMSTR (INTGB)
        SGRP= 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT1)
      END IF
C
C     do linear interpolation
      UZRAB= UZRA(I)+ (INTGB- INTG1)/(INTG2- INTG1)*
     $       (UZRA(I+ 1)- UZRA(I))
C
C     now it is known how much the inflow will change UZRA -
C     translate this to a quantity of inflow, inches/ivl
      UZI= (UZRAB- UZRAA)*UZSN
      IF (UZI .LT. 0.0) THEN
C       negative inflow shouldn't happen, but does for
C       extremely small PDRO
C        IF (UZI .LT. -1.0E-3) THEN
C          error - @@@ warn about bad value
C          WRITE (99,*) 'UZINF: UZI highly negative -',UZI
C        END IF
        UZI= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZINF2
     I                    (PDRO,UZSN,UZS,
     O                     UZI)
C
C     + + + PURPOSE + + +
C     Compute inflow to upper zone during this interval, using
C     "fully forward" type algorithm  as used in HSPX,ARM and NPS.
C     Note:  although this method should give results closer to those
C     produced by HSPX, etc., its output will be more sensitive to
C     Delt than that given by subroutine uzinf
C
C     + + + DUMMY ARGUMENTS + + +
      REAL    PDRO,UZSN,UZS,UZI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PDRO   - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     UZI    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL    UZRAT,K1,K2,UZFRAC
C
C     + + + END SPECIFICATIONS + + +
C
      UZRAT= UZS/UZSN
C
      IF (UZRAT .LT. 2.0) THEN
        K1= 3.0- UZRAT
        UZFRAC= 1.0- (UZRAT*0.5)*((1.0/(1.0+ K1))**K1)
      ELSE
C
        K2= (2.0*UZRAT)- 3.0
        UZFRAC= (1.0/(1.0+ K2))**K2
      END IF
C
      UZI= PDRO*UZFRAC
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZONE
     I                   (UZSN,UZI,UZLI,UZIRR,INFILT,INFFAC,LZRAT,
     M                    UZS,
     O                    PERC)
C
C     + + + PURPOSE + + +
C     Simulate upper zone behavior.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       INFFAC,INFILT,LZRAT,PERC,UZI,UZLI,UZIRR,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UZSN   - upper zone nominal storage
C     UZI    - ???
C     UZLI   - lateral inflow into upper zone
C     UZIRR  - irrigation application into upper zone
C     INFILT - ???
C     INFFAC - ???
C     LZRAT  - ???
C     UZS    - initial upper zone storage
C     PERC   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       UZRAT
C
C     + + + END SPECIFICATIONS + + +
C
C     percolation will be based on UZRAT at the start of the interval
      UZRAT= UZS/UZSN
C
C     add inflow to UZS
      UZS= UZS+ UZI+ UZLI+ UZIRR
C
C     percolation
      IF ((UZRAT- LZRAT) .GT. 0.01) THEN
C       simulate percolation
C       units of perc are inches/ivl
        PERC= 0.1*INFILT*INFFAC*UZSN*(UZRAT- LZRAT)**3
C
        IF (PERC .GT. UZS) THEN
C         computed value is too high so merely empty storage
          PERC= UZS
          UZS= 0.0
        ELSE
C         computed value is ok
          UZS= UZS- PERC
        END IF
      ELSE
C       assume there is no percolation
        PERC= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWAACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section PWATER.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE  'cplpw.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2,I3,I5,I6,I8
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I3= 3
      I5= 5
      I6= 6
      I8= 8
C     handle flux groups containing segment-wide variables
C
      IF ( (SLIFP .GT. 0) .OR. (ULIFP .GT. 0) .OR. (ILIFP .GT. 0) .OR.
     $     (LLIFP .GT. 0) .OR.(ALIFP .GT. 0) ) THEN
C       lateral input fluxes are being considered and printed
        CALL ACCVEC (I5,PWIF(1,FRMROW),
     M               PWIF(1,TOROW))
      END IF
C
      CALL ACCVEC (I6,PWCF1(1,FRMROW),
     M             PWCF1(1,TOROW))
C
      CALL ACCVEC (I8,PWCF3(1,FRMROW),
     M             PWCF3(1,TOROW))
C
      CALL ACCVEC (I6,PWCF5(1,FRMROW),
     M             PWCF5(1,TOROW))
C
      CALL ACCVEC (I2,PWCF7(1,FRMROW),
     M             PWCF7(1,TOROW))
C
      IF (IRRGFG .GE. 1) THEN
C       irrigation turned on
        PIRIF(TOROW)= PIRIF(TOROW) + PIRIF(FRMROW)
        CALL ACCVEC (I2,IRRCF1(1,FRMROW),
     M               IRRCF1(1,TOROW))
        CALL ACCVEC (I3,IRRCF2(1,FRMROW),
     M               IRRCF2(1,TOROW))
        CALL ACCVEC (I6,IRRCF3(1,FRMROW),
     M               IRRCF3(1,TOROW))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWAPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     water balance, and print out results.  The arrays PSTAT1 through
C     Pstat3, piflx, and pcflx1 through pcflx7 have identical
C     structures to PWST1 through PWST3, PWIF, and PWCF1 through
C     Pwcf7 apart from dropping the dimension lev for fluxes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UNITFG,LEV,PRINTU,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE   'cplpw.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,I0,I1,I2,I3,I5,I6,I8,I9,ACNT,CLEN(56),EXDAT(5)
      REAL      DFACTA,DFACTB,PCFLX1(6),PCFLX3(8),PCFLX5(6),
     $          PCFLX7(2),PIFLX(5),PPERS,PPERSS,PSTAT1(9),
     $          PIRFX1(2),PIRFX2(3),PIRFX3(6),PRZWS,APRINT(56)
      CHARACTER*8    UNITID,CSTAT1(9),CWST3(2),CCFLX1(6),
     $               CIFLX(5),CCFLX3(8),CCFLX5(6)
      CHARACTER*24   CIRFX1(2),CIRFX2(3),CIRFX3(6)
      CHARACTER*256  CHEAD(56)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PWATER ***')
 2010 FORMAT (/,'   STATE VARIABLES',13X,
     $  '      PERS      CEPS      SURS       UZS      IFWS       LZS',
     $        '      AGWS      GWVS    INFFAC    PETADJ')
 2015 FORMAT (/,'   STATE VARIABLES',13X,
     $  '    PERS    CEPS    SURS     UZS    IFWS     LZS',
     $        '    AGWS    GWVS    TGWS    GWEL  INFFAC  PETADJ')
 2020 FORMAT (/,'   STATE VARIABLES',13X,
     $  '      PERS      CEPS      SURS       UZS      IFWS       LZS',
     $        '      AGWS      GWVS')
 2025 FORMAT (/,'   STATE VARIABLES',13X,
     $  '    PERS    CEPS    SURS     UZS    IFWS     LZS',
     $        '    AGWS    GWVS    TGWS    GWEL')
 2030 FORMAT (31X,8(8X,'IN'))
 2035 FORMAT (31X,9(6X,'IN'),6X,'FT')
 2040 FORMAT (31X,8(8X,'MM'))
 2045 FORMAT (31X,9(6X,'MM'),7X,'M')
 2050 FORMAT (31X,11F10.3)
 2055 FORMAT (31X,12F8.3)
 2060 FORMAT (/,37X,'RZWS')
 2065 FORMAT (39X,'IN')
 2070 FORMAT (39X,'MM')
 2075 FORMAT (31X,F10.3)
 2080 FORMAT (/,'   FLUXES')
 2090 FORMAT ('     EXTNL INFLOWS & OUTFLOWS  ',
     $  '  MOISTURE<-----OUTFLOWS TO STREAM---------->(SUM) DEEP PERC',
     $        '<----------------LATERAL INFLOWS----------------->')
 2100 FORMAT (31X,'      SUPY      SURO      IFWO      AGWO',
     $        '      PERO      IGWI     SURLI      UZLI     IFWLI',
     $        '      LZLI     AGWLI')
 2110 FORMAT (31X,11(8X,'IN'))
 2120 FORMAT (31X,11(8X,'MM'))
 2130 FORMAT (31X,11F10.3)
 2140 FORMAT ('     EXTNL INFLOWS & OUTFLOWS  ',
     $  '  MOISTURE<-----OUTFLOWS TO STREAM---------->(SUM) DEEP PERC')
 2150 FORMAT (31X,'      SUPY      SURO      IFWO      AGWO',
     $        '      PERO      IGWI')
 2160 FORMAT (31X,6(8X,'IN'))
 2170 FORMAT (31X,6(8X,'MM'))
 2180 FORMAT (31X,6F10.3)
 2200 FORMAT (/,'     EVAPOTRANSPIRATION         POTENTIAL<---------',
     $        '-----------ET COMPONENTS-------------------->(SUM)')
 2210 FORMAT (31X,'       PET      CEPE      UZET      LZET',
     $        '     AGWET     BASET      TAET')
 2212 FORMAT (/,'     EVAPOTRANSPIRATION         POTENTIAL<---------',
     $        '----------------ET COMPONENTS------------------------',
     $        '->(SUM)')
 2215 FORMAT (31X,'       PET      CEPE      SURET     UZET      LZET',
     $        '     AGWET     BASET      TAET')
 2220 FORMAT (31X,7(8X,'IN'))
 2225 FORMAT (31X,8(8X,'IN'))
 2230 FORMAT (31X,7(8X,'MM'))
 2235 FORMAT (31X,8(8X,'MM'))
 2240 FORMAT (31X,8F10.3)
 2260 FORMAT (/,'     INTERNAL FLUXES',11X,
     $  '      IFWI       UZI     INFIL      PERC       LZI      AGWI')
 2270 FORMAT (31X,6(8X,'IN'))
 2280 FORMAT (31X,6(8X,'MM'))
 2290 FORMAT (31X,6F10.3)
 2300 FORMAT (/,'     IRRIGATION FLUXES',7X,
     $          '     IRRIG   IRRIG<----WITHDRAWALS------->',
     $          '<----------------APPLICATIONS------------------>')
 2305 FORMAT (31X,'  DEMAND   SHORT  EXTERN  ACT GW  RCHRES',
     $          '  CANOPY    SURF   UPPER   LOWER     AGW   TOTAL')
 2310 FORMAT (31X,11(6X,'IN'))
 2320 FORMAT (31X,11(6X,'MM'))
 2330 FORMAT (31X,11F8.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I3= 3
      I5= 5
      I6= 6
      I8= 8
      I9= 9
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT1(1) = 'CEPS'
      CSTAT1(2) = 'SURS'
      CSTAT1(3) = 'UZS'
      CSTAT1(4) = 'IFWS'
      CSTAT1(5) = 'LZS'
      CSTAT1(6) = 'AGWS'
      CSTAT1(7) = 'GWVS'
      CSTAT1(8) = 'TGWS'
      CSTAT1(9) = 'GWEL'
      CWST3(1)  = 'INFFAC'
      CWST3(2)  = 'PETADJ'
      CCFLX1(1) = 'SUPY'
      CCFLX1(2) = 'SURO'
      CCFLX1(3) = 'IFWO'
      CCFLX1(4) = 'AGWO'
      CCFLX1(5) = 'PERO'
      CCFLX1(6) = 'IGWI'
      CIFLX(1)  = 'SURLI'
      CIFLX(2)  = 'UZLI'
      CIFLX(3)  = 'IFWLI'
      CIFLX(4)  = 'LZLI'
      CIFLX(5)  = 'AGWLI'
      CCFLX3(1) = 'PET'
      CCFLX3(2) = 'CEPE'
      CCFLX3(3) = 'SURET'
      CCFLX3(4) = 'UZET'
      CCFLX3(5) = 'LZET'
      CCFLX3(6) = 'AGWET'
      CCFLX3(7) = 'BASET'
      CCFLX3(8) = 'TAET'
      CCFLX5(1) = 'IFWI'
      CCFLX5(2) = 'UZI'
      CCFLX5(3) = 'INFIL'
      CCFLX5(4) = 'PERC'
      CCFLX5(5) = 'LZI'
      CCFLX5(6) = 'AGWI'
      CIRFX1(1) = 'IRRDEM'
      CIRFX1(2) = 'IRSHRT'
      CIRFX2(1) = 'IRDRAW1' 
      CIRFX2(2) = 'IRDRAW2' 
      CIRFX2(3) = 'IRDRAW3' 
      CIRFX3(1) = 'IRRAPP1' 
      CIRFX3(2) = 'IRRAPP2' 
      CIRFX3(3) = 'IRRAPP3' 
      CIRFX3(4) = 'IRRAPP4' 
      CIRFX3(5) = 'IRRAPP5' 
      CIRFX3(6) = 'IRRAPP6' 
C
C     dimensionless variables do not need to be converted
C     assign conversion constant for dimensional variables with
C     depth units
      IF (UNITFG .EQ. 1) THEN
C       english units
        DFACTA= 1.0
      ELSE
C       metric units
        DFACTA= 25.4
      END IF
C
      DFACTB= 0.0
C
C     convert dimensional variables to external units
C
C     segment-wide state variables
      CALL TRNVEC (I9,PWST1,DFACTA,DFACTB,
     O             PSTAT1)
      IF (HWTFG .EQ. 1) THEN
C       using high water table algorithms
        PSTAT1(9)= PSTAT1(9)+ GWDATM
        IF (UNITFG .EQ. 1) THEN
C         english units
          PSTAT1(9)= PSTAT1(9)/12.0
        ELSE
C         metric units
          PSTAT1(9)= PSTAT1(9)/39.4
        END IF
      END IF
C
      IF (IRRGFG .GE. 1) THEN
C       irrigation considered
        PRZWS= RZWS*DFACTA
      END IF
C
      PPERS= PWST4(1)*DFACTA
      PPERSS= PWST4(LEV)*DFACTA
C
      IF ( (SLIFP .GT. 0) .OR. (ULIFP .GT. 0) .OR. (ILIFP .GT. 0) .OR.
     $     (LLIFP .GT. 0) .OR.(ALIFP .GT. 0) ) THEN
C       lateral inflows are being handled
        CALL TRNVEC (I5,PWIF(1,LEV),DFACTA,DFACTB,
     O               PIFLX)
      END IF
C
C     computed fluxes
      CALL TRNVEC (I6,PWCF1(1,LEV),DFACTA,DFACTB,
     O             PCFLX1)
C
      CALL TRNVEC (I8,PWCF3(1,LEV),DFACTA,DFACTB,
     O             PCFLX3)
C
      CALL TRNVEC (I6,PWCF5(1,LEV),DFACTA,DFACTB,
     O             PCFLX5)
C
      CALL TRNVEC (I2,PWCF7(1,LEV),DFACTA,DFACTB,
     O             PCFLX7)
C
      IF (IRRGFG .GE. 1) THEN
C       irrigation computed fluxes
        CALL TRNVEC (I2,IRRCF1(1,LEV),DFACTA,DFACTB,
     O               PIRFX1)
        CALL TRNVEC (I3,IRRCF2(1,LEV),DFACTA,DFACTB,
     O               PIRFX2)
        CALL TRNVEC (I6,IRRCF3(1,LEV),DFACTA,DFACTB,
     O               PIRFX3)
      END IF
C
C     write to unit PRINTU
C
      IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
        IF (CSNOFG .EQ. 1) THEN
C         snow is being considered
          IF (HWTFG .EQ. 0) THEN
C           not using high water table algorithms
            WRITE (PRINTU,2010)
          ELSE
C           using high water table algorithms
            WRITE (PRINTU,2015)
          END IF
        ELSE
C         snow is not being considered
          IF (HWTFG .EQ. 0) THEN
C           not using high water table algorithms
            WRITE (PRINTU,2020)
          ELSE
C           using high water table algorithms
            WRITE (PRINTU,2025)
          END IF
        END IF
C
        IF (UNITFG .EQ. 1) THEN
C         english units
          IF (HWTFG .EQ. 0) THEN
C           not using high water table algorithms
            WRITE (PRINTU,2030)
          ELSE
C           using high water table algorithms
            WRITE (PRINTU,2035)
          END IF
        ELSE
C         metric units
          IF (HWTFG .EQ. 0) THEN
C           not using high water table algorithms
            WRITE (PRINTU,2040)
          ELSE
C           using high water table algorithms
            WRITE (PRINTU,2045)
          END IF
        END IF
      END IF
C
      IF (CSNOFG .EQ. 1) THEN
C       snow is being considered
        IF (HWTFG .EQ. 1) THEN
C         using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2055)  PPERS, PSTAT1, PWST3
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C           compile values for binary printout
            ACNT = ACNT + 1
            APRINT(ACNT) = PPERS
            CHEAD(ACNT) = 'PERS'
            CLEN(ACNT) = 4
            DO 10 I = 1, 9
              ACNT = ACNT + 1
              APRINT(ACNT) = PSTAT1(I)
              CHEAD(ACNT) = CSTAT1(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10         CONTINUE
            DO 11 I = 1, 2
              ACNT = ACNT + 1
              APRINT(ACNT) = PWST3(I)
              CHEAD(ACNT) = CWST3(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 11         CONTINUE
          END IF
        ELSE
C         not using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2050)  PPERS, (PSTAT1(I),I=1,7), PWST3
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C           compile values for binary printout
            ACNT = ACNT + 1
            APRINT(ACNT) = PPERS
            CHEAD(ACNT) = 'PERS'
            CLEN(ACNT) = 4
            DO 20 I = 1, 7
              ACNT = ACNT + 1
              APRINT(ACNT) = PSTAT1(I)
              CHEAD(ACNT) = CSTAT1(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20         CONTINUE
            DO 21 I = 1, 2
              ACNT = ACNT + 1
              APRINT(ACNT) = PWST3(I)
              CHEAD(ACNT) = CWST3(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 21         CONTINUE
          END IF
        END IF
      ELSE
C       snow is not being considered
        IF (HWTFG .EQ. 1) THEN
C         using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2055)  PPERS, PSTAT1
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C           compile values for binary printout
            ACNT = ACNT + 1
            APRINT(ACNT) = PPERS
            CHEAD(ACNT) = 'PERS'
            CLEN(ACNT) = 4
            DO 30 I = 1, 9
              ACNT = ACNT + 1
              APRINT(ACNT) = PSTAT1(I)
              CHEAD(ACNT) = CSTAT1(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30         CONTINUE
          END IF
        ELSE
C         not using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2050)  PPERS, (PSTAT1(I),I=1,7)
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C           compile values for binary printout
            ACNT = ACNT + 1
            APRINT(ACNT) = PPERS
            CHEAD(ACNT) = 'PERS'
            CLEN(ACNT) = 4
            DO 31 I = 1, 7
              ACNT = ACNT + 1
              APRINT(ACNT) = PSTAT1(I)
              CHEAD(ACNT) = CSTAT1(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 31         CONTINUE
          END IF
        END IF
      END IF
C
      IF (IRRGFG .EQ. 2) THEN
C      irrigation demand based on root zone water storage
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2060)
        END IF
        IF (UNITFG .EQ. 1) THEN
C         english units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2065)
          END IF
        ELSE
C         metric units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2070)
          END IF
        END IF
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2075) PRZWS
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for binary printout
          ACNT = ACNT + 1
          APRINT(ACNT) = PRZWS
          CHEAD(ACNT) = 'RZWS'
          CLEN(ACNT) = 4
        END IF
      END IF
C
C     fluxes
      IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
        WRITE (PRINTU,2080)
      END IF
C
C     external inflows & outflows
      IF ( (SLIFP .GT. 0) .OR. (ULIFP .GT. 0) .OR. (ILIFP .GT. 0) .OR.
     $     (LLIFP .GT. 0) .OR.(ALIFP .GT. 0) ) THEN
C       lateral inflow is considered
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2090)
          WRITE (PRINTU,2100)
        END IF
C
        IF (UNITFG .EQ. 1) THEN
C         english units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2110)
          END IF
        ELSE
C         metric units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2120)
          END IF
        END IF
C
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2130)  PCFLX1,PIFLX
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for binary printout
          DO 40 I = 1, 6
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
          DO 41 I = 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX(I)
            CHEAD(ACNT) = CIFLX(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 41       CONTINUE
        END IF
      ELSE
C       no lateral inflow considered
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2140)
          WRITE (PRINTU,2150)
        END IF
C
        IF (UNITFG .EQ. 1) THEN
C         english units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2160)
          END IF
        ELSE
C         metric units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2170)
          END IF
        END IF
C
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2180)  PCFLX1
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for binary printout
          DO 50 I = 1, 6
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50       CONTINUE
        END IF
      END IF
C
C     evapotranspiration printout
C
      IF (HWTFG .EQ. 0) THEN
C       not using high water table algorithms
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2200)
          WRITE (PRINTU,2210)
        END IF
      ELSE
C       using high water table algorithms
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2212)
          WRITE (PRINTU,2215)
        END IF
      END IF
C
      IF (UNITFG .EQ. 1) THEN
C       english units
        IF (HWTFG .EQ. 0) THEN
C         not using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2220)
          END IF
        ELSE
C         using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2225)
          END IF
        END IF
      ELSE
C       metric units
        IF (HWTFG .EQ. 0) THEN
C         not using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2230)
          END IF
        ELSE
C         using high water table algorithms
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2235)
          END IF
        END IF
      END IF
C
      IF (HWTFG .EQ. 0) THEN
C       not using high water table algorithms
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2240)  (PCFLX3(I), I= 1,2),(PCFLX3(I), I= 4,8)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for binary printout
          DO 60 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 60       CONTINUE
          DO 61 I = 4, 8
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 61       CONTINUE
        END IF
      ELSE
C       using high water table algorithms
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2240)  PCFLX3
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for binary printout
          DO 70 I = 1, 8
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 70       CONTINUE
        END IF
      END IF
C
C     internal fluxes
      IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
        WRITE (PRINTU,2260)
      END IF
C
      IF (UNITFG .EQ. 1) THEN
C       english units
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2270)
        END IF
      ELSE
C       metric units
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2280)
        END IF
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
        WRITE (PRINTU,2290)  PCFLX5
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C       compile values for binary printout
        DO 80 I = 1, 6
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX5(I)
          CHEAD(ACNT) = CCFLX5(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 80     CONTINUE
      END IF
C
      IF (IRRGFG .GE. 1) THEN
C       irrigation considered
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2300)
          WRITE (PRINTU,2305)
        END IF
        IF (UNITFG .EQ. 1) THEN
C         english units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2310)
          END IF
        ELSE
C         metric units
          IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
            WRITE (PRINTU,2320)
          END IF
        END IF
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2330) PIRFX1,PIRFX2,PIRFX3
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for binary printout
          DO 90 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PIRFX1(I)
            CHEAD(ACNT) = CIRFX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 90       CONTINUE
          DO 91 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PIRFX2(I)
            CHEAD(ACNT) = CIRFX2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 91       CONTINUE
          DO 92 I = 1, 6
            ACNT = ACNT + 1
            APRINT(ACNT) = PIRFX3(I)
            CHEAD(ACNT) = CIRFX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 92       CONTINUE
        END IF
      END IF
C
C     water balance check and report
      IF (UNITFG .EQ. 1) THEN
C       english units
        UNITID= '  INCHES'
      ELSE
C       metric units
        UNITID= '      MM'
      END IF
C
      I= 1
      CALL BALCHK (I,LSNO,DATIM,MESSU,PRINTU,MSGFL,PPERSS,PPERS,
     I             PCFLX7(1),PCFLX7(2),UNITID,I1,
     M             PWWCNT(1))
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(3) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'PWATER  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         WRITE (99,*) 'Writing Header PERLND ',LSNO,'PWATER',BFLAG(3)
C         set bflag to negative to not write headers anymore
          BFLAG(3) = -BFLAG(3)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'PWATER  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWARST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section PWATER.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE    'cplpw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I3,I5,I6,I8
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I3= 3
      I5= 5
      I6= 6
      I8= 8
C     handle flux groups containing
C
      CALL SETVEC (I5,0.0,
     O             PWIF(1,LEV))
C
      CALL SETVEC (I6,0.0,
     O             PWCF1(1,LEV))
C
      CALL SETVEC (I8,0.0,
     O             PWCF3(1,LEV))
C
      CALL SETVEC (I6,0.0,
     O             PWCF5(1,LEV))
C
      CALL SETVEC (I2,0.0,
     O             PWCF7(1,LEV))
C
      PIRIF(LEV)= 0.0
C
      CALL SETVEC (I2,0.0,
     O             IRRCF1(1,LEV))
      CALL SETVEC (I3,0.0,
     O             IRRCF2(1,LEV))
      CALL SETVEC (I6,0.0,
     O             IRRCF3(1,LEV))
C
C     keep present water storage in state variable used for
C     material balance check
      PWST4(LEV)= PWST4(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWATPB
C
C     + + + PURPOSE + + +
C     Handle section PWATER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE    'cplpw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RPMFP .GE. 1) THEN
        PAD(RPMFP+ IVL1)= RPARM
      END IF
C
      IF (SUPYFP .GE. 1) THEN
        PAD(SUPYFP+ IVL1)= SUPY
      END IF
C
      IF (SURIFP .GE. 1) THEN
        PAD(SURIFP+ IVL1)= SURI
      END IF
C
      IF (INFFP .GE. 1) THEN
        PAD(INFFP+ IVL1)= INFIL
      END IF
C
      IF (UZIFP .GE. 1) THEN
        PAD(UZIFP+ IVL1)= UZI
      END IF
C
      IF (IIFP .GE. 1) THEN
        PAD(IIFP+ IVL1)= IFWI
      END IF
C
      IF (SOFP .GE. 1) THEN
        PAD(SOFP+ IVL1)= SURO
      END IF
C
      IF (IOFP .GE. 1) THEN
        PAD(IOFP+ IVL1)= IFWO
      END IF
C
      IF (PCFP .GE. 1) THEN
        PAD(PCFP+ IVL1)= PERC
      END IF
C
      IF (LZIFP .GE. 1) THEN
        PAD(LZIFP+ IVL1)= LZI
      END IF
C
      IF (AIFP .GE. 1) THEN
        PAD(AIFP+ IVL1)= AGWI
      END IF
C
      IF (IGIFP .GE. 1) THEN
        PAD(IGIFP+ IVL1)= IGWI
      END IF
C
      IF (AOFP .GE. 1) THEN
        PAD(AOFP+ IVL1)= AGWO
      END IF
C
      IF (POFP .GE. 1) THEN
        PAD(POFP+ IVL1)= PERO
      END IF
C
      IF (PETFP .GE. 1) THEN
        PAD(PETFP+ IVL1)= PET
      END IF
C
      IF (TAETFP .GE. 1) THEN
        PAD(TAETFP+ IVL1)= TAET
      END IF
C
      IF (BASEFP .GE. 1) THEN
        PAD(BASEFP+ IVL1)= BASET
      END IF
C
      IF (CEFP .GE. 1) THEN
        PAD(CEFP+ IVL1)= CEPE
      END IF
C
      IF (SEFP .GE. 1) THEN
        PAD(SEFP + IVL1)= SURET
      END IF
C
      IF (UEFP .GE. 1) THEN
        PAD(UEFP+ IVL1)= UZET
      END IF
C
      IF (AEFP .GE. 1) THEN
        PAD(AEFP+ IVL1)= AGWET
      END IF
C
      IF (LZETFP .GE. 1) THEN
        PAD(LZETFP+ IVL1)= LZET
      END IF
C
      IF (IRDMFP .GE. 1) THEN
        PAD(IRDMFP+ IVL1)= IRRDEM
      END IF
C
      IF (IRSHFP .GE. 1) THEN
        PAD(IRSHFP+ IVL1)= IRSHRT
      END IF
C
      DO 10 I= 1, 3
        IF (IRDRFP(I) .GE. 1) THEN
          PAD(IRDRFP(I)+ IVL1)= IRDRAW(I)
        END IF
 10   CONTINUE
C
      DO 20 I= 1, 6
        IF (IRAPFP(I) .GE. 1) THEN
          PAD(IRAPFP(I)+ IVL1)= IRRAPP(I)
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWATPT
C
C     + + + PURPOSE + + +
C     Handle section PWATER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE   'cplpw.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CEPSFP .GE. 1) THEN
        PAD(CEPSFP+ IVL1)= CEPS
      END IF
C
      IF (UZSFP .GE. 1) THEN
        PAD(UZSFP+ IVL1)= UZS
      END IF
C
      IF (LZSFP .GE. 1) THEN
        PAD(LZSFP+ IVL1)= LZS
      END IF
C
      IF (SSFP .GE. 1) THEN
        PAD(SSFP+ IVL1)= SURS
      END IF
C
      IF (ISFP .GE. 1) THEN
        PAD(ISFP+ IVL1)= IFWS
      END IF
C
      IF (AGWSFP .GE. 1) THEN
        PAD(AGWSFP+ IVL1)= AGWS
      END IF
C
      IF (TGWSFP .GE. 1) THEN
        PAD(TGWSFP+ IVL1)= TGWS
      END IF
C
      IF (GWELFP .GE. 1) THEN
        PAD(GWELFP+ IVL1)= GWEL+ GWDATM
      END IF
C
      IF (PERSFP .GE. 1) THEN
        PAD(PERSFP+ IVL1)= PERS
      END IF
C
      IF (GWVSFP .GE. 1) THEN
        PAD(GWVSFP+ IVL1)= GWVS
      END IF
C
      IF (INFCFP .GE. 1) THEN
        PAD(INFCFP+ IVL1)= INFFAC
      END IF
C
      IF (PETAFP .GE. 1) THEN
        PAD(PETAFP+ IVL1)= PETADJ
      END IF
C
      IF (RZWSFP .GE. 1) THEN
        PAD(RZWSFP+ IVL1)= RZWS
      END IF
C
      RETURN
      END
