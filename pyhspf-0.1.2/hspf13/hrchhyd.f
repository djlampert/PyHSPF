C
C
C
      SUBROUTINE   PHYDR
C
C     + + + PURPOSE + + +
C     Process the input to the hydr section of the rchres module
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR1 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,N,IX,IC,IG,FG,I1,I2,YY,MM,DD,FTBDSN,FTABNO,ROWADD,
     $             RETCOD,SCLU,SGRP,TBNO,TBSB,NVAL,ICAT,IVAL(MXCUSR),
     $             OECNT,CINIT(MXCAT),CATPOS(MXCUSR),BGRP,LEVST,LEVND
      REAL         AFACT,PSAREA,RVAL(MXCUSR*6),VFACT,RTMP,PCVOL,SUMFRC,
     $             EQVOL
      DOUBLE PRECISION DTMP
      CHARACTER*4  CTAG4
      CHARACTER*12 CPRIOR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CPRIOR,CPRIO1),(CTAG1,CTAG4)
      CHARACTER*1  CPRIO1(12),CTAG1(4)
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,IFIX,FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI,ITABLE,OMSG,RTABLE,FTABLE,FTBWDM,DAYVAL,TAGVAL
      EXTERNAL   OMSTC,ZIPD,ZIPR,OMSTI,OMSTR,CATSRT,DISCH,FNDROW,AUXIL
      EXTERNAL   SHEAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I2/1,2/,BGRP/12/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (F12.0)
 1010 FORMAT (1X,I4,2(1X,I2),1X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION HYDR')
 2010 FORMAT (A4)
 2020 FORMAT (/,' INITIAL STORAGES BY CATEGORY',/,' CATEGORY NAME   ',
     $          '          CVOL')
 2030 FORMAT (25X,'AC-FT')
 2040 FORMAT (27X,'MM3')
 2050 FORMAT (  1X,A16,2X,1PE10.3)
 2060 FORMAT (  ' ALL CATEGORIES',4X,1PE10.3)
 2070 FORMAT (3A4)
 2080 FORMAT (/,' AUXILIARY STATE VARIABLES - GROUP 1',
     $        /,'        DEP     STAGE     SAREA     ',
     $          'AVDEP      TWID      HRAD')
 2090 FORMAT (  '         FT        FT     ACRES     ',
     $          '   FT        FT        FT')
 2100 FORMAT (  '          M         M        HA     ',
     $          '    M         M         M')
 2110 FORMAT (  ' ',1P6E10.3)
 2120 FORMAT (/,' AUXILIARY STATE VARIABLES - GROUP 2',
     $        /,'      AVVEL    AVSECT')
 2130 FORMAT (  '     FT/SEC       FT2')
 2140 FORMAT (  '      M/SEC        M2')
 2150 FORMAT (  ' ',1P2E10.3)
 2160 FORMAT (/,' AUXILIARY STATE VARIABLES - GROUP 3',
     $        /,'      USTAR       TAU')
 2170 FORMAT (  '     FT/SEC    LB/FT2')
 2180 FORMAT (  '      M/SEC     KG/M2')
 2190 FORMAT (  ' ',1P2E10.3)
 2200 FORMAT (/,' AUXILIARY VARIABLES NOT PROCESSED DUE TO',
     $          ' ERROR IN FTABLE')
 2210 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION HYDR')
C
C     + + + HISTORY + + +
C     12/11/2003   JTL   corrected metric GAM from 1000. to 9806.
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV .GT. 1)  THEN
C       processing hydr message
        WRITE (MESSU,2000)
      END IF
C     cluster containing error and warning message details
      SCLU= 341
C
C     warning message counter initialization
      I= 2
      N= 0
      CALL ZIPI (I,N,
     O           HYWCNT)
C
C     error message counter initialization
      I= 6
      N= 0
      CALL ZIPI (I,N,
     O           HYECNT)
C
      IF (NCAT .GE. 1) THEN
C       there are categories in category block - place in osv
        NCATS= NCAT
      END IF
C
C     process values in table-type HYDR-PARM1
      TBNO= 5
      TBSB= 1
      NVAL= 19
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             HYPM1(1))
C
C     check for consistency among "auxiliary flags"
      IF ( ( (AUX2FG .EQ. 1) .AND. (AUX1FG .EQ. 0) ) .OR.
     $     ( (AUX3FG .EQ. 1) .AND. (AUX2FG .EQ. 0) ) ) THEN
        SGRP= 10
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     compute consequential data
      NODFV= 0
      NODGT= 0
      DO 10 IX= 1, NEXITS
        IF (ODFVFG(IX) .NE. 0) THEN
C         outflow a function of volume
          NODFV= NODFV+ 1
        END IF
C
        IF (ODGTFG(IX) .NE. 0) THEN
C         outflow a function of time
          NODGT= NODGT+ 1
        END IF
 10   CONTINUE
C
C     process values in table-type HYDR-PARM2
C     two of the values (FTBDSN and FTABNO) are actually integer,
C     but are read as real values with the other real values
      TBNO= 6
      TBSB= 1
      NVAL= 7
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL(1))
      FTBDSN= RVAL(1)
      FTABNO= RVAL(2)
      LEN=    RVAL(3)
      DELTH=  RVAL(4)
      STCOR=  RVAL(5)
      KS=     RVAL(6)
      DB50=   RVAL(7)
      SLOPE=  DELTH/LEN
C     von karmen constant
      AKAPPA= 0.4
C
C     factors for conversion between external & internal units and
C     constants whose values depend on unit system
      IF (UUNITS .EQ. 1) THEN
C       english system
C       acres to ft2
        AFACT= 43560.0
C       acre-ft to ft3
        VFACT= AFACT
C       unit weight of water
        GAM= 62.4
C       gravitational acceleration
        GRAV= 32.2
      ELSE
C       metric system
C       ha to m2
        AFACT= 10000.0
C       mm3 to m3
        VFACT= 1.0E06
C       unit weight of water
CJTL    corrected metric GAM 12/11/2003 from 1000.
C        GAM=1000.
        GAM= 9806.
C       gravitational acceleration
        GRAV= 9.81
      END IF
C
C     process the ftable
      IF (FTBDSN .EQ. 0) THEN
C       ftable from uci
        CALL FTABLE (FTABNO,AFACT,VFACT,
     O               NROWS,NCOLS,RCHTAB,VZERPT,RETCOD)
      ELSE
C       ftable from wdm file
        CALL FTBWDM (FTABNO,FTBDSN,AFACT,VFACT,UUNITS,
     O               NROWS,NCOLS,RCHTAB,VZERPT,RETCOD)
      END IF
C
C     initialize other variables not controlled by user
      ROWPT= VZERPT
C
C     compute other consequential information
      COKS= 1.0- KS
      FACTA1= 1.0/(COKS*DELTS)
C
      IF (VCONFG .EQ. 1) THEN
C       f(vol) adjustment factors vary through the year
C       get values from table-type MON-CONVF
        TBNO= 7
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               CONVFM(1))
C       value for first day of run
        CONVF= DAYVAL (CONVFM(MON),CONVFM(NXTMON),DAY,NDAYS)
      ELSE
        CONVF= 1.0
      END IF
C
C     process values in table-type HYDR-IRRIG
      TBNO= 8
      TBSB= 1
      NVAL= 2
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL(1))
      IREXIT= RVAL(1)
      IRMINV= RVAL(2)
C
      IF (IREXIT .GE. 1) THEN
C       check that no demand flags are set
        IF ( (ODFVFG(IREXIT) .NE. 0) .OR.
     $       (ODGTFG(IREXIT) .NE. 0) ) THEN
C         error - invalid demand flag
          CALL OMSTI (IREXIT)
          SGRP= 30
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     initialize irrigation variables
      RIRWDL= 0.0
      RIRDEM= 0.0
      RIRSHT= 0.0
C
C     process values in table-type HYDR-INIT
      TBNO= 9
      TBSB= 1
      NVAL= 12
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL(1))
      VOL= RVAL(1)
C
      IF (NCATS .LE. 0) THEN
C       no categories active
        ICAT= 0
      ELSE
C       parse category for initial storage
        WRITE (CTAG4,2010) RVAL(2)
        I= 1
        OECNT= ECOUNT
        CALL TAGVAL (CTAG4(3:4),I,MESSU,MSGFL,SCLU,BGRP,
     M               ECOUNT,
     O               ICAT)
        IF (OECNT .GT. ECOUNT) THEN
C         error - tag was unrecognized
          SGRP= 31
          CALL OMSTC (I2,CTAG1(3))
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
      DO 20 IX= 1, NEXITS
        COLIND(IX)= RVAL(IX+ 2)
        IF (NCATS .LE. 0) THEN
C         no categories
          OUTDGT(IX)= RVAL(IX+ 7)
        ELSE
C         zero outdgt for adding category demands later
          OUTDGT(IX)= 0.0
        END IF
 20   CONTINUE
C
C     initialize category variables
      DTMP= 0.0
      CALL ZIPD (MXCAT,DTMP,
     O           CVOL)
      N= MXCAT*MXEXIT
      RTMP= 0.0
      CALL ZIPR (N,RTMP,
     O           CDFVOL)
      CALL ZIPR (N,RTMP,
     O           COTDGT)
C
C     category processing
C
      IF (NCATS .GE. 1) THEN
C       read category-specific tables
C
C       process values in table-type HYDR-CATEGORY
        TBNO= 10
        TBSB= 1
        NVAL= 8
        CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M               IVAL)
        DO 30 I= 1, NVAL- 1
          WRITE (CTAG4,2010) IVAL(I)
          OECNT= ECOUNT
          CALL TAGVAL (CTAG4(3:4),I1,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 CATEG(I))
          IF (OECNT .GT. ECOUNT) THEN
C           error - tag was unrecognized
            SGRP= 32
            CALL OMSTC (I2,CTAG1(3))
            CALL OMSTI (I)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
 30     CONTINUE
        NCOGT= IVAL(8)
C
C       distribute initial volume
C       
        IF (ICAT .GT. 0) THEN
C         user specified a category for all initial storage
          CVOL(ICAT)= VOL
        ELSE IF (ICAT .LT. 0) THEN
C         process values in table-type HYDR-CINIT
          TBNO= 11
          TBSB= 1
          NVAL= -ICAT* 2
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL(1))
          SUMFRC= 0.0
          DO 40 I= 1, -ICAT
            WRITE (CTAG4,2010) RVAL(I*2- 1)
            OECNT= ECOUNT
            CALL TAGVAL (CTAG4(3:4),I1,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   CINIT(I))
            IF (OECNT .GT. ECOUNT) THEN
C             error - tag was unrecognized
              SGRP= 33
              CALL OMSTC (I2,CTAG1(3))
              CALL OMSTI (I)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
            SUMFRC= SUMFRC+ RVAL(I*2)
            CVOL(CINIT(I))= VOL*RVAL(I*2)
 40       CONTINUE
          IF (SUMFRC .LE. 0.0) THEN
C           divide equally among all listed categories
            EQVOL= -VOL/(FLOAT (ICAT))
            DO 50 I= 1, -ICAT
              CVOL(IC)= EQVOL
 50         CONTINUE
          ELSE IF (ABS (SUMFRC- 1.0) .GT. 1.0E-5) THEN
C           error - fractions do not sum to one or zero
            SGRP= 34
            CALL OMSTR (SUMFRC)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        ELSE
C         assume equal volumes
          EQVOL= VOL/(FLOAT (NCATS))
          DO 60 IC= 1, NCATS
            CVOL(IC)= EQVOL
 60       CONTINUE
        END IF
C
        IF (CPREC .LT. 0) THEN
C         process values in table-type HYDR-CPREC
          TBNO= 12
          TBSB= 1
          NVAL= -CPREC* 2
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL(1))
          SUMFRC= 0.0
          DO 70 I= 1, -CPREC
            WRITE (CTAG4,2010) RVAL(I*2- 1)
            OECNT= ECOUNT
            CALL TAGVAL (CTAG4(3:4),I1,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   CPRECC(I))
            IF (OECNT .GT. ECOUNT) THEN
C             error - tag was unrecognized
              SGRP= 35
              CALL OMSTC (I2,CTAG1(3))
              CALL OMSTI (I)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
            CPRECF(I)= RVAL(I*2)
            SUMFRC= SUMFRC+ RVAL(I*2)
 70       CONTINUE
          IF ( (SUMFRC .GT. 0.0) .AND.
     $         (ABS (SUMFRC- 1.0) .GT. 1.0E-5) ) THEN
C           error - fractions do not sum to one or zero
            SGRP= 36
            CALL OMSTR (SUMFRC)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
C
        IF (CEVAP .LT. 0) THEN
C         process values in table-type HYDR-CEVAP
          TBNO= 13
          TBSB= 1
          NVAL= -CEVAP* 3
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL(1))
          DO 80 I= 1, -CEVAP
            WRITE (CTAG4,2010) RVAL(I*3- 2)
            OECNT= ECOUNT
            CALL TAGVAL (CTAG4(3:4),I1,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   CEVAPC(I))
            IF (OECNT .GT. ECOUNT) THEN
C             error - tag was unrecognized
              SGRP= 37
              CALL OMSTC (I2,CTAG1(3))
              CALL OMSTI (I)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
            CEVAPP(I)= RVAL(I*3- 1)
            CEVAPF(I)= RVAL(I*3)
 80       CONTINUE
C
C         sort by priority
          N= -CEVAP
          I= 1
          CALL CATSRT (N,I,
     M                 CEVAPC,CEVAPP,CEVAPF,
     O                 CATPOS)
C
C         check for fractions summing to unity or zero
C
C         begin do-until loop on levels
          LEVND= 0
 90       CONTINUE
C           look for end of priority level
            SUMFRC= 0.0
            I= LEVND
            LEVST= LEVND+ 1
 100        CONTINUE
              I= I+ 1
              SUMFRC= SUMFRC+ CEVAPF(I)
              IF (I .GE. N) THEN
C               end of all levels
                LEVND= I
              ELSE
C               check if next is same level
                IF (CEVAPP(I+ 1) .NE. CEVAPP(I)) THEN
C                 next is not same level
                  LEVND= I
                END IF
              END IF
            IF (LEVST .GT. LEVND) GO TO 100
C
            IF ( (SUMFRC .GT. 0.0) .AND.
     $           (ABS (SUMFRC- 1.0) .GT. 1.0E-5) ) THEN
C             error - fractions do not sum to one or zero
              SGRP= 38
              CALL OMSTR (SUMFRC)
              CALL OMSTI (CEVAPP(LEVND))
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
C
C         end of loop on levels
          IF (LEVND .LT. N) GO TO 90
        END IF
C
        N= 0
        DO 110 IX= 1, NEXITS
          IF (CFVOL(IX) .LT. 0) THEN
C           this exit uses hydr-cfvol
            N= N- CFVOL(IX)
          END IF
 110    CONTINUE
        IF (N .GT. 0) THEN
C         process values in table-type HYDR-CFVOL
          TBNO= 14
          TBSB= 1
          NVAL= N*4
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL(1))
          DO 120 I= 1, N
            IX= RVAL(I*4- 2)
            WRITE (CTAG4,2010) RVAL(I*4- 3)
            OECNT= ECOUNT
            CALL TAGVAL (CTAG4(3:4),I1,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   CFVOLC(I,IX))
            IF (OECNT .GT. ECOUNT) THEN
C             error - tag was unrecognized
              SGRP= 39
              CALL OMSTC (I2,CTAG1(3))
              CALL OMSTI (I)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
            CFVOLP(I,IX)= IFIX (RVAL(I*4- 1)+ 0.1)
            CFVOLF(I,IX)= RVAL(I*4)
 120      CONTINUE
C
C         sort by priority
          DO 150 IX= 1, NEXITS
            N= -CFVOL(IX)
            IF (N .GT. 0) THEN
C             this exit has specified fvol categories
              I= 1
              CALL CATSRT (N,I,
     M                     CFVOLC(1,IX),CFVOLP(1,IX),CFVOLF(1,IX),
     O                     CATPOS)
C
C             check for fractions summing to unity or zero
C
C             begin do-until loop on levels
              LEVND= 0
 130          CONTINUE
C               look for end of priority level
                SUMFRC= 0.0
                I= LEVND
                LEVST= LEVND+ 1
 140            CONTINUE
                  I= I+ 1
                  SUMFRC= SUMFRC+ CFVOLF(I,IX)
                  IF (I .GE. N) THEN
C                   end of all levels
                    LEVND= I
                  ELSE
C                   check if next is same level
                    IF (CFVOLP(I+ 1,IX) .NE. CFVOLP(I,IX)) THEN
C                     next is not same level
                      LEVND= I
                    END IF
                  END IF
                IF (LEVST .GT. LEVND) GO TO 140
C
                IF ( (SUMFRC .GT. 0.0) .AND.
     $               (ABS (SUMFRC- 1.0) .GT. 1.0E-5) ) THEN
C                 error - fractions do not sum to one or zero
                  SGRP= 40
                  CALL OMSTR (SUMFRC)
                  CALL OMSTI (CFVOLP(LEVND,IX))
                  CALL OMSTI (IX)
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
C
C             end of loop on levels
              IF (LEVND .LT. N) GO TO 130
            END IF
 150      CONTINUE
        END IF
C
        IF (NCOGT .GT. 0) THEN
C         process values in table-type HYDR-CDEMAND
          TBNO= 15
          TBSB= 1
          NVAL= NCOGT* 6
          CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M                 RVAL(1))
          DO 200 IG= 1, NCOGT
            WRITE (CTAG4,2010) RVAL(IG*6- 5)
            OECNT= ECOUNT
            CALL TAGVAL (CTAG4(3:4),I1,MESSU,MSGFL,SCLU,BGRP,
     M                   ECOUNT,
     O                   COGTC(IG))
            IF (OECNT .GT. ECOUNT) THEN
C             error - tag was unrecognized
              SGRP= 41
              CALL OMSTC (I2,CTAG1(3))
              CALL OMSTI (I)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
            COGTE(IG)= IFIX (RVAL(IG*6- 4)+ 0.1)
            WRITE (CPRIOR,2070) RVAL(IG*6-3),RVAL(IG*6-2),RVAL(IG*6-1)
C           try reading priority as a real number
            READ (CPRIOR,1000,ERR= 160) COGTP(IG)
            GO TO 190
 160        CONTINUE
C             priority not a single number - try reading as date
              READ (CPRIOR,1010,ERR= 170) YY,MM,DD
              GO TO 180
 170        CONTINUE
C             error - priority not read correctly
              I= 12
              CALL OMSTC (I,CPRIO1)
              SGRP= 11
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
 180        CONTINUE
C             convert priority date to real number
              IF (YY .GT. 0) THEN
C               a year was given
                IF (MM .LE. 0) THEN
C                 month defaults to january
                  MM= 1
                END IF
                IF (DD .LE. 0) THEN
C                  day defaults to the first
                   DD= 1
                END IF
              END IF
              COGTP(IG)= FLOAT (YY)+ (FLOAT (MM))/100.0+
     $                  (FLOAT (DD))/10000.0
 190        CONTINUE
            FG= ODGTFG(COGTE(IG))
            COTDGT(FG,COGTC(IG))= RVAL(IG*6)
            OUTDGT(FG)= OUTDGT(FG)+ COTDGT(FG,COGTC(IG))
 200      CONTINUE
C
C         sort by exit, then by priority
          I= 2
          CALL CATSRT (NCOGT,I,
     M                 COGTC,COGTE,COGTP,
     O                 CATPOS)
        END IF
C     end of category parameters
      END IF
C
C     compute initial conditions
C
      IF (RETCOD .EQ. 0) THEN
C       compute and echo initial values of point values
C
        IF ( (NCATS .GE. 1) .AND. (OUTLEV .GT. 2) ) THEN
C         echo header for initial category storages
          WRITE (MESSU,2020)
          IF (UUNITS .EQ. 1) THEN
C           english units
            WRITE (MESSU,2030)
          ELSE
C           metric units
            WRITE (MESSU,2040)
          END IF
          IF (ICAT .GT. 0) THEN
C           echo initial storage for single category category
            PCVOL= CVOL(ICAT)/VFACT
            WRITE (MESSU,2050) CATNAM(ICAT),PCVOL
          ELSE IF (ICAT .LT. 0) THEN
C           echo initial storage for each listed category
            DO 210 I= 1, -ICAT
              PCVOL= CVOL(CINIT(I))/VFACT
              WRITE (MESSU,2050) CATNAM(CINIT(I)),PCVOL
 210        CONTINUE
          ELSE
C           echo equal initial storage for all categories
            PCVOL= EQVOL/VFACT  
            WRITE (MESSU,2060) PCVOL
          END IF
        END IF
C
C       compute discharge rate(s) at start of run, based on init cond
        CALL DISCH (SCLU,OUTLEV)
C
        IF (AUX1FG .GE. 1) THEN
C         find initial values of depth, surface area, average depth,
C         top width, and hydraulic radius
          IF (NODFV .EQ. 0) THEN
C           subroutine DISCH did not find current row
            CALL FNDROW (NROWS,NCOLS,RCHTAB,VOL,RCHNO,MESSU,MSGFL,SCLU,
     I                   DATIM,
     M                   ROWPT,HYWCNT)
          END IF
          ROWADD= (ROWPT- 1)*NCOLS+ 1
          CALL AUXIL (NCOLS,RCHTAB,ROWADD,VOL,STCOR,LEN,
     I                RCHNO,MESSU,MSGFL,DATIM,SCLU,AUX1FG,
     M                HYECNT,
     O                DEP,STAGE,SAREA,AVDEP,TWID,HRAD)
C
          IF (OUTLEV .GT. 2) THEN
C           echo aux1 state variable values
            WRITE (MESSU,2080)
            IF (UUNITS .EQ. 1) THEN
C             english units
              WRITE (MESSU,2090)
            ELSE
C             metric units
              WRITE (MESSU,2100)
            END IF
C
            PSAREA= SAREA/AFACT
            WRITE (MESSU,2110) DEP, STAGE, PSAREA, AVDEP, TWID, HRAD
          END IF
C
          IF (AUX2FG .EQ. 1) THEN
C           find initial values of average velocity and cross section
            IF (VOL .GT. 0.0) THEN
C             water in reach
              AVVEL= LEN*RO/VOL
              AVSECT= VOL/LEN
            ELSE
C             reach dry
              AVVEL= 0.0
              AVSECT= 0.0
            END IF
C
            IF (OUTLEV .GT. 2) THEN
C             echo aux2 state variable values
              WRITE (MESSU,2120)
              IF (UUNITS .EQ. 1) THEN
                WRITE (MESSU,2130)
C               english units
              ELSE
                WRITE (MESSU,2140)
C               metric units
              END IF
              WRITE (MESSU,2150) AVVEL, AVSECT
            END IF
C
            IF (AUX3FG .EQ. 1) THEN
C             find initial values of bed shear stress and shear velocity
              CALL SHEAR (LKFG,AVDEP,AVVEL,HRAD,DB50,AKAPPA,GAM,
     I                    GRAV,SLOPE,
     O                    USTAR,TAU)
              IF (OUTLEV .GT. 2) THEN
C               echo aux3 state variable values
                WRITE (MESSU,2160)
                IF (UUNITS .EQ. 1) THEN
C                 english units
                  WRITE (MESSU,2170)
                ELSE
C                 metric units
                  WRITE (MESSU,2180)
                END IF
                WRITE (MESSU,2190) USTAR,TAU
              END IF
            END IF
          END IF
        END IF
      ELSE
C       no processing aux variables, problem with ftable
        WRITE (MESSU,2200)
      END IF
C
      IF (OUTLEV .GT. 1) THEN
C       finished processing hydr
        WRITE (MESSU,2210)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CATFRC
     I                    (OUTVOL,NCATS,NFRAC,MXUSRC,CATF,PRIFRC,
     I                     FRACS,
     M                     CVOLT,CFRAC,
     O                     COVOL)
C
C     + + + PURPOSE + + +
C     Allocate a fixed outflow volume among categories according to priorities,
C     user-specified fractions, and/or current storage fractions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NCATS,NFRAC,MXUSRC,CATF(MXUSRC),PRIFRC(MXUSRC)
      REAL             OUTVOL,FRACS(MXUSRC),COVOL(NCATS)
      DOUBLE PRECISION CVOLT(NCATS),CFRAC(NCATS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTVOL - outflow volume to be allocated
C     NCATS  - number of categories active
C     NFRAC  - if positive, the primary category for outflow; if negative,
C              the number of categories specifed for outflow allocation
C     MXUSRC - maximum number of user-specified category allocations
C     CATF   - category of an allocation
C     PRIFRC - priority of an allocation
C     FRACS  - fraction of an allocation
C     CVOLT  - volume of water in category above bed
C     CFRAC  - fraction of total volume belonging to each category
C     COVOL  - allocated outflow volumes for each category
C
C     + + + PARAMETERS + + +
      INCLUDE 'phcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER LEVST,LEVND,IFV,IC,N
      REAL    R0
      DOUBLE PRECISION FRCSUM,CFSUM,AVAIL,TOTAL,LOVOL,TAKE,LVOL
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R0/0.0/
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize
      LOVOL= OUTVOL
      CALL ZIPR (NCATS,R0,
     O           COVOL)
C
      IF (NFRAC .GT. 0) THEN
C       allocate as much as possible to single specified category
        IF (LOVOL .LE. CVOLT(NFRAC)) THEN
C         can take all from category
          CVOLT(NFRAC)= CVOLT(NFRAC)- LOVOL
          COVOL(NFRAC)= COVOL(NFRAC)+ LOVOL
          LOVOL= 0.0
        ELSE
C         take all of storage and get rest later from remaining categories
          LOVOL= LOVOL- CVOLT(NFRAC)
          COVOL(NFRAC)= COVOL(NFRAC)+ CVOLT(NFRAC)
          CVOLT(NFRAC)= 0.0
        END IF
      ELSE IF (NFRAC .LT. 0) THEN
C       use list of specified categories, from highest to lowest
C
        N= -NFRAC
        LEVND= 0
        IF (LOVOL .GT. 0.0) THEN
C         need to allocate outflow volume
C
C         begin do-until loop on levels
 10       CONTINUE
C
C           look for end of priority level
            AVAIL= 0.0
            IFV= LEVND
            LEVST= LEVND+ 1
 20         CONTINUE
              IFV= IFV+ 1
              AVAIL= AVAIL+ CVOLT(CATF(IFV))
              IF (IFV .GE. N) THEN
C               end of all levels
                LEVND= IFV
              ELSE
C               check if next is same level
                IF (PRIFRC(IFV+ 1) .NE. PRIFRC(IFV)) THEN
C                 next is not same level
                  LEVND= IFV
                END IF
              END IF
            IF (LEVST .GT. LEVND) GO TO 20
C
C           now try to satisfy outflow from the current priority level
C           based on fractions
C
C           do-until outflow satisfied or available storage at level is gone
 30         CONTINUE
C
C             first sum up fractions to normalize
              FRCSUM= 0.0
              CFSUM= 0.0
              DO 40 IFV= LEVST, LEVND
                IC= CATF(IFV)
                IF (CVOLT(IC) .GT. 0.0) THEN
C                 this category has remaining storage
                  FRCSUM= FRCSUM+ FRACS(IFV)
                  CFSUM= CFSUM+ CFRAC(IC) 
                END IF
 40           CONTINUE
C
C             now go back and take
              TOTAL= 0.0
              DO 50 IFV= LEVST, LEVND
                IC= CATF(IFV)
                IF (FRCSUM .GT. 0.0) THEN
C                 user specified fractions
                  TAKE= LOVOL*FRACS(IFV)/FRCSUM
                ELSE IF (CFSUM .GT. 0.0) THEN
C                 use current fractions
                  TAKE= LOVOL*CFRAC(IC)/CFSUM
                ELSE
C                 no water to take
                  TAKE= 0.0
                END IF
                IF (CVOLT(IC) .GT. TAKE) THEN
C                 take all of this category's share
                  COVOL(IC)= COVOL(IC)+ TAKE
                  AVAIL= AVAIL- TAKE
                  TOTAL= TOTAL+ TAKE
                  CVOLT(IC)= CVOLT(IC)- TAKE
                ELSE
C                 take whatever is there
                  COVOL(IC)= COVOL(IC)+ CVOLT(IC)
                  AVAIL= AVAIL- CVOLT(IC)
                  TOTAL= TOTAL+ CVOLT(IC)
                  CVOLT(IC)= 0.0
                END IF
 50           CONTINUE
              LOVOL= LOVOL- TOTAL
C             end of loop iterating on a level
            IF ( (LOVOL .GT. 1.0E-5) .AND.
     $           (AVAIL .GT. 1.0E-5) ) GO TO 30
C         end of loop on levels
          IF ( (LOVOL .GT. 1.0E-5) .AND. (LEVND .LT. N) ) GO TO 10
        END IF
      END IF
C
      IF (NFRAC .NE. 0) THEN
C       volume fractions have changed
C
C       get total available
        LVOL= 0.0
        DO 60 IC= 1, NCATS
          LVOL= LVOL+ CVOLT(IC)
 60     CONTINUE
        DO 65 IC= 1, NCATS
          IF (LVOL .GT. 0.0) THEN
C           compute fraction
            CFRAC(IC)= CVOLT(IC)/LVOL
          ELSE
C           reset fraction
            CFRAC(IC)= 1.0/NCATS
          END IF
 65     CONTINUE
      END IF
C
      IF (LOVOL .GT. 0.0) THEN
C       now must use remaining categories at current storage fraction
C
        DO 70 IC= 1, NCATS
          IF (CVOLT(IC) .GT. 0.0) THEN
C           use this category
            TAKE= LOVOL*CFRAC(IC)
            IF (CVOLT(IC) .GT. TAKE) THEN
C             take all
              COVOL(IC)= COVOL(IC)+ TAKE
              CVOLT(IC)= CVOLT(IC)- TAKE
            ELSE
C             take what is there
              COVOL(IC)= COVOL(IC)+ CVOLT(IC)
              CVOLT(IC)= 0.0
            END IF
          END IF
 70     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CATSRT
     I                    (NVAL,SORTFG,
     M                     CATS,IVAL,RVAL,
     O                     POS)
C
C     + + + PURPOSE + + +
C     Sort category priorities together.  Algorithm is modified from ASRT*.
C     Low priority numbers are highest priority, but zero is lowest priority.
C     Final arrays have highest priority first.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NVAL,SORTFG,CATS(NVAL),IVAL(NVAL),POS(NVAL)
      REAL    RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVAL   - number of values in each array to sort
C     SORTFG - 1: sort integers only, 2: sort integers, then reals
C     CATS   - array of category numbers
C     IVAL   - array of integer values (integer priority or exit number)
C     RVAL   - array of real values (real priority or fraction)
C     POS    - array of positions
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,J,CTMP,ITMP
      REAL    RTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPI,ASRTI,ASRTR
C
C     + + + END SPECIFICATIONS + + +
C
      I= 0
      CALL ZIPI (NVAL,I,
     O           POS)
C
      IF (SORTFG .GE. 1) THEN
C       sort by integer
C
C       first make zeroes a large number
        DO 10 I= 1, NVAL
          IF (IVAL(I) .EQ. 0) THEN
C           make a large number
            IVAL(I)= 32767
          END IF
 10     CONTINUE
C
C       now sort in place
        I= 0
        CALL ASRTI (I,NVAL,
     M              IVAL,
     O              POS)
        DO 30 I= 1, NVAL
          IF (POS(I).NE.I) THEN
C           need to move, first save whats in target space
            CTMP= CATS(I)
            ITMP= IVAL(I)
            RTMP= RVAL(I)
C           move sorted data to target position
            CATS(I)= CATS(POS(I))
            IVAL(I)= IVAL(POS(I))
            RVAL(I)= RVAL(POS(I))
C           move temp data to source position
            CATS(POS(I))= CTMP
            IVAL(POS(I))= ITMP
            RVAL(POS(I))= RTMP
C           find the pointer to the other value we are moving
            J= I
 20         CONTINUE
              J= J+ 1
            IF (POS(J).NE.I) GO TO 20
            POS(J)= POS(I)
            POS(I)= I
          END IF
 30     CONTINUE
C
C       now return to zero
        DO 40 I= 1, NVAL
          IF (IVAL(I) .EQ. 32767) THEN
C           set back to zero
            IVAL(I)= 0
          END IF
 40     CONTINUE
      END IF
C
      IF (SORTFG .GE. 2) THEN
C       sort by real
C
C       first make zeroes a large number
        DO 50 I= 1, NVAL
          IF (RVAL(I) .EQ. 0.0) THEN
C           make a large number
            RVAL(I)= 1.0E30
          END IF
 50     CONTINUE
C
        I= 0
        CALL ASRTR (I,NVAL,
     M              RVAL,
     O              POS)
        DO 70 I= 1, NVAL
          IF (POS(I).NE.I) THEN
C           need to move, first save whats in target space
            CTMP= CATS(I)
            ITMP= IVAL(I)
            RTMP= RVAL(I)
C           move sorted data to target position
            CATS(I)= CATS(POS(I))
            IVAL(I)= IVAL(POS(I))
            RVAL(I)= RVAL(POS(I))
C           move temp data to source position
            CATS(POS(I))= CTMP
            IVAL(POS(I))= ITMP
            RVAL(POS(I))= RTMP
C           find the pointer to the other value we are moving
            J= I
 60         CONTINUE
              J= J+ 1
            IF (POS(J).NE.I) GO TO 60
            POS(J)= POS(I)
            POS(I)= I
          END IF
 70     CONTINUE
C
C       now return to zero
        DO 80 I= 1, NVAL
          IF (RVAL(I) .GE. 1.0E30) THEN
C           set back to zero
            RVAL(I)= 0.0
          END IF
 80     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DISCH
     I                   (SCLU,OUTLEV)
C
C     + + + PURPOSE + + +
C     Find the discharge rate(s) at the start of the run, based on
C     initial conditions, or after an irrigation withdrawal, based
C     on the new volume.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SCLU,OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SCLU   - cluster containing error and warning message details
C     OUTLEV - run interpreter output level; set to zero for runtime calls
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
      INCLUDE    'chcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N,FG,IG,IC,IX,ROWAD1,ROWAD2
      REAL       A1,A2,OD1(MXEXIT),OD2(MXEXIT),ROD1,ROD2,V1,V2,
     $           FO(MXEXIT),FCO(MXCAT),R0
      DOUBLE PRECISION COINT(MXCAT),CFRAC(MXCAT)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   FNDROW,DEMAND,ZIPR,CATFRC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R0/0.0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' INITIAL TOTAL OUTFLOW RATE:   ',1PE10.3)
 2010 FORMAT (/,' INITIAL OUTFLOWS THROUGH INDIVIDUAL EXITS:')
 2020 FORMAT (  ' EXIT',13X,12I10)
 2030 FORMAT (  ' FLOW',13X,1P12E10.3)
 2040 FORMAT (  '   BY CATEGORY')
 2050 FORMAT (1X,A16,1X,1P12E10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NODFV.GT.0) THEN
C       one or more outflow rates depend on stored volume
        CALL FNDROW (NROWS,NCOLS,RCHTAB,VOL,RCHNO,MESSU,MSGFL,SCLU,
     I               DATIM,
     M               ROWPT,HYWCNT)
C
C       get addresses, in RCHTAB of first elements in rows which
C       bracket VOL
        ROWAD1= (ROWPT-1)*NCOLS+ 1
        ROWAD2= ROWAD1+ NCOLS
C
C       find corresponding volumes
        V1= RCHTAB(ROWAD1+2)
        V2= RCHTAB(ROWAD2+2)
C
C       find corresponding outflow demands
        CALL DEMAND (NCOLS,RCHTAB(ROWAD1),NEXITS,ODFVFG,ODGTFG,CONVF,
     I               COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I               MXEXIT,COTDGT,VOL,DELTS,
     O               OD1,ROD1)
C
        CALL DEMAND (NCOLS,RCHTAB(ROWAD2),NEXITS,ODFVFG,ODGTFG,CONVF,
     I               COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I               MXEXIT,COTDGT,VOL,DELTS,
     O               OD2,ROD2)
C
C       interpolate
        A1= (V2-VOL)/(V2-V1)
        A2= 1.0- A1
        DO 10 IX= 1,NEXITS
          O(IX)= A1*OD1(IX)+ A2*OD2(IX)
 10     CONTINUE
        RO= A1*ROD1+ A2*ROD2
      ELSE
C       no outflow demands have an f(vol) component
        CALL DEMAND (NCOLS,RCHTAB,NEXITS,ODFVFG,ODGTFG,CONVF,
     I               COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I               MXEXIT,COTDGT,VOL,DELTS,
     O               O,RO)
      END IF
C
      IF (NCATS .GE. 1) THEN
C       allocate initial flows among categories
C
        N= MXEXIT*NCATS
        CALL ZIPR (N,R0,
     O             CO)
        CALL ZIPR (N,R0,
     O             COREL)
        CALL ZIPR (NCATS,R0,
     O             CRO)
        DO 20 IX= 1, NEXITS
          FO(IX)= O(IX)
 20     CONTINUE
C
C       find initial gt rates, and overall fvol rates for each exit
        DO 30 IG= 1, NCOGT
          IC= COGTC(IG)
          IX= COGTE(IG)
          FG= ODGTFG(IX)
          COREL(FG,IC)= COTDGT(FG,IC)
          CO(IX,IC)= CO(IX,IC)+ COTDGT(FG,IC)
          CRO(IC)= CRO(IC)+ COTDGT(FG,IC)
          FO(IX)= FO(IX)- COTDGT(FG,IC)
          IF (ABS (FO(IX)) .LT. O(IX)*1.0E-5) THEN
C           prevent underflow
            FO(IX)= 0.0
          END IF
 30     CONTINUE
C
        IF (NODFV .GT. 0) THEN
C         now allocate fvol rate
C
C         compute storage fractions and set dummy max outflow
          DO 40 IC= 1, NCATS
C           dummy max outflow
            COINT(IC)= 1.0E30
            IF (VOL .GT. 0.0) THEN
C             compute fraction
              CFRAC(IC)= CVOL(IC)/VOL
            ELSE
C             default fraction
              CFRAC(IC)= 1.0/NCATS
            END IF
 40       CONTINUE
C
C         now compute initial category fvol rates
          DO 60 IX= 1, NEXITS
            IF (IX .NE. IREXIT) THEN
C             normal fvol demand
              CALL CATFRC (FO(IX),NCATS,CFVOL(IX),MXCUSR,CFVOLC(1,IX),
     I                     CFVOLP(1,IX),CFVOLF(1,IX),
     M                     COINT,CFRAC,
     O                     FCO)
              DO 50 IC= 1, NCATS
C               add this to initial gt rate
                CO(IX,IC)= CO(IX,IC)+ FCO(IC)
                CRO(IC)= CRO(IC)+ FCO(IC)
 50           CONTINUE
            END IF
 60       CONTINUE
        END IF
      END IF
C
      IF (OUTLEV .GT. 2) THEN
C       echo initial outflow rates
        WRITE (MESSU,2000)  RO
        IF (NCATS .GE. 1) THEN
C         echo initial category outflow rates
          WRITE (MESSU,2040)  
          DO 70 IC= 1, NCATS
            WRITE (MESSU,2050) CATNAM(IC),CRO(IC)
 70       CONTINUE
        END IF
        IF (NEXITS .GT. 1) THEN
C         echo initial outflow by exit
          WRITE (MESSU,2010)
          WRITE (MESSU,2020)  (IX,IX= 1, NEXITS)
          WRITE (MESSU,2030)  (O(IX),IX= 1, NEXITS)
          IF (NCATS .GE. 1) THEN
C           echo initial category outflow rates
            WRITE (MESSU,2040)  
            DO 80 IC= 1, NCATS
              WRITE (MESSU,2050) CATNAM(IC),(CO(IX,IC),IX= 1, NEXITS)
 80         CONTINUE
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FTABLE
     I                    (FTABNO,AFACT,VFACT,
     O                     NROWS,NCOLS,RCHTAB,VZERPT,RETCOD)
C
C     + + + PURPOSE + + +
C     Process an FTABLE referred to in the input to the HYDR section
C     of the RCHRES module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FTABNO,NCOLS,NROWS,VZERPT,RETCOD
      REAL       AFACT,RCHTAB(500),VFACT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTABNO - ftable number
C     AFACT  - area conversion factor
C     VFACT  - volume conversion factor
C     NROWS  - number of rows in ftable
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     VZERPT - pointer to highest row for which volume is zero
C     RETCOD - return code
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION INTERP1 + + +
      INCLUDE    'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      DUMMY,ERRFG,IBASE,KEY,KEYND,KEYST,I,
     $             MESSU,NC,NO,NR,SCLU,SGRP,MSGFL,I0,VALSET
      REAL         PDEPTH,PVOL,RVAL(8)
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UCIBF,UCIBF1)
      CHARACTER*1  UCIBF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER    VALNO
C
C     + + + EXTERNALS + + +
      EXTERNAL   VALNO,OMSG,OMSTI,OMSTC,DUMPER,GETUCI,GETVEC
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (2I5)
 1020 FORMAT (8F10.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING FTABLE NO. ',I3)
 2060 FORMAT (/,' FINISHED PROCESSING FTABLE NO. ',I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
      SCLU= 341
C
      RETCOD= 0
C
      IF (OUTLEV.GT.3) THEN
C       processing message
        WRITE (MESSU,2000)  FTABNO
      END IF
C
C     check that the reference is valid
      NO= VALNO(NFTABS,TABINX,FTABNO)
      IF (NO .EQ. 0) THEN
C       error - ftable referred to not found in ftable index
        CALL OMSTI (FTABNO)
        SGRP= 14
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        RETCOD= 1
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
        READ (UCIBF,1010,ERR=10)  NROWS, NCOLS
          GO TO 20
 10     CONTINUE
C         error - invalid numeric input
          CALL OMSTI (FTABNO)
          I= 10
          CALL OMSTC (I,UCIBF1)
          SGRP= 28
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          RETCOD= 2
 20     CONTINUE
C       check values from heading
        IF (NROWS.LE.0) THEN
C         no rows
          ERRFG= 1
        ELSE IF (NCOLS.LT.3 .OR. NCOLS.GT.8) THEN
C         not enough or too many columns
          ERRFG= 1
        ELSE
          DUMMY= NCOLS*NROWS
          IF (DUMMY.GT.500) THEN
C           too much space required
            ERRFG= 1
          ELSE
C           heading values ok
            ERRFG= 0
          END IF
        END IF
C
        IF (ERRFG.EQ.1) THEN
C         error - ftable must have one or more rows, between three
C         and eight columns and not more than 500 values
          CALL OMSTI (FTABNO)
          CALL OMSTI (NROWS)
          CALL OMSTI (NCOLS)
          SGRP= 15
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          RETCOD= 2
        ELSE
C         process the ftable
          ERRFG= 0
          PDEPTH= 0.0
          PVOL= 0.0
C
C         dountil nr= nrows or errfg= 1
          VZERPT= 0
          NR= 0
 30       CONTINUE
            NR= NR+ 1
            IF (KEY .EQ. KEYND) THEN
C             error - no. of rows spec'd for ftable puts it beyond
C             its end delimiter
              ERRFG= 1
              CALL OMSTI (FTABNO)
              CALL OMSTI (NROWS)
              CALL OMSTI (KEYND)
              SGRP= 16
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              RETCOD= 3
            ELSE
C             process the row
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              IF (FILE(7) .GT. 0) THEN
C               pest supplemental file in use
C               see if this record needs updating
                CALL GETVEC (MESSU, MSGFL, NCOLS, I0, OUTLEV,
     M                       UCIBF, RVAL, ECOUNT,
     O                       VALSET)          
              ELSE
C               no pest supplemental file in use
                VALSET = 0
              END IF
              IF (VALSET .EQ. 0) THEN
C               no values from pest sup file
                READ (UCIBF,1020,ERR=40)  RVAL
              END IF
                GO TO 45
 40           CONTINUE
C               error - invalid numeric input
                CALL OMSTI (NR)
                CALL OMSTI (FTABNO)
                I= 80
                CALL OMSTC (I,UCIBF1)
                SGRP= 29
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                RETCOD= 4
 45           CONTINUE
C
C             check values are not negative
              ERRFG= 0
              DO 50 NC= 1,NCOLS
                IF (RVAL(NC).LT.0.0) THEN
C                 negative value
                  ERRFG= 1
                END IF
 50           CONTINUE
C
              IF (ERRFG.EQ.1) THEN
C               error - negative value
                CALL OMSTI (FTABNO)
                CALL OMSTI (NR)
                SGRP= 17
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                RETCOD= 4
              END IF
C
C             check that depth and volume are not decreasing
              IF (RVAL(1).LT.PDEPTH .OR. RVAL(3).LT.PVOL) THEN
C               error
                CALL OMSTI (NR)
                CALL OMSTI (FTABNO)
                SGRP= 18
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                RETCOD= 5
              END IF
C
C             if (rval(3).ne.0.0) then
              IF (RVAL(3) .LE. 1.0E-10) THEN
C               update value which points to highest row for which
C               volume is zero
                VZERPT= NR
              END IF
C
C             remember depth and volume for processing next row
              PDEPTH= RVAL(1)
              PVOL= RVAL(3)
C
C             convert values to internal units
C             area
              RVAL(2)= RVAL(2)*AFACT
C             volume
              RVAL(3)= RVAL(3)*VFACT
C
C             store values in rchtab(*)
              IBASE= (NR-1)*NCOLS
              DO 90 NC= 1,NCOLS
                RCHTAB(IBASE+NC)= RVAL(NC)
 90           CONTINUE
            END IF
C           loop back for more rows
          IF (NR.NE.NROWS .AND. ERRFG.NE.1) GO TO 30
C
          IF (VZERPT.EQ.0) THEN
C           error - ftable must have at least one row with zero volume
            CALL OMSTI (FTABNO)
            CALL OMSTI (VZERPT)
            SGRP= 19
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            RETCOD= 6
          END IF
        END IF
      END IF
C
      IF (OUTLEV.GT.3) THEN
        WRITE (MESSU,2060)  FTABNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FTBWDM
     I                    (FTABNO,FTBDSN,AFACT,VFACT,UUNITS,
     O                     NROWS,NCOLS,RCHTAB,VZERPT,RETCOD)
C
C     + + + PURPOSE + + +
C     Process a wdms ftable referred to in the input to the hydr section
C     of the rchres module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FTABNO,FTBDSN,NCOLS,NROWS,VZERPT,UUNITS,RETCOD
      REAL         AFACT,RCHTAB(500),VFACT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTABNO - ftable number
C     FTBDSN - ftable dataset number
C     AFACT  - area conversion factor
C     VFACT  - volume conversion factor
C     UUNITS - system of units   1-english, 2-metric
C     NROWS  - number of rows in ftable
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     VZERPT - pointer to highest row for which volume is zero
C     RETCOD - return code
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION INTERP1 + + +
      INCLUDE      'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      DUMMY,ERRFG,I,I1,MESSU,NR,
     $             WDMSFL,MSGFL,LREC,TBCNT,TGRPPT,SCLU,SGRP,
     $             TGRP,TQNU,TFLDS,TLEN(16),TCOL(16),TNUM(4),
     $             AGRP,AQNU,AFLDS,ALEN(16),ACOL(16),ASPA,ANUM(4),
     $             DATFLG,FSTCOL,LSTCOL,DEPCOL,VOLCOL,ARECOL
      REAL         PDEPTH,PVOL,TBRBUF(200)
      CHARACTER*1  TTYP(16),ATYP(16),MFID(2)
      CHARACTER*16 TABNAM,MTBNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTI,CHKWDM,WDTBFX,WDTBSP,WTBGET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA         TABNAM/'FTABLE          '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING WDMS FTABLE: DSN NO. ',I3,
     $        ';  TABLE NO. ',I3)
 2060 FORMAT (/,' FINISHED PROCESSING WDMS FTABLE: DSN NO. ',I3,
     $        ';  TABLE NO. ',I3)
 2070 FORMAT (  ' ',F10.3,F10.2,5(1X,1PE10.3))
 2080 FORMAT (/,'      DEPTH      AREA     VOLUME      DISCH')
 2090 FORMAT (/,'      DEPTH      AREA     VOLUME      DISCH',
     $          '      DISCH')
 2100 FORMAT (/,'      DEPTH      AREA     VOLUME      DISCH',
     $          '      DISCH      DISCH')
 2110 FORMAT (/,'      DEPTH      AREA     VOLUME      DISCH',
     $          '      DISCH      DISCH      DISCH')
 2120 FORMAT (  '       (ft)   (acres)    (ac-ft)    (ft3/s)')
 2130 FORMAT (  '       (ft)   (acres)    (ac-ft)    (ft3/s)',
     $          '    (ft3/s)')
 2140 FORMAT (  '       (ft)   (acres)    (ac-ft)    (ft3/s)',
     $          '    (ft3/s)    (ft3/s)')
 2150 FORMAT (  '       (ft)   (acres)    (ac-ft)    (ft3/s)',
     $          '    (ft3/s)    (ft3/s)    (ft3/s)')
 2160 FORMAT (  '        (m)      (ha)      (Mm3)     (m3/s)')
 2170 FORMAT (  '        (m)      (ha)      (Mm3)     (m3/s)',
     $          '     (m3/s)')
 2180 FORMAT (  '        (m)      (ha)      (Mm3)     (m3/s)',
     $          '     (m3/s)     (m3/s)')
 2190 FORMAT (  '        (m)      (ha)      (Mm3)     (m3/s)',
     $          '     (m3/s)     (m3/s)     (m3/s)')
C
C     + + + END SPECIFICATIONS + + +
C
      MESSU= FILE(1)
      WDMSFL= FILE(14)
      MSGFL= FILE(15)
      SCLU= 341
C
      RETCOD= 0
      I1= 1
      DATFLG= 1
C
      IF (OUTLEV.GT.3) THEN
C       processing message
        WRITE (MESSU,2000)  FTBDSN,FTABNO
      END IF
C
C     initialize wdms file if wdmsfl is being used in this run
      IF (WDMSFL .EQ. 0) THEN
C       error - wdms file not available, cannot process ftable
        CALL OMSTI (FTBDSN)
        CALL OMSTI (FTABNO)
        SGRP= 20
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        RETCOD= 1
      ELSE
C       check wdms file and initialize wdms routines
        CALL CHKWDM (WDMSFL,MESSU,MSGFL,
     M               ECOUNT)
C
C       check that the table dataset exists and contains table ftabno
        CALL WDTBFX (WDMSFL,FTBDSN,FTABNO,TABNAM,
     O               TBCNT,LREC,TGRPPT,MFID,TGRP,TQNU,NROWS,RETCOD)
C
        IF (RETCOD.NE.0) THEN
C         error - dataset does not exist in wdms file, or is not
C         a table dataset, or specified table is not in dataset
          CALL OMSTI (FTBDSN)
          CALL OMSTI (FTABNO)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         process ftable dataset
          CALL WDTBSP (MSGFL,TGRP,TQNU,
     O                 MTBNAM,TFLDS,TTYP,TLEN,TCOL,NCOLS,TNUM,
     O                 AGRP,AQNU,AFLDS,ATYP,ALEN,ACOL,ASPA,ANUM,
     O                 RETCOD)
C
          IF (RETCOD.NE.0) THEN
C           program bug
            CALL OMSTI (FTBDSN)
            CALL OMSTI (FTABNO)
            CALL OMSTI (RETCOD)
            SGRP= 26
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          ELSE
C           get table data
            CALL WTBGET
     I                  (WDMSFL,FTBDSN,TABNAM,FTABNO,DATFLG,
     I                   I1,NROWS,I1,NCOLS,
     O                   TBRBUF,RETCOD)
C
            IF (RETCOD.NE.0) THEN
C             program bug
              CALL OMSTI (FTBDSN)
              CALL OMSTI (FTABNO)
              CALL OMSTI (RETCOD)
              SGRP= 27
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             process ftable
              IF (OUTLEV.GT.5) THEN
C               print ftable
C               call wtbprt
C               above not implemented, so use decode routine and write
C               ix= nrows*ncols
C               call wtbdcd (tflds,tnum,nrows,ncols,tlen,ttyp,tcol,
C    I                       tbrbuf,ix,
C    O                       tbcbuf,retcod)
C
                IF (UUNITS.EQ.1) THEN
C                 english units
                  IF (NCOLS .LE. 4) THEN
C                   one discharge column
                    WRITE (MESSU,2080)
                    WRITE (MESSU,2120)
                  ELSE IF (NCOLS .EQ. 5) THEN
C                   two discharge columns
                    WRITE (MESSU,2090)
                    WRITE (MESSU,2130)
                  ELSE IF (NCOLS .EQ. 6) THEN
C                   three discharge columns
                    WRITE (MESSU,2100)
                    WRITE (MESSU,2140)
                  ELSE IF (NCOLS .EQ. 7) THEN
C                   four discharge columns
                    WRITE (MESSU,2110)
                    WRITE (MESSU,2150)
                  END IF
                ELSE IF (UUNITS .EQ. 2) THEN
C                 metric units
                  IF (NCOLS .LE. 4) THEN
C                   one discharge column
                    WRITE (MESSU,2080)
                    WRITE (MESSU,2160)
                  ELSE IF (NCOLS .EQ. 5) THEN
C                   two discharge columns
                    WRITE (MESSU,2090)
                    WRITE (MESSU,2170)
                  ELSE IF (NCOLS .EQ. 6) THEN
C                   three discharge columns
                    WRITE (MESSU,2100)
                    WRITE (MESSU,2180)
                  ELSE IF (NCOLS .EQ. 7) THEN
C                   four discharge columns
                    WRITE (MESSU,2110)
                    WRITE (MESSU,2190)
                  END IF
                END IF
C               write out ftable values
                DO 5 NR= 1, NROWS
                  FSTCOL= (NR- 1)*NCOLS + 1
                  LSTCOL= NR*NCOLS
                  WRITE (MESSU,2070) (TBRBUF(I),I=FSTCOL,LSTCOL)
 5              CONTINUE
              END IF
C
C             check the ftable dimensions
              IF (NROWS .LE. 0) THEN
C               no rows
                ERRFG= 1
              ELSE IF (NCOLS.LT.3 .OR. NCOLS.GT.8) THEN
C               not enough or too many columns
                ERRFG= 1
              ELSE
                DUMMY= NCOLS*NROWS
                IF (DUMMY.GT.500) THEN
C                 too much space required
                  ERRFG= 1
                ELSE
C                 heading values ok
                  ERRFG= 0
                END IF
              END IF
C
              IF (ERRFG.EQ.1) THEN
C               error - ftable must have one or more rows, between three
C               and eight columns and not more than 500 values
                CALL OMSTI (FTBDSN)
                CALL OMSTI (FTABNO)
                CALL OMSTI (NROWS)
                CALL OMSTI (NCOLS)
                SGRP= 22
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                RETCOD= 2
              ELSE
C               check ftable data
                VZERPT= 0
                PDEPTH= 0.0
                PVOL= 0.0
                NR= 0
 10             CONTINUE
C                 process a row
                  NR= NR + 1
                  FSTCOL= (NR-1)*NCOLS + 1
                  LSTCOL= NR*NCOLS
                  ERRFG= 0
C
C                 check values are not negative
                  DO 20 I= FSTCOL,LSTCOL
                    IF (TBRBUF(I).LT.0.0) THEN
C                     value is negative
                      ERRFG= 1
                    END IF
 20               CONTINUE
C
                  IF (ERRFG.EQ.1) THEN
C                   error - negative value
                    CALL OMSTI (FTBDSN)
                    CALL OMSTI (FTABNO)
                    CALL OMSTI (NR)
                    SGRP= 23
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                    RETCOD= 3
                  END IF
C
C                 check depth and volume
                  DEPCOL= FSTCOL
                  VOLCOL= FSTCOL + 2
                  ARECOL= FSTCOL + 1
                  IF (TBRBUF(DEPCOL).LT.PDEPTH.OR.
     $                TBRBUF(VOLCOL).LT.PVOL) THEN
C                   error - depth and/or volume is decreasing
                    CALL OMSTI (FTBDSN)
                    CALL OMSTI (FTABNO)
                    CALL OMSTI (NR)
                    SGRP= 24
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                    RETCOD= 4
                  END IF
C
                  IF (TBRBUF(VOLCOL) .LE. 1.0E-10) THEN
C                   update value which points to highest row for which
C                   volume is zero
                    VZERPT= NR
                  END IF
C
C                 remember depth and volume for processing next row
                  PDEPTH= TBRBUF(DEPCOL)
                  PVOL= TBRBUF(VOLCOL)
C
C                 convert values to internal units
C                 area
                  TBRBUF(ARECOL)= TBRBUF(ARECOL)*AFACT
C                 volume
                  TBRBUF(VOLCOL)= TBRBUF(VOLCOL)*VFACT
C
C                 store values in rchtab(*)
                  DO 30 I= FSTCOL,LSTCOL
                    RCHTAB(I)= TBRBUF(I)
 30               CONTINUE
C                 loop back form more rows if needed
                IF (NR.NE.NROWS .AND. ERRFG.NE.1) GO TO 10
C
                IF (VZERPT.EQ.0) THEN
C                 ftable must have at least one row with zero volume
                  CALL OMSTI (FTBDSN)
                  CALL OMSTI (FTABNO)
                  CALL OMSTI (VZERPT)
                  SGRP= 25
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                  RETCOD= 5
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
      IF (OUTLEV .GT. 3) THEN
C       end processing message
        WRITE (MESSU,2060)  FTBDSN,FTABNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYDR
C
C     + + + PURPOSE + + +
C     Simulate the hydraulic behavior of a reach or mixed reservoir
C     double precision variables - VOL,VOLS,VOLT.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          IX,FG,IC,ROWADD,SCLU,SGRP,N,I,CATPOS(MXCUSR)
      REAL             SAS,VOLPEV,CORELS(MXEXIT,MXCAT),RTMP,IRRDEM,
     $                 COTDGS(MXEXIT,MXCAT),CVOLEV(MXCAT),DEMVOL,
     $                 CIRRWD(MXCAT)
      DOUBLE PRECISION VOLT,CVOLT(MXCAT),CFRAC(MXCAT),CKVOL,CCORR
C
C     + + + INTRINSICS + + +
      INTRINSIC        ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL         CATSRT,CATFRC,ROUTE,NOROUT,OMSTD,OMSTI,OMSTR
      EXTERNAL         OMSG,AUXIL,SHEAR
C
C     + + + HISTORY + + +
C     2005  BRB added potential et to output summaries
C
C     + + + END SPECIFICATIONS + + +
C
C     message file cluster
      SCLU= 341
C
      IF (NCATS .GE. 1) THEN
C       sort category specifications
C
C       evaporation
        IF (CEVAP .LT. 0) THEN
C         sort by priority
          N= -CEVAP
          I= 1
          CALL CATSRT (N,I,
     M                 CEVAPC,CEVAPP,CEVAPF,
     O                 CATPOS)
        END IF
C
C       fvol
        DO 90 IX= 1, NEXITS
          IF (CFVOL(IX) .LT. 0) THEN
C           sort by priority
            N= -CFVOL(IX)
            I= 1
            CALL CATSRT (N,I,
     M                   CFVOLC(1,IX),CFVOLP(1,IX),CFVOLF(1,IX),
     O                   CATPOS)
          END IF 
 90     CONTINUE
C
C       gt
        IF (NCOGT .GT. 0) THEN
C         sort by exit, then by priority
          I= 2
          CALL CATSRT (NCOGT,I,
     M                 COGTC,COGTE,COGTP,
     O                 CATPOS)
        END IF
      END IF
C
      IF (IREXIT .GE. 1) THEN
C       irrigation exit is set
C
        IF (RIRWDL .GT. 0.0) THEN
C         irrigation withdrawal is being made
C
C         set new volume
          VOL= VOL- RIRWDL
          IF (VOL .LT. IRMINV) THEN
C           correct for roundoff error
            VOL= IRMINV
          END IF
C
          IF (NCATS .GE. 1) THEN
C           allocate categories for irrigation withdrawal
C
C           first set prior category volume fractions
            DO 5 IC= 1, NCATS
              IF (VOL .GT. 0) THEN
C               fraction of total belonging to this category
                CFRAC(IC)= CVOL(IC)/ VOL
              ELSE
C               set fraction to default
                CFRAC(IC)= 1.0/NCATS
              END IF
 5          CONTINUE
C
C           now do actual allocation
            RTMP= RIRWDL
            CALL CATFRC (RTMP,NCATS,CFVOL(IREXIT),MXCUSR,
     I                   CFVOLC(1,IREXIT),CFVOLP(1,IREXIT),
     I                   CFVOLF(1,IREXIT),
     M                   CVOL,CFRAC,
     O                   CIRRWD)
          END IF
C
C         recalculate other state variables needed for interval initial values
          I= 0
          CALL DISCH (SCLU,I)
          IF (AUX1FG .GE. 1) THEN
C           recompute surface area and depth for later calculations
            CALL FNDROW (NROWS,NCOLS,RCHTAB,VOL,RCHNO,MESSU,MSGFL,SCLU,
     I                   DATIM,
     M                   ROWPT,HYWCNT)
            ROWADD= (ROWPT- 1)*NCOLS+ 1
            CALL AUXIL (NCOLS,RCHTAB,ROWADD,VOL,STCOR,LEN,RCHNO,MESSU,
     I                  MSGFL,DATIM,SCLU,AUX1FG,
     M                  HYECNT,
     O                  DEP,STAGE,SAREA,AVDEP,TWID,HRAD)
          END IF
C
C         set outflow demand for irrigation exit
          IRRDEM= RIRWDL/DELTS
        ELSE
C         no irrigation withdrawal
          IRRDEM= 0.0
C         recompute total routed outflow rate
          RO= 0.0
          DO 7 IX= 1, NEXITS
            IF (IX .NE. IREXIT) THEN
              RO= RO+ O(IX)
            END IF
 7        CONTINUE
        END IF
      END IF
C
C     save values of state variables at start of interval
      VOLS= VOL
      SAS= SAREA
      ROS= RO
      DO 20 IX= 1, NEXITS
        IF (IX .EQ. IREXIT) THEN
C         routing doesn't depend on previous irrigation withdrawal
          OS(IX)= 0.0
        ELSE
C         normal demand
          OS(IX)= O(IX)
        END IF
        DO 10 IC= 1, NCATS
          COTDGS(IX,IC)= COTDGT(IX,IC)
          CORELS(IX,IC)= COREL(IX,IC)
 10     CONTINUE
 20   CONTINUE
C
C     inputs to reach (if any)
C
      IF (NCATS .LE. 0) THEN
C       no categories, so use general inflow
        IF (IVOLFP .GT. 0) THEN
C         get volume of inflow entering thru gate inflo
          IVOL= PAD(IVOLFP + IVL1)
        ELSE
C         no inflow thru gate inflo
          IVOL= 0.0
        END IF
      ELSE
C       look for category input into this reach
        IVOL= 0.0
        DO 30 IC= 1, NCATS
          IF (CIVLFP(IC) .GT. 0) THEN
C           inflow for this category
            CIVOL(IC)= PAD(CIVLFP(IC) + IVL1)
C           also increment total inflow volume
            IVOL= IVOL+ CIVOL(IC)
          ELSE
C           no inflow for this category
            CIVOL(IC)= 0.0
          END IF
 30     CONTINUE
      END IF
C
C     calculate volume of water in system after inflow only
      VOLT= VOLS+ IVOL
C
      IF (NCATS .GE. 1) THEN
C       adjust category volumes
        DO 40 IC= 1, NCATS
          CVOLT(IC)= CVOL(IC)+ CIVOL(IC)
          IF (VOLT .GT. 0) THEN
C           fraction of total belonging to this category
            CFRAC(IC)= CVOLT(IC)/ VOLT
          ELSE
C           set fraction to default
            CFRAC(IC)= 1.0/NCATS
          END IF
 40     CONTINUE
      END IF
C
      IF (PRECFP .GT. 0 .AND. AUX1FG .NE. 0) THEN
C       find quantity of water contributed by precip on surface
        PREC= PAD(PRECFP+ IVL1)
        PRSUPY= PREC*SAS
C
C       calc volume in reach after inflow and precip
        VOLT= VOLT+ PRSUPY
C
        IF (NCATS .GE. 1) THEN
C         adjust category volumes within reach for precip
          IF (CPREC .EQ. 0) THEN
C           prorate among all categories by fraction
            DO 50 IC= 1, NCATS
              CVOLT(IC)= CVOLT(IC)+ (PRSUPY*CFRAC(IC))
 50         CONTINUE
          ELSE
            IF (CPREC .GT. 0) THEN
C             all precip to one category
              CVOLT(CPREC)= CVOLT(CPREC)+ PRSUPY
            ELSE IF (CPREC .LT. 0) THEN
C             use given fractions
              DO 55 I= 1, -CPREC
                CVOLT(CPRECC(I))= CVOLT(CPRECC(I))+ PRSUPY*CPRECF(I)
 55           CONTINUE
            END IF
C
            IF (VOLT .GT. 0) THEN
C             adjust fraction of total belonging to each category
              DO 60 IC= 1, NCATS
                IF (VOLT .GT. 0) THEN
C                 fraction of total belonging to this category
                  CFRAC(IC)= CVOLT(IC)/ VOLT
                ELSE
C                 set fraction to default
                  CFRAC(IC)= 1.0/NCATS
                END IF
 60           CONTINUE
            END IF
C
          END IF
        END IF
      ELSE
C       no water contributed by precip on surface
        PRSUPY= 0.0
      END IF
C
      IF (PEVFP .GT. 0 .AND. AUX1FG .NE. 0) THEN
C       subtract evaporation, determine potential
        POTEV(1)= PAD(PEVFP+ IVL1)
        VOLPEV= POTEV(1)*SAS
        IF (VOLPEV .GE. VOLT) THEN
C         evap has emptied the reach, all current volume to evap
          VOLEV= VOLT
          VOLT= 0.0
          IF (NCATS .GE. 1) THEN
C           all categories empty
            DO 70 IC= 1, NCATS
              CVOLT(IC)= 0.0
              CFRAC(IC)= 1.0/NCATS
 70         CONTINUE
          END IF
        ELSE
C         take out just what was required, adjust volume in reach
          VOLEV= VOLPEV
          VOLT= VOLT- VOLEV
          IF (NCATS .GE. 1) THEN
C           adjust category volumes
            CALL CATFRC (VOLEV,NCATS,CEVAP,MXCUSR,CEVAPC,CEVAPP,CEVAPF,
     M                   CVOLT,CFRAC,
     O                   CVOLEV)
          END IF
        END IF
              
      ELSE
C       dont consider evap
        VOLEV= 0.0
      END IF
C
C     get any input time series which govern outflow demands
      DO 120 IX= 1, NEXITS
        FG= ODGTFG(IX)
        IF (FG .GT. 0) THEN
C         there is a gt demand for this exit
          IF (NCATS .GE. 1) THEN
C           check for separate category demand time series
            OUTDGT(FG)= 0.0
            DO 110 IC= 1, NCATS
              IF (CODGTX(FG,IC) .GT. 0) THEN
C               an outflow demand has a component derived directly
C               from an input time series
                COTDGT(FG,IC)= PAD(CODGTX(FG,IC) + IVL1)
                OUTDGT(FG)= OUTDGT(FG)+ COTDGT(FG,IC)
              END IF
 110        CONTINUE
          ELSE
C           an outflow demand has a component derived directly
C           from a single input time series
            IF (ODGTFP(FG) .GT. 0) THEN
C             a time series was provided for this outflow demand
              OUTDGT(FG)= PAD(ODGTFP(FG) + IVL1)
            END IF
          END IF
        END IF
C
        IF (COLIFP(IX) .GT. 0) THEN
C         an outflow demand has a component f(vol), where the function
C         f varies with time - time series colind indicates which pair
C         of columns in rchtab must be used to evaluate f(vol)
          COLIND(IX)= PAD(COLIFP(IX) + IVL1)
        END IF
 120  CONTINUE
C
C     find the state of the reach or reservoir at the end of
C     the time interval and the outflows during the interval
C
      IF (NODFV .GT. 0) THEN
C       at least one outflow demand depends on volume
C       thus, hydraulic routing is required
        CALL ROUTE (DAYFG,VCONFG,NEXITS,MXEXIT,CONVFM,DELTS,KS,COKS,OS,
     I              ROS,VOLT,NROWS,NCOLS,RCHTAB,VZERPT,FACTA1,FUNCT,MON,
     I              NXTMON,DAY,NDAYS,ODFVFG,ODGTFG,COLIND,OUTDGT,COTDGT,
     I              RCHNO,MESSU,MSGFL,SCLU,DATIM,MXCAT,NCATS,MXCUSR,
     I              NCOGT,COGTC,COGTE,CFVOL,CFVOLC,CFVOLP,CFVOLF,IREXIT,
     I              IRRDEM,CIRRWD,
     M              CORELS,CVOLT,CFRAC,CONVF,ROWPT,HYECNT,HYWCNT,
     O              VOL,O,RO,OVOL,ROVOL,COREL,CVOL,CO,CRO,COVOL,
     O              CROVOL)
      ELSE
C       no outflow demands depend on vol
        CALL NOROUT (VOLT,KS,COKS,OS,ROS,DELTS,NEXITS,MXEXIT,ODGTFG,
     I               OUTDGT,COTDGT,VZERPT,AUX1FG,NROWS,NCOLS,
     I               RCHTAB,RCHNO,MESSU,MSGFL,SCLU,DATIM,NCATS,NCOGT,
     I               COGTC,COGTE,IREXIT,IRRDEM,CIRRWD,
     M               CORELS,CVOLT,CFRAC,ROWPT,HYWCNT,
     O               VOL,O,RO,OVOL,ROVOL,COREL,CVOL,CO,CRO,COVOL,
     O               CROVOL)
      END IF
C
      IF (NCATS .GE. 1) THEN
C       do category checks
C
C       check for roundoff error
        CKVOL= 0.0
        DO 130 IC= 1, NCATS
Cthj @    IF (CVOL(IC) .LT. 0.0) CVOL(IC)= 0.0
          CKVOL= CKVOL+ CVOL(IC)
 130    CONTINUE
        IF (CKVOL .NE. VOL) THEN
C         correct for roundoff error
          IF (CKVOL .GT. 0.0) THEN
C           compute correction factor
            CCORR= VOL/CKVOL
            IF ( (CCORR .GT. 0.0) .AND.
     $           (ABS (CCORR- 1.0) .GT. 0.001) .AND.
     $           (VOL .GT. 0.01) ) THEN
C             error - category error too high
              CALL OMSTD (DATIM)
              CALL OMSTI (RCHNO)
              RTMP= VOL
              CALL OMSTR (RTMP)
              RTMP= CKVOL
              CALL OMSTR (RTMP)
              RTMP= CCORR
              CALL OMSTR (RTMP)
              SGRP= 7
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   HYECNT(3))
            END IF
C
            DO 140 IC= 1, NCATS
              CVOL(IC)= CVOL(IC)*CCORR
 140        CONTINUE
          ELSE
C           error - cannot correct
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            RTMP= VOL
            CALL OMSTR (RTMP)
            RTMP= CKVOL
            CALL OMSTR (RTMP)
            SGRP= 8
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 HYECNT(6))
            CVOL(1)= VOL
          END IF
        END IF
C
C       check for demand deficits
        DO 160 IX= 1, NEXITS
          FG= ODGTFG(IX)
          IF (FG .GT. 0) THEN
C           check for deficits from this exit
            DO 150 IC= 1, NCATS
              IF (CODGTX(FG,IC) .GT. 0) THEN
C               see if current demand was met
                DEMVOL= (COTDGS(FG,IC)*KS+ COTDGT(FG,IC)*COKS)*DELTS
                IF (DEMVOL .GT. COVOL(IX,IC)) THEN
C                 there was a deficit
                  CDFVOL(IX,IC)= CDFVOL(IX,IC)+ DEMVOL- COVOL(IX,IC)
                END IF
              END IF
 150        CONTINUE
          END IF
 160    CONTINUE
      END IF
C
      IF (AUX1FG .GE. 1) THEN
C       compute depth, surface area, average depth and topwidth
C       get vol in single precision
        ROWADD= (ROWPT - 1)*NCOLS + 1
        CALL AUXIL (NCOLS,RCHTAB,ROWADD,VOL,STCOR,LEN,
     I              RCHNO,MESSU,MSGFL,DATIM,SCLU,AUX1FG,
     M              HYECNT,
     O              DEP,STAGE,SAREA,AVDEP,TWID,HRAD)
C
        IF (AUX2FG .EQ. 1) THEN
C         calc av velocity and cross section
          IF (VOL .GT. 0.0) THEN
C           av velocity
            AVVEL= LEN*RO/VOL
C           av cross section
            AVSECT= VOL/LEN
          ELSE
C           no water, no velocity or av cross section
            AVVEL= 0.0
            AVSECT= 0.0
          END IF
C
          IF (AUX3FG .EQ. 1) THEN
C           calculate bed shear stress and shear velocity
            CALL SHEAR (LKFG,AVDEP,AVVEL,HRAD,DB50,AKAPPA,GAM,
     I                  GRAV,SLOPE,
     O                  USTAR,TAU)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DEMAND
     I                   (NCOLS,TABROW,NEXITS,ODFVFG,ODGTFG,
     I                    CONVF,COLIND,OUTDGT,FUNCT,MXCAT,NCATS,MAXEX,
     I                    COREL,VOLT,DELTS,
     O                    OD,ROD)
C
C     + + + PURPOSE + + +
C     Find OD(*),ROD which apply at a specified row in RCHTAB.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NCOLS,NEXITS,ODFVFG(NEXITS),ODGTFG(NEXITS),
     $                 FUNCT(NEXITS),MXCAT,NCATS,MAXEX
      REAL             TABROW(NCOLS),CONVF,COLIND(NEXITS),
     $                 OUTDGT(NEXITS),COREL(MAXEX,MXCAT),DELTS,
     $                 OD(NEXITS),ROD
      DOUBLE PRECISION VOLT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCOLS  - number of columns in ftable
C     TABROW - current ftable rows values
C     NEXITS - number of exits from the operation
C     ODFVFG - flag indicating outflow as function of volume for each exit
C     ODGTFG - flag indicating outflow as function of time for each exit
C     CONVF  - f(vol) adjustment factor
C     COLIND - column containing outflow demand (may require interp between 2)
C     OUTDGT - current time varing outflow demand amount
C     FUNCT  - user-specified function for combining outflow demand components
C     MXCAT  - max number of categories
C     NCATS  - number of categories active
C     MAXEX  - maximum number of exits
C     COREL  - current time-varying outflow demand amount for each category
C              adjusted for category shortage
C     VOLT   - volume in the reach after inflow, precip and evap
C     DELTS  - simulation interval in seconds
C     OD     - rate of demanded outflow for end of interval by exit
C     ROD    - total rate of demanded outflow for end of interval
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    COL1,COL2,FG1,FG2,IC,IX
      REAL       ODFV(MXEXIT),ODGT(MXEXIT),OD1,OD2,RCOL,RCOL1,RTMP
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,IFIX,MAX,MIN
C
C     + + + END SPECIFICATIONS + + +
C
      ROD= 0.
C
C     get function of volume or irrigation demand
      DO 10 IX= 1,NEXITS
        FG1= ODFVFG(IX)
        IF (FG1 .NE. 0) THEN
C         outflow demand n has a f(vol) component
          IF (FG1 .GT. 0) THEN
C           flag-ptr points directly to col containing f(vol) in rchtab
            ODFV(IX)= TABROW(FG1)*CONVF
          ELSE
C           i is element of array colind(*) which indicates the col
C           nos containing f(vol) in rchtab for this outflow demand
C           find the columns to use
            FG1= ABS(FG1)
            RCOL= COLIND(FG1)
            COL1= IFIX (RCOL)
            COL2= COL1 + 1
            RCOL1= COL1
C
            IF ((ABS(RCOL - RCOL1)) .GE. 1.0E-6) THEN
C             interpolate between two adjacent columns in rchtab
              OD1= TABROW(COL1)
              OD2= TABROW(COL2)
              ODFV(IX)= (OD1+ (RCOL- RCOL1)*(OD2- OD1))*CONVF
            ELSE
C             use the value in a single column
              ODFV(IX)= TABROW(COL1)*CONVF
            END IF
          END IF
        END IF
 10   CONTINUE
C
C     get function of time demand
      DO 30 IX= 1, NEXITS
        FG2= ODGTFG(IX)
        IF (FG2 .GE. 1) THEN
C         outflow demand also has a g(t) component
          IF (NCATS .LE. 0) THEN
C           no categories
            ODGT(IX)= OUTDGT(FG2)
          ELSE
C           categories
            ODGT(IX)= 0.0
            DO 20 IC= 1, NCATS
              ODGT(IX)= ODGT(IX)+ COREL(FG2,IC)
 20         CONTINUE
          END IF
        ELSE
C         no g(t) component
          ODGT(IX)= 0.0
        END IF
 30   CONTINUE
C
C     combine the two outflow demand components using the
C     user-specified function
C     casentry funct(n)
      DO 40 IX= 1, NEXITS
        OD(IX)= 0.0
        IF (ODFVFG(IX) .NE. 0) THEN
C         there is a function of colume demand
          IF (ODGTFG(IX) .LE. 0) THEN
C           there is no function of time demand
            OD(IX)= ODFV(IX)
          ELSE
C           combine - start case
            IF (FUNCT(IX) .EQ. 2) THEN
C             case max
              OD(IX)= MAX (ODFV(IX),ODGT(IX))
            ELSE IF (FUNCT(IX) .EQ. 4) THEN
C             case max with special reservoir operation. 
C             In this case, odgt is a time series of target volumes
C             instead of outflow rate.
C
C             The total outflow demand is computed as the maximum of
C              1) the volume-dependent demand (min flow for example) and
C              2) the difference between the actual volume and
C                 the target volume.
C
              RTMP= (VOLT- ODGT(IX))/DELTS
              OD(IX)= MAX (ODFV(IX),RTMP)
            ELSE IF (FUNCT(IX) .EQ. 3) THEN
C             case sum
              OD(IX)= ODFV(IX)+ ODGT(IX)
            ELSE
C             case min
              OD(IX)= MIN (ODFV(IX),ODGT(IX))
            END IF
          END IF
        ELSE IF (ODGTFG(IX) .GT. 0) THEN
C         there is function of time demand
          OD(IX)= ODGT(IX)
        ELSE
C         there are no demands
          OD(IX)= 0.0
        END IF
C       find total outflow demand
        ROD= ROD + OD(IX)
 40   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CATADJ
     I                   (NEXITS,MXCATS,NCATS,MAXEX,COTDGT,KS,COKS,
     I                    DELTS,FVFG,CVOLT,CFRAC,MXUSRC,CFVOL,CFVOLC,
     I                    CFVOLP,CFVOLF,NCOGT,COGTC,COGTE,ODGTFG,
     M                    CORELS,OSEFF,ROSEFF,
     O                    COREL)
C
C     + + + PURPOSE + + +
C     Adjust category demand time series when current category storage
C     is not sufficient.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NEXITS,MXCATS,NCATS,MAXEX,FVFG,MXUSRC,
     $                 CFVOL(NEXITS),CFVOLC(MXUSRC,NEXITS),
     $                 CFVOLP(MXUSRC,NEXITS),NCOGT,COGTC(MXUSRC),
     $                 COGTE(MXUSRC),ODGTFG(NEXITS)
      REAL             COTDGT(MAXEX,MXCATS),KS,COKS,DELTS,
     $                 CFVOLF(MXUSRC,NEXITS),CORELS(MAXEX,MXCATS),
     $                 OSEFF(NEXITS),ROSEFF,COREL(MAXEX,MXCATS)
      DOUBLE PRECISION CVOLT(MXCATS),CFRAC(MXCATS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NEXITS - number of exits from the operation
C     MXCATS - max number of categories
C     NCATS  - number of categories active
C     MAXEX  - maximum number of exits
C     COTDGT - value of category demand time series at end of interval
C     KS     - weighting factor
C     COKS   - complement of KS (1-KS)
C     DELTS  - simulation interval in seconds
C     FVFG   - flag indicating whether f(vol) demands are being considered
C     CVOLT  - volume in each category after inflow, precip, and evap
C     CFRAC  - fraction of total volume belonging to each category
C     MXUSRC - maximum number of user-specified category allocations
C     CFVOL  - if positive, the primary category for fvol demands from each
C              exit; if negative, the number of categories specifed for such
C              demands
C     CFVOLC - category of an fvol allocation
C     CFVOLP - priority of an fvol allocation
C     CFVOLF - fraction of an fvol allocation
C     NCOGT  - number of category demands
C     COGTC  - category of a category demand
C     COGTE  - exit of a category demand
C     ODGTFG - flag indicating outflow as function of time for each exit
C     CORELS - time-varying outflow demand amount for each category
C              at beginning of interval - input: original; output: effective
C     OSEFF  - effective starting flow rate from each exit
C     ROSEFF - total effective starting flow rate
C     COREL  - current time-varying outflow demand amount for each category
C              adjusted for category shortage
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
      INCLUDE 'phcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          N,FG,IC,IX,IG
      REAL             R0,KSF,COKSF,COINT,FOS(MXEXIT),FOVOL,FCOV(MXCAT),
     $                 CSHORT
      DOUBLE PRECISION CVINT(MXCAT)
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL COPYD,ZIPR,COPYR,CATFRC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R0/0.0/
C
C     + + + END SPECIFICATIONS + + +
C
C     vol intercept begins as starting volume
      CALL COPYD (NCATS,CVOLT,
     O            CVINT)
      N= MXEXIT*NCATS
      CALL ZIPR (N,R0,
     O           COREL)
C
C     calculate factors for ks and coks
      KSF= KS*DELTS
      COKSF= COKS*DELTS
C
      IF (KS .GT. 0.0) THEN
C       check ks term
C
        IF (FVFG .GE. 1) THEN
C       check ks term of fvol water
C
          CALL COPYR (NEXITS,OSEFF,
     O                FOS)
C
C         subtract off category demands to determine fvol ks water
          DO 10 IG= 1, NCOGT
            IC= COGTC(IG)
            IX= COGTE(IG)
            FG= ODGTFG(IX)
            FOS(IX)= FOS(IX)- CORELS(FG,IC)
            IF (ABS (FOS(IX)) .LT. OSEFF(IX)*1.0E-5) THEN
C             prevent underflow
              FOS(IX)= 0.0
            END IF
 10       CONTINUE
C
C         allocate fvol water and reduce volume intercept
          DO 20 IX= 1, NEXITS
            FOVOL= FOS(IX)*KS*DELTS
            CALL CATFRC (FOVOL,NCATS,CFVOL(IX),MXUSRC,CFVOLC(1,IX),
     I                   CFVOLP(1,IX),CFVOLF(1,IX),
     M                   CVINT,CFRAC,
     O                   FCOV)
 20       CONTINUE
        END IF
C
C       now check gt water
        DO 30 IG= 1, NCOGT
          IC= COGTC(IG)
          IX= COGTE(IG)
          FG= ODGTFG(IX)
C
C         determine maximum ks demand rate
          COINT= CVINT(IC)/KSF
C
          IF (COINT .LT. CORELS(FG,IC)) THEN
C           reduce effective starting flow rates
            CSHORT= CORELS(FG,IC)- COINT
            OSEFF(IX)= OSEFF(IX)- CSHORT
            ROSEFF= ROSEFF- CSHORT
C
C           reset volume intercept and reduce ks demand
            CORELS(FG,IC)= COINT
            CVINT(IC)= 0.0
          ELSE
C           reduce volume intercept
            CVINT(IC)= CVINT(IC)- DBLE (CORELS(FG,IC)*KSF)
            IF (ABS (CVINT(IC)) .LT. CVOLT(IC)*1.0E-5) THEN
C             prevent underflow
              CVINT(IC)= 0.0
            END IF
          END IF
 30     CONTINUE
      END IF
C
C     loop through demands from highest to lowest priority to satisfy
C     as many as possible
      DO 40 IG= 1, NCOGT
        IC= COGTC(IG)
        IX= COGTE(IG)
        FG= ODGTFG(IX)
C
C       determine maximum coks rate
        COINT= CVINT(IC)/COKSF
        IF (COINT .LE. COTDGT(FG,IC)) THEN
C         not enough water for coks demand
          CVINT(IC)= 0.0
          COREL(FG,IC)= COINT
        ELSE
C         enough water for coks demand
          CVINT(IC)= CVINT(IC)- DBLE (COTDGT(FG,IC)*COKSF)
          IF (ABS (CVINT(IC)) .LT. CVOLT(IC)*1.0E-5) THEN
C           prevent underflow
            CVINT(IC)= 0.0
          END IF
          COREL(FG,IC)= COTDGT(FG,IC)
        END IF
C
 40   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CATALL
     I                   (NEXITS,MAXEX,OSEFF,O,KS,COKS,DELTS,MXCATS,
     I                    NCATS,CVOLT,CORELS,COREL,FVFG,CFVOL,MXUSRC,
     I                    CFVOLC,CFVOLP,CFVOLF,ODGTFG,NCOGT,COGTC,
     I                    COGTE,FUNCT,IREXIT,
     M                    CFRAC,
     O                    CVOL,CO,CRO,COVOL,CROVOL)
C
C     + + + PURPOSE + + +
C     Allocate final storages, outflow rates, and outflow volumes to
C     categories.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NEXITS,MAXEX,MXCATS,NCATS,FVFG,CFVOL(NEXITS),
     $                 MXUSRC,CFVOLC(MXUSRC,NEXITS),
     $                 CFVOLP(MXUSRC,NEXITS),
     $                 ODGTFG(NEXITS),NCOGT,COGTC(MXUSRC),COGTE(MXUSRC),
     $                 FUNCT(NEXITS),IREXIT
      REAL             OSEFF(NEXITS),O(NEXITS),KS,COKS,DELTS,
     $                 CORELS(MAXEX,MXCATS),COREL(MAXEX,MXCATS),
     $                 CFVOLF(MXUSRC,NEXITS),CO(MAXEX,MXCATS),
     $                 CRO(MXCATS),COVOL(MAXEX,MXCATS),CROVOL(MXCATS)
      DOUBLE PRECISION CVOLT(MXCATS),CFRAC(MXCATS),CVOL(MXCATS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NEXITS - number of exits from the operation
C     MAXEX  - maximum number of exits
C     OSEFF  - effective starting flow rate from each exit
C     O      - rate of outflow from each reach exit at end of interval
C     KS     - weighting factor
C     COKS   - complement of KS (1-KS)
C     DELTS  - simulation interval in seconds
C     MXCATS - max number of categories
C     NCATS  - number of categories active
C     CVOLT  - volume in each category after inflow, precip, and evap
C     CORELS - time-varying outflow demand amount for each category
C              at beginning of interval - input: original; output: effective
C     COREL  - current time-varying outflow demand amount for each category
C              adjusted for category shortage
C     FVFG   - flag indicating whether there is fvol water to allocate
C     CFVOL  - if positive, the primary category for fvol demands from each
C              exit; if negative, the number of categories specifed for such
C              demands
C     MXUSRC - maximum number of user-specified category allocations
C     CFVOLC - category of an fvol allocation
C     CFVOLP - priority of an fvol allocation
C     CFVOLF - fraction of an fvol allocation
C     ODGTFG - flag indicating outflow as function of time for each exit
C     NCOGT  - number of category demands
C     COGTC  - category of a category demand
C     COGTE  - exit of a category demand
C     FUNCT  - user-specified function for combining outflow demand components
C     IREXIT - exit number for irrigation withdrawals
C     CFRAC  - fraction of total volume belonging to each category
C     CVOL   - volume of water in category above bed
C     CO     - rate of outflow from each category exit at end of interval
C     CRO    - total rate of outflow from category at end of interval
C     COVOL  - volume of water leaving category in an interval by exit
C     CROVOL - total volume of water leaving category in an interval
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
      INCLUDE 'phcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER N,IG,IC,IX,FG
      REAL    R0,FOS(MXEXIT),FO(MXEXIT),FOVOL,FCOVKS(MXCAT),
     $        FCOVCK(MXCAT),FCO
      DOUBLE PRECISION LVOL
C
C     + + + INTRINSICS + + +
      INTRINSIC  DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPR,COPYD,COPYR,CATFRC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R0/0.0/
C
C     + + + END SPECIFICATIONS + + +
C
C     reset category variables
      N= MAXEX*NCATS
      CALL ZIPR (N,R0,
     O           COVOL)
      CALL ZIPR (N,R0,
     O           CO)
      CALL ZIPR (NCATS,R0,
     O           CROVOL)
      CALL ZIPR (NCATS,R0,
     O           CRO)
      CALL COPYD (NCATS,CVOLT,
     O            CVOL)
C
C     initialize fvol rates
      CALL COPYR (NEXITS,OSEFF,
     O            FOS)
      CALL COPYR (NEXITS,O,
     O            FO)
C
C     allocate gt water and get fvol rates
      DO 10 IG= 1, NCOGT
        IC= COGTC(IG)
        IX= COGTE(IG)
        FG= ODGTFG(IX)
C
C       subtract all demands from appropriate category in priority order
C
        FOS(IX)= FOS(IX)- CORELS(FG,IC)
        IF (FOS(IX) .LT. 0.0) THEN
C         prevent underflow
          FOS(IX)= 0.0
        END IF
        IF ( (FUNCT(IX) .EQ. 1) .AND. (FO(IX) .LT. COREL(FG,IC)) ) THEN
C         outflow was reduced for this exit due to min(f,g)
          COREL(FG,IC)= FO(IX)
          FO(IX)= 0.0
        ELSE
C         outflow was not reduced
          FO(IX)= FO(IX)- COREL(FG,IC)
        END IF
        IF (FO(IX) .LT. 0.0) THEN
C         prevent underflow
          FO(IX)= 0.0
        END IF
        CO(IX,IC)= CO(IX,IC)+ COREL(FG,IC)
        CRO(IC)= CRO(IC)+ COREL(FG,IC)
        COVOL(IX,IC)= (CORELS(FG,IC)*KS+ COREL(FG,IC)*COKS)*DELTS
        CROVOL(IC)= CROVOL(IC)+ COVOL(IX,IC)
        CVOL(IC)= CVOL(IC)- DBLE (COVOL(IX,IC))
        IF (CVOL(IC) .LT. 0.0) THEN
C         prevent underflow
          CVOL(IC)= 0.0
        END IF
 10   CONTINUE
C
      IF (NCOGT .GE. 1) THEN
C       volume fractions have changed
C
C       get total available
        LVOL= 0.0
        DO 20 IC= 1, NCATS
          LVOL= LVOL+ CVOL(IC)
 20     CONTINUE
        DO 30 IC= 1, NCATS
          IF (LVOL .GT. 0.0) THEN
C           compute fraction
            CFRAC(IC)= CVOL(IC)/LVOL
          ELSE
C           reset fraction
            CFRAC(IC)= 1.0/NCATS
          END IF
 30     CONTINUE
      END IF
C
      IF (FVFG .GT. 0) THEN
C       now allocate fvol water
        DO 50 IX= 1, NEXITS
C
          IF (IX .NE. IREXIT) THEN
C           allocate ks water
            FOVOL= FOS(IX)*KS*DELTS
            IF (FOVOL .GT. 0.0) THEN
C             there is water to allocate
              CALL CATFRC (FOVOL,NCATS,CFVOL(IX),MXUSRC,CFVOLC(1,IX),
     I                     CFVOLP(1,IX),CFVOLF(1,IX),
     M                     CVOL,CFRAC,
     O                     FCOVKS)
            ELSE
C             no water
              CALL ZIPR (NCATS,R0,
     O                   FCOVKS)
            END IF
C
C           allocate coks water
            FOVOL= FO(IX)*COKS*DELTS
            IF (FOVOL .GT. 0.0) THEN
C             there is water to allocate
              CALL CATFRC (FOVOL,NCATS,CFVOL(IX),MXUSRC,CFVOLC(1,IX),
     I                     CFVOLP(1,IX),CFVOLF(1,IX),
     M                     CVOL,CFRAC,
     O                     FCOVCK)
            ELSE
C             no water
              CALL ZIPR (NCATS,R0,
     O                   FCOVCK)
            END IF
            DO 40 IC= 1, NCATS
C             add this outflow
              FCO= FCOVCK(IC)/(COKS*DELTS)
              CO(IX,IC)= CO(IX,IC)+ FCO
              CRO(IC)= CRO(IC)+ FCO
              COVOL(IX,IC)= COVOL(IX,IC)+ FCOVKS(IC)+ FCOVCK(IC)
              CROVOL(IC)= CROVOL(IC)+ FCOVKS(IC)+ FCOVCK(IC)
 40         CONTINUE
          END IF
 50     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FNDROW
     I                   (NROWS,NCOLS,RCHTAB,VOL,RCHNO,MESSU,MSGFL,SCLU,
     I                    DATIM,
     M                    ROWPT,HYWCNT)
C
C     + + + PURPOSE + + +
C     Search RCHTAB, starting at the row indicated by ROWPT, until
C     we reach the row which is the lower boundary of the interval
C     containing VOL. ROWPT is updated to point to this row.
C     If finding VOL requires extrapolation of the RCHTAB, we keep in
C     the last interval of the table, with ROWPT pointing to
C     Nrows-1.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NROWS,NCOLS,RCHNO,MESSU,MSGFL,SCLU,DATIM(5),
     $                 ROWPT,HYWCNT(2)
      REAL             RCHTAB(500)
      DOUBLE PRECISION VOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROWS  - number of rows in ftable
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     VOL    - volume of water in reach above bed
C     RCHNO  - reach number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing error and warning message details
C     DATIM  - date and time of day
C     ROWPT  - starting row to search in ftable, updated for next time
C     HYWCNT - count of warning messages for section HYDR
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    MOVE,ROWAD1,ROWAD2,SGRP
      REAL       V1,V2,VOLSP
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTD,OMSG,OMSTI,OMSTR
C
C     + + + END SPECIFICATIONS + + +
C
      VOLSP= VOL
C
C     dountil MOVE=0
 10   CONTINUE
        ROWAD1= (ROWPT - 1)*NCOLS + 1
        V1= RCHTAB(ROWAD1 + 2)
        ROWAD2= ROWAD1 + NCOLS
        V2= RCHTAB(ROWAD2 + 2)
C
C       check whether VOL is between V1 and V2. if not, indicate which
C       way to move in RCHTAB. if VOL requires extrapolation of the
C       table, give a warning and indicate to stay in the last interval
        IF (VOLSP .GT. V2) THEN
C         move up a row
          MOVE= 1
          ROWPT= ROWPT + 1
C         check for extrapolation
          IF (ROWPT .EQ. NROWS) THEN
C           warn that extrapolation of rchtab will take place
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTI (NROWS)
            CALL OMSTR (V1)
            CALL OMSTR (V2)
            CALL OMSTR (VOLSP)
            SGRP= 6
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 HYWCNT(1))
C           back to top row
            ROWPT= ROWPT - 1
C           no more movement
            MOVE= 0
          END IF
        ELSE IF (VOLSP .LT. V1) THEN
C         move down a row
          MOVE=  -1
          ROWPT= ROWPT - 1
        ELSE
C         got to correct spot
          MOVE= 0
        END IF
      IF (MOVE .NE. 0) GO TO 10
C     end dountil
C
      RETURN
      END
C
C
C
      SUBROUTINE   NOROUT
     I                   (VOLT,KS,COKS,OS,ROS,DELTS,NEXITS,MAXEX,ODGTFG,
     I                    OUTDGT,COTDGT,VZERPT,AUX1FG,NROWS,NCOLS,
     I                    RCHTAB,RCHNO,MESSU,MSGFL,SCLU,DATIM,NCATS,
     I                    NCOGT,COGTC,COGTE,IREXIT,IRRDEM,CIRRWD,
     M                    CORELS,CVOLT,CFRAC,ROWPT,HYWCNT,
     O                    VOL,O,RO,OVOL,ROVOL,COREL,CVOL,CO,CRO,COVOL,
     O                    CROVOL)
C
C     + + + PURPOSE + + +
C     Find the state of the reach or reservoir at the end of
C     the time interval and the outflows from it.
C
C     This algorithm is used where none of the outflow demands
C     depend on VOL.  Hence, hydraulic routing is not required.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NEXITS,MAXEX,ODGTFG(NEXITS),VZERPT,AUX1FG,NROWS,
     $                 NCOLS,RCHNO,MESSU,MSGFL,SCLU,DATIM(5),NCATS,
     $                 NCOGT,COGTC(NCOGT),COGTE(NCOGT),IREXIT,ROWPT,
     $                 HYWCNT(2)
      REAL             KS,COKS,OS(NEXITS),ROS,DELTS,OUTDGT(NEXITS),
     $                 COTDGT(MAXEX,NCATS),RCHTAB(500),IRRDEM,
     $                 CIRRWD(NCATS),CORELS(MAXEX,NCATS),O(NEXITS),RO,
     $                 OVOL(NEXITS),ROVOL,COREL(MAXEX,NCATS),
     $                 CO(MAXEX,NCATS),CRO(NCATS),COVOL(MAXEX,NCATS),
     $                 CROVOL(NCATS)
      DOUBLE PRECISION VOLT,CVOLT(NCATS),CFRAC(NCATS),VOL,CVOL(NCATS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOLT   - volume in the reach after inflow, precip and evap
C     KS     - weighting factor
C     COKS   - complement of KS (1-KS)
C     OS     - rate of outflow from each exit of reach at start of interval
C     ROS    - total rate of outflow from reach at start of interval
C     DELTS  - simulation interval in seconds
C     NEXITS - number of exits from the operation
C     MAXEX  - maximum number of exits
C     ODGTFG - flag indicating outflow as function of time for each exit
C     OUTDGT - current time varing outflow demand amount
C     COTDGT - value of category demand time series at end of interval
C     VZERPT - pointer to highest row with zero volume
C     AUX1FG - auxiliary calculation flag - 0: no calculation
C                                 1: calc DEP,STAGE,AVDEP,TWID,HRAD,SAREA
C                                    if VOL > 0
C                                 2: calc same even if VOL = 0
C     NROWS  - number of rows in ftable
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     RCHNO  - reach number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing error and warning message details
C     DATIM  - date and time of day
C     NCATS  - number of categories active
C     NCOGT  - number of category demands
C     COGTC  - category of a category demand
C     COGTE  - exit of a category demand
C     IREXIT - exit number for irrigation demand
C     IRRDEM - rate of irrigation demand
C     CIRRWD - irrigation withdrawal from each category
C     CORELS - time-varying outflow demand amount for each category
C              at beginning of interval - input: original; output: effective
C     CVOLT  - volume in each category after inflow, precip, and evap
C     CFRAC  - fraction of total volume belonging to each category
C     ROWPT  - starting row to search in ftable, updated for next time
C     HYWCNT - count of warning messages for section HYDR
C     VOL    - volume of water in reach above bed
C     O      - rate of outflow from each reach exit at end of interval
C     RO     - total rate of outflow from reach at end of interval
C     OVOL   - volume of water leaving reach in an interval by exit
C     ROVOL  - total volume of water leaving reach in an interval
C     COREL  - current time-varying outflow demand amount for each category
C              adjusted for category shortage
C     CVOL   - volume of water in category above bed
C     CO     - rate of outflow from each category exit at end of interval
C     CRO    - total rate of outflow from category at end of interval
C     COVOL  - volume of water leaving category in an interval by exit
C     CROVOL - total volume of water leaving category in an interval
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
      INCLUDE 'phcat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          IC,IX,I0,IFIVE(5),CDUMI(MXCUSR)
      REAL             OD(MXEXIT),ROD,TRO,R0,R1,CDUMR(MXCUSR),RFIVE(5),
     $                 OINTSP,OSEFF(MXEXIT),ROSEFF,CIRRDM
      DOUBLE PRECISION OINT,VOLINT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYR,CATADJ,ZIPR,DEMAND,FNDROW,CATALL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,IFIVE/0,0,0,0,0,0/
      DATA R0,R1,RFIVE/0.0,1.0,0.0,0.0,0.0,0.0,0.0/
C
C     + + + END SPECIFICATIONS + + +
C
      ROSEFF= ROS
      CALL COPYR (NEXITS,OS,
     O            OSEFF)
      IF (NCATS .GE. 1) THEN
C       categories - see if any categories run out
        CALL CATADJ (NEXITS,MXCAT,NCATS,MAXEX,COTDGT,KS,COKS,DELTS,I0,
     I               CVOLT,CFRAC,MXCUSR,CDUMI,CDUMI,CDUMI,CDUMR,NCOGT,
     I               COGTC,COGTE,ODGTFG,
     M               CORELS,OSEFF,ROSEFF,
     O               COREL)
      END IF
C
C     find intercept of EQ 4 on VOL axis
      VOLINT= VOLT- KS*ROSEFF*DELTS
      IF (ABS (VOLINT) .LT. VOLT*1.0E-5) THEN
C       prevent underflow
        VOLINT= 0.0
      END IF
C
      IF (VOLINT .LE. 0.0) THEN
C       case 3 -- no solution to simultaneous equations
C       set state variables to reflect empty condition
        ROWPT= VZERPT
        VOL= 0.0
        RO= 0.0
        CALL ZIPR (NEXITS,R0,
     O             O)
        ROVOL= VOLT
        DO 10 IX= 1, NEXITS
          IF (ROSEFF .GT. 0.0) THEN
C           prorate by starting flow rates
            OVOL(IX)= ROVOL*OSEFF(IX)/ROSEFF
          ELSE
C           divide equally among exits
            OVOL(IX)= ROVOL/NEXITS
          END IF
 10     CONTINUE
      ELSE
C       case 1 or 2
C       find intercept of eq 4 on the o axis
        OINT= VOLINT/(DELTS*COKS)
        OINTSP= OINT
C
C       find the outflow demands
        CALL DEMAND (NCOLS,RCHTAB,NEXITS,IFIVE,ODGTFG,R1,RFIVE,
     I               OUTDGT,IFIVE,MXCAT,NCATS,MXEXIT,COREL,VOLT,DELTS,
     O               OD,ROD)
C
        IF (OINTSP .GE. ROD) THEN
C         case 1 -- outflow demands are met in full
          RO= ROD
          VOL= VOLINT- COKS*RO*DELTS
          IF (VOL .LT. 1.0E-5) THEN
C           prevent underflow
            VOL= 0.0
          END IF
          DO 100 IX= 1, NEXITS
            O(IX)= OD(IX)
 100      CONTINUE
        ELSE
C         case 2 -- outflow demands cannot be met in full
C         find the actual outflows -- satisfy as many as poss in full
C         satisfy others partially or not at all
C
          ROWPT= VZERPT
          VOL= 0.0
          RO = 0.0
C
          DO 105 IX= 1,NEXITS
            TRO= RO+ OD(IX)
            IF (TRO .LE. OINTSP) THEN
C             demand met
              O(IX)= OD(IX)
              RO= TRO
            ELSE
C             demand part met
              O(IX)= OINTSP- RO
              RO= OINTSP
            END IF
 105      CONTINUE
        END IF
C
        IF (AUX1FG .GE. 1) THEN
C         set rchtab row pointer, ready for subroutine auxil
          CALL FNDROW (NROWS,NCOLS,RCHTAB,VOL,RCHNO,MESSU,MSGFL,SCLU,
     I                 DATIM,
     M                 ROWPT,HYWCNT)
        END IF
C
        IF ( (IREXIT .GE. 1) .AND. (IRRDEM .GT. 0.0) ) THEN
C         an irrigation demand was made before routing
          OSEFF(IREXIT)= IRRDEM
          O(IREXIT)= IRRDEM
          ROSEFF= ROSEFF+ IRRDEM
          RO= RO+ IRRDEM
        END IF
C
C       estimate the volumes of outflow
        ROVOL= (KS*ROSEFF+ COKS*RO)*DELTS
        DO 110 IX= 1, NEXITS
          OVOL(IX)= (KS*OSEFF(IX)+ COKS*O(IX))*DELTS
 110    CONTINUE
      END IF
C
      IF (NCATS .GE. 1) THEN
C       allocate outflows among categories
        CALL CATALL (NEXITS,MXEXIT,OSEFF,O,KS,COKS,DELTS,MXCAT,NCATS,
     I               CVOLT,CORELS,COREL,I0,CDUMI,MXCUSR,CDUMI,CDUMI,
     I               CDUMR,ODGTFG,NCOGT,COGTC,COGTE,IFIVE,IREXIT,
     M               CFRAC,
     O               CVOL,CO,CRO,COVOL,CROVOL)
C
        IF ( (IREXIT .GE. 1) .AND. (IRRDEM .GT. 0.0) ) THEN
C         an irrigation demand was made before routing
          DO 120 IC= 1, NCATS
            CIRRDM= CIRRWD(IC)/DELTS
            CO(IREXIT,IC)= CIRRDM
            COVOL(IREXIT,IC)= CIRRWD(IC)
            CRO(IC)= CRO(IC)+ CIRRDM
            CROVOL(IC)= CROVOL(IC)+ CIRRWD(IC)
 120      CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ROUTE
     I                  (DAYFG,VCONFG,NEXITS,MAXEX,CONVFM,DELTS,KS,COKS,
     I                   OS,ROS,VOLT,NROWS,NCOLS,RCHTAB,VZERPT,FACTA1,
     I                   FUNCT,MON,NXTMON,DAY,NDAYS,ODFVFG,ODGTFG,
     I                   COLIND,OUTDGT,COTDGT,RCHNO,MESSU,MSGFL,SCLU,
     I                   DATIM,MXCAT,NCATS,MXCUSR,NCOGT,COGTC,COGTE,
     I                   CFVOL,CFVOLC,CFVOLP,CFVOLF,IREXIT,IRRDEM,
     I                   CIRRWD,
     M                   CORELS,CVOLT,CFRAC,CONVF,ROWPT,HYECNT,HYWCNT,
     O                   VOL,O,RO,OVOL,ROVOL,COREL,CVOL,CO,CRO,COVOL,
     O                   CROVOL)
C
C     + + + PURPOSE + + +
C     Find the state of the reach or reservoir at the end of
C     the time interval and the outflows from it, used if one or more
C     of the outflow demands depend on VOL.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          DAYFG,VCONFG,NEXITS,MAXEX,NROWS,NCOLS,VZERPT,
     $                 FUNCT(NEXITS),MON,NXTMON,DAY,NDAYS,
     $                 ODFVFG(NEXITS),ODGTFG(NEXITS),RCHNO,MESSU,MSGFL,
     $                 SCLU,DATIM(5),MXCAT,NCATS,MXCUSR,NCOGT,
     $                 COGTC(MXCUSR),COGTE(MXCUSR),CFVOL(NEXITS),
     $                 CFVOLC(MXCUSR,NEXITS),CFVOLP(MXCUSR,NEXITS),
     $                 IREXIT,ROWPT,HYECNT(5),HYWCNT(2)
      REAL             CONVFM(12),DELTS,KS,COKS,OS(NEXITS),ROS,
     $                 RCHTAB(500),FACTA1,COLIND(NEXITS),OUTDGT(NEXITS),
     $                 COTDGT(MAXEX,MXCAT),CFVOLF(MXCUSR,NEXITS),
     $                 IRRDEM,CIRRWD(NCATS),CORELS(MAXEX,MXCAT),CONVF,
     $                 O(NEXITS),RO,OVOL(NEXITS),ROVOL,
     $                 COREL(MAXEX,MXCAT),CO(MAXEX,MXCAT),CRO(MXCAT),
     $                 COVOL(MAXEX,MXCAT),CROVOL(MXCAT)
      DOUBLE PRECISION VOLT,CVOLT(MXCAT),CFRAC(MXCAT),VOL,CVOL(MXCAT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VCONFG - flag indicating outflow demand varies by month
C     NEXITS - number of exits from the operation
C     MAXEX  - maximum number of exits
C     CONVFM - monthly f(vol) adjustment factor
C     DELTS  - simulation interval in seconds
C     KS     - weighting factor
C     COKS   - complement of KS (1-KS)
C     OS     - rate of outflow from each exit of reach at start of interval
C     ROS    - total rate of outflow from reach at start of interval
C     VOLT   - volume in the reach after inflow, precip and evap
C     NROWS  - number of rows in ftable
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     VZERPT - pointer to highest row with zero volume
C     FACTA1 - term in eq 18 = 1.0/(COKS*DELTS)
C     FUNCT  - user-specified function for combining outflow demand components
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     ODFVFG - flag indicating outflow as function of volume for each exit
C     ODGTFG - flag indicating outflow as function of time for each exit
C     COLIND - column containing outflow demand (may require interp between 2)
C     OUTDGT - current time varing outflow demand amount
C     COTDGT - value of category demand time series at end of interval
C     RCHNO  - reach number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing error and warning message details
C     DATIM  - date and time
C     MXCAT  - max number of categories
C     NCATS  - number of categories active
C     NCOGT  - number of category demands
C     COGTC  - category of a category demand
C     COGTE  - exit of a category demand
C     CFVOL  - if positive, the primary category for fvol demands from each
C              exit; if negative, the number of categories specifed for such
C              demands
C     MXCUSR - maximum number of user-specified category allocations
C     CFVOLC - category of an fvol allocation
C     CFVOLP - priority of an fvol allocation
C     CFVOLF - fraction of an fvol allocation
C     IREXIT - exit number for irrigation demand
C     IRRDEM - rate of irrigation demand
C     CIRRWD - irrigation withdrawal from each category
C     CORELS - time-varying outflow demand amount for each category
C              at beginning of interval - input: original; output: effective
C     CVOLT  - volume in each category after inflow, precip, and evap
C     CFRAC  - fraction of total volume belonging to each category
C     CONVF  - f(vol) adjustment factor
C     ROWPT  - starting row to search in ftable, updated for next time
C     HYECNT - count of error messages for section HYDR
C     HYWCNT - count of warning messages for section HYDR
C     VOL    - volume of water in reach above bed
C     O      - rate of outflow from each reach exit at end of interval
C     RO     - total rate of outflow from reach at end of interval
C     OVOL   - volume of water leaving reach in an interval by exit
C     ROVOL  - total volume of water leaving reach in an interval
C     COREL  - current time-varying outflow demand amount for each category
C              adjusted for category shortage
C     CVOL   - volume of water in category above bed
C     CO     - rate of outflow from each category exit at end of interval
C     CRO    - total rate of outflow from category at end of interval
C     COVOL  - volume of water leaving category in an interval by exit
C     CROVOL - total volume of water leaving category in an interval
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          IC,IX,ROWAD,I1
      REAL             ODZ(MXEXIT),RODZ,TRO,R0,OSEFF(MXEXIT),ROSEFF,
     $                 OINTSP,CIRRDM
      DOUBLE PRECISION OINT,VOLINT
C
C     + + + FUNCTIONS + + +
      REAL             DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC        ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL         DAYVAL,COPYR,CATADJ,ZIPR,DEMAND,SOLVE,CATALL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R0/0.0/
      DATA I1/1/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       first interval of the day
        IF (VCONFG .EQ. 1) THEN
C         conversion factor for f(vol) discharge components is
C         allowed to vary monthly throughout the year,
C         interpolate for the daily value
          CONVF= DAYVAL (CONVFM(MON),CONVFM(NXTMON),DAY,NDAYS)
        ELSE
C         conversion factor for f(vol) discharge components does not
C         vary throughout the year
          CONVF= 1.0
        END IF
      END IF
C
      ROSEFF= ROS
      CALL COPYR (NEXITS,OS,
     O            OSEFF)
      IF (NCATS .GE. 1) THEN
C       categories - see if any categories run out
        CALL CATADJ (NEXITS,MXCAT,NCATS,MAXEX,COTDGT,KS,COKS,DELTS,
     I               I1,CVOLT,CFRAC,MXCUSR,CFVOL,CFVOLC,CFVOLP,
     I               CFVOLF,NCOGT,COGTC,COGTE,ODGTFG,
     M               CORELS,OSEFF,ROSEFF,
     O               COREL)
      END IF
C
C     find intercept of EQ 4 on VOL axis
      VOLINT= VOLT- KS*ROSEFF*DELTS
      IF (ABS (VOLINT) .LT. VOLT*1.0E-5) THEN
C       prevent underflow
        VOLINT= 0.0
      END IF
C
      IF (VOLINT .LE. 0.0) THEN
C       case 3 -- no solution to simultaneous equations
C       set state variables to reflect empty condition
        ROWPT= VZERPT
        VOL= 0.0
        RO= 0.0
        CALL ZIPR (NEXITS,R0,
     O             O)
        ROVOL= VOLT
        DO 10 IX= 1, NEXITS
          IF (ROSEFF .GT. 0.0) THEN
C           prorate by starting flow rates
            OVOL(IX)= ROVOL*OSEFF(IX)/ROSEFF
          ELSE
C           divide equally among exits
            OVOL(IX)= ROVOL/NEXITS
          END IF
 10     CONTINUE
C
      ELSE
C       case 1 or 2 -- solve simultaneous equations
C       find intercept on o axis (coks is 1.0 - ks)
        OINT= VOLINT/(DELTS*COKS)
        OINTSP= OINT
C
C       find outflow demand for zero volume
        ROWAD= (VZERPT- 1)*NCOLS + 1
        CALL DEMAND (NCOLS,RCHTAB(ROWAD),NEXITS,ODFVFG,ODGTFG,CONVF,
     I               COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I               MXEXIT,COREL,VOLT,DELTS,
     O               ODZ,RODZ)
C
        IF (OINTSP .GT. RODZ) THEN
C         case 1 -- outflow demands can be met in full
C         solve simultaneous equations
          CALL SOLVE (NROWS,NCOLS,RCHTAB,FACTA1,OINT,NEXITS,ODFVFG,
     I                ODGTFG,CONVF,COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I                MXEXIT,COREL,RCHNO,MESSU,MSGFL,SCLU,
     I                DATIM,VOLT,DELTS,
     M                ROWPT,HYECNT,HYWCNT,
     O                VOL,O,RO)
C
        ELSE
C         case 2 -- outflow demands cannot be met in full
          VOL= 0.0
          ROWPT= VZERPT
C         find the actual outflows -- satisfy as many as poss in full
C         satisfy others partially or not at all
C
          RO= 0.
          DO 100 IX= 1, NEXITS
            TRO= RO+ ODZ(IX)
            IF (TRO .LE. OINTSP) THEN
C             demand met
              O(IX)= ODZ(IX)
              RO= TRO
            ELSE
C             demand part met
              O(IX)= OINTSP - RO
              RO= OINTSP
            END IF
 100      CONTINUE
        END IF
C
        IF ( (IREXIT .GE. 1) .AND. (IRRDEM .GT. 0.0) ) THEN
C         an irrigation demand was made before routing
          OSEFF(IREXIT)= IRRDEM
          O(IREXIT)= IRRDEM
          ROSEFF= ROSEFF+ IRRDEM
          RO= RO+ IRRDEM
        END IF
C
C       estimate the volumes of outflow
        ROVOL= (KS*ROSEFF+ COKS*RO)*DELTS
        DO 110 IX= 1, NEXITS
          OVOL(IX)= (KS*OSEFF(IX)+ COKS*O(IX))*DELTS
 110    CONTINUE
      END IF
C
      IF (NCATS .GE. 1) THEN
C       allocate outflows among categories
        CALL CATALL (NEXITS,MXEXIT,OSEFF,O,KS,COKS,DELTS,MXCAT,NCATS,
     I               CVOLT,CORELS,COREL,I1,CFVOL,MXCUSR,CFVOLC,CFVOLP,
     I               CFVOLF,ODGTFG,NCOGT,COGTC,COGTE,FUNCT,IREXIT,
     M               CFRAC,
     O               CVOL,CO,CRO,COVOL,CROVOL)
C
        IF ( (IREXIT .GE. 1) .AND. (IRRDEM .GT. 0.0) ) THEN
C         an irrigation demand was made before routing
          DO 120 IC= 1, NCATS
            CIRRDM= CIRRWD(IC)/DELTS
            CO(IREXIT,IC)= CIRRDM
            COVOL(IREXIT,IC)= CIRRWD(IC)
            CRO(IC)= CRO(IC)+ CIRRDM
            CROVOL(IC)= CROVOL(IC)+ CIRRWD(IC)
 120      CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOLVE
     I                  (NROWS,NCOLS,RCHTAB,FACTA1,OINT,NEXITS,ODFVFG,
     I                   ODGTFG,CONVF,COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I                   MAXEX,COREL,RCHNO,MESSU,MSGFL,SCLU,
     I                   DATIM,VOLT,DELTS,
     M                   ROWPT,HYECNT,HYWCNT,
     O                   VOL,O,RO)
C
C     + + + PURPOSE + + +
C     Solve the simultaneous equations used in case 1.  This might
C     involve searching thru RCHTAB to find the appropriate
C     segment of the O vs VOL curve.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          NROWS,NCOLS,NEXITS,ODFVFG(NEXITS),ODGTFG(NEXITS),
     $                 FUNCT(NEXITS),NCATS,MAXEX,RCHNO,MESSU,MSGFL,SCLU,
     $                 DATIM(5),ROWPT,HYECNT(5),HYWCNT(2),MXCAT
      REAL             RCHTAB(500),FACTA1,CONVF,COLIND(NEXITS),
     $                 OUTDGT(NEXITS),COREL(MAXEX,MXCAT),DELTS,
     $                 O(NEXITS),RO
      DOUBLE PRECISION OINT,VOLT,VOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROWS  - number of rows in ftable
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     FACTA1 - term in eq 18 = 1.0/(COKS*DELTS)
C     OINT   - intercept of eq 4 on the o axis
C     NEXITS - number of exits from the operation
C     ODFVFG - flag indicating outflow as function of volume for each exit
C     ODGTFG - flag indicating outflow as function of time for each exit
C     CONVF  - f(vol) adjustment factor
C     COLIND - column containing outflow demand (may require interp between 2)
C     OUTDGT - current time varing outflow demand amount
C     FUNCT  - user-specified function for combining outflow demand components
C     MXCAT  - max number of categories
C     NCATS  - number of categories active
C     MAXEX  - maximum number of exits
C     COREL  - current time-varying outflow demand amount for each category
C              adjusted for category shortage
C     RCHNO  - reach number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     SCLU   - cluster containing error and warning message details
C     DATIM  - date and time of day
C     VOLT   - volume in the reach after inflow, precip and evap
C     DELTS  - simulation interval in seconds
C     ROWPT  - starting row to search in ftable, updated for next time
C     HYECNT - count of error messages for section HYDR
C     HYWCNT - count of warning messages for section HYDR
C     VOL    - volume of water in reach above bed
C     O      - rate of outflow from each reach exit at end of interval
C     RO     - total rate of outflow from reach at end of interval
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          IDUMMY,J,MOVE,PREMOV,ROWAD1,ROWAD2,SGRP
      REAL             DIFF,FACTR,OD1(MXEXIT),OD2(MXEXIT),ROD1,ROD2,V1,
     $                 V2,VR4
      DOUBLE PRECISION DET,DETV,FACTA2,FACTB2,FACTC2,V
C
C     + + + INTRINSICS + + +
      INTRINSIC        ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL         DEMAND,OMSG,OMSTI,OMSTR,OMSTD
C
C     + + + END SPECIFICATIONS + + +
C
C     premov will be used to check whether we are in a trap
C     initially, set to an arbitrary value not= -1 or 1
      PREMOV= -20
C
C     dountil MOVE= 0
 10   CONTINUE
C       get the volumes and outflow demands which apply to this
C       segment of the o vs vol curve
        ROWAD1= (ROWPT - 1)*NCOLS + 1
        V1= RCHTAB(ROWAD1 + 2)
        CALL DEMAND (NCOLS,RCHTAB(ROWAD1),NEXITS,ODFVFG,ODGTFG,CONVF,
     I               COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I               MXEXIT,COREL,VOLT,DELTS,
     O               OD1,ROD1)
C
        ROWAD2= (ROWPT)*NCOLS + 1
        V2= RCHTAB(ROWAD2 + 2)
        CALL DEMAND (NCOLS,RCHTAB(ROWAD2),NEXITS,ODFVFG,ODGTFG,CONVF,
     I               COLIND,OUTDGT,FUNCT,MXCAT,NCATS,
     I               MXEXIT,COREL,VOLT,DELTS,
     O               OD2,ROD2)
C
C       do a trial solve of the equations
C       facta1 is evaluated by the run interpreter= 1/(coks*delts)
C       factb1= 1,
C       factc1= oint
C
        FACTA2= ROD1 - ROD2
        FACTB2= V2 - V1
        FACTC2= V2*ROD1 - V1*ROD2
C
C       find the determinants
        DET= FACTA1*FACTB2 - FACTA2
        IF ((ABS(DET)) .LE. 0.0) THEN
C         error -- system is indeterminate
          CALL OMSTD (DATIM)
          CALL OMSTI (RCHNO)
          CALL OMSTR (V1)
          CALL OMSTR (V2)
          CALL OMSTR (ROD1)
          CALL OMSTR (ROD2)
          SGRP= 1
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               HYECNT(1))
          DET= 0.0001
        END IF
        DETV= OINT*FACTB2 - FACTC2
        V= DETV/DET
        IF (V .LT. 0.0) THEN
C         avoid underflow
          V= 0.0
        END IF
C
C       check whether v is between v1 and v2. if not, indicate which
C       way to move in rchtab. if v requires extrapolation of the
C       table, give a warning and indicate to stay in the last
C       interval.
C
        IF (V .GT. V2) THEN
C         move up
          MOVE= 1
          ROWPT= ROWPT + 1
C         check for extrapolation
          IF (ROWPT .EQ. NROWS) THEN
C           warn that extrapolation of rchtab will take place
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTI (NROWS)
            CALL OMSTR (V1)
            CALL OMSTR (V2)
C           need real version of v for message routine
            VR4= V
            CALL OMSTR (VR4)
            SGRP= 6
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 HYWCNT(1))
            ROWPT= ROWPT - 1
            MOVE= 0
          END IF
        ELSE IF (V .LT. V1) THEN
C         move down
          MOVE= -1
          ROWPT= ROWPT - 1
        ELSE
C         we are there
          MOVE= 0
        END IF
C
C       check whether algorithm is in a trap, yo-yoing back and forth
        IDUMMY= MOVE + PREMOV
        IF (IDUMMY .EQ. 0) THEN
C         error - trapped with an oscillating condition
          CALL OMSTD (DATIM)
          CALL OMSTI (RCHNO)
          CALL OMSTI (MOVE)
          CALL OMSTI (PREMOV)
          SGRP= 2
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               HYECNT(2))
C         get out of the trap by settling in this segment of the curve
          MOVE= 0
C         leave rowpt at whatever value it now has
        END IF
C
        PREMOV= MOVE
      IF (MOVE .NE. 0) GO TO 10
C     end dountil
C
C     found the solution
      VOL= V
      RO= OINT - FACTA1*VOL
      IF (VOL .LT. 1.0E-5) THEN
C       avoid underflow
Cthj    next line added to make material balance correct
        RO= OINT
        VOL= 0.0
      END IF
C
      IF (RO .LT. 1.0E-10) THEN
C       avoid underflow
        RO= 0.0
      END IF
C
      IF ((ABS(RO)) .LE. 0.0) THEN
C       no total outflow, so no exit outflow
        DO 20 J= 1,NEXITS
          O(J)= 0.0
 20     CONTINUE
      ELSE
        DIFF= VOL - V1
        IF (DIFF .LT. 0.01) THEN
C         avoid underflow
          FACTR= 0.0
        ELSE
C         interpolate values of individual outflow rates
          FACTR= (DIFF)/(V2 - V1)
        END IF
        DO 30 J= 1,NEXITS
          O(J)= OD1(J) + (OD2(J) - OD1(J))*FACTR
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   AUXIL
     I                  (NCOLS,RCHTAB,ROWADD,VOL,STCOR,LEN,
     I                   RCHNO,MESSU,MSGFL,DATIM,SCLU,AUX1FG,
     M                   HYECNT,
     O                   DEP,STAGE,SAREA,AVDEP,TWID,HRAD)
C
C     + + + PURPOSE + + +
C     Compute depth, stage, surface area, average depth, topwidth,
C     and hydraulic radius.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          MSGFL,HYECNT(5),MESSU,NCOLS,RCHNO,ROWADD,
     $                 DATIM(5),SCLU,AUX1FG
      REAL             AVDEP,DEP,HRAD,LEN,RCHTAB(500),SAREA,STAGE,
     $                 STCOR,TWID
      DOUBLE PRECISION VOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCOLS  - number of columns in ftable
C     RCHTAB - ftable stored as a scaler
C     ROWADD - address in scaler ftable of current row
C     VOL    - volume of water in reach above bed
C     STCOR  - stage correction factor
C     LEN    - length of reach
C     RCHNO  - reach number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     DATIM  - date and time of day
C     SCLU   - cluster containing error and warning message details
C     AUX1FG - auxiliary calculation flag - 0: no calculation
C                                 1: calc DEP,STAGE,AVDEP,TWID,HRAD,SAREA
C                                    if VOL > 0
C                                 2: calc same even if VOL = 0
C     HYECNT - count of error messages for section HYDR
C     DEP    - depth of reach at deepest point
C     STAGE  - water stage at deepest point within reach
C     SAREA  - surface area of reach
C     AVDEP  - reach average depth (volume/surface area)
C     TWID   - reach top width (surface area/length)
C     HRAD   - hydraulic radius
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    COUNT,SGRP
      REAL       A,B,C,DEP1,DEP2,DFRDEP,FRDEP,RDEP1,RDEP2,SA1,SA2,
     $           VOL1,VOL2
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTD,OMSG,OMSTR,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      IF (VOL .GT. 0.0) THEN
C       compute depth of flow -- start by evaluating
C       coefficients for search using Newton's method
C       manual eq (34,35)
        SA1= RCHTAB(ROWADD + 1)
        SA2= RCHTAB(ROWADD + 1 + NCOLS)
        A= SA2 - SA1
        B= 2.0*SA1
        VOL1= RCHTAB(ROWADD + 2)
        VOL2= RCHTAB(ROWADD + 2 + NCOLS)
        C= -(VOL - VOL1)/(VOL2 - VOL1)*(B + A)
C
C       search using newton's method
        RDEP2= 0.5
        COUNT= 0
C       dountil (abs(rdep2 -rdep1) <.001)
 10     CONTINUE
          RDEP1= RDEP2
          COUNT= COUNT + 1
          FRDEP= A*RDEP1**2 + B*RDEP1 + C
          DFRDEP= 2.0*A*RDEP1 + B
          RDEP2= RDEP1 - FRDEP/DFRDEP
C
          IF (COUNT .GT. 100) THEN
C           error -- no convergence
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTR (A)
            CALL OMSTR (B)
            CALL OMSTR (C)
            CALL OMSTR (RDEP1)
            CALL OMSTR (RDEP2)
            CALL OMSTI (COUNT)
            SGRP= 4
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 HYECNT(4))
          END IF
        IF (ABS(RDEP2 - RDEP1) .GE. 0.001) GO TO 10
C       end dountil
C
        IF (RDEP2 .GT. 1.0 .OR. RDEP2 .LT. 0.0) THEN
C         error -- solution converged to point outside valid range
          CALL OMSTD (DATIM)
          CALL OMSTI (RCHNO)
          CALL OMSTR (A)
          CALL OMSTR (B)
          CALL OMSTR (C)
          CALL OMSTR (RDEP1)
          CALL OMSTR (RDEP2)
          CALL OMSTI (COUNT)
          SGRP= 5
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               HYECNT(5))
        END IF
C
C       depth, manual eq (36)
        DEP1= RCHTAB(ROWADD)
        DEP2= RCHTAB(ROWADD + NCOLS)
        DEP= DEP1 + RDEP2*(DEP2 - DEP1)
C        
C       surface area calculation, manual eq (38)
        SAREA= SA1 + A*RDEP2
C
C       average depth calculation, manual eq (39)
        AVDEP= VOL/SAREA
C
C       top-width calculation, manual eq (40)
        TWID= SAREA/LEN
C
C       hydraulic radius, manual eq (41)
        HRAD= (AVDEP*TWID)/(2.0*AVDEP + TWID)
      ELSE IF (AUX1FG .EQ. 2) THEN
C       DEP= RCHTAB(ROWADD)
        SAREA= RCHTAB(ROWADD+ 1)
        AVDEP= 0.0
        TWID= SAREA/LEN
        HRAD= 0.0
      ELSE
C       no water
        DEP= 0.0
        SAREA= 0.0
        AVDEP= 0.0
        TWID= 0.0
        HRAD= 0.0
      END IF
C
C     stage calculation and output, manual eq (37)
      STAGE= DEP + STCOR
C
      RETURN
      END
C
C
C
      SUBROUTINE   SHEAR
     I                  (LKFG,AVDEP,AVVEL,HRAD,DB50,AKAPPA,GAM,
     I                   GRAV,SLOPE,
     O                   USTAR,TAU)
C
C     + + + PURPOSE + + +
C     Calculate bed shear stress and shear velocity
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LKFG
      REAL       AKAPPA,AVDEP,AVVEL,DB50,GAM,GRAV,HRAD,SLOPE,TAU,USTAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LKFG   - lake flag, 1:lake, 0:stream
C     AVDEP  - reach average depth (volume/surface area)
C     AVVEL  - reach average velocity (length*ro/volume)
C     HRAD   - hydraulic radius
C     DB50   - mean diameter of bed material
C     AKAPPA - von karmen constant
C     GAM    - density of water
C     GRAV   - acceleration due to gravity
C     SLOPE  - slope of reach
C     USTAR  - bed shear velocity
C     TAU    - bed shear stress
C
C     + + + INTRINSICS + + +
      INTRINSIC  SQRT,ALOG10
C
C     + + + END SPECIFICATIONS + + +
C
      IF (AVDEP .GT. 0.0) THEN
C       water in reach
        IF (LKFG .EQ. 1) THEN
C         use formula appropriate to a lake- from "hydraulics
C         of sediment transport", by w.h. graf- eq.8.49, manual eq (42)
          USTAR= AVVEL/(17.66+(ALOG10(AVDEP/(96.5*DB50)))*2.3/AKAPPA)
c         manual eq (43)
          TAU= GAM*(USTAR**2)/GRAV
        ELSE
C         use formula appropriate to a river or stream, manual eq (44)
          USTAR= SQRT(GRAV*SLOPE*HRAD)
C         manual eq (45)
          TAU= SLOPE*GAM*HRAD
        END IF
      ELSE
C       water body is dry - values undefined
        USTAR= 0.0
        TAU= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYDACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in module group HYDR for printout.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + HISTORY + + +
C     2005  BRB added potential et to output summaries
C
C     + + + END SPECIFICATIONS + + +
C
C     total inflow flux
      HYIF(TOROW)= HYIF(TOROW) + HYIF(FRMROW)
C
      IF (NCATS .GT. 0) THEN
C       category inflow
        CALL ACCVEC (MXCAT,HYIFC(1,FRMROW),
     M               HYIFC(1,TOROW))
      END IF
C
C     precip,evap,and outflow
      I= 3
      CALL ACCVEC (I,HYCF1(1,FRMROW),
     M             HYCF1(1,TOROW))
C
      IF (NCATS .GT. 0) THEN
C       category outflow
        CALL ACCVEC (MXCAT,HYCF1C(1,FRMROW),
     M               HYCF1C(1,TOROW))
      END IF
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        CALL ACCVEC (NEXITS,HYCF2(1,FRMROW),
     M               HYCF2(1,TOROW))
        IF (NCATS .GT. 0) THEN
C         category outflow
          DO 10 I= 1, NCATS
            CALL ACCVEC (NEXITS,HYCF2C(1,I,FRMROW),
     M                   HYCF2C(1,I,TOROW))
 10       CONTINUE
        END IF
      END IF
C
      IF(PEVFP .GT. 0) THEN
C       potential et
        POTEV(TOROW)= POTEV(TOROW) + POTEV(FRMROW)
      END IF
C
      IF (IREXIT .GE. 1) THEN
C       reset irrigation withdrawal for next interval
        I= 2
        CALL ACCVEC (I,HYIRCF(1,FRMROW),
     M               HYIRCF(1,TOROW))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYDPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     materials balance and print out results.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV,PRINTU,UNITFG,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
      INCLUDE    'chcat.inc'
C
C     + + + SAVES + + +
C     LASTOP - last operation printed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,I0,I1,I2,IC,IX,JLEN,RLEN,TLEN,TPOS,ACNT,
     $              CLEN(19+MXEXIT+(MXCAT*(MXEXIT+2))),EXDAT(5)
      REAL          AFACTA,LFACTA,LFACTB,PAVVEL,PAVSEC,PCFLX1(3),
     $              SFACTA,PCFLX2(MXEXIT),PIFLX,PRO,PSAREA,PSTAT(5),
     $              PTAU,PUSTAR,PVOL,PVOLS,TFACTA,VFACTA,VFACTB,VOLDIF,
     $              VOLIN,PCST(MXCAT,2),PCFLX(MXCAT,MXEXIT+2),PIRFLX(3),
     $              APRINT(19+MXEXIT+(MXCAT*(MXEXIT+2))),L2FACT,PPOTEV
      CHARACTER*1   CSTR(2)
      CHARACTER*8   UNITID,CSTAT(5),CCST(MXCAT,2),CCFLX2(MXEXIT),
     $              CCFLX(MXCAT,MXEXIT+2),CCFLX1(3),CIRFLX(3)
      CHARACTER*256 CHEAD(19+MXEXIT+(MXCAT*(MXEXIT+2)))
      CHARACTER*132 TBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (TBUFF,TBUF1)
      CHARACTER*1   TBUF1(132)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,RHTSTR,INTCHR,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** HYDR ***')
 2010 FORMAT (/,'   STATE VARIABLES',18X,'  VOL       DEP     STAGE',
     $          '     AVDEP      TWID      HRAD     SAREA     AVVEL',
     $          '    AVSECT')
 2020 FORMAT (  '     ENTIRE REACH:',18X,'AC-FT        FT        FT',
     $          '        FT        FT        FT     ACRES      FT/S',
     $          '       FT2')
 2030 FORMAT (  '     ENTIRE REACH:',18X,'  MM3         M         M',
     $          '         M         M         M        HA       M/S',
     $          '        M2')
 2040 FORMAT (1PG10.3)
 2050 FORMAT (132A1)
 2060 FORMAT (/,35X,'PUSTAR      PTAU   OUTFLOW')
 2070 FORMAT (  35X,'  FT/S    LB/FT2     FT3/S')
 2080 FORMAT (  35X,'   M/S     KG/M2      M3/S')
 2090 FORMAT (/,'   STATE VARIABLES',18X,'  VOL       DEP     STAGE',
     $          '     AVDEP      TWID      HRAD     SAREA     AVVEL',
     $          '    AVSECT   OUTFLOW')
 2100 FORMAT (  '     ENTIRE REACH:',18X,'AC-FT        FT        FT',
     $          '        FT        FT        FT     ACRES      FT/S',
     $          '       FT2     FT3/S')
 2110 FORMAT (  '     ENTIRE REACH:',18X,'  MM3         M         M',
     $          '         M         M         M        HA       M/S',
     $          '        M2      M3/S')
 2120 FORMAT (/,'   STATE VARIABLES',18X,'  VOL       DEP     STAGE',
     $          '     AVDEP      TWID      HRAD     SAREA   OUTFLOW')
 2130 FORMAT (  '     ENTIRE REACH:',18X,'AC-FT        FT        FT',
     $          '        FT        FT        FT     ACRES     FT3/S')
 2140 FORMAT (  '     ENTIRE REACH:',18X,'  MM3         M         M',
     $          '         M         M         M        HA      M3/S')
 2150 FORMAT (/,'   STATE VARIABLES',20X,'VOL   OUTFLOW')
 2160 FORMAT (  '     ENTIRE REACH:',18X,'AC-FT     FT3/S')
 2170 FORMAT (  '     ENTIRE REACH:',18X,'  MM3      M3/S')
 2180 FORMAT (  '     BY CATEGORY:  ')
 2190 FORMAT (/,'   FLUXES                           TOTAL    PRECIP',
     $          '      EVAP     TOTAL    INDIVIDUAL GATE OUTFLOWS')
 2200 FORMAT (  '                                   INFLOW          ',
     $          '      LOSS   OUTFLOW', 5I10)
 2210 FORMAT (  '     VOLUMES  (AC-FT)                IVOL    PRSUPY',
     $          '     VOLEV     ROVOL                OVOL')
 2220 FORMAT (  '     VOLUMES  (MM3)                  IVOL    PRSUPY',
     $          '     VOLEV     ROVOL                OVOL')
 2230 FORMAT (/,'   FLUXES                           TOTAL    PRECIP',
     $          '      EVAP     TOTAL')
 2240 FORMAT (  '     ENTIRE REACH:                 INFLOW          ',
     $          '      LOSS   OUTFLOW')
 2250 FORMAT (  '       VOLUMES  (AC-FT)              IVOL    PRSUPY',
     $          '     VOLEV     ROVOL')
 2260 FORMAT (  '       VOLUMES  (MM3)                IVOL    PRSUPY',
     $          '     VOLEV     ROVOL')
 2270 FORMAT (/,'     POTENTIAL ET (IN)              POTEV')
 2280 FORMAT (/,'     POTENTIAL ET (MM)              POTEV')
 2290 FORMAT (/,'     IRRIGATION (AC-FT)            RIRDEM    RIRWDL',
     $          '    RIRSHT')
 2300 FORMAT (/,'     IRRIGATION (MM3)              RIRDEM    RIRWDL',
     $          '    RIRSHT')
C
C     + + + HISTORY + + +
C     2005  BRB added potential et to output summaries
C
C     + + + END SPECIFICATIONS + + +
C
C     Note: local arrays have same dimensions as corresponding arrays in
C           osv, except for dropping of dimension lev, where applicable
C
      I0= 0
      I1= 1
      I2= 2
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT(1) = 'DEP'
      CSTAT(2) = 'STAGE'
      CSTAT(3) = 'AVDEP'
      CSTAT(4) = 'TWID'
      CSTAT(5) = 'HRAD'
      DO 1 IC= 1, NCATS
        CALL INTCHR (IC, I2, I1,
     O               JLEN, CSTR)
        CCST(IC,1) = 'CVOL-'
        CCST(IC,2) = 'CRO-'
        DO 2 I= 1, JLEN
          CCST(IC,1) = TRIM(CCST(IC,1)) // CSTR(I)
          CCST(IC,2) = TRIM(CCST(IC,2)) // CSTR(I)
 2      CONTINUE
 1    CONTINUE
      CCFLX1(1) = 'PRSUPY'
      CCFLX1(2) = 'VOLEV'
      CCFLX1(3) = 'ROVOL'
      DO 3 IX= 1, NEXITS
        CALL INTCHR (IX, I2, I1,
     O               JLEN, CSTR)
        CCFLX2(IX) = 'OVOL-'
        DO 4 I= 1, JLEN
          CCFLX2(IX) = TRIM(CCFLX2(IX)) // CSTR(I)
 4      CONTINUE
 3    CONTINUE
      DO 5 IC= 1, NCATS
        CALL INTCHR (IC, I2, I1,
     O               JLEN, CSTR)
        CCFLX(IC,1) = 'IVOL'
        CCFLX(IC,2) = 'ROVOL'
        DO 6 I= 1, JLEN
          CCFLX(IC,1) = TRIM(CCFLX(IC,1)) // CSTR(I)
          CCFLX(IC,2) = TRIM(CCFLX(IC,2)) // CSTR(I)
 6      CONTINUE
        DO 7 IX= 1, NEXITS
          CCFLX(IC,IX+2) = 'ROVOL'
          CALL INTCHR (IC, I2, I1,
     O                 JLEN, CSTR)
          DO 8 I= 1, JLEN
            CCFLX(IC,IX+2) = TRIM(CCFLX(IC,IX+2)) // CSTR(I)
 8        CONTINUE
          CALL INTCHR (IX, I2, I1,
     O                 JLEN, CSTR)
          CCFLX(IC,IX+2) = TRIM(CCFLX(IC,IX+2)) // '-'
          DO 9 I= 1, JLEN
            CCFLX(IC,IX+2) = TRIM(CCFLX(IC,IX+2)) // CSTR(I)
 9        CONTINUE
 7      CONTINUE
 5    CONTINUE
      CIRFLX(1) = 'RIRDEM'
      CIRFLX(2) = 'RIRWDL'
      CIRFLX(3) = 'RIRSHT'
C
C     dimensionless variables do not need to be converted
C     assign values to parameters used for conversion from internal to
C     external units
      IF (UNITFG .EQ. 1) THEN
C       printout is in english units
        IF (UUNITS .EQ. 1) THEN
C         english to english
          VFACTA= 2.295684E-05
          VFACTB= 0.0
          LFACTA= 1.0
          LFACTB= 0.0
          L2FACT= 12.0
          AFACTA= 2.295684E-05
          SFACTA= 1.0
          TFACTA= 1.0
        ELSE
C         metric to english
          VFACTA= 8.12E-04
          VFACTB= 0.0
          LFACTA= 3.28
          LFACTB= 0.0
          L2FACT= 39.37
          AFACTA= 2.47E-04
          SFACTA= 10.76
          TFACTA= 0.2048
        END IF
      ELSE
C       printout is in metric system
        IF (UUNITS .EQ. 1) THEN
C         english to metric
          VFACTA= 2.83E-08
          VFACTB= 0.0
          LFACTA= .305
          LFACTB= 0.0
          L2FACT= 304.8
          AFACTA= 9.29E-06
          SFACTA= 9.29E-02
          TFACTA= 4.883
        ELSE
C         metric to metric
          VFACTA= 1.0E-06
          VFACTB= 0.0
          LFACTA= 1.0
          LFACTB= 0.0
          L2FACT= 1000.0
          AFACTA= 1.0E-04
          SFACTA= 1.0
          TFACTA= 1.0
        END IF
      END IF
C     convert variables to external units
C
C     rchres-wide variables - state variables and storages
      PVOL= VOLUME(1)*VFACTA
      PVOLS= PREVOL(LEV - 1)*VFACTA
      PRO= RO*SFACTA*LFACTA
C
      IF (AUX1FG .GE. 1) THEN
        I= 5
        CALL TRNVEC (I,HYST(1),LFACTA,LFACTB,
     O               PSTAT(1))
        PSAREA= SAREA*AFACTA
C
        IF (AUX2FG .EQ. 1) THEN
          PAVVEL= AVVEL*LFACTA
          PAVSEC= AVSECT*SFACTA
C
          IF (AUX3FG .EQ. 1) THEN
            PUSTAR= USTAR*LFACTA
            PTAU= TAU*TFACTA
          END IF
        END IF
      END IF
C
C     inflow fluxes
      PIFLX= HYIF(LEV)*VFACTA
C
C     computed fluxes - prec, evap, outflow
      I= 3
      CALL TRNVEC (I,HYCF1(1,LEV),VFACTA,VFACTB,
     O             PCFLX1(1))
C
      IF (NEXITS .GT. 1) THEN
        CALL TRNVEC (NEXITS,HYCF2(1,LEV),VFACTA,VFACTB,
     O               PCFLX2(1))
      END IF
C
      IF (NCATS .GE. 1) THEN
C       calculate printout category variables
        DO 20 IC= 1, NCATS
C         category volume
          PCST(IC,1)= CVOL(IC)*VFACTA
C         category outflow rate
          PCST(IC,2)= CRO(IC)*SFACTA*LFACTA
C         category inflow flux
          PCFLX(IC,1)= HYIFC(IC,LEV)*VFACTA
C         category total outflow flux
          PCFLX(IC,2)= HYCF1C(IC,LEV)*VFACTA
          IF (NEXITS .GT. 1) THEN
C           category exit outflow flux
            DO 10 IX= 1, NEXITS
              PCFLX(IC,2+IX)= HYCF2C(IX,IC,LEV)*VFACTA
 10         CONTINUE
          END IF
 20     CONTINUE
      END IF
C
      IF (PEVFP .GT. 0) THEN
C       potential et (input timeseries)
        PPOTEV= POTEV(LEV)*L2FACT
      END IF
C
      IF (IREXIT .GE. 1) THEN
C       irrigation fluxes
        PIRFLX(1)= HYIRCF(1,LEV)*VFACTA+ VFACTB
        PIRFLX(3)= HYIRCF(2,LEV)*VFACTA+ VFACTB
        IF (NEXITS .GT. 1) THEN
C         use corresponding value of ovol
          PIRFLX(2)= PCFLX2(IREXIT)
        ELSE IF (IREXIT .EQ. 1) THEN
C         use rovol
          PIRFLX(2)= PCFLX1(3)
        ELSE
C         no withdrawals
          PIRFLX(2)= 0.0
        END IF
      END IF
C
C     write to unit PRINTU
      IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
C       initialize output array
        TBUFF= ' '
        RLEN= 9
        TLEN= RLEN+ 1
        TPOS= 32
      END IF
C
      IF (AUX1FG .GE. 1) THEN
C       first aux flag on
        IF (AUX2FG .EQ. 1) THEN
C         first and second aux flag on
          IF (AUX3FG .EQ. 1) THEN
C           all flags on - max output
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              WRITE (PRINTU,2010)
              IF (UNITFG .EQ. 1) THEN
C               english units
                WRITE (PRINTU,2020)
              ELSE
C               metric units
                WRITE (PRINTU,2030)
              END IF
C             build output buffer
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PVOL
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
C             compile values for direct access printout
              ACNT = ACNT + 1
              APRINT(ACNT) = PVOL
              CHEAD(ACNT) = 'VOL'
              CLEN(ACNT) = 3
            END IF
            DO 30 I= 1, 5
              IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
                TPOS= TPOS+ RLEN+ 1
                WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PSTAT(I)
                CALL RHTSTR (TLEN,
     M                       TBUF1(TPOS))
              END IF
              IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
                ACNT = ACNT + 1
                APRINT(ACNT) = PSTAT(I)
                CHEAD(ACNT) = CSTAT(I)
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              END IF
 30         CONTINUE
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PSAREA
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PSAREA
              CHEAD(ACNT) = 'SAREA'
              CLEN(ACNT) = 5
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PAVVEL
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PAVVEL
              CHEAD(ACNT) = 'AVVEL'
              CLEN(ACNT) = 5
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PAVSEC
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PAVSEC
              CHEAD(ACNT) = 'AVSECT'
              CLEN(ACNT) = 6
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN
              WRITE(PRINTU,2050) (TBUF1(I), I=1,TPOS)
            END IF
C
C           second line of output
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              WRITE (PRINTU,2060)
              IF (UNITFG .EQ. 1) THEN
C               english units
                WRITE (PRINTU,2070)
              ELSE
C               metric units
                WRITE (PRINTU,2080)
              END IF
C             build output buffer for second buffer
              TBUFF= ' '
              TPOS= 32
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PUSTAR
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PUSTAR
              CHEAD(ACNT) = 'USTAR'
              CLEN(ACNT) = 5
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PTAU
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PTAU
              CHEAD(ACNT) = 'TAU'
              CLEN(ACNT) = 3
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PRO
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PRO
              CHEAD(ACNT) = 'RO'
              CLEN(ACNT) = 2
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN
              WRITE (PRINTU,2050) (TBUF1(I), I= 1, TPOS)
            END IF
          ELSE
C           only flags one and two on
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              WRITE (PRINTU,2090)
              IF (UNITFG .EQ. 1) THEN
C               english units
                WRITE (PRINTU,2100)
              ELSE
C               metric units
                WRITE (PRINTU,2110)
              END IF
C             build output buffer
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PVOL
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
C             compile values for direct access printout
              ACNT = ACNT + 1
              APRINT(ACNT) = PVOL
              CHEAD(ACNT) = 'VOL'
              CLEN(ACNT) = 3
            END IF
            DO 40 I= 1, 5
              IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
                TPOS= TPOS+ RLEN+ 1
                WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PSTAT(I)
                CALL RHTSTR (TLEN,
     M                       TBUF1(TPOS))
              END IF
              IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
                ACNT = ACNT + 1
                APRINT(ACNT) = PSTAT(I)
                CHEAD(ACNT) = CSTAT(I)
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              END IF
 40         CONTINUE
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PSAREA
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PSAREA
              CHEAD(ACNT) = 'SAREA'
              CLEN(ACNT) = 5
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PAVVEL
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PAVVEL
              CHEAD(ACNT) = 'AVVEL'
              CLEN(ACNT) = 5
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PAVSEC
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PAVSEC
              CHEAD(ACNT) = 'AVSECT'
              CLEN(ACNT) = 6
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PRO
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PRO
              CHEAD(ACNT) = 'RO'
              CLEN(ACNT) = 2
            END IF
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN
              WRITE(PRINTU,2050) (TBUF1(I), I=1,TPOS)
            END IF
          END IF
        ELSE
C         only flag one on
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            WRITE (PRINTU,2120)
            IF (UNITFG .EQ. 1) THEN
C             english units
              WRITE (PRINTU,2130)
            ELSE
C             metric units
              WRITE (PRINTU,2140)
            END IF
C           build output buffer
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PVOL
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
C           compile values for direct access printout
            ACNT = ACNT + 1
            APRINT(ACNT) = PVOL
            CHEAD(ACNT) = 'VOL'
            CLEN(ACNT) = 3
          END IF
          DO 50 I= 1, 5
            IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
              TPOS= TPOS+ RLEN+ 1
              WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PSTAT(I)
              CALL RHTSTR (TLEN,
     M                     TBUF1(TPOS))
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PSTAT(I)
              CHEAD(ACNT) = CSTAT(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            END IF
 50       CONTINUE
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS+ RLEN+ 1
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PSAREA
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PSAREA
            CHEAD(ACNT) = 'SAREA'
            CLEN(ACNT) = 5
          END IF
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS+ RLEN+ 1
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PRO
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PRO
            CHEAD(ACNT) = 'RO'
            CLEN(ACNT) = 2
          END IF
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS+ RLEN
            WRITE(PRINTU,2050) (TBUF1(I), I= 1, TPOS)
          END IF
        END IF
      ELSE
C       no aux flags on
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          WRITE (PRINTU,2150)
          IF (UNITFG .EQ. 1) THEN
C           english units
            WRITE (PRINTU,2160)
          ELSE
C           metric units
            WRITE (PRINTU,2170)
          END IF
C         build output buffer
          WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PVOL
          CALL RHTSTR (TLEN,
     M                 TBUF1(TPOS))
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
C         compile values for direct access printout
          ACNT = ACNT + 1
          APRINT(ACNT) = PVOL
          CHEAD(ACNT) = 'VOL'
          CLEN(ACNT) = 3
        END IF
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          TPOS= TPOS+ RLEN+ 1
          WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PRO
          CALL RHTSTR (TLEN,
     M                 TBUF1(TPOS))
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PRO
          CHEAD(ACNT) = 'RO'
          CLEN(ACNT) = 2
        END IF
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          TPOS= TPOS+ RLEN
          WRITE (PRINTU,2050) (TBUF1(I), I= 1, TPOS)
        END IF
      END IF
C
      IF (NCATS .GE. 1) THEN
C       category state variable output
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) WRITE (PRINTU,2180)
        DO 60 IC= 1, NCATS
C         first volume, then flow
C         build output buffer
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TBUFF= ' '
            TBUFF(8:25)= CATNAM(IC)
            TPOS= 32
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCST(IC,1)
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PCST(IC,1)
            CHEAD(ACNT) = CCST(IC,1)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
C           determine offset for outflow
            IF (AUX1FG.GE.1 .AND. AUX2FG.EQ.1 .AND. AUX3FG.EQ.1) THEN
              TPOS= TPOS+ 20
            ELSE IF (AUX1FG.GE.1 .AND. AUX2FG.EQ.1) THEN
              TPOS= TPOS+ 90
            ELSE IF (AUX1FG.GE.1) THEN
              TPOS= TPOS+ 70
            ELSE IF (AUX1FG.EQ.0 .AND. AUX2FG.EQ.0 .AND.
     $               AUX3FG.EQ.0) THEN
              TPOS= TPOS+ 10
            END IF
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCST(IC,2)
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PCST(IC,2)
            CHEAD(ACNT) = CCST(IC,2)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS + RLEN
            WRITE(PRINTU,2050) (TBUF1(I), I= 1, TPOS)
          END IF
 60     CONTINUE
      END IF
C
C     flux output
      IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
        IF (NEXITS .GT. 1) THEN
C         output for each exit
          WRITE (PRINTU,2190)
C
          WRITE (PRINTU,2200) (IX, IX= 1, NEXITS)
          IF (UNITFG .EQ. 1) THEN
C           english units
            WRITE (PRINTU,2210)
          ELSE
C           metric units
            WRITE (PRINTU,2220)
          END IF
        ELSE
C         just total output
          WRITE (PRINTU,2230)
          WRITE (PRINTU,2240)
          IF (UNITFG .EQ. 1) THEN
C           english units
            WRITE (PRINTU,2250)
          ELSE
C           metric units
            WRITE (PRINTU,2260)
          END IF
        END IF
      END IF
C
C     build output buffer
      IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
C       initialize output array
        TBUFF= ' '
        TPOS= 32
        WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PIFLX
        CALL RHTSTR (TLEN,
     M               TBUF1(TPOS))
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
        ACNT = ACNT + 1
        APRINT(ACNT) = PIFLX
        CHEAD(ACNT) = 'IVOL'
        CLEN(ACNT) = 4
      END IF
      DO 70 I= 1, 3
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          TPOS= TPOS+ RLEN+ 1
          WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCFLX1(I)
          CALL RHTSTR (TLEN,
     M                 TBUF1(TPOS))
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(I)
          CHEAD(ACNT) = CCFLX1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
 70   CONTINUE
      IF (NEXITS .GT. 1) THEN
C       print outflow volume by exit
        DO 80 IX= 1, NEXITS
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS+ RLEN+ 1
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCFLX2(IX)
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX2(IX)
            CHEAD(ACNT) = CCFLX2(IX)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
 80     CONTINUE
      END IF
      IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
        TPOS= TPOS+ RLEN
        WRITE (PRINTU,2050) (TBUF1(I), I= 1, TPOS)
      END IF
C
      IF (NCATS .GE. 1) THEN
C       category flux output
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) WRITE (PRINTU,2180)
        DO 100 IC= 1, NCATS
C         first inflow, then outflow
C         build output buffer
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TBUFF= ' '
            TBUFF(8:25)= CATNAM(IC)
            TPOS= 32
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCFLX(IC,1)
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX(IC,1)
            CHEAD(ACNT) = CCFLX(IC,1)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS+ 3*(RLEN+ 1)
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCFLX(IC,2)
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX(IC,2)
            CHEAD(ACNT) = CCFLX(IC,2)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (NEXITS .GT. 1) THEN
C           print category outflows by exit
            DO 90 IX= 1, NEXITS
              IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
                TPOS= TPOS+ RLEN+ 1
                WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PCFLX(IC,2+IX)
                CALL RHTSTR (TLEN,
     M                       TBUF1(TPOS))
              END IF
              IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
                ACNT = ACNT + 1
                APRINT(ACNT) = PCFLX(IC,2+IX)
                CHEAD(ACNT) = CCFLX(IC,2+IX)
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              END IF
 90         CONTINUE
          END IF
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            TPOS= TPOS+ RLEN
            WRITE (PRINTU,2050) (TBUF1(I), I= 1, TPOS)
          END IF
 100    CONTINUE
      END IF
C
      IF (PEVFP .GT. 0) THEN
C       potential et
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          IF (UNITFG .EQ. 1) THEN
C           english units
            WRITE (PRINTU,2270)
          ELSE
C           metric units
            WRITE (PRINTU,2280)
          END IF
          TBUFF= ' '
          TPOS= 32
          WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PPOTEV
          CALL RHTSTR (TLEN,
     M                 TBUF1(TPOS))
          TPOS= TPOS+ RLEN
          WRITE (PRINTU,2050) (TBUF1(I), I= 1, TPOS)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PPOTEV
          CHEAD(ACNT) = 'POTEV'
          CLEN(ACNT) = 5
        END IF
      END IF
C
      IF (IREXIT .GE. 1) THEN
C       irrigation fluxes
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          IF (UNITFG .EQ. 1) THEN
C           english units
            WRITE (PRINTU,2290)
          ELSE
C           metric units
            WRITE (PRINTU,2300)
          END IF
          TBUFF= ' '
          TPOS= 32
        END IF
C       build output buffer
        DO 110 I= 1, 3
          IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
            WRITE (TBUFF(TPOS:TPOS+RLEN),2040) PIRFLX(I)
            CALL RHTSTR (TLEN,
     M                   TBUF1(TPOS))
            TPOS= TPOS+ RLEN+ 1
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PIRFLX(I)
            CHEAD(ACNT) = CIRFLX(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
 110    CONTINUE
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          WRITE (PRINTU,2050) (TBUF1(I), I= 1, TPOS)
        END IF
      END IF
C
C     material balance
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '   AC-FT'
      ELSE
C       metric
        UNITID= '     MM3'
      END IF
C     total volume entering RCHRES
      VOLIN= PIFLX+ PCFLX1(1)
C     calculate net volume of water entering RCHRES
      VOLDIF= VOLIN- (PCFLX1(2)+ PCFLX1(3))
C
      I= 3
      CALL BALCHK (I,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I             PVOLS,PVOL,VOLIN,VOLDIF,UNITID,I1,
     M             HYWCNT(2))
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(1) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'HYDR    ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
          IF (BUNIT(2) .LE. 0 .OR. BINU .EQ. BUNIT(2)) THEN
C           set bflag to negative to not write headers anymore
            BFLAG(1) = -BFLAG(1)
          END IF
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'HYDR    ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYDRRB
C
C     + + + PURPOSE + + +
C     mean valued timeseries from section HYDR
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IX,IC
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,3
        IF (HYCF1X(I) .GT. 0) THEN
C         1:prsupy, 2:volev, 3:rovol
          PAD(HYCF1X(I)+ IVL1)= HYCF1(I,1)
        END IF
 10   CONTINUE
C
      IF (RCVFP .GT. 0) THEN
C       inflow
        PAD(RCVFP+ IVL1)= IVOL
      END IF
C
      IF (NEXITS .GT. 1) THEN
C       quantities belonging each exit
        DO 20 IX= 1, NEXITS
          IF (OVFP(IX) .GT. 0) THEN
C           exit outflow
            PAD(OVFP(IX)+ IVL1)= OVOL(IX)
          END IF
 20     CONTINUE
      END IF
C
      IF (NCATS .GE. 1) THEN
C       might have category timeseries to output
        DO 40 IC= 1, NCATS
          IF (CROVFP(IC) .GT. 0) THEN
C           category outflow
            PAD(CROVFP(IC)+ IVL1)= CROVOL(IC)
          END IF
          IF (RCCVFP(IC) .GT. 0) THEN
C           category inflow
            PAD(RCCVFP(IC)+ IVL1)= CIVOL(IC)
          END IF
          IF (NEXITS .GT. 1) THEN
C           quantities belonging each exit
            DO 30 IX= 1, NEXITS
              IF (COVFP(IX,IC) .GT. 0) THEN
C               category-exit outflow
                PAD(COVFP(IX,IC)+ IVL1)= COVOL(IX,IC)
              END IF
 30         CONTINUE
          END IF
 40     CONTINUE
      END IF
C
      IF (IREXIT .GE. 1) THEN
C       irrigation fluxes
        IF (RIRDMX .GT. 0) THEN
C         irrigation demand
          PAD(RIRDMX+ IVL1)= RIRDEM
        END IF
        IF (RIRSHX .GT. 0) THEN
C         irrigation shortfall
          PAD(RIRSHX+ IVL1)= RIRSHT
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYDRRP
C
C     + + + PURPOSE + + +
C     point valued timeseries from section HYDR
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IX,IC
C
C     + + + END SPECIFICATIONS + + +
C
      IF (VOLFP .GT. 0) THEN
C       volume of water
        PAD(VOLFP + IVL1)= VOL
      END IF
C
      DO 10 I= 1, 5
        IF (HYSTX(I) .GT. 0) THEN
C         1:dep,2:stage,3:avdep,4:twid,5:hydraulic radius
          PAD(HYSTX(I) + IVL1)= HYST(I)
        END IF
 10   CONTINUE
C
      IF (SAFP .GT. 0) THEN
C       surface area
        PAD(SAFP + IVL1)= SAREA
      END IF
C
      IF (AVVFP .GT. 0) THEN
C       average velocity
        PAD(AVVFP + IVL1)= AVVEL
      END IF
C
      IF (AVSFP .GT. 0) THEN
C       ave cross sectional area
        PAD(AVSFP + IVL1)= AVSECT
      END IF
C
      IF (USTFP .GT. 0) THEN
C       shear velocity
        PAD(USTFP + IVL1)= USTAR
      END IF
C
      IF (TAUFP .GT. 0) THEN
C       bed shear stress
        PAD(TAUFP + IVL1)= TAU
      END IF
C
      IF (ROFP .GT. 0) THEN
C       total rate of outflow
        PAD(ROFP + IVL1)= RO
      END IF
C
      IF (NEXITS .GT. 1) THEN
C       quantities belonging to each exit
        DO 20 IX= 1, NEXITS
          IF (OFP(IX) .GT. 0) THEN
C           rate of outflow by exit
            PAD(OFP(IX) + IVL1)= O(IX)
          END IF
 20     CONTINUE
      END IF
C
      IF (NCATS .GE. 1) THEN
C       might have category timeseries to output
        DO 40 IC= 1, NCATS
          IF (CVOLFP(IC) .GT. 0) THEN
C           volume of water in category
            PAD(CVOLFP(IC) + IVL1)= CVOL(IC)
          END IF
          IF (CROFP(IC) .GT. 0) THEN
C           rate of outflow from category
            PAD(CROFP(IC) + IVL1)= CRO(IC)
          END IF
          DO 30 IX= 1, NEXITS
            IF (NEXITS .GT. 1) THEN
C             quantities belonging to each exit
              IF (COFP(IX,IC) .GT. 0) THEN
C               rate of outflow from exit-category
                PAD(COFP(IX,IC) + IVL1)= CO(IX,IC)
              END IF
            END IF
            IF (CDFVFP(IX,IC) .GT. 0) THEN
C             cumulative deficit
              PAD(CDFVFP(IX,IC) + IVL1)= CDFVOL(IX,IC)
            END IF
 30       CONTINUE
 40     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYDRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for module section HYDR.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + HISTORY + + +
C     2005  BRB added potential et to output summaries
C
C     + + + END SPECIFICATIONS + + +
C
C     inflow flux
      HYIF(LEV)= 0.0
C     other fluxes - 1:prsupy, 2:volev, 3:rovol
      I= 3
      CALL SETVEC (I,0.0,
     O             HYCF1(1,LEV))
C     category inflow flux
      CALL SETVEC (MXCAT,0.0,
     O             HYIFC(1,LEV))
C
      IF (NCATS .GT. 0) THEN
C       category outflow flux
        CALL SETVEC (MXCAT,0.0,
     O               HYCF1C(1,LEV))
      END IF
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        CALL SETVEC (NEXITS,0.0,
     O               HYCF2(1,LEV))
        IF (NCATS .GT. 0) THEN
C         category outflow
          DO 10 I= 1, NCATS
            CALL SETVEC (NEXITS,0.0,
     O                   HYCF2C(1,I,LEV))
 10       CONTINUE
        END IF
      END IF
C
C     potential et
      POTEV(LEV)= 0.0
C
      IF (IREXIT .GE. 1) THEN
C       irrigation fluxes
        I= 2
        CALL SETVEC (I,0.0,
     O               HYIRCF(1,LEV))
      END IF
C
C     keep present volume in state variable used for balance check
      PREVOL(LEV - 1)= VOL
C
      RETURN
      END
C
C
C
      SUBROUTINE   HYIRST
C
C     + + + PURPOSE + + +
C     Reset irrigation variables
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HYDR2 + + +
      INCLUDE    'crhhd.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      RIRDEM= 0.0
      RIRSHT= 0.0
      RIRWDL= 0.0
C
      RETURN
      END
