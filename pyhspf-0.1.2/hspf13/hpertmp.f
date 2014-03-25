C
C
C
      SUBROUTINE   PPSTMP
C
C     + + + PURPOSE + + +
C     Process input for section pstemp
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PSTEMP1 + + +
      INCLUDE    'cplst.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,I1,I2,I4,RDFLAG
C
C     + + + FUNCTIONS + + +
      REAL      CELSUS
C
C     + + + EXTERNALS + + +
      EXTERNAL  CELSUS,ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PSTEMP')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PSTEMP')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     process values in table-type pstemp-parm1
      I2= 47
      I4= 4
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             PTPM1)
C
C     check to see if we need to read pstemp-parm2
      RDFLAG= 0
      DO 10 J= 1, 3
        IF (PTPM1(J) .EQ. 0) THEN
C         parm is constant - need table
          RDFLAG= 1
        END IF
 10   CONTINUE
C
      IF (RDFLAG .EQ. 1) THEN
C       process values in table-type pstemp-parm2
        I2= 48
        IF (TSOPFG .EQ. 1) THEN
C         lgtp2 not used - only get first 5 values
          I4= 5
        ELSE
C         need lgtp2
          I4= 6
        END IF
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               PTST2)
      ELSE
C       dummy values until dayval is called on first interval
        DO 20 J= 1, 6
          PTST2(J)= 0.0
 20     CONTINUE
      END IF
C
C     convert to internal units
      IF (UUNITS.EQ.1) THEN
C       aslt already converted
        IF (TSOPFG.EQ.1) THEN
          ULTP1= CELSUS(ULTP1)
          LGTP1= CELSUS(LGTP1)
        ELSE
C         tspofg is 0 or 2 - only difference is lower/gw layer
C         temperature is function of upper temperature instead of
C         air temperature
          ULTP2= 0.555*ULTP2
C         lgtp2 already converted
        END IF
      END IF
C
      IF (SLTVFG.EQ.1) THEN
C       get monthly values of surface layer temp. regression params -
C       mon-aslt
        I2= 49
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ASLTM)
C
C       table-type mon-bslt
        I2= 50
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               BSLTM)
      END IF
C
      IF (ULTVFG.EQ.1) THEN
C       get monthly values of upper layer temp.
C       calculation parameters
C       table-type mon-ultp1
        I2= 51
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ULTP1M)
C
C       table-type mon-ultp2
        I2= 52
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ULTP2M)
C
        IF (UUNITS.EQ.1) THEN
C         convert to internal units
          IF (TSOPFG.EQ.1) THEN
            DO 30 J= 1,12
              ULTP1M(J)= CELSUS(ULTP1M(J))
 30         CONTINUE
          ELSE
            DO 40 J= 1,12
              ULTP2M(J)= 0.555*ULTP2M(J)
 40         CONTINUE
          END IF
        END IF
      END IF
C
      IF (LGTVFG.EQ.1) THEN
C       get monthly values of lower and groundwater
C       layer temp. calculation parameters
C       table-type mon-lgtp1
        I2= 53
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               LGTP1M)
C
        IF (TSOPFG.EQ.1) THEN
          IF (UUNITS .EQ. 1) THEN
C           convert english to metric (internal)
            DO 50 J= 1,12
              LGTP1M(J)= CELSUS(LGTP1M(J))
 50         CONTINUE
          END IF
        ELSE
C         table-type mon-lgtp2
          I2= 54
          I4= 12
          CALL RTABLE (I2,I1,I4,UUNITS,
     M                 LGTP2M)
        END IF
      END IF
C
C     initial values - table-type pstemp-temps
      I2= 55
      I4= 4
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             PTST1)
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSTEMP
C
C     + + + PURPOSE + + +
C     Estimate soil temperatures in a pervious land segment
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PSTEMP2 + + +
      INCLUDE 'cplst.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL,I,I6,ERRFG,SCLU,SGRP
      REAL        AULT,BULT,LGSMO,LGTDIF,LGTMPS,ULSMO,ULTDIF,ULTMPS,
     1            TMP(3),FIXTMP(2)
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM,LAY(3)
C
C     + + + FUNCTIONS + + +
      REAL      DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYVAL,HREQTS,OMSTD,OMSTI,OMSTC,OMSTR,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FIXTMP/-100,100/
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM,MSECNM/'PERLND','PSTEMP','ATEMP '/
C
C     + + + SAVE VARIABLES + + +
      INTEGER      ERRCNT
      SAVE         ERRCNT
C      
C     + + + HISTORY + + +
C     8/6/2004 BRB added check of computed soil temperatures to avoid 
C     computational errors
C
C     + + + END SPECIFICATIONS + + +
C
      I6= 6
      IF(STFG .EQ. 1) THEN
        ERRCNT= 0
      END IF
C      
C     obtain latest values for temperature calculation parameters
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (SLTVFG.EQ.1) THEN
C         surface layer temperature regression parameters are
C         allowed to vary throughout the year
C         interpolate for the daily values
C         linearly interpolate aslt between two values from the
C         monthly array asltm(12)
          ASLT= DAYVAL(ASLTM(MON),ASLTM(NXTMON),DAY,NDAYS)
C         linearly interpolate bslt between two values from the
C         monthly array bsltm(12)
          BSLT= DAYVAL(BSLTM(MON),BSLTM(NXTMON),DAY,NDAYS)
        ELSE
C         surface temperature regression parameters do not vary
C         throughout the year. values for aslt and bslt have been
C         supplied by the run interpreter
        END IF
C
        IF (ULTVFG.EQ.1) THEN
C         upper layer temperature parameters are allowed to
C         vary throughout the year
C         interpolate for the daily values
C         linearly interpolate ultp1 between two values from the
C         monthly array ultp1m(12)
          ULTP1= DAYVAL(ULTP1M(MON),ULTP1M(NXTMON),DAY,NDAYS)
C         linearly interpolate ultp2 between two values from the
C         monthly array ultp2m(12)
          ULTP2= DAYVAL(ULTP2M(MON),ULTP2M(NXTMON),DAY,NDAYS)
        ELSE
C         upper layer temperature parameters do not vary throughout
C         the year. values for ultp1 and ultp2 have been supplied by
C         the run interpreter
        END IF
C
        IF (LGTVFG.EQ.1) THEN
C         lower layer and groundwater temperature parameters are
C         allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate lgtp1 between two values from the
C         monthly array lgtp1m(12)
          LGTP1= DAYVAL(LGTP1M(MON),LGTP1M(NXTMON),DAY,NDAYS)
          IF (TSOPFG.NE.1) THEN
C           second parameter is needed
C           linearly interpolate lgtp2 between two values from the
C           monthly array lgtp2m(12)
            LGTP2= DAYVAL(LGTP2M(MON),LGTP2M(NXTMON),DAY,NDAYS)
          END IF
C
        ELSE
C         lower layer and groundwater temperature parameters do not
C         vary throughout the year. lgtp1 value and lgtp2 value if
C         needed have been supplied by the run interpreter
        END IF
C
      END IF
C
      IF (AIRTFG.EQ.0) THEN
C       read from the inpad
C       if start of run, set airts equal to airtmp
CTHJ        IF (STFG .EQ. 1) AIRTS= PAD(AIRTFP+IVL1)
CTHJ        AIRTMP= PAD(AIRTFP+IVL1)
        REQFG= 2
        TSNAM= 'AIRTMP'
        CALL HREQTS (AIRTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AIRTMP)
        IF (STFG .EQ. 1) AIRTS= AIRTMP
      ELSE
C       air temperatures, in degrees f, are available from section
C       atemp
      END IF
C
C     convert to centigrade
      AIRTCS= (AIRTS-32.0)*0.555
      AIRTC = (AIRTMP-32.0)*0.555
C
C     determine soil temperatures - units are deg c
C     temperature of surface layer is always estimated using a
C     linear regression with air temperature
C
      IF (HRFG.EQ.1) THEN
C       it is time to update surface layer temperature
        SLTMP= ASLT+ BSLT*AIRTC
      END IF
C
      IF (TSOPFG.EQ.1) THEN
C       compute subsurface temperature using regression and
C       monthly values
        IF (HRFG.EQ.1) THEN
C         it is time to update subsurface temperatures
C         temperature of upper layer is computed by regression
C         with air temperature
          AULT = ULTP1
          BULT = ULTP2
          ULTMP= AULT+ BULT*AIRTC
C
C         temperature of lower layer and groundwater were
C         interpolated from monthly values
          LGTMP= LGTP1
        END IF
C
      ELSE
C       tsopfg is 0 or 2
C       compute subsurface temperatures using a mean departure
C       from air temperature plus a smoothing factor -
C       if tsopfg is 2, the lower/gw layer temperature is
C       a function of upper layer temperature instead of
C       air temperature
        ULSMO = ULTP1
        ULTDIF= ULTP2
        ULTMPS= ULTMP
        ULTMP = ULTMPS+ ULSMO*(AIRTCS+ULTDIF-ULTMPS)
C
        LGSMO = LGTP1
        LGTDIF= LGTP2
        LGTMPS= LGTMP
        IF (TSOPFG .EQ. 0) THEN
C         original method - lower/gw temp based on air temp
          LGTMP = LGTMPS+ LGSMO*(AIRTCS+LGTDIF-LGTMPS)
        ELSE
C         new method for corps of engineers 10/93 -
C         lower/gw temp based on upper temp
          LGTMP = LGTMPS+ LGSMO*(ULTMP+LGTDIF-LGTMPS)
        END IF
      END IF
C   
C     check temperatures for invalid results
C     this error stops printing after 20 total occurrences
C     it is not fatal, so that pre-existing models do not stop
C     (note: this message is stored in the PEST seq file)
      SCLU= 309
      LAY(1)= 'SLTMP '
      LAY(2)= 'ULTMP '
      LAY(3)= 'LGTMP '
      TMP(1)= SLTMP
      TMP(2)= ULTMP
      TMP(3)= LGTMP
      DO 10 I= 1, 3
        ERRFG= 0
        IF (TMP(I) .LT. -100.) THEN
          ERRFG= 1
        ELSE IF (TMP(I) .GT. 100.) THEN  
          ERRFG= 2
        END IF
        IF (ERRFG .NE. 0) THEN
          CALL OMSTD (DATIM)
          CALL OMSTI (LSNO)
          CALL OMSTC (I6,LAY(I))
          CALL OMSTR (TMP(I))
          CALL OMSTR (FIXTMP(ERRFG))
          SGRP = 3
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ERRCNT)
          TMP(I)= FIXTMP(ERRFG)
        END IF
 10   CONTINUE      
      SLTMP= TMP(1)
      ULTMP= TMP(2)
      LGTMP= TMP(3)
C
C     update airts for next interval if section atemp not active
      IF (AIRTFG .EQ. 0) AIRTS= AIRTMP
C
      RETURN
      END
C
C     4.2(1).13.5
C
      SUBROUTINE PSTPT
C
C     + + + PURPOSE + + +
C     Handle section PSTEMP.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PSTEMP2 + + +
      INCLUDE 'cplst.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pstemp
      IF (AIRTCX.GE.1) THEN
        PAD(AIRTCX+IVL1)=AIRTC
      END IF
C
      IF (SLTFP.GE.1) THEN
        PAD(SLTFP+IVL1) = SLTMP
      END IF
C
      IF (ULTFP.GE.1) THEN
        PAD(ULTFP+IVL1) = ULTMP
      END IF
C
      IF (LGTFP.GE.1) THEN
        PAD(LGTFP+IVL1) = LGTMP
      END IF
C
      RETURN
      END
C
C     4.2(1).15.2.5
C
      SUBROUTINE PTPRT
     I                 (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quanities from internal to external units and print out
C     results.  the array pstat1 has identical structure as pptst1
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    UNITFG,LEV,PRINTU,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PSTEMP2 + + +
      INCLUDE    'cplst.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,I0,I1,I4,ACNT,CLEN(4),EXDAT(5)
      REAL          PSTAT1(4),TFACTA,TFACTB,APRINT(4)
      CHARACTER*8   CSTAT1(4)
      CHARACTER*256 CHEAD(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PSTEMP ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT (/,'     TEMPERATURES (DEG F)',13X,
     $        'AIR<--------SOIL LAYERS--------->')
 2030 FORMAT (/,'     TEMPERATURES (DEG C)',13X,
     $        'AIR<--------SOIL LAYERS--------->')
 2040 FORMAT (' ',43X,'SURFACE     UPPER  LOWER/GW')
 2050 FORMAT (' ',36X,'AIRT     SLTMP     ULTMP     LGTMP')
 2060 FORMAT (' ',30X,4F10.1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0=0
      I1=1
      I4=4
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT1(1) = 'AIRT'
      CSTAT1(2) = 'SLTMP'
      CSTAT1(3) = 'ULTMP'
      CSTAT1(4) = 'LGTMP'
C
C     pstemp has no fluxes only state variables
C
      IF (UNITFG.NE.1) GO TO 10
C       english system
        TFACTA= 1.8
        TFACTB= 32.0
        GO TO 20
C
 10   CONTINUE
C       metric system
        TFACTA= 1.0
        TFACTB= 0.0
 20   CONTINUE
C
C     convert to external units
      CALL TRNVEC (I4,PTST1,TFACTA,TFACTB,
     O             PSTAT1)
C
      IF (PRINTU .GT. 0) THEN
C       write to unit printu
        WRITE (PRINTU,2000)
        WRITE (PRINTU,2010)
C
        IF (UNITFG.NE.1) GO TO 30
          WRITE (PRINTU,2020)
          GO TO 40
 30     CONTINUE
          WRITE (PRINTU,2030)
 40     CONTINUE
C
        WRITE (PRINTU,2040)
        WRITE (PRINTU,2050)
        WRITE (PRINTU,2060)  PSTAT1
      END IF
C
      IF (BINU .GT. 0) THEN
C       compile values for binary printout
        DO 50 I = 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT1(I)
          CHEAD(ACNT) = CSTAT1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50     CONTINUE
      END IF
C
      IF (BINU .GT. 0) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(5) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'PSTEMP  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(5) = -BFLAG(5)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'PSTEMP  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
