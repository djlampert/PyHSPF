C
C
C
      SUBROUTINE   PSHADE
     I                   (RVAL)
C
C     + + + PURPOSE + + +
C     Process shade input data.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL        RVAL(7)                       
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RVAL   - parameters from table SHADE-PARM
C      
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH1 + + +
      INCLUDE    'crhsh.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,TOPERR,VEGERR
C
C     + + + END SPECIFICATIONS + + +
C
C     cluster containing error and warning message details
      SCLU= 344
C
      TOPFL=  RVAL(2)
      VEGFL=  RVAL(3)
      NSSP=   RVAL(4)
      LATDEG= RVAL(5)
      LONDEG= RVAL(6)
      LONSTD= RVAL(7)
      LATRAD= LATDEG*0.0174533
      IF (LONSTD .GT. 0) DLT1= -(LONSTD - LONDEG)/15.
      IF (LONSTD .LT. 0) DLT1=  (LONSTD - LONDEG)/15.
C
C     get topographic shade data
      CALL RTOPOG (TOPFL,NSSP,MXSP,RCHNO,
     O             SO,TSA,TSAL,TSAR,TOPERR)
C
      IF (TOPERR .EQ. 1) THEN
C       error reading topographic data file
        SGRP= 11
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     get vegetation data
      CALL RVEG (VEGFL,NSSP,MXSP,MXPL,RCHNO,TSAL,TSAR,
     O           SKOP,VSAL,VSAR,NVPL,NVPR,DISL,DISR,WIDL,WIDR,
     O           HABSL,HABSR,HDEML,HDEMR,DENL,DENR,VEGERR)
C
      IF (VEGERR .EQ. 1) THEN
C       error reading vegetation data file
        SGRP= 12
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      RETURN
      END  
C
C
C
      SUBROUTINE   RTOPOG
     I                    (TOPFL,NSSP,MXSP,RCHNO,
     O                     SO,TSA,TSAL,TSAR,ERRFG)
C
C     + + + PURPOSE + + +
C     Read topographic input file for reach and compute stream orientation
C     and topographic shade angle in the two directions perpendicular to 
C     the stream.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TOPFL,NSSP,MXSP,RCHNO,TSA(MXSP,12),ERRFG       
      REAL        SO(MXSP),TSAL(MXSP),TSAR(MXSP)                       
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TOPFL  - unit number of topographic data file                  
C     NSSP   - number of stream sample points (SSP) in RCHRES
C     MXSP   - maximum number of SSP's
C     RCHNO  - RCHRES ID number for comparison with value from file
C     SO     - stream orientation in units of radians 
C     TSA    - topographic shade angles in 12 directions (degrees, integer)
C     TSAL   - topographic shade angle perpendicular to left bank (radians)
C     TSAR   - topographic shade angle perpendicular to right bank (radians)
C     ERRFG  - error flag (-) =0 if no errors; =1 if error(s) detected
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     NSP,I,J,IRR,ISP,J1,J2,NAZL,NAZR,XUTM(250),YUTM(250)
      REAL        PI,AZL,AZR,AZLDEG,AZRDEG,AZ1,AZ2,XABS,YABS
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,ATAN,INT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2I4,2I8,17X,12I3) 
C
C     + + + END SPECIFICATIONS + + +
C
      PI= 3.141592654
      ERRFG= 0
C
C     read topography data
      NSP= 0
      DO 10 I= 1, MXSP
        READ (TOPFL,1000,ERR=60,END=20) IRR,ISP,XUTM(I),YUTM(I),
     1                 (TSA(I,J),J=1,12)
        IF (IRR .NE. RCHNO .OR. ISP .NE. I) GO TO 60
        NSP= NSP + 1    
 10   CONTINUE
C 
C     number of sample points in the current RCHRES (RCHNO)
 20   CONTINUE
        IF (NSP .NE. NSSP) GO TO 60
C
C     compute stream orientation and topographic shade angles in
C     the two directions perpendicular to the stream
      DO 50 I= 1, NSP
	IF (XUTM(I+1) .GT. XUTM(I) .AND. YUTM(I+1) .EQ. YUTM(I)) THEN 
          SO(I)= 0.5*PI
	  TSAL(I)= TSA(I,1)*PI/180.
          TSAR(I)= TSA(I,7)*PI/180.
          GO TO 40
	END IF
C
	IF (XUTM(I+1) .LT. XUTM(I) .AND. YUTM(I+1) .EQ. YUTM(I)) THEN 
          SO(I)= 1.5*PI
	  TSAL(I)= TSA(I,7)*PI/180.
          TSAR(I)= TSA(I,1)*PI/180.
	  GO TO 40
	END IF
C	
	IF (XUTM(I+1) .EQ. XUTM(I) .AND. YUTM(I+1) .GT. YUTM(I)) THEN 
          SO(I)= 0
	  TSAL(I)= TSA(I,10)*PI/180.
          TSAR(I)= TSA(I,4)*PI/180.
          GO TO 40
	END IF
C
	IF (XUTM(I+1) .EQ. XUTM(I) .AND. YUTM(I+1) .LT. YUTM(I)) THEN 
          SO(I)= PI
          TSAL(I)= TSA(I,4)*PI/180.
          TSAR(I)= TSA(I,10)*PI/180.
          GO TO 40
	END IF
C
        XABS= ABS(XUTM(I+1) - XUTM(I))
        YABS= ABS(YUTM(I+1) - YUTM(I))
        SO(I)= ATAN(XABS/YABS)
C
	IF (XUTM(I+1) .GT. XUTM(I) .AND. YUTM(I+1) .GT. YUTM(I)) THEN
          SO(I)= SO(I)
          AZL= SO(I) + 1.5*PI
	  AZR= SO(I) + 0.5*PI
	  GO TO 30
	END IF
C	
	IF (XUTM(I+1) .GE. XUTM(I) .AND. YUTM(I+1) .LT. YUTM(I)) THEN
          SO(I)= PI - SO(I)
          AZL= SO(I) - 0.5*PI
	  AZR= SO(I) + 0.5*PI
	  GO TO 30
	END IF
C	
        IF (XUTM(I+1) .LT. XUTM(I) .AND. YUTM(I+1) .LT. YUTM(I)) THEN
          SO(I)= PI + SO(I)
	  AZL= SO(I) - 0.5*PI
	  AZR= SO(I) + 0.5*PI
	  GO TO 30
	END IF	  
C
        IF (XUTM(I+1) .LE. XUTM(I) .AND. YUTM(I+1) .GT. YUTM(I)) THEN
          SO(I)= 2.*PI - SO(I)
	  AZL= SO(I) - 0.5*PI
	  AZR= SO(I) - 1.5*PI
	  GO TO 30
	END IF	  
C
30      CONTINUE
          AZLDEG= AZL*180./PI
          AZRDEG= AZR*180./PI
          NAZL= INT(AZLDEG/30.)
          NAZR= INT(AZRDEG/30.)
C
          AZ1= AZLDEG - NAZL*30.
          AZ2= 30. - AZ1
	  J1= NAZL + 1
	  J2= NAZL + 2
	  IF (J2 .EQ. 13) J2= 1
          TSAL(I)= (AZ1*TSA(I,J1) + AZ2*TSA(I,J2))/(30.*180./PI)
C
          AZ1= AZRDEG - NAZR*30.
          AZ2= 30. - AZ1
	  J1= NAZR + 1
	  J2= NAZR + 2
	  IF (J2 .EQ. 13) J2= 1
          TSAR(I)= (AZ1*TSA(I,J1) + AZ2*TSA(I,J2))/(30.*180./PI)
C
 40     CONTINUE
          IF (I .EQ. (NSP - 1)) THEN
            TSAL(I+1)= (AZ1*TSA(I+1,J1) + AZ2*TSA(I+1,J2))/(30.*180./PI)
            TSAR(I+1)= (AZ1*TSA(I+1,J1) + AZ2*TSA(I+1,J2))/(30.*180./PI)
          END IF
 50   CONTINUE
C
C     orientation of the last two sample points assumed identical 
      SO(NSP)= SO(NSP-1)
C      
      GO TO 70
C
 60   CONTINUE
C     error on read - one of the following occurred:
C       error reading a number
C       end of file before number of sample points (NSSP)
C       incorrect sample point ID number (must be in numerical order)
C       incorrect RCHRES ID number
        ERRFG= 1
C
 70   CONTINUE
C     completed processing
C
      RETURN
      END  
C
C
C
      SUBROUTINE   RVEG
     I                  (VEGFL,NSSP,MXSP,MXPL,RCHNO,TSAL,TSAR,
     O                   SKOP,VSAL,VSAR,NVPL,NVPR,DISL,DISR,WIDL,WIDR,
     O                   HABSL,HABSR,HDEML,HDEMR,DENL,DENR,ERRFG)
C
C     + + + PURPOSE + + +
C     Read vegetation input file for reach, identify highest forest   
C     polygon, and compute the vegetation shade angles and sky 
C     openness.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     VEGFL,NSSP,MXSP,MXPL,RCHNO,NVPL(MXSP),NVPR(MXSP),
     $            DISL(MXSP,MXPL),DISR(MXSP,MXPL),WIDL(MXSP,MXPL),
     $            WIDR(MXSP,MXPL),HABSL(MXSP,MXPL),HABSR(MXSP,MXPL),
     $            HDEML(MXSP,MXPL),HDEMR(MXSP,MXPL),ERRFG          
      REAL        SKOP(MXSP),TSAL(MXSP),TSAR(MXSP),VSAL(MXSP),
     $            VSAR(MXSP),DENL(MXSP,MXPL),DENR(MXSP,MXPL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VEGFL  - unit number of vegetation data file                  
C     NSSP   - number of stream sample points (SSP) in RCHRES
C     MXSP   - maximum number of SSP's
C     MXPL   - maximum number of polygons per SSP on each side of stream
C     RCHNO  - RCHRES ID number for comparison with value from file
C     SKOP   - sky openness (-) (0 - 1) 
C     TSAL   - topographic shade angle perpendicular to left bank (radians)
C     TSAR   - topographic shade angle perpendicular to right bank (radians)
C     VSAL   - vegetation shade angle perpendicular to left bank (radians)
C     VSAR   - vegetation shade angle perpendicular to right bank (radians)
C     NVPL   - number of vegetation polygons on left bank (-)
C     NVPR   - number of vegetation polygons on right bank (-)
C     DISL   - distance from left edge of stream to vegetation polygon (meters, integer)
C     DISR   - distance from right edge of stream to vegetation polygon (meters, integer)
C     WIDL   - width of vegetation polygon on left side of stream (meters, integer)
C     WIDR   - width of vegetation polygon on right side of stream (meters, integer)
C     HABSL  - average absolute height of left side vegetation polygon (meters, integer)
C     HABSR  - average absolute height of right side vegetation polygon (meters, integer)
C     HDEML  - average height of left side vegetation polygon in reference to 
C              stream surface (meters, integer)
C     HDEMR  - average height of right side vegetation polygon in reference to 
C              stream surface (meters, integer)
C     DENL   - average density of left side vegetation canopy (-) (0-1)
C     DENR   - average density of right side vegetation canopy (-) (0-1)
C     ERRFG  - error flag (-) =0 if no errors; =1 if error(s) detected
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,IRR,ISP,NMXHL,NMXHR
      REAL        PI,MXHL,MXHR
      CHARACTER*1 BANK,FGCODE
C
C     + + + INTRINSICS + + +
      INTRINSIC  AMAX1,ATAN
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2I4,2(1X,A1))
 1010 FORMAT (20X,4I6,G6.0)
C
C     + + + END SPECIFICATIONS + + +
C
      PI= 3.141592654
      ERRFG= 0
C  
C     read vegetation data, identify the highest forest polygon, and
C     compute the vegetation shade angles and sky openness
C
C     loop of SSPs
      DO 30 I= 1, NSSP
        NVPL(I)= 0
        NVPR(I)= 0
        MXHL= 0.
        MXHR= 0.
        NMXHL= 0
        NMXHR= 0
C        
C       loop of polygons 
        DO 10 J= 1, MXPL*2         
          READ (VEGFL,1000,ERR=40,END=50) IRR,ISP,BANK,FGCODE
          IF (ISP .EQ. (I+1)) THEN
C           next SSP          
      	    BACKSPACE (VEGFL)
	    GO TO 20
          END IF
C
          IF (IRR .NE. RCHNO .OR. ISP .NE. I) GO TO 40
C
          IF (FGCODE .EQ. 'F') THEN
C           forest polygon - process
            BACKSPACE (VEGFL)
C
            IF (BANK .EQ. 'L') THEN
C             left bank            
              NVPL(I)= NVPL(I) + 1
              READ (VEGFL,1010,ERR=40) DISL(I,NVPL(I)),WIDL(I,NVPL(I)),
     1             HABSL(I,NVPL(I)),HDEML(I,NVPL(I)),DENL(I,NVPL(I))
              IF (MXHL .LT. HDEML(I,NVPL(I))) THEN
    	        MXHL= HDEML(I,NVPL(I))
	        NMXHL= NVPL(I)
              END IF 
            END IF
C          	 
            IF (BANK .EQ. 'R') THEN
              NVPR(I)= NVPR(I) + 1
              READ (VEGFL,1010,ERR=40) DISR(I,NVPR(I)),WIDR(I,NVPR(I)),
     1               HABSR(I,NVPR(I)),HDEMR(I,NVPR(I)),DENR(I,NVPR(I))
              IF (MXHR .LT. HDEMR(I,NVPR(I))) THEN
    	        MXHR= HDEMR(I,NVPR(I))
	        NMXHR= NVPR(I)
              END IF           
            END IF
C            
          ELSE IF (FGCODE .EQ. 'G' .OR. FGCODE .EQ. 'X') THEN
C           gap polygon - anything needed?
          END IF
C
cthj          GO TO 30
 10     CONTINUE
C
 20     CONTINUE
        IF (NMXHL .EQ. 0 .OR. NMXHL .GT. MXPL .OR.
     1      NMXHR .EQ. 0 .OR. NMXHR .GT. MXPL) THEN
C         error 
          GO TO 40
        ELSE
          IF (DISL(I,NMXHL) .EQ. 0) DISL(I,NMXHL)= 1
          VSAL(I)= ATAN(MXHL/DISL(I,NMXHL))
          IF (DISR(I,NMXHR) .EQ. 0) DISR(I,NMXHR)= 1
          VSAR(I)= ATAN(MXHR/DISR(I,NMXHR))   
          SKOP(I)= (PI - AMAX1(TSAL(I),VSAL(I)) - 
     1              AMAX1(TSAR(I),VSAR(I)))/PI
        END IF
C
 30   CONTINUE
C      
      GO TO 50
C
 40   CONTINUE
C     error on read - one of the following occurred:
C       error reading a number
C       end of file before number of sample points (NSSP)
C       incorrect sample point ID number (must be in numerical order)
C       incorrect RCHRES ID number
        ERRFG= 1
C
C     completed processing
 50   CONTINUE
C
      RETURN
      END  
C
C
C
      SUBROUTINE   SHADEH
     O                    (SRAD)
C
C     + + + PURPOSE + + +
C     Compute stream shading based on topographic features and vegetation.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL        SRAD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SRAD   - radiation absorbed by stream
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH1 + + +
      INCLUDE    'crhsh.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,NDM(12)
      REAL        HRADG,HRADB,HRADD,TWIDM,SS,CC,TT,RR,RRSQ,LHA,RADEH,KT
C
C     + + + INTRINSICS + + +
      INTRINSIC  COS,SIN,ACOS,INT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2I4,2I8,17X,12I3) 
C
C     + + + END SPECIFICATIONS + + +
C
      DATA  NDM/31,28,31,30,31,30,31,31,30,31,30,31/
C
      IF (DAYFG .EQ. 1) THEN
C       compute the Julian day
        JDAY= 0
        IF (MON .NE. 1) THEN	
          DO 10 I= 1, MON-1
            JDAY= JDAY + NDM(I)
 10       CONTINUE
        END IF 
        JDAY= JDAY + DAY
C          
        DEC=   0.40928*COS(0.01721*(172. - JDAY))
        SS=    SIN(LATRAD)*SIN(DEC)
        CC=    COS(LATRAD)*COS(DEC)
        TT=    -SS/CC
        HFDLT= 57.29578*ACOS(TT)/15.
        RR=    1.0 + 0.017*COS(0.01721*(186. - JDAY))
        RRSQ=  RR*RR
C       RADEI= (2.0*2.0*HFDLT*60.)/RRSQ (RADEI is never used)
        LHA=   0.261873*HFDLT
        RADEH= (458.36624*2.0*CC*(SIN(LHA) - LHA*COS(LHA)))/RRSQ
C       RADEH= (458.36624*2.0*(LHA*SS + CC*SIN(LHA)))/RRSQ
C       divide daily global radiation into beam and diffuse
        KT= DSOLAR/RADEH
        DF= 0.938 + 1.071*KT - 5.146*KT**2 + 2.982*KT**3 - 
     1      (0.009 - 0.078*KT)*SIN(0.017214*(JDAY-40))
        IF (DF .GT. 1.) THEN
          DF= 1.
        END IF 
      END IF
C
C     dis-aggregate current time step radiation into beam and diffuse      
      HRADG= SOLRAD
      IF (DF .GE. 1.) THEN
        HRADD= HRADG
        HRADB= 0.0
      ELSE
        HRADD= DF*HRADG
        HRADB= HRADG - HRADD
      END IF 
C
C     compute standard sunrise and sunset hour (not needed)
C     IRIS= INT(12. - HFDLT + DLT1) + 1
C     ISET= INT(12. + HFDLT + DLT1) + 1
C
C     if necessary, convert TWID to meters
      TWIDM= TWID
      IF (UUNITS .EQ. 1) THEN
        TWIDM= TWID*0.3048
      END IF
C
      CALL SHDCLC (TWIDM,HR,DLT1,LATRAD,DEC,NSSP,MXSP,MXPL,
     I             TSA,SO,NVPL,NVPR,DISL,DISR,WIDR,WIDL,
     I             HABSL,HABSR,HDEML,HDEMR,DENL,DENR,SKOP,
     I             HRADG,HRADD,HRADB,
     O             SRAD,SHDFAC)
C
      RETURN
      END  
C
C
C
      SUBROUTINE   SHDCLC
     I                    (TWID,HR,DLT1,LATRAD,DEC,NSSP,MXSP,MXPL,
     I                     TSA,SO,NVPL,NVPR,DISL,DISR,WIDR,WIDL,
     I                     HABSL,HABSR,HDEML,HDEMR,DENL,DENR,SKOP,
     I                     HRADG,HRADD,HRADB,
     O                     RADSHD,SHDFAC)
C
C     + + + PURPOSE + + +
C     Compute shading factor for current time based on computations
C     in David Chen's SHADE program.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     HR,NSSP,MXSP,MXPL,TSA(MXSP,12),NVPL(MXSP),
     $            NVPR(MXSP),DISL(MXSP,MXPL),DISR(MXSP,MXPL),
     $            WIDL(MXSP,MXPL),WIDR(MXSP,MXPL),
     $            HABSL(MXSP,MXPL),HABSR(MXSP,MXPL),
     $            HDEML(MXSP,MXPL),HDEMR(MXSP,MXPL)
      REAL        TWID,DLT1,LATRAD,DEC,SO(MXSP),
     $            DENL(MXSP,MXPL),DENR(MXSP,MXPL),SKOP(MXSP),
     $            HRADG,HRADD,HRADB,RADSHD,SHDFAC                       
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TWID   - top width of stream (m)
C     HR     - hour of day 
C     DLT1   - time difference between standard and local times (hours)
C     LATRAD - latitude (radians)
C     DEC    - solar declination (radians)
C     NSSP   - number of stream sample points in RCHRES
C     MXSP   - maximum number of SSP's
C     MXPL   - maximum number of polygons per SSP on each side of stream
C     TSA    - topographic shade angles in 12 directions (degrees, integer)
C     SO     - stream orientation (radians) 
C     NVPL   - number of vegetation polygons on left bank
C     NVPR   - number of vegetation polygons on right bank
C     DISL   - distance from left edge of stream to vegetation polygon (meters, integer)
C     DISR   - distance from right edge of stream to vegetation polygon (meters, integer)
C     WIDL   - width of vegetation polygon on left side of stream (meters, integer)
C     WIDR   - width of vegetation polygon on right side of stream (meters, integer)
C     HABSL  - average absolute height of left side vegetation polygon (meters, integer)
C     HABSR  - average absolute height of right side vegetation polygon (meters, integer)
C     HDEML  - average height of left side vegetation polygon in reference to 
C              stream surface (meters, integer)
C     HDEMR  - average height of right side vegetation polygon in reference to 
C              stream surface (meters, integer)
C     DENL   - average density of left side vegetation canopy (-) (0-1)
C     DENR   - average density of right side vegetation canopy (-) (0-1)
C     SKOP   - sky openness (-) (0 - 1) 
C     HRADG  - input radiation for current time step (langleys)
C     HRADD  - diffuse radiation for current time step (langleys)
C     HRADB  - beam radiation for current time step (langleys)
C     RADSHD - radiation absorbed by stream after shading (langleys)
C     SHDFAC - effective shading factor - fraction of incident global radiation not absorbed(-)
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1 SWBK
      INTEGER     ISP,NAZ,K1,K2,NVP,NCB,IB,J,L,JMAX,K
      REAL        LHA,COSZEN,ZEN,PI,ALT,SINAZ,AZ,ZENDEG,ALTDEG,AZDEG,
     $            ALDO,AZ1,AZ2,TSAZ,SOSP,FRCSHD,DIS(MXPL),WID(MXPL),
     $            HABS(MXPL),HDEM(MXPL),DEN(MXPL),SINFAC,HMIN,
     $            SHDWID(MXPL),LAMDA,PLMAX,PLMIN,HFCV,AVEPL,
     $            SHDDEN(MXPL),TEMP,VEGSHD,DENMAX,ACDEN,OVHSHD,
     $            RADBSP,RADDSP,LOH
C
C     + + + INTRINSICS + + +
      INTRINSIC  SIN,COS,ACOS,ASIN,INT,ABS,TAN,ALOG,AMIN1,EXP
C
C     + + + END SPECIFICATIONS + + +
C
      PI = 3.141592654
      RADSHD= 0.
C      
C     compute solar zenith and azimuth at the middle of current hour 
      IF (HR .LT. 12) LHA= (HR + 12 - DLT1 - 0.5)*0.26180
      IF (HR .GE. 12) LHA= (HR - 12 - DLT1 - 0.5)*0.26180
      COSZEN= SIN(LATRAD)*SIN(DEC) + COS(LATRAD)*COS(DEC)*COS(LHA)
      IF (COSZEN .GT. 1.) COSZEN= 1.
      IF (COSZEN .LT. -1.) COSZEN= -1.
      ZEN= ACOS(COSZEN)
      IF (ZEN .GT. 0.5*PI) ZEN= 0.5*PI
      ALT= 0.5*PI - ZEN
      IF (ALT. LE. 0.) ALT= 0.
      SINAZ= COS(DEC)*SIN(LHA)/COS(ALT)
      IF (SINAZ .GT. 1.) SINAZ= 1.
      IF (SINAZ .LT. -1.) SINAZ= -1.
      AZ= ASIN(SINAZ) + PI
      ZENDEG= ZEN*180./PI
      ALTDEG= ALT*180./PI
      AZDEG= AZ*180./PI
C
C     compute albedo of water surface for beam radiation
      IF (ZENDEG .LE. 80.) THEN
        ALDO= 0.091/COS(ZEN) - 0.0386
      ELSE IF (ZENDEG .GT. 80.) THEN
        ALDO= 0.0515*ZENDEG - 3.635
      END IF
C
C     river sample point loops       
      DO 110 ISP= 1, NSSP
C
C       first component: beam-direct normal radiation
        NAZ= INT(AZDEG/30.)
        AZ1= AZDEG - NAZ*30.
        AZ2= 30. - AZ1
        K1= NAZ + 1
        K2= NAZ + 2
        IF (K2 .EQ. 13) K2= 1
        TSAZ= (AZ1*TSA(ISP,K1) + AZ2*TSA(ISP,K2))/30.
        SOSP= SO(ISP)
C
C       sunbeam parallel to the stream - no vegetation shade
        IF (AZ .EQ. SOSP .OR. AZ .EQ. (SOSP + 1.5708)) THEN
          FRCSHD= 0.
          GO TO 90
        END IF
C       sunbeam can not pass the mountain ridge - topographic shade
        IF (ALTDEG .LE. TSAZ) GO TO 100
C
C       no topographic shade, sunbeam reaches the valley, estimate 
C       vegetation shade 
C
C       identify the sunward bank
        IF (SOSP .LE. 1.5708) THEN
          IF (AZ .LT. SOSP .OR. AZ .GT. (SOSP + 1.5708)) THEN
            SWBK= 'L'
          ELSE IF (AZ .LT. SOSP .AND. AZ .LT. (SOSP + 1.5708)) THEN
            SWBK= 'R' 
          END IF
        END IF           
        IF (SOSP .GT. 1.5708) THEN
          IF (AZ .GT. (SOSP - 1.5708) .AND. AZ .LT. SOSP) THEN
            SWBK= 'L'
          ELSE IF (AZ .GT. SOSP .OR. AZ .LT. (SOSP - 1.5708)) THEN
            SWBK= 'R' 
          END IF
        END IF
C         
        IF (SWBK .EQ. 'L') THEN
          NVP= NVPL(ISP)
          IF (NVP .EQ. 0) THEN
            FRCSHD= 0.
            GO TO 90
          END IF
          DO 10 K= 1, NVP
            DIS(K)= DISL(ISP,K)
            WID(K)= WIDL(ISP,K)
            HABS(K)= HABSL(ISP,K)
            HDEM(K)= HDEML(ISP,K)
            DEN(K)= DENL(ISP,K)*0.01
 10       CONTINUE
        ELSE IF (SWBK .EQ. 'R') THEN
          NVP= NVPR(ISP)
          IF (NVP .EQ. 0) THEN
            FRCSHD= 0.
            GO TO 90
          END IF
          DO 20 K= 1, NVP
            DIS(K)= DISR(ISP,K)
            WID(K)= WIDR(ISP,K)
            HABS(K)= HABSR(ISP,K)
            HDEM(K)= HDEMR(ISP,K)
            DEN(K)= DENR(ISP,K)*0.01
 20       CONTINUE
        END IF
C
C       indentify and characterize shade-contributing buffers
        NCB= 0
        SINFAC= ABS(SIN(ABS(AZ - SOSP)))
        DO 30 IB= 1, NVP
          IF (DIS(IB) .LE. 0.0) DIS(IB)= 1.
          HMIN= DIS(IB)*TAN(ALT)/SINFAC
          IF (HDEM(IB) .GT. HMIN) THEN
C           buffer high enough to contribute shade
            NCB= NCB + 1
C           compute effective shade width (perpendicular to the stream) 
C           using Quigley's equation 
            SHDWID(NCB)= HDEM(IB)*SINFAC/TAN(ALT) - DIS(IB)
            IF (SHDWID(NCB) .LT. 0.) SHDWID(NCB)= 0.
            IF (SHDWID(NCB) .GT. TWID) SHDWID(NCB)= TWID
C           compute actual shade density using Beer's law
C           first, compute extinction coefficient 
            LAMDA= -ALOG(1 - DEN(IB))/HABS(IB)
C           second, compute average path length of sunbeam
            PLMAX= AMIN1(SHDWID(NCB),WID(IB))/(SINFAC*COS(ALT))
            HFCV= (TWID + DIS(IB))*TAN(ALT)/SINFAC
            IF (HDEM(IB) .LE. HFCV) THEN
              PLMIN= 0.
            ELSE
              PLMIN= TWID/(SINFAC*COS(ALT))
            END IF
            IF (PLMIN .GT. PLMAX) PLMIN= PLMAX
            AVEPL= (PLMAX + PLMIN)/2.
            IF (AVEPL .GT. 30.) AVEPL= 30. 
C           third, compute shade density
            SHDDEN(NCB)= 1. - EXP(-AVEPL*LAMDA)
c           shdden(ncb)= den(ib)
          ELSE
C           buffer not high enough to contribute shade     
          END IF
 30     CONTINUE
C       sorting of shades based on width in increasing order
        DO 50 IB= 1,NCB - 1
          L= IB + 1
          DO 40 J= L, NCB
            IF ((SHDWID(IB) - SHDWID(J)) .GT. 0.) THEN
              TEMP= SHDWID(IB)
              SHDWID(IB)= SHDWID(J)
              SHDWID(J)= TEMP
              TEMP= SHDDEN(IB)
              SHDDEN(IB)= SHDDEN(J)
              SHDDEN(J)= TEMP
            END IF
 40       CONTINUE
 50     CONTINUE
C       compute accumulated vegetation shade of all contributing buffers
        VEGSHD= 0.
        DO 80 IB= 1, NCB
          DENMAX= SHDDEN(IB)
          JMAX= IB
          DO 60 J= IB, NCB
            IF (DENMAX .LT. SHDDEN(J)) THEN
              DENMAX= SHDDEN(J)
              JMAX= J
            END IF
 60       CONTINUE
          ACDEN= 0
          DO 70 K= IB, NCB
            IF (K .NE. JMAX) THEN
              ACDEN= ACDEN + 0.2*SHDDEN(K)
            ELSE IF (K .EQ. JMAX) THEN
              ACDEN= ACDEN + SHDDEN(K)
            END IF
 70       CONTINUE
          IF (ACDEN .GT. 1.) ACDEN= 1.
          IF (IB .EQ. 1) THEN
            VEGSHD= VEGSHD + SHDWID(IB)*ACDEN
          ELSE
            VEGSHD= VEGSHD + (SHDWID(IB) - SHDWID(IB-1))*ACDEN
          END IF
 80     CONTINUE 
C       shade contributed by overhanging canopy
        LOH= 0.1*HABS(1) - (DIS(1) + 2.)
        IF (LOH .GT. 0.) THEN 
          IF (LOH .GE. TWID) OVHSHD= TWID*DEN(1) 
          IF (LOH .LT. TWID) OVHSHD= LOH*DEN(1) 
C         shaded fraction of stream surface
        END IF
        FRCSHD= (VEGSHD + OVHSHD)/TWID
        IF (FRCSHD .GT. 1.) FRCSHD= 1.
C       direct-horizontal beam radiation after shaded 
 90     CONTINUE 
        RADBSP= (1. - FRCSHD)*HRADB
C       direct beam radiation absorded by water 
        RADBSP= RADBSP*(1. - ALDO) 
C
C       second component: diffuse radiation
 100    CONTINUE 
        RADDSP= 0.91*SKOP(ISP)*HRADD
C
C       total radiation: beam plus diffuse
        RADSHD= RADSHD + RADBSP + RADDSP
C
        RADBSP= 0.
        RADDSP= 0.
C       
C       close river sample point loop
 110  CONTINUE 
C
C     calculate mean flux of radiation for the whole RCHRES
      RADSHD= RADSHD/NSSP
C
C     compute solar factor and effective shading
C     HRADG= HRADB + HRADD  (not needed - input by user)
      IF (HRADG .LE. 0.) THEN
        SHDFAC = 1.0
      ELSE
        SHDFAC = 1. - RADSHD/HRADG
      END IF
C
      RETURN
      END
