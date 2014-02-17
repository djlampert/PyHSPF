C
C
C
      SUBROUTINE   PPIRRG
     I                    (MSGFL,NOPNS,OPNTAB)
C
C     + + + PURPOSE + + +
C     Initialize irrigation common block and OSV addresses.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MSGFL,NOPNS,OPNTAB(20,NOPNS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - unit number of message wdm file
C     NOPNS  - number of operations in run
C     OPNTAB - table of operation information
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cirrig.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,SCLU,SGRP,CLEN,INITFG,CONT,IPRADD,IPKADD,TOPNO,
     $             SOPNO,IRCHNO,IROSVK,MOSV,KEY,CHK,PREV,OFFSET,OMCODE,
     $             OSVKST
      CHARACTER*1  CHSTR1(80)
      CHARACTER*6  RCHTYP
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*80 CHSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC INT
C
C     + + + FUNCTIONS + + +
      INTEGER  OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL WMSGTT,OPNNO,GETOSV,PUTOSV
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,MOSV/1,500/
      DATA RCHTYP/'RCHRES'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (10I10)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 313
C
C     first read in key addresses
C
      SGRP= 1
      INITFG= 1
      CLEN= 70
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) IPRADD,IPKADD,IRVADD,IRMADD,IRWADD,IRDADD,IRSADD
C
C     now loop through all operations to set osv keys
C
      DO 10 TOPNO= 1, NOPNS
C
        OMCODE= OPNTAB(4,TOPNO)
        IF (OMCODE .EQ. 1) THEN
C         operation is a perlnd - process
C
C         fetch source reach number from perlnd osv
          CHK= INT ((IPRADD- 1)/500)+ 1
          OSVKST= OPNTAB(7,TOPNO)
          KEY= OSVKST+ CHK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
          OFFSET= IPRADD- 500*(CHK- 1)
          IRCHNO= IROSV(OFFSET)
C
          IF (IRCHNO .GE. 1) THEN
C           source reach is specified
            
C           check to see if source reach is active
            SOPNO= OPNNO (RCHTYP,IRCHNO,IRCHNO,NOPNS,OPNTAB,I1,NOPNS)
            IF (SOPNO .GE. 1) THEN
C             source reach is active
C
C             set perlnd osv key for source reach
              IROSVK= OPNTAB(7,SOPNO)
C
C             check to see if same osv chunk for osv key
              PREV= CHK
              CHK= INT ((IPKADD- 1)/500)+ 1
              IF (CHK .NE. PREV) THEN
C               osv key in different osv chunk - get it
                KEY= IROSVK+ CHK- 1
                CALL GETOSV (KEY,KEY,MOSV,
     O                       IROSV)
              END IF
C
C             now place key in perlnd osv and save
              OFFSET= IPKADD- 500*(CHK- 1)
              IROSV(OFFSET)= IROSVK
              CALL PUTOSV (KEY,KEY,MOSV,IROSV)
            END IF
          END IF
        END IF
10    CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIRRIG
C
C     + + + PURPOSE + + +
C     Calculate irrigation demand, actual application, and, as
c     necessary, withdraw it from the designated source.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE     'cplpw.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,LEV,ISRC,LEVFG,DONEFG
      REAL     R,SRCDEM(3),REMDEM,TOTFRC,RCHDEM,RSHORT,CRITEL,SUBDEM
      DOUBLE PRECISION RCHAVL,RCHWDL,AVAIL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL PIRAWD,PIRSKD,PIRGTR,PIRSTR,ZIPR
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate irrigation demand  
C
      IF (IRRGFG .EQ. 1) THEN
C       irrigation demand is input as a timeseries
        IRRDEM= IRRINP
      ELSE IF (IRRGFG .EQ. 2) THEN
C       irrigation demand is based on allowable water depletion
        CALL PIRAWD (SZONFG,VCRDFG,VAWDFG,DATIM,MESSU,MSGFL,NDAY,DAYFG,
     I               NXTMON,NDAYS,CRDEPM,IRAWDM,NCRP,CRPDAT,CRPSTG,
     I               CRPAWD,CRPRDP,SOILD,WILTP,WILTPT,FLDCAP,FDCAP,AWC,
     I               HWTFG,IROPFG,SURS,UZS,IFWS,LZS,GWEL,SELV,
     I               UELV,LELV,UPGW,PGW,PCW,AGWS,TGWS,ULGWS,LLGWS,IREFF,
     I               ARZI,
     M               CRDEP,IRAWD,
     O               RZWSC,RZWS,IRRDEM,SUBDEM)
      ELSE IF (IRRGFG .EQ. 3) THEN
C       irrigation demand is scheduled
        CALL PIRSKD (DATIM,DELT,NDAY,NSKED,IRDATE,IRDURA,IRRATE,
     O               IRRDEM)
      END IF
C
C     reset current withdrawal and shortfall
      I= 4
      R= 0.0
      CALL ZIPR (I,R,
     O           IRDRAW)
      I= 6
      CALL ZIPR (I,R,
     O           IRRAPP)
      IRSHRT= 0.0
      RCHDEM= 0.0
C
      IF (IRRDEM .GT. 0.0) THEN
C       draw from stated sources
C
C       determine water available from each source
C       available imports are considered infinite
        AVAIL(1)= 1.0E20
C       groundwater limited to active storage
        AVAIL(2)= TGWS
C
        IF (IROSVK .GE. 1) THEN
C         check reach to determine current volume and committed withdrawals
          CALL PIRGTR (IROSVK,
     O                 RCHAVL,RCHWDL,RCHDEM,RSHORT)
C         all uncommitted water above minimum is available
          IF ( (RCHAVL .GT. 0.0) .AND. (IRAFAC .GT. 0.0) ) THEN
C           some available water is not yet committed
            AVAIL(3)= RCHAVL/IRAFAC
          ELSE
C           no further uncommitted water available
            AVAIL(3)= 0.0
          END IF
        ELSE
C         no active reach specified
          AVAIL(3)= 0.0
        END IF
C
C       check each source at each level
        REMDEM= IRRDEM
        DONEFG= 0
        LEV= 0
 10     CONTINUE
          LEV= LEV+ 1
C
C         calculate initial demands for each source at this level
          DO 20 ISRC= 1, 3
            IF (IRRSRC(ISRC,LEV) .GT. 0.0) THEN
C             there is a demand for this source at this level
              SRCDEM(ISRC)= IRRSRC(ISRC,LEV)*REMDEM
            ELSE
C             no demand for this source at this level
              SRCDEM(ISRC)= 0.0
            END IF
 20       CONTINUE
C
C         now satisfy all demands that can be met at this level
          LEVFG= 0
 30       CONTINUE
C
C           get water from all specified sources at this level
            TOTFRC= 0.0
            DO 40 ISRC= 1, 3
              IF (SRCDEM(ISRC) .GT. 0.0) THEN
C               try to satisfy the demand for this source at this level
                IF (SRCDEM(ISRC) .LE. AVAIL(ISRC)) THEN
C                 can satisfy in full
                  IRDRAW(ISRC)= IRDRAW(ISRC)+ SRCDEM(ISRC)
                  REMDEM= REMDEM- SRCDEM(ISRC)
                  AVAIL(ISRC)= AVAIL(ISRC)- SRCDEM(ISRC)
                  TOTFRC= TOTFRC+ IRRSRC(ISRC,LEV)
                ELSE
C                 take all available
                  IF (ISRC .EQ. 3) THEN
C                   compute reach shortfall
                    RSHORT= RSHORT+ (SRCDEM(ISRC)- AVAIL(ISRC))*IRAFAC
                  END IF
                  IRDRAW(ISRC)= IRDRAW(ISRC)+ AVAIL(ISRC)
                  REMDEM= REMDEM- AVAIL(ISRC)
                  AVAIL(ISRC)= 0.0
                END IF
                IF (ISRC .EQ. 3) THEN
C                 set reach demand
                  RCHDEM= RCHDEM+ SRCDEM(ISRC)*IRAFAC
                END IF
              END IF
 40         CONTINUE
C
            IF (REMDEM .LT. 0.0001) THEN
C             demand fulfilled
              DONEFG= 1
              LEVFG= 1
            ELSE IF (TOTFRC .EQ. 0) THEN
C             no other sources with available water at this level
              LEVFG= 1
            ELSE
C             reset demands
              DO 50 ISRC= 1, 3
                IF (AVAIL(ISRC) .LE. 0.0) THEN
C                 no water remains for this source
                  SRCDEM(ISRC)= 0.0
                ELSE IF (IRRSRC(ISRC,LEV) .GT. 0.0) THEN
C                 get prorated fraction of remaining demand from source
                  SRCDEM(ISRC)= IRRSRC(ISRC,LEV)/TOTFRC*REMDEM
                END IF
 50           CONTINUE
            END IF
C
C         end loop on sources for this level
          IF (LEVFG .EQ. 0) GO TO 30
C
C       end loop on levels
        IF ( (LEV .LT. 3) .AND. (DONEFG .EQ. 0) ) GO TO 10
C
C       update internal storages
        IF (IRDRAW(2) .GT. 0.0) THEN
C         water was drawn from groundwater
          TGWS= TGWS- IRDRAW(2)
          AGWS= AGWS- IRDRAW(2)
          IF (AGWS .LT. 0.0) THEN
C           no more active storage
            AGWS= 0.0
          END IF
          GWVS= GWVS- IRDRAW(2)
          IF (GWVS .LT. 0.0) THEN
C           no more wedge storage
            GWVS= 0.0
          END IF
        END IF
        IF ( (RCHDEM .GT. 0.0) .AND. (IROSVK .GE. 1) ) THEN
C         water was demanded from an active rchres        
          RCHWDL= RCHWDL+ IRDRAW(3)*IRAFAC
          CALL PIRSTR (IROSVK,RCHWDL,RCHDEM,RSHORT)
        END IF
      END IF
C
C     final accounting
      IRRAPP(6)= IRDRAW(1)+ IRDRAW(2)+ IRDRAW(3)
      IF (IRRDEM .GT. IRRAPP(6)) THEN
C       there was a shortfall
        IRSHRT= IRRDEM- IRRAPP(6)
        IF (IRSHRT .LE. 0.001) THEN
C         close enough
          IRSHRT= 0.0
        END IF
      ELSE
C       all demand was satisfied
        IRSHRT= 0.0
      END IF
C
C     finally calculate actual application to each soil zone
C
      IF ( (HWTFG .EQ. 1) .AND. (IROPFG .EQ. 1) ) THEN
C       check that water table is high enough to make soil application
        CRITEL= SELV- CRDEP- CAPRIS
        IF (GWEL .LT. CRITEL) THEN
C         water table too low to use subirrigation - all water goes to gw
          IRRAPP(5)= IRRAPP(6)
        ELSE
C         apply gw directly, then use user fractions for remainder
          IF (SUBDEM .LT. IRRAPP(6)) THEN
C           there is enough available water to exceed subsurface demand
            IRRAPP(5)= SUBDEM
            DO 60 I= 1, 5
              IRRAPP(I)= IRRAPP(I)+ (IRRAPP(6)- SUBDEM)* IRRTGT(I)
 60         CONTINUE
          ELSE
C           not enough water available to reach critical elevation
C           all goes to gw
            IRRAPP(5)= IRRAPP(6)
          END IF
        END IF
      ELSE
C       apply user fractions normally
        DO 70 I= 1, 5
          IRRAPP(I)= IRRAPP(6)* IRRTGT(I)
 70     CONTINUE
      END IF
C         
      RETURN
      END
C
C
C
      SUBROUTINE   PIRSKD
     I                    (DATIM,DELT,NDAY,NSKED,IRDATE,IRDURA,IRRATE,
     O                     IRRDEM)
C
C     + + + PURPOSE + + +
C     Determine irrigation demand based on user-specified
C     schedule.  Annual events may not cross a year boundary.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER DATIM(5),NDAY(12),NSKED,IRDATE(5,NSKED),
     $        IRDURA(NSKED)
      REAL    DELT,IRRATE(NSKED),IRRDEM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATIM  - date and time of day
C     DELT   - time step of run in minutes
C     NDAY   - number of days in each month
C     NSKED  - number of scheduled irrigation applications
C     IRDATE - date of each scheduled application
C     IRDURA - duration of each application in minutes
C     IRRATE - application rate in inches/hr
C     IRRDEM - irrigation demand
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,N,LDATE(5),DMIN,IDELT
C
C     + + + INTRINSICS + + +
      INTRINSIC MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL COPYI,DIFTIM
C
C     + + + END SPECIFICATIONS + + +
C
      IRRDEM= 0.0
C
      DO 10 I= 1, NSKED
        N= 5
        CALL COPYI (N,IRDATE(1,I),
     O              LDATE)
C
        IF (LDATE(1) .EQ. 0) THEN
C         annual event
          LDATE(1)= DATIM(1)
        END IF
C
C       determine time difference to current date
        CALL DIFTIM (LDATE,DATIM,NDAY,
     O               DMIN)
        IDELT= DELT
        N= MAX (IDELT,IRDURA(I))
        IF ( (DMIN .GE. 0) .AND. (DMIN .LT. N) ) THEN
C         in middle of application
          IRRDEM= IRRDEM+ IRRATE(I)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIRGTR
     I                    (IROSVK,
     O                     RCHAVL,RCHWDL,RCHDEM,RSHORT)
C
C     + + + PURPOSE + + +
C     Retrieve storage and irrigation withdrawal commitments from
C     RCHRES OSV.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IROSVK
      REAL    RCHDEM,RSHORT
      DOUBLE PRECISION RCHAVL,RCHWDL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IROSVK - OSV file key for source RCHRES
C     RCHAVL - volume in RCHRES available for irrigation withdrawal
C     RCHWDL - volume of water that RCHRES has committed for
C              withdrawals so far this interval
C     RCHDEM - volume of water that has been demanded from RCHRES so
C              far this interval
C     RSHORT - volume of shortage of irrigation withdrawals so far this
C              interval
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cirrig.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MOSV,KEY,CHUNK,PREV,OFFSET
      DOUBLE PRECISION RCHVOL,RCHMIN
C
C     + + + EXTERNALS + + +
      EXTERNAL GETOSV
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MOSV/500/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IROSVK .GT. 0) THEN
C       source reach is active - check for supply and previous commitments
C
C       supply
        CHUNK= INT ((IRVADD- 1)/500)+ 1
        KEY= IROSVK+ CHUNK- 1
        CALL GETOSV (KEY,KEY,MOSV,
     O               IROSV)
        OFFSET= IRVADD- 500*(CHUNK- 1)
        OFFSET= (OFFSET- 1)/2+ 1
        RCHVOL= IROSVD(OFFSET)
C
C       minimum volume that cannot be withdrawn for irrigation
        PREV= CHUNK
        CHUNK= INT ((IRMADD- 1)/500)+ 1
        IF (CHUNK .NE. PREV) THEN
C         min volume in different osv chunk - get it
          KEY= IROSVK+ CHUNK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
        END IF
        OFFSET= IRMADD- 500*(CHUNK- 1)
        OFFSET= (OFFSET- 1)/2+ 1
        RCHMIN= IROSVD(OFFSET)
C
C       committed withdrawal so far
        PREV= CHUNK
        CHUNK= INT ((IRWADD- 1)/500)+ 1
        IF (CHUNK .NE. PREV) THEN
C         withdrawal in different osv chunk - get it
          KEY= IROSVK+ CHUNK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
        END IF
        OFFSET= IRWADD- 500*(CHUNK- 1)
        OFFSET= (OFFSET- 1)/2+ 1
        RCHWDL= IROSVD(OFFSET)
C
C       determine total available volume
        RCHAVL= RCHVOL- RCHMIN- RCHWDL
        IF (RCHAVL .LT. 0.0) THEN
C         none available
          RCHAVL= 0.0
        END IF
C
C       total demand so far
        PREV= CHUNK
        CHUNK= INT ((IRDADD- 1)/500)+ 1
        IF (CHUNK .NE. PREV) THEN
C         total demand in different osv chunk - get it
          KEY= IROSVK+ CHUNK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
        END IF
        OFFSET= IRDADD- 500*(CHUNK- 1)
        RCHDEM= IROSVR(OFFSET)
C
C       shortage so far
        PREV= CHUNK
        CHUNK= INT ((IRSADD- 1)/500)+ 1
        IF (CHUNK .NE. PREV) THEN
C         shortage in different osv chunk - get it
          KEY= IROSVK+ CHUNK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
        END IF
        OFFSET= IRSADD- 500*(CHUNK- 1)
        RSHORT= IROSVR(OFFSET)
      ELSE
C       rchres not active - no supply
        RCHAVL= 0.0
        RCHWDL= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIRSTR
     I                    (IROSVK,RCHWDL,RCHDEM,RSHORT)
C
C     + + + PURPOSE + + +
C     Set new value of water committed for withdrawals in RCHRES OSV.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IROSVK
      REAL    RCHDEM,RSHORT
      DOUBLE PRECISION RCHWDL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IROSVK - OSV file key for source RCHRES
C     RCHWDL - volume of water that RCHRES has committed for
C              withdrawals so far this interval
C     RCHDEM - volume of water that has been demanded from RCHRES so
C              far this interval
C     RSHORT - volume of shortage of irrigation withdrawals so far this
C              interval
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cirrig.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MOSV,KEY,CHUNK,PREV,OFFSET
C
C     + + + EXTERNALS + + +
      EXTERNAL PUTOSV
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MOSV/500/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IROSVK .GT. 0) THEN
C       source reach is active
C
C       set new value of cumulative withdrawal in rchres osv
        CHUNK= INT ((IRWADD- 1)/500)+ 1
        KEY= IROSVK+ CHUNK- 1
        CALL GETOSV (KEY,KEY,MOSV,
     O               IROSV)
        OFFSET= IRWADD- 500*(CHUNK- 1)
        OFFSET= (OFFSET- 1)/2+ 1
        IROSVD(OFFSET)= RCHWDL
C 
C       set new value of cumulative demand in rchres osv
        PREV= CHUNK
        CHUNK= INT ((IRDADD- 1)/500)+ 1
        IF (CHUNK .NE. PREV) THEN
C         write old chunk and fetch new chunk
          CALL PUTOSV (KEY,KEY,MOSV,IROSV)
          KEY= IROSVK+ CHUNK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
        END IF
        OFFSET= IRDADD- 500*(CHUNK- 1)
        IROSVR(OFFSET)= RCHDEM
C 
C       set new value of cumulative shortage in rchres osv
        PREV= CHUNK
        CHUNK= INT ((IRSADD- 1)/500)+ 1
        IF (CHUNK .NE. PREV) THEN
C         write old chunk and fetch new chunk
          CALL PUTOSV (KEY,KEY,MOSV,IROSV)
          KEY= IROSVK+ CHUNK- 1
          CALL GETOSV (KEY,KEY,MOSV,
     O                 IROSV)
        END IF
        OFFSET= IRSADD- 500*(CHUNK- 1)
        IROSVR(OFFSET)= RSHORT
        CALL PUTOSV (KEY,KEY,MOSV,IROSV)
      ELSE
C       error - program bug - source rchres not active
        WRITE (*,*) 'BUG IN OSV KEY FOR RCHRES IRRIGATION WITHDRAWAL'
        STOP
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIRSTF
     I                    (IRFLAG)
C
C     + + + PURPOSE + + +
C     Set global irrigation flag
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IRFLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IROSVK - global irrigation flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cirrig.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IRRWFG= IRFLAG
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIRGTF
     I                    (IRFLAG)
C
C     + + + PURPOSE + + +
C     Set global irrigation flag
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IRFLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IROSVK - global irrigation flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cirrig.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IRFLAG= IRRWFG
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIRAWD
     I                    (SZONFG,VCRDFG,VAWDFG,DATIM,MESSU,MSGFL,NDAY,
     I                     DAYFG,NXTMON,NDAYS,CRDEPM,IRAWDM,NCRP,
     I                     CRPDAT,CRPSTG,CRPAWD,CRPRDP,SOILD,WILTP,
     I                     WILTPT,FLDCAP,FDCAP,AWC,HWTFG,IROPFG,SURS,
     I                     UZS,IFWS,LZS,GWEL,SELV,UELV,LELV,UPGW,PGW,
     I                     PCW,AGWS,TGWS,ULGWS,LLGWS,IREFF,ARZI,
     M                     CRDEP,IRAWD,
     O                     RZWSC,RZWS,IRRDEM,SUBDEM)
C
C     + + + PURPOSE + + +
C     Determine irrigation demand based on a crop demand algorithm based
C     on the AFSIRS model.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SZONFG,VCRDFG,VAWDFG,DATIM(5),MESSU,MSGFL,NDAY(12),DAYFG,
     $        NXTMON,NDAYS,NCRP,CRPDAT(4,NCRP),HWTFG,IROPFG
      REAL    CRDEPM(12),IRAWDM(12),CRPSTG(4,NCRP),CRPAWD(4,NCRP),
     $        CRPRDP(2,NCRP),SOILD(4),WILTP,WILTPT(4),FLDCAP,FDCAP(4),
     $        AWC(4),SURS,UZS,IFWS,LZS,GWEL,SELV,UELV,LELV,UPGW,PGW,
     $        PCW,AGWS,TGWS,ULGWS,LLGWS,IREFF,ARZI,CRDEP,IRAWD,RZWSC,
     $        RZWS,IRRDEM,SUBDEM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SZONFG - flag indicating whether soil parameters vary by soil zone
C     VCRDFG - flag indicating if crop root depth varies monthly or seasonally
C     VAWDFG - flag indicating if allowable water depletion varies monthly
C              or seasonally
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - unit number of message wdm file
C     NDAY   - number of days in each month
C     DAYFG  - flag for first day or day change
C     NXTMON - next calendar month
C     NDAYS  - no. of days in this month
C     CRDEPM - monthly crop root depth
C     IRAWDM - monthly allowable water depletion
C     NCRP   - number of crops per year
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPSTG - fractions of season comprising each stage
C     CRPAWD - allowable water depletion for each crop stage
C     CRPRDP - crop root depth for beginning and end of each season
C     SOILD  - depth of soil layers
C     WILTP  - wilting point for all soil layers (depth/depth)
C     WILTPT - wilting point for each soil layer (depth)
C     FLDCAP - field capacity for all soil layers
C     FDCAP  - field capacity for each soil layer
C     AWC    - available water capacity for each soil layer
C     HWTFG  - flag indicating whether high water table algorithms used
C     IROPFG - flag indication irrigation type: 1=subsurface, 0=normal
C     SURS   - surface storage
C     UZS    - upper zone storage
C     IFWS   - interflow storage
C     LZS    - lower zone storage
C     GWEL   - groundwater elevation
C     SELV   - surface elevation
C     UELV   - upper influence elevation
C     LELV   - lower influence elevation
C     UPGW   - upper layer porosity for gravity water
C     PGW    - lower layer porosity for gravity water
C     PCW    - porosity for cohesion water
C     AGWS   - active gw storage
C     TGWS   - total gw storage above datum
C     ULGWS  - active gw storage when water is at uelv
C     LLGWS  - active gw storage when water is at lelv
C     IREFF  - irrigation efficiency
C     ARZI   - irrigation areal fraction
C     CRDEP  - crop root depth
C     IRAWD  - allowable water depletion
C     RZWSC  - root zone water storage capacity
C     RZWS   - root zone water storage
C     IRRDEM - irrigation demand
C     SUBDEM - for subirrigation, application needed to reach critical
C              elevation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  MON,DAY,ICROP,LAY,BOTLAY,SEASTG,SGFLAG
      REAL     STGFRC,CUMDEP,BOTFRC,AFDCAP,AWILTP,MINWS,NIR,SUBELV,
     $         CRDELV,FRAC,CRGWS
C
C     + + + FUNCTIONS + + +
      REAL     DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL CRPSEL,STGSEL,DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      MON= DATIM(2)
      DAY= DATIM(3)
C
      SUBDEM= 0.0
      SGFLAG= 0
C
C     determine if currently within an irrigation season
      CALL CRPSEL (MON,DAY,CRPDAT,NCRP,
     O             ICROP)
C
C     update monthly and seasonal values
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
C
        IF ((VCRDFG .EQ. 2) .OR. (VAWDFG .EQ. 2)) THEN
C         get current values of crop root depth and allowable water depletion
C
          IF (ICROP .GE. 1) THEN
C           determine stage and fraction of current season
            CALL STGSEL (NCRP,ICROP,DATIM,MESSU,MSGFL,NDAY,CRPDAT,
     I                   CRPSTG,
     O                   SEASTG,STGFRC)
          END IF
        END IF
C
        IF (VCRDFG .EQ. 1) THEN
C         monthly crop root depth
          CRDEP= DAYVAL (CRDEPM(MON),CRDEPM(NXTMON),DAY,NDAYS)
        ELSE IF (VCRDFG .EQ. 2) THEN
C         seasonal crop root depth
          IF (ICROP .EQ. 0) THEN
C           not in crop season
            CRDEP= 0.0
          ELSE
C           get crop stage value
            IF (SEASTG .EQ. 1) THEN
C             first stage
              CRDEP= CRPRDP(1,ICROP)
            ELSE IF (SEASTG .EQ. 2) THEN
C             second stage
              CRDEP= CRPRDP(1,ICROP)+ STGFRC*
     $                               (CRPRDP(2,ICROP)-CRPRDP(1,ICROP))
            ELSE
C             third or fourth stage
              CRDEP= CRPRDP(2,ICROP)
            END IF
          END IF
        END IF
C
        IF (VAWDFG .EQ. 1) THEN
C         monthly allowable water depletion
          IRAWD= DAYVAL (IRAWDM(MON),IRAWDM(NXTMON),DAY,NDAYS)
        ELSE IF (VAWDFG .EQ. 2) THEN
C         seasonal allowable water depletion
          IF (ICROP .EQ. 0) THEN
C           not in crop season
            IRAWD= 0.0
          ELSE
C           get crop stage value
            IRAWD= CRPAWD(SEASTG,ICROP)
          END IF
        END IF
      END IF
C
      IF (ICROP .EQ. 0) THEN
C       no active crop
        IRRDEM= 0.0
      ELSE
C       must compute crop demand
C
C       determine deepest soil layer reached by root zone
        CUMDEP= 0.0
        LAY= 0
        BOTLAY= 0
 10     CONTINUE
          LAY= LAY+ 1
          CUMDEP= CUMDEP+ SOILD(LAY)
          IF (CRDEP .LE. CUMDEP) THEN
C           found bottom layer
            BOTLAY= LAY
            BOTFRC= (CRDEP- CUMDEP+ SOILD(LAY))/SOILD(LAY)
          END IF
        IF ( (BOTLAY .EQ. 0) .AND. (LAY .LT. 4) ) GO TO 10
C
        IF (BOTLAY .EQ. 0) THEN
C         root zone extends beyond bottom of groundwater zone
          BOTLAY= 4
          BOTFRC= 1.0
        END IF
C
C       compute root zone water storage capacity and aggregate field capacity
C
        IF (SZONFG .EQ. 0) THEN
C         soil parameters same for all soil layers
          AFDCAP= FLDCAP*CRDEP
          AWILTP= WILTP*CRDEP
          RZWSC= AWC(1)*CRDEP+ AWILTP
        ELSE
C         soil parameters vary by soil layer
          AFDCAP= 0.0
          AWILTP= 0.0
          RZWSC= 0.0
          LAY= 0
 20       CONTINUE
            LAY= LAY+ 1
            IF (LAY .LT. BOTLAY) THEN
C             count full layer depth
              AFDCAP= AFDCAP+ FDCAP(LAY)*SOILD(LAY)
              AWILTP= AWILTP+ WILTPT(LAY)
              RZWSC= RZWSC+ AWC(LAY)*SOILD(LAY)+ WILTPT(LAY)
            ELSE
C             only count part of layer in root zone
              AFDCAP= AFDCAP+ BOTFRC*FDCAP(LAY)*SOILD(LAY)
              AWILTP= AWILTP+ BOTFRC*WILTPT(LAY)
              RZWSC= RZWSC+ BOTFRC*AWC(LAY)*SOILD(LAY)+
     $                      BOTFRC*WILTPT(LAY)
            END IF
          IF (LAY .LT. BOTLAY) GO TO 20
        END IF
C
C       compute current root zone water storage
        IF (HWTFG .EQ. 0) THEN
C         normal hydrology - use layer depths
          IF (BOTLAY .EQ. 1) THEN
C           surface layer only - assume all surface storage in root zone
            RZWS= SURS
          ELSE IF (BOTLAY .EQ. 2) THEN
C           root zone extends into upper zone
            RZWS= SURS+ BOTFRC*(UZS+ IFWS)
          ELSE IF (BOTLAY .EQ. 3) THEN
C           root zone extends into lower zone
            RZWS= SURS+ UZS+ IFWS+ BOTFRC*LZS
          ELSE
C           root zone extends into groundwater zone
            RZWS= SURS+ UZS+ IFWS+ LZS
            IF (BOTFRC .GE. 1.0) THEN
C             entire active groundwater zone within root zone
              RZWS= RZWS+ AGWS
            END IF
          END IF
        ELSE
C         high water table on - look at elevations
          CRDELV= SELV- CRDEP
          RZWS= SURS
          IF (CRDELV .GT. UELV) THEN
C           only part of upper and interflow storage available
            IF (GWEL .GT. UELV) THEN
C             upper zone (and assume interflow) storage all above gw
              IF (GWEL .GE. SELV) THEN
C               dummy fraction
                FRAC= 1.0
              ELSE
C               compute fraction
                FRAC= CRDEP/(SELV- GWEL)
              END IF
              IF (FRAC .GT. 1.0) THEN
C               gw reaches root zone - all available
                FRAC= 1.0
              END IF
            ELSE
C             upper zone (and assume interflow) storage fully distributed
              FRAC= CRDEP/(SELV- UELV)
            END IF
            RZWS= RZWS+ (UZS+ IFWS)*FRAC
          ELSE
C           all of upper and interflow storage available
            RZWS= RZWS+ UZS+ IFWS
          END IF
          IF (CRDELV .GT. LELV) THEN
C           only part of lower storage available
            IF (GWEL .GT. LELV) THEN
C             lower zone storage all above gw
              IF (GWEL .GE. SELV) THEN
C               dummy fraction
                FRAC= 1.0
              ELSE
C               compute fraction
                FRAC= CRDEP/(SELV- GWEL)
              END IF
              IF (FRAC .GT. 1.0) THEN
C               gw reaches root zone - all available
                FRAC= 1.0
              END IF
            ELSE
C             lower zone storage fully distributed
              FRAC= CRDEP/(SELV- LELV)
            END IF
            RZWS= RZWS+ LZS*FRAC
          ELSE
C           all of lower storage available
            RZWS= RZWS+ LZS
          END IF
          IF (GWEL .GT. CRDELV) THEN
C           groundwater reaches root zone
C
C           no basic irrigation if groundwater reaches root zone
c@@@            SGFLAG= 1
C
C           compute water required to reach root zone
            IF (CRDELV .GE. UELV) THEN
              CRGWS= ULGWS- IFWS- UZS+ (CRDELV- UELV)*UPGW
            ELSE IF (CRDELV .GE. LELV) THEN
              CRGWS= LLGWS+ (CRDELV- LELV)*PCW
            ELSE
              CRGWS= CRDELV*(PCW+ PGW)
            END IF
C
C           add water to root zone storage
            RZWS= RZWS+ (TGWS- CRGWS)
          END IF
        END IF
C
C       adjust for areal fraction
        RZWS= RZWS*ARZI
        RZWSC= RZWSC*ARZI
        AWILTP= AWILTP*ARZI
        AFDCAP= AFDCAP*ARZI
C
C       compute the minimum water storage above which no irrigation
C       is needed
        MINWS= (RZWSC- AWILTP)*(1.0- IRAWD)+ AWILTP
C      write (99,*) 'RZWSC AWILTP AFDCAP ARZI MINWS',
C     $              RZWSC,AWILTP,AFDCAP,ARZI,MINWS
C
        IF (SURS .GT. 0.0) THEN
C         no basic irrigation if water standing on surface
c@@@          SGFLAG= 1
        END IF
C
C       determine basic net irrigation demand
        IF ( (SGFLAG .EQ. 1) .OR. (RZWS .GE. MINWS) ) THEN
C         no basic irrigation occurs
          NIR= 0.0
        ELSE
C         compute basic net demand
          NIR= AFDCAP- RZWS
        END IF
C
        IF ( (HWTFG .EQ. 1) .AND. (IROPFG .EQ. 1) ) THEN
C         check elevation for subsurface irrigation
          SUBELV= SELV- CRDEP
          IF (GWEL .LT. SUBELV) THEN
C           gw below root zone - add water to make gw rise
            IF (SUBELV .LT. LELV) THEN
C             entire rise takes place in groundwater region
              SUBDEM= (SUBELV- GWEL)*(PCW+ PGW)
            ELSE IF (SUBELV .LT. UELV) THEN
C             rise extends into lower region
              IF (GWEL .LT. LELV) THEN
C               rise begins in groundwater region
                SUBDEM= (LELV-GWEL)*(PCW+ PGW)+ (SUBELV-LELV)*PGW
              ELSE
C               entire rise takes place in lower region
                SUBDEM= (SUBELV- GWEL)*PGW
              END IF
            ELSE
C             rise extends into upper region
              IF (GWEL .LT. LELV) THEN
C               rise begins in groundwater region
                SUBDEM= (LELV-GWEL)*(PCW+ PGW)+ (UELV-LELV)*PGW+
     $                  (SUBELV-UELV)*UPGW
              ELSE IF (GWEL .LT. UELV) THEN
C               rise begins in lower region
                SUBDEM= (UELV-GWEL)*PGW+ (SUBELV-UELV)*UPGW
              ELSE
C               entire rise takes place in upper region
                SUBDEM= (SUBELV- GWEL)*UPGW
              END IF
            END IF
          END IF
        END IF
C
C       finally compute gross demand
        IRRDEM= NIR/IREFF+ SUBDEM
      END IF
C
cccc      I0= 0
cccc      R1= 4.0*UZSN+ 2.5*LZSN
cccc      R2= R1- RZWSC
cccc      R3= UZS+ LZS- RZWS
cccc      write (99,3000) (DATIM(I),I=1,4),I0,RZWS,RZWSC,MINWS
cccc 3000 FORMAT (6X,I4,4I3,3(2X,G12.5))
C
      RETURN
      END
C
C
C
      SUBROUTINE   STGSEL
     I                    (NCRP,ICROP,DATIM,MESSU,MSGFL,NDAY,CRPDAT,
     I                     CRPSTG,
     O                     SEASTG,STGFRC)
C
C     + + + PURPOSE + + +
C     Determine which stage of the current crop season that the current
C     date falls, as well as what fraction of that stage has passed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NCRP,ICROP,DATIM(5),MESSU,MSGFL,NDAY(12),CRPDAT(4,NCRP),
     $        SEASTG
      REAL    CRPSTG(4,NCRP),STGFRC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCRP   - number of crops per year
C     ICROP  - index of current crop; or zero if none is current
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - unit number of message wdm file
C     NDAY   - number of days in each month
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPSTG - fractions of season comprising each stage
C     SEASTG - stage of current crop season
C     STGFRC - fraction of current stage that has passed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PMON,PDAY,HMON,HDAY,YR,MON,DAY,PDATIM(5),HDATIM(5),
     $          EKNT,NOWDIF,TOTDIF,STG
      REAL      SEAFRC
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL  STDATE,ENDATE,DIFTIM
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize dummy error count
      EKNT= 0
C
      PMON= CRPDAT(1,ICROP)
      PDAY= CRPDAT(2,ICROP)
      HMON= CRPDAT(3,ICROP)
      HDAY= CRPDAT(4,ICROP)
C
      YR= DATIM(1)
      MON= DATIM(2)
      DAY= DATIM(3)
C
      IF ( (PMON .LT. HMON) .OR.
     $     ( (PMON .EQ. HMON) .AND.
     $       (PDAY .LT. HDAY) ) ) THEN
C       current season does not cross year boundary
C       planting and harvest both this calendar year
        PDATIM(1)= YR
        HDATIM(1)= YR
      ELSE
C       current season crosses year boundary
        IF ( (MON .GT. PMON) .OR.
     $       ( (MON .EQ. PMON) .AND.
     $         (DAY .GE. PDAY) ) ) THEN
C         near end of year after plant date
          PDATIM(1)= YR
          HDATIM(1)= YR+ 1
        ELSE
C         near beginning of year before harvest date
          PDATIM(1)= YR- 1
          HDATIM(1)= YR
        END IF
      END IF
C
C     fill in rest of planting and harvest dates
      PDATIM(2)= PMON
      PDATIM(3)= PDAY
      PDATIM(4)= 0
      PDATIM(5)= 0
      HDATIM(2)= HMON
      HDATIM(3)= HDAY
      HDATIM(4)= 0
      HDATIM(5)= 0
C
C     convert season start and end dates to internal format
      CALL STDATE (NDAY,MESSU,MSGFL,
     M             EKNT,PDATIM)
      CALL ENDATE (NDAY,MESSU,MSGFL,
     M             EKNT,HDATIM)
C
C     determine time since start of season to now and to end
      CALL DIFTIM (PDATIM,DATIM,NDAY,
     O             NOWDIF)
      CALL DIFTIM (PDATIM,HDATIM,NDAY,
     O             TOTDIF)
C
C     now find fraction of season, stage, and fraction of stage
      SEAFRC= FLOAT (NOWDIF)/FLOAT (TOTDIF)
      STG= 0
      SEASTG= 0
 10   CONTINUE
        STG= STG+ 1
        IF (SEAFRC .LE. CRPSTG(STG,ICROP)) THEN
C         this is current stage
          SEASTG= STG
          STGFRC= SEAFRC/CRPSTG(STG,ICROP)
        ELSE
C         not yet current stage
          SEAFRC= SEAFRC- CRPSTG(STG,ICROP)
        END IF
      IF ( (STG .LT. 4) .AND. (SEASTG .EQ. 0) ) GO TO 10
C
      RETURN
      END

