C
C     3.6
C
      SUBROUTINE OSUP
     I                (SDATIM,EDATIM,RUNMIN,RUNWID)
C
C     + + + PURPOSE + + +
C     Write the operations supervisor instruction file - osupfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    EDATIM(5),RUNMIN,SDATIM(5),RUNWID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     RUNMIN - ???
C     RUNWID - ???
C
C     + + + COMMON BLOCKS- INTERP1 + + +
      INCLUDE    'crin1.inc'
      INCLUDE    'cosupm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DELT,DUMOS1(7),DUMOS2(2),DUMOS3(3),
     $           GRP,GRPND,GRPST,IDUMMY,INSPAN,LSPAN,NX,OKEY,
     $           OMCODE,OPND,OPST,OPTNO,OSVKND,OSVKST,
     $           REPEAT,REPLST,TSGKND,TSGKST,TSPKND,TSPKST,
     $           WID,WIDTH,WLAST,XDELT,XREPET,XSPAN,XWIDTH,
     $           EXUPFG,EXTWID
C
C     + + + EXTERNALS + + +
      EXTERNAL   HSPF_INI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA       DUMOS1,DUMOS2,DUMOS3/12*0/
C
C     + + + HISTORY + + +
C     2002  THJ allowed external control and interaction for IHM
C     2005  PBD enhanced external control capability to allow external width > width
C
C     + + + END SPECIFICATIONS + + +
C
      OKEY  = 1
C
C     general run information
      OSUPM(1,OKEY)= NXGRPS
      DO 10 I= 1, 5
        OSUPM(I+1,OKEY)= SDATIM(I)
        OSUPM(I+6,OKEY)= EDATIM(I)
 10   CONTINUE
C
C     exgroup loop
      DO 70 NX= 1,NXGRPS
C       because expad is not implemented in this release of hspf,
C       we assign dummy values to its properties.
        XREPET= 1
        XWIDTH= 0
        XDELT = 0
        XSPAN = RUNMIN
        GRPST = EXGTAB(1,NX)
        GRPND = EXGTAB(2,NX)
        NGRPS = GRPND- GRPST+ 1
        OKEY  = OKEY+ 1
        OSUPM(1,OKEY)= XREPET
        OSUPM(2,OKEY)= XWIDTH
        OSUPM(3,OKEY)= XDELT
        OSUPM(4,OKEY)= NGRPS
        DO 20 I= 1, 7
          OSUPM(I+4,OKEY)= DUMOS1(I)
 20     CONTINUE
C
C       ingroup loop
        DO 60 GRP= GRPST,GRPND
          DELT  = GRPTAB(3,GRP)
          WID   = GRPTAB(5,GRP)
          OPST  = GRPTAB(1,GRP)
          OPND  = GRPTAB(2,GRP)
          NOPNS = OPND- OPST+ 1
C
C         initialize width restriction due to external operations
          CALL HSPF_INI (DELT,OPST,OPND,OPNTAB,
     O                   EXUPFG,EXTWID)
          IF (EXTWID .EQ. 0) THEN
C           no external restriction
            WIDTH = WID
          ELSE
C           change inspan to external restriction
            IF (EXTWID .LT. WID) THEN
              WIDTH = EXTWID
            ELSE
C             don't use an external restriction
              WIDTH = WID
            END IF
          END IF
          INSPAN= DELT*WIDTH
C
C         find out how many inspans there are in an exspan
          IDUMMY= XSPAN/INSPAN
          LSPAN = XSPAN- (IDUMMY*INSPAN)
          IF (LSPAN .EQ. 0) THEN
C           last inspan is complete
            WLAST = WIDTH
            REPEAT= IDUMMY
          ELSE
C           last inspan is incomplete
            WLAST = LSPAN/DELT
            REPEAT= IDUMMY+ 1
          END IF
C
          REPLST= REPEAT
          OKEY  = OKEY+ 1
          OSUPM(1,OKEY)= REPEAT
          OSUPM(2,OKEY)= REPLST
          OSUPM(3,OKEY)= WIDTH
          OSUPM(4,OKEY)= WLAST
          OSUPM(5,OKEY)= DELT
          OSUPM(6,OKEY)= NOPNS
          OSUPM(7,OKEY)= RUNWID
          OSUPM(8,OKEY)= EXUPFG
          OSUPM(9,OKEY)= EXTWID
          DO 30 I= 1, 2
            OSUPM(I+9,OKEY)= DUMOS2(I)
 30       CONTINUE
C
C         operation loop
          DO 50 OPNO= OPST,OPND
            OMCODE= OPNTAB(4,OPNO)
            OPTNO = OPNTAB(3,OPNO)
            TSGKST= OPNTAB(17,OPNO)
            TSGKND= OPNTAB(18,OPNO)
            TSPKST= OPNTAB(19,OPNO)
            TSPKND= OPNTAB(20,OPNO)
            OSVKST= OPNTAB(7,OPNO)
            OSVKND= OPNTAB(8,OPNO)
C
            OKEY  = OKEY+ 1
            OSUPM(1,OKEY)= OMCODE
            OSUPM(2,OKEY)= OPTNO
            OSUPM(3,OKEY)= TSGKST
            OSUPM(4,OKEY)= TSGKND
            OSUPM(5,OKEY)= TSPKST
            OSUPM(6,OKEY)= TSPKND
            OSUPM(7,OKEY)= OSVKST
            OSUPM(8,OKEY)= OSVKND
            DO 40 I= 1, 3
              OSUPM(I+8,OKEY)= DUMOS3(I)
 40         CONTINUE
C
 50       CONTINUE
C
 60     CONTINUE
C
 70   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTOSU
     I                   (KEY,OSUBF1,OSUBF2,OSUBF3,OSUBF4,OSUBF5,OSUBF6,
     I                    OSUBF7,OSUBF8,OSUBF9,OSUB10,OSUB11)
C
C     + + + PURPOSE + + +
C     Write an OSUP line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KEY,OSUBF1,OSUBF2,OSUBF3,OSUBF4,OSUBF5,OSUBF6,
     1             OSUBF7,OSUBF8,OSUBF9,OSUB10,OSUB11
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEY    - record number
C     OSUBF1 - ???
C     OSUBF2 - ???
C     OSUBF3 - ???
C     OSUBF4 - ???
C     OSUBF5 - ???
C     OSUBF6 - ???
C     OSUBF7 - ???
C     OSUBF8 - ???
C     OSUBF9 - ???
C     OSUB10 - ???
C     OSUB11 - ???
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'cosupm.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      OSUPM(1,KEY) = OSUBF1
      OSUPM(2,KEY) = OSUBF2
      OSUPM(3,KEY) = OSUBF3
      OSUPM(4,KEY) = OSUBF4
      OSUPM(5,KEY) = OSUBF5
      OSUPM(6,KEY) = OSUBF6
      OSUPM(7,KEY) = OSUBF7
      OSUPM(8,KEY) = OSUBF8
      OSUPM(9,KEY) = OSUBF9
      OSUPM(10,KEY) = OSUB10
      OSUPM(11,KEY) = OSUB11
C
      RETURN
      END
