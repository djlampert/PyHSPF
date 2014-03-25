C
C
C
      SUBROUTINE   ALLOC
     I                  (PGPKEY,GRPST,GRPND,RUNWID)
C
C     + + + PURPOSE + + +
C     Allocate rows in the inpad and generate tsget/tsput instructions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GRPND,GRPST,PGPKEY,RUNWID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PGPKEY - ???
C     GRPST  - ???
C     GRPND  - ???
C     RUNWID - ???
C
C     + + + COMMON BLOCKS- INTERP3 + + +
      INCLUDE    'crin3.inc'
      INCLUDE    'crin3c.inc'
      INCLUDE    'cmdum.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MSGFL,GRP,I0,I1,J,MAXROW,MESSU,MROW,OPND,
     $          OPST,OSVREC,OSVSZ,PADVO,ROWLEN,TSGKST,
     $          TSPKST,WIDTH,WKND,WKST
C
C     + + + INTRINSICS + + +
      INTRINSIC  MAX0
C
C     + + + EXTERNALS + + +
      EXTERNAL  TARGET,RELESE,SOURCE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ALLOCATING INPAD ROWS AND GENERATING PRIMITIVE ',
     $          'TSGET/TSPUT INSTRUCTIONS')
 2010 FORMAT (/,' INGROUP NO.',I3,'    OSVSZ=',I5,'    INPAD WIDTH=',
     $          I6,' INTERVALS','    MAXROW=',I5,'  RUNWID=',I5)
 2020 FORMAT (/,' FINISHED ALLOCATING INPAD ROWS AND GENERATING ',
     $          'TSGET/TSPUT INSTRUCTIONS')
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
C
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      IF (OUTLEV .GT. 4) THEN
C       allocating message
        WRITE (MESSU,2000)
      END IF
C
C     maximum possible no. of rows in inpad
      MROW= MXROW
C
C     ingrp loop
      DO 30 GRP= GRPST,GRPND
C       initialize
        DO 10 J= 1,MROW
          AVFG(J) = 1
          RELFG(J)= 0
 10     CONTINUE
        MAXROW= 0
C
C       initialize osv size to space taken by tsget/tsput
C       instructions - 1000 r4 words
        OSVSZ= 1000
C
C       operation loop
        OPST= GRPTAB(1,GRP)
        OPND= GRPTAB(2,GRP)
C
        DO 20 OPNO= OPST,OPND
C         tsgkst is key at which first primitive tsget instruction
C         will be written, if there are any
          TSGKST= PGPKEY+ 1
C
C         consider this operation as a target in network block
          WKST  = OPNTAB(13,OPNO)
          WKND  = OPNTAB(14,OPNO)
C
          CALL TARGET (I0,WKST,WKND,MROW,
     I                 MESSU,MSGFL,EXTKW1,RUNWID,
     M                 PGPKEY,AVFG,ECOUNT,MAXROW,
     O                 RELFG)
C
C         consider operation as a target in ext sources block
          WKST= OPNTAB(9,OPNO)
          WKND= OPNTAB(10,OPNO)
C
          CALL TARGET (I1,WKST,WKND,MROW,
     I                 MESSU,MSGFL,EXTKW1,RUNWID,
     M                 PGPKEY,AVFG,ECOUNT,MAXROW,
     O                 RELFG)
C
          IF ( (RUNWID .EQ. 0) .AND. (MAXROW .GT. 0) ) THEN
C           release rows occupied by inputs to this operation,
C           so they can be reused by outputs, as long as no
C           conditional special actions prevent release
            CALL RELESE (MAXROW,
     M                   RELFG,
     O                   AVFG)
          END IF
C
          IF (PGPKEY .GE. TSGKST) THEN
C           some primitive instructions were written
            OPNTAB(17,OPNO)= TSGKST
            OPNTAB(18,OPNO)= PGPKEY
          END IF
C
C         consider this operation as a source in the ext targets block
          TSPKST= PGPKEY+ 1
          WKST  = OPNTAB(15,OPNO)
          WKND  = OPNTAB(16,OPNO)
C
          CALL SOURCE (I1,WKST,WKND,MROW,
     I                 MESSU,MSGFL,EXTKW1,
     M                 PGPKEY,AVFG,ECOUNT,MAXROW,
     O                 RELFG)
C
C         consider this operation as a source in the network block
          WKST= OPNTAB(11,OPNO)
          WKND= OPNTAB(12,OPNO)
C
          CALL SOURCE (I0,WKST,WKND,MROW,
     I                 MESSU,MSGFL,EXTKW1,
     M                 PGPKEY,AVFG,ECOUNT,MAXROW,
     O                 RELFG)
C
          IF ( (RUNWID .EQ. 0) .AND. (MAXROW .GT. 0) ) THEN
C           release rows, as long as no
C           conditional special actions prevent release
            CALL RELESE (MAXROW,
     M                   RELFG,
     O                   AVFG)
          END IF
C
          IF (PGPKEY .GE. TSPKST) THEN
C           some primitive tsput instructions were written
            OPNTAB(19,OPNO)= TSPKST
            OPNTAB(20,OPNO)= PGPKEY
          END IF
C
C         find space occupied by osv for this operation
          OSVREC= OPNTAB(8,OPNO)- OPNTAB(7,OPNO)+ 1
C
C         update space required by osv's in this ingrp
          OSVSZ= MAX0(OSVSZ,OSVREC*500)
C
 20     CONTINUE
C
C       inpad virtual origin
        PADVO= OSVSZ
C
C       calculate inpad row width - no. of r4 values
        IF (MAXROW .GT. 0) THEN
          ROWLEN= (SCRSIZ - PADVO)/MAXROW
          WIDTH = ROWLEN- 1
        ELSE
C         undefined
          WIDTH= -999
        END IF
C
        GRPTAB(4,GRP)= PADVO
        GRPTAB(5,GRP)= WIDTH
        IF (OUTLEV .GT. 2) THEN
C         tell about the pad
          WRITE (MESSU,2010) GRP,OSVSZ,WIDTH,MAXROW,RUNWID
        END IF
C
 30   CONTINUE
C
      IF (OUTLEV .GT. 4) THEN
C       done message
        WRITE (MESSU,2020)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHAIN
     I                  (OPST,OPND,WKST,WKND,SRTCOL,RESCOL,ST,
     I                   ND,MAXOPN,
     O                   OPNTAB,WKSTCH,WKNDCH)
C
C     + + + PURPOSE + + +
C     Sort records wkst thru wknd in workfl in opn sequence order.
C     the sort is made on values in field srtcol and the result is
C     stored in field rescol.  the keys to the records at which
C     entries for each operation start and end are stored in columns
C     st and nd of opntab
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ND,OPND,MAXOPN,OPNTAB(20,MAXOPN),OPST,RESCOL,SRTCOL,ST,
     $          WKND,WKNDCH,WKST,WKSTCH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPST   - ???
C     OPND   - ???
C     WKST   - ???
C     WKND   - ???
C     SRTCOL - ???
C     RESCOL - ???
C     ST     - ???
C     ND     - ???
C     MAXOPN - ???
C     OPNTAB - ???
C     WKSTCH - ???
C     WKNDCH - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,OPNO,PREVKY,PREVOP,REC1(50),REC2(50),WKEY,RWFG,LEN,
     $          MXKY
C
C     + + + EXTERNALS + + +
      EXTERNAL  WORKIO
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize keys for start and end of chain
      WKSTCH= 0
      WKNDCH= 0
      LEN   = 50
C
      PREVOP= 0
C     loop thru operations
      DO 70 OPNO= OPST,OPND
C       loop thru workfile records
        DO 60 WKEY= WKST,WKND
          RWFG= 0
          MXKY= 0
          CALL WORKIO (RWFG,LEN,WKEY,
     M                 REC2,MXKY)
          IF (REC2(SRTCOL) .EQ. OPNO) THEN
C           this is the next entry
            IF (OPNO .NE. PREVOP) THEN
C             operation has changed - keep the starting key
              OPNTAB(ST,OPNO)= WKEY
C
              IF (PREVOP.GT.0) THEN
C               keep ending key for previous operation
                OPNTAB(ND,PREVOP)= PREVKY
              ELSE
C               keep key to start of chain
                WKSTCH= WKEY
              END IF
            END IF
C
            IF (PREVOP .GT. 0) THEN
C             record the previous entry, with chaining information
              REC1(RESCOL)= WKEY
              RWFG= 1
              MXKY= 0
              CALL WORKIO (RWFG,LEN,PREVKY,
     M                     REC1,MXKY)
            END IF
C
C           "present" data now become "previous" data
            DO 40 J=1,50
              REC1(J)= REC2(J)
 40         CONTINUE
            PREVOP= OPNO
            PREVKY= WKEY
          END IF
 60     CONTINUE
 70   CONTINUE
C
      IF (PREVOP .GT. 0) THEN
C       keep key for the last entry
        OPNTAB(ND,PREVOP)= PREVKY
C       keep key to end of chain
        WKNDCH= PREVKY
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHANGE
     I                   (EXTSFG,SDT,STKND,STTRN,STKEY,ROW,
     I                    SEQCOL,RESCOL)
C
C     + + + PURPOSE + + +
C     Change any subsequent appearances of a source or target to refer
C     to a given inpad row.  if we are dealing with an external source
C     (extsfg= 1), also change sdelt, stkind, and sttran.  the entries
C     are processed according to value in field SEQCOL, starting at
C     record STKEY
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EXTSFG,RESCOL,ROW,SDT,SEQCOL,STKEY,STKND,STTRN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXTSFG - ???
C     SDT    - ???
C     STKND  - ???
C     STTRN  - ???
C     STKEY  - ???
C     ROW    - ???
C     SEQCOL - ???
C     RESCOL - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KEY,REC(50),RWFG,LEN,MXKY
C
C     + + + EXTERNALS + + +
      EXTERNAL  WORKIO
C
C     + + + END SPECIFICATIONS + + +
C
      IF (STKEY .NE. 0) THEN
        LEN= 50
        KEY= STKEY
C
C       whiledo key not= 0
 10     CONTINUE
          RWFG= 0
          MXKY= 0
          CALL WORKIO (RWFG,LEN,KEY,
     M                 REC,MXKY)
C         change field to refer to given row
          REC(RESCOL)= ROW
C
          IF (EXTSFG .EQ. 1) THEN
C           also change stkind and sttran
            REC(4) = SDT
            REC(23)= STKND
            REC(24)= STTRN
          END IF
C
          RWFG= 1
          MXKY= 0
          CALL WORKIO (RWFG,LEN,KEY,
     M                 REC,MXKY)
          KEY= REC(SEQCOL)
C         loop back when more to do
        IF (KEY .NE. 0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OSVFPT
     I                   (GRPST,GRPND)
C
C     + + + PURPOSE + + +
C     Assign values to the flag-pointers for the time series in each
C     operation in this exgrp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GRPND,GRPST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GRPST  - ???
C     GRPND  - ???
C
C     + + + COMMON BLOCKS- INTERP3,OSV + + +
      INCLUDE 'crin3.inc'
      INCLUDE 'crin3c.inc'
      INCLUDE 'cmosv.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GRP,KEYND,KEYST,MESSU,OPND,OPST,PADVO,WIDTH,
     $          WKND,WKST1,WKST2,WKST3,WKST4,I1,I2,I3,I4,I
C
C     + + + EXTERNALS + + +
      EXTERNAL  OSVPRO,GETOSV,PUTOSV
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(//,' ',132('+'),
     $       //,' TIMESERIES USED BY OPERATION',3X,A4,A2,2X,I3)
 2010 FORMAT (/,'   INPUT TIMESERIES')
 2020 FORMAT (/,'     FROM EXTERNAL SOURCES',
     $        /,'     TYPE      #  INTERVAL   TRN STR    ',
     $          '     MFACT    GROUP   MEMBER  S1  S2   ',/)
 2050 FORMAT (/,'     FROM OTHER OPERATIONS(NETWORK)',
     $        /,'     TYPE      # GROUP   MEMBER  S1  S2 ',
     $          '     MFACT    GROUP   MEMBER  S1  S2   ',/)
 2080 FORMAT (/,'   OUTPUT TIMESERIES')
 2090 FORMAT (/,'     TO OTHER OPERATIONS(NETWORK)',
     $        /,'     GROUP   MEMBER  S1  S2        MFACT',
     $          '  TYPE      # GROUP   MEMBER  S1  S2   ',/)
 2120 FORMAT (/,'     TO EXTERNAL TARGETS',
     $        /,'     GROUP   MEMBER  S1  S2        MFACT',
     $          '   TRN STR  TYPE      #  INTERVAL      ',/)
C
C     + + + END SPECIFICATIONS + + +
C
      MESSU = FILE(1)
C
C     ingrp loop
      DO 200 GRP= GRPST,GRPND
        OPST = GRPTAB(1,GRP)
        OPND = GRPTAB(2,GRP)
        PADVO= GRPTAB(4,GRP)
        WIDTH= GRPTAB(5,GRP)
C
        DO 100 OPNO= OPST,OPND
C         read in the osv for this operation
          KEYST= OPNTAB(7,OPNO)
          KEYND= OPNTAB(8,OPNO)
          CALL GETOSV (KEYST,KEYND,MAXOSV,
     O                 OSV)
C
          IF (OUTLEV .GT. 2) THEN
C           timeseries used message
            WRITE(MESSU,2000) (OPNTAB(I,OPNO),I=1,3)
          END IF
C
C         process input time series
          WKST1= OPNTAB(9,OPNO)
          WKST2= OPNTAB(13,OPNO)
C
          IF (OUTLEV .GT. 2 .AND. (WKST1.GT.0 .OR. WKST2.GT.0)) THEN
C           input timeseries
            WRITE(MESSU,2010)
          END IF
C
          IF (WKST1 .GT. 0) THEN
C           from external sources
            IF (OUTLEV .GT. 2) THEN
C             message
              WRITE(MESSU,2020)
            END IF
C
            WKND = OPNTAB(10,OPNO)
            I1   = 41
            I2   = 44
            I3   = 37
            I4   = 1
C
            CALL OSVPRO (WKST1,WKND,I1,I2,I3,I4,PADVO,WIDTH,
     I                   MESSU,OUTLEV,TRNKWL,MAXOPN,OPNTAB,MAXOSV,
     M                   OSV)
          END IF
C
C         process input time series, from network block
          IF (WKST2 .GT. 0) THEN
C           from other operations
            IF (OUTLEV .GT. 2) THEN
C             message
              WRITE(MESSU,2050)
            END IF
C
            WKND= OPNTAB(14,OPNO)
            I1  = 41
            I2  = 44
            I3  = 37
            I4  = 2
C
            CALL OSVPRO (WKST2,WKND,I1,I2,I3,I4,PADVO,WIDTH,
     I                   MESSU,OUTLEV,TRNKWL,MAXOPN,OPNTAB,MAXOSV,
     M                   OSV)
          END IF
C
C         enhancement to check for missing required input timeseries
C         will go here
C
C         process output time series
          WKST3= OPNTAB(11,OPNO)
          WKST4= OPNTAB(15,OPNO)
C
          IF (OUTLEV .GT. 2 .AND. (WKST3.GT.0 .OR. WKST4.GT.0)) THEN
C           output timeseries
            WRITE(MESSU,2080)
          END IF
C
          IF (WKST3 .GT. 0) THEN
C           from operations
            IF (OUTLEV .GT. 2) THEN
C             message
              WRITE(MESSU,2090)
            END IF
C
            WKND= OPNTAB(12,OPNO)
            I1  = 15
            I2  = 18
            I3  = 11
            I4  = 3
C
            CALL OSVPRO (WKST3,WKND,I1,I2,I3,I4,PADVO,WIDTH,
     I                   MESSU,OUTLEV,TRNKWL,MAXOPN,OPNTAB,MAXOSV,
     M                   OSV)
          END IF
C
          IF (WKST4 .GT. 0) THEN
C           from ext targets
            IF (OUTLEV .GT. 2) THEN
              WRITE(MESSU,2120)
            END IF
C
C           process output time series from the ext targets block
            WKND= OPNTAB(16,OPNO)
            I1  = 15
            I2  = 18
            I3  = 11
            I4  = 4
C
            CALL OSVPRO (WKST4,WKND,I1,I2,I3,I4,PADVO,WIDTH,
     I                   MESSU,OUTLEV,TRNKWL,MAXOPN,OPNTAB,MAXOSV,
     M                   OSV)
          END IF
C
C         write the osv out to osvfl
          CALL PUTOSV (KEYST,KEYND,MAXOSV,OSV)
C
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   OSVPRO
     I                   (WKST,WKND,SRTCOL,ROWCOL,SUBCOL,TYP,
     I                    PADVO,WIDTH,MESSU,OUTLEV,TRNKWL,MAXOPN,
     I                    OPNTAB,MAXOSV,
     M                    OSV)
C
C     + + + PURPOSE + + +
C     Process a group of records from workfl, converting inpad row
C     assignments to osv flag-pointer values.  wkst and wknd are the
C     starting and ending keys for the search.  srtcol is the field
C     containing the key to the next entry in workfl, field rowcol
C     contains the inpad row and field subcol contains the location
C     of the flag-ptr in the osv
C
C     + + + HISTORY + + +
C     12/7/2004 jlk&pbd - added suppemental frac for MFACT change during
C         execution
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MAXOPN,MAXOSV,OSV(MAXOSV),PADVO,ROWCOL,SRTCOL,SUBCOL,
     #            WIDTH,WKND,WKST,MESSU,OUTLEV,TYP,
     #            OPNTAB(20,MAXOPN)
      CHARACTER*4 TRNKWL(8)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKST   - ???
C     WKND   - ???
C     SRTCOL - ???
C     ROWCOL - ???
C     SUBCOL - ???
C     TYP    - ???
C     PADVO  - ???
C     WIDTH  - inpad width
C     MESSU  - ftn unit no. to be used for printout of messages
C     OUTLEV - run interpreter output level
C     TRNKWL - ???
C     MAXOPN - ???
C     OPNTAB - ???
C     MAXOSV - ???
C     OSV    - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NEWKEY,OSVSUB,REC(50),ROW,ROWLEN,WKEY,LEN,RWFG,I,J,
     $             TRNIND,MXKY
      REAL         OMFACT
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (REC(21),RREC),(REC(22),FRAC)
      REAL         RREC, FRAC
C
C     + + + EXTERNALS + + +
      EXTERNAL     WORKIO
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (5X,A4,A2,1X,I4,5X,I5,6X,A4,4X,1PE10.3,4X,A4,A2,2X,
     $        A4,A2,2(2X,I2))
 2020 FORMAT (5X,A4,A2,1X,I4,1X,A4,A2,2X,A4,A2,2(2X,I2),1X,1PE10.3,
     $        4X,A4,A2,2X,A4,A2,2(2X,I2))
 2030 FORMAT (5X,A4,A2,2X,A4,A2,2(2X,I2),3X,1PE10.3,2X,A4,A2,1X,
     $        I4,1X,A4,A2,2X,A4,A2,2(2X,I2))
 2040 FORMAT (5X,A4,A2,2X,A4,A2,2(2X,I2),3X,1PE10.3,6X,A4,2X,
     $        A4,A2,1X,I4,5X,I5)
C
C     + + + END SPECIFICATIONS + + +
C
      LEN   = 50
      RWFG  = 0
      ROWLEN= WIDTH+ 1
C     there are entries in workfl for this operation
      NEWKEY= WKST
C
C     dountil wkey= wknd
 10   CONTINUE
        WKEY= NEWKEY
        MXKY= 0
        CALL WORKIO (RWFG,LEN,WKEY,
     M               REC,MXKY)
        NEWKEY= REC(SRTCOL)
        ROW   = REC(ROWCOL)
C
        IF (OUTLEV .GT. 2) THEN
          OMFACT = RREC* FRAC
          IF (TYP .EQ. 1) THEN
            IF (REC(3) .LT. 0) THEN
              REC(3)= -REC(3)
            ENDIF
            TRNIND= REC(20)
            WRITE (MESSU,2010) (REC(I),I=1,3),REC(19),
     $                          TRNKWL(TRNIND),OMFACT,(REC(I),I=31,36)
          ELSE IF (TYP .EQ. 2) THEN
            J= REC(3)
            WRITE (MESSU,2020) (REC(I),I=1,2),OPNTAB(3,J),
     $                         (REC(I),I=5,10),OMFACT,(REC(I),I=31,36)
          ELSE IF (TYP .EQ. 3) THEN
            J= REC(29)
            WRITE (MESSU,2030) (REC(I),I=5,10),OMFACT,(REC(I),I=27,28),
     $                          OPNTAB(3,J),(REC(I),I=31,36)
          ELSE IF (TYP .EQ. 4) THEN
            IF (REC(29) .LT. 0) THEN
              REC(29)= -REC(29)
            ENDIF
            TRNIND= REC(24)
            WRITE (MESSU,2040) (REC(I),I=5,10),OMFACT,TRNKWL(TRNIND),
     $                         (REC(I),I=27,30)
          END IF
        END IF
C
        IF (ROW .GT. 0) THEN
C         a row was allocated
          OSVSUB     = REC(SUBCOL)
          OSV(OSVSUB)= PADVO+ (ROW-1)*ROWLEN
        END IF
C
      IF (WKEY .NE. WKND) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   RELESE
     I                   (MAXROW,
     M                    RELFG,
     O                    AVFG)
C
C     + + + PURPOSE + + +
C     Release all rows flagged for release
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAXROW,AVFG(MAXROW),RELFG(MAXROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAXROW - ???
C     RELFG  - ???
C     AVFG   - flag indicating whether or not each IMPAD row is available
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NR
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 NR= 1,MAXROW
        IF (RELFG(NR) .EQ. 1) THEN
          AVFG(NR) = 1
          RELFG(NR)= 0
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   ROW
     I                       (MROW,MESSU,MSGFL,
     M                        AVFG,ECOUNT,MAXROW)
C
C     + + + PURPOSE + + +
C     Find the number of the first available row in the inpad
C     (or expad).  return this value and mark the row "unavailable".
C     if applicable, update the flag which indicates the maximum
C     number of rows occupied at one time.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MROW,
     $          AVFG(MROW),ECOUNT,
     $          MSGFL,MESSU,MAXROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MROW   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     AVFG   - flag indicating whether or not each IMPAD row is available
C     ECOUNT - count(s) of specific errors
C     MAXROW - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTI,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
      ROW = 0
      SCLU= 207
C     dountil row= mrow or avfg(row)= 1
 10   CONTINUE
        ROW= ROW+ 1
      IF (ROW .NE. MROW .AND. AVFG(ROW) .NE. 1) GO TO 10
C
      IF (ROW .EQ. MROW .AND. AVFG(ROW) .EQ. 0) THEN
C       error - no rows are available
        CALL OMSTI (MROW)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
C
      END IF
C
C     mark row unavailable
      AVFG(ROW)= 0
      IF (ROW .GT. MAXROW) THEN
        MAXROW= ROW
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SAME
     I                 (EXTSFG,SEQCOL,RESCOL,CHK1,CHK2,WKST1,
     I                  WKND1,WKST2,WKND2,MESSU,MSGFL,
     I                  ECOUNT)
C
C     + + + PURPOSE + + +
C     Consider records wkst1 thru wknd1 and wkst2 thru wknd2 in
C     workfl.  chain all occurrences of the same value in fields chk1
C     and chk2 (for external sources also check fields 27 and 28 -
C     stkind/sttran).  consider entries in order indicated by value
C     in field seqcol, put result in column rescol.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CHK1,CHK2,EXTSFG,RESCOL,SEQCOL,WKND1,WKND2,WKST1,
     $          WKST2,MESSU,MSGFL,
     $          ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXTSFG - ???
C     SEQCOL - ???
C     RESCOL - ???
C     CHK1   - ???
C     CHK2   - ???
C     WKST1  - ???
C     WKND1  - ???
C     WKST2  - ???
C     WKND2  - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - ftn unit no. for hspf messages
C     ECOUNT - count(s) of specific errors
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CHKKEY,DUMKEY,FINDFG,NXTKY1,NXTKY2,REC1(50),MXKY,
     $            REC2(50),WKEY,WLST,RWFG,LEN,IX,I4,SCLU,SGRP
      CHARACTER*4 CTSS,CSEQ,CTMP1,CTMP2
C
C     + + + INTRINSICS + + +
      INTRINSIC   IABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    WORKIO,OMSG,OMSTI,OMSTC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  CTSS,CSEQ/'TSS ','SEQ '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 207
      LEN = 50
      I4= 4
C
      IF (WKST1 .GT. 0) THEN
C       first range is not null
        WKEY= WKST1
C
        IF (WKST2 .GT. 0) THEN
C         2nd range not null
          WLST= WKND2
        ELSE
C         2nd range null
          WLST= WKND1
        END IF
      ELSE IF (WKST2 .EQ. 0) THEN
C       both null
        WKEY= 0
        WLST= 0
      ELSE
C       neither null
        WKEY= WKST2
        WLST= WKND2
      END IF
C
C     whiledo wkey not= wlst
 70   IF (WKEY .EQ. WLST) GO TO 200
        RWFG= 0
        MXKY= 0
        CALL WORKIO (RWFG,LEN,WKEY,
     M               REC1,MXKY)
C       find key of next record to be considered
        IF (WKEY .NE. WKND1) GO TO 80
          NXTKY1= WKST2
          GO TO 90
 80     CONTINUE
          NXTKY1= REC1(SEQCOL)
 90     CONTINUE
C
C       initialize search "state" variables
        FINDFG= 0
        DUMKEY= WKEY
        NXTKY2= NXTKY1
C
C       search for record with matching value
C       whiledo findfg= 0 and dumkey not= wlst
 100    IF (FINDFG .NE. 0 .OR. DUMKEY .EQ. WLST) GO TO 170
C         get key of next record, the one to be checked
          CHKKEY= NXTKY2
          RWFG  = 0
          MXKY  = 0
          CALL WORKIO (RWFG,LEN,CHKKEY,
     M                 REC2,MXKY)
          IF (REC2(CHK1) .NE. REC1(CHK1) .OR. REC2(CHK2) .NE.
     $       REC1(CHK2)) GO TO 140
C           same time series
            IF (EXTSFG .NE. 1) GO TO 120
              WRITE(CTMP1,2000) REC1(1)
              WRITE(CTMP2,2000) REC2(1)
              IF (CTMP1 .EQ. CSEQ .AND. CTMP2 .EQ. CSEQ) THEN
C               whoops - can't have one seq source going to 2 targets
                IX= IABS(REC1(CHK1))
                CALL OMSTC (I4,CSEQ)
                CALL OMSTI (IX)
                SGRP = 2
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              END IF
C
C             stkind/sttran/svol must also match
              IF (REC2(23) .NE. REC1(23) .OR. REC2(24) .NE. REC1(24)
     $            .OR. REC2(1) .NE. REC1(1)) GO TO 110
C               yes
                FINDFG= 1
 110          CONTINUE
              GO TO 130
C
 120        CONTINUE
C
              IF (EXTSFG .NE. -1) GOTO 128
                WRITE(CTMP1,2000) REC1(27)
                WRITE(CTMP2,2000) REC2(27)
                IF (CTMP1.EQ.CTSS .AND. CTMP2.EQ.CTSS) THEN
C                 whoops- can't have multiple sources for one tss target
                  CALL OMSTC (I4,CTSS)
                  CALL OMSTI (REC1(CHK1))
                  SGRP = 3
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
 128          CONTINUE
C
              FINDFG= 1
C
 130        CONTINUE
C
 140      CONTINUE
C
C         get the key of the next record to be checked
          IF (CHKKEY .NE. WKND1) GO TO 150
            NXTKY2= WKST2
            GO TO 160
 150      CONTINUE
            NXTKY2= REC2(SEQCOL)
 160      CONTINUE
          DUMKEY= CHKKEY
          GO TO 100
C
 170    CONTINUE
C
        IF (FINDFG .NE. 1) GO TO 180
C         match was found
          REC1(RESCOL)= CHKKEY
          GO TO 190
 180    CONTINUE
          REC1(RESCOL)= 0
 190    CONTINUE
C
        RWFG=1
        MXKY= 0
        CALL WORKIO (RWFG,LEN,WKEY,
     M               REC1,MXKY)
        WKEY= NXTKY1
        GO TO 70
C
 200  CONTINUE
C
      IF (WLST .EQ. 0) GO TO 210
C       last entry needs to be processed
        RWFG=0
        MXKY= 0
        CALL WORKIO (RWFG,LEN,WLST,
     M               REC1,MXKY)
        REC1(RESCOL)= 0
        RWFG        = 1
        MXKY        = 0
        CALL WORKIO (RWFG,LEN,WLST,
     M               REC1,MXKY)
C
 210  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOURCE
     I                   (EXTTFG,WKST,WKND,MROW,
     I                    MESSU,MSGFL,EXTKW1,
     M                    PRPKEY,AVFG,ECOUNT,MAXROW,
     O                    RELFG)
C
C     + + + PURPOSE + + +
C     Process entries from the network or ext targets block,
C     considering operation as a source
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MROW,
     $            AVFG(MROW),ECOUNT,
     $            MSGFL,EXTTFG,MAXROW,MESSU,PRPKEY,RELFG(MROW),
     $            WKND,WKST
      CHARACTER*1 EXTKW1(16)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXTTFG - ???
C     TSPUTF - ???
C     WKST   - ???
C     WKND   - ???
C     MROW   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     EXTKW1 - ???
C     PRPKEY - ???
C     AVFG   - flag indicating whether or not each IMPAD row is available
C     ECOUNT - count(s) of specific errors
C     MAXROW - ???
C     RELFG  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    REC(50),NEWKEY,SAMDCD,SCHAIN,SDELT,SGAPCD,SGRPN(2),
     $           SINF(22),SMEMN(2),SMEMSB(2),SNUM,SOFFST,SROW,
     $           SSNXT,STKIND,STNXT,STTRAN,SVOL(2),TAMDCD,TCHAIN,
     $           TDELT,TGAPCD,TGRPN(2),TINF(22),TMEMN(2),TMEMSB(2),
     $           TNUM,TOFFST,TROW,TSNXT,TTNXT,TVOL(2),WKEY,MXKY,
     $           I,I0,I2,I4,I17,I18,RWFG,LEN,SFRC,TFRC,KWDNO
      REAL       A,B,AORIG,FRAC
      CHARACTER*4 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE(CHSTR,CHSTR1)
      CHARACTER*1 CHSTR1(4)
      EQUIVALENCE  (REC(1),SINF(1)),          (REC(1),SVOL(1)),
     $             (REC(3),SNUM),             (REC(4),SDELT),
     $             (REC(5),SGRPN(1)),         (REC(7),SMEMN(1)),
     $             (REC(9),SMEMSB(1)),        (REC(11),SOFFST),
     $             (REC(12),SAMDCD),          (REC(13),SGAPCD),
     $             (REC(14),SFRC),            (REC(15),SCHAIN),
     $             (REC(16),STNXT),           (REC(17),SSNXT),
     $             (REC(18),SROW),            (REC(23),STKIND),
     $             (REC(24),STTRAN),          (REC(25),A),
     $             (REC(26),B)
      EQUIVALENCE  (REC(21),AORIG),           (REC(22),FRAC)
C
      EQUIVALENCE  (REC(27),TINF(1)),         (REC(27),TVOL(1)),
     $             (REC(29),TNUM),            (REC(30),TDELT),
     $             (REC(31),TGRPN(1)),        (REC(33),TMEMN(1)),
     $             (REC(35),TMEMSB(1)),       (REC(37),TOFFST),
     $             (REC(38),TAMDCD),          (REC(39),TGAPCD),
     $             (REC(40),TFRC),            (REC(41),TCHAIN),
     $             (REC(42),TTNXT),           (REC(43),TSNXT),
     $             (REC(44),TROW)
C
C     + + + FUNCTIONS + + +
      INTEGER    ROW,CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ROW,CHKSTR,WORKIO,CHANGE,TSINS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I2  = 2
      I4  = 4
      I17 = 17
      I18 = 18
      RWFG= 0
      LEN = 50
      IF (WKST .GT. 0) THEN
C       there are entries for this operation
        NEWKEY= WKST
C       dountil wkey= wknd
 10     CONTINUE
          WKEY= NEWKEY
          MXKY= 0
C
          CALL WORKIO (RWFG,LEN,WKEY,
     M                 REC,MXKY)
          IF (SROW .EQ. 0) THEN
C           this source does not yet have a row
            SROW= ROW(MROW,MESSU,MSGFL,
     M                AVFG,ECOUNT,MAXROW)
C
C           change this and any subsequent references to this
C           source (in ext targets and network blocks) to refer to
C           this row
            CALL CHANGE (I0,I0,I0,I0,WKEY,SROW,I17,I18)
C
          END IF
C
          IF (EXTTFG .EQ. 1) THEN
C           ext targets block
C           volume is 1 for tss and seq, 2 for wdms and dss
            I = 1
C           search last two keywords
            WRITE (CHSTR,2000) TVOL(1)
            CHSTR1(4)= ' '
            KWDNO= CHKSTR(I4,I2,CHSTR1,EXTKW1(9))
            IF (KWDNO .NE. 0) THEN
C             wdm or dss
              I = 2
            END IF
C           AORIG is set in PAIRS to WORKFL address 21
C           FRAC needs to be added for secondary update during run
            CALL TSINS (I,TNUM,TDELT,SROW,TOFFST,STKIND,
     I                  STTRAN,I0,A,B,TMEMSB,TAMDCD,TGAPCD,TFRC,
     I                  SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                  TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                  PRPKEY)
C
C           flag the row for release - this instruction may be
C           reversed when network entries are processed
            RELFG(SROW)= 1
C
          ELSE
C           network block - no action required
C           don't release this row because that will be done when
C           the last operation for which it is a source is processed
            RELFG(SROW)= 0
C
          END IF
          NEWKEY= SCHAIN
C
        IF (WKEY .NE. WKND) GO TO 10
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TARGET
     I                   (EXTSFG,WKST,WKND,MROW,
     I                    MESSU,MSGFL,EXTKW1,RUNWID,
     M                    PRGKEY,AVFG,ECOUNT,MAXROW,
     O                    RELFG)
C
C     + + + PURPOSE + + +
C     process entries in workfl, derived from ext sources or network
C     block, considering operation as a target
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     EXTSFG,WKST,WKND,MROW,MESSU,MSGFL,RUNWID,PRGKEY,
     $            AVFG(MROW),ECOUNT,MAXROW,RELFG(MROW)
      CHARACTER*1 EXTKW1(16)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXTSFG - ???
C     WKST   - ???
C     WKND   - ???
C     MROW   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     EXTKW1 - ???
C     RUNWID - ??? 
C     PRGKEY - ???
C     AVFG   - flag indicating whether or not each IMPAD row is available
C     ECOUNT - count(s) of specific errors
C     MAXROW - ???
C     RELFG  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   REC(50),NEWKEY,SAMDCD,SCHAIN,SDELT,SFRC,SGAPCD,
     $          SGRPN(2),SINF(22),SMEMN(2),SMEMSB(2),SNUM,SOFFST,SROW,
     $          SSNXT,STKIND,STNXT,STTRAN,SVOL(2),TAMDCD,TCHAIN,
     $          TDELT,TFRC,TGAPCD,TGRPN(2),TINF(22),TMEMN(2),TMEMSB(2),
     $          TNUM,TOFFST,TROW,TSNXT,TTNXT,TVOL(2),WKEY,KWDNO,
     $          I,I0,I1,I2A,I2B,I2,I3,I4,RWFG,LEN,MXKY
      REAL      A,B,AORIG
      CHARACTER*4 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE(CHSTR,CHSTR1)
      CHARACTER*1 CHSTR1(4)
      EQUIVALENCE  (REC(1),SINF(1)),          (REC(1),SVOL(1)),
     $             (REC(3),SNUM),             (REC(4),SDELT),
     $             (REC(5),SGRPN(1)),         (REC(7),SMEMN(1)),
     $             (REC(9),SMEMSB(1)),       (REC(11),SOFFST),
     $             (REC(12),SAMDCD),          (REC(13),SGAPCD),
     $             (REC(14),SFRC),            (REC(15),SCHAIN),
     $             (REC(16),STNXT),           (REC(17),SSNXT),
     $             (REC(18),SROW),            (REC(23),STKIND),
     $             (REC(24),STTRAN),          (REC(25),A),
     $             (REC(26),B)
      EQUIVALENCE  (REC(21),AORIG),           (REC(22),FRAC)
C
      EQUIVALENCE  (REC(27),TINF(1)),         (REC(27),TVOL(1)),
     $             (REC(29),TNUM),            (REC(30),TDELT),
     $             (REC(31),TGRPN(1)),        (REC(33),TMEMN(1)),
     $             (REC(35),TMEMSB(1)),       (REC(37),TOFFST),
     $             (REC(38),TAMDCD),          (REC(39),TGAPCD),
     $             (REC(40),TFRC),            (REC(41),TCHAIN),
     $             (REC(42),TTNXT),           (REC(43),TSNXT),
     $             (REC(44),TROW)
C
C     + + + FUNCTIONS + + +
      INTEGER    ROW,CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ROW,CHKSTR,WORKIO,TSINS,CHANGE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I3  = 3
      I4  = 4
      LEN = 50
      RWFG= 0
C
      IF (WKST .GT. 0) THEN
C       there are entries for this operation
        NEWKEY= WKST
C
        IF (EXTSFG .EQ. 1) THEN
C         move external sources into inpad where necessary
C
C         dountil wkey= wknd
 10       CONTINUE
            WKEY= NEWKEY
            MXKY= 0
            CALL WORKIO (RWFG,LEN,WKEY,
     M                   REC,MXKY)
            IF (SROW .EQ. 0) THEN
C             this source is not yet in the inpad
              IF ( (STNXT .EQ. 0) .AND. (RUNWID .EQ. 0) ) THEN
C               it's not required again in this ingroup, with same
C               functional
C               deal with this later
C
              ELSE
C               allocate a row and move it in, replacing present
C               contents of row
                SROW= ROW(MROW,MESSU,MSGFL,
     M                    AVFG,ECOUNT,MAXROW)
C               volume is 1 for tss and seq, 2 for wdms and dss
                I = 1
C               search last two keywords
                WRITE(CHSTR,2000) SVOL(1)
                CHSTR1(4)= ' '
                KWDNO= CHKSTR(I4,I2,CHSTR1,EXTKW1(9))
                IF (KWDNO .NE. 0) THEN
C                 wdm or dss
                  I = 2
                END IF
C               AORIG is set in PAIRS to WORKFL address 21
                CALL TSINS (I,SNUM,SDELT,SROW,SOFFST,STKIND,
     I                      STTRAN,I0,1.0,0.0,SMEMSB,SAMDCD,SGAPCD,
     I                      SFRC,
     I                      SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                      TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                      PRGKEY)
C
C               change this, and other references to this source, to
C               reflect its new location and, possibly, form
                IF (STKIND .EQ. 3) STKIND= 2
                STTRAN= 1
                I2A   =16
                I2B   =18
                CALL CHANGE (I1,TDELT,STKIND,STTRAN,WKEY,SROW,
     I                       I2A,I2B)
C
              END IF
C
            END IF
            NEWKEY= TCHAIN
C
          IF (WKEY .NE. WKND) GO TO 10
C
        END IF
C
C       get sources to the target operations
        NEWKEY= WKST
C       dountil wkey= wknd
 20     CONTINUE
          WKEY= NEWKEY
          MXKY= 0
          CALL WORKIO (RWFG,LEN,WKEY,
     M                 REC,MXKY)
          IF (SROW .EQ. 0) THEN
C           source is not yet in inpad (and it's not required again
C           in this ingroup, else it would have been moved in already)
            IF (TROW .EQ. 0) THEN
C             target doesn't have a row
              TROW= ROW(MROW,MESSU,MSGFL,
     M                  AVFG,ECOUNT,MAXROW)
C
C             change this and any future references to this target
C             to refer to trow
              I2A=42
              I2B=44
              CALL CHANGE (I0,I0,I0,I0,WKEY,TROW,I2A,I2B)
C
C             move in the source, replacing existing values in row
              I = 1
C             search last two keywords
              WRITE(CHSTR,2000) SVOL(1)
              CHSTR1(4)= ' '
              KWDNO= CHKSTR(I4,I2,CHSTR1,EXTKW1(9))
              IF (KWDNO .NE. 0) THEN
C               wdm or dss
                I = 2
              END IF
C             AORIG is set in PAIRS to WORKFL address 21
              CALL TSINS (I,SNUM,SDELT,TROW,SOFFST,STKIND,
     I                    STTRAN,I0,A,B,SMEMSB,SAMDCD,SGAPCD,SFRC,
     I                    SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                    TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                    PRGKEY)
C
C             mark the row for later release
              RELFG(TROW)= 1
C
            ELSE
C             target already has a row
C             move in the source, adding to existing values
              I = 1
C             search last two keywords
              WRITE(CHSTR,2000) SVOL(1)
              CHSTR1(4)= ' '
              KWDNO= CHKSTR(I4,I2,CHSTR1,EXTKW1(9))
              IF (KWDNO .NE. 0) THEN
C               wdm or dss
                I = 2
              END IF
C             AORIG is set in PAIRS to WORKFL address 21
              CALL TSINS (I,SNUM,SDELT,TROW,SOFFST,STKIND,
     I                    STTRAN,I1,A,B,SMEMSB,SAMDCD,SGAPCD,SFRC,
     I                    SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                    TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                    PRGKEY)
C
            END IF
C
          ELSE
C           source is already in inpad
            IF (TROW .EQ. 0) THEN
C             target doesn't yet have a row
              IF ( (STNXT .EQ. 0) .AND. (RUNWID .EQ. 0) ) THEN
C               target uses same row as source
                TROW= SROW
              ELSE
C               target needs a row
                TROW = ROW (MROW,MESSU,MSGFL,
     M                    AVFG,ECOUNT,MAXROW)
              END IF
C
C             mark row for later release
              RELFG(TROW)= 1
C
C             alter this, and any subsequent references to this
C             target to refer to trow
              I2A=42
              I2B=44
              CALL CHANGE (I0,I0,I0,I0,WKEY,TROW,I2A,I2B)
C
C             move source from srow to trow, replacing existing
C             values
C             AORIG is set in PAIRS to WORKFL address 21
              CALL TSINS (I3,SROW,SDELT,TROW,SOFFST,STKIND,
     I                    STTRAN,I0,A,B,SMEMSB,SAMDCD,SGAPCD,SFRC,
     I                    SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                    TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                    PRGKEY)
C
            ELSE
C             target has a row
C             add source to target
C             AORIG is set in PAIRS to WORKFL address 21
              CALL TSINS (I3,SROW,SDELT,TROW,SOFFST,STKIND,
     I                    STTRAN,I1,A,B,SMEMSB,SAMDCD,SGAPCD,SFRC,
     I                    SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                    TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                    PRGKEY)
              IF ( (STNXT .EQ. 0) .AND. (RUNWID .EQ. 0) ) THEN
C               release the source row now
                AVFG(SROW)= 1
              END IF
C
            END IF
C
          END IF
          NEWKEY= TCHAIN
C
        IF (WKEY .NE. WKND) GO TO 20
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSINS
     I                  (VOLUME,NUM,DELTAT,ROW,OFFSET,STKIND,
     I                   STTRAN,INMODE,A,B,MEMSB,AMDCD,GAPCD,FRC,
     I                   SVOL,SNUM,SGRPN,SMEMN,SMEMSB,AORIG,
     I                   TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     M                   PRKEY)
C
C     + + + PURPOSE + + +
C     Write a primitive tsget/tsput instruction
C
C     + + + HISTORY + + +
C     12/6/2004 - jlk&pbd - add additional information for MFACT
C       change during execution
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    AMDCD,DELTAT,FRC,GAPCD,INMODE,MEMSB(2),NUM,
     $           OFFSET,PRKEY,ROW,STKIND,STTRAN,VOLUME,
     $           SVOL(2),SNUM,SGRPN(2),SMEMN(2),SMEMSB(2),
     $           TVOL(2),TNUM,TGRPN(2),TMEMN(2),TMEMSB(2)
      REAL       A,B,AORIG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOLUME - ???
C     NUM    - ???
C     DELTAT - time step in minutes for source/target
C     ROW    - ???
C     OFFSET - ???
C     STKIND - ???
C     STTRAN - ???
C     INMODE - ???
C     A      - multiplying factor for the linear transform A*X + B
C     B      - shift value for the linear transform
C     MEMSB  - ???
C     AMDCD  - ???
C     GAPCD  - ???
C     FRC    - ???
C     PRKEY  - ???
C     SVOL   - source volume name
C     SNUM   - source volume number
C     SGRPN  - source group name
C     SMEMN  - source member name
C     SMEMSB - source member subscripts
C     AORIG  - original multiplication factor
C     TVOL   - target volume name
C     TNUM   - target volume number
C     TGRPN  - target group name
C     TMEMN  - target member name
C     TMEMSB - target member subscripts
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FILE,FORMT(2),NULLFG,VOLCOD,
     $           REC(35),I35
      REAL       RREC(35)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (REC(1),RREC(1))
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   PUTTSI
C
C     + + + END SPECIFICATIONS + + +
C
      I35= 35
C
      IF (VOLUME .EQ. 1) THEN
C       may be tss or seq
        IF (NUM .LT. 0) THEN
C         sequential file
          FILE  = -NUM
          VOLCOD= 1
        ELSE
C         tss dataset
          FILE  = NUM
          VOLCOD= 4
        END IF
C
      ELSE IF (VOLUME .EQ. 2) THEN
C       may be wdms or dss
        IF (NUM .LT. 0) THEN
C         dss dataset
          FILE  = -NUM
          VOLCOD= 6
        ELSE
C         wdms dataset
          FILE  = NUM
          VOLCOD= -MEMSB(2)
        END IF
C
      ELSE
C       scratch pad
        VOLCOD= VOLUME
        FILE  = NUM
C
      END IF
C
C     2004/12/9 jlk-force no null for MFACT changes during exec
C        only a subset of possible nulls were being identified
C        PerPerRch SOLRAD only caught PerRch, not all 3
C        PerPer PREC, GATMP, etc did not catch anything
C        we need the detailed info from the null instructions later
      NULLFG= 0
C     IF (VOLCOD .EQ. 3) THEN
C       inpad to inpad - check for null instruction
C       IF (FILE .EQ. ROW) THEN
C         IF (STKIND .LT. 3 .AND. STTRAN .EQ. 1) THEN
C           IF (INMODE .EQ. 0) THEN
C             if (a .ne. 1.0 .or. b .ne. 0.0) go to 10
C             IF ((ABS(A-1.0)) .GT. 1.0E-5 .OR. (ABS(B)) .GT. 0.0)
C    $                GO TO 10
C               null instruction
C               NULLFG= 1
C10           CONTINUE
C           END IF
C         END IF
C       END IF
C     END IF
C
      IF (VOLCOD .EQ. 1) THEN
C       format codes (for seq input) have been stored in member
C       subscript fields
        FORMT(1)= MEMSB(1)
        FORMT(2)= MEMSB(2)
      ELSE IF (VOLCOD .LT. 0) THEN
C       quality of data for wdms was stored in member
C       subscript field
        FORMT(1)= MEMSB(1)
        FORMT(2)= 0
      ELSE
C       format field is not meaningful
        FORMT(1)= 0
        FORMT(2)= 0
      END IF
C
      IF (NULLFG .EQ. 0) THEN
C       write primitive instruction
        PRKEY   = PRKEY+ 1
        REC(1)  = VOLCOD
        REC(2)  = FILE
        REC(3)  = DELTAT
        REC(4)  = ROW
        REC(5)  = OFFSET
        REC(6)  = STKIND
        REC(7)  = STTRAN
        REC(8)  = INMODE
        RREC(9) = A
        RREC(10)= B
        REC(11) = FORMT(1)
        REC(12) = FORMT(2)
        REC(13) = AMDCD
        REC(14) = GAPCD
        REC(15) = FRC
        REC(16) = SVOL(1)
        REC(17) = SVOL(2)
        REC(18) = SNUM
        REC(19) = SGRPN(1)
        REC(20) = SGRPN(2)
        REC(21) = SMEMN(1)
        REC(22) = SMEMN(2)
        REC(23) = SMEMSB(1)
        REC(24) = SMEMSB(2)
        RREC(25)= AORIG
        REC(26) = TVOL(1)
        REC(27) = TVOL(2)
        REC(28) = TNUM
        REC(29) = TGRPN(1)
        REC(30) = TGRPN(2)
        REC(31) = TMEMN(1)
        REC(32) = TMEMN(2)
        REC(33) = TMEMSB(1)
        REC(34) = TMEMSB(2)
        REC(35) = 0
        CALL PUTTSI (I35,PRKEY,REC)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WKDMP1
     I                   (WKST,WKND,MESSU)
C
C     + + + PURPOSE + + +
C     Dump a specified range of the workfl to messu
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,WKND,WKST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKST   - ???
C     WKND   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,REC(50),WKEY,RWFG,LEN,MXKY
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (REC(25),A), (REC(26),B)
      EQUIVALENCE  (REC(21),AORIG), (REC(22),FRAC)
      REAL      A,B, AORIG, FRAC
C
C     + + + EXTERNALS + + +
      EXTERNAL  WORKIO
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(/,' RECORDS WRITTEN TO WRKSPA ARE:')
 2010 FORMAT(  ' RECORD',I5,/,' ',A4,A2,I3,I4,A4,A2,A4,A2,2I2,I4,2I2,
     $                            I4,4I3,2I2,2E10.3,A4,A2,I3,I4,A4,A2,
     $                            A4,A2,2I2,I4,2I2,I4,4I3,2E10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (MESSU,2000)
      RWFG=0
      LEN =50
C
      WKEY= WKST
C     whiledo wkey<= wknd
 10   IF (WKEY .LE. WKND) THEN
        MXKY= 0
        CALL WORKIO (RWFG,LEN,WKEY,
     M               REC,MXKY)
        WRITE (MESSU,2010)  WKEY, (REC(J),J=1,18),
     $    (REC(J),J=23,24), A, B,
     $    (REC(J),J=27,44), AORIG, FRAC
        WKEY= WKEY+ 1
        GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WKDMP2
     I                   (WKST,WKND,MESSU)
C
C     + + + PURPOSE + + +
C     Dump a set of primitive tsget or tsput instructions
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,WKND,WKST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WKST   - ???
C     WKND   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,REC(35),WKEY,I35
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (REC(9),A),  (REC(10),B), (REC(25),AORIG)
      REAL      A, B, AORIG
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETTSI
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' RECORD',I5,/,' ',8I4,2E10.3,4I4,I8,1E10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I35= 35
C
      WKEY= WKST
C     whiledo wkey<= wknd
 10   IF (WKEY .LE. WKND) THEN
        CALL GETTSI (I35,WKEY,
     O               REC)
        WRITE (MESSU,2000)  WKEY, (REC(J),J=1,8), A, B,
     $    (REC(J),J=11,15), AORIG
        WKEY= WKEY+ 1
        GO TO 10
      END IF
C
      RETURN
      END
