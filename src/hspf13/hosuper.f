C
C     4.0
C
      SUBROUTINE OSUPER
     I                  (FILES,
     O                   RETCOD)
C
C     + + + PURPOSE + + +
C     This module is the operations supervisor.  it calls the various
C     operating modules in the correct sequence and ensures that the
C     various input and output time series are moved into and out of
C     the inpad.  it also reads the osvs from disc and writes them
C     back to disc when an operation terminates or is interrupted
C
C     + + + HISTORY + + +
C     2002  THJ allowed external control and interaction for IHM
C     12/20/2004 - jlk&pbd - added variable to keep track of current time
C       for mfact special action, added code to change inspan width
C       based on mfact special action
C     2005  PBD enhanced external control capability to allow external width > width
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FILES(15),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILES  - array of file unit numbers
C     RETCOD - return code, 1 for user interrupt
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OSV + + +
      INCLUDE      'cmosv.inc'
      INCLUDE      'cmpad.inc'
      INCLUDE      'cosupm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      COUNT,DELT,EXGINF(4),EXKEY,FSTCAL,GRP,INGINF(9),
     $             INKEY,LAST,LSTCAL,MESSU,NGRPS,NOPNS,NXGRPS,OMCODE,
     $             OPINF(8),OPKEY,OPN,OPTNO,OSUPKY,I11,I4,I8,I9,
     $             OSVKND,OSVKST,REPEAT,REPLST,RUNINF(11),TSGKND,
     $             TSGKST,TSPKND,TSPKST,WID,WIDTH,WLAST,XCOUNT,
     $             XDELT,XGRP,XREPET,XWIDTH,FOPKEY,LOPKEY,GKEY,PKEY,
     $             IW,IVLS,RUNWID,EXTFG,STIVL,I1,IOPT,STATFG,IVLDON,
     $             STECNT,MSGFL,STFIL,EXUPFG,CURDAT(6),NXTDAT(6),
     $             I2,I6,MUPFG,INSPAC,CUMWID,CUMEXT,WIDREM,EXTWID
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (RUNINF(1),NXGRPS)
C
      EQUIVALENCE  (EXGINF(1),XREPET), (EXGINF(2),XWIDTH),
     $             (EXGINF(3),XDELT),  (EXGINF(4),NGRPS)
C
      EQUIVALENCE  (INGINF(1),REPEAT), (INGINF(2),REPLST),
     $             (INGINF(3),WID),    (INGINF(4),WLAST),
     $             (INGINF(5),DELT),   (INGINF(6),NOPNS),
     $             (INGINF(7),RUNWID), (INGINF(8),EXUPFG),
     $             (INGINF(9),EXTWID)
C
      EQUIVALENCE  (OPINF(1),OMCODE),  (OPINF(2),OPTNO),
     $             (OPINF(3),TSGKST),  (OPINF(4),TSGKND),
     $             (OPINF(5),TSPKST),  (OPINF(6),TSPKND),
     $             (OPINF(7),OSVKST),  (OPINF(8),OSVKND)
C
C     + + + INTRINSICS + + +
      INTRINSIC    MIN
C
C     + + + FUNCTIONS + + +
      INTEGER      CKUSER
C
C     + + + EXTERNALS + + +
      EXTERNAL     HSPSTA,TSGET,GETOSV,PERLND,IMPLND,RCHRES,COPY
      EXTERNAL     HDISPL,DURANL,GENER,MUTSIN,PUTOSV,TSPUT,PLTGEN
      EXTERNAL     COPYI,CKUSER,GSTVFL,PSTVFL
      EXTERNAL     TIMADD,SPECLM,EXDATE 
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I2,I4,I5,I6,I8,I9,I11/1,2,4,5,6,8,9,11/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' COMMENCING EXECUTION')
 2010 FORMAT (' OPINF ',8I4)
 2020 FORMAT (1X,A,I5,5I3)
 2030 FORMAT (1X,A,8I6)
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize variables
      MESSU= FILES(1)
      MSGFL= FILES(15)
      STFIL= FILES(9)
      FSTCAL= 1
      STECNT= 0
C
C     first record
      OSUPKY= 1
C
      WRITE (MESSU,2000)
C
C     get information about this run
      CALL COPYI (I11,OSUPM(1,OSUPKY),
     O            RUNINF)
C     save starting date of run
      CALL COPYI (I5,RUNINF(2),NXTDAT)
      CALL EXDATE(NXTDAT,CURDAT)
      CURDAT(6)= 0
C      WRITE(99,2020) "OSUPER:START:CURDAT:", CURDAT
C
C     find the starting key for the first exgroup
      EXKEY= OSUPKY +1
C
C     Exgroup loop
      XGRP = 0
 10   CONTINUE
        XGRP= XGRP+ 1
C       write(99,*) 'xgrp,nxgrps',xgrp,nxgrps
C       get information about this exgroup
        CALL COPYI (I4,OSUPM(1,EXKEY),
     O              EXGINF)
C
C       exspan loop
        XCOUNT = 0
 20     CONTINUE
          XCOUNT = XCOUNT+ 1
C         find key for first ingroup
          INKEY= EXKEY+ 1
C
C         ingroup loop
          GRP = 0
 30       CONTINUE
            GRP = GRP+ 1
            CALL COPYI (I9,OSUPM(1,INKEY),
     O                  INGINF)
C           find the number of inspans in this exspan
            IF (XCOUNT.EQ.XREPET) THEN
              LAST= REPLST
            ELSE
              LAST= REPEAT
            END IF
C            WRITE(99,2030) 'OSUPER:grp,runwid,last',
C     1                             GRP,RUNWID,LAST
C
            IF (EXTWID.GT.WID) THEN
C             using external update functionality and 
C             external control width is greater than an inspan, 
C             be sure we have inspans ending at desired points.
C             keep track of cumulative inspan width processed
C             and cumulative external control width target
              CUMWID = 0
              CUMEXT = EXTWID
              WIDREM = 0
            ELSE
C             don't worry about cumulative external control width
              CUMEXT = -1
              WIDREM = 0
            END IF
C
C           inspan loop
            COUNT = 0
            IVLDON= 0
 40         CONTINUE
C              
C             find the width of this inspan
              IF (COUNT+1.EQ.LAST) THEN
                WIDTH= WLAST
              ELSE
                WIDTH= WID
              END IF
C
C              WRITE(99,*) 'OSUPER: b4EXT UPDATE ',CUMEXT,CUMWID,WIDTH,WIDREM
              IF (CUMEXT.GT.0) THEN
C               using external update functionality and 
C               external control width is greater than an inspan
C
C               for this example,
C
C                   wid       wid       wid       wid    wlast
C               |---------|---------|---------|---------|----|
C               |------------|------------|------------|
C                   extwid       extwid       extwid
C
C               the widths need to correspond to the hashes below:
C
C               |         |  |------|     |   |        ||    |  
C                             widrem
C                     a     b    c     d    e      f   g  h 
C
                IF (WIDREM.GT.0) THEN 
C                 get here after each extwid, 
C                 run what's left of the wid    (c,e,g)
                  IF (WIDREM.LT.WIDTH) THEN
C                   usually the case but might not be at the end of a run
                    WIDTH = WIDREM
                  END IF
                  WIDREM = 0 
C                 increment cumulative width computed
                  CUMWID = CUMWID + WIDTH 
C                  WRITE(99,*) 'OSUPER: EXT UPDATE WIDTH TYPE C ',CUMWID
C                  WRITE(99,*) 'OSUPER: EXT UPDATE ',CUMEXT,WIDTH,WIDREM
                ELSEIF (CUMWID+WIDTH .LT. CUMEXT) THEN
C                 regular case, there is room to run another width
C                 increment cumulative width computed   (a,h)
                  CUMWID = CUMWID + WIDTH
C                  WRITE(99,*) 'OSUPER: EXT UPDATE WIDTH TYPE A ',CUMWID
C                  WRITE(99,*) 'OSUPER: EXT UPDATE ',CUMEXT,WIDTH,WIDREM
                ELSEIF (CUMWID+WIDTH .GT. CUMEXT) THEN
C                 a whole additional width would be too much,
C                 run only to next external control width  (b,d,f)
C                 remember how much of the full width is remaining
                  WIDREM = CUMWID+WIDTH - CUMEXT
                  WIDTH = CUMEXT - CUMWID
C                 increment cumulative variables
                  CUMWID = CUMWID + WIDTH
                  CUMEXT = CUMEXT + EXTWID
C                  WRITE(99,*) 'OSUPER: EXT UPDATE WIDTH TYPE B ',CUMWID
C                  WRITE(99,*) 'OSUPER: EXT UPDATE ',CUMEXT,WIDTH,WIDREM
                ELSEIF (CUMWID+WIDTH .EQ. CUMEXT) THEN
C                 just the right amount of width, just increment
                  CUMWID = CUMWID + WIDTH
                  CUMEXT = CUMEXT + EXTWID
C                  WRITE(99,*) 'OSUPER: EXT UPDATE WIDTH TYPE X ',CUMWID
C                  WRITE(99,*) 'OSUPER: EXT UPDATE ',CUMEXT,WIDTH,WIDREM
                END IF
              END IF
C
              IF (WIDREM.EQ.0) THEN 
C               increment the count except when completing an external width
                COUNT = COUNT+ 1
              END IF
C
C             get ready to perform the operations in this ingroup
              OPKEY = INKEY+ 1
              FOPKEY= OPKEY
              LOPKEY= OPKEY+ NOPNS- 1
C
              IF (EXUPFG .EQ. 1) THEN
C               using external update functionality
                IF (CUMEXT .GT. 0) THEN
C                 external control width is greater than an inspan
                  IF (CUMWID-WIDTH .EQ. CUMEXT-EXTWID) THEN 
C                   at the end of an external width, 
C                   check for external control and interaction
                    CALL EXT_UPDATE (FILES(11),FOPKEY,LOPKEY,OSUPM)
                  END IF
                ELSE 
C                 check for external control and interaction
                  CALL EXT_UPDATE (FILES(11),FOPKEY,LOPKEY,OSUPM)
                END IF
              END IF
C
C             set up runwid loop
              STATFG= 0
              STIVL = 1
C
              IF (RUNWID .GT. 0) THEN
C               get all external time series before running any
C                WRITE(99,2030)
C     1            'OSUPER:runwid>0, getting all time series'
                EXTFG= 1
                DO 50 GKEY= FOPKEY, LOPKEY
                  CALL COPYI (I8,OSUPM(1,GKEY),
     O                        OPINF)
                  IF (TSGKST.GT.0) THEN
                    CALL TSGET (FILES,TSGKST,TSGKND,
     I                          DELT,I1,WIDTH,FSTCAL,EXTFG)
                  END IF
 50             CONTINUE
              END IF
C
              IVLS= WIDTH
C             WRITE(99,*)' set ivls',IVLS
C
C             begin do-until loop for runwid
 60           CONTINUE
C
                OPKEY= FOPKEY
C
                IF (RUNWID .GT. 0) THEN
C                 only run runwid or remaining intervals
                  IW= MIN (RUNWID,IVLS)
                ELSE
C                 run entire remaining width
                  IW= IVLS
C                 IW= WIDTH
                END IF
C
C                WRITE(99,2030) 
C     1            'OSUPER:width,ivls,runwid,iw',
C     1                    WIDTH,IVLS,RUNWID,IW
C
C               how many time steps to next mfact special action?
                CALL SPECLX(CURDAT,DELT,
     O                      INSPAC)
C
                IF (INSPAC .EQ. 0) THEN
C                 special action update to mfactr
                  CALL SPECLM (MESSU,CURDAT,
     I                         FOPKEY,LOPKEY,OSUPM,
     O                         MUPFG)
C                 check again for next
                  CALL SPECLX(CURDAT,DELT,
     O                        INSPAC)
C                  WRITE(99,2030) 'OSUPER:MUPFG,INSPAC:',MUPFG,INSPAC
                END IF
C
                IF (INSPAC .GT. 0 .AND. INSPAC.LT.IW) THEN
C                 adjust number of timesteps to be run to get 
C                 to next mfact special action
                  IW = INSPAC
                END IF
C
C                WRITE(99,2030)
C     1            'OSUPER:width,ivls,runwid,iw,inspac',
C     1                    WIDTH,IVLS,RUNWID,IW,INSPAC
C
C               determine whether or not this is the first or last
C               time through operation loop
                FSTCAL= 0
                LSTCAL= 0
C               WRITE(99,*)'xcount,xrepet,count,last,stivl,width,iw',
C     #                      XCOUNT,XREPET,COUNT,LAST,STIVL,WIDTH,IW
                IF ( (XCOUNT .EQ. 1) .AND. (COUNT .EQ. 1) .AND.
     #               (STIVL .EQ. 1) ) THEN
                  FSTCAL= 1
C                  WRITE(99,*)'fstcal'
                END IF
C
                IF ( (XCOUNT .EQ. XREPET) .AND.(COUNT .EQ. LAST) .AND.
     #               ((STIVL+ IW) .GT. WIDTH) ) THEN
                  LSTCAL= 1
C                  WRITE(99,*)'lstcal'
                END IF
C             
C               opn loop
                OPN = 0
 70             CONTINUE
                  OPN = OPN+ 1
C
C                 write (99,*) '  opn,opkey:',OPN,OPKEY
                  CALL COPYI (I8,OSUPM(1,OPKEY),
     O                        OPINF)
C
C                 WRITE (MESSU,2010) OPINF
C
                  STATFG= STATFG+ 1
                  IF (STATFG .LE. NOPNS) THEN
C                   show status for pc version
                    IOPT = 5
                    CALL HSPSTA (IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO)
C                   WRITE(99,*) 'OSUPER,HSPSTA:',OPN,NOPNS,LAST,COUNT
                  END IF
C
                  IF (RUNWID .EQ. 0) THEN
C                   need to get all time series for operation
                    EXTFG= 0
                  ELSE
C                   external done, just get internal
                    EXTFG= 2
                  END IF
C
C                 get time series data not already done
                  IF (TSGKST.GT.0) THEN
                    CALL TSGET (FILES,TSGKST,TSGKND,
     I                          DELT,STIVL,IW,FSTCAL,EXTFG)
                    IF (FSTCAL .EQ. 1) THEN
                       OPCODE = OPCODE
C                     details first time thru
C                      write(99,2030)
C     1                  'OSUPER:TSGET:omc,optno,kst,knd,stivl,iw',
C     1                  OMCODE,OPTNO,TSGKST,TSGKND,STIVL,IW
                    END IF
                  END IF
C
C                 read the osv from disc
                  CALL GETOSV (OSVKST,OSVKND,MAXOSV,
     O                         OSV)
C
C                 call the appropriate operating module
C                 casentry omcode
C                  write(99,*)'omcode,opn,nopns',omcode,opn,nopns
                  GO TO (110,120,130,140,150,160,170,180,190,200,210),
     $                   OMCODE
C                   case 1
 110                  CONTINUE
                        CALL PERLND (STIVL,IW)
                      GO TO 300
C
C                   case 2
 120                  CONTINUE
                        CALL IMPLND (STIVL,IW)
                      GO TO 300
C
C                   case 3
 130                  CONTINUE
                        CALL RCHRES (STIVL,IW,FSTCAL)
                      GO TO 300
C
C                   case 4
 140                  CONTINUE
                        CALL COPY (STIVL,IW)
                      GO TO 300
C
C                   case 5
 150                  CONTINUE
                        CALL PLTGEN (STIVL,IW,LSTCAL)
                      GO TO 300
C
C                   case 6
 160                  CONTINUE
                        CALL HDISPL (STIVL,IW,LSTCAL)
                      GO TO 300
C
C                   case 7
 170                  CONTINUE
                        CALL DURANL (STIVL,IW,FSTCAL,LSTCAL)
                      GO TO 300
C
C                   case 8
 180                  CONTINUE
                        CALL GENER (STIVL,IW)
                      GO TO 300
C
C                   case 9
 190                  CONTINUE
                        CALL MUTSIN (STIVL,IW)
                      GO TO 300
C
C                   case 10
 200                  CONTINUE
                        CALL BMPRAC (STIVL,IW)
                      GO TO 300
C
C                   case 11
 210                  CONTINUE
                        CALL REPORT (STIVL,IW,LSTCAL)
                      GO TO 300
 300              CONTINUE
C                 endcase
C
C                 write the osv to disc
                  CALL PUTOSV (OSVKST,OSVKND,MAXOSV,OSV)
C
                  IF (RUNWID .EQ. 0) THEN
C                   put all time series
                    EXTFG= 0
                  ELSE
C                   only put internal time series
                    EXTFG= 2
                  END IF
C
                  IF (TSPKST.GT.0) THEN
C                   output time series from the inpad
C                    WRITE(99,2030) 'OSUPER:TSPUT:',TSPKST
                    CALL TSPUT (FILES,TSPKST,TSPKND,
     I                          DELT,STIVL,IW,FSTCAL,LSTCAL,EXTFG)
                  END IF
C
C                 increment key for osupfl
                  OPKEY = OPKEY+ 1
C
C                 check for external cancel command
                  RETCOD= CKUSER ()
C
C               end of operation loop
                IF (RETCOD.EQ.0 .AND. OPN .LT.NOPNS) GO TO 70
C
                IVLS  = IVLS - IW
                IVLDON= IVLDON+ IW
                IF (RUNWID .GT. 0) THEN
                  STIVL = STIVL+ IW
                END IF
C                WRITE (99,2030)'OSUPER:updated:ivls,stivl,iw',
C     1                                         IVLS,STIVL,IW
C
C               status file write
                IF (STFIL .NE. 0) THEN
C                 status file specified
C                 WRITE(99,*) 'HSPF:write status file'
                  CALL PSTVFL (STFIL,IVLDON,DELT,NOPNS,LSTCAL,MESSU,
     I                         MSGFL,
     M                         STECNT)
                END IF
C
C               update current time
                CALL TIMADD (CURDAT,I2,DELT,IW,NXTDAT)
                CALL COPYI (I6,NXTDAT,CURDAT)
C                WRITE(99,2020) "OSUPER:end interval loop:CURDAT:",
C     1                                                   CURDAT
C
C             end of interval loop
              IF (IVLS.GT.0 .AND. RETCOD.EQ.0) GO TO 60
C
              IF (RUNWID .GT. 0) THEN
C               put all external time series
C                WRITE(99,2030) 'OSUPER:put external time series'
                EXTFG= 1
                DO 310 PKEY= FOPKEY, LOPKEY
                  CALL COPYI (I8,OSUPM(1,PKEY),
     O                        OPINF)
                  IF (TSPKST.GT.0) THEN
                    CALL TSPUT (FILES,TSPKST,TSPKND,
     $                          DELT,I1,WIDTH,FSTCAL,LSTCAL,EXTFG)
                  END IF
 310            CONTINUE
              END IF
C
C              WRITE(99,2030) 'OSUPER:end inpsan loop:COUNT,LAST,RETCOD',
C     1                                               COUNT,LAST,RETCOD 
C
C           end of inspan loop
            IF (COUNT.LT.LAST .AND. RETCOD.EQ.0) GO TO 40
C           get key for start of next ingroup
            INKEY= OPKEY
C
C            WRITE(99,2030) 'OSUPER:end ingroup loop:GRP,RETCOD',
C     1                                              GRP,RETCOD
C         end of ingroup loop
          IF (GRP.LT.NGRPS .AND. RETCOD.EQ.0) GO TO 30
C
C       end of exspan loop
        IF (XCOUNT.LT.XREPET .AND. RETCOD.EQ.0) GO TO 20
C       get key for start of next exgroup
        EXKEY= OPKEY
C
C     end of exgroup loop
      IF (XGRP.LT.NXGRPS .AND. RETCOD.EQ.0) GO TO 10
C
      RETURN
      END
