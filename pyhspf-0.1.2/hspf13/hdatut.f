C
C     4.02
C
      SUBROUTINE   ADDTIM
     I                   (DMIN,NDAY,PIVL,PYREND,
     M                    DATIM,PIVLNO,
     O                    NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,
     O                    EMONFG,EPYRFG)
C
C     + + + PURPOSE + + +
C     Add a specified interval (day/hour/min) to a given date/time to
C     obtain a new date/time.  Also, set time-related flags which
C     application modules need.  Dates and times are in internal format.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DATIM(5),DAYFG,DMIN,EDAYFG,EMONFG,EPYRFG,HRFG,
     $           NDAY(12),NDAYS,NXTMON,PIVL,PIVLNO,PYREND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DMIN   - ???
C     NDAY   - ???
C     PIVL   - ???
C     PYREND - ???
C     DATIM  - date and time of day
C     PIVLNO - ???
C     NDAYS  - no. of days in this month
C     NXTMON - next calendar month
C     HRFG   - ???
C     DAYFG  - flag for first day or day change
C     EDAYFG - ???
C     EMONFG - ???
C     EPYRFG - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DAY,HR,MIN,MON,TDAY,THRS,TMINS,YR
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMNH
C
C     + + + END SPECIFICATIONS + + +
C
      HRFG  = 0
      DAYFG = 0
      EDAYFG= 0
      EMONFG= 0
      EPYRFG= 0
      YR    = DATIM(1)
      MON   = DATIM(2)
      DAY   = DATIM(3)
      HR    = DATIM(4)
      MIN   = DATIM(5)
C
      IF (PIVLNO .EQ. PIVL) THEN
        PIVLNO= 0
      END IF
C
      PIVLNO= PIVLNO + 1
      TMINS = MIN + DMIN
      THRS  = (TMINS - 1)/60
      MIN   = TMINS - THRS*60
C
      IF (THRS .GT. 0) THEN
C       hour has changed
        HRFG= 1
        THRS= THRS + HR
        TDAY= (THRS - 1)/24
        HR  = THRS - TDAY*24
C
        IF (TDAY .GT. 0) THEN
C         day has changed
          DAYFG= 1
          TDAY = TDAY + DAY
          NDAYS= DAYMNH(YR,MON,NDAY)
C
          IF (TDAY .GT. NDAYS) THEN
C           month has changed
C           dountil tday<= ndays
 10         CONTINUE
              TDAY= TDAY - NDAYS
              IF (MON .EQ. 12) THEN
                YR = YR + 1
                MON= 1
              ELSE
                MON= MON + 1
              END IF
              NDAYS= DAYMNH(YR,MON,NDAY)
C           end dountil
            IF (TDAY .GT. NDAYS) GO TO 10
C
            IF (MON .EQ. 12) THEN
              NXTMON= 1
            ELSE
              NXTMON= MON + 1
            END IF
C
          END IF
          DAY= TDAY
        END IF
C
      END IF
C
C     set printout flags
      IF (MIN .EQ. 60 .AND. HR .EQ. 24) THEN
C       last interval of the day
        EDAYFG= 1
C
        IF (DAY .EQ. NDAYS) THEN
C         last interval of the month
          EMONFG= 1
          IF (MON .EQ. PYREND) THEN
C           last interval in printout year
            EPYRFG= 1
          END IF
C
        END IF
C
      END IF
C
      DATIM(1)= YR
      DATIM(2)= MON
      DATIM(3)= DAY
      DATIM(4)= HR
      DATIM(5)= MIN
C
      RETURN
      END
C
C     1.2.10
C
      INTEGER FUNCTION   DAYMNH
     I                           (YR,MON,NDAMON)
C
C     + + + PURPOSE + + +
C     Find the no. of days in a given year and month, allowing for the
C     case of a leap year.  NDAMON(*) contains the no. of days in each
C     month of the calendar year.  The value for February is not used.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MON,NDAMON(12),YR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - ???
C     MON    - calendar month
C     NDAMON - no. of days in each month of calendar year
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I4,I100,I400
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      I100 = 100
      I400 = 400
C
      IF (MON .NE. 2) THEN
C       no problem
        DAYMNH= NDAMON(MON)
      ELSE
C       check whether this is a leap year
        IF (MOD(YR,I100) .EQ. 0) THEN
C         on a century boundary
          IF (MOD(YR,I400) .EQ. 0) THEN
C           on a 400 year boundary
            DAYMNH= 29
          ELSE
            DAYMNH= 28
          END IF
        ELSE
          IF (MOD(YR,I4) .EQ. 0) THEN
C           leap year
            DAYMNH= 29
          ELSE
            DAYMNH= 28
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C     1.2.22
C
      SUBROUTINE   DIFTIM
     I                  (SDATIM,EDATIM,NDAMON,
     O                   DIFMIN)
C
C     + + + PURPOSE + + +
C     Find the difference, in minutes, between an ending and a starting
C     date/time (both given in internal format)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIFMIN,EDATIM(5),
     $           NDAMON(12),SDATIM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     DIFMIN - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DDATIM(5),EYR,EYROFF,SYR,SYROFF,YR,YROFF
C
C     + + + EXTERNAL + + +
      EXTERNAL   YRMINS
C
C     + + + END SPECIFICATIONS + + +
C
      SYR= SDATIM(1)
      EYR= EDATIM(1)
C
      IF (SYR .GT. EYR) THEN
C     dummy value
        DIFMIN= -9999
      ELSE
C       find the offset of the starting date/time from the start of
C       it's year
        CALL YRMINS
     I              (SDATIM,NDAMON,
     O               SYROFF)
C       find the offset of the ending date/time from the start of its
C       year
        CALL YRMINS
     I              (EDATIM,NDAMON,
     O               EYROFF)
        DIFMIN= EYROFF- SYROFF
C
C       add in the length of any intervening years
        YR= SYR
C       whiledo yr< eyr
 20     CONTINUE
        IF (YR .LT. EYR) THEN
C         ddatim is the end of year yr
          DDATIM(1)= YR
          DDATIM(2)= 12
          DDATIM(3)= 31
          DDATIM(4)= 24
          DDATIM(5)= 60
C
          CALL YRMINS
     I                (DDATIM,NDAMON,
     O                 YROFF)
          DIFMIN= DIFMIN+ YROFF
          YR= YR+ 1
          GO TO 20
        END IF
      END IF
C
      RETURN
      END
C
C     1.2.21
C
      INTEGER FUNCTION   DYPMON
     I                          (YEAR,MON)
C
C     + + + PURPOSE + + +
C     Return days per month
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    YEAR,MON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - ???
C     MON    - calendar month
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    ND(12),FLAG
C
C
C     + + + EXTERNAL + + +
      EXTERNAL   LPYEAR
C
C     + + + END SPECIFICATIONS + + +
C
      ND(1)  = 31
      ND(2)  = 28
      ND(3)  = 31
      ND(4)  = 30
      ND(5)  = 31
      ND(6)  = 30
      ND(7)  = 31
      ND(8)  = 31
      ND(9)  = 30
      ND(10) = 31
      ND(11) = 30
      ND(12) = 31
C
      CALL LPYEAR
     I            (YEAR,
     O             FLAG)
      IF (FLAG .EQ. 1) THEN
        ND(2) = 29
      END IF
C
      DYPMON = ND(MON)
C
      RETURN
      END
C
C     3.01.3
C
      SUBROUTINE ENDATE
     I                  (NDAMON,MESSU,MSGFL,
     M                   ECOUNT,EDATIM)
C
C     + + + PURPOSE + + +
C     Check that a supplied ending date/time is valid and convert it to
C     internal form
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,EDATIM(5),MSGFL,
     $           MESSU,NDAMON(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDAMON - no. of days in each month of calendar year
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     ECOUNT - count(s) of specific errors
C     EDATIM - ending date/time
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DAY,ERRFG,HR,MIN,MON,YR,SCLU,SGRP,NDAYS
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMNH,OMSG,OMSTI,HDATIN
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 204
      YR  = EDATIM(1)
      MON = EDATIM(2)
      DAY = EDATIM(3)
      HR  = EDATIM(4)
      MIN = EDATIM(5)
C
C     if mon, day, or hr are zero, assume user left the field blank -
C     substitute default values
      IF (MON.EQ.0) MON= 12
C
C     validate the "supplied" date/time
      ERRFG= 0
      IF (YR.GE.1) GO TO 10
        ERRFG= 1
        YR   = 1
        GO TO 30
 10   CONTINUE
        IF (YR.LE.32767) GO TO 20
          ERRFG= 1
          YR   = 32767
 20     CONTINUE
 30   CONTINUE
C
      IF (MON.GE.1) GO TO 40
        ERRFG= 1
        MON  = 1
        GO TO 60
 40   CONTINUE
        IF (MON.LE.12) GO TO 50
          ERRFG= 12
          MON  = 12
 50     CONTINUE
 60   CONTINUE
C
C     find out how many days there are in this month
      NDAYS= DAYMNH(YR,MON,NDAMON)
      IF (DAY.EQ.0) DAY= NDAYS
      IF (HR.EQ.0.AND.MIN.EQ.0) HR= 24
C
      IF (DAY.GE.1) GO TO 70
        ERRFG= 1
        DAY  = 1
        GO TO 90
 70   CONTINUE
        IF (DAY.LE.NDAYS) GO TO 80
          ERRFG= 1
          DAY  = NDAYS
 80     CONTINUE
 90   CONTINUE
C
      IF (HR.GE.0) GO TO 100
        ERRFG= 1
        HR   = 0
        GO TO 120
 100  CONTINUE
        IF (HR.LE.24) GO TO 110
          ERRFG= 1
          HR   = 24
 110    CONTINUE
 120  CONTINUE
C
      IF (MIN.GE.0) GO TO 130
        ERRFG= 1
        MIN  = 0
        GO TO 150
 130  CONTINUE
        IF (MIN.LE.59) GO TO 140
          ERRFG= 1
          MIN  = 59
 140    CONTINUE
 150  CONTINUE
C
      IF (ERRFG.NE.1) GO TO 160
C       error - supplied ending date/time is invalid
        CALL OMSTI (EDATIM(1))
        CALL OMSTI (EDATIM(2))
        CALL OMSTI (EDATIM(3))
        CALL OMSTI (EDATIM(4))
        CALL OMSTI (EDATIM(5))
        SGRP = 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 160  CONTINUE
C
C     convert to internal representation
      CALL HDATIN
     M           (YR,MON,DAY,HR,MIN)
C
      EDATIM(1)= YR
      EDATIM(2)= MON
      EDATIM(3)= DAY
      EDATIM(4)= HR
      EDATIM(5)= MIN
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDATIN
     M                   (YR,MON,DAY,HR,MIN)
C
C     + + + PURPOSE + + +
C     convert date to hspf internal format
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR,MON,DAY,HR,MIN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PREMON,NDAYS
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      HR= HR+ 1
C
      IF (MIN.EQ.0) THEN
C       on an hour boundary
        MIN= 60
        HR = HR- 1
C
        IF (HR.EQ.0) THEN
C         on a day boundary
          HR = 24
          DAY= DAY- 1
C
          IF (DAY.EQ.0) THEN
C           on a month boundary
            IF (MON.GT.1) THEN
              PREMON= MON- 1
            ELSE
              PREMON= 12
            END IF
C
C           find no. of days in "previous" month
            NDAYS= DAYMON(YR,PREMON)
            DAY  = NDAYS
            MON  = PREMON
C
            IF (MON.EQ.12) THEN
C             on a year boundary
              YR= YR- 1
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C     1.2.15
C
      SUBROUTINE   EXDATE
     I                   (IDATIM,
     O                   EXDAT)
C
C     + + + PURPOSE + + +
C     Convert a date/time from internal to standard external format.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IDATIM(5),EXDAT(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDATIM - internal year/mon/day/hr/min
C     EXDAT  -  external date format
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, 5
        EXDAT(I)= IDATIM(I)
 10   CONTINUE
C
      IF (IDATIM(5) .EQ.  60) THEN
        EXDAT(5)= 0
      ELSE
        EXDAT(4)= IDATIM(4) - 1
      END IF
C
      RETURN
      END
C
C     4.1.07
C
      SUBROUTINE FDATIM
     I                 (TIME,YEAR,TYREND,DATIM)
C
C     + + + PURPOSE + + +
C     Find date/time from time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TIME,YEAR,TYREND,DATIM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TIME   - ???
C     YEAR   - ???
C     TYREND - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INC,DMON(12),YR,MN,DY,HR,MIN,DYOFYR,MNOFDY,
     $          YRLEN,TOFF,MINPDY,DLIM
C
C     + + + EXTERNALS + + +
      EXTERNAL  LPYEAR
C
C     + + + END SPECIFICATIONS + + +
C
      MINPDY=1440
C     set values dependent on year length
      CALL LPYEAR (YEAR,   INC)
      YRLEN = 525600
      IF (INC.EQ.1) YRLEN = YRLEN + 1440
C
C     compute minute of year
      TOFF = TIME- (TYREND- YRLEN)
C
C     compute day of year and minute of day
      DYOFYR = (TOFF+1439)/1440
      MNOFDY = TOFF - MINPDY*(DYOFYR-1)
C
C     compute month of year and day of month
      DMON(1)  = 31
      DMON(2)  = 28 + INC
      DMON(3)  = 31
      DMON(4)  = 30
      DMON(5)  = 31
      DMON(6)  = 30
      DMON(7)  = 31
      DMON(8)  = 31
      DMON(9)  = 30
      DMON(10) = 31
      DMON(11) = 30
      DMON(12) = 31
C
      MN   = 1
      DLIM = DMON(MN)
      DY   = DYOFYR
   70 IF (DYOFYR .LE. DLIM) GO TO 80
        DY   = DYOFYR- DLIM
        MN   = MN+ 1
        DLIM = DLIM+ DMON(MN)
        GO TO 70
   80 CONTINUE
C     mn gives month of year, dy gives day of month
C
C     compute hour of day and minute of hour - internal form
      YR = YEAR
      IF (MNOFDY .NE. 0) GO TO 130
        DY  = DY- 1
        HR  = 24
        MIN = 60
        IF (DY .NE. 0) GO TO 120
          MN = MN- 1
          IF (MN.NE. 0) GO TO 90
            MN = 12
            YR = YR- 1
   90     CONTINUE
          IF (MN .NE. 2) GO TO 100
            DY = 28+ INC
            GO TO 110
  100     CONTINUE
            DY = DMON(MN)
  110     CONTINUE
  120   CONTINUE
        GO TO 140
  130 CONTINUE
        HR  = (MNOFDY+59)/60
        MIN = MNOFDY- 60*(HR-1)
  140 CONTINUE
C
      DATIM(1) = YR
      DATIM(2) = MN
      DATIM(3) = DY
      DATIM(4) = HR
      DATIM(5) = MIN
C
      RETURN
      END
C
C     3.5.8.1.1
C
      SUBROUTINE FIRINT
     I                  (DT,START,
     O                   STRINT)
C
C     + + + PURPOSE + + +
C     Compute the date/time,strint, of the interval of length dt
C     which starts at or immediately before the date/time in starts.
C     1 <= dt <= 1440
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DT,START(5),STRINT(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DT     - ???
C     START  - ???
C     STRINT - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   YR,MON,DAY,MIN,DDAY,HR,NDMON,DYPMON
C
C     + + + EXTERNALS + + +
      EXTERNAL  DYPMON
C     + + + END SPECIFICATIONS + + +
C
      YR =START(1)
      MON=START(2)
      DAY=START(3)
C
C     compute minute of the day
      MIN=(START(4)-1)*60 +START(5)
C
C     discard fractional part of dt
      MIN=((MIN+DT)/DT)*DT
C
C     convert to normal form
      DDAY=MIN/1440
      MIN =MIN-DDAY*1440
      IF (MIN .NE. 0) GO TO 10
        MIN =1440
        DDAY=DDAY-1
 10   CONTINUE
      HR =MIN/60
      MIN=MIN-HR*60
      IF (MIN .NE. 0) GO TO  20
        MIN=60
        GO TO  30
 20   CONTINUE
        HR=HR +1
 30   CONTINUE
C
      DAY  =DAY +DDAY
      NDMON=DYPMON(YR,MON)
      IF (DAY .LE. NDMON) GO TO 50
        DAY=DAY-NDMON
        MON=MON +1
        IF (MON .LE. 12) GO TO 40
          MON=1
          YR =YR +1
 40     CONTINUE
 50   CONTINUE
C
      STRINT(1)=YR
      STRINT(2)=MON
      STRINT(3)=DAY
      STRINT(4)=HR
      STRINT(5)=MIN
C
      RETURN
      END
C
C     1.2.18
C
      SUBROUTINE   LPYEAR
     I                  (YEAR,
     O                  LPYRFG)
C
C     + + + PURPOSE + + +
C     Returns a leap year flag, lpyrfg, that is on if the year is a
C     leap year.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LPYRFG,YEAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - ???
C     LPYRFG - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I4,I100,I400
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      I100 = 100
      I400 = 400
C
      IF ( MOD(YEAR,I100) .EQ. 0) THEN
C       on a century boundary
        IF ( MOD(YEAR,I400) .EQ. 0) THEN
C         on a 400 year boundary
          LPYRFG= 1
        ELSE
          LPYRFG= 0
        END IF
      ELSE
        IF ( MOD(YEAR,I4) .EQ. 0) THEN
C         leap year
          LPYRFG= 1
        ELSE
          LPYRFG= 0
        END IF
      END IF
C
      RETURN
      END
C
C     3.01.2
C
      SUBROUTINE STDATE
     I                  (NDAMON,MESSU,MSGFL,
     M                   ECOUNT,SDATIM)
C
C     + + + PURPOSE + + +
C     Check that a supplied starting date/time is valid and convert it
C     to internal form.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECOUNT,MSGFL,MESSU,
     $           NDAMON(12),SDATIM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDAMON - no. of days in each month of calendar year
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     ECOUNT - count(s) of specific errors
C     SDATIM - starting date/time
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DAY,ERRFG,HR,MIN,MON,NDAYS,YR,SCLU,SGRP
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMNH,OMSG,OMSTI,HDATIN
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 204
      YR  = SDATIM(1)
      MON = SDATIM(2)
      DAY = SDATIM(3)
      HR  = SDATIM(4)
      MIN = SDATIM(5)
C
C     if mon and/or day are zero, assume user left field blank -
C     supply the value 1
      IF (MON.EQ.0) MON= 1
      IF (DAY.EQ.0) DAY= 1
C
C     validate the "supplied" date/time
      ERRFG= 0
      IF (YR.GE.1) GO TO 10
        ERRFG= 1
        YR   = 1
        GO TO 30
 10   CONTINUE
        IF (YR.LE.32767) GO TO 20
          ERRFG= 1
          YR   = 32767
 20     CONTINUE
 30   CONTINUE
C
      IF (MON.GE.1) GO TO 40
        ERRFG= 1
        MON  = 1
        GO TO 60
 40   CONTINUE
        IF (MON.LE.12) GO TO 50
          ERRFG= 1
          MON  = 12
 50     CONTINUE
 60   CONTINUE
C
C     find no. of days in this month
      NDAYS= DAYMNH(YR,MON,NDAMON)
      IF (DAY.GE.1) GO TO 70
        ERRFG= 1
        DAY  = 1
        GO TO 90
 70   CONTINUE
        IF (DAY.LE.NDAYS) GO TO 80
          ERRFG= 1
          DAY  = NDAYS
 80     CONTINUE
 90   CONTINUE
C
      IF (HR.GE.0) GO TO 100
        ERRFG= 1
        HR   = 0
        GO TO 120
 100  CONTINUE
        IF (HR.LE.23) GO TO 110
          ERRFG= 1
          HR   = 23
 110    CONTINUE
 120  CONTINUE
C
      IF (MIN.GE.0) GO TO 130
        ERRFG= 1
        MIN  = 0
        GO TO 150
 130  CONTINUE
        IF (MIN.LE.59) GO TO 140
          ERRFG= 1
          MIN  = 59
 140    CONTINUE
 150  CONTINUE
C
      IF (ERRFG.NE.1) GO TO 160
C       error - supplied starting date/time is invalid
        CALL OMSTI (SDATIM(1))
        CALL OMSTI (SDATIM(2))
        CALL OMSTI (SDATIM(3))
        CALL OMSTI (SDATIM(4))
        CALL OMSTI (SDATIM(5))
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 160  CONTINUE
C
C     convert to internal representation
      CALL HDATIN
     M           (YR,MON,DAY,HR,MIN)
C
      SDATIM(1)= YR
      SDATIM(2)= MON
      SDATIM(3)= DAY
      SDATIM(4)= HR
      SDATIM(5)= MIN
C
      RETURN
      END
C
C     1.2.19
C
      SUBROUTINE   TDIF
     I                (BYRI,YRI,MNI,DYI,
     O                 DIFF)
C
C     + + + PURPOSE + + +
C     Compute difference in minutes from the start of
C     the base year, byr, until the start of the day dy
C     in month mn and year yr.  use julian date formula
C     given by almanac for computers for the year 1978,
C     nautical almanac office, united states naval ob-
C     servatory, washington, d.c. 20390
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    BYRI,YRI,MNI,DYI,DIFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BYRI   - base year
C     YRI    - year
C     MNI    - month
C     DYI    - day
C     DIFF   - difference in minutes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    MJDB,MJD
C
C     + + + END SPECIFICATIONS + + +
C
      IF (BYRI .GT. 1900 .AND. YRI .GT. 1900) THEN
        DIFF = 1440*(367*(YRI-BYRI)+ DYI- 1-
     $    (7*(YRI+ (MNI+9)/12))/4+ 275*MNI/9+
     $    7*BYRI/4- 30)
      ELSE
C       more complex case - 1900 was not a leap year
        MJDB = 367*BYRI- 7*BYRI/4+ 1
        IF (BYRI .LE. 1900) THEN
          MJDB = MJDB+ 1
        END IF
C
        MJD = 367*YRI- 7*(YRI+ (MNI+9)/12)/4+
     $    275*MNI/9+ DYI
        IF (100*YRI+ MNI .LE. 190002) THEN
          MJD = MJD+ 1
        END IF
C
        DIFF = 1440*(MJD-MJDB)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE TIMHTW
     I                   (MESSU,MSGFL,
     M                    DATIM)
C
C     + + + PURPOSE + + +
C     Convert hspf internal time to wdms time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MESSU,MSGFL,DATIM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DUMCNT,SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTI,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU    = 204
      DATIM(6)= 0
C
      IF (DATIM(5) .EQ. 60) THEN
C       hour boundary
        DATIM(5) = 0
C
      ELSE
C       not on an hour boundary
        DATIM(4) = DATIM(4) - 1
        IF (DATIM(4) .LT. 0) THEN
          CALL OMSTI (DATIM(1))
          CALL OMSTI (DATIM(2))
          CALL OMSTI (DATIM(3))
          CALL OMSTI (DATIM(4))
          CALL OMSTI (DATIM(5))
          SGRP  = 3
          DUMCNT= 0
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               DUMCNT)
        END IF
      END IF
C
      RETURN
      END
C
C     3.01.1
C
      SUBROUTINE YRMINS
     I                  (DATIM,NDAMON,
     O                   YROFF)
C
C     + + + PURPOSE + + +
C     Find the offset (in mins) of a given date/time from the start of
C     that year
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DATIM(5),NDAMON(12),YROFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATIM  - date and time of day
C     NDAMON - no. of days in each month of calendar year
C     YROFF  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DAY,DAY1,HR,M,MIN,MON,NDAYS,YR
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMNH
C
C     + + + END SPECIFICATIONS + + +
C
      YR = DATIM(1)
      MON= DATIM(2)
      DAY= DATIM(3)
      HR = DATIM(4)
      MIN= DATIM(5)
C
      YROFF= 0
      M    = 1
C     whiledo m< mon
 10   IF (M.GE.MON) GO TO 20
        NDAYS= DAYMNH(YR,M,NDAMON)
        YROFF= YROFF+ 1440*NDAYS
        M    = M+ 1
        GO TO 10
 20   CONTINUE
C
      DAY1 = DAY - 1
      YROFF= YROFF+ (DAY1)*1440+ (HR-1)*60 + MIN
C
      RETURN
      END
C
C     3.5.8.03
C
      SUBROUTINE YROFF
     I                 (DATIM,
     O                  TOFF)
C
C     + + + PURPOSE + + +
C     Compute offset in minutes from start of calendar year
C     given by datim(1) to the date/time given by datim
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATIM(5),TOFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATIM  - date and time of day
C     TOFF   - ???
C
C     + + + EXTERNALS + + +
      EXTERNAL   TDIF
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TDIF(DATIM(1),DATIM(1),DATIM(2),DATIM(3),  TOFF)
C
C     add in the hour/minute information
      TOFF = TOFF+ (DATIM(4)-1)*60 + DATIM(5)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TDIFX
     I                  (BYRI,DATEI,
     O                   DIFF)
C
C     + + + PURPOSE + + +
C     Compute difference in minutes from the start of
C     the base year until the start of the given date
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    BYRI,DATEI(5),DIFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BYRI   - base year
C     DATEI  - date to calc difference from
C     DIFF   - difference in minutes
C
C     + + + EXTERNALS + + +
      EXTERNAL   TDIF
C
C     + + + END SPECIFICATIONS + + +
C
C     calc whole day difference
      CALL TDIF (BYRI,DATEI(1),DATEI(2),DATEI(3),
     O           DIFF)
C     add hours and minutes difference
      DIFF = DIFF+ DATEI(4)*60 + DATEI(5)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MJDATE
     I                    (EXDAT,
     O                     JTIME,DYFRAC)
C
C     + + + PURPOSE + + +
C     Convert external format integer date to modified julian date and
C     fraction of the day.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EXDAT(5)
      DOUBLE PRECISION JTIME,DYFRAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXDAT  - external date format
C     JTIME  - modified julian date and time of day
C     DYFRAC - fraction of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,A,B,Y,M,MJD,XDAT(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL COPYI,TIMCVT
C
C     + + + INTRINSICS + + +
      INTRINSIC INT,DBLE
C
C     + + + END SPECIFICATIONS + + +
C
      I= 5
      CALL COPYI (I,EXDAT,
     O            XDAT)
      XDAT(6)= 0
C     convert to midnight convention of 00:00
      CALL TIMCVT
     M            (XDAT)
C
      IF (XDAT(2) .GT. 2) THEN
C       use actual values for March-December
        Y= XDAT(1)
        M= XDAT(2)
      ELSE
C       use artificial values for Jan-Feb
        Y= XDAT(1)- 1
        M= XDAT(2)+ 12
      END IF
C
C     set up other values
      A= Y/100
      B= 2 - A + A/4
C
C     compute julian date
      MJD= (36525*Y)/100+ INT (30.6001*(M+ 1))+ XDAT(3)+ B- 679006
C
C     convert to double precision
      DYFRAC= DBLE (XDAT(4))/24.D0+ DBLE (XDAT(5))/1440.D0
      JTIME= DBLE (MJD)+ DYFRAC
C
      RETURN
      END
