C     utdate.f 2.1 9/4/91
C
C
C
      SUBROUTINE   CKDATE
     I                    (DATE1, DATE2,
     O                     FLAG)
C
C     + + + PURPOSE + + +
C     Determine the calendar order of two dates.  The dates are
C     assumed to be valid.
C     Examples:      DATE1               DATE2         FLAG
C              1980/10/1 00:00:00  1980/10/1 24:00:00   -1
C              1980/10/1 24:00:00  1980/10/1 00:00:00    1
C              1980/10/1 24:00:00  1980/10/2 00:00:00   -1
C              1980/10/2 00:00:00  1980/10/1 24:00:00    1
C              1980/10/1 24:00:00  1980/10/1 24:00:00    0
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6), DATE2(6), FLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first date
C     DATE2  - second date
C     FLAG   - flag indicating order of dates
C               1 - DATE1 follows DATE2
C               0 - DATE1 is the same date as DATE2
C              -1 - DATE2 follows DATE1
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + END SPECIFICATIONS + + +
C
      FLAG = -99
      I = 0
C
 10   CONTINUE
        I = I + 1
C
        IF (DATE1(I) .LT. DATE2(I)) THEN
C         first date is before second date
          FLAG = -1
        ELSE
C         first date follows or equals second date
          IF (DATE1(I) .GT. DATE2(I)) THEN
C           first date follows second date
            FLAG = 1
          ELSE IF (I .EQ. 6) THEN
C           all parts of dates are equal
            FLAG = 0
          END IF
        END IF
C
      IF (I.LT.6 .AND. FLAG.EQ.-99) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CMPTIM
     I                   ( TCODE1, TSTEP1, TCODE2, TSTEP2,
     O                     TSTEPF, TCDCMP )
C
C     + + + PURPOSE + + +
C     Compare one time unit and step to a second time unit and
C     step.  Two flags are returned.  The first flag indicates
C     compatible/incompatible time steps.  The second flag
C     indicates which time step is smaller.  Time steps are
C     considered compatible if one is an even multiple of the
C     other.  One hour and 30 minutes are compatible; one hour
C     and 90 minutes are incompatible.  Comparison of time units
C     and time steps which cross the day-month boundry are handled
C     a little different.  If the smaller time step is a day or
C     less and is compatible with 1 day and the larger time step
C     is compatible with one month, than the smaller and the
C     larger time steps are considered to be compatible.  The time
C     step of a day or less will be considered to be the smaller
C     time step.
C     EXAMPLES:  TCODE1 TSTEP1 TCODE2 TSTEP2 TSTEPF TCDCMP
C                  3      1      2      60     0      0
C                  3      1      2      90     1      1
C                  3      1      2      30     0      2
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  TCODE1, TSTEP1, TCODE2, TSTEP2, TSTEPF, TCDCMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TCODE1 - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C     TSTEP1 - time step, in TCODE1 units
C     TCODE2 - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C     TSTEP2 - time step in TCODE2 units
C     TSTEPF - time step compatability flag
C              0 - compatible time steps
C              1 - incompatible time steps
C     TCDCMP - flag indicating order of time steps
C               0 - time steps are the same
C               1 - first time step is smaller
C               2 - second time step is smaller
C              -1 - time units span day-month boundry
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TC(2), TS(2), TSX, TCX, TSFX(2), TCDX(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   CMPTM2
C
C     + + + END SPECIFICATIONS + + +
C
      TC(1) = TCODE1
      TC(2) = TCODE2
      TS(1) = TSTEP1
      TS(2) = TSTEP2
C
      IF (TC(1) .LT. 1  .OR.  TC(1) .GT. 6     .OR.
     >    TC(2) .LT. 1  .OR.  TC(2) .GT. 6     .OR.
     >    TS(1) .LT. 1  .OR.  TS(1) .GT. 1440  .OR.
     >    TS(2) .LT. 1  .OR.  TS(2) .GT. 1440) THEN
C       an invalid time units code or time step
        TSTEPF = 1
        TCDCMP = -1
      ELSE IF ((TC(1) .LE. 4  .AND.  TC(2) .GE. 5)  .OR.
     >         (TC(2) .LE. 4  .AND.  TC(1) .GE. 5)) THEN
C       special case for time units that cross day-month boundry
        TSTEPF = 1
        TCDCMP = -1
        IF (TC(1) .LE. 4) THEN
C         first time unit is day or smaller, second is month or larger
          TSX = 1
          TCX = 4
          CALL CMPTM2 ( TC(1), TS(1), TCX, TSX, TSFX(1), TCDX(1) )
          TSX = 1
          TCX = 5
          CALL CMPTM2 ( TC(2), TS(2), TCX, TSX, TSFX(2), TCDX(2) )
          IF (TSFX(1) .EQ. 0  .AND.  TSFX(2) .EQ. 0) THEN
C           times compatible with boundaries
            IF ((TCDX(1) .EQ. 0  .OR.  TCDX(1) .EQ. 1)  .AND.
     >          (TCDX(2) .EQ. 0  .OR.  TCDX(2) .EQ. 2)) THEN
C             smaller time a day or less, larger time a month or more
              TSTEPF = 0
              TCDCMP = 1
            END IF
          END IF
        ELSE
C         second time unit is day or smaller, first is month or larger
          TSX = 1
          TCX = 5
          CALL CMPTM2 ( TC(1), TS(1), TCX, TSX, TSFX(1), TCDX(1) )
          TSX = 1
          TCX = 4
          CALL CMPTM2 ( TC(2), TS(2), TCX, TSX, TSFX(2), TCDX(2) )
          IF (TSFX(1) .EQ. 0  .AND.  TSFX(2) .EQ. 0) THEN
C           times compatible with boundaries
            IF ((TCDX(1) .EQ. 0  .OR.  TCDX(1) .EQ. 2)  .AND.
     >          (TCDX(2) .EQ. 0  .OR.  TCDX(2) .EQ. 1)) THEN
C             larger time a month or more, smaller time a day or less
              TSTEPF = 0
              TCDCMP = 2
            END IF
          END IF
        END IF
      ELSE
C       valid time steps and units do not cross day-month boundry
        CALL CMPTM2 ( TC(1), TS(1), TC(2), TS(2), TSTEPF, TCDCMP )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CMPTM2
     M                   ( TC1, TS1, TC2, TS2,
     O                     TSTEPF, TCDCMP )
C
C     + + + PURPOSE + + +
C     This routine compares one time unit and step to a second time
C     unit and step.  Two flags are returned.  The first flag
C     indicates compatible/incompatible time steps.  The second flag
C     indicates which timestep is smaller.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  TC1, TC2, TS1, TS2, TSTEPF, TCDCMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TC1    - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C     TS1    - time step, in TC1 units
C     TC2    - time units code, see TC1
C     TS2    - time step, in TC2 units
C     TSTEPF - time step compatability flag
C              0 - compatible time series
C              1 - incompatible time steps
C     TCDCMP - flag indicating order of time steps
C               0 - time steps are the same
C               1 - first time step is smaller
C               2 - second time step is smaller
C              -1 - time units span day-month boundry
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CONVDN(7)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  CONVDN / 0, 60, 60, 24, 0, 12, 100 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((TC1 .LE. 4  .AND.  TC2 .GT. 4)  .OR.
     >    (TC1 .GT. 4  .AND.  TC2 .LE. 4)) THEN
C       time units span day-month boundry
        TSTEPF = 1
        TCDCMP = -1
      ELSE
C       acceptable time units
        IF (TC1 .NE. TC2) THEN
C         time units not same, adjust larger to agree with smaller
          IF (TC1 .LT. TC2) THEN
C           Adjust second time units to agree with first
 100        CONTINUE
              TS2 = TS2 * CONVDN(TC2)
              TC2 = TC2 - 1
            IF (TC1 .LT. TC2) GO TO 100
          ELSE
C           Adjust first time units to agree with second
 120        CONTINUE
              TS1 = TS1 * CONVDN(TC1)
              TC1 = TC1 - 1
            IF (TC2 .LT. TC1) GO TO 120
          END IF
        END IF
C
C       Time units converted, check time step
        TSTEPF = 0
        IF (TS1 .EQ. TS2) THEN
C         Same time step
          TCDCMP = 0
        ELSE IF (TS1 .LT. TS2) THEN
C         First time step smaller
          TCDCMP = 1
          IF (MOD(TS2,TS1) .NE. 0) TSTEPF = 1
        ELSE
C         Second time step smaller
          TCDCMP = 2
          IF (MOD(TS1,TS2) .NE. 0) TSTEPF = 1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATCHK
     I                   (DATE,
     O                    ERROR)
C
C     + + + PURPOSE + + +
C     Check DATE for valid entries.  For each valid element in
C     DATE, the corresponding element in ERROR is set to zero.
C     For each invalid element in DATE, the corresponding element
C     in ERROR is set to one.
C     Valid date elements are:
C       1800 <= year   <= 9999
C         1  <= month  <= 12
C         1  <= day    <= 31 or as appropriate for month and year
C         0  <= hour   <= 24
C         0  <= minute <= 60
C         0  <= second <= 60
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6),ERROR(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - Date to be checked
C     ERROR  - Array containing flags for valid DATE elements:
C              0 - good, 1 - invalid
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,DS,YR,MO,DY,HR,MI,SC
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      YR = DATE(1)
      MO = DATE(2)
      DY = DATE(3)
      HR = DATE(4)
      MI = DATE(5)
      SC = DATE(6)
C
      I= 6
      J= 0
      CALL ZIPI (I,J,ERROR)
C
C     check range on each number
      IF (YR.LT.1800.OR.YR.GE.9999) THEN
        ERROR(1) = 1
      END IF
      IF (MO.LT.1.OR.MO.GT.12) THEN
        ERROR(2) = 1
        DS = 31
      ELSE
        DS = DAYMON(YR,MO)
      END IF
      IF (DY.LT.1.OR.DY.GT.DS) THEN
        ERROR(3) = 1
      END IF
      IF (HR.LT.0.OR.HR.GT.24) THEN
        ERROR(4) = 1
      END IF
      IF (MI.LT.0.OR.MI.GT.60) THEN
        ERROR(5) = 1
      END IF
      IF (SC.LT.0.OR.SC.GT.60) THEN
        ERROR(6) = 1
      END IF
C
C     if day or month are zero, then hour,min,sec should be zero
      IF (MO.EQ.0.AND.DY.NE.0) ERROR(3) = 1
      IF (MO.EQ.0.AND.HR.NE.0) ERROR(4) = 1
      IF (MO.EQ.0.AND.MI.NE.0) ERROR(5) = 1
      IF (MO.EQ.0.AND.SC.NE.0) ERROR(6) = 1
      IF (DY.EQ.0.AND.HR.NE.0) ERROR(4) = 1
      IF (DY.EQ.0.AND.MI.NE.0) ERROR(5) = 1
      IF (DY.EQ.0.AND.SC.NE.0) ERROR(6) = 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATNXT
     I                   (INTRVL,UPBACK,
     M                    DATE)
C
C     + + + PURPOSE + + +
C     Based on the value of UPBACK, this routine adds or subtracts
C     the time interval INTRVL from the current date and time DATE.
C     The time convention has midnite as 24:00 of previous day, not
C     as 00:00 of next day.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6),UPBACK
      INTEGER*4 INTRVL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INTRVL - time step, in minutes
C     UPBACK - flag indicating direction in time to move:
C              >0  - move forward in time
C              <=0 - move back in time
C     DATE   - date to be modified
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   YEAR,MONTH,DAY,HR,MIN,SEC
      INTEGER*4 DHR,DMIN,TT,I4T24,I4T60,I4T0
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I4T24= 24
      I4T60= 60
      I4T0 = 0
C
      YEAR = DATE(1)
      MONTH = DATE(2)
      DAY = DATE(3)
      HR = DATE(4)
      MIN = DATE(5)
      SEC = DATE(6)
C
      DMIN = MIN
      DHR = HR
C
      IF (UPBACK.GT.0) THEN
C       move forward in time
        DMIN = DMIN + INTRVL
        IF (DMIN.LT.I4T60.AND.DHR.LT.I4T24) GO TO 49
          TT = DMIN/60
          DMIN = DMIN - TT*60
          DHR = DHR + TT
          IF (DHR.LT.I4T24) GO TO 48
          IF (DHR.EQ.I4T24.AND.DMIN.EQ.I4T0) GO TO 48
            TT = DHR/24
C           special case for daily timestep to keep convention
            IF (MOD(DHR,I4T24).EQ.0.AND.DMIN.EQ.I4T0) TT = TT-1
            DHR = DHR - TT*24
            DAY = DAY + TT
 40         CONTINUE
            IF (DAY.GT.DAYMON(YEAR,MONTH)) THEN
              DAY = DAY - DAYMON(YEAR,MONTH)
              MONTH = MONTH + 1
              IF (MONTH.GT.12) THEN
                MONTH = 1
                YEAR = YEAR + 1
              END IF
              GO TO 40
            END IF
 48       CONTINUE
 49     CONTINUE
      ELSE
C       move back in time
        DMIN = DMIN - INTRVL
        IF (DMIN.LE.I4T0) THEN
          TT = DMIN/60 - 1
          DMIN = DMIN-TT*60
          DHR = DHR + TT
          IF (DMIN.EQ.I4T60) THEN
            DMIN = 0
            DHR = DHR + 1
          END IF
          IF (DHR.LE.I4T0) THEN
            TT = DHR/24 - 1
            DHR = DHR-TT*24
            DAY = DAY + TT
  70        CONTINUE
            IF (DAY.LE.0) THEN
              MONTH = MONTH - 1
              IF (MONTH.LE.0) THEN
                MONTH = 12
                YEAR = YEAR - 1
              END IF
              DAY = DAYMON(YEAR,MONTH) + DAY
              GO TO 70
            END IF
          END IF
        END IF
      END IF
C
      MIN = DMIN
      HR = DHR
      DATE(1) = YEAR
      DATE(2) = MONTH
      DATE(3) = DAY
      DATE(4) = HR
      DATE(5) = MIN
      DATE(6) = SEC
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   DAYMON
     I                           (YR,MON)
C
C     + + + PURPOSE + + +
C     Return the number of days in the given month for the given
C     year, with leap year taken into account.  For an invalid
C     month, -1 is returned.  For an invalid year and a valid month,
C     the correct number of days is returned, with February = 28.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MON,YR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year, valid range is 1 - 9999
C     MON    - month, valid range is 1 - 12
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I4,I100,I400,NDAMON(12)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAMON/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     + + + END SPECIFICATIONS + + +
C
      I4 = 4
      I100 = 100
      I400 = 400
      IF (MON.EQ.2) THEN
        IF (YR .LE. 0  .OR.  YR .GT. 9999) THEN
C         invalid year
          DAYMON = 28
        ELSE IF (MOD(YR,I100).EQ.0) THEN
C         check whether this is a leap year on a century boundary
          IF (MOD(YR,I400).EQ.0) THEN
C           on a 400 year boundary
            DAYMON = 29
          ELSE
            DAYMON = 28
          END IF
        ELSE
          IF (MOD(YR,I4).EQ.0) THEN
C           leap year
            DAYMON = 29
          ELSE
            DAYMON = 28
          END IF
        END IF
C
      ELSE IF (MON.GE.1 .AND. MON.LE.12) THEN
C       no problem
        DAYMON = NDAMON(MON)
      ELSE
C       invalid month
        DAYMON = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DLIMIT
     I                   ( DATES, NDSN, FSLS,
     O                     DATE )
C
C     + + + PURPOSE + + +
C     Depending on the value of FSLS, find the earliest or
C     latest date in the array of dates DATES.  CKDATE is
C     used to determine the calendar order between dates.
C     The dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDSN, FSLS, DATE(6)
      INTEGER   DATES(6,NDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATES  - array of NDSN dates
C     NDSN   - number of dates
C     FSLS   - indicator for type of date to look for
C              1 - finds first date
C              2 - finds last date
C     DATE   - depending on the value of FSLS, the first or last
C              date in DATES
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, FLG, L
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     assume first/last
      L = 6
      CALL COPYI ( L, DATES, DATE )
      IF (FSLS .EQ. 1) THEN
C       find any before assumed first
        DO 100 N = 2, NDSN
          CALL CKDATE( DATE, DATES(1,N), FLG )
          IF (FLG .EQ. 1) THEN
C           new first
            CALL COPYI (L,DATES(1,N),DATE)
          END IF
 100    CONTINUE
      ELSE IF (FSLS .EQ. 2) THEN
C       find any after assumed last
        DO 200 N = 1, NDSN
          CALL CKDATE( DATE, DATES(1,N), FLG )
          IF (FLG .EQ. -1) THEN
C           new last
            CALL COPYI (L,DATES(1,N),DATE)
          END IF
 200    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   JDMODY
     I                    (YEAR,JDAY,
     O                     MON,DAY)
C
C     + + + PURPOSE + + +
C     Convert a julian day to month and day, leap year taken
C     into account.  YEAR and JDAY are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YEAR,JDAY,MON,DAY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - year
C     JDAY   - julian date
C     MON    - month
C     DAY    - day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LPFG,MOCUM(12,2)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MOCUM /31,59,90,120,151,181,212,243,273,304,334,365,
     1            31,60,91,121,152,182,213,244,274,305,335,366/
C
C     + + + END SPECIFICATIONS + + +
C
C     check for leap year
      LPFG= 1
      IF (MOD(YEAR,4).EQ.0.AND.
     1   (MOD(YEAR,100).NE.0.OR.MOD(YEAR,400).EQ.0)) LPFG= 2
C
      MON= 0
 10   CONTINUE
        MON= MON+ 1
      IF (JDAY.GT.MOCUM(MON,LPFG)) GOTO 10
C
      DAY= JDAY
      IF (MON.GT.1) DAY= JDAY- MOCUM(MON-1,LPFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NUMPTS
     I                   (DATE1,DATE2,DELT,
     O                    NPTS)
C
C     + + + PURPOSE + + +
C     Calculate the number of time steps between two dates.
C     If the second date is before the first date, a zero is
C     returned.  The first date is assumed to be at the end
C     of the first time step.  The dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),DELT,NPTS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first (start) date
C     DATE2  - second (end) date
C     DELT   - time step, in minutes
C     NPTS   - number of time steps between first and second date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NPD,YR,M,SYR,SMO,SDY,SHR,SMI,EYR,EMO,EDY,EHR,EMI,
     1          ERRFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      SYR = DATE1(1)
      SMO = DATE1(2)
      SDY = DATE1(3)
      SHR = DATE1(4)
      SMI = DATE1(5)
      EYR = DATE2(1)
      EMO = DATE2(2)
      EDY = DATE2(3)
      EHR = DATE2(4)
      EMI = DATE2(5)
C
      NPTS = 0
C     number per day
      NPD = 1440/DELT
      YR = SYR
      M = SMO
C
C     if end date before start date, return npts of 0
      CALL CKDATE (DATE1,DATE2,ERRFLG)
      IF (ERRFLG.LE.0) THEN
C
        IF (SMO.LT.EMO.OR.YR.LT.EYR) THEN
C         start and stop not same month
          NPTS = NPTS + (DAYMON(SYR,SMO)+1-SDY)*NPD
C
C         middle months
 56       CONTINUE
            M = M + 1
            IF (M.GT.12) THEN
              M = 1
              YR = YR + 1
            END IF
C
            IF (M.EQ.EMO.AND.YR.GE.EYR) GO TO 58
              NPTS = NPTS + DAYMON(YR,M)*NPD
              GO TO 56
C
 58       CONTINUE
C         final month
          NPTS = NPTS + EDY*NPD
        ELSE
C         start and stop in the same month
          NPTS = NPTS + (EDY+1-SDY)*NPD
        END IF
C
C       have correct number of days
        IF (NPD.GT.1) THEN
C         for timesteps less than one day
C         first day
          NPTS = NPTS - (SHR*60 + SMI)/DELT + 1
C         last day
          NPTS = NPTS - ((23-EHR)*60 + (60-EMI))/DELT
        END IF
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMADD
     I                    (DATE1,TCODE,TSTEP,NVALS,
     O                     DATE2)
C
C     + + + PURPOSE + + +
C     Add NVALS time steps to first date to compute second date.
C     The first date is assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),TCODE,TSTEP,DATE2(6)
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - starting date
C     TCODE  - time units
C              1 - second          5 - month
C              2 - minute          6 - year
C              3 - hour            7 - century
C              4 - day
C     TSTEP  - time step in TCODE units
C     NVALS  - number of time steps to be added
C     DATE2  - new date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 CARRY,TIMTMP(6),I4ZRO,I4TMP,DPM
      INTEGER   I,STPOS,DONFG,IYR,IMO
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO= 0
      DO 5 I= 1,6
        TIMTMP(I)= DATE1(I)
 5    CONTINUE
C
C     figure out how much time to add and where to start
      CARRY= NVALS* TSTEP
      STPOS= TCODE
      IF (STPOS.EQ.7) THEN
C       the time units are centuries, convert to years
        STPOS= 6
        CARRY= CARRY* 100
      END IF
C
C     add the time, not changing insig. parts
      GO TO (10,20,30,40,50,60), STPOS
C
 10   CONTINUE
C       seconds
        TIMTMP(6)= TIMTMP(6)+ CARRY
        CARRY    = TIMTMP(6)/ 60
        TIMTMP(6)= TIMTMP(6)- (CARRY*60)
 20   CONTINUE
C       minutes
        TIMTMP(5)= TIMTMP(5)+ CARRY
        CARRY    = TIMTMP(5)/ 60
        TIMTMP(5)= TIMTMP(5)- (CARRY*60)
 30   CONTINUE
C       hours
        TIMTMP(4)= TIMTMP(4)+ CARRY
        CARRY    = TIMTMP(4)/ 24
        TIMTMP(4)= TIMTMP(4)- (CARRY*24)
        IF (TIMTMP(4).EQ.I4ZRO.AND.TIMTMP(5).EQ.I4ZRO.AND.
     1      TIMTMP(6).EQ.I4ZRO) THEN
C         this is the day boundry problem
          TIMTMP(4)= 24
          CARRY = CARRY- 1
        END IF
 40   CONTINUE
C       days
        TIMTMP(3)= TIMTMP(3)+ CARRY
        DONFG= 0
 45     CONTINUE
          IYR= TIMTMP(1)
          IMO= TIMTMP(2)
          DPM= DAYMON(IYR,IMO)
          IF (TIMTMP(3).GT.DPM) THEN
C           add another month
            TIMTMP(3)= TIMTMP(3)- DPM
            TIMTMP(2)= TIMTMP(2)+ 1
            I4TMP= 12
            IF (TIMTMP(2).GT.I4TMP) THEN
              TIMTMP(2)= 1
              TIMTMP(1)= TIMTMP(1)+ 1
            END IF
          ELSE IF (TIMTMP(3).LE.0) THEN
C           subtract another month
            TIMTMP(2)= TIMTMP(2)- 1
            I4TMP= 0
            IF (TIMTMP(2).EQ.I4TMP) THEN
              TIMTMP(1)= TIMTMP(1)- 1
              TIMTMP(2)= 12
            END IF
            IYR= TIMTMP(1)
            IMO= TIMTMP(2)
            TIMTMP(3)= TIMTMP(3)- DAYMON(IYR,IMO)
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 45
C       month and year updated here, so
        CARRY= 0
 50   CONTINUE
C       months
        TIMTMP(2)= TIMTMP(2)+ CARRY
        CARRY    = (TIMTMP(2)-1)/ 12
        TIMTMP(2)= TIMTMP(2)- (CARRY*12)
 60   CONTINUE
C       years
        TIMTMP(1)= TIMTMP(1)+ CARRY
C     end computed go to
      IF (TCODE.GE.5) THEN
C       check days/month
        IYR= TIMTMP(1)
        IMO= TIMTMP(2)
        I4TMP= DAYMON(IYR,IMO)
        IF (I4TMP.LT.TIMTMP(3)) TIMTMP(3)= I4TMP
        IF (DAYMON(DATE1(1),DATE1(2)).EQ.DATE1(3)) TIMTMP(3)= I4TMP
      END IF
C
      DO 100 I= 1,6
        DATE2(I)= TIMTMP(I)
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMBAK
     I                    (TCODE,
     M                     DATE)
C
C     + + + PURPOSE + + +
C     Subtract one time interval at the given units TCODE from DATE.
C     The date is assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TCODE,DATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TCODE  - time units code
C              1 - seconds            5 - months
C              2 - minutes            6 - years
C              3 - hours              7 - centuries
C              4 - days
C     DATE   - date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONE,I,NEWDAY
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I     = TCODE
      NEWDAY= 0
C
 5    CONTINUE
C
        GO TO (10,20,30,40,50,60,70),I
C
 10     CONTINUE
C         seconds
          DATE(6)  = DATE(6)-1
          IF (DATE(6).LT.0) THEN
            DATE(6)= 59
            I     = I+ 1
            DONE  = 0
          ELSE
            DONE  = 1
          END IF
          GO TO 80
C
 20     CONTINUE
C         minutes
          DATE(5)  = DATE(5)-1
          IF (DATE(5).LT.0) THEN
            DATE(5)= 59
            I     = I+ 1
            DONE  = 0
          ELSE
            DONE  = 1
          END IF
          GO TO 80
C
 30     CONTINUE
C         hours
          DATE(4)  = DATE(4)-1
          IF (DATE(4).LT.0) THEN
            DATE(4)= 23
            I     = I+ 1
            DONE  = 0
          ELSE IF (DATE(4).EQ.0.AND.DATE(5).EQ.0.AND.DATE(6).EQ.0) THEN
            DATE(4)= 24
            I     = I+ 1
            DONE  = 0
          ELSE
            DONE= 1
          END IF
          GO TO 80
C
 40     CONTINUE
C         days
          DATE(3)= DATE(3)-1
          IF (DATE(3).LE.0) THEN
            NEWDAY= 1
            I   = I+ 1
            DONE= 0
          ELSE
            DONE= 1
          END IF
          GO TO 80
C
 50     CONTINUE
C         months
          DATE(2)= DATE(2)-1
          IF (DATE(2).LE.0) THEN
            DATE(2)= 12
            I   = I+ 1
            DONE= 0
          ELSE
            DONE= 1
          END IF
          GO TO 80
C
 60     CONTINUE
C         years
          DATE(1)= DATE(1)-1
          DONE= 1
          GO TO 80
C
 70     CONTINUE
C         centuries
          DATE(1)= DATE(1)-100
          DONE= 1
          GO TO 80
C
 80     CONTINUE
      IF (DONE.EQ.0) GO TO 5
C
      IF (NEWDAY.EQ.1) THEN
        DATE(3)= DAYMON(DATE(1),DATE(2))
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   TIMCHK
     I                            (DATE1,DATE2)
C
C     + + + PURPOSE + + +
C     Determine the calendar order of two dates.
C     The dates are assumed to be valid.
C     TIMCHK = 1 if DATE1 < DATE2
C            = 0 if DATE1 = DATE2
C            =-1 if DATE1 > DATE2
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first date
C     DATE2  - second date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,TMSDAT(6),TMEDAT(6),LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMCNV, COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     make a copy of the dates
      LEN = 6
      CALL COPYI( LEN, DATE1, TMSDAT )
      CALL COPYI( LEN, DATE2, TMEDAT )
C     convert dates to old format
      CALL TIMCNV (TMSDAT)
      CALL TIMCNV (TMEDAT)
C
C     ***************************
C     how about trying this???
C     CALL CKDATE (TMEDAT,TMSDAT,I)
C     instead of the rest of this wonderful code
C     ***************************
C
      I= 0
C     check years
      IF (TMSDAT(1).LT.TMEDAT(1)) THEN
        I= 1
      ELSE IF (TMSDAT(1).GT.TMEDAT(1)) THEN
        I= -1
      ELSE IF (TMSDAT(2).LT.TMEDAT(2)) THEN
C     checking months
        I= 1
      ELSE IF (TMSDAT(2).GT.TMEDAT(2)) THEN
        I= -1
      ELSE IF (TMSDAT(3).LT.TMEDAT(3)) THEN
C     checking days
        I= 1
      ELSE IF (TMSDAT(3).GT.TMEDAT(3)) THEN
        I= -1
      ELSE IF (TMSDAT(4).LT.TMEDAT(4)) THEN
C     checking hours
        I= 1
      ELSE IF (TMSDAT(4).GT.TMEDAT(4)) THEN
        I= -1
      ELSE IF (TMSDAT(5).LT.TMEDAT(5)) THEN
C     checking minutes
        I= 1
      ELSE IF (TMSDAT(5).GT.TMEDAT(5)) THEN
        I= -1
      ELSE IF (TMSDAT(6).LT.TMEDAT(6)) THEN
C     checking seconds
        I= 1
      ELSE IF (TMSDAT(6).GT.TMEDAT(6)) THEN
        I= -1
      END IF
C
      TIMCHK= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMCNV
     M                    (DATE)
C
C     + + + PURPOSE + + +
C     Convert a date that uses the midnight convention of 00:00
C     to the convention 24:00.  For example, 1982/10/01 00:00:00
C     would be converted to the date 1982/09/30 24:00:00.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date being converted
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DATE(4).EQ.0.AND.DATE(5).EQ.0.AND.DATE(6).EQ.0) THEN
C       date using new day boundry convention, convert to old
        DATE(4)= 24
        DATE(3)= DATE(3)- 1
        IF (DATE(3).EQ.0) THEN
          DATE(2)= DATE(2)- 1
          IF (DATE(2).EQ.0) THEN
            DATE(1)= DATE(1)- 1
            DATE(2)= 12
          END IF
          DATE(3)= DAYMON(DATE(1),DATE(2))
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMCVT
     M                   (DATE)
C
C     + + + PURPOSE + + +
C     Convert a date that uses the midnight convention of 24:00
C     to the convention 00:00.  For example, 1982/09/30 24:00:00
C     would be converted to the date 1982/10/01 00:00:00.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date being converted
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DATE(4).EQ.24 .AND. DATE(5).EQ.0 .AND. DATE(6).EQ.0) THEN
C       old midnight convention, convert to new
        DATE(4) = 0
        DATE(3) = DATE(3) + 1
        IF (DATE(3) .GT. DAYMON(DATE(1),DATE(2))) THEN
C         month boundry
          DATE(3) = 1
          DATE(2) = DATE(2) + 1
          IF (DATE(2) .GT. 12) THEN
C           year boundry
            DATE(2) = 1
            DATE(1) = DATE(1) + 1
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMDIF
     I                    (DATE1,DATE2,TCODE,TSTEP,
     O                     NVALS)
C
C     + + + PURPOSE + + +
C     Calculate the number of time steps between two dates.  Part
C     intervals at a time step less than TCODE and TSSTEP are not
C     included.  If the second date is before the first date, or the
C     second date is the same as the first date, the number of time
C     steps will be returned as 0.  Dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first (starting) date
C     DATE2  - second (ending) date
C     TCODE  - time units code
C              1 - seconds     5 - months
C              2 - minutes     6 - years
C              3 - hours       7 - centuries
C              4 - days
C     TSTEP  - time step in TCODE units
C     NVALS  - number of time steps between DATE1 and DATE2
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NDAYS,DONFG,I,NADJ,TMPSTR(6),TMPEND(6),LEN
      INTEGER*4 TM4STR(6),TM4END(6),I4TMP
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON, TIMCNV, COPYI, TIMADD, TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TIMCHK(DATE1,DATE2) .EQ. 1) THEN
C       end date follows start date, make temp copies of dates
        LEN = 6
        CALL COPYI ( LEN, DATE1, TMPSTR )
        CALL COPYI ( LEN, DATE2, TMPEND )
C
C       convert dates to old format
        CALL TIMCNV (TMPSTR)
        CALL TIMCNV (TMPEND)
C
C       copy dates to I*4 to preserve precision
        DO 2 I= 1,6
          TM4STR(I)= TMPSTR(I)
          TM4END(I)= TMPEND(I)
 2      CONTINUE
C
        GO TO (5,5,5,5,50,60,70), TCODE
 5      CONTINUE
C         figure out how many days
          DONFG= 0
          NDAYS= -TM4STR(3)
 8        CONTINUE
            IF (TM4STR(1).LT.TM4END(1).OR.
     1        (TM4STR(1).EQ.TM4END(1).AND.TM4STR(2).LT.TM4END(2))) THEN
              TMPSTR(1)= TM4STR(1)
              TMPSTR(2)= TM4STR(2)
              NDAYS= NDAYS+ DAYMON(TMPSTR(1),TMPSTR(2))
              TM4STR(2)= TM4STR(2)+ 1
              I4TMP= 13
              IF (TM4STR(2).EQ.I4TMP) THEN
                TM4STR(2)= 1
                TM4STR(1)= TM4STR(1)+ 1
              END IF
            ELSE
              DONFG= 1
            END IF
          IF (DONFG.EQ.0) GO TO 8
          NDAYS= NDAYS+ TM4END(3)
C
          GO TO (10,20,30,40),TCODE
 10       CONTINUE
C           seconds
            NVALS= ((((NDAYS*24)+
     1               TM4END(4)-TM4STR(4))* 60+
     2              TM4END(5)-TM4STR(5))* 60+
     3             TM4END(6)-TM4STR(6))/ TSTEP
            GO TO 45
 20       CONTINUE
C           minutes
            NVALS= (((NDAYS*24)+
     1              TM4END(4)-TM4STR(4))* 60+
     2             TM4END(5)-TM4STR(5))/ TSTEP
            GO TO 45
 30       CONTINUE
C           hours
            NVALS= ((NDAYS*24)+
     1             TM4END(4)- TM4STR(4))/ TSTEP
            GO TO 45
 40       CONTINUE
C           days
            NVALS= NDAYS/ TSTEP
 45       CONTINUE
          GO TO 90
C
 50     CONTINUE
C         months
          NVALS= ((TM4END(1)-TM4STR(1))*12+TM4END(2)-TM4STR(2))/TSTEP
          GO TO 90
C
 60     CONTINUE
C         years
          NVALS= (TM4END(1)-TM4STR(1))/ TSTEP
          GO TO 90
C
 70     CONTINUE
C         centuries
          NVALS= (TM4END(1)-TM4STR(1))/(TSTEP*100)
          GO TO 90
C
 90     CONTINUE
C
        DO 95 I= 1,6
          TMPSTR(I)= TM4STR(I)
          TMPEND(I)= TM4END(I)
 95     CONTINUE
        DONFG= 0
100     CONTINUE
          CALL TIMADD (DATE1,TCODE,TSTEP,NVALS,
     O                 TMPEND)
          NADJ= TIMCHK(DATE2,TMPEND)
          IF (NADJ.EQ.1.AND.NVALS.GE.1) THEN
C           estimate too high
            NVALS= NVALS- 1
          ELSE
C           estimate ok
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 100
      ELSE
C       end date is the same as or before start date
        NVALS = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMDFX
     I                    (DATE1,DATE2,
     O                     NVALS,TCODE,TSTEP)
C
C     + + + PURPOSE + + +
C     Calculate the number of values between two dates, including
C     units and time step.  First tries at one year time step, then
C     TCODE is decreased by one and tried again until the exact
C     time difference is determined.  Dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1 - first (starting) date
C     DATE2 - second (ending) date
C     NVALS  - number of values at the output TSTEP and TCODE
C     TCODE  - time units code
C              1 - seconds     5 - months
C              2 - minutes     6 - years
C              3 - hours       7 - centuries
C              4 - days
C     TSTEP  - time step in TCODE units
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG,TIMTMP(6)
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMADD, TIMCHK, TIMDIF
C
C     + + + END SPECIFICATIONS + + +
C
      TCODE= 6
      TSTEP= 1
      DONFG= 0
C
 10   CONTINUE
        CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP,
     O               NVALS)
        CALL TIMADD (DATE1,TCODE,TSTEP,NVALS,
     O               TIMTMP)
        IF (TIMCHK(DATE2,TIMTMP).EQ.0) THEN
C         we have call exact units, etc
          DONFG= 1
        ELSE
C         try again with shorter units
          TCODE= TCODE- 1
          IF (TCODE.EQ.0) THEN
C           problem with dates
            WRITE (*,*) 'BAD TIMDFX',DATE1,DATE2
            DONFG = 1
          END IF
        END IF
C
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTMCMN
     I                   ( NDAT, STRT, STOP, TSTEP, TCODE,
     O                     SDAT, EDAT, TS, TC, RETCOD )
C
C     + + + PURPOSE + + +
C     Determine the time period common to a number of pairs of dates.
C     Determine the smallest common time step and unit.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDAT, STRT(6,NDAT), STOP(6,NDAT), TSTEP(NDAT),
     $          TCODE(NDAT), SDAT(6), EDAT(6), TS, TC,
     $          RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDAT   - number of pairs of dates
C     STRT   - array of beginning dates
C     STOP   - array of ending dates
C     TSTEP  - array of time steps
C     TCODE  - array of time units codes
C              1 - second        4 - day
C              2 - minute        5 - month
C              3 - hour          6 - year
C     SDAT   - common starting data
C     EDAT   - common ending date
C     TS     - smallest common time step
C     TC     - smallest common time units
C              1 - second        4 - day
C              2 - minute        5 - month
C              3 - hour          6 - year
C     RETCOD - return code
C               0 - there is a common time period and time step and units
C              -1 - there is no common time period
C              -2 - there is a common time period, but the time step and
C                    time units are not compatible
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RETC, N, TSTEPF, TCDCMP, LN
C
C     + + + LOCAL DEFINITIONS + + +
C     TSTEPF - time step compatibility flag
C              0 - compatible time steps
C              1 - incompatible time steps
C     TCDCMP - flag indicating order of time steps
C               0 - time steps are the same
C               1 - first time step is smaller
C               2 - second time step is smaller
C              -1 - time units span day-month boundry
C
C     + + + EXTERNALS + + +
      EXTERNAL  DATCMN, CMPTIM, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
C     get common time period
      CALL DATCMN ( NDAT, STRT, STOP, SDAT, EDAT, RETC )
      IF (RETC .EQ. 0) THEN
C       get common time step and units
        TS = TSTEP(1)
        TC = TCODE(1)
        N  = 1
 100    CONTINUE
C         look for smallest common time step and unit
          N = N + 1
          CALL CMPTIM ( TCODE(N), TSTEP(N), TC, TS, TSTEPF, TCDCMP )
          IF (TSTEPF .EQ. 0  .AND.  TCDCMP .NE. -1) THEN
C           compatible time steps, do not span day-month boundry
            IF (TCDCMP .EQ. 2) THEN
C             new larger time step
              TS = TSTEP(N)
              TC = TCODE(N)
            END IF
          ELSE
C           incompatible time steps or time units span day-month boundry
            RETC = -2
          END IF
        IF (N .LT. NDAT  .AND.  RETC .EQ. 0) GO TO 100
        IF (RETC .EQ. -2) THEN
C         time step and time units are not all compatible
          TS = 0
          TC = 0
        END IF
      ELSE
C       no common time period
        RETC = -1
        LN = 6
        N  = 0
        CALL ZIPI ( LN, N, SDAT )
        CALL ZIPI ( LN, N, EDAT )
        TS = 0
        TC = 0
      END IF
      RETCOD = RETC
C
      RETURN
      END
C              
C
C
      SUBROUTINE   DATCMN
     I                   ( NDAT, STRT, STOP,
     O                     SDAT, EDAT, RETCOD )
C
C     + + + PURPOSE + + +
C     Determine the time period common to a number of pairs of dates.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDAT, STRT(6,NDAT), STOP(6,NDAT), SDAT(6), EDAT(6),
     $          RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDAT   - number of pairs of dates
C     STRT   - array of beginning dates
C     STOP   - array of ending dates
C     SDAT   - common starting data
C     EDAT   - common ending date
C     RETCOD - return code
C               0 - there is a common time period
C              -1 - there is no common time period
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   FSLS, DATE(6,2), RETC, LEN, ZIP
C
C     + + + EXTERNALS + + +
      EXTERNAL  DLIMIT, CKDATE, COPYI, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
C     get latest start date
      FSLS = 2
      CALL DLIMIT ( STRT, NDAT, FSLS, DATE(1,1) )
C     get earliest end date
      FSLS = 1
      CALL DLIMIT ( STOP, NDAT, FSLS, DATE(1,2) )
C     is start data before end date?
      CALL CKDATE ( DATE(1,1), DATE(1,2), RETC )
      IF (RETC .EQ. -1) THEN
C       common start date before common end date, as hoped for
        LEN = 6
        CALL COPYI ( LEN, DATE(1,1), SDAT )
        CALL COPYI ( LEN, DATE(1,2), EDAT )
        RETCOD = 0
      ELSE
C       begin and end the same (0) or common start after common end (1)
        LEN = 6
        ZIP = 0
        CALL ZIPI ( LEN, ZIP, SDAT )
        CALL ZIPI ( LEN, ZIP, EDAT )
        RETCOD = -1
      END IF
C
      RETURN
      END
