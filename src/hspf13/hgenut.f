C
C     1.2.5
C
      REAL   FUNCTION   CELSUS
     I                        (FTEMP)
C
C     + + + PURPOSE + + +
C     Convert temperature in Fahrenheit to Celsius.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       FTEMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTEMP  - ???
C
C     + + + END SPECIFICATIONS + + +
C
      CELSUS= (FTEMP - 32.0)*0.5555
C
      RETURN
      END
