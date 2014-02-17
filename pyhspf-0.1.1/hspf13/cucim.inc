C
C     parameter for size of in memory UCI file
      INTEGER   MXUCI,MXSKWD,MXCKWD
      PARAMETER(MXUCI=70000,MXSKWD=1000,MXCKWD=2000)
C
C     in memory common block for OSUP file
      COMMON/CUCIMC/UCIM,UKWD,UEND
      CHARACTER*80  UCIM(MXUCI)
      CHARACTER*12  UKWD(MXSKWD)
      CHARACTER*4   UEND
C
C     UCIM   - uci record
C     UKWD   - keyword
C
      COMMON/CUCIM/UMSGFL,UMESSU,UOUTLV,
     #             FREREC,TYPREC,NXTREC,PREREC,
     #             NUKWD,NUKWLC,UKWDTY,UKWDFP,UKWDCT,
     #             UKWSRC,UKWERC,UKWPTN,UCINDT
      INTEGER      UMSGFL,UMESSU,UOUTLV
Cthj  the following were changed from i*2 to i*4 (the default) to allow
Cthj  uci files longer than 32767 lines
      INTEGER      FREREC,TYPREC(MXUCI),NXTREC(MXUCI),PREREC(MXUCI),
     #             NUKWD,NUKWLC,UKWDTY(MXSKWD),
     #             UKWDFP(MXSKWD),UKWDCT(MXSKWD),
     #             UKWSRC(MXCKWD),UKWERC(MXCKWD),UKWPTN(MXCKWD),
     #             UCINDT(MXUCI)
C
C     UMSGFL - message file unit number
C     UMESSU - output file unit number
C     FREREC - next free uci record
C     TYPREC - type of uci record
C     NXTREC - next uci record
C     PREREC - previous uci record
C     NUKWD  - number of keywords
C     NUKWLC - number of keyword locations
C     UKWDID - identifier of keyword
C     UKWDTY - type of keyword
C     UKWDFP - pointer to loc of info about first keyword
C     UKWDCT - counter of available copies of keyword
C     UKWSRC - uci record which contains keyword
C     UKWERC - uci record which contains assoc END keyword
C     UKWPTN - pointer to loc on info about next instance of keyword
C     UCINDT - number of spaces that line was un-indented
C
