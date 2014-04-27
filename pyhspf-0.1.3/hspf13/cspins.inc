C
C     include file CSPINS.INC - common block to store special action
C     instructions
C
C     + + + HISTORY + + +
C     12/16/2004 - jlk&pbd - added new variables for changing
C        MFACT during execution
C
      COMMON /CSPINS/SPWSIV,SPINS,SPPTR,
     $               SPUMIN,SPUMPO,SPUMPT,NSPUM,SPOUTM
C
      INTEGER      SPWSIV,SPINS(LENSPI,MXSPIN),SPPTR(MXSPIN),
     $             SPUMIN(25,MXSPUM),SPUMPO(MXSPUM),SPUMPT,NSPUM,
     $             SPOUTM
C
      EQUIVALENCE (SPINS,SPINSR),(SPUMIN,SPUMIR)
      REAL         SPINSR(LENSPI,MXSPIN),SPUMIR(25,MXSPUM)
C
C     SPWSIV - Last interval for which workspace has been updated
C     SPINS  - Special Action instructions
C              SPINS(1,*)  - YR      - Year
C              SPINS(2,*)  - MON     - Month
C              SPINS(3,*)  - DAY     - Day
C              SPINS(4,*)  - HR      - Hour
C              SPINS(5,*)  - MIN     - Minute
C              SPINS(6,*)  - TYPCOD  - Type code: 2=Integer, 3=Real,
C                                      4=Double Precision
C              SPINS(7,*)  - ADDR    - >0: Address in OSV
C                                      <0: ABS is Workspace address
C              SPINS(8,*)  - ACTCOD  -  1:    =   2:   +=   3:   -=
C                                       4:   *=   5:   /=   6:  MIN
C                                       7:  MAX   8:  ABS   9:  INT
C                                      10:   ^=  11:   LN  12:  LOG
C                                      13:  MOD  14: MOVT  15: MOV1
C                                      16: MOV2
C              SPINS(9,*)  - IVAL     - Integer version of Action value
C              SPINSR(9,*) - RVAL     - Real version of Action value
C              SPINS(10,*) - VNAME(1) - Name of target variable (first 4 chars)
C              SPINS(11,*) - VNAME(2) -            "            (last 2 chars)
C              SPINS(12,*) - VSUB(1)  - Subscripts of target variable
C              SPINS(13,*) - VSUB(2)  -            "
C              SPINS(14,*) - VSUB(3)  -            "
C                  or      - TARUVQ   - Index of target Workspace UVQUAN
C              SPINS(15,*) - DSIND    - Index of distribution
C              SPINS(16,*) - DSPOS    - Current position within distribution
C              SPINS(17,*) - DEFFRC   - Current deferred fraction from
C                                       distribution
C              SPINS(18,*) - LOGBLK   - Logical block containing Action
C              SPINS(19,*) - ACTUVQ   - Index of UVQUAN holding action value
C              SPINS(20,*) - ALLDEL   - >0: minutes to advance date for undated
C                                           (=delt)
C                                       =0: date is not updated
C                                       <0: minutes to advance date for deferred
C              SPINS(21,*) - ACCADD   - Address of variable accumulator.
C              SPINS(22,*) - UCIPOS   - UCI order (after expansion of repeats
C                                       and multiple UVNAMEs)
C              SPINS(23,*) - HDRPTR   - pointer to first header line in buffer
C              SPINS(24,*) - HDRNUM   - number of header lines in buffer
C              SPINS(25,*) - FTRPTR   - pointer to first footer line in buffer
C              SPINS(26,*) - FTRNUM   - number of footer lines in buffer
C     SPPTR  - Sort pointers for Special Action instructions
C     SPUMIN - Special Actions update mfact instructions (SPUMIR-real version)
C              SPUMIN(1,*) - source volume name
C              SPUMIN(3,*) - source volume number
C              SPUMIN(4,*) - source group name
C              SPUMIN(6,*) - source member name
C              SPUMIN(8,*) - source member subscripts
C              SPUMIR(10,*) - new mfact
C              SPUMIN(11,*) - update function
C              SPUMIN(12,*) - target volume name
C              SPUMIN(14,*) - target volume number
C              SPUMIN(15,*) - target group name
C              SPUMIN(17,*) - target member name
C              SPUMIN(19,*) - target member subscripts
C              SPUMIN(21,*)  - Year
C              SPUMIN(22,*)  - Month
C              SPUMIN(23,*)  - Day
C              SPUMIN(24,*)  - Hour
C              SPUMIN(25,*)  - Minute
C     SPUMPO - Sort by date position for recs in SPUMIN
C     SPUMPT - Sort pointer for mfact update instructions
c     NSPUM  - Number of specialactions update mfact instructions
C     SPOUTM - Special action output level for MFACT changes
C
      COMMON/CSPCH/SPHDR,SPFTR
C
      CHARACTER*80 SPHDR(MXSPHF)
      CHARACTER*20 SPFTR(MXSPHF)
