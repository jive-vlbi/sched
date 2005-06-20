C*VLBOPE -- open a VLB data file [Convex-UNIX]
C+
      INTEGER FUNCTION VLBOPE (UNIT, NAME, TYPE, STATUS, RESULT)
      INTEGER UNIT
      CHARACTER*(*) NAME
      CHARACTER*(*) TYPE
      CHARACTER*(*) STATUS
      CHARACTER*(*) RESULT
C
C Input parameters:
C   UNIT : Fortran unit number to be used
C   NAME : file name
C   TYPE : file type, one of the strings
C      'MERGE'  merge format
C      'FITS'   disk FITS format (image or uvfits)
C      'MODEL'  model file (text)
C      'TEXT'   arbitrary text file
C   STATUS :
C      'OLD' to open a file for reading (file must already exist)
C      'NEW' to open a new file for writing (file must not already
C            exist, unless the operating system can create a new
C            version)
C
C Output parameters:
C   VLBOPE : receives 1 if the file was opened successfully,
C      0 if the open failed
C   RESULT : receives the full file name, if the file
C      was opened successfully, or am error message (text) if the
C      open failed; the declared length of this variable should
C      be big enough to accommodate the result; 255 bytes should be
C      enough.
C
C Discussion: this encapsulates all system-dependent OPEN parameters
C like READONLY, CARRIAGECONTROL in a single subroutine.  Moving to
C a different operating system should require only this routine to be 
C changed, assuming the operating system can cope with Fortran formatted
C and unformatted read and write.
C-----------------------------------------------------------------------
      INTEGER ITYPE, ISTAT, IER, LEN1, BLKNO
      CHARACTER*11 FMT
      CHARACTER*4  CC
      CHARACTER*80 STRING
      COMMON /FITS2/ BLKNO
C
C Verify the TYPE argument.
C
      IF (TYPE.EQ.'MERGE') THEN
          ITYPE = 1
          FMT = 'UNFORMATTED'
          CC = 'NONE'
      ELSE IF (TYPE.EQ.'FITS') THEN
          ITYPE = 2
          FMT = 'UNFORMATTED'
          CC = 'NONE'
      ELSE IF (TYPE.EQ.'MODEL' .OR. TYPE.EQ.'TEXT') THEN
          ITYPE = 3
          FMT = 'FORMATTED'
          CC = 'LIST'
      ELSE
          CALL ERROR('VLBOPE: invalid argument TYPE='//TYPE)
      END IF
C
C Verify the STATUS argument.
C
      IF (STATUS.EQ.'OLD') THEN
          ISTAT = 1
      ELSE IF (STATUS.EQ.'NEW') THEN
          ISTAT = 2
      ELSE
          CALL ERROR('VLBOPE: invalid argument STATUS='//STATUS)
      END IF
C
C Attempt to open the file.
C
      IF (ISTAT.EQ.1) THEN
C         -- 'OLD' file
          IF (ITYPE.EQ.2) THEN
              OPEN (UNIT=UNIT, FILE=NAME, READONLY, STATUS='OLD',
     1              RECORDTYPE='FIXED', RECL=2880, ACCESS='DIRECT',
     2              FORM=FMT, IOSTAT=IER)
              BLKNO = 0
          ELSE
              OPEN (UNIT=UNIT, FILE=NAME, READONLY, STATUS='OLD',
     1              ACCESS='SEQUENTIAL', FORM=FMT, IOSTAT=IER)
          END IF
      ELSE
C         -- 'NEW' file
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='NEW',
     1          ACCESS='SEQUENTIAL', FORM=FMT, CARRIAGECONTROL=CC, 
     2          IOSTAT=IER)
      END IF
C
C Success: find the complete file name.
C
      IF (IER.EQ.0) THEN
          INQUIRE (UNIT=UNIT, NAME=RESULT)
          VLBOPE = 1
C
C Failure: determine the error, and issue a message.
C
      ELSE
          IF (ISTAT.EQ.1) THEN
              CALL PUTOUT('++ Cannot find '//TYPE//' file: '//
     1                NAME(1:LEN1(NAME)))
          ELSE
              CALL PUTOUT('++ Cannot create '//TYPE//' file: '//
     1                NAME(1:LEN1(NAME)))
          END IF
          CALL GERROR(STRING)
          CALL PUTOUT('++ '//STRING(1:LEN1(STRING)))
          RESULT = STRING
          VLBOPE = 0
      END IF
C-----------------------------------------------------------------------
      END
