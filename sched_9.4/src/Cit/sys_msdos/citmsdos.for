C*ERROR -- print error message on stdout, and abort [Convex-UNIX]
C+
      SUBROUTINE ERROR(STRING)
      CHARACTER*(*) STRING
C
C This subroutine prints its argument (a character string) on the 
C standard output, prefixed with '++ERROR++', and then terminates 
C execution of the program, returning an error status to its parent
C (shell or command interpreter).
C
C Argument:
C  STRING (input)  : text of message
C
C Subroutines required:
C  EXIT
C  PUTOUT
C
C History:
C  1987 Apr 8 - TJP.
C  1991 May 18 - RCW.  Remove EXIT call.
C-----------------------------------------------------------------------
      CALL PUTOUT('+++ERROR+++ ')
      CALL PUTOUT( STRING )
      STOP
C
      END
C*KPACK  -- write characters into numeric array (KEYIN)
C+
      SUBROUTINE KPACK(S, A)
      CHARACTER*(*) S
      DOUBLE PRECISION A(*)
C
C KEYIN: write data from character string into array
C 1991 May 18 - RCW:  MSDOS version.
C-----------------------------------------------------------------------
      INTEGER I, ND, LS
      INTEGER*1 B(256)
      DOUBLE PRECISION  C(32)
      EQUIVALENCE (B(1),C(1))
C
      LS = LEN(S)
      IF(LS.GT.256) CALL ERROR(' KPACK: String too long ')
      ND = (LS-1)/8 + 1
      DO 10 I=1,LS
          B(I) = ICHAR(S(I:I))
   10 CONTINUE
      IF( 8*ND .GT. LS ) THEN
         DO 20 I = LS+1, ND*8
            B(I) = ICHAR(' ')
   20    CONTINUE
      END IF
      DO 30 I = 1, ND
         A(I) = C(I)
   30 CONTINUE
      END
C*KRDLIN -- read one line of text from input file (KEYIN) [UNIX]
C+
      SUBROUTINE KRDLIN(UNIT, REC, MAXREC, OUTC, DIALOG, REFLEC, IER)
      INTEGER UNIT, MAXREC, OUTC, IER
      LOGICAL DIALOG, REFLEC
      CHARACTER*(*) REC
C
C KEYIN: read one line of text from input file and perform DCL symbol
C substitution.
C-----------------------------------------------------------------------
      INTEGER LEN1, NSUB
      CHARACTER*255 T
C
      IF (UNIT.EQ.0) GOTO 340
   10 IF (DIALOG) WRITE (OUTC, '(''* '',$)')
      READ (UNIT, '(A)', END=340) T
      MAXREC = LEN1(T)
      IF (MAXREC.LT.1) GOTO 10
      IF (REFLEC) WRITE (OUTC, '(1X,A)') T(1:MAXREC)
      CALL SYMSUB(T(1:MAXREC), REC, MAXREC, NSUB)
      REC(MAXREC+1:) = ' '
      IF (NSUB.GT.0) WRITE (OUTC, '(1X,A)') REC(1:MAXREC)
      IER = 0
      RETURN
  340 IER = 1
      RETURN
      END
C*PROGNM -- return file name of current program [Convex-UNIX]
C+
      SUBROUTINE PROGNM (PROG)
      CHARACTER*(*) PROG
C
C Return the file name of the currently executing program, minus
C any directories.
C
C Argument:
C  PROG   (output) : receives the file name.
C
C Subroutines required:
C  GETARG (Convex)
C
C History:
C  1990 Jan  9 - TJP.
C-----------------------------------------------------------------------
      CHARACTER*255 T
      INTEGER I,J
C
      CALL GETARG(0, T)
      I = 1
   10 J = INDEX(T(I:),'/')
      IF (J.EQ.0) THEN
          PROG = T(I:)
      ELSE
          I = I+J
          GOTO 10
      END IF
C
      END
C*PUTOUT -- write line on standard output [Convex-UNIX]
C+
      SUBROUTINE PUTOUT(TEXT)
      CHARACTER*(*) TEXT
C
C This subroutine writes one line on the standard output; the text to be
C written is supplied as a character-string argument.
C
C Argument:
C  TEXT   (input)  : character string for output.
C
C Subroutines required:
C  Fortran formatted I/O.
C
C History:
C  1987 Nov 11 - TJP
C  1991 May 20 - RCW  Modified msdos version to add blank at start of line.
C                     msdos seems to use Fortran carriage control.
C-----------------------------------------------------------------------
      WRITE (6,'(1X,A)') TEXT
      END
C*SCRNAM -- create a name for a scratch file [Convex-UNIX]
C+
      SUBROUTINE SCRNAM (TAG, NAME, L)
      CHARACTER*(*) TAG
      CHARACTER*(*) NAME
      INTEGER       L
C
C Create a name for a scratch file. This subroutine isolates an
C operating system dependency.
C
C On VMS, the name created is of the form
C "SYS$SCRATCH:tag.TMP" where "tag" is the supplied string.
C Note that NAME must be long enough to hold the result, i.e., 
C 16+LEN(TAG).
C 
C Arguments:
C  TAG    (input)  tag to be used to create the file name
C  NAME   (output) resulting file name
C  L      (output) number of significant characters in NAME.
C-----------------------------------------------------------------------
      CHARACTER*(*) SUFFIX
      PARAMETER (SUFFIX='.SCR')
      INTEGER  GETPID
C
      NAME = TAG//SUFFIX
      L = LEN(TAG) + LEN(SUFFIX) 
      IF (L.GT.LEN(NAME)) CALL ERROR('Bad arguments in SCRNAM')
      END
      LOGICAL FUNCTION TSTTTY(LUNIT)
      INTEGER LUNIT
C
C Return TRUE if the specified Fortran unit is connected to a terminal.
C  For now with msdos version, force return of .false.
C------------------------------------------------------------------------
      LOGICAL ISATTY
C
      TSTTTY = .FALSE.
      END
      INTEGER FUNCTION VMSHLP(TOPIC)
C Dummy version for non-VMS machines
      VMSHLP = 0
      END
C
C*VLBOPE -- open a VLB data file [MSDOS/MICROSOFT FORTRAN]
C+
      INTEGER FUNCTION VLBOPE (UNIT, NAME, TYPE, STATUS, RESULT)
      INTEGER UNIT
      CHARACTER*(*) NAME
      CHARACTER*(*) TYPE
      CHARACTER*(*) STATUS
      CHARACTER*(*) RESULT
C
C This routine encapsulates all system-dependent OPEN parameters
C like READONLY, CARRIAGECONTROL in a single subroutine.  Moving to
C a different operating system should require only this routine to be 
C changed, assuming the operating system can cope with Fortran formatted
C and unformatted read and write.
C
C Input arguments:
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
C      'SCRATCH'  Open new scratch file.  Close on termination of
C            program.
C
C Output arguments:
C   VLBOPE : receives 1 if the file was opened successfully,
C      0 if the open failed
C   RESULT : receives the full file name, if the file
C      was opened successfully, or am error message (text) if the
C      open failed; the declared length of this variable should
C      be big enough to accommodate the result; 255 bytes should be
C      enough.
C
C Subroutines required:
C   ERROR
C   LEN1
C   PUTOUT
C   ERRSNS (VMS)
C   SYS$GETMSG (VMS)
C
C History:
C   1988 Jun 9 - TJP.  VAX/VMS
C   1988 Nov 29 - RCW.  MSDOS/MICROSOFT
C        Some modes not tested.  Parameters RECORDTYPE and CARRIAGECONTROL
C        removed and not replaced with any equivalent.
C        No detailed information on file open failure obtained.  How?
C        
C-----------------------------------------------------------------------
      INTEGER ITYPE, ISTAT, IER
      INTEGER LEN1
      CHARACTER*11 FMT
      CHARACTER*5  TYPEC
      CHARACTER*3  STATUC
      CHARACTER*50 NAMEC
C
C Verify the TYPE argument.
C
      IF (TYPE.EQ.'MERGE') THEN
          ITYPE = 1
          FMT = 'UNFORMATTED'
      ELSE IF (TYPE.EQ.'FITS') THEN
          ITYPE = 2
          FMT = 'UNFORMATTED'
      ELSE IF (TYPE.EQ.'MODEL' .OR. TYPE.EQ.'TEXT') THEN
          ITYPE = 3
          FMT = 'FORMATTED'
      ELSE
          TYPEC = TYPE
          CALL ERROR('VLBOPE: invalid argument TYPE='//TYPEC)
      END IF
C
C Verify the STATUS argument.
C
      IF (STATUS.EQ.'OLD') THEN
          ISTAT = 1
      ELSE IF (STATUS.EQ.'NEW') THEN
          ISTAT = 2
      ELSE IF (STATUS.EQ.'SCRATCH') THEN
          ISTAT = 3
      ELSE
          STATUC = STATUS
          CALL ERROR('VLBOPE: invalid argument STATUS='//STATUC)
      END IF
C
C Attempt to open the file.
C
      IF (ISTAT.EQ.1) THEN
C         -- 'OLD' file
          IF (ITYPE.EQ.2) THEN
              OPEN (UNIT=UNIT, FILE=NAME, MODE='READ', STATUS='OLD',
     1              RECL=2880,
     2              FORM=FMT, IOSTAT=IER)
          ELSE
              OPEN (UNIT=UNIT, FILE=NAME, MODE='READ', STATUS='OLD',
     1              FORM=FMT, IOSTAT=IER)
          END IF
      ELSE IF (ISTAT.EQ.2) THEN
C         -- 'NEW' file
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='NEW',
     1          FORM=FMT, IOSTAT=IER)
      ELSE
C         -- 'SCRATCH' file
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='SCRATCH',
     1          FORM=FMT, IOSTAT=IER)
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
          TYPEC = TYPE
          NAMEC = NAME(1:LEN1(NAME))
          IF (ISTAT.EQ.1) THEN
              CALL PUTOUT('++ Cannot find '//TYPEC//' file: '//
     1                NAMEC)
          ELSE
              CALL PUTOUT(' ++ Cannot create '//TYPEC//' file: '//
     1                NAMEC) 
              CALL PUTOUT(' ++ Does it already exist?')
          END IF
C         I don't know how to get details of error
          RESULT = ' File open error'
          VLBOPE = 0
      END IF
C-----------------------------------------------------------------------
      END
C*SYMSUB -- DCL symbol substitution [UNIX]
C+
	SUBROUTINE SYMSUB (A,B,L,N)
	CHARACTER*(*) A,B
        INTEGER L,N
C
C SYMSUB: perform DCL symbol substitution in a string. For MSDOS, this
C routine just copies input to output, without making any changes.
C
C Arguments:
C  Input:  CHARACTER*(*) A	Input string.
C  Output: CHARACTER*(*) B	Output string; may be same as A.
C  Output: INTEGER L		The number of characters in B.
C  Output: INTEGER N		The number of substitutions performed.
C
C History:
C  Version 1.0:  1983 Dec 5	T.J. Pearson
C  Version 1.1:  1984 Jan 2	TJP; change from &XX to {XX}
C  Version 1.2:  1991 Feb 14    TJP: Unix version.
C  Version 1.3:  1991 May 18    RCW: MSDOS version.
C-----------------------------------------------------------------------
      INTEGER I
C
      L = MIN(LEN(A),LEN(B))
      DO 10 I=1,L
          B(I:I) = A(I:I)
   10 CONTINUE
      N = 0
      END
      SUBROUTINE IDATE( IM, ID, IY )
C
C     Routine for CIT programs to get current date from system.
C     Use Microsoft GETDAT routine.
C
      INTEGER    IM, ID, IY
      INTEGER*2  JM, JD, JY
C
      CALL GETDAT( JY, JM, JD )
      IM = JM
      ID = JD
      IY = JY
C
      RETURN
      END
