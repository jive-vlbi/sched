C$Procedure      DCYPHR ( Decypher the meaning of an IOSTAT code)
 
      SUBROUTINE DCYPHR ( IOSTAT, FOUND, DIAGNS )
 
C$ Abstract
C
C     Given an IOSTAT code returned by a read, write, open,
C     inquire, or close statement, this routine returns a
C     brief text description of the problem.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C       ERROR
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               IOSTAT
      LOGICAL               FOUND
      CHARACTER*(*)         DIAGNS
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IOSTAT     I   The value of IOSTAT returned by a FORTRAN function
C      FOUND      O   TRUE if the value of IOSTAT was found
C      DIAGNS     O   A string describing the meaning of IOSTAT
C
C$ Detailed_Input
C
C     IOSTAT      is the non-zero value of IOSTAT returned by
C                 some intrinsic FORTRAN I/O facility such as
C                 OPEN, INQUIRE, READ, WRITE, or CLOSE.
C
C$ Detailed_Output
C
C     FOUND       is set to TRUE if the value of IOSTAT was found,
C                 otherwise it is returned as false.
C
C     DIAGNS      is a string that describes the meaning of IOSTAT.
C                 you should declare DIAGNS to be at least
C                 CHARACTER*(800) to ensure that the message will
C                 fit into DIAGNS.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the meaning of IOSTAT is not available within this
C        routine, DIAGNS will be returned with a string of the
C        form:
C
C           The value of IOSTAT was #.  The meaning of this
C           value is not available via the SPICE system.
C           Please consult your FORTRAN manual for the
C           meaning of this code.
C
C        where the character '#' will be replaced by a string
C        giving the input value of IOSTAT.
C
C$ Particulars
C
C     This routine is a utility for aiding in the construction
C     of messages relating to the failure of FORTRAN I/O.
C
C$ Examples
C
C     Suppose that you get a positive value of IOSTAT as the
C     result of a FORTRAN I/O statement and that you'd like to
C     present a descriptive diagnostic.
C
C        CALL DCYPHR ( IOSTAT, DIAGNS )
C        WRITE (*,*) DIAGNS
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 21-APR-1994 (HAN) (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Get the meaning of an IOSTAT value.
C
C-&
 
 
      LOGICAL               FIRST
      LOGICAL               NEXT
      LOGICAL               HP
      LOGICAL               SUN
      LOGICAL               SGI
      LOGICAL               VAX
      LOGICAL               PC
      LOGICAL               ALPHA
 
      INTEGER               LBND
      INTEGER               UBND
      INTEGER               N
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               ROOM
      PARAMETER           ( ROOM   =  2 )
 
 
      INTEGER               MAXMSG
      PARAMETER           ( MAXMSG = 90 )
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 800 )
 
      CHARACTER*(WDSIZE)    ATTR   ( ROOM )
      CHARACTER*(MAXLEN)    MESSGE ( MAXMSG )
 
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
 
      IF ( FIRST ) THEN
 
         CALL PLTFRM ( 2, N, ATTR )
 
         NEXT  = FIRST .AND. (ATTR(1) .EQ. 'NEXT ')
         HP    = FIRST .AND. (ATTR(1) .EQ. 'HP   ')
         SUN   = FIRST .AND. (ATTR(1) .EQ. 'SUN  ')
         SGI   = FIRST .AND. (ATTR(1) .EQ. 'SGI  ')
         VAX   = FIRST .AND. (ATTR(1) .EQ. 'VAX  ')
         PC    = FIRST .AND. (ATTR(1) .EQ. 'PC   ')
         ALPHA = FIRST .AND. (ATTR(1) .EQ. 'ALPHA')
         FIRST = .FALSE.
 
      END IF
 
 
 
      IF ( NEXT ) THEN
 
         LBND = 9999
         UBND = 10032
 
         MESSGE(1) = 'The file is not open for reading.'
         MESSGE(2) = 'The file is not open for writing.'
         MESSGE(3) = 'The file was not found.'
         MESSGE(4) = 'The record length specified was negative or 0.'
         MESSGE(5) = 'I/O buffer allocation failed.'
 
         MESSGE(6) = 'The iolist specifier was bad.'
         MESSGE(7) = 'The format string is in error.'
         MESSGE(8) = 'The repeat count is illegal.'
         MESSGE(9) = 'The hollerith count exceeds remaining '
     .   //          'format string.'
         MESSGE(10) = 'The format string is missing an opening "(".'
         MESSGE(11) = 'The format string has unmatched parentheses.'
         MESSGE(12) = 'The format string has unmatched quotes.'
         MESSGE(13) = 'A format descriptor is non-repeatable.'
         MESSGE(14) = 'The program attempted to read past end of '
     .   //           'the file.'
         MESSGE(15) = 'The file specification was bad.'
         MESSGE(16) = 'The format group table overflowed.'
         MESSGE(17) = 'An illegal character was present in '
     .   //           'numeric input.'
         MESSGE(18) = 'No record was specified while using '
     .   //           'direct access I/O.'
         MESSGE(19) = 'The maximum record number was exceeded.'
         MESSGE(20) = 'An illegal file type was supplied for use '
     .   //           'with namelist directed I/O'
         MESSGE(21) = 'An illegal input for namelist directed '
     .   //           'I/O was encountered.'
         MESSGE(22) = 'A variable is not present in the current '
     .   //           'namelist.'
         MESSGE(23) = 'A variable type or size does not match '
     .   //           'edit descriptor.'
         MESSGE(24) = 'An llegal direct access record number was '
     .   //           'used.'
 
         MESSGE(25) = 'An internal file was used illegally.'
         MESSGE(26) = 'The OPEN specifiere "RECL=" is only '
     .   //           'valid for direct access files'
         MESSGE(27) = 'The Open specifiere "BLOCK=" is '
     .   //           'only valid for unformatted sequential files.'
         MESSGE(28) = 'The program was unable to truncate the '
     .   //           'file after rewind, backspace,or endfile.'
         MESSGE(29) = 'It''s illegal to use formatted I/O on an '
     .   //           'entire structure.'
         MESSGE(30) = 'An illegal (negative) unit was specified.'
         MESSGE(31) = 'The specifications in a RE-OPEN do not '
     .   //           'match aprevious OPEN.'
         MESSGE(32) = 'An implicit OPEN can not be used '
     .   //           'for direct access files.'
         MESSGE(33) = 'The file already exists. It cannot be '
     .   //           'opened as a new file.'
 
      ELSE IF ( SUN ) THEN
 
         LBND      = 99
         UBND      = 126
 
         MESSGE(1) = 'The format string is in error.'
         MESSGE(2) = 'The unit number is illegal.'
         MESSGE(3) = 'The logical unit was opened for unformatted '
     .   //          'I/O, not formatted.'
         MESSGE(4) = 'The logical unit was opened for formatted '
     .   //          'I/O, not unformatted.'
         MESSGE(5) = 'The logical unit was opened for sequential '
     .   //          'access, or the logical record length was '
     .   //          'specified as zero.'
         MESSGE(6) = 'The logical unit was opened for direct '
     .   //          'I/O, not sequential.'
         MESSGE(7) = 'The program was unable to backspace the file.'
         MESSGE(8) = 'The format specified a left tab beyond the '
     .   //          'beginning of an internal input record.'
         MESSGE(9) = 'The system cannot return status information '
     .   //          'about the file. Perhaps the directory is '
     .   //          'unreadable.'
         MESSGE(10) = 'Repeat counts in list-directed I/O must be '
     .   //           'followed by an asterisk with no blank spaces.'
 
         MESSGE(11) = 'The program attempted to read past the end '
     .   //           'of a record.'
 
         MESSGE(12) = 'The program was unable to truncate an '
     .   //           'external sequential file on close, '
     .   //           'backspace, or rewind.'
         MESSGE(13) = 'The list input is incomprehensible.'
         MESSGE(14) = 'The library dynamically creates buffers for '
     .   //           'internal use. The program is too big, and '
     .   //           'thus ran out of free space.'
         MESSGE(15) = 'The logical unit was not open.'
         MESSGE(16) = 'An unexpected character was encountered. '
     .   //           'Some format conversions cannot tolerate '
     .   //           'nonnumeric data.'
         MESSGE(17) = 'Logical data must be true or false.'
         MESSGE(18) = 'The program tried to open an existing file '
     .   //           'with "STATUS = NEW".'
         MESSGE(19) = 'The program tried to open a nonexistent file '
     .   //           'with "STATUS=OLD".'
         MESSGE(20) = 'The program caused an unknown system error. '
     .   //           'Contact your system administrator!'
         MESSGE(21) = 'Direct access of a file requires seek '
     .   //           'ability. Sequential unformatted I/O and '
     .   //           'tabbing left also require seek ability.'
         MESSGE(22) = 'An illegal argument was specified in the '
     .   //           'statement.'
         MESSGE(23) = 'The repeat count for list-directed input '
     .   //           'must be a positive integer.'
         MESSGE(24) = 'An illegal operation was attempted on the '
     .   //           'device associated with the unit.'
         MESSGE(25) = 'The program tried to open too many files. '
     .   //           'The limit is 64.'
         MESSGE(26) = 'The logical unit was not open.'
         MESSGE(27) = 'A namelist read encountered an invalid '
     .   //           'data item.'
 
      ELSE IF ( HP ) THEN
 
         LBND = 899
         UBND = 989
 
         MESSGE( 1) = 'Error in format. Format specification '
     .   //           'does not start with a left parenthesis or '
     .   //           'end with a right parenthesis, or contains '
     .   //           'unrecognizable code or string; format '
     .   //           'specification is too long for library '
     .   //           'internal buffer. Change the format '
     .   //           'specification to proper syntax; split the '
     .   //           'format specifications into several '
     .   //           'statements. '
 
         MESSGE( 2) = 'I/O with illegal unit number attempted. '
     .   //           'Negative unit number was used in an I/O '
     .   //           'statement. Use integers greater than or '
     .   //           'equal to 0 for an I/O number. '
 
         MESSGE( 3) = 'Formatted I/O attempted on unformatted '
     .   //           'file. Formatted I/O was attempted on a '
     .   //           'file opened for unformatted I/O. Open the '
     .   //           'file for formatted I/O; do unformatted '
     .   //           'I/O on this file. '
 
         MESSGE( 4) = 'Unformatted I/O attempted on formatted '
     .   //           'file. Unformatted I/O was attempted on a '
     .   //           'file opened for formatted I/O. Open the '
     .   //           'file for unformatted I/O; do formatted '
     .   //           'I/O on this file. '
 
         MESSGE( 5) = 'Direct I/O attempted on sequential file. '
     .   //           'Direct operation attempted on sequential '
     .   //           'file; direct operation attempted on '
     .   //           'opened file connected to a terminal. Use '
     .   //           'sequential operations on this file; open '
     .   //           'file for direct access; do not do direct '
     .   //           'I/O on a file connected to a terminal. '
 
         MESSGE( 6) = 'Error in list- or name-directed read of '
     .   //           'logical data. Found repeat value, but no '
     .   //           'asterisk; first character after optional '
     .   //           'decimal point was not "T" or "F". Change '
     .   //           'input data to correspond to syntax '
     .   //           'expected by list-directed input of '
     .   //           'logicals; use input statement that '
     .   //           'corresponds to syntax of input data. '
 
         MESSGE( 7) = 'Illegal sequential I/O to tty attempted1. '
     .   //           'Executed a BACKSPACE, REWIND, formatted '
     .   //           'READ, or formatted WRITE, on this '
     .   //           'sequential file or device. Use a file or '
     .   //           'device that is considered blocked in '
     .   //           'HP-UX. '
 
         MESSGE( 8) = 'List- or name-directed read of character '
     .   //           'data attempted. Found repeat value, but '
     .   //           'no asterisk; character not delimited by '
     .   //           'quotation marks. Change input data to '
     .   //           'correspond to syntax expected by '
     .   //           'list-directed input of characters; use '
     .   //           'input statement that corresponds to '
     .   //           'syntax of input data. '
 
         MESSGE( 9) = 'Open of file with bad path-name '
     .   //           'attempted. Tried to open a file that the '
     .   //           'system would not allow for one of the '
     .   //           'following reasons: 1.  A component of the '
     .   //           'path prefix is not a directory. 2.  The '
     .   //           'named file does not exist. 3.  Search '
     .   //           'permission is denied for a component of '
     .   //           'the path prefix. Correct the path-name to '
     .   //           'invoke the file intended; check that the '
     .   //           'file is not corrupt; be sure that search '
     .   //           'permissions are set properly. '
 
         MESSGE(10) = 'Sequential I/O attempted on direct file. '
     .   //           'Attempted a BACKSPACE, REWIND, or ENDFILE '
     .   //           'on a direct file. Open the file for '
     .   //           'sequential access; do not use BACKSPACE, '
     .   //           'REWIND, or ENDFILE. '
 
         MESSGE(11) = 'Access past end of record attempted. '
     .   //           'Tried to do I/O on record of a file past '
     .   //           'beginning or end of record. Perform I/O '
     .   //           'operation within bounds of the record; '
     .   //           'increase record length. '
 
         MESSGE(12) = 'Recursive I/O attempted1. An I/O '
     .   //           'specifier or item in an I/O list '
     .   //           'attempted to do I/O (that is, calls to '
     .   //           'functions that do I/O). Remove calls to '
     .   //           'functions that do I/O from the '
     .   //           'specifier/list item; remove I/O '
     .   //           'statements from the function called by '
     .   //           'the specifier/list item. '
 
         MESSGE(13) = 'Error in list- or name-directed read of '
     .   //           'complex data. While reading complex data, '
     .   //           'one of the following problems has '
     .   //           'occurred: 1.  No left parenthesis or no '
     .   //           'repeat value. 2.  Found repeat value, but '
     .   //           'no asterisk. 3.  No comma after real '
     .   //           'part. 4.  No closing right parenthesis. '
     .   //           'Change input data to correspond to syntax '
     .   //           'expected by list-directed input of '
     .   //           'complex numbers; use input statement '
     .   //           'corresponding to syntax of input data. '
 
         MESSGE(14) = 'Out of free space. Library cannot store '
     .   //           'file name (from OPEN statement) or '
     .   //           'characters read (from list-directed '
     .   //           'read). Use shorter file name or read '
     .   //           'fewer characters; use fewer file names or '
     .   //           'read fewer character strings. '
 
         MESSGE(15) = 'Access of unconnected unit attempted. '
     .   //           'Unit specified in I/O statement has not '
     .   //           'previously been connected to anything. '
     .   //           'Connect unit (that is, OPEN it) before '
     .   //           'attempting I/O on it; perform I/O on '
     .   //           'another, already connected, unit. '
 
         MESSGE(16) = 'Read unexpected character. While reading '
     .   //           'an integer, read a character that was not '
     .   //           'a digit, "+", "-", comma, end-of-line or '
     .   //           'blank; while reading a real number, read '
     .   //           'a character that was not a digit, "+", '
     .   //           '"-", comma, end-of-line, blank, "d", "D", '
     .   //           '"e", "E", or period. Remove from input '
     .   //           'data any characters that are illegal in '
     .   //           'integers or real numbers. '
 
         MESSGE(17) = 'Error in read of logical data. A blank '
     .   //           'was read when logical data was expected. '
     .   //           'Change input data to correspond to syntax '
     .   //           'expected when reading logical data; use '
     .   //           'input statement corresponding to syntax '
     .   //           'of input data. '
 
         MESSGE(18) = 'Open with named scratch file attempted. '
     .   //           'Executed OPEN statement with '
     .   //           'STATUS=''SCRATCH'', but also named the '
     .   //           'file (FILE= filename). Either open file '
     .   //           'with STATUS=''SCRATCH'', or name the file '
     .   //           'in an OPEN statement, but not both. '
 
         MESSGE(19) = 'Open of existing file with STATUS=''NEW'' '
     .   //           'attempted. Executed OPEN statement with '
     .   //           'STATUS=''NEW'', but file already exists. '
     .   //           'Use OPEN without STATUS specifier, or '
     .   //           'with STATUS=''OLD'', or '
     .   //           'STATUS=''UNKNOWN''. '
 
         MESSGE(20) = 'The value of IOSTAT was 919.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. . '
 
         MESSGE(21) = 'Open of file connected to different unit '
     .   //           'attempted. Executed OPEN statement with '
     .   //           'file name that is already associated with '
     .   //           'a UNIT specifier. Use an OPEN statement '
     .   //           'with a file name that is not connected to '
     .   //           'a unit name; open the connected file to '
     .   //           'the same unit name. '
 
         MESSGE(22) = 'Unformatted open with BLANK specifier '
     .   //           'attempted. OPEN statement specified '
     .   //           'FORM=''UNFORMATTED'' and BLANK= xx. Use '
     .   //           'either FORM=''FORMATTED'' or BLANK= xx, '
     .   //           'but not both, when opening files. '
 
         MESSGE(23) = 'I/O on illegal record attempted. '
     .   //           'Attempted to read a record of a formatted '
     .   //           'or unformatted direct file that is beyond '
     .   //           'the current end-of-file. Read records '
     .   //           'that are within the bounds of the file. '
 
         MESSGE(24) = 'Open with illegal FORM specifier '
     .   //           'attempted. FORM specifier did not begin '
     .   //           'with "F", "f", "U", or "u". Use either '
     .   //           '''FORMATTED'' or ''UNFORMATTED'' for the '
     .   //           'FORM specifier in an OPEN statement. '
 
         MESSGE(25) = 'Close of scratch file with '
     .   //           'STATUS=''KEEP'' attempted. The file '
     .   //           'specified in the CLOSE statement was '
     .   //           'previously opened with ''SCRATCH'' '
     .   //           'specified in the STATUS specifier. Open '
     .   //           'the file with a STATUS other than '
     .   //           '''SCRATCH''; do not specify '
     .   //           'STATUS=''KEEP'' in the CLOSE statement '
     .   //           'for this scratch file. '
 
         MESSGE(26) = 'Open with illegal STATUS specifier '
     .   //           'attempted. STATUS specifier did not begin '
     .   //           'with "O", "o", "N", "n", "S", "s", "U", '
     .   //           'or "u". Use ''OLD'', ''NEW'', '
     .   //           '''SCRATCH'', or ''UNKNOWN'' for the '
     .   //           'STATUS specifier in OPEN statement. '
 
         MESSGE(27) = 'Close with illegal STATUS specifier '
     .   //           'attempted. STATUS specifier did not begin '
     .   //           'with "K", "k", "D", or "d". statement. '
 
         MESSGE(28) = 'Open with illegal ACCESS specifier '
     .   //           'attempted. ACCESS specifier did not begin '
     .   //           'with "S", "s", "D", or "d". Use '
     .   //           '''SEQUENTIAL'' or ''DIRECT'' for the '
     .   //           'ACCESS specifier in an OPEN statement. '
 
         MESSGE(29) = 'Open of sequential file with RECL '
     .   //           'specifier attempted. OPEN statement had '
     .   //           'both ACCESS=''SEQUENTIAL'' and RECL= xx '
     .   //           'specified. Omit RECL specifier; specify '
     .   //           'ACCESS=''DIRECT''. '
 
         MESSGE(30) = 'Open of direct file with no RECL '
     .   //           'specifier attempted. OPEN statement has '
     .   //           'ACCESS=''DIRECT'', but no RECL specifier. '
     .   //           'Add RECL specifier; specify '
     .   //           'ACCESS=''SEQUENTIAL''. or Open of direct '
     .   //           'file with no RECL or RECL=0 attempted1 '
     .   //           'OPEN statement has ACCESS=''DIRECT'', but '
     .   //           'no RECL specifier. Add RECL specifier; '
     .   //           'specify ACCESS=''SEQUENTIAL''. '
 
         MESSGE(31) = 'Open with RECL less than 1 attempted. '
     .   //           'RECL specifier in OPEN statement was less '
     .   //           'than or equal to zero. Use a positive '
     .   //           'number for RECL specifier in OPEN '
     .   //           'statement. or Open with RECL less than '
     .   //           'zero attempted. RECL specifier in OPEN '
     .   //           'statement was less than or equal to zero. '
     .   //           'Use a positive number for RECL specifier '
     .   //           'in OPEN statement. '
 
         MESSGE(32) = 'Open with illegal BLANK specifier '
     .   //           'attempted. BLANK specifier did not begin '
     .   //           'with "N", "n", "Z", or "z". Use ''NULL'' '
     .   //           'or ''ZERO'' for BLANK specifier in OPEN '
     .   //           'statement. '
 
         MESSGE(33) = 'Too many units open at once. The program '
     .   //           'attempted to have greater than 60 files '
     .   //           'open at once. Close a presently open file '
     .   //           'before opening another. '
 
         MESSGE(34) = 'End of file encountered. Attempted to '
     .   //           'read beyond the end of a sequential file. '
     .   //           'Read records that are within bounds of '
     .   //           'the file. '
 
         MESSGE(35) = 'The value of IOSTAT was 934.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(36) = 'Internal library error. A rare software '
     .   //           'error has occurred. Report the error. '
 
         MESSGE(37) = 'The value of IOSTAT was 936.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(38) = 'Access of record <=0 attempted. Access of '
     .   //           'direct file specifier REC= negative '
     .   //           'number or 0. Use an integer greater than '
     .   //           '0 in the REC= specifier. '
 
         MESSGE(39) = 'List I/O of unknown type attempted. An '
     .   //           'internal error has occurred. Report the '
     .   //           'error. '
 
         MESSGE(40) = 'Open of inaccessible file attempted. When '
     .   //           'opening a file with STATUS=''OLD'', '
     .   //           'component of the path is not a directory, '
     .   //           'the named file does not exist, or the '
     .   //           'path points outside a process or '
     .   //           'allocated address space. Use legal '
     .   //           'pathname; insure existence of file; or '
     .   //           'open with STATUS=''NEW''. '
 
         MESSGE(41) = 'Open attempted. Too many files open; file '
     .   //           'permissions do not allow access. Close '
     .   //           'some files before opening more; change '
     .   //           'read/write access of file to allow open. '
 
         MESSGE(42) = 'Error in sequential unformatted read. '
     .   //           'Attempt to prepare file for sequential '
     .   //           'unformatted read failed. Use existing, '
     .   //           'non-corrupt file and be sure the system '
     .   //           'is not corrupt. '
 
         MESSGE(43) = 'Error in list- or name-directed read. '
     .   //           'System detected error while trying to do '
     .   //           'list read. Be sure system and file are '
     .   //           'not corrupt. '
 
         MESSGE(44) = 'Error in direct formatted read. System '
     .   //           'encountered problem while reading a '
     .   //           'character from specified external file. '
     .   //           'Be sure file and system are not corrupt. '
 
         MESSGE(45) = 'Error in direct unformatted I/O. System '
     .   //           'found error while concluding direct '
     .   //           'unformatted I/O call. Be sure file and '
     .   //           'system are not corrupt. '
 
         MESSGE(46) = 'Error in formatted I/O. System found '
     .   //           'error while reading or writing formatted '
     .   //           'data; usually means more characters were '
     .   //           'requested than exist in a record. Be sure '
     .   //           'format matches data.  Be sure file and '
     .   //           'system are not corrupt. '
 
         MESSGE(47) = 'Error in list I/O. List I/O was attempted '
     .   //           'on an unformatted file. Do list I/O on '
     .   //           'formatted file. '
 
         MESSGE(48) = 'Edit descriptor not compatible with type '
     .   //           'of item. Use an edit descriptor that is '
     .   //           'compatible with the data item; use a data '
     .   //           'item that is compatible with the edit '
     .   //           'descriptor. '
 
         MESSGE(49) = 'Write to write-protected file attempted. '
     .   //           'Change write protection bit to allow '
     .   //           'write; do not write to this file. '
 
         MESSGE(50) = 'Read from read-protected file attempted. '
     .   //           'Change read protection bit to allow read; '
     .   //           'do not read from this file. '
 
         MESSGE(51) = 'Value out of range. An index to an array '
     .   //           'or substring reference was outside of the '
     .   //           'declared limits. Check all indexes to '
     .   //           'arrays and substrings. '
 
         MESSGE(52) = 'Label out of bounds in assigned GOTO. The '
     .   //           'value of the variable did not correspond '
     .   //           'to any of the labels in the list in an '
     .   //           'assigned GOTO statement. Check for a '
     .   //           'possible logic error in the program or an '
     .   //           'incorrect list in the assigned GOTO '
     .   //           'statement. '
 
         MESSGE(53) = 'Zero increment value in DO loop. A DO '
     .   //           'loop with a zero increment has produced '
     .   //           'an infinite loop. Check for a logic error '
     .   //           'in the program. '
 
         MESSGE(54) = 'No repeatable edit descriptor in format '
     .   //           'statement. A repeat count was given for '
     .   //           'an edit descriptor that does not allow '
     .   //           'repetition. Add at least one repeatable '
     .   //           'edit descriptor to the format statement. '
 
         MESSGE(55) = 'Illegal use of empty format attempted. An '
     .   //           'empty format specification, (), was used '
     .   //           'with the list items specified. Remove the '
     .   //           'items from I/O list; fill in the format '
     .   //           'specifications with the appropriate '
     .   //           'format descriptors. '
 
         MESSGE(56) = 'Open with no FILE= and STATUS ''OLD'' or '
     .   //           '''NEW'' attempted. Status ''NEW'' or '
     .   //           '''OLD'' was attempted and FILE= was not '
     .   //           'specified. Change the STATUS specifier to '
     .   //           '''SCRATCH'' or ''UNKNOWN''; add the file '
     .   //           'specifier. '
 
         MESSGE(57) = 'The value of IOSTAT was 956.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(58) = 'Format descriptor incompatible with '
     .   //           'numeric item in I/O list. A numeric item '
     .   //           'in the I/O list was matched with a '
     .   //           'nonnumeric format descriptor. Match '
     .   //           'format descriptors to I/O list. or File '
     .   //           'could not be truncated. Physical length '
     .   //           'of file could not be forced to match the '
     .   //           'logical length. '
 
         MESSGE(59) = 'Format descriptor incompatible with '
     .   //           'character item in I/O list. A character '
     .   //           'item in the I/O list was matched with a '
     .   //           'format descriptor other than "A" or "R". '
     .   //           'Match format descriptors to I/O list. or '
     .   //           'Unexpected character in NAMELIST read. An '
     .   //           'illegal character was found in '
     .   //           'NAMELIST-directed input. Be sure input '
     .   //           'data conforms to the syntax rules for '
     .   //           'NAMELIST-directed input. '
 
         MESSGE(60) = 'Format descriptor incompatible with '
     .   //           'logical item in I/O list. A logical item '
     .   //           'in the I/O list was matched with a format '
     .   //           'descriptor other than "L". Match format '
     .   //           'descriptors to I/O list. or Illegal '
     .   //           'subscript/substring in NAMELIST read. An '
     .   //           'invalid subscript or substring specifier '
     .   //           'was found in NAMELIST-directed input. '
     .   //           'Possible causes:  bad syntax, '
     .   //           'subscript/substring component '
     .   //           'out-of-bounds, wrong number of '
     .   //           'subscripts, substring on non-CHARACTER '
     .   //           'variable. Check input data for syntax '
     .   //           'errors.  Be sure subscript/substring '
     .   //           'specifiers are correct for data type. '
 
         MESSGE(61) = 'Format error: Missing starting left '
     .   //           'parenthesis. Format did not begin with a '
     .   //           'left parenthesis. Begin format with a '
     .   //           'left parenthesis. or Too many values in '
     .   //           'NAMELIST read. Too many input values were '
     .   //           'found during a NAMELIST-directed READ. '
     .   //           'This message will be generated by '
     .   //           'attempts to fill variables beyond their '
     .   //           'memory limits. Remove excess values from '
     .   //           'input data. '
 
         MESSGE(62) = 'Variable not in NAMELIST group. A '
     .   //           'variable name was encountered in the '
     .   //           'input stream which was not declared as '
     .   //           'part of the current NAMELIST group. Check '
     .   //           'input data with NAMELIST group '
     .   //           'declaration for differences. Format '
     .   //           'error: Invalid format descriptor. Format '
     .   //           'descriptor did not begin with a character '
     .   //           'that can start a legal format descriptor. '
     .   //           'Specify correct format descriptor. '
 
         MESSGE(63) = 'Unexpected character found following a '
     .   //           'number in the format string. Format '
     .   //           'error:  Character in the set '
     .   //           'IFEDGMNK@OLAR(PHX expected and not found. '
     .   //           'Specify correct format descriptor to '
     .   //           'follow number. or NAMELIST I/O attempted '
     .   //           'on unformatted file1 An illegal NAMELIST '
     .   //           'I/O operation was attempted on an '
     .   //           'unformatted file. OPEN file with '
     .   //           'FORM=''FORMATTED''. '
 
         MESSGE(64) = 'Format error: Trying to scale unscalable '
     .   //           'format specifier. The specifier being '
     .   //           'scaled is not "F", "E", "D", "M", "N", or '
     .   //           '"G". Scale only specifiers for '
     .   //           'floating-point I/O. or COUNT exceeds '
     .   //           'buffer length in ENCODE/DECODE1 The count '
     .   //           'of characters to be transferred exceeds '
     .   //           'the internal buffer length. Either '
     .   //           'transfer fewer characters or use a larger '
     .   //           'buffer. '
 
         MESSGE(65) = 'Format error: Parentheses too deeply '
     .   //           'nested. Too many left parentheses for the '
     .   //           'format processor to stack. Nest '
     .   //           'parentheses less deeply. '
 
         MESSGE(66) = 'Format error: Invalid tab specifier. A '
     .   //           'specifier beginning with "T" is not a '
     .   //           'correct tab specifier. Correct the '
     .   //           'specifier beginning with "T". '
 
         MESSGE(67) = 'Format error: Invalid blank specifier. A '
     .   //           'specifier beginning with "B" did not have '
     .   //           '"N" or "Z" as the next character. Correct '
     .   //           'the specifier beginning with "B". '
 
         MESSGE(68) = 'Format error: Specifier expected but end '
     .   //           'of format found. The end of the format '
     .   //           'was reached when another specifier was '
     .   //           'expected. Check the end of the format for '
     .   //           'a condition that would lead the processor '
     .   //           'to look for another specifier (possibly a '
     .   //           'missing right parenthesis). '
 
         MESSGE(69) = 'Format error: Missing separator. Other '
     .   //           'specifier found when /, :, or ) expected. '
     .   //           'Insert separator where needed. '
 
         MESSGE(70) = 'Format error: Digit expected. Number not '
     .   //           'found following format descriptor '
     .   //           'requiring a field width. Specify field '
     .   //           'width where required. '
 
         MESSGE(71) = 'Format error: Period expected in floating '
     .   //           'point format descriptor. No period was '
     .   //           'found to specify the number of decimal '
     .   //           'places in an "F", "G", "E", or "D" format '
     .   //           'descriptor. Specify the number of decimal '
     .   //           'places for the field. '
 
         MESSGE(72) = 'Format error: Unbalanced parentheses. '
     .   //           'More right parentheses than left '
     .   //           'parentheses were found. Correct format so '
     .   //           'parentheses balance. '
 
         MESSGE(73) = 'Format error: Invalid string in format. '
     .   //           'String extends past the end of the format '
     .   //           'or is too long for buffer. Check for '
     .   //           'unbalanced quotation mark or for "H" '
     .   //           'format count too large; or break up long '
     .   //           'string. '
 
         MESSGE(74) = 'Record length different in subsequent '
     .   //           'OPEN. Record length specified in '
     .   //           'redundant OPEN conflicted with the value '
     .   //           'as opened. Only BLANK= specifier may be '
     .   //           'changed by a redundant OPEN. '
 
         MESSGE(75) = 'Record accessed past end of internal file '
     .   //           'record (variable). An attempt was made to '
     .   //           'transfer more characters than internal '
     .   //           'file length. Match READ or WRITE with '
     .   //           'internal file size. '
 
         MESSGE(76) = 'Illegal new file number requested in fset '
     .   //           'function. The file number requested to be '
     .   //           'set was not a legal file system file '
     .   //           'number. Check that the OPEN succeeded and '
     .   //           'the file number is correct. '
 
         MESSGE(77) = 'Unexpected character in "NAMELIST" read. '
     .   //           'An illegal character was found in '
     .   //           'NAMELIST-directed input. Be sure input '
     .   //           'data conforms to the syntax rules for '
     .   //           '"NAMELIST"-directed input; remove illegal '
     .   //           'character from data. '
 
         MESSGE(78) = 'Illegal subscript or substring in '
     .   //           '"NAMELIST" read. An invalid subscript or '
     .   //           'substring specifier was found in '
     .   //           'NAMELIST-directed input.  Possible '
     .   //           'causes:  bad syntax, subscript/substring '
     .   //           'component out-of-bounds, wrong number of '
     .   //           'subscripts, substring on non-CHARACTER '
     .   //           'variable. Check input data for syntax '
     .   //           'errors.  Be sure subscript/substring '
     .   //           'specifiers are correct for data type; '
     .   //           'specify only array elements within the '
     .   //           'bounds of the array being read. '
 
         MESSGE(79) = 'Too many values in "NAMELIST" read. Too '
     .   //           'many input values were found during a '
     .   //           'NAMELIST-directed READ. This message will '
     .   //           'be generated by attempts to fill '
     .   //           'variables beyond their memory limits. '
     .   //           'Supply only as many values as the length '
     .   //           'of the array. '
 
         MESSGE(80) = 'Variable not in "NAMELIST" group. A '
     .   //           'variable name was encountered in the '
     .   //           'input stream which was not declared as '
     .   //           'part of the current NAMELIST group. Read '
     .   //           'only the variables in this NAMELIST. '
 
         MESSGE(81) = '"NAMELIST" I/O attempted on unformatted '
     .   //           'file. An illegal NAMELIST I/O operation '
     .   //           'was attempted on an unformatted (binary) '
     .   //           'file. OPEN file with FORM=''FORMATTED''; '
     .   //           'use NAMELIST I/O only on formatted files. '
 
         MESSGE(82) = 'Value out of range in numeric read. Value '
     .   //           'read for the numeric item is too big or '
     .   //           'too small. Read only the values that fit '
     .   //           'in the range of the numeric type being '
     .   //           'read. '
 
         MESSGE(83) = 'The value of IOSTAT was 982.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(84) = 'The value of IOSTAT was 983.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(85) = 'The value of IOSTAT was 984.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(86) = 'The value of IOSTAT was 985.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(87) = 'The value of IOSTAT was 986.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(88) = 'The value of IOSTAT was 987.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(89) = 'The value of IOSTAT was 988.  No '
     .   //           'explanation is provided in the HP '
     .   //           'documentation for this value of IOSTAT. '
 
         MESSGE(90) = '`Illegal FORTRAN NLS call: FORTRAN source '
     .   //           'code must be compiled with -Y. The '
     .   //           'FORTRAN source file was not compiled with '
     .   //           'the -Y option and NLS features were used. '
     .   //           'The problem is critical enough that '
     .   //           'program execution cannot continue. '
 
      ELSE IF ( SGI ) THEN
 
         LBND =  99
         UBND = 169
 
         MESSGE( 1) = 'error in format '
         MESSGE( 2) = 'out of space for unit table '
         MESSGE( 3) = 'formatted i/o not allowed '
         MESSGE( 4) = 'unformatted i/o not allowed '
         MESSGE( 5) = 'direct i/o not allowed '
         MESSGE( 6) = 'sequential i/o not allowed '
         MESSGE( 7) = 'can''t backspace file '
         MESSGE( 8) = 'null file name '
         MESSGE( 9) = 'can''t stat file '
         MESSGE(10) = 'unit not connected '
         MESSGE(11) = 'off end of record '
         MESSGE(12) = 'truncation failed in end file '
         MESSGE(13) = 'incomprehensible list input '
         MESSGE(14) = 'out of free space '
         MESSGE(15) = 'unit not connected '
         MESSGE(16) = 'read unexpected character '
         MESSGE(17) = 'blank logical input field '
         MESSGE(18) = 'bad variable type '
         MESSGE(19) = 'bad namelist name '
         MESSGE(20) = 'variable not in namelist '
         MESSGE(21) = 'no end record '
         MESSGE(22) = 'namelist subscript out of range '
         MESSGE(23) = 'negative repeat count '
         MESSGE(24) = 'illegal operation for unit '
         MESSGE(25) = 'off beginning of record '
         MESSGE(26) = 'no * after repeat count '
         MESSGE(27) = '''new'' file exists '
         MESSGE(28) = 'can''t find ''old'' file '
         MESSGE(29) = 'unknown system error '
         MESSGE(30) = 'requires seek ability '
         MESSGE(31) = 'illegal argument '
         MESSGE(32) = 'duplicate key value on write '
         MESSGE(33) = 'indexed file not open '
         MESSGE(34) = 'bad isam argument '
         MESSGE(35) = 'bad key description '
         MESSGE(36) = 'too many open indexed files '
         MESSGE(37) = 'corrupted isam file '
         MESSGE(38) = 'isam file not opened for exclusive access '
         MESSGE(39) = 'record locked '
         MESSGE(40) = 'key already exists '
         MESSGE(41) = 'cannot delete primary key '
         MESSGE(42) = 'beginning or end of file reached '
         MESSGE(43) = 'cannot find requested record '
         MESSGE(44) = 'current record not defined '
         MESSGE(45) = 'isam file is exclusively locked '
         MESSGE(46) = 'filename too long '
         MESSGE(47) = 'cannot create lock file '
         MESSGE(48) = 'record too long '
         MESSGE(49) = 'key structure does not match file '
     .   //           'structure '
         MESSGE(50) = 'direct access on an indexed file not '
     .   //           'allowed '
         MESSGE(51) = 'keyed access on a sequential file not '
     .   //           'allowed '
         MESSGE(52) = 'keyed access on a relative file not '
     .   //           'allowed '
         MESSGE(53) = 'append access on an indexed file not '
     .   //           'allowed '
         MESSGE(54) = 'must specify record length '
         MESSGE(55) = 'key field value type does not match key '
     .   //           'type '
         MESSGE(56) = 'character key field value length too long '
         MESSGE(57) = 'fixed record on sequential file not '
     .   //           'allowed '
         MESSGE(58) = 'variable records allowed only on '
     .   //           'unformatted sequential file '
         MESSGE(59) = 'stream records allowed only on formatted '
     .   //           'sequential file '
         MESSGE(60) = 'maximum number of records in direct '
     .   //           'access file exceeded '
         MESSGE(61) = 'attempt to write to a readonly file '
         MESSGE(62) = 'must specify key descriptions '
         MESSGE(63) = 'carriage control not allowed for '
     .   //           'unformatted units '
         MESSGE(64) = 'indexed files only '
         MESSGE(65) = 'cannot use on indexed file '
         MESSGE(66) = 'cannot use on indexed or append file '
         MESSGE(67) = 'error in closing file '
         MESSGE(68) = 'invalid code in format specification '
         MESSGE(69) = 'invalid record number in direct access '
     .   //           'file '
         MESSGE(70) = 'cannot have endfile record on '
     .   //           'non-sequential file '
 
      ELSE IF ( VAX ) THEN
 
         LBND = 0
         UBND = 68
 
         MESSGE( 1) = 'Not a Fortran-specific error. '
         MESSGE( 2) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 2 '
         MESSGE( 3) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 3 '
         MESSGE( 4) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 4 '
         MESSGE( 5) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 5 '
         MESSGE( 6) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 6 '
         MESSGE( 7) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 7 '
         MESSGE( 8) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 8 '
         MESSGE( 9) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 9 '
         MESSGE(10) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 10 '
         MESSGE(11) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 11 '
         MESSGE(12) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 12 '
         MESSGE(13) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 13 '
         MESSGE(14) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 14 '
         MESSGE(15) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 15 '
         MESSGE(16) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 16 '
         MESSGE(17) = 'Syntax error in NAMELIST input. '
         MESSGE(18) = 'Too many values for NAMELIST variable. '
         MESSGE(19) = 'Invalid reference to variable in NAMELIST '
     .   //           'input. '
         MESSGE(20) = 'REWIND error. '
         MESSGE(21) = 'Duplicate file specifications. '
         MESSGE(22) = 'Input record too long. '
         MESSGE(23) = 'BACKSPACE error '
         MESSGE(24) = 'End-of-file during read. '
         MESSGE(25) = 'Record number outside range. '
         MESSGE(26) = 'OPEN or DEFINE FILE required. '
         MESSGE(27) = 'Too many records in IO statement. '
         MESSGE(28) = 'CLOSE error. '
         MESSGE(29) = 'File not found. '
         MESSGE(30) = 'Open failure. '
         MESSGE(31) = 'Mixed file access modes. '
         MESSGE(32) = 'Invalid logical unit number. '
         MESSGE(33) = 'ENDFILE error. '
         MESSGE(34) = 'Unit already open. '
         MESSGE(35) = 'Segmented record format error. '
         MESSGE(36) = 'Attempt to access non-existent record. '
         MESSGE(37) = 'Inconsistent record length. '
         MESSGE(38) = 'Error during write. '
         MESSGE(39) = 'Error during read. '
         MESSGE(40) = 'Recursive IO operation. '
         MESSGE(41) = 'Insufficient virtual memory. '
         MESSGE(42) = 'No such device. '
         MESSGE(43) = 'File name specification error. '
         MESSGE(44) = 'Inconsistent record type. '
         MESSGE(45) = 'Keyword value error in OPEN statement. '
         MESSGE(46) = 'Inconsistent OPENCLOSE parameters. '
         MESSGE(47) = 'Write to READONLY file. '
         MESSGE(48) = 'Invalid argument to Fortran Run-Time '
     .   //           'Library. '
         MESSGE(49) = 'Invalid key specification. '
         MESSGE(50) = 'Inconsistent key change or duplicate key. '
         MESSGE(51) = 'Inconsistent file organization. '
         MESSGE(52) = 'Specified record locked. '
         MESSGE(53) = 'No current record. '
         MESSGE(54) = 'REWRITE error. '
         MESSGE(55) = 'DELETE error. '
         MESSGE(56) = 'UNLOCK error. '
         MESSGE(57) = 'FIND error. '
         MESSGE(58) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 58 '
         MESSGE(59) = 'List-directed IO syntax error. '
         MESSGE(60) = 'Infinite format loop. '
         MESSGE(61) = 'Formatvariable-type mismatch. '
         MESSGE(62) = 'Syntax error in format. '
         MESSGE(63) = 'Output conversion error. '
         MESSGE(64) = 'Input conversion error. '
         MESSGE(65) = 'No diagnostics are available other than '
     .   //           'the value of IOSTAT is 65 '
         MESSGE(66) = 'Output statement overflows record. '
         MESSGE(67) = 'Input statement requires too much data. '
         MESSGE(68) = 'Variable format expression value error. '
 
      ELSE IF ( PC ) THEN
 
         LBND = 2
         UBND = 1
 
      ELSE
 
         LBND = 2
         UBND = 1
 
      END IF
 
 
      IF (      ( IOSTAT .GT. LBND )
     .    .AND. ( IOSTAT .LE. UBND ) ) THEN
 
         DIAGNS = MESSGE ( IOSTAT - LBND )
         FOUND  = .TRUE.
 
      ELSE
 
         DIAGNS = 'The value of IOSTAT was #.  The meaning of this '
     .   //       'value is not available via the SPICE system. '
     .   //       'Please consult your FORTRAN manual for the '
     .   //       'meaning of this code.'
 
         CALL REPMI ( DIAGNS, '#', IOSTAT, DIAGNS )
         FOUND = .FALSE.
 
      END IF
 
      RETURN
      END
