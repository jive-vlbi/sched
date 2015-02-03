C$Procedure ZZASCII ( determine/verify EOL terminators in a text file )
 
      SUBROUTINE ZZASCII ( FILE, LINE, CHECK, TERMIN )
 
      IMPLICIT NONE
 
C$ Abstract
C
C     Returns a string indicating the line terminators of an ASCII file
C     and, if requested, stops execution if the terminator does match
C     the one that is native to the platform on which the toolkit was
C     compiled.
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
C     None.
C
C$ Keywords
C
C     FILE TYPE
C
C$ Declarations
 
      CHARACTER*(*)         FILE
      CHARACTER*(*)         LINE
      LOGICAL               CHECK
      CHARACTER*(*)         TERMIN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FILE       I   Name of the text file to scan.
C     LINE       I   The work string for file reads.
C     CHECK      I   Flag directing to check for mismatched EOL.
C     TERMIN     0   The deduced terminator ID.
C
C$ Detailed_Input
C
C     FILE       the name of the ASCII file to scan for a line
C                terminator
C
C     LINE       a character string of sufficient length to perform the
C                line reads from FILE.
C
C     CHECK      a logical flag that, if set to .TRUE., instructs this
C                routine to check terminator that has been determined
C                against the one that is native to the platform, on
C                which the toolkit was compiled, and to generate error
C                if it was not the case. If set to .FALSE., instructs
C                the routine to bypass the check.
C
C$ Detailed_Output
C
C     TERMIN     the terminator ID extracted from FILE. The possible
C                values:
C
C                'CR'    - carriage return (Mac classic)
C                'LF'    - line feed (Unix)
C                'CR-LF' - carriage return and line feed (DOS)
C                '?'     - unable to determine, possibly
C                          due to an error event
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) A SPICE(STRINGTOOSHORT) error signals if LINE has length less
C        than 3.
C
C     2) A SPICE(FILEOPENFAILED) error signals if the file of interest
C        fails to open, i.e. IOSTAT < 0.
C
C     3) A text kernel found to contain non-native line terminators
C        and abort of the run was requested by causes this routine to
C        signal the error SPICE(INCOMPATIBLEEOL).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The function scans a string read from a text file to determine
C     the native platform of the file. The functions response is
C     unpredictable if it scans a binary file.
C
C$ Examples
C
C     To return EOL terminator for a given file:
C
C         CHARACTER*(5)    TERMIN
C         CHARACTER*(64)   LINE
C
C          ... given a file name
C          ... and a line long enough to hold a text string
C              from FILE
C
C         CALL ZZASCII( FILE, LINE, .FALSE., TERMIN )
C
C         CALL TOSTDO( 'FOUND FILE TERMINATOR '//TERMIN )
C
C     To stop if EOL terminator for a given file, if detected
C     successfully, is not native to this platform:
C
C         CHARACTER*(5)    TERMIN
C         CHARACTER*(64)   LINE
C
C          ... given a file name
C          ... and a line long enough to hold a text string
C              from FILE
C
C         CALL ZZASCII( FILE, LINE, .TRUE., TERMIN )
C
C     If the EOL terminator was not native, the call will generate
C     SPICE(INCOMPATIBLEEOL) error.
C
C$ Restrictions
C
C     1) The terminator detection is not performed if the read from
C        the file fails because the file is smaller than the allocated
C        LINE size or for any other reason.
C
C     2) The terminator detection is not possible if the length of the
C        first text line in the file exceeds the length of the LINE
C        work space.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C     B.V. Semenov     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.3.1, 26-OCT-2006 (EDW)
C
C        Expanded error message explanation the
C        routine outputs when the file-of-interest
C        includes non-native text line terminators.
C
C-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 1.0.0, 17-FEB-2004 (EDW) (BVS)
C
C-&
 
C$ Index_Entries
C
C     determine ascii text file end-of-line type
C
C-&
 
C
C     SPICELIB functions.
C
      LOGICAL               EQSTR
      LOGICAL               RETURN
      INTEGER               RTRIM
 
C
C     Local parameters.
C
      INTEGER               EOLTSZ
      PARAMETER           ( EOLTSZ = 5 )
 
C
C     Local variables.
C
      CHARACTER*(EOLTSZ)    NATIVE
 
      INTEGER               DOSCNT
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               MACCNT
      INTEGER               NUMBER
      INTEGER               RECLEN
      INTEGER               UNXCNT
 
C
C     Discovery check-in. Can't determine the terminator in RETURN
C     mode.
C
      IF ( RETURN() ) THEN
         TERMIN = '?'
         RETURN
      END IF
 
C
C     Check-in to the error system.
C
      CALL CHKIN( 'ZZASCII' )
 
C
C     Retrieve the native line terminator.
C
      CALL ZZPLATFM ( 'TEXT_FORMAT', NATIVE )
 
C
C     If it is VAX, return immediately with undefined terminator.
C
      IF ( EQSTR( NATIVE, 'VAX' ) ) THEN
 
         TERMIN = '?'
 
         CALL CHKOUT ( 'ZZASCII' )
         RETURN
 
      END IF
 
C
C     Set the record lenght that will be used to read data from
C     the file.
C
      RECLEN  = LEN(LINE)
 
C
C     Check the length of the work string is sufficient to perform the
C     operations. Less than 3 is a no-op.
C
      IF ( LEN(LINE) .LT. 3 ) THEN
 
         TERMIN = '?'
 
         CALL SETMSG ( 'Work string lacks sufficient length'
     .              // ' to perform operation.')
         CALL SIGERR ( 'SPICE(STRINGTOOSHORT)' )
         CALL CHKOUT ( 'ZZASCII' )
         RETURN
 
      END IF
 
C
C     Find a free logical unit for file access.
C
      CALL GETLUN ( NUMBER )
 
C
C     Open the file for DIRECT access.
C
      OPEN ( UNIT               = NUMBER  ,
     .       FILE               = FILE( :RTRIM(FILE) ),
     .       ACCESS             = 'DIRECT',
     .       STATUS             = 'OLD'   ,
     .       RECL               = RECLEN  ,
     .       IOSTAT             = IOSTAT  )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
C
C        The open failed, can't determine the terminator if the routine
C        can't open the file.
C
         TERMIN = '?'
 
C
C        Execute a close, J.I.C.
C
         CLOSE( NUMBER )
 
         CALL SETMSG ( 'File open failed for file ''$1''. IOSTAT '
     .               //' value $2.' )
         CALL ERRCH  ( '$1', FILE            )
         CALL ERRINT ( '$2', IOSTAT          )
         CALL SIGERR ( 'SPICE(FILEOPENFAIL)' )
         CALL CHKOUT ( 'ZZASCII' )
         RETURN
 
      END IF
 
C
C     Read a line into the LINE variable assigned by the user.
C
      LINE = ' '
 
      READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) LINE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
C
C        If something went wrong during this read, a part or the whole
C        returned line may contain garbage. Instead of examining it and
C        making wrong determination based on it, set terminator to
C        undefined and return.
C
         TERMIN = '?'
 
C
C        Execute a close, J.I.C.
C
         CLOSE( NUMBER )
 
         CALL CHKOUT ( 'ZZASCII' )
         RETURN
 
      END IF
 
C
C     We have a line of text data. Use ICHAR to scan for carriage
C     returns and line feeds and count how may of various recognized
C     line termination sequences are in this line.
C
      DOSCNT = 0
      UNXCNT = 0
      MACCNT = 0
 
      I      = 1
 
      DO WHILE ( I .LT. LEN(LINE) )
 
C
C        Check for ICHAR values of 10 (LF) and 13 (CR).
C
         IF      ( ICHAR(LINE(I:I)) .EQ. 10 ) THEN
 
C
C           Found a UNIX line terminator LF.
C
            UNXCNT = UNXCNT + 1
 
         ELSE IF ( ICHAR(LINE(I:I)) .EQ. 13 ) THEN
 
C
C           Found CR, increment character counter and check
C           the next character.
C
            I = I + 1
 
            IF ( ICHAR(LINE(I:I)) .EQ. 10 ) THEN
 
C
C              Found a DOS line terminator CR+LF.
C
               DOSCNT = DOSCNT + 1
 
            ELSE
 
C
C              Found a Classic Mac line terminator CR.
C
               MACCNT = MACCNT + 1
 
            END IF
 
         END IF
 
         I = I + 1
 
      END DO
 
C
C     Examine the counters.
C
      IF      ( DOSCNT .GT. 0 .AND.
     .          UNXCNT .EQ. 0 .AND.
     .          MACCNT .EQ. 0       ) THEN
 
C
C        Only DOS terminator counter is non-zero. ID the file as DOS.
C
         TERMIN = 'CR-LF'
 
      ELSE IF ( DOSCNT .EQ. 0 .AND.
     .          UNXCNT .GT. 0 .AND.
     .          MACCNT .EQ. 0       ) THEN
 
 
C
C        Only Unix terminator counter is non-zero. ID the file as UNIX.
C
         TERMIN =  'LF'
 
      ELSE IF ( DOSCNT .EQ. 0 .AND.
     .          UNXCNT .EQ. 0 .AND.
     .          MACCNT .GT. 0       ) THEN
 
C
C        Only Mac terminator counter is non-zero. ID the file as Mac
C        Classic.
C
         TERMIN =  'CR'
 
      ELSE
 
C
C        We can get here in two cases. First if the line did not
C        contain any CRs or LFs. Second if the line contained more than
C        one kind of terminators. In either case the format of the file
C        is unclear.
C
         TERMIN = '?'
 
      END IF
 
C
C     Close the file.
C
      CLOSE ( NUMBER )
 
C
C     If we were told check the terminator against the native one, do
C     it.
C
      IF ( CHECK ) THEN
 
C
C        If the terminator was identified and does not match the native
C        one, error out.
C
         IF ( .NOT. EQSTR( TERMIN, NATIVE ) .AND.
     .        .NOT. EQSTR( TERMIN, '?'    )       ) THEN
 
 
            CALL SETMSG ( 'Text file ''$1'' contains lines '
     .               //   'terminated with ''$2'' while '
     .               //   'the expected terminator for '
     .               //   'this platform is ''$3''. SPICE '
     .               //   'cannot process the file in the '
     .               //   'current form. This problem '
     .               //   'likely occurred because the '
     .               //   'file was copied in binary mode '
     .               //   'between operating systems where '
     .               //   'the operating systems use different '
     .               //   'text line terminators. Try '
     .               //   'converting the file to native text '
     .               //   'form using a utility such as '
     .               //   'dos2unix or unix2dos.'     )
 
            CALL ERRCH  ( '$1', FILE   )
            CALL ERRCH  ( '$2', TERMIN )
            CALL ERRCH  ( '$3', NATIVE )
            CALL SIGERR ( 'SPICE(INCOMPATIBLEEOL)' )
            CALL CHKOUT ( 'ZZASCII' )
            RETURN
 
         END IF
 
      END IF
 
      CALL CHKOUT( 'ZZASCII' )
 
      RETURN
      END
