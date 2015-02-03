C$Procedure GETFNM_1 ( Get a filename from standard input )
 
      SUBROUTINE GETFNM_1 ( PRMPT, FSTAT, FNAME, VALID )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine prompts the user for a valid filename.
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
C     UTILITY
C
C$ Declarations
 
      EXTERNAL              PROMPT
 
      CHARACTER*(*)         PRMPT
      CHARACTER*(*)         FSTAT
      CHARACTER*(*)         FNAME
      LOGICAL               VALID
 
      INTEGER               PRMLEN
      PARAMETER           ( PRMLEN = 80 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PRMPT      I   The prompt to use when asking for the filename.
C     FSTAT      I   Status of the file: 'OLD' or 'NEW'.
C     FNAME      O   A valid filename typed in by the user.
C     VALID      O   A logical flag indicating a valid filename.
C     PRMLEN     P   Maximum length allowed for a prompt before
C                    truncation.
C
C$ Detailed_Input
C
C     PRMPT    is a character string that will be displayed from the
C              current cursor position that informs a user that input
C              is expected. Prompts should be fairly short, since we
C              need to declare some local storage. The current maximum
C              length of a prompt is given by the parameter PRMLEN.
C
C     FSTAT    This is the status of the filename entered. It should
C              be 'OLD' when prompting for the filename of a file which
C              already exists, and 'NEW' when prompting for the
C              filename of a file which does not already exist or is to
C              be over written.
C
C$ Detailed_Output
C
C     FNAME    is a character string that contains a valid filename
C              typed in by the user. A valid filename is defined
C              simply to be a nonblank character string with no
C              embedded blanks, nonprinting characters, or characters
C              having decimal values > 126.
C
C     VALID    A logical flag which indicates whether or not the
C              filename entered is valid, i.e., a nonblank character
C              string with no leading or embedded blanks, which
C              satisfies the constraints for validity imposed.
C
C$ Parameters
C
C     PRMLEN   The maximum length for an input prompt string.
C
C$ Exceptions
C
C     1) If the input file status is not equal to 'NEW' or 'OLD' after
C        being left justified and converted to upper case, the error
C        SPICE(INVALIDARGUMENT) will be signalled. The error handling
C        is then reset.
C
C     2) If the filename entered at the prompt is blank, the error
C        SPICE(BLANKFILENAME) will be signalled. The error handling is
C        then reset.
C
C     3) If the filename contains an illegal character, a nonprinting
C        character or embedded blanks, the error
C        SPICE(ILLEGALCHARACTER) will be signalled.
C
C     4) If the file status is equal to 'OLD' after being left
C        justified and converted to upper case and the file specified
C        by the filename entered at the prompt does not exist, the
C        error SPICE(FILEDOESNOTEXIST) will be signalled.
C
C     5) If the file status is equal to 'NEW' after being left
C        justified and converted to upper case and the file specified
C        by the filename entered at the prompt already exists, the
C        error SPICE(FILEALREADYEXISTS) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility that allows you to "easily" request a valid,
C     filename from a program user.  At a high level, it frees you
C     from the peculiarities of a particular FORTRAN's implementation
C     of cursor control.
C
C     A valid filename is defined as a nonblank character string with
C     no embedded blanks, nonprinting characters, or characters with
C     decimal values > 126. Leading blanks are removed, and trailing
C     blanks are ignored.
C
C     If an invalid filename is entered, this routine provides a
C     descriptive error message and halts the execution of the
C     process which called it by using a Fortran STOP.
C
C$ Examples
C
C     EXAMPLE 1:
C
C        FNAME = ' '
C        PRMPT = 'Filename? '
C        FSTAT = 'OLD'
C
C        CALL GETFNM_1( PRMPT, FSTAT, FNAME, VALID )
C
C     The user sees the following displayed on the screen:
C
C        Filename? _
C
C     where the underbar, '_', represents the cursor position.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.17.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 6.16.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 6.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 6.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.13.0, 14-DEC-2010 (EDW)
C
C        Declared PROMPT as EXTERNAL.
C
C        Unfied Version and Revision sections, eliminated Revision
C        section. Corrected error in 09-DEC-1999 Version entry.
C        Version ID changed to 6.0.9 from 7.0.0.
C
C-    Beta Version 6.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 6.11.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    Beta Version 6.10.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 6.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 6.8.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 6.7.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    Beta Version 6.6.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    Beta Version 6.5.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    Beta Version 6.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    Beta Version 6.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    Beta Version 6.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Beta Version 6.1.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Beta Version 6.1.0, 16-AUG-2000 (WLT)
C
C        Added PC-LINUX environment
C
C-    Beta Version 6.0.9, 09-DEC-1999 (WLT)
C
C        This routine now calls EXPFNM_2 only UNIX environments
C
C-    Beta Version 6.0.0, 20-JAN-1998 (NJB)
C
C        Now calls EXPFNM_2 to attempt to expand environment variables.
C
C        Fixed a typo or two at various places in the header.
C
C-    Beta Version 5.1.0, 31-JAN-1996 (KRG)
C
C        Fixed a pedantic Fortran syntax error dealing with input
C        strings that are dimensioned CHARACTER*(*).
C
C        A local character string is now declared, and a parameter,
C        PRMLEN, has been added to the interface description for this
C        subroutine. PRMLEN defines the maximum length allowed for a
C        prompt before it is truncated.
C
C-    Beta Version 5.0.0, 05-JUL-1995 (KRG)
C
C        Modified the routine to handle all of its own error messages
C        and error conditions. The routine now signals an error
C        immediately resetting the error handling when an exceptional
C        condition is encountered. This is done so that input attempts
C        may continue until a user decides to stop trying.
C
C        Added several exceptions to the $ Exceptions section of the
C        header.
C
C-    Beta Version 4.0.1, 25-APR-1994 (KRG)
C
C        Removed some incorrect comments from the $ Particulars section
C        of the header. Something about a looping structure that is not
C        a part of the code now, if it ever was.
C
C        Fixed a typo or two at various places in the header.
C
C-    Beta Version 4.0.0, 29-SEP-1993 (KRG)
C
C        Added the character reperesnted by decimal 127 to the BADCHR.
C        It should have been there, but it wasn't.
C
C-    Beta Version 3.0.0, 10-SEP-1993 (KRG)
C
C        Made the file status variable FSTAT case insensitive.
C
C        Added code to the  file status .EQ. 'NEW' case to set the
C        valid flag to .FALSE. and set an appropriate error message
C        about the file already existing.
C
C-    Beta Version 2.0.0, 02-APR-1993 (KRG)
C
C        The variable BADCHR was not saved which caused problems on
C        some computers. This variable is now saved.
C
C-    Beta Version 1.0.0, 01-JUN-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      prompt for a filename with error handling
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               CPOS
      INTEGER               LASTNB
      INTEGER               RTRIM
 
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local Parameters
C
      CHARACTER*(*)         DPRMPT
      PARAMETER           ( DPRMPT = 'Filename? ' )
 
      CHARACTER*(*)         OSTAT
      PARAMETER           ( OSTAT = 'OLD' )
 
      CHARACTER*(*)         NSTAT
      PARAMETER           ( NSTAT = 'NEW' )
 
 
C
C     Maximum length of a filename.
C
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 1000 )
C
C     Length of an error action
C
      INTEGER               ACTLEN
      PARAMETER           ( ACTLEN =  10 )
C
C     Local Variables
C
      CHARACTER*(162)       BADCHR
      CHARACTER*(FNMLEN)    MYFNAM
      CHARACTER*(PRMLEN)    MYPRMT
      CHARACTER*(FNMLEN)    NAMBUF
      CHARACTER*(ACTLEN)    OLDACT
      CHARACTER*(3)         STATUS
 
      INTEGER               I
      INTEGER               LENGTH
 
      LOGICAL               TRYAGN
      LOGICAL               FIRST
      LOGICAL               MYVLID
C
C     Saved Variables
C
      SAVE                  BADCHR
      SAVE                  FIRST
C
C     Initial Values
C
      DATA                  FIRST   / .TRUE. /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETFNM_1' )
      END IF
C
C     We are going to be signalling errors and resetting the error
C     handling, so we need to be in RETURN mode. First we get the
C     current mode and save it, then we set the mode to return. Upon
C     leaving the subroutine, we will restore the error handling mode
C     that was in effect when we entered.
C
      CALL ERRACT ( 'GET', OLDACT   )
      CALL ERRACT ( 'SET', 'RETURN' )
C
C     If this is the first time this routine has been called,
C     initialize the ``bad character'' string.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         DO I = 0, 32
            BADCHR(I+1:I+1) = CHAR( I )
         END DO
 
         DO I = 1, 129
            BADCHR(33+I:33+I) = CHAR( 126+I )
         END DO
 
      END IF
C
C     Left justify and convert the file status to upper case for
C     comparisons.
C
      CALL LJUST ( FSTAT,  STATUS )
      CALL UCASE ( STATUS, STATUS )
C
C     Check to see if we have a valid status for the filename.
C
      IF ( ( STATUS .NE. OSTAT ) .AND. ( STATUS .NE. NSTAT ) ) THEN
 
         CALL SETMSG ( 'The file status ''#'' was not valid. The'
     .   //            ' file status must have a value of ''NEW'''
     .   //            ' or ''OLD''.'                              )
         CALL ERRCH  ( '#', STATUS                                 )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'GETFNM_1'                                  )
         RETURN
 
      END IF
C
C     Store the input value for the prompt into our local value. We do
C     this for pedantic Fortran compilers that issue warnings for
C     CHARACTER*(*) variables used with concatenation.
C
      MYPRMT = PRMPT
C
C     Read in a potential filename, and test it for validity.
C
      TRYAGN = .TRUE.
 
      DO WHILE ( TRYAGN )
C
C        Set the value of the valid flag to .TRUE.. We assume that the
C        name entered will be a valid one.
C
         MYVLID = .TRUE.
C
C        Get the filename.
C
         IF ( MYPRMT .EQ. ' ' ) THEN
            CALL PROMPT ( DPRMPT, MYFNAM )
         ELSE
            CALL PROMPT ( MYPRMT(:LASTNB(MYPRMT))//' ',  MYFNAM )
         END IF
 
         IF ( FAILED() ) THEN
            MYVLID = .FALSE.
         END IF
 
C
C        Translate the name if it's an environment variable.
C
         CALL EXPFNM_2 ( MYFNAM, NAMBUF )
 
         MYFNAM = NAMBUF
 
         IF ( FAILED() ) THEN
            MYVLID = .FALSE.
         END IF
 
         IF ( MYVLID ) THEN
 
            IF ( MYFNAM .EQ. ' ' ) THEN
 
               MYVLID = .FALSE.
 
               CALL SETMSG ( 'The filename entered was blank.' )
               CALL SIGERR ( 'SPICE(BLANKFILENAME)'            )
 
            END IF
 
         END IF
 
         IF ( MYVLID ) THEN
C
C           Left justify the filename.
C
            CALL LJUST ( MYFNAM, MYFNAM )
C
C           Check for bad characters in the filename.
C
            LENGTH = LASTNB( MYFNAM )
            I      = CPOS ( MYFNAM(:LENGTH), BADCHR, 1 )
 
            IF ( I .GT. 0 ) THEN
 
               MYVLID = .FALSE.
 
               CALL SETMSG ( 'The filename entered contains non'
     .         //            ' printing characters or embedded'
     .         //            ' blanks.'                          )
               CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'           )
 
            END IF
 
         END IF
 
         IF ( MYVLID ) THEN
C
C           We know that the filename that was entered was nonblank and
C           had no bad characters. So, now we take care of the status
C           question.
C
            IF ( STATUS .EQ. OSTAT ) THEN
 
               IF ( .NOT. EXISTS( MYFNAM(:RTRIM(MYFNAM)) ) ) THEN
 
                  MYVLID = .FALSE.
 
                  CALL SETMSG ( 'A file with the name ''#'' does not'
     .            //            ' exist.'                            )
                  CALL ERRCH  ( '#', MYFNAM                           )
                  CALL SIGERR ( 'SPICE(FILEDOESNOTEXIST)'             )
 
               END IF
 
            ELSE IF ( STATUS .EQ. NSTAT ) THEN
 
               IF ( EXISTS( MYFNAM(:RTRIM(MYFNAM)) ) ) THEN
 
                  MYVLID = .FALSE.
 
                  CALL SETMSG ( 'A file with the name ''#'' already'
     .            //            ' exists.'                           )
                  CALL ERRCH  ( '#', MYFNAM                          )
                  CALL SIGERR ( 'SPICE(FILEALREADYEXISTS)'           )
 
               END IF
 
            END IF
 
         END IF
 
         IF ( MYVLID ) THEN
 
            TRYAGN = .FALSE.
 
         ELSE
 
            CALL WRITLN ( ' ', 6 )
            CALL CNFIRM ( 'Try again? (Yes/No) ', TRYAGN )
            CALL WRITLN ( ' ', 6 )
 
            IF ( TRYAGN ) THEN
               CALL RESET
            END IF
 
 
         END IF
 
      END DO
C
C     At this point, we have done the best we can. If the status
C     was new, we might still have an invalid filename, but the
C     exact reasons for its invalidity are system dependent, and
C     therefore hard to test.
C
 
      VALID = MYVLID
 
      IF ( VALID ) THEN
         FNAME = MYFNAM(:RTRIM(MYFNAM))
      END IF
C
C     Restore the error action.
C
      CALL ERRACT ( 'SET', OLDACT )
 
      CALL CHKOUT ( 'GETFNM_1' )
      RETURN
 
      END
