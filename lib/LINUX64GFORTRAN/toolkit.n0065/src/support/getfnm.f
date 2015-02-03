C$Procedure GETFNM ( Get a filename from standard input )
 
      SUBROUTINE GETFNM ( PRMPT, FSTAT, FNAME, VALID, MESSG )
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
      CHARACTER*(*)         MESSG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PRMPT      I   The prompt to use when asking for the filename.
C     FSTAT      I   Status of the file: 'OLD' or 'NEW'.
C     FNAME      O   A valid filename typed in by the user.
C     VALID      O   A logical flag indicating a valid filename.
C     MESSG      O   A descriptive message for an invalid filename.
C
C$ Detailed_Input
C
C     PRMPT    is a character string that will be displayed from the
C              active position of the cursor to the end of string
C              that lets a user know that input is expected.
C
C     FSTAT    This is the status of the filename entered. It should
C              be 'OLD' when prompting for the filename of a file which
C              already exists, and 'NEW' when prompting for the filename
C              of a file which does not already exist or is to be over
C              written.
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
C     MESSG    A brief descriptive message which describes why a
C              particular filename was not valid. Blank if a valid
C              filename is entered.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
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
C        CALL GETFNM( PRMPT, FSTAT, FNAME, VALID, MESSG )
C
C     The user sees the following displayed on his screen:
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
C-    SPICELIB Version 5.17.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 5.16.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 5.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 5.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 5.13.0, 14-DEC-2010 (EDW)
C
C        Declared PROMPT as EXTERNAL.
C
C        Unfied Version and Revision sections, eliminated Revision
C        section. Corrected error in 09-DEC-1999 Version entry.
C        Version ID changed to 5.0.9 from 7.0.0.
C
C-    Beta Version 5.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 5.11.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    Beta Version 5.10.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 5.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 5.8.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 5.7.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    Beta Version 5.6.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    Beta Version 5.5.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    Beta Version 5.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    Beta Version 5.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    Beta Version 5.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Beta Version 5.1.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Beta Version 5.1.0, 16-AUG-2000 (WLT)
C
C        Added the PC-LINUX environment
C
C-    Beta Version 5.0.9, 09-DEC-1999 (WLT)
C
C        This routine now calls EXPFNM_2 only in UNIX environments
C
C-    Beta Version 5.0.0, 20-JAN-1998 (NJB)
C
C        Now calls EXPFNM_2 to attempt to expand environment variables.
C
C        Fixed a typo or two at various places in the header.
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
C        The variable BADCHR was not saved which caused problems on some
C        computers. This variable is now saved.
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
      LOGICAL               RETURN
      LOGICAL               FAILED
 
      INTEGER               MAXNAM
      PARAMETER           ( MAXNAM = 1000 )
 
      CHARACTER*(MAXNAM)    NAMBUF
 
C
C     Local Parameters
C
      CHARACTER*(*)         DPRMPT
      PARAMETER           ( DPRMPT = 'Filename? ' )
 
      CHARACTER*(*)         OSTAT
      PARAMETER           ( OSTAT  = 'OLD' )
 
      CHARACTER*(*)         NSTAT
      PARAMETER           ( NSTAT  = 'NEW' )
 
 
 
 
C
C     Local Variables
C
      CHARACTER*(162)       BADCHR
      CHARACTER*(3)         STATUS
 
      INTEGER               I
      INTEGER               LENGTH
 
      LOGICAL               FIRST
C
C     Saved Variables
C
      SAVE BADCHR
      SAVE FIRST
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
         CALL CHKIN ( 'GETFNM' )
      END IF
C
C     If this is the first time this routine has been called, initialize
C     the ``bad character'' string.
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
C     Set the value of the valid flag to .TRUE.. We might as well assume
C     that the name entered will be a valid one.
C
      VALID = .TRUE.
C
C     Left justify and convert the file status to upper case for
C     comparisons.
C
      CALL LJUST ( FSTAT,  STATUS )
      CALL UCASE ( STATUS, STATUS )
C
C     Check to see if we have a valid status for the filename.
C
      IF ( ( STATUS .NE. OSTAT ) .AND.
     .     ( STATUS .NE. NSTAT ) ) THEN
 
         VALID = .FALSE.
         MESSG = 'The status '''//STATUS//''' was not recognized.'
         CALL CHKOUT ( 'GETFNM' )
         RETURN
 
      END IF
C
C     Read in a potential filename, and test it for validity.
C
      IF ( PRMPT .EQ. ' ' ) THEN
 
         CALL PROMPT( DPRMPT, FNAME )
 
      ELSE
 
         CALL PROMPT ( PRMPT, FNAME )
 
      END IF
 
C
C     The string we just obtained could be an environment variable.
C     If so, expand it.
C
      CALL EXPFNM_2 ( FNAME, NAMBUF )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'GETFNM' )
         RETURN
      END IF
 
      FNAME = NAMBUF
 
      IF ( FNAME .EQ. ' ' ) THEN
 
         VALID = .FALSE.
         MESSG = 'A blank filename is not valid.'
         CALL CHKOUT ( 'GETFNM' )
         RETURN
 
      END IF
 
C
C     Left justify the filename.
C
      CALL LJUST ( FNAME, FNAME )
 
C
C     Check for bad characters in the filename.
C
      LENGTH = LASTNB( FNAME )
      I      = CPOS ( FNAME(:LENGTH), BADCHR, 1 )
 
      IF ( I .GT. 0 ) THEN
 
         VALID = .FALSE.
         MESSG = 'Invalid filename. Illegal character encountered:' //
     .           ' decimal value: #'
         CALL REPMI  ( MESSG, '#', ICHAR(FNAME(I:I)), MESSG )
         CALL CHKOUT ( 'GETFNM' )
         RETURN
 
      END IF
 
C
C     We know that the filename that was entered was nonblank and had
C     no bad characters. So, now we take care of the status question.
C
      IF ( STATUS .EQ. OSTAT ) THEN
 
         IF ( .NOT. EXISTS( FNAME(1:RTRIM(FNAME))  ) ) THEN
 
            VALID = .FALSE.
            MESSG = 'The file does not exist.'
            CALL CHKOUT ( 'GETFNM' )
            RETURN
 
         END IF
 
      ELSE IF ( STATUS .EQ. NSTAT ) THEN
 
         IF ( EXISTS( FNAME(1:RTRIM(FNAME))  ) ) THEN
 
            VALID = .FALSE.
            MESSG = 'The file already exists.'
            CALL CHKOUT ( 'GETFNM' )
            RETURN
 
         END IF
 
      END IF
 
C
C     At this point, we have done the best we can. If the status
C     was new, we might still have an invalid filename, but the
C     exact reasons for its invalidity are system dependent.
C
      CALL CHKOUT ( 'GETFNM' )
 
      RETURN
      END
