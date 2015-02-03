C
C$ Procedure     GETOPT_2 ( Get option string from a specified list )
C
      SUBROUTINE GETOPT_2 ( TITLE,  TINDNT, NOPT, OPTNAM, OPTTXT,
     .                      OINDNT, OPTION )
      IMPLICIT NONE
 
C
C$ Abstract
C
C     Display a list of options in a standard menu format, and get
C     an option from a user returning the corresponding value from
C     a specified list of option values.
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
C     None.
C
C$ Declarations
 
      CHARACTER*(*)         TITLE
      INTEGER               TINDNT
      INTEGER               NOPT
      CHARACTER*(*)         OPTNAM(*)
      CHARACTER*(*)         OPTTXT(*)
      INTEGER               OINDNT
      INTEGER               OPTION
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TITLE      I   Title for the option menu.
C     NOPT       I   Number of options available.
C     OPTNAM     I   Names for the options (the selection names).
C     NAMLEN     I   Length of all of the option names.
C     OPTTXT     I   Brief text describing an option.
C     TXTLEN     I   Length of the descriptive text for all options.
C     OPTVAL     I   The value returned when its option is selected.
C     OPTION     O   The value of the option selected.
C
C$ Detailed_Input
C
C     TITLE    Title for the option menu.
C
C     NOPT     The number of menu options to be displayed.
C
C     OPTNAM   A list of short (mnemonic) names for the menu options.
C              These are the names used to selectan option.
C
C     NAMLEN   The maximum length of the short names for the menu
C              options. This number should probably be kept small,
C              say 6 characters or less.
C
C     OPTTXT   A list of character strings which contain brief
C              descriptions for each of the menu options. These
C              character strings should be kept relatively short.
C
C     TXTLEN   The maximum length of the brief descriptions of the
C              menu options. This number should probably be relatively
C              small small, say 50 characters or less.
C
C     OPTVAL   A list of textual values one of which will be returned
C              when a menu option is selected.
C
C$ Detailed_Output
C
C     OPTION   The value of the option selected from the menu, as
C              specified by the appropriate value of OPTVAL.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the number of options, NOPT, is not > 0, the error
C          SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If the length of the option names, NAMLEN, is  not > 0,
C          the error SPICE(INVALIDARGUMENT) will be signalled.
C
C     3)   If the length of the option text, TXTLEN, is  not > 0,
C          the error SPICE(INVALIDARGUMENT) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will display a menu of options in a standardized
C     format, promting for an option to be selected. This routine
C     will not return to the caller until one of the supplied options
C     has been selected.
C
C     Please note that the lengths of the option names, OPTNAM, and
C     the descriptive text for each option, OPTTXT, should be kept
C     reasonable, they both need to fit on the same output line with
C     a width of 80 characters. 13 characters out of the 80 available
C     are used for spacing and menu presentation, so there are 67
C     characters available for the option name and the descriptive text
C     combined.
C
C$ Examples
C
C     None.
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
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    Beta Version 3.0.0, 03-SEP-1992  (KRG)
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
      INTEGER               ISRCHC
      INTEGER               NBWID
 
      LOGICAL               RETURN
C
C     Local variables
C
      CHARACTER*(*)         STARS
      PARAMETER           ( STARS = '*****' )
 
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    MSG
      CHARACTER*(LINLEN)    MYOPT
      CHARACTER*(LINLEN)    SPACE
 
      INTEGER               ITASK
      INTEGER               NAMLEN
 
      LOGICAL               DONE
C
C     Saved variables
C
C     None.
C
C
C     Initial values
C
C     None.
C
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETOPT_2' )
      END IF
C
C     Check to make sure that the number of menu options is positive.
C     if it is not, then signal an error with an appropriate error
C     message.
C
      IF ( NOPT .LT. 1 ) THEN
 
         CALL SETMSG ( 'The number of options was not positive: #.' )
         CALL ERRINT ( '#', NOPT )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'GETOPT_2' )
         RETURN
 
      END IF
C
C     Do until we get an option
C
 
      NAMLEN = NBWID ( OPTNAM, NOPT )
 
      DONE  = .FALSE.
      SPACE = ' '
      DO WHILE ( .NOT. DONE )
C
C        Display the menu title if it is non blank
C
         IF ( TITLE .NE. ' ' ) THEN
 
            IF ( TINDNT .GT. 0 ) THEN
               LINE  = SPACE(1:TINDNT) // TITLE
            ELSE
               LINE  = TITLE
            END IF
 
            WRITE (*,*)
            WRITE (*,*) LINE
 
         END IF
         LINE = ' '
         WRITE (*,*) LINE(1:1)
 
         DO ITASK = 1, NOPT
 
            IF ( OPTNAM(ITASK) .NE. ' ' ) THEN
               MYOPT = '( '
     .         //      OPTNAM(ITASK)(1:NAMLEN)
     .         //      ' ) '
     .         //      OPTTXT(ITASK)
            ELSE
               MYOPT = SPACE(1:NAMLEN + 5)
     .         //      OPTTXT(ITASK)
            END IF
 
 
            IF ( OINDNT .GT. 0 ) THEN
               LINE = SPACE(1:OINDNT)
     .         //     MYOPT
            ELSE
               LINE = MYOPT
            END IF
 
            WRITE (*,*) LINE(1:RTRIM(LINE))
 
         END DO
 
C
C        Initialize the task indicator to zero, invalid task.
C
         ITASK  = 0
 
         WRITE (*,*)
         CALL PROMPT ( SPACE(1:OINDNT) // 'Option: ', LINE )
 
         IF ( LINE .NE. ' ' ) THEN
 
            CALL LJUST( LINE, LINE )
            CALL UCASE( LINE, LINE )
 
            ITASK = ISRCHC( LINE, NOPT, OPTNAM)
 
            IF ( ITASK .EQ. 0 ) THEN
 
               MSG =  STARS
     .         //    ' ''#'' was not a valid option.'
     .         //    ' Please try again.'
               CALL REPMC ( MSG, '#', LINE, MSG )
               WRITE (*,*)
               WRITE (*,*) STARS
               WRITE (*,*) MSG(:RTRIM(MSG))
               WRITE (*,*) STARS
 
            ELSE
 
               OPTION = ITASK
               DONE = .TRUE.
 
            END IF
 
         END IF
 
      END DO
 
      CALL CHKOUT ( 'GETOPT_2' )
      RETURN
      END
