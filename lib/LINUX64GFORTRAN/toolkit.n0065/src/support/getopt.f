C$Procedure GETOPT ( Get an option from a menu )
 
      SUBROUTINE GETOPT ( TITLE, NOPT, OPTNAM, OPTTXT, OPTION )     
 
C$ Abstract
C
C     Display a list of options in a standard menu format and get
C     an option from a user returning the corresponding index of
C     the option selected.
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

      EXTERNAL              PROMPT

      CHARACTER*(*)         TITLE
      INTEGER               NOPT
      CHARACTER*(*)         OPTNAM(*)
      CHARACTER*(*)         OPTTXT(*)
      INTEGER               OPTION
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TITLE      I   Title for the menu.
C     NOPT       I   Number of options available.
C     OPTNAM     I   Names for the options.
C     OPTTXT     I   Brief text describing an option.
C     OPTVAL     I   The value returned when its option is selected.
C     OPTION     O   The number of the option selected.
C
C$ Detailed_Input
C
C     TITLE    Title for the option menu.
C
C     NOPT     The number of menu options to be displayed.
C     
C     OPTNAM   A list of single character names for the menu options.
C              These are the names used to select an option. The names
C              must each be a single alphanumeric character. All names
C              must be upper case if they are characters.
C              
C              If the option names is a period, '.', then a blank line
C              is to be displayed at that position in the menu list.
C              
C     OPTTXT   A list of character strings which contain brief
C              descriptions for each of the menu options. These
C              character strings should be kept relatively short.
C
C     Please note that the lengths of the option names, OPTNAM, and
C     the descriptive text for each option, OPTTXT, should be kept
C     reasonable, they both need to fit on the same output line with
C     a width of 80 characters. 13 characters out of the 80 available
C     are used for spacing and menu presentation, so there are 67
C     characters available for the option name and the descriptive text
C     combined.
C              
C$ Detailed_Output
C
C     OPTION   The index of the option selected from the menu.
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
C     2)   If the option names are not all upper case alphanumeric
C          characters, the error SPICE(BADOPTIONNAME) will be signalled.
C          
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will display a menu of options in a standardized
C     format, promting for the selection of one of the listed options.
C     This routine will not return to the caller until one of the
C     supplied options has been selected or an error occurs.
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
C     This routine makes explicit use fo the ASCII character sequence.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    Beta Version 4.2.0, 18-DEC-2010 (EDW)
C
C        Declared PROMPT as EXTERNAL. Eliminated unneeded Revisions 
C        section.
C
C-    Beta Version 4.1.0, 05-JUL-1995  (KRG)
C
C        Removed the initial blank line that was printed before the 
C        title of the menu. The calling program should determine the 
C        whitespace requirements for the appearance of the menu 
C        displayed by this routine.
C
C-    Beta Version 4.0.0, 25-APR-1994  (KRG)
C
C        Modified the routine to output the index into the list of menu
C        options rather than a character string representing the option 
C        selected. Also removed several calling arguments that were not 
C        needed anymore.
C
C        Added the capability of inserting a blank line into the menu.
C        This is done by placing a period, '.', into the option name 
C        location where the blank line lshould occur.
C
C        Added the missing $ Index_Entries section to the header.
C
C        Clarified a few of the comments in the header.
C
C-    Beta Version 3.0.0, 03-SEP-1992  (KRG)
C
C-&
 
C$ Index_Entries
C
C      display a menu and get a user's selection
C
C-&


C
C     SPICELIB functions
C
      INTEGER               RTRIM
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local Parameters
C
      INTEGER               TB4POS
      PARAMETER           ( TB4POS = 4       )
      
      INTEGER               TTLPOS
      PARAMETER           ( TTLPOS = 10      )
      
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80      )

C
C     Mnemonic for the standard output.
C
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 6 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    PRMPT
      CHARACTER*(LINLEN)    MSG
       
      INTEGER               I
      INTEGER               IOPT
      
      LOGICAL               DONE
      LOGICAL               OK
      LOGICAL               OKALPH
      LOGICAL               OKDIGI
      LOGICAL               OKEQU

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETOPT' )
      END IF
C
C     Check to make sure that the number of menu options is positive.
C     if it is not, then signal an error with an appropriate error
C     message.
C     
      IF ( NOPT .LT. 1 ) THEN
      
         CALL SETMSG ( 'The number of options was not positive: #.' )
         CALL ERRINT ( '#', NOPT                                    )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                     )
         CALL CHKOUT ( 'GETOPT'                                     )
         RETURN
         
      END IF
C
C     Initialize the option prompt.
C     
      PRMPT = ' '
      PRMPT(TB4POS:) = 'Option: '
C
C     Check to make sure that all of the option names are alphanumeric
C     and uppercase. The only exception is the period, which signals a
C     blank line.
C     
      OK = .TRUE.
      DO I = 1, NOPT
      
         OKDIGI = ( ICHAR ( OPTNAM(I) ) .GE. ICHAR ( '0' ) ) .AND.
     .            ( ICHAR ( OPTNAM(I) ) .LE. ICHAR ( '9' ) )
     
         OKALPH = ( ICHAR ( OPTNAM(I) ) .GE. ICHAR ( 'A' ) ) .AND.
     .            ( ICHAR ( OPTNAM(I) ) .LE. ICHAR ( 'Z' ) )
     
         OKEQU  = ( ICHAR ( OPTNAM(I) ) .EQ. ICHAR ( '.' ) ) 
         
         OK = OK .AND. ( OKDIGI .OR. OKALPH .OR. OKEQU )
     
         IF ( .NOT. OK ) THEN
         
            CALL SETMSG ( 'An illegal option name was found:'  //
     .                    ' option #, name ''#''. '             )
            CALL ERRINT ( '#', I                                )
            CALL SIGERR ( 'SPICE(ILLEGALOPTIONNAME)'            )
            CALL CHKOUT ( 'GETOPT'                              )
            RETURN
            
         END IF
         
      END DO
C
C     Do until we get a valid option.
C     
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )
C
C        Display the menu title if it is non blank
C        
         IF ( TITLE .NE. ' ' ) THEN
         
            LINE = ' '
            LINE (TTLPOS:) = '#'
            CALL REPMC ( LINE, '#', TITLE, LINE )
            CALL WRITLN ( LINE, STDOUT )
            
         END IF
C
C        Display the menu and read in an option.
C
         CALL WRITLN ( ' ', STDOUT )         
         
         DO I = 1, NOPT
            
            LINE = ' '
            IF ( OPTNAM(I) .NE. '.' ) THEN
            
               LINE(TB4POS:) = '( # ) #'
               CALL REPMC ( LINE, '#', OPTNAM(I), LINE )
               CALL REPMC ( LINE, '#', OPTTXT(I), LINE )
               
            END IF
            
            CALL WRITLN ( LINE, STDOUT )
            
         END DO
         
         CALL WRITLN ( ' ', STDOUT )         

         I = RTRIM ( PRMPT ) + 1
         CALL PROMPT ( PRMPT(:I), LINE )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'GETOPT' )
            RETURN
         END IF
C
C        Initialize the option value to zero, invalid option.
C     
         IOPT = 0
         IF ( LINE .EQ. ' ' ) THEN

            CALL WRITLN ( ' ', STDOUT )

         ELSE
         
            CALL LJUST ( LINE, LINE )
            CALL UCASE ( LINE, LINE )
C
C           Check to make sure that the option we got is a valid
C           candidate: It must be alpha numeric.
C           
            OKDIGI = ( ICHAR ( LINE(1:1) ) .GE. ICHAR ( '0' ) ) .AND.
     .               ( ICHAR ( LINE(1:1) ) .LE. ICHAR ( '9' ) )
     
            OKALPH = ( ICHAR ( LINE(1:1) ) .GE. ICHAR ( 'A' ) ) .AND.
     .               ( ICHAR ( LINE(1:1) ) .LE. ICHAR ( 'Z' ) )
         
            OK = OKDIGI .OR. OKALPH
C
C           If we got a valid candidate for an option, see if it is one
C           of the options that we are supplying.
C           
            IF ( OK ) THEN
            
               IOPT = ISRCHC ( LINE(1:1), NOPT, OPTNAM )
               OK   = IOPT .NE. 0
            
            END IF
         
            IF ( .NOT. OK ) THEN
         
               MSG = '''#'' was not a valid option.' //
     .               ' Please try again.'
               CALL REPMC ( MSG, '#', LINE(1:1), MSG )
               CALL WRITLN ( ' ', STDOUT ) 
               LINE          = ' '
               LINE(TB4POS:) = '***'
               CALL WRITLN ( LINE, STDOUT ) 
               LINE(TB4POS:) = '*** #'
               CALL REPMC ( LINE, '#', MSG, LINE )
               CALL WRITLN ( LINE, STDOUT ) 
               LINE(TB4POS:) = '***'
               CALL WRITLN ( LINE, STDOUT ) 
               CALL WRITLN ( ' ', STDOUT ) 
            
            ELSE
      
               OPTION = IOPT
               DONE   = .TRUE.
         
            END IF
         
         END IF
         
      END DO
      
      CALL CHKOUT ( 'GETOPT' )
      RETURN
      END
