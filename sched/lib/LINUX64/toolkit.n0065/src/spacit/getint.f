
C$Procedure      GETINT ( Prompt for an integer value )

      SUBROUTINE GETINT ( PRMPT, VALUE, GOTVAL, ERROR, ERRMSG )

C$ Abstract
C
C     This routine provides a portable means for interactively
C     obtaining an integer value.
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
C
      CHARACTER*(*)         PRMPT
      INTEGER               VALUE
      LOGICAL               GOTVAL
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      PRMPT     I    The prompt to be used for the desired value.
C      VALUE     O    The value entered.
C      GOTVAL    O    Flag indicating that a value was entered
C      ERROR     O    Flag indicating an error occurred.
C      ERRMSG    O    Descriptive error message
C
C$ Detailed_Input
C
C     PRMPT    The prompt to be used for the desired value.
C
C$ Detailed_Output
C
C     VALUE    The new value, if the flag GOTVAL = .TRUE., otherwise
C              nothing meaningful.
C     
C     GOTVAL   A logical flag indicating that a value has been
C              entered.
C              
C     ERROR    A logical flag indicating that an input error occurred.
C              
C              
C     ERRMSG   A descriptive error message when an error occurs,
C              blank otherwise.
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
C     This routine provides an easy to use portable interface for
C     obtaining variable values in an interactive fashion; error
C     information is returned.
C     
C     This is one in a family of routines, one for each data type:
C     
C           GETCHR -- Obtain a character string value 
C           GETDP  -- Obtain a double precision value 
C           GETINT -- Obtain an integer value
C
C$ Examples
C   
C     The following code fragment
C     
C     CALL GETINT ( 'Number? ', VALUE, GOTVAL, ERROR, ERRMSG )
C     
C     would display on the terminal screen
C        
C        Number? _
C        
C     And a user would then type in an integer value.
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
C-    Beta Version 1.0.0, 29-MAY-1992 (KRG)
C
C-&

C$ Index_Entries
C
C      prompt for an integer value
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Other functions
C     
      LOGICAL               BEINT
C
C     Local parameters
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    LINE
      INTEGER               ERRPTR
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETINT' )
      END IF
C
C     Set the initial values
C     
      GOTVAL = .FALSE.
      ERROR  = .FALSE.
      ERRMSG = ' '
C
C     Get the new value
C     
      IF ( PRMPT .NE. ' ' ) THEN
         CALL PROMPT( PRMPT, LINE )
      ELSE
         CALL PROMPT( 'Value: ', LINE )
      END IF
      
      IF ( LINE .NE. ' ' ) THEN
      
         IF ( BEINT( LINE ) ) THEN
         
            CALL NPARSI( LINE, VALUE, ERRMSG, ERRPTR )
            GOTVAL = .TRUE.
            
         ELSE
            
            ERROR  = .TRUE.
            ERRMSG = 'ERROR: ''#'' is not an integer.'
            CALL REPMC ( ERRMSG, '#', LINE, ERRMSG )
            
         END IF
         
      END IF
      
      CALL CHKOUT ( 'GETINT' )
      RETURN
      END
