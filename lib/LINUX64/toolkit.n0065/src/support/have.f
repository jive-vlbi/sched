C$Procedure      HAVE ( Do we have an error? )
 
      LOGICAL FUNCTION HAVE ( ERROR )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine if an error has occurred.
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
C     ERROR
C
C$ Keywords
C
C     ERROR
C
C$ Declarations
 
      CHARACTER*(*)         ERROR ( 2 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ERROR     I/O  Error message array.
C
C     The function returns .TRUE. if an error occurred.
C
C$ Detailed_Input
C
C     ERROR     is the character string array containing an error
C               message.
C
C$ Detailed_Output
C
C     ERROR     is the character string containing an error message.
C               If ERROR was blank on input and an error was detected
C               by the SPICELIB error handling mechanism, ERROR contains
C               the SPICELIB long error message on output.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Beta Version 1.0.0, 14-MAY-1992 (HAN)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
 
 
C
C     Local variables
C
      INTEGER               DEPTH
      INTEGER               I
 
      CHARACTER*(32)        NAME
      CHARACTER*(80)        SMS
 
 
C
C     Check to see if an error occurred.
C
      IF ( ( ERROR( 1 )(1:1) .NE. ' ' ) .OR. FAILED() ) THEN
 
         HAVE = .TRUE.
 
      ELSE
 
         HAVE = .FALSE.
         RETURN
 
      END IF
 
 
C
C     If an error was detected by the SPICELIB error handling and
C     the ERROR message is blank, we need to get the SPICELIB error
C     message. After that, reset the error handling.
C
      IF ( FAILED() .AND. ( ERROR( 1 ) .EQ. ' ' ) ) THEN
 
         CALL GETSMS ( SMS        )
         CALL GETLMS ( ERROR( 1 ) )
 
         CALL PREFIX ( '--', 0, ERROR( 1 ) )
         CALL PREFIX ( SMS,  0, ERROR( 1 ) )
 
         ERROR( 2 ) = 'SPICELIB Trace>'
         CALL TRCDEP ( DEPTH )
 
         DO I = 1, DEPTH
 
           CALL TRCNAM ( I,    NAME          )
 
           IF ( I .EQ. 1 ) THEN
              CALL SUFFIX ( NAME, 1, ERROR( 2 ) )
           ELSE
              CALL SUFFIX ( NAME, 0, ERROR( 2 ) )
           END IF
 
           IF ( I .NE. DEPTH ) THEN
              CALL SUFFIX ( ':',  0, ERROR( 2 ) )
           END IF
 
         END DO
 
         CALL RESET
 
C
C     It is possible that FAILED() is true, even though we already
C     had a recorded error.  To avoid having this show up in a later
C     command, we reset the SPICELIB error handling now.  This isn't
C     really a good solution, but a better one doesn't come to mind
C     at the moment.
C
      ELSE IF ( FAILED() ) THEN
 
         CALL RESET
 
      END IF
 
 
      RETURN
      END
