C$Procedure      CMMORE ( Command Loop---More Commands)
 
      LOGICAL  FUNCTION CMMORE ( COMMND )
 
C$ Abstract
C
C    Determine whether or not more command loop processing
C    should be performed.
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
C     Command Loop
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         COMMND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COMMND     I   A command to be processed by CMLOOP
C
C     The function returns .TRUE. if the command is not the "exit"
C     command. If it is the exit command it returns .FALSE.
C
C$ Detailed_Input
C
C     COMMND     A commmand that should be acted on by CMLOOP
C
C
C$ Detailed_Output
C
C     The function returns .TRUE. if this is not the exit command.
C     The meaning being "there is still more to do in CMLOOP."
C
C     If the input command is equivalent to the exit command
C     (Same words when converted to uppercase) The function
C     returns .FALSE.  The intended meaning is "there is nothing
C     left for CMLOOP to do but cleanup and return."
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is utility function for use by CMLOOP.  It is the
C     function tested each pass through the loop to see if the
C     loop has finished its work
C
C$ Examples
C
C     See CMLOOP.  There is no other use for this function.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    Command Loop Version 1.0.0, 4-AUG-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     More command processing required
C
C-&
C     SPICELIB Functions
C
      INTEGER               RTRIM
      LOGICAL               NECHR
 
C
C     Local Variables.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
 
      CHARACTER*(WDSIZE)    EXIT
 
      INTEGER               LC
      INTEGER               I
      INTEGER               R
 
      LOGICAL               FIRST
      SAVE
 
 
      DATA                  FIRST / .TRUE. /
 
 
C
C     On the first pass we fetch the "exit" command and
C     spruce it up a bit for use when comparing with
C     the input command.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL TRNLAT ( 'EXIT',       EXIT )
         CALL CMPRSS ( ' ', 1, EXIT, EXIT )
         CALL LJUST  (  EXIT,        EXIT )
         R  = RTRIM  (  EXIT )
 
      END IF
C
C     If the input command is shorter than the non-blank
C     length of EXIT, then this cannot be the exit command.
C     There is more to do.
C
C     Note we assign a value to CMMORE so that the compiler
C     won't have a fit about having a function unassigned.
C     The if conditions below ensure that we assign a value
C     but most compilers aren't smart enough to figure that
C     out.
C
      CMMORE = .TRUE.
      LC     = LEN(COMMND)
 
      IF ( LC .LT. R ) THEN
         CMMORE = .TRUE.
         RETURN
      END IF
 
C
C     Check to see if the input command matches the exit command.
C     We do this a character at a time.  We search from the
C     left to right, because most commands are not EXIT and this
C     allows us to quit early in the process.
C
      DO I = 1, R
 
         IF ( NECHR( COMMND(I:I), EXIT(I:I) ) ) THEN
            CMMORE = .TRUE.
            RETURN
         END IF
 
      END DO
 
C
C     It's looking like this might be it.  See if the rest of
C     the input command is blank.
C
      IF ( LC.EQ. R ) THEN
C
C        We've got an exact match.  There are no more commands
C        to look at.
C
         CMMORE = .FALSE.
 
      ELSE IF ( LC .GT. R ) THEN
C
C        There will be more commands only if the rest of the input
C        command is non-blank.
C
         CMMORE =  COMMND(R+1:) .NE. ' '
 
      END IF
 
      RETURN
      END
