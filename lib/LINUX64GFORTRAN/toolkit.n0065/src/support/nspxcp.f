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
      SUBROUTINE NSPXCP ( STRING, ERROR, SCREEN, LOGFIL )
 
      IMPLICIT NONE
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         ERROR ( 2 )
      CHARACTER*(*)         SCREEN
      CHARACTER*(*)         LOGFIL
 
      EXTERNAL               NSPWLN
 
 
 
      CHARACTER*(128)       LSTYLE
      CHARACTER*(128)       SSTYLE
      CHARACTER*(128)       MARGIN
 
      INTEGER               I
      LOGICAL               SCRSTT ( 3 )
      LOGICAL               SAVSTT ( 3 )
 
      SAVE
 
      DATA LSTYLE / 'LEFT 1 RIGHT 78' /
      DATA SSTYLE / 'LEFT 1 RIGHT 78' /
 
 
 
      RETURN
 
 
      ENTRY NSPERR ( STRING, ERROR )
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
C
C     This entry point is intended to be called once for
C     brief error diagnostics and a second time for more detailed
C     diagnostics.  We can tell which is which by examining the
C     first entry of the error array.  If it is non-blank this
C     must be the first such call (because we set it to blank
C     after we get done doing something with it).  A second
C     call can only happen if the special command
C     was entered by the user ('?').  In this case the command
C     manager will not reset the error array and not pass the
C     command to any other routines.  Instead it returns immediately
C     so that this routine can process the second part of the
C     error message.
C
 
      IF ( ERROR(1) .NE. ' ' ) THEN
C
C        We automatically clear the procedure stack whenever
C        an error occurs.
C
         CALL PRCLR ()
 
C
C        First inhibit writing to the log file.
C
         CALL NSPIOH ( 'LOG' )
 
C
C        Now write out only the first component of the error message.
C
         CALL NSPMRG   (            MARGIN         )
         CALL SUFFIX   ( SSTYLE, 1, MARGIN         )
         CALL NICEPR_1 ( ERROR(1),  MARGIN, NSPWLN )
 
C
C        Now inhibit writing to the screen or the save file. But
C        fetch their current state so that we can reset them
C        to exactly their current states.
C
         CALL NSPGST ( 'SCREEN', SCRSTT )
         CALL NSPGST ( 'SAVE',   SAVSTT )
 
         CALL NSPIOH ( 'SCREEN' )
         CALL NSPIOH ( 'SAVE'   )
 
C
C        Reactivate the log file.
C
         CALL NSPIOA ( 'LOG'    )
 
         DO I = 1, 2
            CALL NICEPR_1 ( ERROR(I), LSTYLE, NSPWLN )
         END DO
 
         CALL NSPPST ( 'SCREEN', SCRSTT )
         CALL NSPPST ( 'SAVE',   SAVSTT )
 
         ERROR(1) = ' '
         RETURN
 
      END IF
 
C
C     The only way to get here is for the user to have processed
C     the first half of an error and typed a question mark or
C     blank command. (This relies on all kinds of side effects.
C     Better talk to Bill if you want to be able to figure this out).
C
 
      IF ( STRING   .EQ. '?' ) THEN
 
         IF (ERROR(2) .EQ. ' ' ) THEN
            CALL TRNLAT ( 'NOMOREDIAGNOSTICS', ERROR(2) )
         END IF
C
C        We've already written the second part of the error
C        message to the log file, so we shall inhibit writing
C        there now.
C
         CALL NSPIOH ( 'LOG' )
 
         CALL NSPMRG   (            MARGIN         )
         CALL SUFFIX   ( SSTYLE, 1, MARGIN         )
         CALL NICEPR_1 ( ERROR(2),  MARGIN, NSPWLN )
C
C        Now re-activate the log file.
C
         CALL NSPIOA ( 'LOG' )
 
         ERROR(2) = ' '
 
      END IF
 
      RETURN
 
 
C
C     Set the style string that shall be used for printing
C     errors.
C
      ENTRY NSPSTY ( SCREEN, LOGFIL  )
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
      SSTYLE = SCREEN
      LSTYLE = LOGFIL
 
      RETURN
 
      END
