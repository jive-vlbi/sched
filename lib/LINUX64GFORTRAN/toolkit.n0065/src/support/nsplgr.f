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
      SUBROUTINE NSPLG  ( COMMND, HIDDEN, VSTYLE, HSTYLE, CDELIM )
C
C$ Version
C
C-     Command Loop Configured Version 1.1.0, 21-JUN-1999 (WLT)
C
C         Placed RETURN before first entry point.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
C
C     Save the contents of the command to a log file and any save
C     file that might be open and active.
C
      IMPLICIT NONE
 
      CHARACTER*(*)         COMMND
      LOGICAL               HIDDEN
      CHARACTER*(*)         VSTYLE
      CHARACTER*(*)         HSTYLE
      CHARACTER*(*)         CDELIM
 
      EXTERNAL              NSPWLN
 
 
      INCLUDE              'commdpar.inc'
 
      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = COMSIZ + 1 )
 
      CHARACTER*(LNGSIZ)    MYSTR
      CHARACTER*(STYSIZ)    SEEN
      CHARACTER*(STYSIZ)    HIDE
      CHARACTER*(1)         DELIM
 
      SAVE
 
      DATA   MYSTR / ' ' /
      DATA   SEEN  / 'LEFT 1 RIGHT 78'                       /
      DATA   HIDE  / 'LEADER ;^ LEFT 1 RIGHT 78 HARDSPACE ^' /
      DATA   DELIM / ';' /
      RETURN
C
C     This entry point handles the logging of commands.
C
      ENTRY NSPLOG ( COMMND, HIDDEN )
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
      MYSTR = COMMND
C
C     Inhibit writing to the screen.
C
      CALL NSPIOH ( 'SCREEN' )
      CALL NSPIOA ( 'LOG'    )
 
      IF ( HIDDEN ) THEN
         CALL NICEPR_1   ( COMMND,  HIDE, NSPWLN )
      ELSE
         MYSTR = COMMND
         CALL    SUFFIX   ( DELIM,  0,    MYSTR  )
         CALL NICEPR_1    ( MYSTR,  SEEN, NSPWLN )
      END IF
C
C     Re-activate the screen for writing output.
C
      CALL NSPIOA ( 'SCREEN' )
      CALL NSPIOH ( 'LOG'    )
 
      RETURN
 
C
C     This entry point allows users to set the style used for
C     logging hidden and visible commands.
C
      ENTRY NSPLGS ( VSTYLE, HSTYLE, CDELIM )
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
      SEEN  = VSTYLE
      HIDE  = HSTYLE
      DELIM = CDELIM
 
      RETURN
 
C
C     This entry point allows users to get the style used for
C     logging hidden and visible commands.
C
      ENTRY NSPGLS ( VSTYLE, HSTYLE, CDELIM )
 
      VSTYLE = SEEN
      HSTYLE = HIDE
      CDELIM = DELIM
      RETURN
 
 
      END
