C$Procedure      CMSTUP ( Command Loop Startup )
 
      SUBROUTINE CMSTUP
 
C$ Abstract
C
C     This routine performs command loop start ups associated
C     with information on the command line when the user
C     activated the program.
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
C      None.
C
C$ Keywords
C
C       Command Loop
C
C$ Declarations
 
      IMPLICIT NONE
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine examines the information supplied on the command
C     line when a program was started and sets the symbols indicating
C     whether or not the program is in batch mode and if appropriate
C     sets up to start a command procedure.
C
C     This routine works entirely by side effect.
C
C     Recognized flags are:
C
C     -b                for batch mode
C     -start filename   for starting a startup file.
C
C     Unrecognized options are ignored.
C
C$ Examples
C
C     See the command loop documentation
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 20-NOV-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Command loop set up.
C
C-&
 
C
C     Command loop fucntions
C
      LOGICAL               SETBAT
 
C
C     Below are the various sources from which
C     commands might come.
C
C     NONE
C     COMBUF
C     KEYBRD
C     INPFIL
C
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )
 
      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )
 
      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )
 
      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )
 
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )
 
      CHARACTER*(LNSIZE)    COMMND
      CHARACTER*(LNSIZE)    COMLIN
      CHARACTER*(LNSIZE)    FILE
 
      INTEGER               B
      INTEGER               E
      INTEGER               START
 
      LOGICAL               HAVGO
      LOGICAL               DOBTCH
      LOGICAL               HAVFIL
 
      CALL GETCML ( COMLIN )
 
      START  = 1
      HAVGO  = .FALSE.
      DOBTCH = .FALSE.
      HAVFIL = .FALSE.
 
 
      CALL FNDNWD ( COMLIN, START, B, E )
 
      DO WHILE ( B .GT. 0 )
 
         IF ( COMLIN(B:E) .EQ. '-b' ) THEN
 
            DOBTCH = .TRUE.
 
         ELSE IF ( COMLIN(B:E) .EQ. '-start' ) THEN
 
            HAVGO = .TRUE.
 
         ELSE IF ( HAVGO .AND. .NOT. HAVFIL ) THEN
 
            FILE   = COMLIN(B:E)
            HAVFIL = .TRUE.
 
         END IF
 
         START = E + 1
         CALL FNDNWD ( COMLIN, START, B, E )
      END DO
 
C
C     If we have a batch flag, notify NXTCOM
C
      IF ( DOBTCH ) THEN
 
         DOBTCH = SETBAT()
 
      END IF
 
      IF ( HAVGO .AND. HAVFIL ) THEN
 
         CALL TRNLAT ( 'START',  COMMND )
         CALL SUFFIX (  FILE, 1, COMMND )
         CALL PUTCOM (  COMMND,  COMBUF )
 
      END IF
 
 
      RETURN
      END
