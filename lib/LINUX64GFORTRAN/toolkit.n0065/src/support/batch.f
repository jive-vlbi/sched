C$Procedure      BATCH (Tell whether or not a program is in batch mode)
 
      LOGICAL FUNCTION      BATCH ()
      IMPLICIT NONE
 
C$ Abstract
C
C     This function returns information regarding the interactive
C     status of a program.  If BATCH is TRUE the function is considered
C     to be in background mode.  If BATCH is FALSE the function is
C     considered to be in interactive mode.
C
C     To set a program in batch mode call the entry point SETBAT.
C     To set a program in interactive mode call SETMOD
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
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 20-NOV-1995 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C     Entry points.
C
C
C     Local Variable
C
      LOGICAL               INBTCH
 
      SAVE                  INBTCH
 
      LOGICAL               SETBAT
      LOGICAL               SETMOD
 
      DATA                  INBTCH / .FALSE. /
 
      BATCH = INBTCH
      RETURN
 
 
C$Procedure SETMOD (Set the reader to interative mode.)
 
      ENTRY SETMOD ()
 
C$ Abstact
 
C     Set NXTCOM to interactive mode.  In puts that are expected to
C     come from the keyboard generate an result in a prompt for input
 
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
      INBTCH = .FALSE.
      SETMOD = .TRUE.
      RETURN
 
 
C$Procedure SETBAT (Set the reader to interative mode.)
 
      ENTRY SETBAT ()
 
C$ Abstact
 
C     Set NXTCOM to interactive mode.  In puts that are expected to
C     come from the keyboard generate an result in a prompt for input
 
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
      INBTCH = .TRUE.
      SETBAT = .TRUE.
      RETURN
 
      END
