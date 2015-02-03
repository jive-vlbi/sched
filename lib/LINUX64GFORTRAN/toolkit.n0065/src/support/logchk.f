C$Procedure      LOGCHK ( Log file check )
 
      SUBROUTINE LOGCHK ( DEFALT, USENAM, DOLOG )
 
C$ Abstract
C
C     Determine whether to use a log file, and if so what name
C     pattern to use.
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
 
      IMPLICIT NONE
      CHARACTER*(*)         DEFALT
      CHARACTER*(*)         USENAM
      LOGICAL               DOLOG
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DEFALT     I   Default logfile name pattern
C     USENAM     O   Acutal logfile name pattern that will be used.
C     DOLOG      O   Flag indicating whether or not to use a log file.
C
C$ Detailed_Input
C
C     DEFALT     is a default pattern to use if nothing is specified
C                on the command line.
C
C$ Detailed_Output
C
C     USENAM     is the name to use for the log file or blank
C                if the -nolog flag is supplied on the command line.
C
C     DOLOG      is a logical flag that indicates whether or not
C                to create a log file.
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
C     This is a utility routine for use by the "Command Loop" routines
C     so that one can specify a custom name for a log file (or
C     specify that no log file be used at all.
C
C     The options examined from the command line are:
C
C       -nolog
C       -log <filename>
C
C      This routine does not judge the "fitness" of the name of
C      the logfile, if one is specified on the command line.  Checking
C      for suitability is left to other portions of the system.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 28-DEC-2001 (WLT)
C
C
C-&
C
C     Spicelib Functions.
C
      LOGICAL               EQSTR
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 900 )
 
      CHARACTER*(LONGSZ)    LINE
 
      INTEGER               B
      INTEGER               E
      INTEGER               START
C
C     Until we know otherwise, we set the logname to the default
C     value and set action to "use a log file".
C
      USENAM = DEFALT
      DOLOG  = .TRUE.
      START  =  1
 
      CALL GETCML ( LINE )
      CALL FNDNWD ( LINE, START, B, E )
 
      DO WHILE ( B .GT. 0 )
 
         START = E+1
 
         IF ( EQSTR( LINE(B:E), '-nolog' ) ) THEN
 
            USENAM = ' '
            DOLOG  = .FALSE.
            RETURN
 
         ELSE IF ( EQSTR( LINE(B:E), '-log' ) ) THEN
 
 
            CALL FNDNWD ( LINE, START, B, E )
 
            IF ( E .GT. B ) THEN
               USENAM = LINE(B:E)
            END IF
            RETURN
 
         END IF
 
         CALL FNDNWD ( LINE, START, B, E )
 
      END DO
 
      RETURN
      END
