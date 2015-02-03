C$Procedure      ECHO ( Echo the translation of a string )
 
      SUBROUTINE ECHO ( STRING, TRANSL )
 
C$ Abstract
C
C    Echo a string if echoing is enabled and a string has been
C    translated from its original value.
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
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TRANSL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   is a string
C     TRANSL     I   is string after some kind of processing
C
C$ Detailed_Input
C
C     STRING     is a string.  The intent is that this is some string
C                that the user has specified as a command to a program
C                and that may be subject to some kind of preprocessing
C                such as symbol resolution.
C
C     TRANSL     is the string that results from some user's action on
C                the input STRING.
C
C$ Detailed_Output
C
C     None.
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
C     This is a utility routine for the command loop system.
C
C     If as the result of preprocessing a command, some modificactions
C     are created it is sometimes helpful to see the result
C     of these translations.
C
C     If the echoing is enabled (via the entry point DOECHO) and
C     TRANSL is not the same as STRING.  The translation will
C     be echoed to the user's output device and to the user's log
C     file.
C
C     This routine has 3 companion entry points.
C
C     DOECHO  ---  enables echoing of commands.
C     NOECHO  ---  disables echoing of commands.
C     GTECHO  ---  returns 'YES' if echoing is enabled 'NO'
C                  otherwise.
C
C     By default echoing is disabled.
C
C$ Examples
C
C     Suppose that as a result of symbol resolution the
C     command
C
C        "DOIT"
C
C     becomes
C
C          SELECT A, B, C, FROM TABLE WHERE A < B AND B < C
C          AND C < A ORDER BY A B C
C
C
C     If echoing has been enabled the text below will be sent
C     to the user's screen and log file:
C
C     ;;; SELECT A, B, C, FROM TABLE WHERE A < B AND B < C AND
C     ;   C < A ORDER BY A B C '
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
C-    SPICELIB Version 1.0.0, 28-JUL-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Echo translated commands.
C
C-&
      EXTERNAL              NSPWLN
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    STYLE
      CHARACTER*(LNSIZE)    HIDE
      CHARACTER*(LNSIZE)    SEEN
      CHARACTER*(LNSIZE)    HSTYLE
 
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    DONT
      CHARACTER*(WDSIZE)    REPEAT
      CHARACTER*(WDSIZE)    FRSTWD
      CHARACTER*(WDSIZE)    SCNDWD
      CHARACTER*(WDSIZE)    THRDWD
 
      CHARACTER*(1)         DELIM
      CHARACTER*(1)         CDELIM
      CHARACTER*(3)         FLAG
      CHARACTER*(3)         LEAD
 
      INTEGER               LOC
 
      LOGICAL               STAT  ( 3 )
      LOGICAL               DOIT
      LOGICAL               FIRST
      LOGICAL               WIPE
 
      SAVE
 
      DATA                  DOIT  / .FALSE. /
      DATA                  FIRST / .TRUE. /
 
      IF ( FIRST ) THEN
C
C        Find out what the words for NO and ECHO are
C        in the current language.
C
         FIRST = .FALSE.
 
         CALL TRNLAT ( 'DONT', DONT   )
         CALL TRNLAT ( 'ECHO', REPEAT )
 
      END IF
 
      CALL NTHWD ( TRANSL, 1, FRSTWD, LOC )
      CALL NTHWD ( TRANSL, 2, SCNDWD, LOC )
      CALL NTHWD ( TRANSL, 3, THRDWD, LOC )
 
 
      CALL UCASE ( FRSTWD, FRSTWD )
      CALL UCASE ( SCNDWD, SCNDWD )
      CALL UCASE ( THRDWD, THRDWD )
 
      IF (      FRSTWD .EQ. REPEAT
     .    .AND. SCNDWD .EQ. ' '    ) THEN
 
         WIPE = .TRUE.
         DOIT = .TRUE.
 
      ELSE IF (       FRSTWD .EQ. DONT
     .          .AND. SCNDWD .EQ. REPEAT
     .          .AND. THRDWD .EQ. ' '    ) THEN
 
         WIPE = .TRUE.
         DOIT = .FALSE.
 
      ELSE
 
         WIPE = .FALSE.
 
      END IF
 
 
      IF ( DOIT ) THEN
 
         IF ( STRING .NE. TRANSL ) THEN
C
C           Get the current margins and the delimiter.
C
            CALL NSPMRG ( STYLE )
            CALL GETDEL ( DELIM )
C
C           Create the NICEIO style string it will be of the form
C
C              LEFT 1 RIGHT margin FLAG ;;; LEADER  ;
C
C           (provided of course that ';' is the command
C
            FLAG = DELIM // DELIM // DELIM
            LEAD = DELIM // '++'
 
            CALL PREFIX (  LEAD,     1, STYLE )
            CALL PREFIX ( 'LEADER ', 1, STYLE )
            CALL PREFIX (  FLAG,     1, STYLE )
            CALL PREFIX ( 'FLAG',    1, STYLE )
C
C           Get the current status of the "log" port and
C           for the moment inhibit writing to that port.
C
            CALL NSPGST ( 'LOG', STAT  )
            CALL NSPIOH ( 'LOG'        )
C
C           Display the translated string.
C
            CALL NICEPR_1 ( TRANSL, STYLE, NSPWLN )
C
C           Now re-establish the status of the log port.
C
            CALL NSPPST ( 'LOG', STAT )
C
C           Send the translated string to the log file and
C           do it so that it is a comment in the log file.
C           Note that we use a special logging style for
C           echoing the symbol translation.
C
            HSTYLE = 'LEFT 1 RIGHT 78 '
 
            CALL PREFIX (  LEAD,     1, HSTYLE )
            CALL PREFIX ( 'LEADER ', 1, HSTYLE )
            CALL PREFIX (  FLAG,     1, HSTYLE )
            CALL PREFIX ( 'FLAG',    1, HSTYLE )
 
            CALL NSPGLS ( SEEN,    HIDE,   CDELIM )
            CALL NSPLGS ( SEEN,    HSTYLE, CDELIM )
            CALL NSPLOG ( TRANSL, .TRUE.          )
            CALL NSPLGS ( SEEN,    HIDE,   CDELIM )
 
         END IF
 
      END IF
 
      IF ( WIPE ) THEN
         TRANSL = ' '
      END IF
 
      RETURN
 
C
C     The following entry points allow you to
C
C        1) Enable echoing of translations
C        2) Disable echoing of translations
C        3) Find out the current status of echoing.
C
C     Since the code in each case is trivial, we aren't
C     going to set up those big old nasty NAIF headers.
C     (What a rebel!)
C
      ENTRY DOECHO ()
 
         DOIT = .TRUE.
         RETURN
 
      ENTRY NOECHO ()
 
         DOIT = .FALSE.
         RETURN
 
      ENTRY GTECHO ( STRING )
 
         IF ( DOIT ) THEN
            STRING = 'ENABLED'
         ELSE
            STRING = 'DISABLED'
         END IF
 
         RETURN
 
      END
