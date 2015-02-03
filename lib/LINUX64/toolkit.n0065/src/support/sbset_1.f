 
C$Procedure SBSET ( String buffer, set value )
 
      SUBROUTINE SBSET_1 ( NAME, STR, NAMES,
     .                              PTRS,
     .                              BUFFER  )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Set the value of a string within a string buffer.
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
C     CB, LB, SB
C
C$ Keywords
C
C     ASCII
C     CHARACTER
C     STRING
C     TEXT
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )
 
      CHARACTER*(*)         NAME
      CHARACTER*(*)         STR
      CHARACTER*(*)         NAMES       ( LBCELL:* )
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the string to be stored.
C     STR        I   The string.
C     NAMES,
C     PTRS,
C     BUFFER    I,O  String buffer.
C
C$ Detailed_Input
C
C     NAME        is the name of a string to be stored within a string
C                 buffer. This name may be used to retrieve the string
C                 at some later time.
C
C     STR         is the string to be stored.
C
C     NAMES,
C     PTRS,
C     BUFFER      are the name, pointer, and character components of
C                 a string buffer.
C
C$ Detailed_Output
C
C     NAMES,
C     PTRS,
C     BUFFER      are the name, pointer, and character components of
C                 the same string buffer, now containing the specified
C                 string.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If NAME is longer than the maximum length of the names in
C        the buffer, it is truncated. Thus, long names may conflict,
C        overwriting each other's associated strings.
C
C     2) If a string with the specified name is not already contained
C        in the string buffer, and if the maximum number of strings
C        is currently stored, the error 'SPICE(SBTOOMANYSTRS)' is
C        signalled.
C
C$ Particulars
C
C     SBSET is the only way to get a string into a string buffer.
C
C$ Examples
C
C     The following code fragment stores three strings, associated
C     with the names WHO, WHAT, and WHERE.
C
C        CALL SBSET ( 'WHO',   'Feynman',                 N, P, B )
C        CALL SBSET ( 'WHAT',  'Quantum electrodynamics', N, P, B )
C        CALL SBSET ( 'WHERE', 'Caltech',                 N, P, B )
C
C     The strings can be retrieved using either SBGET,
C
C        CALL SBGET ( 'WHO',   S(1), N, P, B, POS )
C        CALL SBGET ( 'WHAT',  S(2), N, P, B, POS )
C        CALL SBGET ( 'WHERE', S(3), N, P, B, POS )
C
C      or SBGETP,
C
C        CALL SBGET ( 3, S(1), N, P, B, ADDR )
C        CALL SBGET ( 1, S(2), N, P, B, ADDR )
C        CALL SBGET ( 2, S(3), N, P, B, ADDR )
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
C     Dagny Taggart, (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               FRSTNB
      INTEGER               LASTNB
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC
 
C
C     Local variables
C
      INTEGER               MAXSTR
      INTEGER               NSTR
      INTEGER               POS
      INTEGER               F
      INTEGER               L
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBSET_1' )
      END IF
 
C
C     If the buffer already contains a string with this name, remove it.
C
      CALL SBREM_1 ( NAME, NAMES, PTRS, BUFFER )
 
C
C     Recover the (new) essential control information.
C
      MAXSTR = SIZEC ( NAMES )
      NSTR   = CARDC ( NAMES )
 
C
C     Where should the name be inserted?
C
      IF ( NSTR .EQ. MAXSTR ) THEN
         CALL SETMSG ( 'Current limit is #.'  )
         CALL ERRINT ( '#', MAXSTR            )
         CALL SIGERR ( 'SPICE(SBTOOMANYSTRS)' )
 
      ELSE
         POS = LSTLEC ( NAME, NSTR, NAMES(1) ) + 1
 
C
C        Store only the non-blank part of the string. (Store a blank
C        string as a single blank character.)
C
         F = MAX ( 1, FRSTNB ( STR ) )
         L = MAX ( 1, LASTNB ( STR ) )
 
C
C        Add the name of the string to the name list, and the string
C        itself to the LB.
C
         CALL INSLAC ( NAME, 1, POS, NAMES(1), NSTR  )
         CALL SCARDC ( NSTR,         NAMES           )
 
         CALL LBINS_1  ( POS, STR(F:L), PTRS, BUFFER  )
      END IF
 
      CALL CHKOUT ( 'SBSET_1' )
      RETURN
      END
