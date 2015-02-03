 
C$Procedure SBGET ( String buffer, get )
 
      SUBROUTINE SBGET_1 ( NAME, NAMES,
     .                         PTRS,
     .                         BUFFER, STR, POS )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Get (return) a string from a string buffer.
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
      CHARACTER*(*)         NAMES       ( LBCELL:* )
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
      CHARACTER*(*)         STR
      INTEGER               POS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the string to be returned.
C     NAMES,
C     PTRS,
C     BUFFER     I   String buffer.
C     STR        O   The string.
C     POS        O   Position of the string within the buffer.
C
C$ Detailed_Input
C
C     NAME        is the name of a string contained within a string
C                 buffer.
C
C     NAMES,
C     PTRS,
C     BUFFER      are the name, pointer, and character components
C                 of the string buffer.
C
C$ Detailed_Output
C
C     STR         is the string associated with the specified name.
C                 If STRING is shorter than the stored string, it is
C                 truncated. If longer, STRING is padded with spaces.
C
C     POS         is the position of the specified string within the
C                 string buffer, as determined by the ASCII collating
C                 sequence.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the specified string is not in the list, POS is zero
C        and STR is not changed.
C
C$ Particulars
C
C     There are two routines that you can use to retrieve a string
C     from a string buffer:
C
C        SBGET      which takes the name of the string, and returns
C                   the string and its position within the buffer.
C
C        SBGETP     which takes the position of the string within
C                   the buffer, and returns the string and its address
C                   within the name table.
C
C$ Examples
C
C     The following code fragment stores three strings, associated
C     with the names WHO, WHAT, and WHERE.
C
C        CALL SBSET  ( 'WHO',   'Feynman',                 N, P, B )
C        CALL SBSET  ( 'WHAT',  'Quantum electrodynamics', N, P, B )
C        CALL SBSET  ( 'WHERE', 'Caltech',                 N, P, B )
C
C     The strings can be retrieved using either SBGET,
C
C        CALL SBGET  ( 'WHO',   S(1), N, P, B, POS )
C        CALL SBGET  ( 'WHAT',  S(2), N, P, B, POS )
C        CALL SBGET  ( 'WHERE', S(3), N, P, B, POS )
C
C      or SBGETP,
C
C        CALL SBGETP ( 3, S(1), N, P, B, ADDR )
C        CALL SBGETP ( 1, S(2), N, P, B, ADDR )
C        CALL SBGETP ( 2, S(3), N, P, B, ADDR )
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
      INTEGER               BSRCHC
      LOGICAL               RETURN
 
C
C     Local variables
C
      LOGICAL               FOUND
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBGET_1' )
      END IF
 
C
C     Is this string even in the list?
C
      POS = BSRCHC ( NAME, CARDC ( NAMES ), NAMES(1) )
 
C
C     If so, get it.
C
      IF ( POS .GT. 0 ) THEN
         CALL LBGET_1 ( POS, PTRS, BUFFER, STR, FOUND )
      END IF
 
      CALL CHKOUT ( 'SBGET_1' )
      RETURN
      END
