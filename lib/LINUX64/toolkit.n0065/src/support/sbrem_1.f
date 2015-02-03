 
C$Procedure SBREM ( String buffer, remove )
 
      SUBROUTINE SBREM_1 ( NAME, NAMES,
     .                         PTRS,
     .                         BUFFER  )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Remove a string from a string buffer.
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
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the string to be removed.
C     NAMES,
C     PTRS,
C     BUFFER    I,O  String buffer.
C
C$ Detailed_Input
C
C     NAME        is the name of a string currently stored within a
C                 string buffer.
C
C     NAMES,
C     PTRS,
C     BUFFER      are the name, pointer, and character components
C                 of a string buffer.
C
C$ Detailed_Output
C
C     NAMES,
C     PTRS,
C     BUFFER      are the name, pointer, and character components
C                 of the same string buffer, from which the specified
C                 string has been removed.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If NAME is longer than the maximum length of the names in
C        the buffer, it is truncated. Thus, long names may conflict,
C        removing each other's associated strings.
C
C     2) If a string with the specified name is not already contained
C        in the string buffer, nothing happens.
C
C$ Particulars
C
C     SBREM is the only way to get a string out of a string buffer.
C
C$ Examples
C
C     The code fragment
C
C        CALL SBSET ( 'EINSTEIN', 'Brownian motion',  N, P, B      )
C        CALL SBSET ( 'BOHR',     'Atomic structure', N, P, B      )
C        CALL SBGET ( 'EINSTEIN',                     N, P, B, POS )
C
C        WRITE (*,*) 'Found at position ', POS
C
C        CALL SBREM ( 'EINSTEIN', N, P, B      )
C        CALL SBGET ( 'EINSTEIN', N, P, B, POS )
C
C        WRITE (*,*) 'Found at position ', POS
C
C     Produces the following output.
C
C        Found at position 2
C        Found at position 0
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
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NSTR
      INTEGER               POS
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBREM_1' )
      END IF
 
C
C     Recover the essential control information.
C
      NSTR = CARDC ( NAMES )
 
C
C     Which string is to be removed?
C
      POS = BSRCHC ( NAME, NSTR, NAMES(1) )
 
C
C     If the string is not in the buffer, do nothing.
C
      IF ( POS .GT. 0 ) THEN
 
C
C        Remove the name from the name list, and the string from the
C        line buffer.
C
         CALL REMLAC ( 1,    POS, NAMES(1), NSTR  )
         CALL SCARDC ( NSTR,      NAMES           )
 
         CALL LBREM_1  ( POS, PTRS, BUFFER )
 
      END IF
 
      CALL CHKOUT ( 'SBREM_1' )
      RETURN
      END
