 
C$Procedure LBDES ( Line buffer, describe )
 
      SUBROUTINE LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Describe the current internal status of a line buffer.
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
C     CB, LB
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
 
      INTEGER               PTRS         ( LBCELL:* )
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PTRS       I   Pointer component of the buffer.
C     MAXLN      O   Maximum number of lines.
C     NLINE      O   Current number of lines.
C     NCOM       O   Current number of complement intervals.
C     PCARD      O   Current cardinality of PTRS.
C
C$ Detailed_Input
C
C     PTRS        is the pointer component of a line buffer.
C
C$ Detailed_Output
C
C     MAXLN       is the maximum number of lines that can be stored in
C                 the buffer at any one time.
C
C     NLINE       is the number of lines currently stored in the buffer.
C
C     NCOM        is the number of complement intervals (contiguous
C                 spaces in which new lines can be stored) currently
C                 available in the buffer.
C
C     PCARD       is the current cardinality of PTRS.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     This routine is intended primarily for internal use by the
C     line buffer routines. However, the information that it returns
C     can be useful for error checking and debugging purposes.
C
C$ Examples
C
C     In the following code fragment, a check is performed before
C     attempting to use the routine LBAPP.
C
C        CALL LBDES ( PTRS, MAXLN, NLINE, NCOM, PCARD )
C
C        IF ( NLINE .LT. MAXLN ) THEN
C           CALL LBAPP ( LINE, PTRS, BUFFER )
C
C        ELSE
C           WRITE (6,*) 'Sorry, there isn't room for another line.'
C           WRITE (6,*) 'Please delete something and try again.'
C        END IF
C
C     For more examples, see the source code of the other LB routines.
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
      INTEGER               CARDI
      LOGICAL               RETURN
      INTEGER               SIZEI
 
C
C     Local variables
C
      INTEGER               PSIZE
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBDES_1' )
      END IF
 
C
C     Recover some information directly.
C
      PSIZE = SIZEI ( PTRS     )
      PCARD = CARDI ( PTRS     )
      NLINE =         PTRS(-2)
 
C
C     Infer the rest.
C
      MAXLN = ( PSIZE / 4 ) - 1
      NCOM  = ( PCARD / 2 ) - NLINE
 
      CALL CHKOUT ( 'LBDES_1' )
      RETURN
      END
