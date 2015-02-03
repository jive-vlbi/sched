 
C$Procedure DIMCB ( Dimension of character buffer )
 
      INTEGER FUNCTION DIMCB_1 ( BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Return the dimension of a character buffer.
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
C     CB
C
C$ Keywords
C
C     ASCII
C     CHARACTER
C     STRING
C     TEXT
C
C$ Declarations
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0 )
 
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BUFFER     I   Character buffer.
C
C$ Detailed_Input
C
C     BUFFER      is a character buffer.
C
C$ Detailed_Output
C
C     The function returns the dimension of the character buffer
C     (as established by a previous call to CBINIT).
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
C     The dimension of a character buffer is multiplied by the
C     length of the individual elements in the buffer array to
C     give the total size of the buffer.
C
C$ Examples
C
C     The code fragment
C
C        INTEGER            LBCBUF
C        PARAMETER        ( LBCBUF = 0 )
C
C        INTEGER            DIMCB
C        INTEGER            SIZECB
C        CHARACTER*100      BUFFER  ( LBCBUF:200 )
C
C        CALL CBINIT ( 200, BUFFER )
C
C        WRITE (*,*) DIMCB  ( BUFFER ),    ' elements at '
C        WRITE (*,*) LEN    ( BUFFER(1) ), ' characters each totals '
C        WRITE (*,*) SIZECB ( BUFFER ),    ' characters of storage.'
C
C     produces the following output.
C
C        200    elements at
C        100    characters each totals
C        200000 characters of storage.
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
C     Beta Version 1.1.0, 28-Dec-1994 (WLT)
C
C        Gave DIMCB_1 an initial value of zero so that the function
C        will have a value when it returns even if an error is
C        signalled.
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               DIM
C
C     Give the function an initial value even if it is bogus in the
C     event that we are in RETURN mode.
C
      DIMCB_1 = 0
 
C
C     Standard error handling.
C
 
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DIMCB_1' )
      END IF
 
C
C     Only the first eight bytes are used.
C
      CALL DECHAR ( BUFFER(0)(1:8), DIM )
      DIMCB_1 = DIM
 
      CALL CHKOUT ( 'DIMCB_1' )
      RETURN
      END
