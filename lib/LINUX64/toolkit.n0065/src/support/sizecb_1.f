 
C$Procedure SIZECB ( Size of character buffer )
 
      INTEGER FUNCTION SIZECB_1 ( BUFFER )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Return the total size of a character buffer.
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
C     The function returns the total size of the character buffer
C     (as established by a previous call to CBINIT). This is the
C     total number of characters that can be stored in the buffer.
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
C     The size of a character buffer is checked before any operation
C     is performed on the buffer, to detect possible overflows.
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
C-    Beta Version 1.1.0, 28-DEC-1994 (WLT)
C
C        The function is assigned an initial value of 0 so that it
C        will have some value if we are in RETURN mode.
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               DIMCB_1
      LOGICAL               RETURN
 
C
C     Give the function some initial value.  Zero seems as good as
C     anything.
C
      SIZECB_1 = 0
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SIZECB_1' )
      END IF
 
C
C     Size = dimension * length.
C
      SIZECB_1 = DIMCB_1 ( BUFFER ) * LEN ( BUFFER(1) )
 
      CALL CHKOUT ( 'SIZECB_1' )
      RETURN
      END
