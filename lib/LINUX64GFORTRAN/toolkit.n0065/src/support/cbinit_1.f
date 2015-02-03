 
C$Procedure CBINIT ( Character buffer, initialize  )
 
      SUBROUTINE CBINIT_1 ( DIM, BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Initialize a character buffer.
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
 
      INTEGER               DIM
      CHARACTER*(*)         BUFFER      ( LBCBUF:DIM )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DIM        I   Dimension of the character buffer array.
C     BUFFER    I,O  Character buffer.
C
C$ Detailed_Input
C
C     DIM         is the dimension of the array containing the
C                 character buffer to be initialized.
C
C     BUFFER      is the array.
C
C$ Detailed_Output
C
C     BUFFER      is an initialized character buffer.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) The error 'SPICE(NOTLEGALCB)' is signalled whenever any of
C        the following conditions is detected.
C
C           -- The length of the individual array elements is less
C              than eight.
C
C           -- DIM is less than one.
C
C$ Particulars
C
C     A character buffer must be initialized to allow subsequent
C     operations on the buffer to detect possible overflows.
C
C$ Examples
C
C     The following code fragment illustrates the initialization
C     of a character buffer.
C
C        INTEGER               LBCBUF
C        PARAMETER           ( LBCBUF =    0 )
C
C        INTEGER               BUFDIM
C        PARAMETER           ( BUFDIM =  256 )
C
C        INTEGER               BUFLEN
C        PARAMETER           ( BUFLEN = 1024 )
C
C        CHARACTER*(BUFLEN)    BUFFER   ( LBCBUF:BUFDIM )
C         .
C         .
C
C        CALL CBINIT ( BUFDIM, BUFFER )
C
C     In this example, the buffer contains 256K characters of available
C     storage (256 array elements of 1024 characters each). Note that
C     it is only necessary to supply the dimension of the array (256),
C     and not the length of the individual elements (1024).
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
      LOGICAL               RETURN
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CBINIT_1' )
 
         IF ( LEN ( BUFFER(0) ) .LT. 8 ) THEN
            CALL SETMSG ( 'Length is #.'         )
            CALL ERRINT ( '#', LEN ( BUFFER(0) ) )
 
            CALL SIGERR ( 'SPICE(NOTLEGALCB)' )
            CALL CHKOUT ( 'CBINIT_1'            )
            RETURN
 
         ELSE IF ( DIM .LT. 1 ) THEN
            CALL SETMSG ( 'Dimension is #.' )
            CALL ERRINT ( '#', DIM          )
 
            CALL SIGERR ( 'SPICE(NOTLEGALCB)' )
            CALL CHKOUT ( 'CBINIT_1'            )
            RETURN
         END IF
      END IF
 
C
C     Store only the dimension.
C
      CALL ENCHAR ( DIM, BUFFER(0)(1:8) )
 
      CALL CHKOUT ( 'CBINIT_1' )
      RETURN
      END
