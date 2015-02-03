 
C$Procedure LBINIT ( Line buffer, initialize )
 
      SUBROUTINE LBINIT_1 ( PSIZE, VDIM, PTRS, BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Initialize a line buffer.
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
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )
 
      INTEGER               PSIZE
      INTEGER               VDIM
      INTEGER               PTRS        ( LBCELL:PSIZE )
      CHARACTER*(*)         BUFFER      ( LBCBUF:VDIM  )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PSIZE      I   Pointer size.
C     VDIM       I   Value dimension.
C     PTRS,
C     BUFFER    I,O  Line buffer.
C
C$ Detailed_Input
C
C     PTRS        is an integer cell array to be used as the pointer
C                 component of a line buffer.
C
C     PSIZE       is the declared dimension of PTRS.
C
C     BUFFER      is a character buffer array to be used as the
C                 character compnent of a line buffer.
C
C     VDIM        is the declared dimension of BUFFER.
C
C$ Detailed_Output
C
C     PTRS,
C     BUFFER      together are an initialized line buffer.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the line buffer cannot hold even a single line, the error
C        'SPICE(LBINSUFPTRSIZE)' is signalled.
C
C$ Particulars
C
C     A line buffer must be initialized to allow subsequent
C     operations on the buffer to detect possible overflows.
C     Both components of the buffer are initialized by a single
C     call to LBINIT.
C
C     In order to store N lines, PSIZE should be at least 4N+4.
C
C$ Examples
C
C     The following code fragment illustrates the initialization
C     of a typical line buffer.
C
C        INTEGER               LBCELL
C        PARAMETER           ( LBCELL =    -5 )
C
C        INTEGER               LBCBUF
C        PARAMETER           ( LBCBUF =     0 )
C
C        INTEGER               MAXLN
C        PARAMETER           ( MAXLN  =  1000 )
C
C        INTEGER               PSIZE
C        PARAMETER           ( PSIZE  =  4 * MAXLN + 4 )
C
C        INTEGER               BUFDIM
C        PARAMETER           ( BUFDIM =    25 )
C
C        INTEGER               PTRS     ( LBCELL:PSIZE   )
C        CHARACTER*(MAXLN)     BUFFER   ( LBCBUF:BUFDIM  )
C         .
C         .
C
C        CALL LBINIT ( PSIZE, BUFDIM, PTRS, BUFFER )
C
C     In this example, the buffer may be used to store up to 1000 lines
C     averaging 25 characters per line, or 25,000 total characters. The
C     length of any particular line may range from a single character
C     to the entire 25,000 characters.
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
      INTEGER               SIZECB_1
 
C
C     Local variables
C
      INTEGER               MAXLN
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBINIT_1' )
      END IF
 
C
C     Initialize the character buffer first.
C
      CALL CBINIT_1 ( VDIM, BUFFER )
 
C
C     The size must be 4(n+1), where n is the maximum number of
C     lines that can be stored. (The line buffer must be able to
C     store at least one line!)
C
C     Every line buffer starts out with zero lines and one complement
C     interval, which covers the entire CB.
C
      MAXLN = ( PSIZE / 4 ) - 1
 
      IF ( MAXLN .LT. 1 ) THEN
         CALL SIGERR ( 'SPICE(INSUFPTRSIZE)' )
 
      ELSE
         CALL SSIZEI ( 4 * (MAXLN + 1), PTRS )
 
         PTRS(1) = 1
         PTRS(2) = SIZECB_1 ( BUFFER )
 
         CALL LBUPD_1 ( 0, 1, PTRS )
      END IF
 
      CALL CHKOUT ( 'LBINIT_1' )
      RETURN
      END
