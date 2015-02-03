C$Procedure      TEMPB ( Manage SUBTeX temporary buffer )
 
      SUBROUTINE TEMPB ( ACTION, LINE )
 
C$ Abstract
C
C     Add lines to the SUBTeX temporary line buffer.
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
C     SUBTeX
C
C$ Keywords
C
C     SUBTeX
C
C$ Declarations
 
      CHARACTER*(*)         ACTION
      CHARACTER*(*)         LINE
 
C$ Detailed_Input
C
C     ACTION      determines whether the buffer is to be cleared
C                 ('NEW') before adding the line, or whether the
C                 line is to be appended ('ADD') to the existing
C                 buffer.
C
C     LINE        is a line of text to be added to the SUBTeX
C                 temporary line buffer.
C
C$ Detailed_Output
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ACTION     I   'NEW' or 'ADD'.
C     LINE       I   New (processed) line.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SUBTeX(BADTEMPACTION)'
C        is signalled.
C
C$ Particulars
C
C     As each chunk of the input source is processed, the resulting
C     lines are placed in the temporary line buffer. The lines from
C     this buffer ultimately replace the input source.
C
C$ Examples
C
C
C$ Restrictions
C
C     The nominal buffer contains sufficient storage for up to 1000
C     lines averaging 60 characters per line.
C
C$ Literature_References
C
C$Include SUBTeX.REFS
C
C$ Author_and_Institution
C
C     I.M. Underwood (JPL)
C
C$ Version
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL =   -5 )
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =    0 )
 
      INTEGER               MAXLN
      PARAMETER           ( MAXLN  = 1000 )
 
      INTEGER               MAXPTR
      PARAMETER           ( MAXPTR = 4 * ( MAXLN + 1 ) )
 
      INTEGER               AVGLEN
      PARAMETER           ( AVGLEN =  60 )
 
C     INTEGER               PTRS          ( LBCELL:MAXPTR )
 
C     LOGICAL               INIT
 
      CHARACTER*4           WHAT
 
 
 
C
C     Saved variables
C
C     SAVE                  PTRS
C     SAVE                  BUFFER
C     SAVE                  INIT
 
C
C     Initial values
C
C     DATA                  INIT          / .FALSE. /
 
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TEMPB' )
      END IF
 
C
C     Initialize the line buffer if necessary.
C
C     IF ( .NOT. INIT ) THEN
C
C        CALL SSIZEI ( MAXPTR, PTRS   )
C        CALL SDIMCB ( MAXLN,  BUFFER )
C        CALL LBINIT ( PTRS,   BUFFER )
C
C     END IF
 
C
C     Shake or bake?
C
      CALL UCASE ( ACTION, WHAT )
 
      IF ( WHAT .EQ. 'NEW' ) THEN
C        CALL LBINIT ( PTRS, BUFFER )
C        CALL LBAPP  ( LINE, PTRS, BUFFER )
 
         CALL PAGPUT ( LINE )
 
      ELSE IF ( WHAT .EQ. 'ADD' ) THEN
C        CALL LBAPP ( LINE, PTRS, BUFFER )
 
         CALL PAGPUT ( LINE )
 
      ELSE
         CALL SIGERR ( 'SUBTeX(BADTEMPACTION)' )
      END IF
 
      CALL CHKOUT ( 'TEMPB' )
      RETURN
      END
 
