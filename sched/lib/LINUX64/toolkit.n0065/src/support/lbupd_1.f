 
C$Procedure LBUPD ( Line buffer, update )
 
      SUBROUTINE LBUPD_1 ( NLINE, NCOM, PTRS )
      IMPLICIT NONE
 
C$ Abstract
C
C     Update internal information in a line buffer.
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
 
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PTRS        ( LBCELL:* )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NLINE      I   Number of lines stored in the buffer.
C     NCOM       I   Number of complement intervals in the buffer.
C     PTRS      I,O  Pointer compnent of the buffer.
C
C$ Detailed_Input
C
C     NLINE       is the number of lines stored in the buffer, as
C                 the result of some change.
C
C     NCOM        is the number of complement intervals in the buffer,
C                 as the result of the same change.
C
C     PTRS        is the pointer component of a line buffer.
C
C$ Detailed_Output
C
C     PTRS        is the updated pointer component of a line buffer.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) The error 'SPICE(LBCORRUPTED)' is signalled whenever any
C        of the following conditions is detected.
C
C           -- NLINE is less than zero.
C
C           -- NCOM is less than one.
C
C           -- The sum of NLINE and NCOM is greater than the maximum
C              number of lines that can be stored in the buffer.
C
C$ Particulars
C
C     LBUPD is are provided for use by the LB routines in SPICELIB, and
C     should not be called directly except by those routines.
C
C$ Examples
C
C     LBUPD is used by LBINS and LBREM.
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
      INTEGER               SIZEI
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBUPD_1' )
 
         IF (         NLINE               .LT. 0
     .        .OR.    NCOM                .LT. 1
     .        .OR.  ( NLINE + NCOM ) * 2  .GT. SIZEI ( PTRS ) ) THEN
 
            CALL SETMSG ( 'Tried to store # lines, # holes.' )
            CALL ERRINT ( '#', NLINE                         )
            CALL ERRINT ( '#', NCOM                          )
            CALL SIGERR ( 'SPICE(LBCORRUPTED)'               )
            CALL CHKOUT ( 'LBUPD_1'                            )
            RETURN
         END IF
      END IF
 
C
C     Save the current number of lines in element -2. We can infer the
C     cardinality of the cell from the total number of intervals.
C
      PTRS(-2) = NLINE
 
      CALL SCARDI ( 2 * ( NLINE + NCOM ), PTRS )
 
      CALL CHKOUT ( 'LBUPD_1' )
      RETURN
      END
