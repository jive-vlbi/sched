C$Procedure                     HALFPI ( Half the value of pi )
 
      DOUBLE PRECISION FUNCTION HALFPI ( )
 
C$ Abstract
C
C     Return half the value of pi (the ratio of the circumference of
C     a circle to its diameter).
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
C     None.
C
C$ Keywords
C
C     CONSTANTS
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     The function returns half the value of pi.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns half the value of pi (the ratio of
C     a circle's circumference to its diameter), determined by
C     the ACOS function. That is,
C
C           HALFPI = ACOS ( -1.D0 ) * 0.5D0
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The first time the function is referenced, the value is computed
C     as shown above. The value is saved, and returned directly upon
C     subsequent reference.
C
C$ Examples
C
C     The subroutine shown below illustrates the use of HALFPI.
C
C                 SUBROUTINE BFTRAN ( RA, DEC, W, TIPM )
C
C           C
C           C     Compute the transformation from inertial to body
C           C     fixed coordinates, given the directions of the north
C           C     pole and prime meridian of the body.
C           C
C                 DOUBLE PRECISION    RA
C                 DOUBLE PRECISION    DEC
C                 DOUBLE PRECISION    W
C                 DOUBLE PRECISION    TIPM ( 3,3 )
C
C           C
C     SPICELIB functions
C           C
C                 DOUBLE PRECISION      HALFPI
C
C           C
C           C     The transformation is defined by the compund
C           C     rotation
C           C
C           C        [W] [pi/2 - Dec] [RA + pi/2]
C           C           3            1           3
C           C
C                 CALL ROTATE (       RA + HALFPI(),  3, TIPM)
C                 CALL ROTMAT (TIPM,  HALFPI() - DEC, 1, TIPM)
C                 CALL ROTMAT (TIPM,  W,              3, TIPM)
C
C                 RETURN
C                 END
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
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     half the value of pi
C
C-&
 
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      VALUE
      SAVE                  VALUE
 
C
C     Initial values
C
      DATA                  VALUE      / 0.D0 /
 
 
C
C     What is there to say?
C
      IF ( VALUE .EQ. 0.D0 ) THEN
         VALUE = ACOS ( -1.D0 ) * 0.5D0
      END IF
 
      HALFPI = VALUE
 
      RETURN
      END
