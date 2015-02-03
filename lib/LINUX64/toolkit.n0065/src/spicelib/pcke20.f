C$Procedure      PCKE20 ( PCK, evaluate record, type 20 )
 
      SUBROUTINE PCKE20 ( ET, RECORD, EULANG )
 
C$ Abstract
C
C     Evaluate a single PCK data record from a segment of type 20
C     (Chebyshev Polynomials, rotation derivative only).
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
C     PCK
C
C$ Keywords
C
C     ORIENTATION
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      EULANG   ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Evaluation epoch.
C     RECORD     I   Data record.
C     EULANG     O   Euler angles and their derivatives.
C
C$ Detailed_Input
C
C     ET          is an epoch at which the Euler angles state is to be
C                 computed. The epoch is represented as seconds past
C                 J2000 TDB.
C
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will yield Euler angles and Euler angle rates
C                 representing the orientation and angular velocity,
C                 with respect to its base frame, of the reference
C                 frame associated with the input record.
C
C                 The structure of the record is as follows:
C
C                    +--------------------------------------+
C                    | record size (excluding this element) |
C                    +--------------------------------------+
C                    | Coverage interval midpoint           |
C                    +--------------------------------------+
C                    | Coverage interval radius             |
C                    +--------------------------------------+
C                    | Coeffs for ANGLE_1 rate              |
C                    +--------------------------------------+
C                    | Coeffs for ANGLE_2 rate              |
C                    +--------------------------------------+
C                    | Coeffs for ANGLE_3 rate              |
C                    +--------------------------------------+
C                    | ANGLE_1 at interval midpoint         |
C                    +--------------------------------------+
C                    | ANGLE_2 at interval midpoint         |
C                    +--------------------------------------+
C                    | ANGLE_3 at interval midpoint         |
C                    +--------------------------------------+
C
C                 In the above record
C
C                    - Times are expressed as seconds past J2000 TDB.
C                    - Angular components have units of radians.
C                    - Rate coefficients have units of radians/s.
C
C                 RECORD must be declared by the caller with size large
C                 enough to accommodate the largest record that can be
C                 returned by PCKR20.
C
C
C$ Detailed_Output
C
C     EULANG      is a 6-vector containing Euler angles and their
C                 derivatives at time ET. The angles occupy the first
C                 three elements of EULANG; the rates follow. The order
C                 of the components is
C
C                    ( ANGLE_1, ANGLE_2, ANGLE_3, 
C                      rate_1,  rate_2,  rate_3  )
C
C                 The angular units are radians; the rate units are
C                 radians/second.
C                 
C                 The Euler angles represent the orientation, relative
C                 to its base frame, of the PCK frame associated with
C                 the input record. The angles, which are numbered
C                 according to their ordinal position in the logical
C                 records, define a transformation matrix R as follows:
C
C                    R = [ ANGLE_3 ]  [ ANGLE_2 ]  [ ANGLE_1 ]
C                                   3            1            3
C
C                 Here the notation
C
C                    [ THETA ]
C                             i
C
C                 denotes a reference frame rotation of THETA radians
C                 in the right-hand sense about the ith coordinate
C                 axis. See the Rotation Required Reading for further
C                 discussion of this notation.
C
C                 The matrix R transforms vectors expressed in the base
C                 frame to vectors expressed in the PCK frame associated
C                 with RECORD by left multiplication:
C
C                    V    = R * V
C                     PCK        FRAME
C
C                 In cases where the PCK frame is a body-fixed,
C                 right-handed frame with its +Z axis aligned with a
C                 body's north pole, the orientation angles are related
C                 to right ascension (RA) and declination (DEC) of the
C                 PCK frame's north pole, and prime meridian
C                 orientation (W), by the equations
C
C                    ANGLE_1 = RA   + pi/2 radians
C                    ANGLE_2 = pi/2 - DEC  radians
C                    ANGLE_3 = W           radians      
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input record contains an invalid coefficient count, 
C        the error will be diagnosed by a routine in the call tree of
C        this routine.
C
C     2) If the input record contains invalid domain transformation
C        parameters, the error will be diagnosed by a routine in the
C        call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 20 (Chebyshev polynomials,
C     position only) segments are described in the PCK Required Reading
C     file.
C
C     A type 20 segment contains three sets of Chebyshev coefficients,
C     one set each for the derivatives with respect to time of the
C     Euler angles phi, delta and psi. PCKE20 calls the routine SPKE20
C     for each set to evaluate the polynomial and its first derivative.
C
C$ Examples
C
C     The PCKEnn routines are almost always used in conjunction with
C     the corresponding PCKRnn routines, which read the records from
C     binary PCK files.
C
C     The data returned by the PCKRnn routine are in their rawest form,
C     taken directly from the segment.  As such, they will be
C     meaningless to a user unless he/she understands the structure of
C     the data type completely. Given that understanding, however, the
C     PCKRnn routines might be used to examine raw segment data before
C     evaluating it with the PCKEnn routines.
C
C
C     Here we load a binary PCK files and use PCKE20 to get the
C     Euler angles.
C
C  C
C  C     Load binary PCK file.
C  C
C        CALL PCKLOF ('example.pck', HANDLE)
C
C  C
C  C     Get a segment applicable to a specified body and epoch.
C  C
C        CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C        IF ( FOUND ) THEN
C  C
C  C        Look at parts of the descriptor.
C  C
C           CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
C           TYPE   = ICD( NT )
C           REF    = ICD( NR )
C
C           IF ( TYPE .EQ. 20 ) THEN
C  C
C  C           Read in Chebyshev coefficients from segment.
C  C
C              CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
C  C
C  C           Call evaluation routine to get Euler angles
C  C           phi, delta, w.
C  C
C              CALL PCKE20 ( ET, RECORD, EULANG )
C
C
C     The Euler angles and their derivatives are returned
C     in EULANG.
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
C     N.J. Bachman (JPL)
C     K.S. Zukor   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (KSZ)
C
C-&
 
C$ Index_Entries
C
C     evaluate pck_type_20 record
C
C-&
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKE20' )
 
C
C     Call evaluation routine to get Euler angles
C     phi, delta, w.
C
      CALL SPKE20 ( ET, RECORD, EULANG )
 
C
C     Map the third angle into the range (-2pi, 2pi).
C
      EULANG(3) = MOD ( EULANG(3), TWOPI() )
 
      CALL CHKOUT ( 'PCKE20' ) 
      RETURN
      END
 
 
 
 
 
 
