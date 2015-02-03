C$Procedure PCKMAT ( PCK, get transformation matrix at time )

      SUBROUTINE PCKMAT ( BODY, ET, REF, TSIPM, FOUND )

C$ Abstract
C
C      Given a body and epoch, return the name of an inertial
C      reference frame and the 6 x 6 state transformation matrix
C      from that frame to the body fixed frame.
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
C     NAIF_IDS
C     ROTATION
C     TIME
C     PCK
C
C$ Keywords
C
C     TRANSFORMATION
C     ROTATION
C
C$ Declarations

      INTEGER              BODY
      DOUBLE PRECISION     ET
      INTEGER              REF
      DOUBLE PRECISION     TSIPM ( 6, 6 )
      LOGICAL              FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   ID code of some body.
C     ET         I   Epoch of transformation.
C     REF        O   Integer code for inertial reference frame.
C     TSIPM      O   Transformation from Inertial to PM for BODY at ET.
C     FOUND      O   True if data for BODY and ET are found.
C
C$ Detailed_Input
C
C     BODY        is the integer ID code of the body for which the
C                 state transformation matrix is requested. Bodies
C                 are numbered according to the standard NAIF
C                 numbering scheme.  The numbering scheme is
C                 explained in the NAIF_IDS required reading file.
C
C     ET          is the epoch at which the state transformation
C                 matrix is requested.
C
C$ Detailed_Output
C
C     REF         is the integer code for the inertial reference frame
C                 of the state transformation matrix TSIPM. (See the
C                 routine CHGIRF for a full list of inertial reference
C                 frame names.)
C
C     TSIPM       is a 6x6 transformation matrix. It is used to
C                 transform states from inertial coordinates to body
C                 fixed (also called equator and prime meridian --- PM)
C                 coordinates.
C
C                 Given a state S in the inertial reference frame
C                 specified by REF, the corresponding state in the body
C                 fixed reference frame is given by the matrix vector
C                 product:
C
C                    TSIPM * S
C
C                 See the PCK required reading for further details
C                 concerning PCK reference frames.
C
C                 NOTE: The inverse of TSIPM is NOT its transpose. The
C                 matrix, TSIPM, has the structure shown below:
C
C                             -            -
C                            |       :      |
C                            |   R   :  0   |
C                            | ......:......|
C                            |       :      |
C                            | dR_dt :  R   |
C                            |       :      |
C                             -            -
C
C                 where R is a time varying rotation matrix and dR_dt
C                 is its derivative.  The inverse of this matrix is:
C
C                             -              -
C                            |     T  :       |
C                            |    R   :  0    |
C                            | .......:.......|
C                            |        :       |
C                            |      T :   T   |
C                            | dR_dt  :  R    |
C                            |        :       |
C                             -              -
C
C                 The SPICE routine INVSTM is available for producing
C                 this inverse.
C
C      FOUND      if the data allowing the computation of a state
C                 transformation matrix for the requested time and body
C                 are found in a binary PCK file, FOUND will have the
C                 value .TRUE., otherwise it will have the value
C                 .FALSE..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the size of the type 20 PCK record to be  retrieved is too
C         large to fit into RECORD, the error SPICE(PCKRECTOOLARGE)
C         will be signaled.
C
C     2)  Any error that occurs while reading PCK data will be
C         diagnosed by a routine in the call tree of this routine.
C
C     3)  If the requested transformation matrix cannot be computed
C         using data from loaded binary PCK files, FOUND is returned
C         with the value .FALSE.. This is not a SPICE error.
C
C$ Files
C
C     This routine computes transformation matrices using data
C     provided by a loaded binary PCK kernel.
C
C$ Particulars
C
C     The matrix for transforming an inertial state into a body fixed
C     states is the 6x6 matrix shown below as a block structured
C     matrix.
C
C                 -            -
C                |       :      |
C                | TIPM  :  0   |
C                | ......:......|
C                |       :      |
C                | DTIPM : TIPM |
C                |       :      |
C                 -            -
C
C     If a binary PCK file record can be found for the time and body
C     requested, it will be used. The most recently loaded binary PCK
C     file has first priority, followed by previously loaded binary PCK
C     files in backward time order. If no binary PCK file has been
C     loaded, the text P_constants kernel file is used.
C
C
C$ Examples
C
C     Here we load a binary PCK files and use PCKEUL to get the
C     Euler angles.
C
C     C
C     C  Load binary PCK file.
C     C
C        CALL PCKLOF ('example.pck', HANDLE)
C
C     C  Call routine to get transformation matrix.
C
C        CALL PCKMAT ( BODY, ET, REF, TIPM, FOUND )
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K. S. Zukor     (JPL)
C      K. R. Gehringer (JPL)
C      N. J. Bachman   (JPL)
C
C$ Version
C
C-     SPICELIB Version 3.0.0, 03-JAN-2014 (NJB) (EDW)
C
C         Minor edits to Procedure; clean trailing whitespace.
C         Removed unneeded Revisions section.
C
C         Updated to support type 20. Changed long error message
C         for the case of RECORD having insufficient room: the
C         user is no longer advised to modify the record size.
C
C-     SPICELIB Version 2.0.0, 22-MAR-1995 (KRG) (KSZ)
C
C         Added PCK type 03. Added a new exception. Made some minor
C         comment changes.
C
C-     SPICELIB Version 1.0.0, 21-MAR-1995 (KSZ)
C
C         Replaces PCKEUL and returns the transformation
C         matrix rather than the Euler angles.
C
C-&

C$ Index_Entries
C
C     get state transformation matrix from binary PCK file
C
C-&

C
C     SPICELIB functions
C
      LOGICAL              FAILED
      LOGICAL              RETURN

C
C     Local Parameters
C
C     ND and NI values for a PCK file.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )
C
C     Index for the reference frame code in the integer summary.
C
      INTEGER               NR
      PARAMETER           ( NR = 2 )
C
C     Length of the descriptor for a PCK file.
C
      INTEGER               NS
      PARAMETER           ( NS = ND + (NI+1)/2 )
C
C     Index for the data type code in the integer summary.
C
      INTEGER               NT
      PARAMETER           ( NT = 3 )
C
C     Maximum size allowed for a record in a segment of a binary PCK
C     file.
C
      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 130 )
C
C     Number of components in a state vector.
C
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )

C
C     Local Variables
C
      CHARACTER*(40)        IDENT

      DOUBLE PRECISION      DCD    (     ND )
      DOUBLE PRECISION      DESCR  (     NS )
      DOUBLE PRECISION      ESTATE ( NSTATE )
      DOUBLE PRECISION      EULANG ( NSTATE )
      DOUBLE PRECISION      RECORD ( MAXREC )

      INTEGER               HANDLE
      INTEGER               ICD    (     NI )
      INTEGER               RECSIZ
      INTEGER               TYPE

C
C     Standard SPICE Error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKMAT' )

C
C     Get a segment applicable to a specified body and epoch.
C
      CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )

      IF ( FAILED() ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'PCKMAT' )
         RETURN
      END IF

      IF ( FOUND ) THEN

C
C        Look at parts of the descriptor.
C
         CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

         TYPE = ICD( NT )
         REF  = ICD( NR )

         IF ( TYPE .EQ. 02 ) THEN
C
C           Read in Chebyshev coefficients from segment.
C
            CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
C
C           Call evaluation routine to get Euler angles
C           phi, delta, w.
C
            CALL PCKE02 ( ET, RECORD, EULANG )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF
C
C           From the PCK type two file the Euler angles are
C           retrieved in a particular order.  The routine to
C           get the TSIPM matrix from expects them in another
C           order.  Here we change from EULANG to ESTATE, which
C           has this proper order.
C
            ESTATE (1) = EULANG (3)
            ESTATE (2) = EULANG (2)
            ESTATE (3) = EULANG (1)
            ESTATE (4) = EULANG (6)
            ESTATE (5) = EULANG (5)
            ESTATE (6) = EULANG (4)

C
C           Call routine which takes Euler angles to transformation
C           matrix.
C
            CALL EUL2XF ( ESTATE, 3, 1, 3, TSIPM )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF


         ELSE IF ( TYPE .EQ. 03 ) THEN
C
C           Fetch the number of Chebyshev coefficients, compute the
C           record size needed, and signal an error if there is not
C           enough storage in RECORD. The number of coefficients is the
C           first constant value in the generic segment.
C
            CALL SGFCON ( HANDLE, DESCR, 1, 1, RECORD(1) )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF

            RECSIZ = NSTATE * INT(RECORD(1)) + 2

            IF ( RECSIZ .GT. MAXREC ) THEN

               CALL SETMSG ( 'Storage for # double precision numbers'
     .         //            ' is needed for a PCK data record and'
     .         //            ' only # locations were available.'
     .         //            ' Notify the NAIF'
     .         //            ' group of this problem.'                 )
               CALL ERRINT ( '#', RECSIZ                               )
               CALL ERRINT ( '#', MAXREC                               )
               CALL SIGERR ( 'SPICE(PCKKRECTOOLARGE)'                  )
               CALL CHKOUT ( 'PCKMAT'                                  )
               RETURN

            END IF

            CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
            CALL PCKE03 ( ET, RECORD, TSIPM         )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF


         ELSE IF ( TYPE .EQ. 20 ) THEN
C
C           Read in Chebyshev coefficients from segment.
C
            CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
C
C           Call evaluation routine to get Euler angles
C           phi, delta, w.
C
            CALL PCKE20 ( ET, RECORD, EULANG )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF
C
C           From the PCK type 20 file the Euler angles are
C           retrieved in a particular order. The routine to
C           get the TSIPM matrix from expects them in another
C           order. Here we change from EULANG to ESTATE, which
C           has this proper order.
C
            ESTATE (1) = EULANG (3)
            ESTATE (2) = EULANG (2)
            ESTATE (3) = EULANG (1)
            ESTATE (4) = EULANG (6)
            ESTATE (5) = EULANG (5)
            ESTATE (6) = EULANG (4)

C
C           Call routine which takes Euler angles to transformation
C           matrix.
C
            CALL EUL2XF ( ESTATE, 3, 1, 3, TSIPM )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF


         ELSE
C
C           If data matching the requested body and time was not
C           found, FOUND is false.
C
            FOUND = .FALSE.

         END IF

      END IF

      CALL CHKOUT ( 'PCKMAT' )
      RETURN
      END
