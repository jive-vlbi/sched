C$Procedure PCKEUL ( PCK, get Euler angles at time from PCK file )

      SUBROUTINE PCKEUL ( BODY, ET, FOUND, REF, EULANG )

C$ Abstract
C
C      This routine is obsolete.  It supports only the type 02 binary
C      PCK format.  It is maintained only for backward compatibility
C
C      Return Euler angles and their derivatives and their reference
C      frame, given an input time and body and reference frame from
C      a PCK binary file.
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
C      NAIF_IDS
C     ROTATION
C      TIME
C      PCK
C
C$ Keywords
C
C      TRANSFORMATION
C      ROTATION
C
C$ Declarations

      INTEGER              BODY
      DOUBLE PRECISION     ET
      LOGICAL              FOUND
      CHARACTER*(*)        REF
      DOUBLE PRECISION     EULANG   ( 6 )

C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      BODY       I   ID code of body
C      ET         I   Epoch of transformation
C      FOUND      O   True if ET, BODY found in a PCK file
C      REF        O   Name of inertial ref. frame of state
C      EULANG     O   Euler angles and their derivatives.
C
C$ Detailed_Input
C
C      BODY        is the integer ID code of the body for which the
C                  state transformation matrix is requested. Bodies
C                  are numbered according to the standard NAIF
C                  numbering scheme.  The numbering scheme is
C                  explained in the NAIF_IDS required reading file.
C
C      ET          is the epoch at which the state transformation
C                  matrix is requested.
C
C$ Detailed_Output
C
C      FOUND       if the Euler angles for the requested time
C                  and body are found in a PCK binary file,
C                  FOUND is true.  Otherwise, it's false.
C
C      REF         is the name of an inertial ref. frame.
C                  (See the routine CHGIRF for a full list of names.)
C
C      EULANG      the Euler angles and their derivatives at
C                  time ET. The rotation matrix is
C                  [ EULANG(3) ]  [EULANG(2)] [EULANG(1)]
C                               3            1           3
C
C                  and   dEULANG(1)/dt = EULANG(4)
C                        dEULANG(2)/dt = EULANG(5)
C                        dEULANG(3)/dt = EULANG(6)
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      None.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     None.
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
C     C  Call routine to get Euler angles phi, delta, w.
C
C        CALL PCKEUL ( BODY, ET, FOUND, REF, EULANG )
C
C     The Euler angles and their derivatives are returned
C     in EULANG.
C
C$ Restrictions
C
C      A binary PCK kernel must be loaded with PCKLOF before
C      calling this routine.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K. S. Zukor   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-    SPICELIB Version 2.0.0, 21-MAR-1995 (KSZ)
C
C        PCKEUL modified to check in.  PCKMAT takes
C        over for PCKEUL in many cases.  REF now a character.
C
C-    SPICELIB Version 1.1.0, 18-OCT-1994 (KSZ)
C
C        Fixed bug which incorrecly modded DW by two pi.
C
C-    SPICELIB Version 1.0.0, 11-MAR-1994 (KSZ)
C
C-&

C$ Index_Entries
C
C     get Euler angles and their derivatives
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Parameters
C
C     ND    number of double precision components of descriptor
C     NI    number of integer components of descriptor
C     NR    component number of reference frame in integer
C           portion of descriptor
C     NS    size of a packed PCK segment descriptor
C     NT    component number of data type in integer portion
C           of descriptor
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI     =    5 )

      INTEGER               NR
      PARAMETER           ( NR      =   2 )

      INTEGER               NS
      PARAMETER           ( NS      =   5 )

      INTEGER               NT
      PARAMETER           ( NT      =   3 )

      INTEGER               FILEN
      PARAMETER           ( FILEN   = 128 )

C
C  Local Variables
C

      CHARACTER*(40)  IDENT

      DOUBLE PRECISION      DCD     ( ND )
      DOUBLE PRECISION      DESCR   ( NS )
      DOUBLE PRECISION      RECORD  ( 130 )

      INTEGER     TYPE
      INTEGER     HANDLE
      INTEGER     ICD ( NI )
      INTEGER     IREF

C
C     Standard SPICE Error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKEUL' )
      END IF

C
C     Get a segment applicable to a specified body and epoch.
C
      CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )

      IF ( FOUND ) THEN

C
C        Look at parts of the descriptor.
C
         CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
         TYPE   = ICD( NT )
         IREF   = ICD( NR )
         CALL IRFNAM ( IREF, REF )

         IF ( TYPE .EQ. 2 ) THEN

C
C           Read in Chebyshev coefficients from segment.
C
            CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )

C
C           Call evaluation routine to get Euler angles
C           phi, delta, w.
C
            CALL PCKE02 ( ET, RECORD, EULANG )

         ELSE
C
C           If appropriate data was not found, found is false.
C
            FOUND = .FALSE.

         END IF

      END IF

      CALL CHKOUT ( 'PCKEUL' )
      RETURN
      END





