C$Procedure      IRFTRN ( Inertial reference frame transformation )
 
      SUBROUTINE IRFTRN ( REFA, REFB, ROTAB )
 
C$ Abstract
C
C     Return the matrix that transforms vectors from one specified
C     inertial reference frame to another.
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
C     SPK
C
C$ Keywords
C
C     CONVERSION
C     COORDINATES
C     FRAMES
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      CHARACTER*(*)         REFA
      CHARACTER*(*)         REFB
      DOUBLE PRECISION      ROTAB ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     REFA       I   Name of reference frame to transform vectors FROM.
C     REFB       I   Name of reference frame to transform vectors TO.
C     ROTAB      O   REFA-to-REFB transformation matrix.
C
C$ Detailed_Input
C
C     REFA,
C     REFB           Names of two inertial reference frames.  Any names
C                    accepted by the routine IRFNUM may be used.  See
C                    $Particulars for a list of some of the more
C                    commonly used inertial reference frame names.
C
C$ Detailed_Output
C
C     ROTAB          is a rotation matrix that transforms the
C                    coordinates of a vector V relative to the
C                    reference frame specified by REFA to the
C                    coordinates of V relative to the reference frame
C                    specified by REFB.  The transformation is carried
C                    out by the matrix multiplication
C
C                       V = ROTAB * V.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either of the input reference frame names is invalid, the
C         error will be diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Normally applications should call the more general, higher level 
C     routine PXFORM instead of this routine.
C     
C     This routine is a macro that replaces the code fragment
C
C        CALL IRFNUM ( REFA,  CODEA        )
C        CALL IRFNUM ( REFB,  CODEB        )
C        CALL IRFROT ( CODEA, CODEB, ROTAB )
C
C
C     Among the reference frame names accepted by IRFNUM are:
C
C        'J2000'
C        'B1950'
C        'FK4'
C        'DE-96'
C        'DE-102'
C        'DE-108'
C        'DE-111'
C        'DE-114'
C        'DE-118'
C        'DE-122'
C        'DE-125'
C        'DE-130'
C        'DE-200'
C        'DE-202'
C        'GALACTIC'
C
C     See the SPICELIB routine GHGIRF for details.
C
C
C$ Examples
C
C     1)  Transform a vector V1950 from the B1950 to the J2000 
C         reference frame.
C
C            C
C            C     Ask IRFTRN for the matrix that transforms vectors
C            C     from the B1950 to the J2000 reference frame.
C            C
C                  CALL IRFTRN ( 'B1950', 'J2000', TRANS )
C
C            C
C            C     Now transform V1950 to the J2000 reference frame.
C            C
C                  CALL MXV ( TRANS, V1950, V2000 )
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 28-SEP-2004 (NJB)
C
C        Corrected comment in code example in header.  Made other minor
C        updates to header.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1991 (NJB)
C
C-&
 
C$ Index_Entries
C
C     tranformation from one inertial frame to another
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               CODEA
      INTEGER               CODEB
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFTRN' )
      END IF
 
C
C     Encode the reference frame names, and find the transformation
C     matrix.
C
      CALL IRFNUM ( REFA,  CODEA        )
      CALL IRFNUM ( REFB,  CODEB        )
      CALL IRFROT ( CODEA, CODEB, ROTAB )
 
      CALL CHKOUT ( 'IRFTRN' )
      RETURN
      END
