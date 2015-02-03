C$Procedure GETFOV ( Fetch instrument FOV parameters )
 
      SUBROUTINE GETFOV ( INSTID,
     .                    ROOM,
     .                    SHAPE,
     .                    FRAME,
     .                    BSIGHT,
     .                    N,
     .                    BOUNDS  )
 
C$ Abstract
C
C     This subroutine returns the field-of-view (FOV) parameters for
C     a specified instrument.
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
C     IK
C
C$ Keywords
C
C     INSTRUMENT
C
C$ Declarations
 
      IMPLICIT NONE
 
      DOUBLE PRECISION      MINCOS
      PARAMETER           ( MINCOS = 1.0D-15 )
 
      INTEGER               INSTID
      INTEGER               ROOM
      CHARACTER*(*)         SHAPE
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      BSIGHT (3)
      INTEGER               N
      DOUBLE PRECISION      BOUNDS ( 3, *)
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INSTID     I   NAIF ID of an instrument
C     ROOM       I   Maximum number of vectors that can be returned.
C     SHAPE      O   Instrument FOV shape.
C     FRAME      O   Name of the frame in which FOV vectors are defined.
C     BSIGHT     O   Boresight vector.
C     N          O   Number of boundary vectors returned.
C     BOUNDS     O   FOV boundary vectors.
C
C$ Detailed_Input
C
C     INSTID     is the NAIF ID of an instrument.
C
C     ROOM       is the maximum number of 3D vectors that can be
C                returned in BOUNDS.
C
C$ Detailed_Output
C
C     SHAPE      is a character string that describes the "shape" of
C                the field of view.  Possible values returned are:
C
C                   'POLYGON'
C                   'RECTANGLE'
C                   'CIRCLE'
C                   'ELLIPSE'
C
C                If the value of SHAPE is 'POLYGON' the field of view
C                of the instrument is a pyramidal polyhedron. The
C                vertex of the pyramid is at the instrument focal
C                point. The rays along the edges of the pyramid are
C                parallel to the vectors returned in BOUNDS.
C 
C                If the value of SHAPE is 'RECTANGLE' the field of view
C                of the instrument is a rectangular pyramid. The vertex
C                of the pyramid is at the instrument focal point. The
C                rays along the edges of the pyramid are parallel to
C                the vectors returned in BOUNDS.  Moreover, in this
C                case, the boresight points along the axis of symmetry
C                of the rectangular pyramid.
C
C                If the value of SHAPE is 'CIRCLE' the field of view of
C                the instrument is a circular cone about the boresight
C                vector. The vertex of the cone is at the instrument
C                focal point. A single vector will be returned in
C                BOUNDS.  This vector will be parallel to a ray that
C                lies in the cone that makes up the boundary of the
C                field of view.
C
C                If the value of SHAPE is 'ELLIPSE' the field of view
C                of the instrument is a elliptical cone with the
C                boresight vector as the axis of the cone.  In this
C                case two vectors are returned in BOUNDS. One of the
C                vectors returned in BOUNDS points to the end of the
C                semi-major axis of a perpendicular cross section of
C                the elliptic cone.  The other vector points to the end
C                of the semi-minor axis of a perpendicular cross
C                section of the cone.
C
C     FRAME      is the name of the reference frame in which the field
C                of view boundary vectors are defined.
C
C     BSIGHT     is a vector that points in the direction of the
C                center of the field of view.  The length of BSIGHT
C                is not specified other than being non-zero.
C
C     N          is the number of boundary vectors returned.
C
C     BOUNDS     is an array of vectors that point to the "corners" of
C                the instrument field of view.  (See the discussion
C                accompanying shape for an expansion of the term
C                "corner of the field of view.")  Note that the vectors
C                returned in BOUNDS are not necessarily unit vectors.
C
C$ Parameters
C
C     MINCOS     This parameter is the lower limit on the value of the
C                cosine of the cross or reference angles in the ANGLES
C                specification cases. (see Particulars for further
C                discussion).
C$ Exceptions
C
C     1) The error SPICE(FRAMEMISSING) is signaled if the frame
C        associated with the instrument can not be found in the kernel
C        pool.
C
C     2) The error SPICE(SHAPEMISSING) is signaled if the shape of the
C        instrument field of view can not be found in the kernel pool.
C
C     3) The error 'SPICE(SHAPENOTSUPPORTED)' is signaled if the shape
C        specified by the instrument kernel is not one of the four
C        values: 'CIRCLE', 'POLYGON', 'ELLIPSE', 'RECTANGLE'.   If the
C        ANGLES specification is used it must be: 'CIRCLE', 'ELLIPSE',
C        or 'RECTANGLE'.
C
C     4) The error 'SPICE(BORESIGHTMISSING)' is signaled if
C        the direction of the boresight cannot be located in the
C        kernel pool.
C
C     5) The error 'SPICE(BADBORESIGHTSPEC)' is signaled if
C        the number of components for the boresight vector
C        in the kernel pool is not 3.
C
C     6) The error 'SPICE(BOUNDARYMISSING)' is signaled if
C        the boundary vectors for the edge of the field of view
C        cannot be found in the kernel pool.
C
C     7) The error 'SPICE(BOUNDARYTOOBIG)' is signaled if there
C        is insufficient room (as specified by the variable ROOM)
C        to return all of the vectors associated with the boundary
C        of the field of view.
C
C     8) The error 'SPICE(BADBOUNDARY)' is signaled if the number
C        of components of vectors making up the field of view is
C        not a multiple of 3.
C
C     9) The error 'SPICE(BADBOUNDARY)' is signaled if the number
C        of components of vectors making up the field of view is
C        not compatible with the shape specified for the field of
C        view.
C
C    10) The error 'SPICE(REFVECTORMISSING)' is signaled if the
C        reference vector for the ANGLES spec can not be found
C        in the kernel pool.
C
C    11) The error 'SPICE(BADREFVECTORSPEC)' is signaled if the
C        reference vector stored in the kernel pool to support
C        the ANGLES spec contains an in correct number of components,
C        contains 3 character components, or is parallel to the
C        boresight.
C
C    12) The error 'SPICE(REFANGLEMISSING)' is signaled if the
C        reference angle stored in the kernel pool to support
C        the ANGLES spec is absent from the kernel pool.
C
C    13) The error 'SPICE(UNITSMISSING)' is signaled if the
C        keyword that stores the angular units for the angles
C        used in the ANGLES spec is absent from the kernel pool.
C
C    14) The error 'SPICE(CROSSANGLEMISSING)' is signaled if the
C        keyword that stores the cross angle for the ANGLES spec
C        is needed and is absent from the kernel pool.
C
C    15) The error 'SPICE(BADBOUNDARY)' is signaled if the angles
C        for the RECTANGLE/ANGLES spec case have cosines that
C        are less than those stored in the parameter MINCOS.
C
C    16) The error 'SPICE(UNSUPPORTEDSPEC)' is signaled if the
C        class specification contains something other than 'ANGLES'
C        or 'CORNERS'.
C
C    17) In the event that the CLASS_SPEC keyword is absent from the
C        kernel pool for the instrument whose FOV is sought, this
C        module assumes the default CORNERS specification is to be
C        utilized.
C
C$ Files
C
C     This routine relies upon having successfully loaded an instrument
C     kernel (IK-file) via the routine FURNSH prior to calling this
C     routine.
C
C$ Particulars
C
C     This routine provides a common interface to retrieving
C     the geometric characteristics of an instrument field of
C     view for a wide variety of remote sensing instruments
C     across many different space missions.
C
C     Given the NAIF instrument ID, (and having "loaded" the
C     instrument field of view description via the routine FURNSH)
C     this routine returns the bore-sight of the instrument, the
C     "shape" of the field of view, a collection of vectors
C     that point along the edges of the field of view, and the
C     name of the reference frame in which these vectors are defined.
C
C     Currently this routine supports two classes of specifications
C     for FOV definitions: "corners" and "angles".
C
C     The "corners" specification requires the following keywords
C     defining the shape, boresight, boundary vectors, and reference
C     frame of the FOV be provided in one of the text kernel files
C     (normally an IK file) loaded into the kernel pool (in the
C     keywords below <INSTID> is replaced with the instrument ID as
C     passed into the module):
C
C        INS<INSTID>_FOV_CLASS_SPEC         must be set to 'CORNERS' or
C                                           omitted to indicate the
C                                           "corners"-class
C                                           specification.
C                                           
C
C        INS<INSTID>_FOV_SHAPE              must be set to one of these
C                                           values:
C
C                                              'CIRCLE'
C                                              'ELLIPSE'
C                                              'RECTANGLE'
C                                              'POLYGON'
C
C        INS<INSTID>_FOV_FRAME              must contain the name of
C                                           the frame in which the
C                                           boresight and boundary
C                                           corner vectors are defined.
C
C        INS<INSTID>_BORESIGHT              must be set to a 3D vector
C                                           defining the boresight in
C                                           the FOV frame specified in
C                                           the FOV_FRAME keyword.
C
C        INS<INSTID>_FOV_BOUNDARY   or      
C        INS<INSTID>_FOV_BOUNDARY_CORNERS   must be set to one (for
C                                           FOV_SHAPE = 'CIRCLE'), two
C                                           (for FOV_SHAPE =
C                                           'ELLIPSE'), three (for
C                                           FOV_SHAPE = 'RECTANGLE'),
C                                           or three or more (for
C                                           'POLYGON') 3D vectors
C                                           defining the corners of the
C                                           FOV in the FOV frame
C                                           specified in the FOV_FRAME
C                                           keyword.
C
C     The "angles" specification requires the following keywords
C     defining the shape, boresight, reference vector, reference and
C     cross angular extents of the FOV be provided in one of the text
C     kernel files (normally an IK file) loaded into the kernel
C     pool (in the keywords below <INSTID> is replaced with the
C     instrument ID as passed into the module):
C
C        INS<INSTID>_FOV_CLASS_SPEC         must be set to  'ANGLES' to
C                                           indicate the "angles"-class
C                                           specification.
C
C        INS<INSTID>_FOV_SHAPE              must be set to one of these
C                                           values:
C
C                                              'CIRCLE'
C                                              'ELLIPSE'
C                                              'RECTANGLE'
C
C        INS<INSTID>_FOV_FRAME              must contain the name of
C                                           the frame in which the 
C                                           boresight and the computed
C                                           boundary corner vectors are
C                                           defined.
C
C        INS<INSTID>_BORESIGHT              must be set to a 3D vector
C                                           defining the boresight in
C                                           the FOV frame specified in
C                                           the FOV_FRAME keyword.
C
C        INS<INSTID>_FOV_REF_VECTOR         must be set to a 3D vector
C                                           that together with the
C                                           boresight vector defines
C                                           the plane in which the
C                                           first angular extent of the
C                                           FOV specified in the
C                                           FOV_REF_ANGLE keyword is
C                                           measured.
C
C        INS<INSTID>_FOV_REF_ANGLE          must be set to the angle
C                                           that is 1/2 of the total
C                                           FOV angular extent in the 
C                                           plane defined by the 
C                                           boresight and the vector
C                                           specified in the
C                                           FOV_REF_VECTOR keyword.
C
C        INS<INSTID>_FOV_CROSS_ANGLE        must be set to the angle
C                                           that is 1/2 of the total
C                                           FOV angular extent in the
C                                           plane containing the
C                                           boresight and perpendicular
C                                           to the plane defined by the
C                                           boresight and the vector
C                                           specified in the
C                                           FOV_REF_VECTOR keyword.
C                                           This keyword is not
C                                           required for FOV_SHAPE =
C                                           'CIRCLE'.
C
C        INS<INSTID>_FOV_ANGLE_UNITS        must specify units for the
C                                           angles given in the
C                                           FOV_REF_ANGLE and
C                                           FOV_CROSS_ANGLE keywords.
C                                           Any angular units
C                                           recognized by CONVRT are
C                                           acceptable.
C
C     This routine is intended to be an intermediate level routine.
C     It is expected that users of this routine will be familiar
C     with the SPICE frames subsystem and will be comfortable writing
C     software to further manipulate the vectors retrieved by this
C     routine.
C
C$ Examples
C
C     The example program in this section loads the IK file
C     'example.ti' with the following contents defining four FOVs of
C     various shapes and sizes:
C
C        KPL/IK
C        
C        The keywords below define a circular, 10-degree wide FOV with
C        the boresight along the +Z axis of the 'SC999_INST001' frame
C        for an instrument with ID -999001 using the "angles"-class
C        specification.
C        
C        \begindata
C           INS-999001_FOV_CLASS_SPEC       = 'ANGLES'
C           INS-999001_FOV_SHAPE            = 'CIRCLE'
C           INS-999001_FOV_FRAME            = 'SC999_INST001'
C           INS-999001_BORESIGHT            = ( 0.0, 0.0, 1.0 )
C           INS-999001_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
C           INS-999001_FOV_REF_ANGLE        = ( 5.0 )
C           INS-999001_FOV_ANGLE_UNITS      = ( 'DEGREES' )
C        \begintext
C        
C        The keywords below define an elliptical FOV with 2- and
C        4-degree angular extents in the XZ and XY planes and the
C        boresight along the +X axis of the 'SC999_INST002' frame for
C        an instrument with ID -999002 using the "corners"-class
C        specification.
C        
C        \begindata
C           INS-999002_FOV_SHAPE            = 'ELLIPSE'
C           INS-999002_FOV_FRAME            = 'SC999_INST002'
C           INS-999002_BORESIGHT            = ( 1.0, 0.0, 0.0 )
C           INS-999002_FOV_BOUNDARY_CORNERS = ( 1.0, 0.0, 0.01745506,
C                                               1.0, 0.03492077, 0.0 )
C        \begintext
C        
C        The keywords below define a rectangular FOV with 1.2- and
C        0.2-degree angular extents in the ZX and ZY planes and the
C        boresight along the +Z axis of the 'SC999_INST003' frame for
C        an instrument with ID -999003 using the "angles"-class
C        specification.
C        
C        \begindata
C           INS-999003_FOV_CLASS_SPEC       = 'ANGLES'
C           INS-999003_FOV_SHAPE            = 'RECTANGLE'
C           INS-999003_FOV_FRAME            = 'SC999_INST003'
C           INS-999003_BORESIGHT            = ( 0.0, 0.0, 1.0 )
C           INS-999003_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
C           INS-999003_FOV_REF_ANGLE        = ( 0.6 )
C           INS-999003_FOV_CROSS_ANGLE      = ( 0.1 )
C           INS-999003_FOV_ANGLE_UNITS      = ( 'DEGREES' )
C        \begintext
C        
C        The keywords below define a triangular FOV with the boresight
C        along the +Y axis of the 'SC999_INST004' frame for an
C        instrument with ID -999004 using the "corners"-class
C        specification.
C        
C        \begindata
C           INS-999004_FOV_SHAPE            = 'POLYGON'
C           INS-999004_FOV_FRAME            = 'SC999_INST004'
C           INS-999004_BORESIGHT            = (  0.0,  1.0,  0.0 )
C           INS-999004_FOV_BOUNDARY_CORNERS = (  0.0,  0.8,  0.5,
C                                                0.4,  0.8, -0.2,
C                                               -0.4,  0.8, -0.2 )
C        \begintext
C
C     The program shown below loads the IK, fetches parameters for each
C     of the four FOVs and prints these parameters to the screen.
C
C        IMPLICIT              NONE
C
C        INTEGER               MAXBND
C        PARAMETER           ( MAXBND = 4 )
C        
C        INTEGER               NUMINS
C        PARAMETER           ( NUMINS = 4 )
C        
C        INTEGER               WDSIZE
C        PARAMETER           ( WDSIZE = 32 )
C        
C        CHARACTER*(WDSIZE)    FRAME
C        CHARACTER*(WDSIZE)    SHAPE
C        
C        DOUBLE PRECISION      BOUNDS ( 3, MAXBND )
C        DOUBLE PRECISION      BSIGHT ( 3 )
C        
C        INTEGER               I
C        INTEGER               INSIDS ( NUMINS )
C        INTEGER               J
C        INTEGER               N
C        
C        DATA INSIDS / -999001, -999002, -999003, -999004 /
C        
C        CALL FURNSH( 'example.ti' )
C        
C        WRITE (*,*) '--------------------------------------'
C        DO I = 1, NUMINS
C        
C           CALL GETFOV ( INSIDS(I), MAXBND, 
C       .                 SHAPE, FRAME, BSIGHT, N, BOUNDS )
C        
C           WRITE (*,*) 'Instrument ID: ', INSIDS(I)
C           WRITE (*,*) '    FOV shape: ', SHAPE
C           WRITE (*,*) '    FOV frame: ', frame
C           WRITE (*,*) 'FOV boresight: ', BSIGHT
C           WRITE (*,*) '  FOV corners: '
C           DO J = 1, N
C              WRITE (*,*) '               ', 
C       .                  BOUNDS(1,J), BOUNDS(2,J), BOUNDS(3,J)
C           END DO
C           WRITE (*,*) '--------------------------------------'
C        
C        END DO
C        
C        END
C
C     The program produces the following output:
C
C        --------------------------------------
C        Instrument ID:  -999001
C            FOV shape: CIRCLE
C            FOV frame: SC999_INST001
C        FOV boresight:   0.  0.  1.
C          FOV corners:
C                         0.0871557427  0.  0.996194698
C        --------------------------------------
C        Instrument ID:  -999002
C            FOV shape: ELLIPSE
C            FOV frame: SC999_INST002
C        FOV boresight:   1.  0.  0.
C          FOV corners:
C                         1.  0.  0.01745506
C                         1.  0.03492077  0.
C        --------------------------------------
C        Instrument ID:  -999003
C            FOV shape: RECTANGLE
C            FOV frame: SC999_INST003
C        FOV boresight:   0.  0.  1.
C          FOV corners:
C                         0.0104717682  0.00174523267  0.999943647
C                        -0.0104717682  0.00174523267  0.999943647
C                        -0.0104717682 -0.00174523267  0.999943647
C                         0.0104717682 -0.00174523267  0.999943647
C        --------------------------------------
C        Instrument ID:  -999004
C            FOV shape: POLYGON
C            FOV frame: SC999_INST004
C        FOV boresight:   0.  1.  0.
C          FOV corners:
C                         0.  0.8  0.5
C                         0.4  0.8 -0.2
C                        -0.4  0.8 -0.2
C        --------------------------------------
C
C$ Restrictions
C
C     An I-kernel for the instrument specified in INSTID must have been
C     loaded via a call to FURNSH prior to calling this routine and
C     must contain the specification for the instrument field of view
C     consistent with the expectations of this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton   (JPL)
C     N.J. Bachman (JPL)
C     B.V. Semenov (JPL)
C     W.L. Taber   (JPL)
C     F.S. Turner  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.1  05-FEB-2009 (BVS)
C
C        Header updates: added information about required IK keywords;
C        replaced old example with a new one more focused on GETFOV and
C        IK keywords.
C
C-    SPICELIB Version 2.1.0  23-OCT-2005 (NJB) (BVS)
C
C        Fixed bug causing incorrect computation of the boundary
C        vectors for a rectangular FOV specified using the angular
C        extents method if the reference vector was provided as a
C        non-unit vector and/or was non-perpendicular to the 
C        specified boresight.
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, UNORM, VHAT, VSCL and VCROSS calls.
C
C        Replaced header reference to LDPOOL with reference to FURNSH.
C
C-    SPICELIB Version 2.0.1  29-JUL-2003 (NJB) (CHA)  
C
C        Various header changes were made to improve clarity.  Some
C        minor header corrections were made.
C
C-    SPICELIB Version 2.0.0  15-MAY-2001 (FST)
C
C        Updated the routine to support the new ANGLES specification
C        for RECTANGLE, ELLIPSE, and CIRCLE.
C
C-    SPICELIB Version 1.1.2  10-MAY-2000 (WLT)
C
C        Removed the unused variable INDEX.
C
C-    SPICELIB Version 1.1.1  13-APR-2000 (WLT)
C
C        This routine was harvested from the NEAR specific routine
C        of the same name.  It was enhanced to support the 'RECTANGLE'
C        shape for a field of view (a special case of 'POLYGON'
C        added for the sake of Cassini users).
C
C-&
 
C$ Index_Entries
C
C     return instrument's FOV parameters
C
C-&

C$ Revisions
C
C-    SPICELIB Version 2.1.0  23-OCT-2005 (NJB) (BVS)
C
C        Fixed bug causing incorrect computation of the boundary
C        vectors for a rectangular FOV specified using the angular
C        extents method if the reference vector was provided as a
C        non-unit vector and/or was non-perpendicular to the 
C        specified boresight.
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, UNORM, VHAT, VSCL and VCROSS calls.
C
C        Replaced header reference to LDPOOL with reference to FURNSH.
C
C-& 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
      INTEGER               BSRCHC
      INTEGER               RTRIM
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local parameters
C
 
C
C     Keyword Name Length.
C
      INTEGER               KWDNLN
      PARAMETER           ( KWDNLN = 32 )
 
C
C     Maximum Number of Normal Vectors.
C
      INTEGER               MAXNML
      PARAMETER           ( MAXNML =  4 )
 
C
C     Number of CORNER Shapes Supported.
C
      INTEGER               NSHAPS
      PARAMETER           ( NSHAPS =  4 )
 
C
C     Number of ANGLE Shapes Supported.
C
      INTEGER               NUMASP
      PARAMETER           ( NUMASP =  3 )
 
C
C     Maximum Length of String Data from the kernel pool.
C
      INTEGER               VALLEN
      PARAMETER           ( VALLEN = 80 )
 
C
C     Local variables
C
      CHARACTER*(KWDNLN)    ANGSHP ( NUMASP )
      CHARACTER*(VALLEN)    ANGUNT
      CHARACTER*(KWDNLN)    KWAUNT
      CHARACTER*(KWDNLN)    KWBORE
      CHARACTER*(KWDNLN)    KWBOUN
      CHARACTER*(KWDNLN)    KWCANG
      CHARACTER*(KWDNLN)    KWFRAM
      CHARACTER*(KWDNLN)    KWORD
      CHARACTER*(KWDNLN)    KWRANG
      CHARACTER*(KWDNLN)    KWRVEC
      CHARACTER*(KWDNLN)    KWSHAP
      CHARACTER*(KWDNLN)    KWSPEC
      CHARACTER*(KWDNLN)    SHAPID ( NSHAPS )
      CHARACTER*(VALLEN)    SPEC
      CHARACTER*(1)         TYPE
 
      DOUBLE PRECISION      B      ( 3 )
      DOUBLE PRECISION      B1     ( 3 )
      DOUBLE PRECISION      B2     ( 3 )
      DOUBLE PRECISION      BMAG
      DOUBLE PRECISION      COSCAN
      DOUBLE PRECISION      COSRAN
      DOUBLE PRECISION      CRSANG
      DOUBLE PRECISION      NORMAL ( 3, MAXNML )
      DOUBLE PRECISION      REFANG
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      SINCAN
      DOUBLE PRECISION      SINRAN
      DOUBLE PRECISION      TMPANG
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      VMAG
 
      INTEGER               I
      INTEGER               MXCMP
 
      LOGICAL               FOUND
 
      SAVE                  SHAPID
      SAVE                  ANGSHP
 
C
C     Allowed values of shape identifier. Note that these must be
C     supplied in ascending order
C
      DATA  ( SHAPID (I), I =  1, NSHAPS )
     .     /
     .     'CIRCLE',
     .     'ELLIPSE',
     .     'POLYGON',
     .     'RECTANGLE'
     .     /
 
C
C     Allowed values of the shape identifier for the ANGLES
C     specification.  Note that these must be supplied in ascending
C     order.
C
      DATA  ( ANGSHP (I), I =  1, NUMASP )
     .     /
     .     'CIRCLE',
     .     'ELLIPSE',
     .     'RECTANGLE'
     .     /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETFOV' )
      END IF
 
      KWBOUN = 'INS#_FOV_BOUNDARY'
      KWBORE = 'INS#_BORESIGHT'
      KWSHAP = 'INS#_FOV_SHAPE'
      KWFRAM = 'INS#_FOV_FRAME'
      KWSPEC = 'INS#_FOV_CLASS_SPEC'
      KWRVEC = 'INS#_FOV_REF_VECTOR'
      KWRANG = 'INS#_FOV_REF_ANGLE'
      KWCANG = 'INS#_FOV_CROSS_ANGLE'
      KWAUNT = 'INS#_FOV_ANGLE_UNITS'
 
      MXCMP  = 3*ROOM
 
C
C     Look for the frame keyword and get frame name if found,
C     complain if not.
C
      CALL REPMI  ( KWFRAM, '#',    INSTID, KWORD )
      CALL GCPOOL ( KWORD, 1, 1, I, FRAME,  FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The variable, ''#'', specifying the frame '
     .   //            'which instrument # FOV components are '
     .   //            'defined relative to was not found in the '
     .   //            'kernel pool. Check whether IK file for '
     .   //            'the instrument was loaded into the program '
     .   //            'and whether this variable is specified in '
     .   //            'that file.'                                  )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                    )
         CALL ERRINT ( '#', INSTID                                   )
         CALL SIGERR ( 'SPICE(FRAMEMISSING)'                         )
         CALL CHKOUT ( 'GETFOV'                                      )
         RETURN
 
      END IF
 
C
C     Look for the shape keyword and get shape identifier if found,
C     complain if not.
C
      CALL REPMI  ( KWSHAP, '#', INSTID, KWORD )
      CALL GCPOOL ( KWORD, 1, 1, I, SHAPE, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The variable, ''#'', specifying the shape '
     .   //            'of the instrument # FOV was not found in the '
     .   //            'kernel pool. Check whether IK file for '
     .   //            'the instrument was loaded into the program '
     .   //            'and whether this variable is specified in '
     .   //            'that file.' )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                      )
         CALL ERRINT ( '#', INSTID                                     )
         CALL SIGERR ( 'SPICE(SHAPEMISSING)'                           )
         CALL CHKOUT ( 'GETFOV'                                        )
         RETURN
 
      END IF
 
C
C     Create an upper case, left justified value for SHAPE.  This will
C     provide the desired case-insensitivity to the keyword value.
C
      CALL UCASE ( SHAPE, SHAPE )
      CALL LJUST ( SHAPE, SHAPE )
 
C
C     Check whether shape identified that we got is one from the list
C     of supported, complain if not.
C
      IF ( BSRCHC( SHAPE(1:RTRIM(SHAPE)), NSHAPS, SHAPID ) .EQ. 0) THEN
 
         CALL SETMSG ( 'The FOV shape, ''#'', specified in the '
     .   //            'keyword, ''#'', for the instrument # '
     .   //            'is not supported. See GETFOV subroutine '
     .   //            'header for the list of supported instrument '
     .   //            'FOV shapes.' )
         CALL ERRCH  ( '#', SHAPE(1:RTRIM(SHAPE))                     )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                     )
         CALL ERRINT ( '#', INSTID                                    )
         CALL SIGERR ( 'SPICE(SHAPENOTSUPPORTED)'                     )
         CALL CHKOUT ( 'GETFOV'                                       )
         RETURN
 
      END IF
 
C
C     Look for the boresight keyword and get boresight vector if found,
C     complain if not.
C
      CALL REPMI  ( KWBORE, '#',    INSTID, KWORD )
      CALL DTPOOL ( KWORD,   FOUND, I,      TYPE )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The variable, ''#'', specifying the '
     .   //            'boresight of the instrument # was not found '
     .   //            'in the kernel pool. Check whether IK file '
     .   //            'for the instrument was loaded into the '
     .   //            'program and whether this variable is '
     .   //            'specified in that file.'                      )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                     )
         CALL ERRINT ( '#', INSTID                                    )
         CALL SIGERR ( 'SPICE(BORESIGHTMISSING)'                      )
         CALL CHKOUT ( 'GETFOV'                                       )
         RETURN
 
      END IF
 
C
C     Check whether boresight specified by three coordinates;
C     complain if not.
C
      IF ( I .NE. 3 ) THEN
 
         CALL SETMSG ( 'The number of the boresight vector '
     .   //            'components specified in the ''#'' '
     .   //            'variable is not 3, it is #. Correct it '
     .   //            'in the corresponding IK file to be a '
     .   //            '3-dimensional vector. '                  )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
         CALL ERRINT ( '#', I                                    )
         CALL SIGERR ( 'SPICE(BADBORESIGHTSPEC)'                 )
         CALL CHKOUT ( 'GETFOV'                                  )
         RETURN
 
      ELSE IF ( TYPE .NE. 'N' ) THEN
 
         CALL SETMSG ( 'The boresight vector, stored in the ''#'' '
     .   //            'variable, has not been stored as a vector '
     .   //            'of three numbers.  It has been stored as a '
     .   //            'vector of three strings. '                   )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                    )
         CALL SIGERR ( 'SPICE(BADBORESIGHTSPEC)'                     )
         CALL CHKOUT ( 'GETFOV'                                      )
         RETURN
 
      END IF
 
      CALL GDPOOL ( KWORD, 1, 3, I, BSIGHT, FOUND )
 
C
C     At this point we have gotten all the specification independent
C     information.  Now check for the presence of the FOV class
C     specification keyword.  If it's absent, we default to CORNERS.
C
      SPEC = 'CORNERS'
 
      CALL REPMI  ( KWSPEC, '#',    INSTID, KWORD )
      CALL GCPOOL ( KWORD, 1, 1, I, SPEC,   FOUND )
 
      IF ( EQSTR('CORNERS', SPEC) ) THEN
 
C
C        Look for the FOV boundary vectors, check whether output array
C        is big enough to hold them; complain if not.
C
         CALL REPMI  ( KWBOUN, '#', INSTID, KWORD )
         CALL DTPOOL ( KWORD, FOUND, N, TYPE )
 
         IF ( .NOT. FOUND ) THEN
            CALL SUFFIX ( '_CORNERS', 0, KWORD )
            CALL DTPOOL ( KWORD, FOUND, N, TYPE )
         END IF
 
         IF ( .NOT. FOUND ) THEN
 
            CALL REPMI  ( KWBOUN, '#', INSTID, KWORD )
            CALL SETMSG ( 'The variable, ''#'', specifying the '
     .      //            'boundary vectors of the instrument # '
     .      //            'FOV was not found in the kernel pool. '
     .      //            'Check whether IK file for the instrument '
     .      //            'was loaded into the program and whether '
     .      //            'this variable is specified in that file.'  )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL ERRINT ( '#', INSTID                                 )
            CALL SIGERR ( 'SPICE(BOUNDARYMISSING)'                    )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN
 
         END IF
 
C
C        Check whether we have enough room to get all boundary vectors,
C        complain if not.
C
         IF ( N .GT. MXCMP ) THEN
 
            CALL SETMSG ( 'The number of boundary vector components '
     .      //            'specified in the ''#'' pool variable is '
     .      //            'bigger than room to hold them in output '
     .      //            'array specified by the ROOM input '
     .      //            'variable of the GETFOV subroutine.'        )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'                     )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN
 
         END IF
 
C
C        Check whether number of boundary components can be divided by 3
C        without reminder.
C
         IF ( MOD( N, 3 ) .NE. 0 ) THEN
 
            CALL SETMSG ( 'The boundary vector components '
     .      //            'specified in the ''#'' pool variable do  '
     .      //            'not represent a set of 3-dimensional '
     .      //            'vectors. Number of components assigned '
     .      //            'to the variable cannot be divided by 3 '
     .      //            'without reminder. '                        )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                        )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN
 
         END IF
 
C
C        Boundaries are OK. Get them.
C
         CALL GDPOOL ( KWORD, 1, MXCMP, N, BOUNDS, FOUND )
 
         N = N/3
 
         IF      ( SHAPE .EQ. 'CIRCLE'      .AND. N .NE. 1 ) THEN
 
            CALL SETMSG ( 'The boundary is specified to be '
     .      //            'circular, and as such, the values '
     .      //            'associated with keyword, ''#'', '
     .      //            'should contain one vector.  There '
     .      //            'are #.'                              )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))            )
            CALL ERRINT ( '#', N                                )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                  )
            CALL CHKOUT ( 'GETFOV'                              )
            RETURN
 
         ELSE IF ( SHAPE .EQ. 'ELLIPSE'     .AND. N .NE. 2 ) THEN
 
            CALL SETMSG ( 'The boundary is specified to be '
     .      //            'elliptical, and as such, the values '
     .      //            'associated with keyword, ''#'', '
     .      //            'should contain two vectors.  There '
     .      //            'are #.'                               )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))             )
            CALL ERRINT ( '#', N                                 )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                   )
            CALL CHKOUT ( 'GETFOV'                               )
            RETURN
 
         ELSE IF ( SHAPE .EQ. 'RECTANGLE' .AND. N .NE. 4 ) THEN
 
            CALL SETMSG ( 'The boundary is specified to be '
     .      //            'rectangular, and as such, the values '
     .      //            'associated with keyword, ''#'', '
     .      //            'should contain four vectors.  There '
     .      //            'are #.'                                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))              )
            CALL ERRINT ( '#', N                                  )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                    )
            CALL CHKOUT ( 'GETFOV'                                )
            RETURN
 
         ELSE IF ( SHAPE .EQ. 'POLYGON' .AND. N .LT. 3 ) THEN
 
            CALL SETMSG ( 'The boundary is specified to be '
     .      //            'polygonal, and as such, the values '
     .      //            'associated with keyword, ''#'', '
     .      //            'should contain at least three '
     .      //            'vectors.  There are #.'              )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))            )
            CALL ERRINT ( '#', N                                )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                  )
            CALL CHKOUT ( 'GETFOV'                              )
            RETURN
 
         END IF
 
C
C     Now check to see if the FOV specification is ANGLES and
C     compute the boundary corner vectors.
C
      ELSE IF ( EQSTR('ANGLES', SPEC) ) THEN
 
C
C        Check whether shape identified that we got is one from the list
C        of supported shapes for the ANGLE specification; complain
C        if not.
C
         IF (BSRCHC(SHAPE(1:RTRIM(SHAPE)), NUMASP, ANGSHP) .EQ. 0) THEN
 
            CALL SETMSG ( 'The FOV shape, ''#'', specified in the '
     .      //            'keyword, ''#'', for the instrument # '
     .      //            'is not supported for the ANGLES '
     .      //            'specification.'                          )
            CALL ERRCH  ( '#', SHAPE(1:RTRIM(SHAPE))                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
            CALL ERRINT ( '#', INSTID                               )
            CALL SIGERR ( 'SPICE(SHAPENOTSUPPORTED)'                )
            CALL CHKOUT ( 'GETFOV'                                  )
            RETURN
 
         END IF
 
C
C        Now fetch all of the elements independent of shape from the
C        ANGLES specification.  Start by looking for the reference
C        vector keyword.  If found, fetch it otherwise complain.
C
         CALL REPMI  ( KWRVEC, '#',    INSTID, KWORD )
         CALL DTPOOL ( KWORD,   FOUND, I,      TYPE )
 
         IF ( .NOT. FOUND ) THEN
 
            CALL SETMSG ( 'The variable, ''#'', specifying the '
     .      //            'FOV reference vector of the instrument '
     .      //            '# was not found in the kernel pool. '
     .      //            'Check whether IK file for the instrument '
     .      //            'was loaded into the program and whether '
     .      //            'this variable is specified in that file.'  )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL ERRINT ( '#', INSTID                                 )
            CALL SIGERR ( 'SPICE(REFVECTORMISSING)'                   )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN
 
         END IF
 
C
C        Now check whether reference vector is specified by three
C        coordinates; complain if not.
C
         IF ( I .NE. 3 ) THEN
 
            CALL SETMSG ( 'The number of the reference vector '
     .      //            'components specified in the ''#'' '
     .      //            'keyword is not 3, it is #. Check the '
     .      //            'corresponding IK FOV definition for '
     .      //            'errors.'                               )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))              )
            CALL ERRINT ( '#', I                                  )
            CALL SIGERR ( 'SPICE(BADREFVECTORSPEC)'               )
            CALL CHKOUT ( 'GETFOV'                                )
            RETURN
 
         ELSE IF ( TYPE .NE. 'N' ) THEN
 
            CALL SETMSG ( 'The reference vector, stored in ''#'', '
     .      //            'has not been stored as a vector of '
     .      //            'three numbers.  It has been stored as '
     .      //            'a vector of three strings. '             )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
            CALL SIGERR ( 'SPICE(BADREFVECTORSPEC)'                 )
            CALL CHKOUT ( 'GETFOV'                                  )
            RETURN
 
         END IF
 
         CALL GDPOOL ( KWORD, 1, 3, I, REFVEC, FOUND )
 
C
C        We require that the reference vector is not parallel
C        to the boresight vector. Use NORMAL(1,1) to temporarily
C        store the result of the cross product.
C
         CALL VCRSS ( BSIGHT, REFVEC, NORMAL(1,1) )
 
         IF ( VNORM ( NORMAL(1,1) ) .EQ. 0.0D0 ) THEN
 
            CALL SETMSG ( 'The reference vector, stored in ''#'', '
     .      //            'is parallel to the instrument boresight '
     .      //            'vector.  This is not allowed by the '
     .      //            'ANGLES FOV specification.'                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                 )
            CALL SIGERR ( 'SPICE(BADREFVECTORSPEC)'                  )
            CALL CHKOUT ( 'GETFOV'                                   )
            RETURN
 
         END IF
 
C
C        Retrieve the reference angle from the kernel pool.
C
         CALL REPMI  ( KWRANG, '#',    INSTID, KWORD )
         CALL GDPOOL ( KWORD, 1, 1, I, REFANG, FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
            CALL SETMSG ( 'The variable, ''#'', specifying the '
     .      //            'reference angle which describes '
     .      //            'instrument # FOV angular extent was '
     .      //            'not found in the kernel pool. Check '
     .      //            'whether IK file for the instrument '
     .      //            'was loaded into the program and '
     .      //            'whether this variable is specified '
     .      //            'in that file.'                        )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))             )
            CALL ERRINT ( '#', INSTID                            )
            CALL SIGERR ( 'SPICE(REFANGLEMISSING)'               )
            CALL CHKOUT ( 'GETFOV'                               )
            RETURN
 
         END IF
 
C
C        Retrieve the angle units from the kernel pool.
C
         CALL REPMI  ( KWAUNT, '#',    INSTID, KWORD )
         CALL GCPOOL ( KWORD, 1, 1, I, ANGUNT, FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
            CALL SETMSG ( 'The variable, ''#'', specifying the '
     .      //            'angular units in which instrument # '
     .      //            'FOV extent is defined was not found '
     .      //            'in the kernel pool. Check whether '
     .      //            'IK file for the instrument was '
     .      //            'loaded into the program and whether '
     .      //            'this variable is specified in that '
     .      //            'file.'                                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))             )
            CALL ERRINT ( '#', INSTID                            )
            CALL SIGERR ( 'SPICE(UNITSMISSING)'                  )
            CALL CHKOUT ( 'GETFOV'                               )
            RETURN
 
         END IF
 
C
C        Convert the reference angle to radians.
C
         CALL CONVRT ( REFANG, ANGUNT, 'RADIANS', TMPANG )
         REFANG = TMPANG

C
C        Branch to shape specific code.
C
         IF ( SHAPE .EQ. 'CIRCLE' ) THEN
 
C
C           First check to see that the caller left enough room
C           to store the required number of boundary corner
C           vectors.
C
            IF ( ROOM .LT. 1 ) THEN
               CALL SETMSG ( 'The FOV shape for instrument # '
     .         //            'is specified to be circular.  '
     .         //            'There should be room for at '
     .         //            'least one boundary vector.  There '
     .         //            'is room for #. '                    )
               CALL ERRINT ( '#', INSTID                          )
               CALL ERRINT ( '#', ROOM                            )
               CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'              )
               CALL CHKOUT ( 'GETFOV'                             )
               RETURN
            END IF
 
C
C           The plan to compute the boundary corner vector is to
C           rotate the BSIGHT by REFANG towards REFVEC.  To do
C           this first compute the axis we need to rotate about.
C
            CALL VCRSS ( BSIGHT, REFVEC, NORMAL(1,1) )
 
C
C           Now rotate by REFANG about NORMAL(1,1) using the routine
C           VROTV.
C
            CALL VROTV ( BSIGHT, NORMAL(1,1), REFANG, BOUNDS(1,1) )
 
C
C           Lastly, since we computed a single boundary corner vector,
C           set N = 1.
C
            N = 1
 
         ELSE IF ( SHAPE .EQ. 'ELLIPSE' ) THEN
 
C
C           The elliptical case requires the additional cross angle
C           keyword's presence in the kernel pool. Attempt to
C           retrieve it.
C
            CALL REPMI  ( KWCANG, '#',    INSTID, KWORD )
            CALL GDPOOL ( KWORD, 1, 1, I, CRSANG, FOUND )
 
            IF ( .NOT. FOUND ) THEN
 
               CALL SETMSG ( 'The variable, ''#'', specifying the '
     .         //            'cross angle which describes instrument '
     .         //            '# FOV angular extent was not found in '
     .         //            'the kernel pool. Check whether IK file '
     .         //            'for the instrument was loaded into the '
     .         //            'program and whether this variable is '
     .         //            'specified in that file.'                 )
               CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
               CALL ERRINT ( '#', INSTID                               )
               CALL SIGERR ( 'SPICE(CROSSANGLEMISSING)'                )
               CALL CHKOUT ( 'GETFOV'                                  )
               RETURN
 
            END IF
 
C
C           Convert the cross angle to radians.
C
            CALL CONVRT ( CRSANG, ANGUNT, 'RADIANS', TMPANG )
            CRSANG = TMPANG
C
C           Now check to see that the caller left enough room
C           to store the required number of boundary corner
C           vectors.
C
            IF ( ROOM .LT. 2 ) THEN
               CALL SETMSG ( 'The FOV shape for instrument # '
     .         //            'is specified to be elliptical.  '
     .         //            'There should be room for at '
     .         //            'least two boundary vectors.  There '
     .         //            'is room for #. '                     )
               CALL ERRINT ( '#', INSTID                           )
               CALL ERRINT ( '#', ROOM                             )
               CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'               )
               CALL CHKOUT ( 'GETFOV'                              )
               RETURN
            END IF
 
C
C           The plan to compute the first boundary corner vector is
C           to rotate the BSIGHT by REFANG towards REFVEC.  To
C           do this first compute the axis we need to rotate about.
C
            CALL VCRSS ( BSIGHT, REFVEC, NORMAL(1,1) )
 
C
C           Now rotate by REFANG about NORMAL(1,1) using the routine
C           VROTV.
C
            CALL VROTV ( BSIGHT, NORMAL(1,1), REFANG, BOUNDS(1,1) )
 
C
C           At this point we have one boundary vector.  We need the
C           second and final one.  The strategy we will use is the
C           following: rotate BSIGHT by CRSANG towards NORMAL(1,1).
C           This will give us boundary corner vectors listed in a
C           counter-clockwise fashion about the boresight.
C
            CALL VCRSS ( BSIGHT, NORMAL(1,1), TMPVEC      )
            CALL VEQU  ( TMPVEC,              NORMAL(1,2) )
C
C           Now rotate BSIGHT by CRSANG about the NORMAL(1,2) using
C           the routine VROTV.
C
            CALL VROTV ( BSIGHT, NORMAL(1,2), CRSANG, BOUNDS(1,2) )
 
C
C           Lastly, since we computed two boundary corner vectors,
C           set N = 2.
C
            N = 2
 
         ELSE IF ( SHAPE .EQ. 'RECTANGLE' ) THEN
 
C
C           The rectangular case requires the additional cross angle
C           keyword's presence in the kernel pool. Attempt to
C           retrieve it.
C
            CALL REPMI  ( KWCANG, '#',    INSTID, KWORD )
            CALL GDPOOL ( KWORD, 1, 1, I, CRSANG, FOUND )
 
            IF ( .NOT. FOUND ) THEN
 
               CALL SETMSG ( 'The variable, ''#'', specifying the '
     .         //            'cross angle which describes instrument '
     .         //            '# FOV angular extent was not found in '
     .         //            'the kernel pool. Check whether IK file '
     .         //            'for the instrument was loaded into the '
     .         //            'program and whether this variable is '
     .         //            'specified in that file.'                 )
               CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
               CALL ERRINT ( '#', INSTID                               )
               CALL SIGERR ( 'SPICE(CROSSANGLEMISSING)'                )
               CALL CHKOUT ( 'GETFOV'                                  )
               RETURN
 
            END IF
 
C
C           Convert the cross angle to radians.
C
            CALL CONVRT ( CRSANG, ANGUNT, 'RADIANS', TMPANG )
            CRSANG = TMPANG
C
C           Now check to see that the caller left enough room
C           to store the required number of boundary corner
C           vectors.
C
            IF ( ROOM .LT. 4 ) THEN
               CALL SETMSG ( 'The FOV shape for instrument # '
     .         //            'is specified to be rectangular.  '
     .         //            'There should be room for at '
     .         //            'least four boundary vectors.  There '
     .         //            'is room for #. '                      )
               CALL ERRINT ( '#', INSTID                            )
               CALL ERRINT ( '#', ROOM                              )
               CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'                )
               CALL CHKOUT ( 'GETFOV'                               )
               RETURN
            END IF
 
C
C           Here's the general strategy laid out in simple terms:
C
C           (1) Normalize BSIGHT, label it B.
C
C           (2) Compute the unit vector in the plane defined by REFVEC
C               and B that is normal to B and pointing towards
C               REFVEC, label this B1.
C
C           (3) Cross B and B1 to obtain B2. These three vectors
C               form a basis that is 'aligned' with the FOV cone.
C
C           (4) Compute the inward normals to the sides of the
C               rectangular cone in a counter-clockwise order
C               about the boresight:
C
C                 NORMAL(1) = -COS(REFANG)*B1 + SIN(REFANG)*B
C                 NORMAL(2) = -COS(CRSANG)*B2 + SIN(CRSANG)*B
C                 NORMAL(3) =  COS(REFANG)*B1 + SIN(REFANG)*B
C                 NORMAL(4) =  COS(CRSANG)*B2 + SIN(CRSANG)*B
C
C           (5) Compute the appropriate cross products to obtain
C               a set of boundary corner vectors:
C
C                 BOUNDS(1) = NORMAL(1) x NORMAL(2)
C                 BOUNDS(2) = NORMAL(2) x NORMAL(3)
C                 BOUNDS(3) = NORMAL(3) x NORMAL(4)
C                 BOUNDS(4) = NORMAL(4) x NORMAL(1)
C
C           (6) Unitize and scale BOUNDS to match the length
C               of the BSIGHT.
C
C           Start with step (1).
C
            CALL UNORM ( BSIGHT, B, BMAG )
 
C
C           Now proceed to (2). Since we already know that REFVEC
C           and BSIGHT are not parallel, the following yields a
C           non-zero vector:
C
            CALL VPERP ( REFVEC, BSIGHT, B1 )

C
C           Unitize B1. 
C
            CALL VHAT  ( B1,     TMPVEC )
            CALL VEQU  ( TMPVEC, B1     )
 
C
C           Step (3), compute B2 by crossing B and B1.
C
            CALL VCRSS ( B, B1, B2 )
 
C
C           Before proceeding onto step (4), verify that the
C           results of the calculations in step (4) will make
C           sense.  Check the cosines of CRSANG and REFANG.
C           Signal an error if both are not positive numbers.
C           Use MINCOS as a tolerance.
C
            COSRAN = DCOS ( REFANG )
            COSCAN = DCOS ( CRSANG )
 
            IF ( ( COSRAN .LT. MINCOS ) .OR.
     .           ( COSCAN .LT. MINCOS )      ) THEN
 
               CALL SETMSG ( 'The angular extents specified '
     .         //            'in the FOV definition for '
     .         //            'instrument # result in degenerate '
     .         //            'or improper boundary corner '
     .         //            'vectors.  This usually is the case '
     .         //            'when one or both of the angles '
     .         //            'specified is 90 degrees.'            )
               CALL ERRINT ( '#', INSTID                           )
               CALL SIGERR ( 'SPICE(BADBOUNDARY)'                  )
               CALL CHKOUT ( 'GETFOV'                              )
               RETURN
 
            END IF
 
C
C           Compute the NORMAL vectors to complete step (4).
C
            SINRAN = DSIN ( REFANG )
            SINCAN = DSIN ( CRSANG )
 
            CALL VLCOM ( -COSRAN, B1, SINRAN, B, NORMAL(1,1) )
            CALL VLCOM ( -COSCAN, B2, SINCAN, B, NORMAL(1,2) )
            CALL VLCOM (  COSRAN, B1, SINRAN, B, NORMAL(1,3) )
            CALL VLCOM (  COSCAN, B2, SINCAN, B, NORMAL(1,4) )
 
C
C           We are almost finished. Compute the boundary corner
C           vectors completing step (5).
C
            CALL VCRSS ( NORMAL(1,1), NORMAL(1,2), BOUNDS(1,1) )
            CALL VCRSS ( NORMAL(1,2), NORMAL(1,3), BOUNDS(1,2) )
            CALL VCRSS ( NORMAL(1,3), NORMAL(1,4), BOUNDS(1,3) )
            CALL VCRSS ( NORMAL(1,4), NORMAL(1,1), BOUNDS(1,4) )
 
C
C           Step (6), normalize the boundary corner vectors
C           and scale by BMAG, the magnitude of BSIGHT.
C
            DO I = 1, 4
 
               CALL UNORM ( BOUNDS(1,I),  TMPVEC,  VMAG        )
               CALL VSCL  ( BMAG,         TMPVEC,  BOUNDS(1,I) )
 
            END DO
 
C
C           Lastly since we are returning 4 boundary corner vectors,
C           set N = 4.
C
            N = 4
 
         ELSE
 
C
C           If we end up here something is terribly wrong with
C           this module or SPICE in general.
C
            CALL SETMSG ( 'This error is never supposed to '
     .      //            'occur. We have an undefined shape '
     .      //            'for the ANGLES specification that '
     .      //            'passed the shape check.'            )
            CALL SIGERR ( 'SPICE(BUG)'                         )
            CALL CHKOUT ( 'GETFOV'                             )
            RETURN
 
         END IF
 
      ELSE
 
         CALL SETMSG ( 'The FOV class specification is set to ''#'' '
     .   //            'which is currently unsupported. See the '
     .   //            'GETFOV subroutine header for more '
     .   //            'information.'                                 )
         CALL ERRCH  ( '#', SPEC                                      )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDSPEC)'                       )
         CALL CHKOUT ( 'GETFOV'                                       )
         RETURN
 
      END IF
 
C
C     Standard SPICE error handling.
C
      CALL CHKOUT ( 'GETFOV' )
      RETURN
 
      END
