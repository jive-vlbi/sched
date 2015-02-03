C$Procedure ZZDYNFRM ( Dynamic state transformation evaluation )

      SUBROUTINE ZZDYNFRM ( INFRAM, CENTER, ET, XFORM, BASFRM )
      IMPLICIT NONE 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     For a specified dynamic frame, find the transformation
C     that maps states from the dynamic frame to its base frame.
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
C     CK
C     FRAMES
C     PCK
C     SPK
C
C$ Keywords
C
C     FRAMES
C     PRIVATE
C
C$ Declarations

      INCLUDE 'frmtyp.inc'
      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzdyn.inc'

      INTEGER               INFRAM
      INTEGER               CENTER
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      XFORM ( 6, 6 )
      INTEGER               BASFRM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INFRAM     I   Frame ID code for a SPICE dynamic reference frame.
C     CENTER     I   ID code for the center of the input frame.
C     ET         I   An epoch in seconds past J2000 TDB.
C     XFORM      O   The requested state transformation matrix.
C     BASFRM     O   Frame ID of base frame associated with INFRAM.
C
C$ Detailed_Input
C
C     INFRAM      is the frame ID code for a dynamic reference frame.
C                 Note that this interface differs from that of TKFRAM,
C                 which uses a class ID to identify the frame.
C
C                 In this routine, we refer this frame both as the
C                 "input frame" and the "defined frame."
C
C     CENTER      is NAIF ID code for the center of the frame
C                 designated by INFRAM.  This code, although derivable
C                 from INFRAM, is passed in for convenience.
C
C     ET          is an epoch in ephemeris seconds past J2000 for which
C                 the caller requests a state transformation matrix.
C
C$ Detailed_Output
C
C     XFORM       is a 6x6 matrix that transforms states relative to
C                 INFRAM to states relative to BASFRM.
C
C     BASFRM      is the frame ID code of the base frame associated
C                 with INFRAM.  The 6x6 matrix XFORM transforms states
C                 relative to INFRAM to states relative to BASFRM. The
C                 state transformation is performed by left-multiplying
C                 by XFORM a state expressed relative to INFRAM. This
C                 is easily accomplished via the subroutine call shown
C                 below.
C
C                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
C
C$ Parameters
C
C     See include file zzdyn.inc.
C
C$ Exceptions
C
C     1)  If a dynamic frame evaluation requires unavailable kernel
C         data, the error will be diagnosed by routines in the call
C         tree of this routine.
C
C     2)  If a precession model is used to implement a frame centered
C         at a body for which the model is not applicable, the error
C         SPICE(INVALIDSELECTION) will be signaled.
C
C     3)  If a nutation model is used to implement a frame centered
C         at a body for which the model is not applicable, the error
C         SPICE(INVALIDSELECTION) will be signaled.
C
C     4)  If an obliquity model is used to implement a frame centered
C         at a body for which the model is not applicable, the error
C         SPICE(INVALIDSELECTION) will be signaled.
C
C     5)  If an unrecognized precession model is specified, the 
C         error SPICE(NOTSUPPORTED) is signaled.
C
C     6)  If an unrecognized nutation model is specified, the 
C         error SPICE(NOTSUPPORTED) is signaled.
C
C     7)  If an unrecognized obliquity model is specified, the 
C         error SPICE(NOTSUPPORTED) is signaled.
C
C     8)  If an attempt to look up the center of a frame does
C         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
C         signaled.
C
C     9)  In a two-vector frame definition, if a constant vector
C         specification method is not recognized, the error
C         SPICE(NOTSUPPORTED) is signaled.
C
C     10) In a two-vector frame definition, if a vector definition
C         method is not recognized, the error SPICE(NOTSUPPORTED) 
C         is signaled.
C
C     11) If an unrecognized dynamic frame family is specified, the 
C          error SPICE(NOTSUPPORTED) is signaled.
C
C     12) If an unrecognized dynamic frame definition style is
C         specified, the error SPICE(NOTSUPPORTED) is signaled.
C
C     13) If an unrecognized dynamic frame rotation state is
C         specified, the error SPICE(NOTSUPPORTED) is signaled.
C
C     14) If both a freeze epoch and a rotation state are specified,
C         the error SPICE(FRAMEDEFERROR) is signaled.
C
C     15) If neither a freeze epoch nor a rotation state are specified
C         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
C         signaled.
C
C     16) In a two-vector frame definition, if an invalid axis
C         specification is encountered, the error SPICE(INVALIDAXIS) is
C         signaled.
C
C     17) In a two-vector frame definition using a target near point
C         vector, if the body-fixed frame associated with the target
C         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
C
C     18) If the state of the near point on a target as seen from
C         an observer cannot be computed, the error 
C         SPICE(DEGENERATECASE) is signaled.
C
C     19) If a dynamic frame evaluation requires excessive recursion
C         depth, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     20) When a two-vector dynamic frame is evaluated, if the
C         primary and secondary vectors have angular separation less
C         than the minimum allowed value, or if the angular separation
C         differs from Pi by less than the minimum allowed value, the
C         error SPICE(DEGENERATECASE) is signaled.  The default minimum
C         separation is given by the parameter LBSEP; this value may be
C         overridden by supplying a different value in the frame
C         definition.           
C
C     21) If invalid units occur in a frame definition, the error
C         will be diagnosed by a routine in the call tree of this
C         routine.
C
C     22) If an invalid Euler axis sequence occurs in a frame
C         definition, the error will be diagnosed by a routine in the
C         call tree of this routine.
C
C$ Files
C
C     1) SPK files containing data for each observer and target
C        are required to support two-vector frames.  Note that
C        observer-target pairs can be implicit, as in the case
C        of a constant vector whose frame is evaluated at a 
C        light-time corrected epoch:  the light time the frame
C        center to an observer must be computable in this case,
C        which implies the state of the frame center as seen by
C        the observer must be computable.        
C
C     2) Any of SPK, CK, PCK, and frame kernels will also be required
C        if any frames referenced in the definition of INFRAM (as a
C        base frame, velocity vector frame, or constant vector frame)
C        require them, or if any vectors used to define INFRAM require
C        these data in order to be computable.
C
C     3) When CK data are required, one or more associated SCLK kernels
C        ---normally, one kernel per spacecraft clock---are 
C        required as well.  A leapseconds kernel may be required
C        whenever an SCLK kernel is required.
C
C     4) When a two-vector frame is defined using a target near point,
C        a PCK file giving orientation and providing a triaxial shape
C        model for the target body is required.
C
C$ Particulars
C
C     Currently only parameterized dynamic frames are supported by
C     this routine.
C
C     Currently supported parameterized dynamic families are:
C
C        Two-vector
C        ==========
C
C           Vector definitions
C           ------------------
C           Observer-target position 
C           Observer-target velocity  
C           Near point on target  
C           Constant vector in specified frame
C
C
C        Mean Equator and Equinox of Date
C        ================================
C
C           Bodies and models
C           -----------------
C           Earth:  1976 IAU precession model
C
C
C        Mean Ecliptic and Equinox of Date
C        ================================
C
C           Bodies and models
C           -----------------
C           Earth:  1976 IAU precession model
C                   1980 IAU mean obliquity model
C
C
C        True Equator and Equinox of Date
C        ================================
C
C           Bodies and models
C           -----------------
C           Earth:  1976 IAU precession model
C                   1980 IAU nutation model
C
C
C        Euler frames
C        ============
C        
C           Euler angle definitions
C           -----------------------
C           Polynomial 
C
C
C$ Examples
C
C     See FRMGET.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
C
C     2) Many numerical problems can occur when dynamic frames
C        are evaluated.  Users must determine whether dynamic frame
C        definitions are suitable for their applications. See the
C        Exceptions section for a list of possible problems.
C
C     3) Use of aberration corrections may lead to severe loss of
C        accuracy in state transformation derivatives. 
C
C     4) Two-vector frame definitions can suffer extreme loss of
C         precision due to near-singular geometry.
C
C     5) Two-vector frame definitions involving velocity vectors
C        require numerical differentiation in order to compute the
C        derivative of the state transformation.  Such derivatives may
C        have low accuracy.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 02-MAR-2010 (NJB)
C
C        Typo in Brief_I/O section was corrected: "Class ID"
C        was changed to "Frame ID." Corrected order of header
C        sections.
C
C-    SPICELIB Version 1.1.0, 23-OCT-2005 (NJB)
C
C        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
C
C        Call to ZZBODVCD was replaced with call to BODVCD.
C
C-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB) 
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP

      INTEGER               ISRCHC

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'ZZDYNFRM' )


      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               VNAMLN
      PARAMETER           ( VNAMLN = 4 )
    
C
C     Local variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(1)         AXES   ( 3 )
      CHARACTER*(KVLEN)     AXNAME
      CHARACTER*(FRNMLN)    CFRMNM
      CHARACTER*(BDNMLN)    CTRNAM
      CHARACTER*(CORLEN)    CVCORR
      CHARACTER*(KVLEN)     DYNSTL
      CHARACTER*(KVLEN)     DYNFAM
      CHARACTER*(FRNMLN)    INNAME
      CHARACTER*(KVNMLN)    ITMABC ( 2 ) 
      CHARACTER*(KVNMLN)    ITMAXE ( 2 ) 
      CHARACTER*(KVNMLN)    ITMCOF ( 3 ) 
      CHARACTER*(KVNMLN)    ITMDEC ( 2 ) 
      CHARACTER*(KVNMLN)    ITMFRM ( 2 ) 
      CHARACTER*(KVNMLN)    ITMLAT ( 2 )
      CHARACTER*(KVNMLN)    ITMLON ( 2 )
      CHARACTER*(KVNMLN)    ITMOBS ( 2 )
      CHARACTER*(KVNMLN)    ITMRA  ( 2 )
      CHARACTER*(KVNMLN)    ITMSEP
      CHARACTER*(KVNMLN)    ITMSPC ( 2 )
      CHARACTER*(KVNMLN)    ITMTRG ( 2 )
      CHARACTER*(KVNMLN)    ITMUNT ( 2 )
      CHARACTER*(KVNMLN)    ITMVDF ( 2 )
      CHARACTER*(KVNMLN)    ITMVEC ( 2 )
      CHARACTER*(KVLEN)     NUTMOD
      CHARACTER*(KVLEN)     OBLMOD
      CHARACTER*(FRNMLN)    BASNAM
      CHARACTER*(KVLEN)     PRCMOD
      CHARACTER*(KVLEN)     ROTSTA
      CHARACTER*(KVLEN)     SPEC
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(KVLEN)     TMPFAM
      CHARACTER*(KVLEN)     UNITS
      CHARACTER*(KVLEN)     VECDEF ( 2 )
      CHARACTER*(FRNMLN)    VELFRM
      CHARACTER*(VNAMLN)    VNAME  ( 2 )

      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      ANGLES ( 2 )
      DOUBLE PRECISION      COEFFS ( MAXCOF, 3 )
      DOUBLE PRECISION      CTRPOS ( 3 )
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIRVEC ( 3 )
      DOUBLE PRECISION      DMOB
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EULANG ( 6 )
      DOUBLE PRECISION      FET
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MINSEP
      DOUBLE PRECISION      MOB
      DOUBLE PRECISION      NUTXF  ( 6, 6 )
      DOUBLE PRECISION      OBLXF  ( 6, 6 )
      DOUBLE PRECISION      POLY   ( 0 : 1 )
      DOUBLE PRECISION      PRECXF ( 6, 6 )
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      S2     ( 6, 2 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      STALT  ( 2 )
      DOUBLE PRECISION      STEMP  ( 6 )
      DOUBLE PRECISION      STNEAR ( 6 )
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      TARRAY ( 3 )
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      VARRAY ( 3, 3 )
      DOUBLE PRECISION      VET
      DOUBLE PRECISION      VFLT
      DOUBLE PRECISION      XF2000 ( 6, 6 )
      DOUBLE PRECISION      XFINV  ( 6, 6 )
      DOUBLE PRECISION      XFTEMP ( 6, 6 )
      DOUBLE PRECISION      XIPM   ( 6, 6 )
      DOUBLE PRECISION      XOUT   ( 6, 6 )

      INTEGER               AXIS   ( 2 )
      INTEGER               CFRMID
      INTEGER               CVOBS
      INTEGER               DEGS   ( 3 )
      INTEGER               EARTH
      INTEGER               FRCID
      INTEGER               FRCLS
      INTEGER               FRCTR
      INTEGER               FRID
      INTEGER               I
      INTEGER               IAXES  ( 3 )
      INTEGER               J
      INTEGER               J2000
      INTEGER               N
      INTEGER               OBS
      INTEGER               TARG

      LOGICAL               CORBLK ( NABCOR )
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               FROZEN
      LOGICAL               INERT
      LOGICAL               MEANEC
      LOGICAL               MEANEQ
      LOGICAL               NEGATE
      LOGICAL               OFDATE
      LOGICAL               TRUEEQ

C
C     Saved variables
C
      SAVE                  AXES
      SAVE                  EARTH
      SAVE                  FIRST
      SAVE                  J2000
      SAVE                  ITMABC
      SAVE                  ITMAXE
      SAVE                  ITMCOF
      SAVE                  ITMDEC
      SAVE                  ITMFRM
      SAVE                  ITMLAT
      SAVE                  ITMLON
      SAVE                  ITMOBS
      SAVE                  ITMRA
      SAVE                  ITMSEP
      SAVE                  ITMSPC
      SAVE                  ITMTRG
      SAVE                  ITMUNT
      SAVE                  ITMVDF
      SAVE                  ITMVEC
      SAVE                  VNAME

C
C     Initial values
C
      DATA                  AXES   / KVX,    KVY,     KVZ     /
      DATA                  FIRST  / .TRUE.                   /
      DATA                  ITMCOF / KWEAC1, KWEAC2,  KWEAC3  /
      DATA                  ITMSEP / KWATOL                   /
      DATA                  VNAME  / KWPRI,           KWSEC   /


 
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )


      IF ( FIRST ) THEN
C
C        Get the ID code for the J2000 frame.
C
         CALL IRFNUM ( 'J2000', J2000 )

C
C        Get the ID code for the earth (we needn't check the found 
C        flag).
C
         CALL BODN2C ( 'EARTH', EARTH, FND )

C
C        Initialize "item" strings used to create kernel variable
C        names.
C
         DO I = 1, 2
C
C           Vector axis:
C
            ITMAXE(I) = VNAME(I) // KWVAXI
C
C           Vector definition:
C
            ITMVDF(I) = VNAME(I) // KWVCDF
C
C           Vector aberration correction:
C
            ITMABC(I) = VNAME(I) // KWVABC
C
C           Vector frame:
C
            ITMFRM(I) = VNAME(I) // KWVFRM
C
C           Vector observer:
C
            ITMOBS(I) = VNAME(I) // KWVOBS
C
C           Vector target:
C
            ITMTRG(I) = VNAME(I) // KWVTRG
C
C           Vector longitude:
C
            ITMLON(I) = VNAME(I) // KWLON
C
C           Vector latitude:
C
            ITMLAT(I) = VNAME(I) // KWLAT
C
C           Vector right ascension:
C
            ITMRA(I)  = VNAME(I) // KWRA
C
C           Vector declination:
C
            ITMDEC(I) = VNAME(I) // KWDEC
C
C           Vector units:
C
            ITMUNT(I) = VNAME(I) // KWUNIT
C
C           Constant vector coordinate specification:
C
            ITMSPC(I) = VNAME(I) // KWVSPC
C
C           Constant vector in cartesian coordinates, literal value:
C
            ITMVEC(I) = VNAME(I) // KWVECT

         END DO


         FIRST = .FALSE.

      END IF

      
C
C     Initialize the output arguments.
C     
      CALL CLEARD ( 36, XFORM )

      BASFRM = 0

C
C     Initialize certain variables to ensure that we don't do
C     arithmetic operations using bogus, possibly large,
C     undefined values.
C
      CALL CLEARD ( 36, NUTXF  )
      CALL CLEARD ( 36, OBLXF  )
      CALL CLEARD ( 36, PRECXF )
      CALL CLEARD ( 36, XF2000 )
      CALL CLEARD ( 36, XFINV  )
      CALL CLEARD ( 36, XIPM   )

      MOB    = 0.D0
      DMOB   = 0.D0
      T0     = 0.D0
      FROZEN = .FALSE.

C
C     Get the input frame name.
C
      CALL FRMNAM ( INFRAM, INNAME )

C
C     We need the name of the base frame.
C
      CALL ZZDYNFID ( INNAME, INFRAM, KWBFRM, BASFRM )
      CALL FRMNAM   ( BASFRM, BASNAM )

C
C     The output frame code and name are set.
C
C     Look up the dynamic frame definition style from the kernel pool.  
C     The kernel variable's name might be specified by name or ID.
C
      CALL ZZDYNVAC ( INNAME, INFRAM, KWSTYL, 1, N, DYNSTL )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     At this time, the only supported dynamic frame definition style is
C     PARAMETERIZED.
C
      IF (  EQSTR( DYNSTL, KVPARM )  ) THEN
C
C        Parameterized dynamic frames belong to families.  Look up
C        the family for this frame.
C 
         CALL ZZDYNVAC ( INNAME, INFRAM, KWFFAM, 1, N, DYNFAM )

         CALL CMPRSS ( ' ', 0, DYNFAM, TMPFAM )
         CALL UCASE  ( TMPFAM,         DYNFAM )

C
C        Determine whether we have an "of-date" frame family.
C        The logical flags used here and respective meanings are:
C
C           MEANEQ   Mean equator and equinox of date
C           TRUEEQ   True equator and equinox of date
C           MEANEC   Mean ecliptic and equinox of date
C
         MEANEQ  =  DYNFAM .EQ. KVMEQT
         TRUEEQ  =  DYNFAM .EQ. KVTEQT
         MEANEC  =  DYNFAM .EQ. KVMECL

         OFDATE  =  MEANEQ .OR. MEANEC .OR. TRUEEQ

C
C        Set the evaluation epoch T0.  Normally this epoch is ET,
C        but if the frame is frozen, the freeze epoch from the
C        frame definition is used.
C 
C        Read the freeze epoch into T0 if a freeze epoch was 
C        specified; let FROZEN receive the FOUND flag value 
C        returned by ZZDYNOAD.
C
         CALL ZZDYNOAD ( INNAME, INFRAM, KWFREZ, 1, N, T0, FROZEN )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
            
         IF ( .NOT. FROZEN ) THEN
C
C           Normal case:  just use the input epoch.
C
            T0 = ET

         END IF

C
C        Look up the rotation state keyword.  Rather than checking
C        FAILED() after every call, we'll do it after we're
C        done with processing the rotation state.
C         
         CALL ZZDYNOAC ( INNAME, INFRAM, KWRSTA, 1, N, ROTSTA, FND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        If the frame is frozen, the rotation state keyword *must be
C        absent*.
C
         IF ( FROZEN .AND. FND ) THEN

            CALL SETMSG ( 'Definition of frame # contains both ' //
     .                    '# and # '                             //
     .                    'keywords; at most one of these must ' //
     .                    'be present in the frame definition. ' //
     .                    'This situation is usually caused by ' //
     .                    'an error in a frame kernel in which ' //
     .                    'the frame is defined.'                )
            CALL ERRCH  ( '#',  INNAME                           )
            CALL ERRCH  ( '#',  KWFREZ                           )
            CALL ERRCH  ( '#',  KWRSTA                           )
            CALL SIGERR ( 'SPICE(FRAMEDEFERROR)'                 )
            CALL CHKOUT ( RNAME                                  )
            RETURN

         END IF

C
C        If the frame belongs to an "of date" family, either the
C        rotation state must be specified or the frame must be
C        frozen.
C
         IF (  OFDATE  .AND.  (.NOT. FROZEN) .AND. (.NOT. FND )  ) THEN

            CALL SETMSG ( 'Definition of frame #, which belongs to ' //
     .                    'parameterized dynamic frame family #, '   //
     .                    'contains neither # nor # keywords; '      //
     .                    'frames in this family require exactly '   //
     .                    'one of these in their frame definitions. '//
     .                    'This situation is usually caused by '     //
     .                    'an error in a frame kernel in which '     //
     .                    'the frame is defined.'                    )
            CALL ERRCH  ( '#',  INNAME                               )
            CALL ERRCH  ( '#',  DYNFAM                               )
            CALL ERRCH  ( '#',  KWFREZ                               )
            CALL ERRCH  ( '#',  KWRSTA                               )
            CALL SIGERR ( 'SPICE(FRAMEDEFERROR)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF

C
C        Set the rotation state logical flag indicating whether
C        the state is 'INERTIAL'.
C
         IF ( FND ) THEN
C
C           A rotation state keyword was found.
C
C           We know the state is not frozen if we arrive here.
C
            INERT  =  EQSTR ( ROTSTA, KVINRT )

            IF ( .NOT. INERT ) THEN
C
C              Catch invalid rotation states here.
C
               IF (  .NOT.  EQSTR ( ROTSTA, KVROTG )  ) THEN

                  CALL SETMSG ( 'Definition of frame # contains '     //
     .                          '# specification #. The only valid '  //
     .                          'rotation states are # or #. '        //
     .                          'This situation is usually caused by '//
     .                          'an error in a frame kernel in which '//
     .                          'the frame is defined.'               )
                  CALL ERRCH  ( '#',  INNAME                          )
                  CALL ERRCH  ( '#',  KWRSTA                          )
                  CALL ERRCH  ( '#',  ROTSTA                          )
                  CALL ERRCH  ( '#',  KVROTG                          )
                  CALL ERRCH  ( '#',  KVINRT                          )
                  CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                 )
                  CALL CHKOUT ( RNAME                                 )
                  RETURN

               END IF

            END IF

         ELSE
C
C           The state is not inertial unless there's a ROTATION_STATE
C           keyword assignment telling us it is.
C
            INERT  = .FALSE.

         END IF

C
C        INERT and FROZEN are both set. The evaluation epoch T0 is also
C        set.
C
C        The following code block performs actions specific to
C        the various dynamic frame families.
C
         IF ( OFDATE ) THEN
C
C           Fetch the name of the true equator and equinox of date 
C           precession model.
C
            CALL ZZDYNVAC ( INNAME, INFRAM, KWPRCM, 1, N, PRCMOD )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

C
C           Get the precession transformation.
C
            IF (  EQSTR( PRCMOD, KVM001 )  ) THEN
C
C              This is the 1976 IAU earth precession model.
C
C              Make sure the center of the input frame is the earth.
C
               IF ( CENTER .NE. EARTH ) THEN

                  CALL BODC2N ( CENTER, CTRNAM, FND )

                  IF ( .NOT. FND ) THEN
                     CALL INTSTR ( CENTER, CTRNAM )
                  END IF

                  CALL SETMSG ( 'Definition of frame # specifies '    //
     .                          'frame center # and precession '      //
     .                          'model #. This precession model '     //
     .                          'is not applicable to body #. '       //
     .                          'This situation is usually caused by '//
     .                          'an error in a frame kernel in which '//
     .                          'the frame is defined.'               )
                  CALL ERRCH  ( '#',  INNAME                          )
                  CALL ERRCH  ( '#',  CTRNAM                          )
                  CALL ERRCH  ( '#',  KVM001                          )
                  CALL ERRCH  ( '#',  CTRNAM                          )
                  CALL SIGERR ( 'SPICE(INVALIDSELECTION)'             )
                  CALL CHKOUT ( RNAME                                 )
                  RETURN

               END IF
C
C              Look up the precession transformation.
C
               CALL ZZEPRC76 ( T0, PRECXF )           

C
C              If we're in the mean-of-date case, invert this
C              transformation to obtain the mapping from the
C              mean-of-date frame to J2000.
C   
               IF ( MEANEQ ) THEN
                  CALL INVSTM ( PRECXF, XFTEMP )
               END IF

            ELSE

               CALL SETMSG ( 'Definition of frame # specifies '    //
     .                       'precession model #, which is not '   //
     .                       'recognized. '                        //
     .                       'This situation is usually caused by '//
     .                       'an error in a frame kernel in which '//
     .                       'the frame is defined.'               )
               CALL ERRCH  ( '#',  INNAME                          )
               CALL ERRCH  ( '#',  PRCMOD                          )
               CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                 )
               CALL CHKOUT ( RNAME                                 )
               RETURN

            END IF

C
C           At this point the precession transformation PRECXF is set.
C           If INFRAM is a mean equator and equinox of date frame, the
C           inverse of PRECXF is currently stored in XFTEMP.

            IF ( TRUEEQ ) THEN
C
C              We need a nutation transformation as well. Get the name
C              of the nutation model.
C             
               CALL ZZDYNVAC ( INNAME, INFRAM, KWNUTM, 1, N, NUTMOD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF
 
C
C              Get the nutation transformation.
C  
               IF (  EQSTR( NUTMOD, KVM002 )  ) THEN
C
C                 This is the 1980 IAU earth nutation model.
C
C                 Make sure the center is the earth.
C
                  IF ( CENTER .NE. EARTH ) THEN

                     CALL BODC2N ( CENTER, CTRNAM, FND )

                     IF ( .NOT. FND ) THEN
                        CALL INTSTR ( CENTER, CTRNAM )
                     END IF

                     CALL SETMSG ( 'Definition of frame # specifies ' //
     .                             'frame center # and nutation '     //
     .                             'model #. This '                   //
     .                             'nutation model is not applicable '//
     .                             'to body #.  This situation is '   //
     .                             'usually caused by an error in a ' //
     .                             'frame kernel in which '           //
     .                             'the frame is defined.'            )
                     CALL ERRCH  ( '#',  INNAME                       )
                     CALL ERRCH  ( '#',  CTRNAM                       )
                     CALL ERRCH  ( '#',  KVM002                       )
                     CALL ERRCH  ( '#',  CTRNAM                       )
                     CALL SIGERR ( 'SPICE(INVALIDSELECTION)'          )
                     CALL CHKOUT ( RNAME                              )
                     RETURN

                  END IF

C
C                 Look up the nutation transformation.
C
                  CALL ZZENUT80 ( T0, NUTXF )

C
C                 Find the transformation from the J2000 frame to the
C                 earth true of date frame.  Invert.
C                 
                  CALL MXMG   ( NUTXF, PRECXF, 6, 6, 6, XFINV )
                  CALL INVSTM ( XFINV, XFTEMP )
                  
               ELSE

                  CALL SETMSG ( 'Definition of frame # specifies '    //
     .                          'nutation model #, which is not '     //
     .                          'recognized. '                        //
     .                          'This situation is usually caused by '//
     .                          'an error in a frame kernel in which '//
     .                          'the frame is defined.'               )
                  CALL ERRCH  ( '#',  INNAME                          )
                  CALL ERRCH  ( '#',  NUTMOD                          )
                  CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                 )
                  CALL CHKOUT ( RNAME                                 )
                  RETURN

               END IF


            ELSE IF ( MEANEC ) THEN
C
C              We need a mean obliquity transformation as well.
C              Get the name of the obliquity model.
C             
               CALL ZZDYNVAC ( INNAME, INFRAM, KWOBQM, 1, N, OBLMOD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

C
C              Get the obliquity transformation.
C  
               IF (  EQSTR( OBLMOD, KVM003 )  ) THEN
C
C                 This is the 1980 IAU earth mean obliquity of
C                 date model.
C
C                 Make sure the center is the earth.
C
                  IF ( CENTER .NE. EARTH ) THEN

                     CALL BODC2N ( CENTER, CTRNAM, FND )

                     IF ( .NOT. FND ) THEN
                        CALL INTSTR ( CENTER, CTRNAM )
                     END IF

                     CALL SETMSG ( 'Definition of frame # specifies ' //
     .                             'frame center # and obliquity '    //
     .                             'model #.  This obliquity model '  //
     .                             'is not applicable to body #. '    //
     .                             'This situation is '               //
     .                             'usually caused by an error in a ' //
     .                             'frame kernel in which '           //
     .                             'the frame is defined.'            )
                     CALL ERRCH  ( '#',  INNAME                       )
                     CALL ERRCH  ( '#',  CTRNAM                       )
                     CALL ERRCH  ( '#',  KVM003                       )
                     CALL ERRCH  ( '#',  CTRNAM                       )
                     CALL SIGERR ( 'SPICE(INVALIDSELECTION)'          )
                     CALL CHKOUT ( RNAME                              )
                     RETURN

                  END IF

C
C                 Create the obliquity transformation. First look up
C                 the obliquity state (angle and angular rate).
C
                  CALL ZZMOBLIQ ( T0, MOB, DMOB )

C
C                 The obliquity rotation is about the mean-of-date
C                 x-axis.  The other Euler angles are identically 
C                 zero; the axes are arbitrary, as long as the 
C                 middle axis is distinct from the other two.
C
                  CALL CLEARD ( 6, EULANG )
                  EULANG(3) = MOB
                  EULANG(6) = DMOB

                  CALL EUL2XF ( EULANG, 1, 3, 1, OBLXF )

C
C                 Find the transformation from the J2000 to the
C                 earth mean ecliptic of date frame.  Invert.
C                 
                  CALL MXMG   ( OBLXF, PRECXF, 6, 6, 6, XFINV )
                  CALL INVSTM ( XFINV, XFTEMP )
                  
               ELSE

                  CALL SETMSG ( 'Definition of frame # specifies '    //
     .                          'obliquity model #, which is not '    //
     .                          'recognized. '                        //
     .                          'This situation is usually caused by '//
     .                          'an error in a frame kernel in which '//
     .                          'the frame is defined.'               )
                  CALL ERRCH  ( '#',  INNAME                          )
                  CALL ERRCH  ( '#',  OBLMOD                          )
                  CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                 )
                  CALL CHKOUT ( RNAME                                 )
                  RETURN

               END IF

            END IF

C
C           At this point, XFTEMP contains the mapping from the
C           specified mean of date or true of date frame to J2000.
C
C           If the base frame is not J2000, we must find the
C           transformation from J2000 to the base frame.
C
            IF ( BASFRM .NE. J2000 ) THEN

               CALL ZZFRMCH0 ( J2000,  BASFRM, T0,      XF2000 )
               CALL MXMG     ( XF2000, XFTEMP, 6, 6, 6, XFORM  )

            ELSE
C
C              Otherwise, XFTEMP is the matrix we want.
C
               CALL MOVED ( XFTEMP, 36, XFORM )

            END IF

C
C           Now XFORM is the state transformation mapping from
C           the input frame INFRAM to the base frame BASFRM.
C
C           This is the end of the work specific to "of-date" frames.
C           From here we drop out of the IF block.  At the end of this
C           routine, the derivative block of XFORM will be zeroed out
C           if the frame is frozen.  If the rotation state is
C           'INERTIAL', we will make sure the transformation between
C           the defined frame and the J2000 frame has time derivative
C           zero.
C

         ELSE IF ( DYNFAM .EQ. KV2VEC ) THEN
C
C           The frame belongs to the TWO-VECTOR family.
C
C           Initialize the array S2.
C
            CALL CLEARD ( 12, S2 )

C
C           Fetch the specifications of the primary and secondary
C           axes.  
C
            DO I = 1, 2
C
C              Get the name of the axis associated with the Ith
C              defining vector.
C
               CALL ZZDYNVAC ( INNAME, INFRAM, ITMAXE(I), 1, N, AXNAME )

               CALL CMPRSS ( ' ', 0, AXNAME, AXNAME )
               CALL UCASE  ( AXNAME,         AXNAME )

C
C              Set the sign flag associated with the axis.
C
               NEGATE  =  AXNAME(1:1) .EQ. '-'

               CALL CMPRSS ( '-', 0, AXNAME, AXNAME )
               CALL CMPRSS ( '+', 0, AXNAME, AXNAME )

               AXIS(I) = ISRCHC ( AXNAME, 3, AXES )


               IF ( AXIS(I) .EQ. 0 ) THEN

                  CALL SETMSG ( 'Definition of frame # associates '   //
     .                          'vector # with axis #.  The only '    //
     .                          'valid axis values are { X, -X, Y, '  //
     .                          '-Y, Z, -Z }. '                       //
     .                          'This situation is usually caused by '//
     .                          'an error in a frame kernel in which '//
     .                          'the frame is defined.'               )
                  CALL ERRCH  ( '#',  INNAME                          )
                  CALL ERRINT ( '#',  I                               )
                  CALL ERRCH  ( '#',  AXNAME                          )
                  CALL SIGERR ( 'SPICE(INVALIDAXIS)'                  )
                  CALL CHKOUT ( RNAME                                 )
                  RETURN

               END IF               

C
C              Find out how the vector is defined:
C
C                 - Observer-target position vector
C                 - Observer-target velocity vector
C                 - Observer-target near point vector
C                 - Constant vector
C
C              VECDEF(I) indicates the vector definition method
C              for the Ith vector.
C              
               CALL ZZDYNVAC ( INNAME,  INFRAM,    ITMVDF(I),   
     .                         1,       N,         VECDEF(I) )

               CALL CMPRSS ( ' ',    0, VECDEF(I), VECDEF(I) )
               CALL UCASE  ( VECDEF(I),            VECDEF(I) )


               IF (  VECDEF(I) .EQ. KVPOSV  ) THEN
C
C                 The vector is the position of a target relative
C                 to an observer.
C
C                 We need a target, observer, and aberration correction.
C
                  CALL ZZDYNBID ( INNAME, INFRAM, ITMTRG(I), TARG )

                  CALL ZZDYNBID ( INNAME, INFRAM, ITMOBS(I), OBS  )

                  CALL ZZDYNVAC ( INNAME, INFRAM, ITMABC(I),    
     .                            1,      N,      ABCORR     )

C
C                 Look up the Ith state vector in the J2000 frame.
C
                  CALL ZZSPKEZ0 ( TARG,    T0,   'J2000',  
     .                            ABCORR,  OBS,  S2(1,I),  LT )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

C
C                 At this point, S2(*,I) contains position and
C                 velocity relative to frame J2000.
C                                 

               ELSE IF ( VECDEF(I) .EQ. KVVELV ) THEN
C
C                 The vector is the velocity of a target relative
C                 to an observer.
C
C                 We need a target, observer, and aberration correction.
C 
                  CALL ZZDYNBID ( INNAME, INFRAM, ITMTRG(I), TARG )

                  CALL ZZDYNBID ( INNAME, INFRAM, ITMOBS(I), OBS  )

                  CALL ZZDYNVAC ( INNAME, INFRAM, ITMABC(I),    
     .                            1,      N,      ABCORR     )

C
C                 We need to know the frame in which the velocity is
C                 defined.
C
                  CALL ZZDYNFID ( INNAME, INFRAM, ITMFRM(I), FRID )
                  CALL FRMNAM   ( FRID,   VELFRM  )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

C
C                 Obtain the velocity vector in the specified velocity
C                 frame.  Also obtain bracketing vectors to support
C                 discrete differentiation.  (See notes in zzdyn.inc
C                 regarding definition of DELTA.)
C
                  DELTA = MAX ( 1.D0,  (2.D0**QEXP) * T0 )

                  CALL VPACK ( T0-DELTA, T0, T0+DELTA, TARRAY )


                  DO J = 1, 3

                     CALL ZZSPKEZ0 ( TARG,   TARRAY(J), VELFRM, 
     .                               ABCORR, OBS,       STEMP,  LT )
C
C                    We compute the derivative using unit
C                    velocity vectors.
C
                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( RNAME )
                        RETURN
                     END IF

                     CALL VHAT ( STEMP(4), VARRAY(1,J) )

                  END DO
               
C
C                 Compute acceleration and fill in the velocity state 
C                 vector S2(*,I).
C
                  CALL QDERIV ( 3, 
     .                          VARRAY(1,1),  VARRAY(1,3),  DELTA, ACC )

                  CALL VEQU   ( VARRAY(1,2),  S2(1,I) )
                  CALL VEQU   ( ACC,          S2(4,I) )

C
C                 We need the epoch VET at which VELFRM is evaluated.
C                 This epoch will be used to transform the velocity's
C                 "state" vector from VELFRM to J2000.  
C
C                 Set the default value of VET here.
C
                  VET = T0

C
C                 Parse the aberration correction.  Find the epoch used
C                 to evaluate the velocity vector's frame.
C
                  CALL ZZPRSCOR ( ABCORR, CORBLK )

                  IF ( CORBLK(LTIDX) ) THEN
C
C                    Light time correction is used.  The epoch used
C                    to evaluate the velocity vector's frame depends
C                    on the frame's observer and center. 
C
C                    Look up the velocity frame's center.
C                    
                     CALL FRINFO ( FRID, FRCTR, FRCLS, FRCID, FND ) 

                     IF ( .NOT. FND ) THEN

                        CALL SETMSG ( 'In definition of frame #, ' //
     .                                'the frame associated with ' //
     .                                'a velocity vector has '     //
     .                                'frame ID code #, but no '   //
     .                                'frame center, frame class, '//
     .                                'or frame class ID was '     //
     .                                'found by FRINFO.  This '    //
     .                                'situation MAY be caused '   //
     .                                'by an error in a frame '    //
     .                                'kernel in which the frame ' //
     .                                'is defined. The problem '   //
     .                                'also could be indicative '  //
     .                                'of a SPICELIB bug.'         )
                        CALL ERRCH  ( '#',  INNAME                 )
                        CALL ERRINT ( '#',  FRID                   )
                        CALL SIGERR ( 'SPICE(FRAMEDATANOTFOUND)'   ) 
                        CALL CHKOUT ( RNAME                        )
                        RETURN

                     END IF

C
C                    If the velocity frame is non-inertial, we'll need
C                    to compute the evaluation epoch for this frame.
C
                     IF ( FRCLS .NE. INERTL ) THEN
C
C                       Obtain light time from the observer to the
C                       frame's center; find the evaluation epoch VET
C                       for the frame.

                        CALL ZZSPKZP0 ( FRCTR,  T0,  'J2000', 
     .                                  ABCORR, OBS, CTRPOS,  VFLT )
                        CALL ZZCOREPC ( ABCORR, T0,  VFLT,    VET  )

                        IF ( FAILED() ) THEN
                           CALL CHKOUT ( RNAME )
                           RETURN
                        END IF

                     END IF

                  END IF

C
C                 The velocity frame's evaluation epoch VET is now set.
C
C                 We must rotate the velocity vector and transform the
C                 acceleration from the velocity frame (evaluated at
C                 VET) to the output frame at T0.  We'll do this in two
C                 stages, first mapping velocity and acceleration into
C                 the J2000 frame.
C 
                  IF ( FRID .NE. J2000 ) THEN

                     CALL ZZFRMCH0 ( FRID,    J2000,  VET,   XF2000  )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( RNAME )
                        RETURN
                     END IF

                     CALL MXVG     ( XF2000,  S2(1,I), 6, 6, STEMP   )
                     CALL MOVED    ( STEMP,   6,             S2(1,I) )

                  END IF


C
C                 At this point, S2(*,I) contains velocity and
C                 acceleration relative to frame J2000.
C                                 

               ELSE IF ( VECDEF(I) .EQ. KVNEAR ) THEN
C
C                 The vector points from an observer to the
C                 sub-observer point (nearest point to the observer) on
C                 the target body.
C
C                 We need a target, observer, and aberration correction.
C
                  CALL ZZDYNBID ( INNAME, INFRAM, ITMTRG(I), TARG )

                  CALL ZZDYNBID ( INNAME, INFRAM, ITMOBS(I), OBS  )

                  CALL ZZDYNVAC ( INNAME, INFRAM, ITMABC(I),    
     .                            1,      N,      ABCORR     )

C
C                 The vector points from the observer to the nearest
C                 point on the target. We need the state of the near
C                 point relative to the observer.
C
C                 We'll look up the state of the target center relative
C                 to the observer and the state of the near point
C                 relative to the target center, both in the body-fixed
C                 frame associated with the target.
C       
C                 Look up the body-fixed frame associated with the
C                 target body.
C
                  CALL CIDFRM ( TARG, CFRMID, CFRMNM, FND )

                  IF ( .NOT. FND ) THEN

                     CALL SETMSG ( 'Definition of frame # requires ' //
     .                             'definition of body-fixed frame ' //
     .                             'associated with target body #. ' //
     .                             'A call to CIDFRM indicated no '  //
     .                             'body-fixed frame is associated ' //
     .                             'with the target body.  This '    //
     .                             'situation can arise when a '     //
     .                             'frame kernel defining the '      //
     .                             'target''s body-fixed frame  '    //
     .                             'lacks the OBJECT_<ID>_FRAME '    //
     .                             'or OBJECT_<name>_FRAME '         //
     .                             'keywords.  The problem also '    //
     .                             'could be caused by an '          //
     .                             'error in a frame kernel in '     //
     .                             'which the parameterized '        //
     .                             'two-vector dynamic frame # '     //
     .                             'is defined.'                     )
                     CALL ERRCH  ( '#',  INNAME                      )
                     CALL ERRINT ( '#',  TARG                        )
                     CALL ERRCH  ( '#',  INNAME                      )
                     CALL SIGERR ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     CALL CHKOUT ( RNAME                             )
                     RETURN

                  END IF

C
C                 Get the radii of the target body.
C
                  CALL BODVCD ( TARG, 'RADII', 3, N, RADII )

C
C                 Look up the Ith state vector in the target-fixed
C                 frame.  Negate the vector to obtain the target-to-
C                 observer vector.
C
                  CALL ZZSPKEZ0 ( TARG,    T0,       CFRMNM,  
     .                            ABCORR,  OBS,      STEMP,    LT )

C
C                 We check FAILED() here because VMINUG is a simple
C                 arithmetic routine that doesn't return on entry.
C
                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL VMINUG   ( STEMP,   6,        STOBS )

                  CALL DNEARP   ( STOBS,   RADII(1), RADII(2), RADII(3),
     .                            STNEAR,  STALT,    FND               )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  IF ( .NOT. FND ) THEN                     

                     CALL SETMSG ( 'In definition of frame #, '      //
     .                             'vector # is defined by the '     //
     .                             'near point on body # as seen '   //
     .                             'from body #.  The state of this '//
     .                             'near point was not found. '      //
     .                             'See the routine DNEARP for '     //
     .                             'an explanation.  This '          //
     .                             'situation MAY be caused '        //
     .                             'by an error in a frame '         //
     .                             'kernel in which the frame '      //
     .                             'is defined. The problem '        //
     .                             'also could be indicative '       //
     .                             'of a SPICELIB bug.'              )
                     CALL ERRCH  ( '#',  INNAME                      )
                     CALL ERRINT ( '#',  I                           )
                     CALL ERRINT ( '#',  TARG                        )
                     CALL ERRINT ( '#',  OBS                         )
                     CALL SIGERR ( 'SPICE(DEGENERATECASE)'           ) 
                     CALL CHKOUT ( RNAME                             )
                     RETURN

                  END IF


C
C                 Find the observer-near point state in the target
C                 body-fixed frame.
C
                  CALL VSUBG ( STNEAR, STOBS, 6, STEMP  )

C
C                 Transform the state to frame J2000.  To get the
C                 required transformation matrix, we'll need to obtain
C                 the epoch associated with CNMFRM.  Parse the
C                 aberration correction and adjust the frame evaluation
C                 epoch as needed.
C
                  CALL ZZCOREPC ( ABCORR, T0, LT, FET )
                       
C
C                 Obtain the matrix for transforming state vectors
C                 from the target center frame to the J2000 frame and
C                 apply it to the observer-to-near point state vector.
C                  
                  CALL ZZFRMCH0 ( CFRMID,  J2000,  FET,   XIPM    )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL MXVG     ( XIPM,    STEMP,  6, 6,  S2(1,I) )

C
C                 At this point, S2(*,I) contains position and
C                 velocity of the near point on the target as 
C                 seen by the observer, relative to frame J2000.
C                                 

               ELSE IF ( VECDEF(I) .EQ. KVCONS ) THEN
C
C                 The vector is constant in a specified frame.
C
C
C                 We need a 3-vector and an associated reference
C                 frame relative to which the vector is specified.
C
C                 Look up the ID of the frame first.
C
                  CALL ZZDYNFID ( INNAME, INFRAM, ITMFRM(I), FRID )

C
C                 Let FET ("frame ET") be the evaluation epoch for
C                 the constant vector's frame.  By default, this
C                 frame is just T0, but if we're using light time
C                 corrections, FET must be adjusted for one-way
C                 light time between the frame's center and the
C                 observer. 
C                 
C                 Set the default value of FET here.
C
                  FET = T0

C
C                 Optionally, there is an aberration correction
C                 associated with the constant vector's frame.
C                 If so, an observer must be associated with the
C                 frame.  Look up the correction first.
C
                  CALL ZZDYNOAC ( INNAME, INFRAM, ITMABC(I),     
     .                            1,      N,      CVCORR,    FND )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  IF ( .NOT. FND ) THEN
                     CVCORR = 'NONE'
                  END IF

                  CALL ZZPRSCOR ( CVCORR, CORBLK )


                  IF ( .NOT. CORBLK(GEOIDX) ) THEN
C
C                    We need to apply an aberration correction to
C                    the constant vector.
C
C                    Check for errors in the aberration correction
C                    specification.
C
C                       - Light time and stellar aberration corrections
C                         are mutually exclusive.
C    
                     IF ( CORBLK(LTIDX) .AND. CORBLK(STLIDX) ) THEN

                        CALL SETMSG ( 'Definition of frame # '       //
     .                                'specifies aberration '        //
     .                                'correction # for constant '   //
     .                                'vector.  Light time and '     //
     .                                'stellar aberration '          //
     .                                'corrections are mutually '    //
     .                                'exclusive for constant '      //
     .                                'vectors used in two-vector '  //
     .                                'parameterized dynamic frame ' //
     .                                'definitions.  This situation '//
     .                                'is usually caused by an '     //
     .                                'error in a frame kernel in '  //
     .                                'which the frame is defined.'  )
                        CALL ERRCH  ( '#',  INNAME                   )
                        CALL ERRCH  ( '#',  CVCORR                   )
                        CALL SIGERR ( 'SPICE(INVALIDOPTION)'         ) 
                        CALL CHKOUT ( RNAME                          )
                        RETURN

                     END IF

                     IF ( CORBLK(LTIDX) ) THEN
C
C                       Light time correction is used.  The epoch used
C                       to evaluate the constant vector's frame depends
C                       on the frame's observer and center.
C
C                       Look up the constant vector frame's center.
C
                        CALL FRINFO ( FRID, FRCTR, FRCLS, FRCID, FND ) 

                        IF ( .NOT. FND ) THEN

                           CALL SETMSG ( 'In definition of frame #, ' //
     .                                   'the frame associated with ' //
     .                                   'a constant vector has '     //
     .                                   'frame ID code #, but no '   //
     .                                   'frame center, frame class, '//
     .                                   'or frame class ID was '     //
     .                                   'found by FRINFO.  This '    //
     .                                   'situation MAY be caused '   //
     .                                   'by an error in a frame '    //
     .                                   'kernel in which the frame ' //
     .                                   'is defined. The problem '   //
     .                                   'also could be indicative '  //
     .                                   'of a SPICELIB bug.'         )
                           CALL ERRCH  ( '#',  INNAME                 )
                           CALL ERRINT ( '#',  FRID                   )
                           CALL SIGERR ( 'SPICE(FRAMEDATANOTFOUND)'   ) 
                           CALL CHKOUT ( RNAME                        )
                           RETURN

                        END IF
C
C                       If the constant vector frame is non-inertial,
C                       we'll need to compute the evaluation epoch for
C                       this frame.
C
                        IF ( FRCLS .NE. INERTL ) THEN
C
C                          Look up the observer associated with the
C                          constant vector's frame.  This observer,
C                          together with the frame's center, determines
C                          the evaluation epoch for the frame.
C
                           CALL ZZDYNBID ( INNAME,    INFRAM, 
     .                                     ITMOBS(I), CVOBS   )

C
C                          Obtain light time from the observer to the
C                          frame's center.
C
                           CALL ZZSPKZP0 ( FRCTR,  T0,   'J2000', 
     .                                     CVCORR, CVOBS, CTRPOS,  LT )

C
C                          Re-set the evaluation epoch for the frame.
C
                           CALL ZZCOREPC ( CVCORR, T0, LT, FET )

                        END IF
C
C                       The constant vector frame's evaluation epoch
C                       FET has been set.
C

                     ELSE IF ( CORBLK(STLIDX) ) THEN
C
C                       Stellar aberration case.
C
C                       The constant vector must be corrected for
C                       stellar aberration induced by the observer's
C                       velocity relative to the solar system 
C                       barycenter.  First, find this velocity in
C                       the J2000 frame.  We'll apply the correction
C                       later, when the constant vector has been
C                       transformed to the J2000 frame.
C
                        CALL ZZDYNBID ( INNAME,    INFRAM, 
     .                                  ITMOBS(I), CVOBS   )
                        
                        CALL ZZSPKSB0 ( CVOBS, T0, 'J2000', STOBS )

                     END IF

                  END IF

C
C                 At this point FET is the frame evaluation epoch
C                 for the frame associated with the constant vector.
C
C                 If stellar aberration correction has been specified,
C                 STOBS is the state of the observer relative to the
C                 solar system barycenter, expressed in the J2000
C                 frame.
C
C                 Get the constant vector specification.
C
                  CALL ZZDYNVAC ( INNAME,  INFRAM,  ITMSPC(I),
     .                            1,       N,       SPEC       )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL CMPRSS ( ' ', 0, SPEC, SPEC )
                  CALL UCASE  ( SPEC,         SPEC )


                  IF ( SPEC .EQ. KVRECC ) THEN
C
C                    The coordinate system is rectangular.
C
C                    Look up the constant vector.
C
                     CALL ZZDYNVAD ( INNAME, INFRAM, ITMVEC(I),
     .                               3,      N,      DIRVEC    )


                  ELSE IF (      ( SPEC .EQ. KVLATC ) 
     .                      .OR. ( SPEC .EQ. KVRADC ) ) THEN
C
C                    The coordinate system is latitudinal or RA/DEC.
C
C                    Look up the units associated with the angles.
C  
                     CALL ZZDYNVAC ( INNAME,  INFRAM,  ITMUNT(I),
     .                               1,       N,       UNITS     )


                     IF ( SPEC .EQ. KVLATC ) THEN
C
C                       Look up longitude and latitude.
C
                        CALL ZZDYNVAD ( INNAME, INFRAM, ITMLON(I),  
     .                                  1,      N,      LON        )

                        CALL ZZDYNVAD ( INNAME, INFRAM, ITMLAT(I), 
     .                                   1,     N,      LAT        )

C
C                       Convert angles from input units to radians.
C
                        CALL CONVRT ( LON, UNITS, KVRADN, ANGLES(1) )
                        CALL CONVRT ( LAT, UNITS, KVRADN, ANGLES(2) )

                     ELSE
C
C                       Look up RA and DEC.  
C                       
                        CALL ZZDYNVAD ( INNAME, INFRAM, ITMRA(I),
     .                                  1,      N,      RA        )

                        CALL ZZDYNVAD ( INNAME, INFRAM, ITMDEC(I),
     .                                  1,      N,      DEC       )

C
C                       Convert angles from input units to radians.
C
                        CALL CONVRT ( RA,  UNITS, KVRADN, ANGLES(1) )
                        CALL CONVRT ( DEC, UNITS, KVRADN, ANGLES(2) )

                     END IF

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( RNAME )
                        RETURN
                     END IF

C
C                    Now  produce a direction vector.
C
                     CALL LATREC ( 1.D0, ANGLES(1), ANGLES(2), DIRVEC )


                  ELSE

                     CALL SETMSG ( 'Definition of two-vector '       //
     .                             'parameterized dynamic frame # '  //
     .                             'includes constant vector '       //
     .                             'specification #, which is '      //
     .                             'not supported.  This situation ' // 
     .                             'is usually caused by an error '  //
     .                             'in a frame kernel in which '     //
     .                             'the frame is defined.'           )
                     CALL ERRCH  ( '#',  INNAME                      )
                     CALL ERRCH  ( '#',  SPEC                        )
                     CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
                     CALL CHKOUT ( RNAME                             )
                     RETURN

                  END IF

C
C                 At this point, the cartesian coordinates of the
C                 vector relative to the constant vector frame
C                 are stored in DIRVEC.
C
C                 Convert the direction vector to the J2000 frame.
C                 Fill in the state vector.  The velocity in the
C                 constant vector's frame is zero.
C
                  CALL VEQU     ( DIRVEC, S2(1,I) )
                  CALL CLEARD   ( 3,      S2(4,I) )

                  IF ( FRID .NE. J2000 ) THEN

                     CALL ZZFRMCH0 ( FRID,   J2000,   FET,   XIPM    )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( RNAME )
                        RETURN
                     END IF

                     CALL MXVG     ( XIPM,   S2(1,I), 6, 6,  STEMP   )
                     CALL MOVED    ( STEMP,  6,              S2(1,I) )

                  END IF

C
C                 The state of the constant vector is now represented
C                 in the J2000 frame, but we may still need to
C                 apply a stellar aberration correction.
C
                  IF ( CORBLK(STLIDX) ) THEN
C
C                    Perform the stellar aberration correction
C                    appropriate to the radiation travel sense. 
                    
                     IF ( CORBLK(XMTIDX) ) THEN
C
C                       The correction is for transmission.
C
                        CALL STLABX ( S2(1,I), STOBS(4), STEMP ) 

                     ELSE
C
C                       The correction is for reception.
C
                        CALL STELAB ( S2(1,I), STOBS(4), STEMP ) 

                     END IF

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( RNAME )
                        RETURN
                     END IF

C
C                    Update the position portion of S2(*,I).
C
                     CALL VEQU ( STEMP, S2(1,I) )

                  END IF

C
C                 At this point, S2(*,I) contains position and velocity
C                 of the constant (constant relative to its associated
C                 frame, that is) vector as seen by the observer,
C                 relative to frame J2000.
C

               ELSE

                  CALL SETMSG ( 'Definition of two-vector '       //
     .                          'parameterized dynamic frame # '  //
     .                          'includes vector '                //
     .                          'definition #, which is '         //
     .                          'not supported.  This situation ' // 
     .                          'is usually caused by an error '  //
     .                          'in a frame kernel in which '     //
     .                          'the frame is defined.'           )
                  CALL ERRCH  ( '#',  INNAME                      )
                  CALL ERRCH  ( '#',  VECDEF(I)                   )
                  CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
                  CALL CHKOUT ( RNAME                             )
                  RETURN

               END IF

C
C              Negate the state vector if the axis has negative sign.
C
               IF ( NEGATE ) THEN
                  CALL VMINUG ( S2(1,I), 6, STEMP   )
                  CALL MOVED  ( STEMP,   6, S2(1,I) )
               END IF


            END DO

C
C           Look up the lower bound for the angular separation of
C           the defining vectors.  Use the default value if none
C           was supplied.
C
            CALL ZZDYNOAD ( INNAME, INFRAM, ITMSEP, 1, N, MINSEP, FND ) 

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
               MINSEP = LBSEP
            END IF 

C
C           Now use our states to compute our state transformation
C           matrix.
C
C           Check the angular separation of the defining vectors. We
C           want to ensure that the vectors are not too close to being
C           linearly dependent.  We can handle both cases---separation
C           close to 0 or separation close to Pi---by comparing the
C           sine of the separation to the sine of the separation limit.
C             
            SEP = VSEP ( S2(1,1), S2(1,2) )

            IF ( SIN(SEP) .LT. SIN(MINSEP) ) THEN

               CALL ETCAL  ( T0, TIMSTR )

               CALL SETMSG ( 'Angular separation of vectors defining '//
     .                       'two-vector parameterized dynamic frame '//
     .                       '# is # (radians); minimum allowed diff' //
     .                       'erence of separation from 0 or Pi is # '//
     .                       'radians.  Evaluation epoch is #.  Extre'//
     .                       'me loss of precision can occur when '   //
     .                       'defining vectors are nearly linearly '  //
     .                       'dependent.  This type of error can be ' //
     .                       'due to using a dynamic frame outside of'//
     .                       ' the time range for which it is meant. '//
     .                       'It also can be due to a conceptual '    //
     .                       'error pertaining to the frame''s defin' //
     .                       'ition, or to an implementation error '  //
     .                       'in the frame kernel containing the '    //
     .                       'frame definition. However, if you wish '//
     .                       'to proceed with this computation, '     //
     .                       'the # keyword can be used in '          //
     .                       'the frame definition to adjust the '    //
     .                       'separation limit.'                      )
               CALL ERRCH  ( '#',  INNAME                             )
               CALL ERRDP  ( '#',  SEP                                )
               CALL ERRDP  ( '#',  MINSEP                             )
               CALL ERRCH  ( '#',  TIMSTR                             )
               CALL ERRCH  ( '#',  KWATOL                             )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
               CALL CHKOUT ( RNAME                                    )
               RETURN

            END IF
 
C
C           We have both states expressed relative to frame J2000
C           at this point.  Find the transformation from INNAME to
C           the frame J2000, then from J2000 to frame BASNAM.
C
            CALL ZZTWOVXF ( S2(1,1), AXIS(1), S2(1,2), AXIS(2), XFORM )
      
            IF ( BASFRM .NE. J2000 ) THEN

               CALL MOVED ( XFORM, 36, XFTEMP )

               CALL ZZFRMCH0 ( J2000, BASFRM,  T0, XF2000 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               CALL MXMG ( XF2000,  XFTEMP,  6,  6,  6, XFORM  )

            END IF

C
C           This is the end of the work specific to two-vector frames.
C           From here we drop out of the IF block.  At the end of this
C           routine, the derivative block of XFORM will be zeroed out
C           if the frame is frozen.  If the rotation state is
C           'INERTIAL', we will make sure the transformation between
C           the defined frame and the J2000 frame has time derivative
C           zero.
C          

         ELSE IF ( DYNFAM .EQ. KVEULR ) THEN
C
C           The frame belongs to the Euler family.
C
C           We expect to specifications of an axis sequence, units,
C           and angles via polynomial coefficients.  We also expect
C           to see an ET epoch.
C
C           Look up the epoch first.  Let DELTA represent the offset
C           of T0 relative to the epoch.
C
C           Initialize EPOCH so subtraction doesn't overflow if EPOCH
C           is invalid due to a lookup error.
C
            EPOCH = 0.D0

            CALL ZZDYNVAD ( INNAME, INFRAM, KWEPOC, 1, N, EPOCH )

            DELTA = T0 - EPOCH

C
C           Now the axis sequence.
C
            CALL ZZDYNVAI ( INNAME, INFRAM, KWEUAX, 3, N, IAXES )

C
C           Now the coefficients for the angles.
C
            DO I = 1, 3
C
C              Initialize N so subtraction doesn't overflow if N
C              is invalid due to a lookup error.
C
               N = 0

               CALL ZZDYNVAD ( INNAME,  INFRAM,  ITMCOF(I),  
     .                         MAXCOF,  N,       COEFFS(1,I) )
         
C
C              Set the polynomial degree for the Ith angle.
C
               DEGS(I) = N - 1

            END DO

C
C           Look up the units associated with the angles.
C  
            CALL ZZDYNVAC ( INNAME, INFRAM, KWUNIT, 1, N, UNITS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

C
C           Evaluate the angles and their derivatives at DELTA.  Convert
C           angles from input units to radians and radians/sec.
C
            DO I = 1, 3

               CALL POLYDS ( COEFFS(1,I), DEGS(I), 1, DELTA, POLY )
C
C              Convert units.  Fill in the Euler angle state vector.
C
               CALL CONVRT ( POLY(0), UNITS, KVRADN, EULANG(I  ) )
               CALL CONVRT ( POLY(1), UNITS, KVRADN, EULANG(I+3) )

            END DO

C
C           Produce a state transformation matrix that maps from
C           the defined frame to the base frame.
C
            CALL EUL2XF ( EULANG, IAXES(1), IAXES(2), IAXES(3), XFORM )

C
C           This is the end of the work specific to Euler frames.
C           From here we drop out of the IF block.  At the end of this
C           routine, the derivative block of XFORM will be zeroed out
C           if the frame is frozen.  If the rotation state is
C           'INERTIAL', we will make sure the transformation between
C           the defined frame and the J2000 frame has time derivative
C           zero. 
C          

         ELSE

            CALL SETMSG ( 'Dynamic frame family # (in definition of '//
     .                    'frame #) is not supported. '              //
     .                    'This situation is usually caused by '     //
     .                    'an error in a frame kernel in which '     //
     .                    'the frame is defined.'                    )
            CALL ERRCH  ( '#',  DYNFAM                               )
            CALL ERRCH  ( '#',  INNAME                               )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF
C
C        This is the end of the IF block that processes the 
C        parameterized dynamic frame families.
C

      ELSE

         CALL SETMSG ( 'Dynamic frame style # (in definition of ' //
     .                 'frame #) is not supported. '              //
     .                 'This situation is usually caused by '     //
     .                 'an error in a frame kernel in which '     //
     .                 'the frame is defined.'                    )
         CALL ERRCH  ( '#',  DYNSTL                               )
         CALL ERRCH  ( '#',  INNAME                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     At this point XFORM is the state transformation matrix mapping
C     from the input frame INFRAM to the base frame BASFRM.
C
C     If the frame has rotation state 'INERTIAL', the frame must have
C     zero derivative with respect to any inertial frame. Set the
C     derivative block accordingly.
C
      IF ( INERT ) THEN
C
C        See whether the base frame is inertial.
C
         CALL IRFNUM ( BASNAM, J )

         IF ( J .GT. 0 ) THEN
C
C           The base frame is  a recognized inertial frame.  Zero
C           out the derivative block.
C
            DO I = 1, 3
               CALL CLEARD ( 3, XFORM(4,I) )
            END DO
            
         ELSE
C
C           The base frame is *not* a recognized inertial frame.
C
C           Create the state transformation matrix that maps from the
C           defined frame to J2000.  Zero out the derivative block of
C           this matrix.  Convert the resulting matrix to the state
C           transformation from the defined frame to the output frame.
C           
            CALL ZZFRMCH0 ( BASFRM, J2000, T0,      XF2000 )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXMG     ( XF2000, XFORM, 6, 6, 6, XFTEMP )

            DO I = 1, 3
               CALL CLEARD ( 3, XFTEMP(4,I) )
            END DO

C
C           XFTEMP now represents the transformation from a
C           constant frame matching the defined frame at T0 to the
C           J2000 frame. Produce the transformation from this constant
C           frame to the output frame.
C
C           To avoid introducing additional round-off error into
C           the rotation blocks of XFORM, we overwrite only the
C           derivative block of XFORM with the derivative block
C           of the "inertial" transformation.
C
            CALL INVSTM ( XF2000, XFINV )
            CALL MXMG   ( XFINV,  XFTEMP, 6, 6, 6, XOUT )

            DO I = 1, 3
               CALL VEQU ( XOUT(4,I), XFORM(4,I) )
            END DO            

         END IF

      END IF

C
C     If the frame is frozen, zero out the derivative block of the
C     transformation matrix.
C
      IF ( FROZEN ) THEN

         DO I = 1, 3
            CALL CLEARD ( 3, XFORM(4,I) )
         END DO

      END IF   

C
C     XFORM and BASFRM are set.
C

      CALL CHKOUT ( RNAME )
      RETURN
      END



