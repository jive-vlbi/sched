
C$ Abstract
C
C     Include file zzdyn.inc
C
C     SPICE private file intended solely for the support of SPICE
C     routines.  Users should not include this file directly due
C     to the volatile nature of this file
C
C     The parameters defined below are used by the SPICELIB dynamic
C     frame subsystem.
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
C$ Parameters
C
C     This file declares parameters required by the dynamic
C     frame routines of the SPICELIB frame subsystem.
C
C$ Restrictions
C
C     The parameter BDNMLN is this routine must be kept 
C     consistent with the parameter MAXL defined in 
C
C        zzbodtrn.inc
C
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 12-JAN-2005 (NJB)
C
C        Parameters KWX, KWY, KWZ renamed to KVX, KVY, KVZ.
C
C-    SPICELIB Version 1.0.0, 22-DEC-2004 (NJB)
C
C-&


C
C     String length parameters
C     ========================
C 
C
C     Kernel variable name length.  This parameter must be 
C     kept consistent with the parameter MAXLEN used in the
C     POOL umbrella routine.
C 
      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

C
C     Length of a character kernel pool datum. This parameter must be
C     kept consistent with the parameter MAXCHR used in the POOL
C     umbrella routine.
C
      INTEGER               KVLEN
      PARAMETER           ( KVLEN  = 80 )

C
C     Reference frame name length.  This parameter must be 
C     kept consistent with the parameter WDSIZE used in the
C     FRAMEX umbrella routine.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Body name length.  This parameter is used to provide a level
C     of indirection so the dynamic frame source code doesn't 
C     have to change if the name of this SPICELIB-scope parameter
C     is changed.  The value MAXL used here is defined in the
C     INCLUDE file
C
C        zzbodtrn.inc
C
C     Current value of MAXL = 36
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )


C
C     Numeric parameters
C     ===================================
C
C     The parameter MAXCOF is the maximum number of polynomial
C     coefficients that may be used to define an Euler angle
C     in an "Euler frame" definition
C     
      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = 20 )

C
C     The parameter LBSEP is the default angular separation limit for
C     the vectors defining a two-vector frame.  The angular separation
C     of the vectors must differ from Pi and 0 by at least this amount.
C
      DOUBLE PRECISION      LBSEP
      PARAMETER           ( LBSEP  = 1.D-3 )

C
C     The parameter QEXP is used to determine the width of
C     the interval DELTA used for the discrete differentiation
C     of velocity in the routines ZZDYNFRM, ZZDYNROT, and their
C     recursive analogs.  This parameter is appropriate for 
C     64-bit IEEE double precision numbers; when SPICELIB
C     is hosted on platforms where longer mantissas are supported,
C     this parameter (and hence this INCLUDE file) will become
C     platform-dependent.
C
C     The choice of QEXP is based on heuristics.  It's believed to
C     be a reasonable choice obtainable without expensive computation.
C
C     QEXP is the largest power of 2 such that 
C
C        1.D0 + 2**QEXP  =  1.D0
C
C     Given an epoch T0 at which a discrete derivative is to be
C     computed, this choice provides a value of DELTA that usually
C     contributes no round-off error in the computation of the function
C     evaluation epochs
C
C        T0 +/- DELTA
C
C     while providing the largest value of DELTA having this form that
C     causes the order of the error term O(DELTA**2) in the quadratric
C     function approximation to round to zero.  Note that the error
C     itself will normally be small but doesn't necessarily round to
C     zero.  Note also that the small function approximation error
C     is not a measurement of the error in the discrete derivative 
C     itself.  
C
C     For ET values T0 > 2**27 seconds past J2000, the value of 
C     DELTA will be set to 
C
C        T0 * 2**QEXP
C      
C     For smaller values of T0, DELTA should be set to 1.D0.
C
      INTEGER               QEXP
      PARAMETER           ( QEXP = -27 )


C
C     Frame kernel parameters
C     =======================
C
C     Parameters relating to kernel variable names (keywords) start
C     with the letters
C
C        KW
C
C     Parameters relating to kernel variable values start with the
C     letters
C
C        KV
C
C
C     Generic parameters
C     ---------------------------------
C
C     Token used to build the base frame keyword:
C      
      CHARACTER*(*)         KWBFRM
      PARAMETER           ( KWBFRM = 'RELATIVE' )

C
C     Frame definition style parameters
C     ---------------------------------
C
C     Token used to build the frame definition style keyword:
C      
      CHARACTER*(*)         KWSTYL
      PARAMETER           ( KWSTYL = 'DEF_STYLE' )

C
C     Token indicating parameterized dynamic frame.
C
      CHARACTER*(*)         KVPARM
      PARAMETER           ( KVPARM = 'PARAMETERIZED' )


C
C     Freeze epoch parameters
C     ---------------------------------
C
C     Token used to build the freeze epoch keyword:
C      
      CHARACTER*(*)         KWFREZ
      PARAMETER           ( KWFREZ = 'FREEZE_EPOCH' )

C
C     Rotation state parameters
C     ---------------------------------
C
C     Token used to build the rotation state keyword:
C      
      CHARACTER*(*)         KWRSTA
      PARAMETER           ( KWRSTA = 'ROTATION_STATE' )

C
C     Token indicating rotating rotation state:
C      
      CHARACTER*(*)         KVROTG
      PARAMETER           ( KVROTG = 'ROTATING' )

C
C     Token indicating inertial rotation state:
C      
      CHARACTER*(*)         KVINRT
      PARAMETER           ( KVINRT = 'INERTIAL' )

C
C     Frame family parameters
C     ---------------------------------
C
C     Token used to build the frame family keyword:
C      
      CHARACTER*(*)         KWFFAM
      PARAMETER           ( KWFFAM = 'FAMILY' )

C
C     Token indicating mean equator and equinox of date frame.
C
      CHARACTER*(*)       KVMEQT
      PARAMETER         ( KVMEQT = 'MEAN_EQUATOR_AND_EQUINOX_OF_DATE'  )

C
C     Token indicating mean ecliptic and equinox of date frame.
C
      CHARACTER*(*)       KVMECL
      PARAMETER         ( KVMECL = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE' )

C
C     Token indicating true equator and equinox of date frame.
C
      CHARACTER*(*)       KVTEQT
      PARAMETER         ( KVTEQT = 'TRUE_EQUATOR_AND_EQUINOX_OF_DATE'  )

C
C     Token indicating two-vector frame.
C
      CHARACTER*(*)         KV2VEC
      PARAMETER           ( KV2VEC = 'TWO-VECTOR'  )

C
C     Token indicating Euler frame.
C
      CHARACTER*(*)         KVEULR
      PARAMETER           ( KVEULR = 'EULER'  )


C
C     "Of date" frame family parameters
C     ---------------------------------
C
C     Token used to build the precession model keyword:
C
      CHARACTER*(*)         KWPRCM
      PARAMETER           ( KWPRCM = 'PREC_MODEL' )

C
C     Token used to build the nutation model keyword:
C
      CHARACTER*(*)         KWNUTM
      PARAMETER           ( KWNUTM = 'NUT_MODEL' )

C
C     Token used to build the obliquity model keyword:
C
      CHARACTER*(*)         KWOBQM
      PARAMETER           ( KWOBQM = 'OBLIQ_MODEL' )

C
C     Mathematical models used to define "of date" frames will
C     likely accrue over time.  We will simply assign them 
C     numbers.
C
C
C     Token indicating the Lieske earth precession model:
C
      CHARACTER*(*)         KVM001
      PARAMETER           ( KVM001 = 'EARTH_IAU_1976' )
      
C
C     Token indicating the IAU 1980 earth nutation model:
C
      CHARACTER*(*)         KVM002
      PARAMETER           ( KVM002 = 'EARTH_IAU_1980' )

C
C     Token indicating the IAU 1980 earth mean obliqity of
C     date model.  Note the name matches that of the preceding
C     nutation model---this is intentional.  The keyword
C     used in the kernel variable definition indicates what
C     kind of model is being defined.
C
      CHARACTER*(*)         KVM003
      PARAMETER           ( KVM003 = 'EARTH_IAU_1980' )


C
C     Two-vector frame family parameters
C     ---------------------------------
C
C     Token used to build the vector axis keyword:
C      
      CHARACTER*(*)         KWVAXI
      PARAMETER           ( KWVAXI = 'AXIS' )

C
C     Tokens indicating axis values:
C
      CHARACTER*(*)         KVX
      PARAMETER           ( KVX    = 'X' )

      CHARACTER*(*)         KVY
      PARAMETER           ( KVY    = 'Y' )

      CHARACTER*(*)         KVZ
      PARAMETER           ( KVZ    = 'Z' )

C
C     Prefixes used for primary and secondary vector definition
C     keywords:
C
      CHARACTER*(*)         KWPRI
      PARAMETER           ( KWPRI  = 'PRI_' )

      CHARACTER*(*)         KWSEC
      PARAMETER           ( KWSEC  = 'SEC_' )
      
C
C     Token used to build the vector definition keyword:
C      
      CHARACTER*(*)         KWVCDF
      PARAMETER           ( KWVCDF = 'VECTOR_DEF' )

C
C     Token indicating observer-target position vector:
C      
      CHARACTER*(*)         KVPOSV
      PARAMETER           ( KVPOSV = 'OBSERVER_TARGET_POSITION' )

C
C     Token indicating observer-target velocity vector:
C      
      CHARACTER*(*)         KVVELV
      PARAMETER           ( KVVELV = 'OBSERVER_TARGET_VELOCITY' )

C
C     Token indicating observer-target near point vector:
C      
      CHARACTER*(*)         KVNEAR
      PARAMETER           ( KVNEAR = 'TARGET_NEAR_POINT' )

C
C     Token indicating constant vector:
C      
      CHARACTER*(*)         KVCONS
      PARAMETER           ( KVCONS = 'CONSTANT' )

C
C     Token used to build the vector observer keyword:
C      
      CHARACTER*(*)         KWVOBS
      PARAMETER           ( KWVOBS = 'OBSERVER' )

C
C     Token used to build the vector target keyword:
C      
      CHARACTER*(*)         KWVTRG
      PARAMETER           ( KWVTRG = 'TARGET' )

C
C     Token used to build the vector frame keyword:
C      
      CHARACTER*(*)         KWVFRM
      PARAMETER           ( KWVFRM = 'FRAME' )

C
C     Token used to build the vector aberration correction keyword:
C      
      CHARACTER*(*)         KWVABC
      PARAMETER           ( KWVABC = 'ABCORR' )

C
C     Token used to build the constant vector specification keyword:
C      
      CHARACTER*(*)         KWVSPC
      PARAMETER           ( KWVSPC = 'SPEC' )

C
C     Token indicating rectangular coordinates used to
C     specify constant vector:
C
      CHARACTER*(*)         KVRECC
      PARAMETER           ( KVRECC = 'RECTANGULAR' )

C
C     Token indicating latitudinal coordinates used to
C     specify constant vector:
C
      CHARACTER*(*)         KVLATC
      PARAMETER           ( KVLATC = 'LATITUDINAL' )

C
C     Token indicating RA/DEC coordinates used to
C     specify constant vector:
C
      CHARACTER*(*)         KVRADC
      PARAMETER           ( KVRADC = 'RA/DEC' )

C
C     Token used to build the cartesian vector literal keyword:
C      
      CHARACTER*(*)         KWVECT
      PARAMETER           ( KWVECT = 'VECTOR' )

C
C     Token used to build the constant vector latitude keyword:
C      
      CHARACTER*(*)         KWLAT
      PARAMETER           ( KWLAT  = 'LATITUDE' )

C
C     Token used to build the constant vector longitude keyword:
C      
      CHARACTER*(*)         KWLON
      PARAMETER           ( KWLON  = 'LONGITUDE' )

C
C     Token used to build the constant vector right ascension keyword:
C      
      CHARACTER*(*)         KWRA
      PARAMETER           ( KWRA   = 'RA' )

C
C     Token used to build the constant vector declination keyword:
C      
      CHARACTER*(*)         KWDEC
      PARAMETER           ( KWDEC  = 'DEC' )

C
C     Token used to build the angular separation tolerance keyword:
C      
      CHARACTER*(*)         KWATOL
      PARAMETER           ( KWATOL = 'ANGLE_SEP_TOL' )

C
C     See the section "Physical unit parameters" below for additional
C     parameters applicable to two-vector frames.
C

C
C     Euler frame family parameters
C     ---------------------------------
C
C     Token used to build the epoch keyword:
C      
      CHARACTER*(*)         KWEPOC
      PARAMETER           ( KWEPOC = 'EPOCH' )

C
C     Token used to build the Euler axis sequence keyword:
C      
      CHARACTER*(*)         KWEUAX
      PARAMETER           ( KWEUAX = 'AXES' )

C
C     Tokens used to build the Euler angle coefficients keywords:
C      
      CHARACTER*(*)         KWEAC1
      PARAMETER           ( KWEAC1 = 'ANGLE_1_COEFFS' )

      CHARACTER*(*)         KWEAC2
      PARAMETER           ( KWEAC2 = 'ANGLE_2_COEFFS' )

      CHARACTER*(*)         KWEAC3
      PARAMETER           ( KWEAC3 = 'ANGLE_3_COEFFS' )

C
C     See the section "Physical unit parameters" below for additional
C     parameters applicable to Euler frames.
C

C
C     Physical unit parameters
C     ---------------------------------
C
C     Token used to build the units keyword:
C      
      CHARACTER*(*)         KWUNIT
      PARAMETER           ( KWUNIT = 'UNITS' )

C
C     Token indicating radians:
C
      CHARACTER*(*)         KVRADN
      PARAMETER           ( KVRADN = 'RADIANS' )

C
C     Token indicating degrees:
C
      CHARACTER*(*)         KVDEGR
      PARAMETER           ( KVDEGR = 'DEGREES' )


C
C     End of include file zzdyn.inc 
C
