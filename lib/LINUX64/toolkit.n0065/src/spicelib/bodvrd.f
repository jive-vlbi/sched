C$Procedure      BODVRD ( Return d.p. values from the kernel pool )
 
      SUBROUTINE BODVRD ( BODYNM, ITEM, MAXN, DIM, VALUES )

C$ Abstract
C
C     Fetch from the kernel pool the double precision values 
C     of an item associated with a body.
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
C     KERNEL
C     NAIF_IDS
C
C$ Keywords
C
C     CONSTANTS
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         BODYNM
      CHARACTER*(*)         ITEM
      INTEGER               MAXN
      INTEGER               DIM
      DOUBLE PRECISION      VALUES  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYNM     I   Body name.
C     ITEM       I   Item for which values are desired. ('RADII',
C                    'NUT_PREC_ANGLES', etc. )
C     MAXN       I   Maximum number of values that may be returned.
C     DIM        O   Number of values returned.
C     VALUES     O   Values.
C
C$ Detailed_Input
C
C     BODYNM     is the name of the body for which ITEM is requested.
C                BODYNM is case-insensitive, and leading and trailing
C                blanks in BODYNM are not significant. Optionally, you
C                may supply the integer ID code for the object as an
C                integer string.  For example both 'MOON' and '301' are
C                legitimate strings that indicate the moon is the body
C                of interest.
C
C     ITEM       is the item to be returned. Together, the NAIF ID
C                code of the body and the item name combine to form a
C                kernel variable name, e.g.,
C
C                      'BODY599_RADII'     
C                      'BODY401_POLE_RA' 
C
C                The values associated with the kernel variable having
C                the name constructed as shown are sought.  Below
C                we'll take the shortcut of calling this kernel variable
C                the "requested kernel variable."
C
C                Note that ITEM *is* case-sensitive.  This attribute
C                is inherited from the case-sensitivity of kernel
C                variable names.
C
C     MAXN       is the maximum number of values that may be returned.
C                The output array VALUES must be declared with size at
C                least MAXN.  It's an error to supply an output array
C                that is too small to hold all of the values associated
C                with the requested kernel variable.
C
C$ Detailed_Output
C
C     DIM        is the number of values returned; this is always the
C                number of values associated with the requested kernel
C                variable unless an error has been signaled.
C
C     VALUES     is the array of values associated with the requested
C                kernel variable.  If VALUES is too small to hold all
C                of the values associated with the kernel variable, the
C                returned values of DIM and VALUES are undefined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input body name cannot be translated to an ID code,
C        and if the name is not a string representation of an integer
C        (for example, '399'), the error SPICE(NOTRANSLATION) is
C        signaled.
C
C     2) If the requested kernel variable is not found in the kernel
C        pool, the error SPICE(KERNELVARNOTFOUND) is signaled.
C
C     3) If the requested kernel variable is found but the associated
C        values aren't numeric, the error SPICE(TYPEMISMATCH) is
C        signaled.
C
C     4) The output array VALUES must be declared with sufficient size
C        to contain all of the values associated with the requested
C        kernel variable.  If the dimension of
C        VALUES indicated by MAXN is too small to contain the
C        requested values, the error SPICE(ARRAYTOOSMALL) is signaled.
C
C     5) If the input dimension MAXN indicates there is more room
C        in VALUES than there really is---for example, if MAXN is
C        10 but values is declared with dimension 5---and the dimension
C        of the requested kernel variable is larger than the actual
C        dimension of VALUES, then this routine may overwrite 
C        memory.  The results are unpredictable.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine simplifies looking up PCK kernel variables by
C     constructing names of requested kernel variables and by
C     performing error checking.
C
C     This routine is intended for use in cases where the maximum
C     number of values that may be returned is known at compile
C     time.  The caller fetches all of the values associated with
C     the specified kernel variable via a single call to this
C     routine.  If the number of values to be fetched cannot be
C     known until run time, the lower-level routine  GDPOOL (an
C     entry point of POOL) should be used instead.  GDPOOL supports 
C     fetching arbitrary amounts of data in multiple "chunks."
C
C     This routine is intended for use in cases where the requested
C     kernel variable is expected to be present in the kernel pool.  If
C     the variable is not found or has the wrong data type, this
C     routine signals an error.  In cases where it is appropriate to
C     indicate absence of an expected kernel variable by returning a
C     boolean "found flag" with the value .FALSE., again the routine
C     GDPOOL should be used.
C
C$ Examples
C
C     1)  When the kernel variable 
C
C            BODY399_RADII
C
C         is present in the kernel pool---normally because a PCK
C         defining this variable has been loaded---the call
C
C            CALL BODVRD ( 'EARTH', 'RADII', 3, DIM, VALUES )
C
C         returns the dimension and values associated with the variable
C         'BODY399_RADII', for example,
C
C            DIM       = 3
C            VALUES(1) = 6378.140
C            VALUES(2) = 6378.140
C            VALUES(3) = 6356.755
C
C
C     2)  The call 
C
C            CALL BODVRD ( 'earth', 'RADII', 3, DIM, VALUES )
C
C         will produce the same results shown in example (1),
C         since the case of the input argument BODYNM is
C         not significant.
C
C
C     3)  The call 
C
C            CALL BODVRD ( '399', 'RADII', 3, DIM, VALUES )
C
C         will produce the same results shown in example (1),
C         since strings containing integer codes are accepted
C         by this routine.
C
C
C     4) The call 
C
C           CALL BODVRD ( 'EARTH', 'radii', 3, DIM, VALUES )
C
C        usually will cause a SPICE(KERNELVARNOTFOUND) error to be
C        signaled, because this call will attempt to look up the
C        values associated with a kernel variable of the name
C
C           'BODY399_radii'
C
C        Since kernel variable names are case sensitive, this
C        name is not considered to match the name
C
C           'BODY399_RADII'
C
C        which normally would be present after a text PCK
C        containing data for all planets and satellites has 
C        been loaded.
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 19-SEP-2013 (BVS)
C
C        Updated to save the input body name and ZZBODTRN state counter
C        and to do name-ID conversion only if the counter has changed.
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to use BODS2C.
C
C-    SPICELIB Version 1.0.0, 23-FEB-2004 (NJB) (BVS) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     fetch constants for a body from the kernel pool
C     physical constants for a body
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to use BODS2C.  This simplifies the name-to-ID
C        mapping code.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C 
      INTEGER               CDELEN
      PARAMETER           ( CDELEN = 16 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )


C
C     Local variables
C
      CHARACTER*(CDELEN)    CODE
      CHARACTER*1           TYPE
      CHARACTER*(NAMLEN)    VARNAM

      INTEGER               BODYID

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBDNM
      INTEGER               SVBDID
      LOGICAL               SVFND1

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVBDNM
      SAVE                  SVBDID
      SAVE                  SVFND1

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
        RETURN
      ELSE
        CALL CHKIN ( 'BODVRD' )
      END IF

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counter.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF
 
C
C     Translate the input name to an ID code.
C     
      CALL ZZBODS2C ( SVCTR1, SVBDNM, SVBDID, SVFND1,
     .                BODYNM, BODYID, FOUND    )
      
      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The body name # could not be translated ' //
     .                 'to a NAIF ID code.  The cause of this '   //
     .                 'problem may be that you need an updated ' //
     .                 'version of the SPICE Toolkit.'            )
         CALL ERRCH  ( '#', BODYNM                                )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'BODVRD'                                   )
         RETURN

      END IF
      
C
C     Construct the variable name from BODY and ITEM.
C
      VARNAM = 'BODY'
 
      CALL INTSTR ( BODYID,    CODE   )
      CALL SUFFIX ( CODE,   0, VARNAM )
      CALL SUFFIX ( '_',    0, VARNAM )
      CALL SUFFIX ( ITEM,   0, VARNAM )

C
C     Make sure the item is present in the kernel pool.
C
      CALL DTPOOL ( VARNAM, FOUND, DIM, TYPE )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The variable # could not be found in the ' //
     .                 'kernel pool.'                              )
         CALL ERRCH  ( '#', VARNAM                                 )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                  )
         CALL CHKOUT ( 'BODVRD'                                    )
         RETURN

      END IF

C
C     Make sure the item's data type is numeric.  
C
      IF ( TYPE .NE. 'N' ) THEN

         CALL SETMSG ( 'The data associated with variable # ' //
     .                 'are not of numeric type.'             )
         CALL ERRCH  ( '#', VARNAM                            )
         CALL SIGERR ( 'SPICE(TYPEMISMATCH)'                  )
         CALL CHKOUT ( 'BODVRD'                               )
         RETURN

      END IF

C
C     Make sure there's enough room in the array VALUES to hold
C     the requested data.  
C
      IF ( MAXN .LT. DIM ) THEN

         CALL SETMSG ( 'The data array associated with variable # ' //
     .                 'has dimension #, which is larger than the ' //
     .                 'available space # in the output array.'    )
         CALL ERRCH  ( '#', VARNAM                                 )
         CALL ERRINT ( '#', DIM                                    )
         CALL ERRINT ( '#', MAXN                                   )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                      )
         CALL CHKOUT ( 'BODVRD'                                    )
         RETURN

      END IF

C
C     Grab the values.  We know at this point they're present in
C     the kernel pool, so we don't check the FOUND flag.
C
      CALL GDPOOL ( VARNAM, 1, MAXN, DIM, VALUES, FOUND )

      CALL CHKOUT ( 'BODVRD' )
      RETURN
      END
