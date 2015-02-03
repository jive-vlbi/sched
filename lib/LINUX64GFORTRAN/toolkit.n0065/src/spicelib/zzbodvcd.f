C$Procedure      ZZBODVCD ( Return d.p. values from the kernel pool )
 
      SUBROUTINE ZZBODVCD ( BODYID, ITEM, MAXN, DIM, VALUES )
      IMPLICIT NONE

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Fetch from the kernel pool the double precision values 
C     of an item associated with a body.  Use an integer ID code
C     rather than name to identify the body of interest.
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
C     PRIVATE
C     CONSTANTS
C
C$ Declarations
 
      INTEGER               BODYID
      CHARACTER*(*)         ITEM
      INTEGER               MAXN
      INTEGER               DIM
      DOUBLE PRECISION      VALUES  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID code.
C     ITEM       I   Item for which values are desired. ('RADII',
C                    'NUT_PREC_ANGLES', etc. )
C     MAXN       I   Maximum number of values that may be returned.
C     DIM        O   Number of values returned.
C     VALUES     O   Values.
C
C$ Detailed_Input
C
C     BODYID     is the NAIF ID code of the body for which ITEM is
C                requested.
C
C     ITEM       is the item to be returned. Together, the NAIF ID
C                code of the body and the item name combine to form a
C                variable name, e.g.,
C
C                      'BODY599_RADII'     
C                      'BODY401_POLE_RA'   
C
C                Note that ITEM *is* case-sensitive.
C
C     MAXN       is the maximum number of values that may be 
C                returned.  The output array VALUES should be
C                declared with size at least MAXN.
C
C$ Detailed_Output
C
C     DIM        is the number of values associated with the variable.
C
C     VALUES     are the values associated with the variable.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the requested item is not found in the kernel pool,
C        the error SPICE(KERNELVARNOTFOUND) is signaled.
C
C     2) If the requested item is found but the associated values
C        aren't numeric, the error SPICE(TYPEMISMATCH) is signaled.
C
C     3) If there's not enough room in the output array to capture
C        the requested values, the error SPICE(ARRAYTOOSMALL) is 
c        signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine simplifies looking up PCK kernel variables by
C     constructing the kernel variable names and performing
C     error checking.
C
C     For cases where it's not an error if the requested variable
C     is not present in the kernel pool, or for cases where the
C     data array associated with the kernel variable is too large
C     to fetch in one shot, use the lower-level routine GDPOOL.
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
C            CALL ZZBODVCD ( 399, 'RADII', 3, DIM, VALUE )
C
C         returns the dimension and values associated with the variable
C         'BODY399_RADII', for example,
C
C            DIM      = 3
C            VALUE(1) = 6378.140
C            VALUE(2) = 6378.140
C            VALUE(3) = 6356.755
C
C     2) The call 
C
C           CALL ZZBODVCD ( 399, 'radii', 3, DIM, VALUE )
C
C        usually will cause a SPICE(KERNELVARNOTFOUND) error to be
C        signaled, because this call will attempt to look up the
C        values associated with a kernel variable of the name
C
C           BODY399_radii
C
C        Since kernel variable names are case sensitive, this
C        name is not considered to match the name
C
C           BODY399_RADII
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
C-     SPICELIB Version 1.0.0, 10-FEB-2004 (NJB) (BVS) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     fetch constants for a body from the kernel pool
C     physical constants for a body
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
C     Local variables
C
      CHARACTER*(CDELEN)    CODE
      CHARACTER*1           TYPE
      CHARACTER*(NAMLEN)    VARNAM

      LOGICAL               FOUND
  
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
        RETURN
      ELSE
        CALL CHKIN ( 'ZZBODVCD' )
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
         CALL CHKOUT ( 'ZZBODVCD'                                  )
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
         CALL CHKOUT ( 'ZZBODVCD'                             )
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
         CALL CHKOUT ( 'ZZBODVCD'                                  )
         RETURN

      END IF

C
C     Grab the values.  We know at this point they're present in
C     the kernel pool, so we don't check the FOUND flag.
C
      CALL GDPOOL ( VARNAM, 1, MAXN, DIM, VALUES, FOUND )

      CALL CHKOUT ( 'ZZBODVCD' )
      RETURN
      END
