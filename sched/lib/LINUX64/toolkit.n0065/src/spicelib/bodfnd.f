C$Procedure BODFND ( Find values from the kernel pool )
 
      LOGICAL FUNCTION BODFND ( BODY, ITEM )
 
C$ Abstract
C
C     Determine whether values exist for some item for any body
C     in the kernel pool.
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
C
C$ Keywords
C
C     CONSTANTS
C
C$ Declarations
 
      INTEGER               BODY
      CHARACTER*(*)         ITEM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   ID code of body.
C     ITEM       I   Item to find ('RADII', 'NUT_AMP_RA', etc.).
C
C$ Detailed_Input
C
C     BODY       is the ID code of the body for which the item is
C                requested. Bodies are numbered according to the
C                standard NAIF numbering scheme.
C
C     ITEM       is the item to be returned. Together, the body and
C                item name combine to form a variable name, e.g.,
C
C                      'BODY599_RADII'
C                      'BODY4_POLE_RA'
C
C$ Detailed_Output
C
C     The result is TRUE if the item is in the kernel pool,
C     and is FALSE if it is not.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     BODVCD, which returns values from the kernel pool, causes an
C     error to be signalled whenever the specified item is not found.
C     In many cases, this is appropriate. However, sometimes the
C     program may attempt to recover, by providing default values,
C     prompting for replacements, and so on.
C
C$ Examples
C
C     In the following example, default values are substituted for
C     bodies for which axes are not found.
C
C        IF ( BODFND ( TARGET, 'RADII' ) ) THEN
C           CALL BODVCD ( TARGET, 'RADII', 3, N, RADII )
C        ELSE
C           CALL VPACK ( 100.D0, 100.D0, 100.D0, RADII )
C        END IF
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      1) Refer to the SPK required reading file for a complete list of
C         the NAIF integer ID codes for bodies.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.2.1, 24-OCT-2005 (NJB)
C
C         Header update:  calls to BODVAR in example code were replaced
C         with calls to BODVCD.  The string 'AXES' and variable AXES
C         were replaced with the string 'RADII' and variable 'RADII'
C         throughout the header.
C
C-     SPICELIB Version 1.2.0, 15-MAR-2002 (NJB)
C
C         Bug fix:  routine was updated to work with string-valued
C         kernel variables.
C
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is TRUE upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990  (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     find constants for a body in the kernel pool
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               VNMLEN
      PARAMETER           ( VNMLEN = 32 )

      INTEGER               NUMLEN
      PARAMETER           ( NUMLEN = 16 )

C
C     Local variables
C
      CHARACTER*(1)         DTYPE
      CHARACTER*(NUMLEN)    CODE
      CHARACTER*(VNMLEN)    VARNAM

      INTEGER               N
  
      LOGICAL               FOUND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
        BODFND = .FALSE.
        RETURN
      ELSE
        CALL CHKIN ( 'BODFND' )
      END IF
 
 
C
C     Construct the variable name from BODY and ITEM.
C
      VARNAM = 'BODY'
 
      CALL INTSTR ( BODY, CODE )
      CALL SUFFIX ( CODE, 0, VARNAM )
      CALL SUFFIX ( '_',  0, VARNAM )
      CALL SUFFIX ( ITEM, 0, VARNAM )
 
C
C     Search the kernel pool for the item.
C
      CALL DTPOOL ( VARNAM, FOUND, N, DTYPE )
 
C
C     Was anything there?
C
      BODFND = FOUND
 
      CALL CHKOUT ( 'BODFND' )
      RETURN
      END
