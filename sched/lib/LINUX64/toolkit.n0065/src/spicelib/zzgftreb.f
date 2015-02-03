C$Procedure ZZGFTREB ( Geometry finder: return body axes )
 
      SUBROUTINE ZZGFTREB ( BODY, AXES )

C$ Abstract
C
C     Return the values of the triaxial radii for any body in the
C     kernel pool.
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
C
C$ Keywords
C
C     CONSTANTS
C
C$ Declarations

      IMPLICIT NONE 

      INTEGER              BODY
      DOUBLE PRECISION     AXES ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   NAIF ID code of body.
C     AXES       O   Length of axes of body (1,2,3, as defined below).
C
C$ Detailed_Input
C
C     BODY       is the NAIF ID code of the body for which the axes are
C                requested. Bodies are numbered according to the
C                standard NAIF numbering scheme described in the
C                required reading (naif_ids.req) document.
C
C$ Detailed_Output
C
C     AXES       are the lengths of the axes of the body, in km.
C
C                      AXES(1)  is the longest equatorial radius of
C                               the body. For satellites, this axis is
C                               typically pointed toward the primary
C                               planet.
C
C                      AXES(2)  is the shortest equatorial radius of
C                               the body.
C
C                      AXES(3)  is the polar radius of the body.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the body specified does not have 3 axes defined,
C          then the error SPICE(ZEROAXISLENGTH) is signaled.
C
C$ Files
C
C     PCK data:  triaxial radii for the target body must be loaded
C     into the kernel pool.  Typically this is done by loading a
C     text PCK file via LDPOOL or a general kernel loader
C     such as FURNSH.
C
C$ Particulars
C
C     ZZGFTREB returns the lengths of the axes of the target body.
C     Appropriate SPK and PCK data must be available to the calling
C     program before this routine is called.
C
C$ Examples
C
C     The call
C
C         CALL ZZGFTREB ( 399,  VALUE )
C
C     returns the values associated with the variable 'BODY399_RADII',
C     for example,
C
C          VALUE(1) = 6378.140
C          VALUE(2) = 6378.140
C          VALUE(3) = 6356.755
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1) Refer to the SPK required reading file for a complete list of
C     the NAIF integer ID codes for bodies.
C
C     2) ''Report of the IAU/IAG/COSPAR Working Group on Cartographic
C     Coordinates and Rotational Elements of the Planets and
C     Satellites: 1991,'' March 3, 1992.
C
C$ Author_and_Institution
C
C     I.M. Underwood (JPL)
C     W.L. Taber     (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0  05-MAR-2003 (EDW)
C
C-&

C$ Index_Entries
C
C     Return the values of the triaxial radii
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               I
      INTEGER               N
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFTREB' ) 
      END IF   
 
 
C
C     Look it up in the kernel pool.
C
      CALL BODVCD ( BODY, 'RADII', 3, N, AXES )
 
      IF ( N .NE. 3 ) THEN
 
         CALL SETMSG ( 'Only # axes were found  '
     .        //       'for ID # . Three axes are needed.' )
         CALL ERRINT ( '#',   N                            )
         CALL ERRINT ( '#',   BODY                         )
         CALL SIGERR ( 'SPICE(ZEROAXISLENGTH)'             )
         CALL CHKOUT ( 'ZZGFTREB'                            )
         RETURN
 
      ELSE
 
         DO I = 1, 3
 
           IF ( AXES(I) .LT. 0.0D0 ) THEN
 
               CALL SETMSG ('The # axis of body # is '
     .         //           'negative.  Please check your '
     .         //           'text PCK file. You should fix the '
     .         //           ' # component of the kernel pool '
     .         //           'variable  BODY#_RADII. '          )
 
               CALL ERRINT ('#', I                             )
               CALL ERRINT ('#', BODY                          )
               CALL ERRINT ('#', I                             )
               CALL ERRINT ('#', BODY                          )
               CALL SIGERR ('SPICE(BADAXISNUMBERS)'            )
               CALL CHKOUT ('ZZGFTREB'                           )
               RETURN
           END IF
 
         END DO
 
      END IF 

      CALL CHKOUT ( 'ZZGFTREB' )
      RETURN
      END
