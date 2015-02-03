C$Procedure ZZVALCOR ( Validate aberration correction )

      SUBROUTINE ZZVALCOR ( ABCORR, ATTBLK )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Validate an aberration correction string suitable for use by
C     the SPK system; return attributes. 
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
C     None.
C
C$ Keywords
C
C     ABERRATION
C     PARSING
C     PRIVATE
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
      
      CHARACTER*(*)         ABCORR
      LOGICAL               ATTBLK ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     ABCORR     I   Aberration correction string.
C     ATTBLK     O   Aberration correction attribute block.
C
C$ Detailed_Input
C
C     ABCORR         is a string representing a aberration
C                    correction.  The supported values are:
C
C                       'CN'
C                       'CN+S'
C                       'LT'
C                       'LT+S'
C                       'NONE'
C                       'XCN'
C                       'XCN+S'
C                       'XLT'
C                       'XLT+S'
C
C                    Note that some values not supported by the
C                    SPICELIB SPK subsystem are supported by
C                    the underlying routine ZZPRSCOR:
C
C                       - The letter 'R' indicates relativistic
C                         corrections.
C
C                       - Stellar aberration-only corrections are
C                         indicated by the strings
C
C                            'S'
C                            'XS'
C
C                    This routine *does not* permit values that 
C                    the SPK system doesn't handle.
C
C                    Case and embedded blanks are not significant in
C                    ABCORR.
C
C                    If ABCORR contains an unsupported value, this
C                    routine will signal an error.
C
C$ Detailed_Output
C
C     ATTBLK         is a block of logical flags indicating the
C                    attributes of the aberration correction 
C                    specified by ABCORR.  The attributes are:
C
C                       - Is the correction "geometric"?
C
C                       - Is light time correction indicated?
C
C                       - Is stellar aberration correction indicated?
C
C                       - Is the light time correction of the 
C                         "converged Newtonian" variety?
C
C                       - Is the correction for the transmission 
C                         case?
C
C                       - Is the correction relativistic? (This
C                         value is always .FALSE. for aberration
C                         corrrection specifications allowed by
C                         this routine.)
C                        
C                    The structure of ATTBLK is defined in the
C                    include file 
C
C                       zzabcorr.inc
C
C                    The size of ATTBLK and the offsets of the
C                    component flags are defined there.
C                    
C$ Parameters
C
C     See INCLUDE file zzabcorr.inc.
C
C$ Exceptions
C
C     1) If the input aberration correction choice is not allowed,
C        the error SPICE(INVALIDOPTION) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is similar to ZZPRSCOR, but stellar aberration-only
C     and relativistic corrections specifications are not allowed
C     by this routine. The allowed values are precisely those allowed
C     by SPKEZR.
C
C$ Examples
C
C     See ZZGFOCIN.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
C
C     2) This routine recognizes some aberration corrections not
C        handled by most SPICELIB routines.  Callers should do
C        their own checking to ensure the parsed correction is
C        acceptable.
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
C-    SPICELIB Version 1.0.0, 11-APR-2008 (NJB) 
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZVALCOR' )

C
C     Parse the aberration correction string and obtain 
C     an attribute block.
C
      CALL ZZPRSCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZVALCOR' )
         RETURN
      END IF

C
C     Check the attribute block. We don't allow relativistic 
C     corrections.
C     
      IF ( ATTBLK(RELIDX) ) THEN

         CALL SETMSG ( 'Aberration correction specification # calls '
     .   //            'for relativistic corrections, which are '
     .   //            'not supported.'                               )
         CALL ERRCH  ( '#',  ABCORR                                   )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                         )
         CALL CHKOUT ( 'ZZVALCOR'                                     )
         RETURN

      END IF

C
C     Stellar aberration corrections are allowed only if light
C     time corrections are specified as well.
C
      IF (  ATTBLK(STLIDX)  .AND.  ( .NOT. ATTBLK(LTIDX) )  ) THEN
         
         CALL SETMSG ( 'Aberration correction specification # calls '
     .   //            'for stellar aberration correction without '
     .   //            'light time correction; this combination '
     .   //            'is not supported.'                            )
         CALL ERRCH  ( '#',  ABCORR                                   )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                         )
         CALL CHKOUT ( 'ZZVALCOR'                                     )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZVALCOR' )
      RETURN
      END



