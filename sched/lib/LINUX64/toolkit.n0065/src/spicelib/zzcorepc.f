C$Procedure ZZCOREPC ( Correct epoch for aberration )

      SUBROUTINE ZZCOREPC ( ABCORR, ET, LT, ETCORR )
      IMPLICIT NONE 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute an aberration corrected epoch, given an aberration 
C     correction specification, an epoch, and a light time.
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

      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ETCORR

      INCLUDE 'zzabcorr.inc'  
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     ABCORR     I   Aberration correction string.
C     ET         I   Ephemeris time, seconds past J2000.
C     LT         I   Light time.
C     ETCORR     O   Light time-corrected epoch.
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
C                       'RL'
C                       'RL+S'
C                       'S'
C                       'XCN'
C                       'XCN+S'
C                       'XLT'
C                       'XLT+S'
C                       'XRL'
C                       'XRL+S'
C                       'XS'
C
C                    Note that some values not supported by the
C                    SPICELIB SPK subsystem are supported by
C                    this routine:
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
C                    Case and leading and trailing blanks are not
C                    significant in ABCORR.
C
C
C     ET             is an epoch, expressed as seconds past J2000 TDB.
C
C     LT             is a light time value, expressed as TDB seconds.
C
C
C$ Detailed_Output
C
C
C     ETCORR         is the input epoch ET, corrected for light time:
C
C                       If the specified aberration correction calls
C                       for some type of light time correction (normal,
C                       converged Newtonian, relativistic), LT will be
C                       added to or subtracted from ET.  If the
C                       correction is of the transmission type, then
C
C                          ETCORR = ET + LT
C
C                       If the correction is of the reception type,
C                       then
C
C                          ETCORR = ET - LT
C
C                       If no light time correction is specified, then
C
C                          ETCORR = ET
C                   
C$ Parameters
C
C     See INCLUDE file zzabcorr.inc.
C
C$ Exceptions
C
C     1) If the input aberration correction choice is not recognized,
C        the error SPICE(INVALIDOPTION) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Many SPICELIB routines have logic branches based on the
C     attributes of aberration corrections.  Much duplicated 
C     parsing code can be avoided by using this routine.
C
C$ Examples
C
C     See ZZDYNFRM.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
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
C-    SPICELIB Version 1.0.0, 24-NOV-2004 (NJB) 
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C

C
C     Local variables
C
      LOGICAL               CORBLK ( ABATSZ )


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCOREPC' )

C
C     Parse the aberration correction string.  Obtain a correction
C     attribute block.
C
      CALL ZZPRSCOR ( ABCORR, CORBLK )


      IF ( CORBLK(LTIDX) ) THEN
C
C        Light time corrections are used.  The output epoch
C        must be adjusted according to whether the correction
C        is for received or transmitted radiation.
C
         IF ( CORBLK(XMTIDX) ) THEN
C
C           This is the transmission case.
C
            ETCORR = ET + LT

         ELSE
C
C           This is the reception case.
C
            ETCORR = ET - LT

         END IF

      ELSE
C
C        Light time corrections are not used.
C
         ETCORR = ET

      END IF

      CALL CHKOUT ( 'ZZCOREPC' )
      RETURN
      END







