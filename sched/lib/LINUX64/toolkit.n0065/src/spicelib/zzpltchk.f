C$Procedure ZZPLTCHK ( Private --- Platform Check )
 
      SUBROUTINE ZZPLTCHK ( OK )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Validate the runtime environment against values assumed by the
C     current toolkit source package.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   Logical indicating the runtime environment is ok.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical when set to .TRUE. indicates that the
C                runtime environment passes any checks implemented
C                by this routine against the configured code.
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
C     1) This routine signals SPICE(BUG) if it determines the runtime
C        environment is incompatible with the configured binary file
C        format.
C
C$ Particulars
C
C     This routine encapsulates a series of checks to diagnose the
C     runtime environment against assumptions configured in the
C     source code.  Configuration errors are reported via the error
C     SPICE(BUG).
C
C$ Examples
C
C     See ZZDDHOPN for sample usage.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 07-AUG-2002 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
C
C     Local Variables
C
      CHARACTER*(WDSIZE)    STRBFF
      CHARACTER*(WDSIZE)    VALUE
      CHARACTER*(WDSIZE)    RTEBFF
 
      INTEGER               BFF
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZPLTCHK' )
      END IF
 
C
C     Verify that the runtime environment's binary file format agrees
C     with the value listed in ZZPLATFM.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', STRBFF )
 
C
C     Determine what the runtime environment binary file format appears
C     to be.
C
      CALL ZZGETBFF ( BFF )
      CALL ZZDDHGSD ( 'BFF', BFF, RTEBFF )
 
C
C     Check results, signal SPICE(BUG) if a discrepancy appears.
C
      IF ( STRBFF .NE. RTEBFF ) THEN
 
         CALL SETMSG   ( 'This version of SPICELIB was originally '
     .   //              'packaged by NAIF for # hardware using '
     .   //              '# with the # compiler.  This '
     .   //              'environment has a binary '
     .   //              'file format of #; however the software is '
     .   //              'running on an environment that has a binary '
     .   //              'file format of #.  This is a severe problem '
     .   //              'and may be because the software package was '
     .   //              'intended for use on a different computer '
     .   //              'system.  It also may be the result of an '
     .   //              'improper port; please contact NAIF.'         )
 
         CALL ZZPLATFM ( 'SYSTEM',   VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )
 
         CALL ZZPLATFM ( 'O/S',      VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )
 
         CALL ZZPLATFM ( 'COMPILER', VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )
 
         CALL ERRCH    ( '#',        STRBFF                            )
 
         IF ( RTEBFF .EQ. ' ' ) THEN
            CALL ERRCH ( '#', 'UNKNOWN' )
         ELSE
            CALL ERRCH ( '#',  RTEBFF   )
         END IF
 
         CALL SIGERR   ( 'SPICE(BUG)'                                  )
         CALL CHKOUT   ( 'ZZPLTCHK'                                    )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZPLTCHK' )
      RETURN
 
      END
