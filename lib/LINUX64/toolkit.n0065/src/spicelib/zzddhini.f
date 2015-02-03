C$Procedure ZZDDHINI ( Private --- DDH Initialize Structures )
 
      SUBROUTINE ZZDDHINI ( NATBFF,
     .                      SUPBFF,
     .                      NUMSUP,
     .                      STRAMH,
     .                      STRARC,
     .                      STRBFF  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize ZZDDHMAN data structures.
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
 
      INTEGER               NATBFF
      INTEGER               SUPBFF ( * )
      INTEGER               NUMSUP
      CHARACTER*(*)         STRAMH ( * )
      CHARACTER*(*)         STRARC ( * )
      CHARACTER*(*)         STRBFF ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NATBFF     O   Native binary file format.
C     SUPBFF     O   List of supported binary file formats for reading.
C     NUMSUP     O   Number of entries returned in SUPBFF.
C     STRAMH     O   List of labels for METHOD ID codes
C     STRARC     O   List of labels for ARCH ID codes
C     STRBFF     O   List of labels for BFF ID codes
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     NATBFF     is an integer code for the binary file format native
C                to this system as described in the include file
C                'zzddhman.inc'.  Possible values are the parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C     SUPBFF     is an array of integer codes for the binary file
C                formats supports for reading.  At the very minimum
C                it includes NATBFF, but potentially NUMBFF entries.
C                See the include file 'zzddhman.inc'.
C
C     NUMSUP     is the number of entries in the SUPBFF list.
C
C     STRAMH     is a list of strings containing the labels for the
C                access method ID codes defined in the include file
C                'zzddhman.inc'.  These values are retrieved from
C                ZZDDHGSD.  See it for details.
C
C     STRARC     is a list of strings containing the labels for the
C                file architecture ID codes defined in the include file
C                'zzddhman.inc'.  These values are retrieved from
C                ZZDDHGSD.  See it for details.
C
C     STRBFF     is a list of strings containing the labels for the
C                binary file format ID codes defined in the include
C                file 'zzddhman.inc'.  These values are retrieved from
C                ZZDDHGSD.  See it for details.
C
C$ Parameters
C
C     See the include file 'zzddhman.inc'.
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
C     This routine populates a variety data structures that ZZDDHMAN
C     requires to perform its functions.
C
C$ Examples
C
C     See ZZDDHMAN for sample usage.
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
C-    SPICELIB Version 1.0.0, 13-AUGUST-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
      LOGICAL               RETURN
 
C
C     Local Variables.
C
      CHARACTER*(STRLEN)    LINSTR
      CHARACTER*(STRSIZ)    TMPSTR
 
      INTEGER               I
 
      LOGICAL               DONE
 
C
C     Standard SPICE error handling with discovery check in/out.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     Populate the STR### arrays.
C
      DO I = 1, NUMAMH
         CALL ZZDDHGSD ( 'METHOD', I, STRAMH(I) )
      END DO
 
      DO I = 1, NUMARC
         CALL ZZDDHGSD ( 'ARCH', I, STRARC(I) )
      END DO
 
      DO I = 1, NUMBFF
         CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
      END DO
 
C
C     Get the native binary file format.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
      CALL UCASE    ( TMPSTR,        TMPSTR )
 
      NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )
 
      IF ( NATBFF .EQ. 0 ) THEN
 
         CALL CHKIN  ( 'ZZDDHINI'                                   )
         CALL SETMSG ( 'The binary file format, ''#'', is not '
     .   //            'supported by this verison of the toolkit. '
     .   //            'This is a serious problem, contact NAIF.'   )
         CALL ERRCH  ( '#', TMPSTR                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'ZZDDHINI'                                   )
         RETURN
 
      END IF
 
C
C     Now fetch the list of supported binary file formats.
C
      CALL ZZPLATFM ( 'READS_BFF', LINSTR )
 
C
C     Parse the wordlist that is sitting in LINSTR.
C
      I = 0
      DONE = .FALSE.
 
      DO WHILE ( .NOT. DONE )
 
C
C        Increment the counter and pop the next word
C        off.
C
         I = I + 1
         CALL NEXTWD ( LINSTR, TMPSTR, LINSTR )
 
C
C        See if we're done.
C
         DONE = ( I .GT. NUMBFF ) .OR. ( TMPSTR .EQ. ' ' )
 
C
C        If we're not done, then convert this string to the
C        appropriate integer code.
C
         IF ( .NOT. DONE ) THEN
 
            SUPBFF(I) = ISRCHC ( TMPSTR, NUMBFF, STRBFF )
 
C
C           Check to see if the binary file format listed
C           is properly supported.
C
            IF ( SUPBFF(I) .EQ. 0 ) THEN
 
               CALL CHKIN  ( 'ZZDDHINI'                             )
               CALL SETMSG ( 'The binary file format, ''#'', is '
     .         //            'not supported by this verison of '
     .         //            'the toolkit. This is a serious '
     .         //            'problem, contact NAIF.            '   )
               CALL ERRCH  ( '#', TMPSTR                            )
               CALL SIGERR ( 'SPICE(BUG)'                           )
               CALL CHKOUT ( 'ZZDDHINI'                             )
               RETURN
 
            END IF
 
         END IF
 
      END DO
 
C
C     Now setup NUMSUP.  Given the way the WHILE loop above executes,
C     we need to subtract one from I to get the number of entries added
C     to SUPBFF.  This smacks of kludge... but it works.
C
      NUMSUP = I - 1
 
      RETURN
      END
