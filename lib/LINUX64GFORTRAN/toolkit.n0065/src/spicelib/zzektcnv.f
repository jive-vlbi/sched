C$Procedure  ZZEKTCNV ( Private: EK, time conversion )
 
      SUBROUTINE ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )
      IMPLICIT NONE

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Convert time strings from EK query to ephemeris time.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ektype.inc'
 
      CHARACTER*(*)         TIMSTR
      DOUBLE PRECISION      ET
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TIMSTR     I   Time string.
C     ET         O   Ephemeris time in seconds past J2000, TDB.
C     ERROR      O   Error flag.
C     ERRMSG     O   Error message.
C
C$ Detailed_Input
C
C     TIMSTR         is a string representing a time value.  The value
C                    make be an SCLK string in the form
C
C                       <clock name> SCLK <clock string>
C
C                    or may be any string acceptable to ST2ET.
C
C$ Detailed_Output
C
C     ET             is the ephemeris time equivalent to the input
C                    time.
C
C     ERROR          is a logical flag indicating whether an error was
C                    detected.  Note that a time string might be
C                    syntactically valid, but incapable of being
C                    converted to ET if the appropriate time kernels
C                    (Leapseconds or SCLK) are not loaded.
C
C     ERRMSG         is an error message describing an error in the
C                    input query, if one was detected.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If any sort of time conversion error occurs, the output flag
C         ERROR is set, and an error message is returned.
C
C         The routine attempts to avoid causing errors that must
C         be trapped by SPICELIB error handling.  Time string syntax
C         errors or missing kernel files, for example, should not trip
C         SPICELIB error handling.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Strings representing time values are interpreted as follows:
C
C        1)  The string is first examined to see whether it's an
C            SCLK string for a recognized clock; if it is, the
C            string is converted to the equivalent ET.
C
C        2)  If the string is not a SCLK string, it is expected
C            to be some sort of standard time representation. 
C            The string is checked to see whether it's in a format 
C            that TPARTV can handle.  If TPARTV can't deal with it,
C            the string is considered to be invalid.
C
C$ Examples
C
C     See ZZEKTRES.
C
C$ Restrictions
C
C     1) A leapseconds kernel must be loaded at the time this routine
C        is called.
C
C     2) In order to convert SCLK strings, an appropriate SCLK kernel
C        must be loaded at the time this routine is called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB) 
C
C        Now converts standard time strings to ET via STR2ET instead
C        of the less general routines ISO2UTC and UTC2ET.
C
C        Bug fix:  modified algorithm to handle case where string
C        "SCLK" appears in SCLK name.
C
C        Bug fix:  construction of error messages in case where 
C        FAILED() returns .TRUE. has been changed so that REPMC is 
C        not called.  Instead, the error-free routine SUFFIX is 
C        used.
C
C-    SPICELIB Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB) 
C
C        Now converts standard time strings to ET via STR2ET instead
C        of the less general routines ISO2UTC and UTC2ET.
C
C        Bug fix:  modified algorithm to handle case where string
C        "SCLK" appears in SCLK name.
C
C        Bug fix:  construction of error messages in case where 
C        FAILED() returns .TRUE. has been changed so that REPMC is 
C        not called.  Instead, the error-free routine SUFFIX is 
C        used.
C
C-&

C
C     SPICELIB functions
C
      INTEGER               POSR
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 160 )

      INTEGER               NCOMP
      PARAMETER           ( NCOMP  = 10 )
 
      INTEGER               MAXMOD
      PARAMETER           ( MAXMOD = 10 )

      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )
 
C
C     Local variables
C
      CHARACTER*(LNSIZE)    LOCSTR
      CHARACTER*(SHORT)     MODIFY ( MAXMOD )
      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(MSGLEN)    SCLMSG
      CHARACTER*(SHORT)     TYPE
 
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TVEC   ( NCOMP )
 
      INTEGER               CLKID
      INTEGER               LOC
      INTEGER               NTVEC
 
      LOGICAL               FND
      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV
 

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKTCNV' )

C
C     No error to start with.
C
      ERROR  =  .FALSE.
      ERRMSG =  ' '
 
C
C     Get a left-justified, compressed, upper-case copy of
C     the string, so we can easily search it for substrings
C     that would identify it as SCLK.  If we do find a
C     match, remove the identifying substring (of the form
C     'MO SCLK', 'VGR1 SCLK', etc.).
C
      CALL CMPRSS ( ' ',   1,  TIMSTR,  LOCSTR )
      CALL LJUST  ( LOCSTR,             LOCSTR )
      CALL UCASE  ( LOCSTR,             LOCSTR )
 
      LOC  =  POSR ( LOCSTR, 'SCLK', RTRIM(LOCSTR) )
       
      IF ( LOC .GT. 0 ) THEN
C
C        It's a SCLK string.  Find the ID code, if we can.
C
         CALL SCN2ID ( LOCSTR(:LOC+3), CLKID, FND )
 
 
         IF ( .NOT. FND ) THEN
C
C           We don't recognize this SCLK type.
C
            ERROR  = .TRUE.
 
            IF ( LOC .GT. 1 ) THEN
 
               ERRMSG = 'Time conversion failed; SCLK type '//
     .                  '<#> was not recognized.'
 
               CALL REPMC ( ERRMSG, '#', TIMSTR(:LOC-1),  ERRMSG )

            ELSE

               ERRMSG = 'Time conversion failed; SCLK name was not ' //
     .                  'supplied.'
            END IF
 
            CALL CHKOUT ( 'ZZEKTCNV' )
            RETURN
 
         END IF
 
C
C        If we got this far, we recognized the SCLK type.
C        Convert the time to ET.
C
         CALL SCPARS ( CLKID, LOCSTR(LOC+4:), ERROR, SCLMSG, SCLKDP )
 
         IF ( FAILED() ) THEN
C
C           We'll arrive here if the required SCLK kernel hasn't 
C           been loaded.
C
            ERROR  =  .TRUE.
            ERRMSG =  'Unexpected SPICELIB error encountered '  //
     .                'while attempting to parse the string <'
            CALL SUFFIX ( TIMSTR, 0, ERRMSG )
            CALL SUFFIX ( '>',    0, ERRMSG )
            CALL CHKOUT ( 'ZZEKTCNV'        )
            RETURN
  
         ELSE IF ( ERROR ) THEN
 
            ERRMSG =  'The string <#> didn''t parse '                 //
     .                'as a spacecraft clock string.'
            CALL REPMC  ( ERRMSG, '#', TIMSTR, ERRMSG )
            CALL SUFFIX ( SCLMSG,  3,          ERRMSG )
            CALL CHKOUT ( 'ZZEKTCNV'                  )
            RETURN
 
         ELSE
 
            CALL SCT2E ( CLKID, SCLKDP, ET )

            IF ( FAILED() ) THEN
 
               ERROR  =  .TRUE.
               ERRMSG =  'Unexpected SPICELIB error encountered '  //
     .                   'while attempting to parse the string <'
               CALL SUFFIX ( TIMSTR, 0, ERRMSG )
               CALL SUFFIX ( '>',    0, ERRMSG )
               CALL CHKOUT ( 'ZZEKTCNV'        )
               RETURN
 
            END IF
 
         END IF
 
 
 
      ELSE
C
C        We could have a standard time string.  Make sure that the 
C        time string is acceptable before actually calling STR2ET.
C
         CALL TPARTV ( LOCSTR, 
     .                 TVEC,   NTVEC, TYPE, 
     .                 MODIFY, MODS,  YABBRV, SUCCES, 
     .                 PICTUR, ERRMSG                 )

         IF ( SUCCES ) THEN
C
C           It's safe to pass the string to STR2ET.
C
            CALL STR2ET ( LOCSTR, ET )

            IF ( FAILED() ) THEN
 
               ERROR  =  .TRUE.
               ERRMSG =  'Unexpected SPICELIB error encountered '  //
     .                   'while attempting to parse the string <'
               CALL SUFFIX ( TIMSTR, 0, ERRMSG )
               CALL SUFFIX ( '>',    0, ERRMSG )
               CALL CHKOUT ( 'ZZEKTCNV'        )
               RETURN
 
            END IF

         ELSE
C
C           The string cannot be parsed by STR2ET.  The error message
C           was set by TPARTV.
C 
            ERROR  =  .TRUE.
            CALL CHKOUT ( 'ZZEKTCNV' )
            RETURN

         END IF
C
C        We're done with the standard time string case.
C
      END IF
C
C     We've parsed a time string, if it was an SCLK or standard string.
C
      CALL CHKOUT ( 'ZZEKTCNV' )
      RETURN
      END
