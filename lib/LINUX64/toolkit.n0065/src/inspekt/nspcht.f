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
      SUBROUTINE NSPCHT ( FORMAT, WIDTH )
C
C-    Version 2.2.0  29-FEB-2000 (WLT)
C
C        Added the call to SCTRAN to handle more SCLKs than just
C        MO and GLL.
C
C-    Version 2.1.0  22-MAY-1997 (WLT)
C
C        Balanced calls to CHKIN/CHKOUT.
C
C-    Version 2.0.0  22-MAY-1997 (WLT)
C
C        Needed to fix the damage caused by replacing DPFMT_1 by
C        DPFMT.  The format picture used in the creation of the
C        width for SCLKS had to be made large enough to hold
C        the modulus of a field.
C
C-    Version 1.1.0  09-JAN-1997 (WLT)
C
C       Replaced call to DPFMT_1 with call to DPFMT.
C
C-    Version 1.0.0  21-AUG-1995 (WLT)
C
C        Fixed the text in an error message.
C
C     This routine accepts a strint that is a potential time format.
C     Checks it and if it is passable, returns the width that
C     would be associated with that format.
C
      IMPLICIT NONE
 
      CHARACTER*(*)         FORMAT
      INTEGER               WIDTH
 
 
C
C     Spicelib functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
      LOGICAL               EQSTR
 
C
C     Local Variables
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 32 )
 
      CHARACTER*(WDSIZE)    GETFLD
      CHARACTER*(WDSIZE)    GETMOD
      CHARACTER*(WDSIZE)    KERVAR
 
      CHARACTER*(128)       MYSTR
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MODULI ( 10 )
      DOUBLE PRECISION      NCMP
 
      INTEGER               I
      INTEGER               ID
      INTEGER               N
 
 
      LOGICAL               FOUND
      LOGICAL               SCLK
 
C
C     Is the desired output an SCLK string? If it is, which spacecraft
C     are we dealing with?
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'NSPCHT' )
 
 
      IF ( EQSTR( FORMAT, 'MOSCLK' ) ) THEN
 
         ID   = -94
         SCLK = .TRUE.
 
      ELSE IF ( EQSTR( FORMAT, 'GLLSCLK' ) ) THEN
 
         ID   = -77
         SCLK = .TRUE.
 
      ELSE
 
         CALL SCN2ID ( FORMAT, ID, SCLK )
 
      END IF
 
C
C     Set up an empty string for use in determining the lenght
C     associated with the input format.
C
      MYSTR = ' '
C
C     If we want an SCLK string, check to see if an SCLK kernel has been
C     loaded. If not, signal an error, otherwise compute the SCLK
C     string.
C
      IF ( SCLK ) THEN
 
         KERVAR = 'SCLK_DATA_TYPE_#'
         CALL REPMI  ( KERVAR, '#', -ID, KERVAR )
 
         CALL EXPOOL ( KERVAR, FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
            CALL SETMSG ( 'An SCLK kernel file for # was not '
     .      //            'loaded. You will need to load one '
     .      //            'before this time format can be used.'      )
            CALL ERRCH  ( '#', FORMAT(1:RTRIM(FORMAT))                )
            CALL SIGERR ( 'SPICE(KERNELNOTLOADED)'                    )
            CALL CHKOUT (  'NSPCHT' )
            RETURN
 
         ELSE
 
            GETFLD = 'SCLK01_N_FIELDS_' // KERVAR(16:)
            GETMOD = 'SCLK01_MODULI_'   // KERVAR(16:)
 
            CALL RTPOOL ( GETFLD, N, NCMP,   FOUND )
            CALL RTPOOL ( GETMOD, N, MODULI, FOUND )
C
C           The format of an SCLK string has the form
C
C           pn/ xxxxxx#xxxxx# ... #xxxxx
C
C           where pn is the partition number and the x's are the
C           integer components of each field.  The '#' character
C           is used to separate fields and may be a period, colon
C           and so on but it is one character wide. Thus there
C           are 4 (for the partition and following space) plus NCMP - 1
C           markers plus the widths of the individual fields.
C
            WIDTH =  3 + INT(NCMP)
 
            DO I = 1, N
               CALL DPFMT ( MODULI(I), 'XXXXXXXXXXXX', MYSTR )
               CALL LJUST ( MYSTR,                     MYSTR )
               WIDTH =  WIDTH + RTRIM(MYSTR)
               MYSTR = ' '
            END DO
 
            CALL CHKOUT ( 'NSPCHT' )
            RETURN
 
         END IF
 
      ELSE
C
C        We have a "normal" time string conversion to compute. Check
C        to see if the leapseconds kernel file has been loaded. If not,
C        it's an error.
C
         CALL EXPOOL ( 'DELTET/DELTA_AT', FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
               CALL SETMSG ( 'The leapseconds kernel file was not '
     .         //            'loaded.'                                )
               CALL SIGERR ( 'SPICE(NOKERNELLOADED)'                  )
               CALL CHKOUT ( 'NSPCHT'                                 )
               RETURN
 
         END IF
 
         ET = 1.0D-8
C
C        For some requested time formats we'll just use a format that
C        we know is correct.
C
         IF ( EQSTR( FORMAT, 'UTC' )) THEN
 
              CALL TIMOUT ( ET, 'YYYY-MON-DD HR:MN:SC ::RND', MYSTR )
 
         ELSE IF ( EQSTR( FORMAT, 'JED' )) THEN
 
              CALL TIMOUT ( ET, 'JD.##### ::TDB ::RND', MYSTR )
 
         ELSE IF ( EQSTR( FORMAT, 'ISO' )) THEN
 
              CALL TIMOUT ( ET, 'YYYY-MM-DDTHR:MN:SC ::RND', MYSTR )
 
         ELSE IF ( EQSTR( FORMAT, 'ISODOY' )) THEN
 
              CALL TIMOUT ( ET, 'YYYY-DOYTHR:MN:SC ::RND', MYSTR )
 
         ELSE
 
              CALL TIMOUT ( ET, FORMAT, MYSTR )
 
         END IF
 
 
         CALL    LJUST ( MYSTR, MYSTR )
         WIDTH = RTRIM ( MYSTR        )
 
      END IF
 
      CALL CHKOUT ( 'NSPCHT' )
      RETURN
 
      END
