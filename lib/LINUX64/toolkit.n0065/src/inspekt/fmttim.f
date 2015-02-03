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
 
C
C     This routine formats time strings.
C
C-    Version 2.0.0  29-FEB-2000 (WLT)
C
C        Added the call to SCTRAN to handle more SCLKs than just
C        MO and GLL.
C
 
      SUBROUTINE FMTTIM ( ET, FORMAT, STRING )
      IMPLICIT NONE
 
 
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FORMAT
      CHARACTER*(*)         STRING
 
 
      INTEGER               RTRIM
      LOGICAL               EQSTR
 
 
      LOGICAL               SCLK
      LOGICAL               FOUND
 
      INTEGER               ID
      INTEGER               I
      INTEGER               OUTLEN
 
      CHARACTER*(80)        KERVAR
 
      CHARACTER*(132)       TMPSTR
 
 
 
      CALL CHKIN ( 'FMTTIM' )
 
C
C     Is the desired output an SCLK string? If it is, which spacecraft
C     are we dealing with?
C
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
C     If we want an SCLK string, check to see if an SCLK kernel has been
C     loaded. If not, signal an error, otherwise compute the SCLK
C     string.
C
      IF ( SCLK ) THEN
 
         KERVAR = 'SCLK_DATA_TYPE_#'
         CALL REPMI  ( KERVAR, '#', -ID, KERVAR )
 
         CALL EXPOOL ( KERVAR, FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
            CALL SETMSG ( 'The SCLK kernel file for body # was not ' //
     .                    'loaded. Please load it. '                  )
            CALL ERRINT ( '#', ID                                     )
            CALL SIGERR ( 'SPICE(KERNELNOTLOADED)'                    )
            CALL CHKOUT ( 'FMTTIM'                                    )
            RETURN
 
         ELSE
 
            CALL   SCE2S ( ID, ET, TMPSTR )
            CALL   LJUST ( TMPSTR, TMPSTR )
            OUTLEN = LEN ( STRING )
 
            IF ( RTRIM( TMPSTR ) .GT. OUTLEN ) THEN
               DO I = 1, OUTLEN
                  STRING(I:I) = '*'
               END DO
            ELSE
               STRING = TMPSTR
            END IF
 
 
         END IF
 
      ELSE
C
C        We have a "normal" time string conversion to compute. Check
C        to see if the leapseconds kernel file has been loaded. If not,
C        it's an error.
C
         CALL EXPOOL ( 'DELTET/DELTA_AT', FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
               CALL SETMSG ( 'The leapseconds kernel file was not '  //
     .                       'loaded.'                                )
               CALL SIGERR ( 'SPICE(NOKERNELLOADED)'                  )
               CALL CHKOUT ( 'FMTTIM'                                 )
               RETURN
 
         END IF
 
C
C        For some requested time formats we'll just use a format that
C        we know is correct.
C
         IF ( EQSTR( FORMAT, 'UTC' )) THEN
 
              CALL TIMOUT ( ET, 'YYYY-MON-DD HR:MN:SC ::RND', TMPSTR )
 
         ELSE IF ( EQSTR( FORMAT, 'JED' )) THEN
 
              CALL TIMOUT ( ET, 'JD.##### ::TDB ::RND', TMPSTR )
 
         ELSE IF ( EQSTR( FORMAT, 'ISO' )) THEN
 
              CALL TIMOUT ( ET, 'YYYY-MM-DDTHR:MN:SC ::RND', TMPSTR )
 
         ELSE IF ( EQSTR( FORMAT, 'ISODOY' )) THEN
 
              CALL TIMOUT ( ET, 'YYYY-DOYTHR:MN:SC ::RND', TMPSTR )
 
         ELSE
 
              CALL TIMOUT ( ET, FORMAT, TMPSTR )
 
         END IF
 
 
         CALL LJUST  ( TMPSTR, TMPSTR )
         OUTLEN = LEN( STRING )
 
         IF ( RTRIM( TMPSTR ) .GT. OUTLEN ) THEN
            DO I = 1, OUTLEN
               STRING(I:I) = '*'
            END DO
         ELSE
            STRING = TMPSTR
         END IF
 
      END IF
 
 
 
      CALL CHKOUT ( 'FMTTIM' )
      RETURN
      END
 
 
 
 
