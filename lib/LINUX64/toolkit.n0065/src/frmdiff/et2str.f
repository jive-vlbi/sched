C$Procedure      ET2STR ( Convert ET to String for FRMDIFF Output )

      SUBROUTINE ET2STR ( ET, TIMFMT, SCLKID, SIGDIG, STRING )

C$ Abstract
C
C     Converts ET to a string for FRMDIFF output.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'frmdiff.inc'

      DOUBLE PRECISION      ET
      CHARACTER*(*)         TIMFMT
      INTEGER               SCLKID
      INTEGER               SIGDIG
      CHARACTER*(*)         STRING

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   ET time
C     TIMFMT     I   Output format (per FRMDIFF spec)
C     SCLKIF     I   SCLK ID
C     SIGDIG     I   Number of significant digits
C     STRING     O   Output time string
C
C$ Detailed_Input
C
C     TBD.
C
C$ Detailed_Output
C
C     TBD.
C
C$ Parameters
C
C     None
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     TBD.
C
C$ Examples
C
C     None.
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
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Version 2.0.0, 27-FEB-2012 (BVS)
C
C        Changed the calling sequence to include additional input
C        SIGDIG.
C
C        Updated to use SIGDIG instead of 14 to specify the number of
C        significant digits in numeric times.
C
C        Replaced calls to SPICELIB's DPSTR with calls to SUPPORT's
C        DPSTRE or local DPSTRP, both of which do not limit the number
C        of significant digits to 14.
C
C-    Version 1.0.0, 30-AUG-2008 (BVS)
C
C        Initial version.
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               EQSTR

C
C     Local variables
C
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      SCLKD

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ET2STR' )
      END IF

C
C     Convert ET to string depending on specified type.
C
      IF      ( EQSTR( TIMFMT, ETVAL ) ) THEN

C
C        Output string should contain ET seconds. Use DPSTRP.
C
         CALL DPSTRP ( ET, SIGDIG, STRING )

      ELSE IF ( EQSTR( TIMFMT, SCTVAL ) ) THEN

C
C        Output string should contain SCLK ticks. Use SCE2C and
C        DPSTRE.
C
         CALL SCE2C ( SCLKID, ET, TICKS )
         CALL DPSTRE( TICKS, SIGDIG, STRING )

      ELSE IF ( EQSTR( TIMFMT, SCSVAL ) ) THEN

C
C        Output string should contain SCLK string. Use SCE2S.
C
         CALL SCE2S( SCLKID, ET, STRING )

      ELSE IF ( EQSTR( TIMFMT, SCDVAL ) ) THEN

C
C        Output string should contain decimal SCLK. Convert ET to SCLK
C        string, then string to decimal form, and package decimal form
C        back into a string :).
C
         CALL SCE2S   ( SCLKID, ET, STRING )
         CALL SC01S2D ( SCLKID, STRING, SCLKD )
         CALL DPSTRE  ( SCLKD, SIGDIG, STRING )

      ELSE

C
C        Output string should be set by TIMOUT using provided TIMFMT.
C
         CALL TIMOUT( ET, TIMFMT, STRING )

      END IF

C
C     All done.
C
      CALL CHKOUT ( 'ET2STR' )

      RETURN

      END
