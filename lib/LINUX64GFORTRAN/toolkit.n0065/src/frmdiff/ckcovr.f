C$Procedure      CKCOVR ( CK coverage as ETs adjusted for round off )

      SUBROUTINE CKCOVR ( CK, IDCODE, NEEDAV, LEVEL, TOL, COVER )

C$ Abstract
C
C     Find the ET coverage window adjusted for SCLK -> ET -> SCLK
C     conversion round off for a specified object in a specified CK
C     file.
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
C     CELLS
C     DAF
C     CK
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     POINTING
C     TIME
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         CK
      INTEGER               IDCODE
      LOGICAL               NEEDAV
      CHARACTER*(*)         LEVEL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      COVER ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CK         I   Name of CK file.
C     IDCODE     I   ID code of object.
C     NEEDAV     I   Flag indicating whether angular velocity is needed.
C     LEVEL      I   Coverage level:  'SEGMENT' OR 'INTERVAL'.
C     TOL        I   Tolerance in ticks.
C     COVER      O   Window giving coverage for IDCODE.
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
C     FACTOR         is the factor to multiply the maximum round off
C                    value for contracting the output window.
C
C$ Exceptions
C
C     1) See exceptions signaled by CKCOV and SCT2E/SCE2C.
C
C$ Files
C
C     TBD.
C
C$ Particulars
C
C     This routine passed all inputs directly to CKCOV, gets SCLK
C     coverage window out of it, does SCLK -> ET -> SCLK conversion for
C     each interval endpoint, computes the difference between each
C     source and resulting SCLK, saves the maximum difference,
C     contracts SCLK window by this difference multiplied by a factor,
C     and then convert the resulting SCLK to ET for output.
C
C     It is possible that a value very near an endpoint would have a
C     greater roundoff than the roundoff at the endpoint. This routine
C     does not attempt to adjust coverage for such cases.
C
C$ Examples
C
C     TBD.
C
C$ Restrictions
C
C     TBD.
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
C-    SPICELIB Version 1.1.0, 13-MAR-2012 (BVS)
C
C        Increased FACTOR from 2.D0 to 3.D0.
C
C-    SPICELIB Version 1.0.0, 09-JUL-2008 (BVS)
C
C-&

C$ Index_Entries
C
C     get coverage window adjusted for roundoff for ck object
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               WNCARD

C
C     Local parameters
C
      DOUBLE PRECISION      FACTOR
      PARAMETER           ( FACTOR = 3.D0 )

C
C     Local variables.
C
      DOUBLE PRECISION      MAXDIF
      DOUBLE PRECISION      HDP1
      DOUBLE PRECISION      HDP2

      INTEGER               I
      INTEGER               SCLKID

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'CKCOVR' )

C
C     Pass all inputs directly to CKCOV to get SCLK coverage window.
C
      CALL CKCOV ( CK, IDCODE, NEEDAV, LEVEL, TOL, 'SCLK', COVER )

C
C     Get spacecraft ID that will be used for time conversions.
C
      CALL CKMETA( IDCODE, 'SCLK', SCLKID )

C
C     Convert each SCLK to ET, then back to SCLK and compute
C     roundoff. Save maximum round off.
C
      MAXDIF = 0.D0

      DO I = 1, 2 * WNCARD( COVER )

         CALL SCT2E( SCLKID, COVER(I), HDP1 )
         CALL SCE2C( SCLKID, HDP1, HDP2 )

         MAXDIF = MAX( MAXDIF, DABS( COVER(I) - HDP2 ) )

      END DO

C
C     If there is a roundoff, contract window on each side by factor *
C     roundoff value.
C
      IF ( MAXDIF .NE. 0.D0 ) THEN
         CALL WNCOND( MAXDIF * FACTOR, MAXDIF * FACTOR, COVER )
      END IF

C
C     Convert SCLK window to ET.
C
      DO I = 1, 2 * WNCARD( COVER )
         CALL SCT2E( SCLKID, COVER(I), HDP1 )
         COVER(I) = HDP1
      END DO

C
C     All done.
C
      CALL CHKOUT ( 'CKCOVR' )
      RETURN
      END
