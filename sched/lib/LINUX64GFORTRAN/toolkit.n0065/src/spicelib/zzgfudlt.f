C$Procedure ZZGFUDLT ( Private --- GF, scalar function < ref value )

      SUBROUTINE ZZGFUDLT ( UDFUNC, ET, ISLESS )

C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     This routine determines if the value of the scalar quantity
C     function is less than a previously defined reference value.
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
C     GF
C
C$ Keywords
C
C     GEOMETRY
C     MATH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzholdd.inc'

      EXTERNAL              UDFUNC

      DOUBLE PRECISION      ET
      LOGICAL               ISLESS

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ZZGET      P   ZZHOLDD retrieves a stored DP value.
C     GF_REF     P   ZZHOLDD acts on the reference value
C                    for a GF search.
C     UDFUNC     I   Name of the routine that computes the scalar value
C                    of interest at ET.
C     ET         I   Time in TDB seconds for which to evaluate UDFUNC.
C     ISLESS     O   Boolean indicating if the scalar value is less than
C                    reference value.
C
C$ Detailed_Input
C
C     UDFUNC     the routine that returns the value of the scalar
C                quantity of interest at time ET. The calling sequence
C                for UDFUNC is:
C
C                   CALL UDFUNC ( ET, VALUE )
C
C                where:
C
C                   ET      a double precision value representing
C                           ephemeris time, expressed as seconds past
C                           J2000 TDB, at which to determine the scalar
C                           value.
C
C                   VALUE   is the value of the geometric quantity
C                           at ET.
C
C     ET         a double precision value representing ephemeris time,
C                expressed as seconds past J2000 TDB at which to
C                determine the value of UDFUNC.
C
C$ Detailed_Output
C
C     ISLESS     a scalar boolean indicating if the value of UDFUNC at
C                ET is less than REFVAL (true) or not (false).
C
C                 Functionally:
C
C                   ISLESS = UDFUNC( ET )  <  REFVAL
C
C$ Parameters
C
C    None.
C
C$ Exceptions
C
C     1) ZZHOLDD will signal the error SPICE(ZZHOLDNOPUT) if this
C        routine is called prior to storing a reference value
C        using a ZZHOLDD "PUT" operation.
C
C$ Files
C
C    None.
C
C$ Particulars
C
C     A ZZHOLDD "PUT" stored the reference value used in the logical
C     operation. A ZZHOLDD "GET" retrieves the value.
C
C$ Examples
C
C    See GFUDS.
C
C$ Restrictions
C
C    None.
C
C$ Literature_References
C
C    None.
C
C$ Author_and_Institution
C
C    N.J. Bachman   (JPL)
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.1.0, 06-AUG-2010 (EDW)
C
C       Change in ZZHOLDD functionality required edit to ZZHOLDD
C       call and inclusion of "zzholdd.inc" parameters file.
C
C-   SPICELIB Version 1.0.0, 16-FEB-2010 (EDW)
C
C-&

C$ Index_Entries
C
C    function less than reference value
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Local Variables
C
      DOUBLE PRECISION      UDVAL
      DOUBLE PRECISION      REFVAL

      LOGICAL               OK


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFUDLT' )

C
C     Default value of false for ISLESS.
C
      ISLESS = .FALSE.

C
C     Call the routine, return the scalar value corresponding to ET.
C
      CALL UDFUNC ( ET, UDVAL )

C
C     Check for an error, return if found.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFUDLT' )
         RETURN
      END IF

C
C     Retrieve the stored reference value. Signal an error if a 
C     get call fails.
C
      CALL ZZHOLDD ( ZZGET, GF_REF, OK, REFVAL )

      IF ( .NOT. OK ) THEN
         CALL SETMSG ( 'ZZHOLDD GET failed. This indicates a logic '
     .           //    'error in the GF code due either to a failure '
     .           //    'to store the GF reference value or a post '
     .           //    'store reset of ZZHOLDD.' )
         CALL SIGERR ( 'SPICE(ZZHOLDDGETFAILED)' )
         CALL CHKOUT ( 'ZZGFUDLT'                )
         RETURN
      END IF

      ISLESS = UDVAL .LT. REFVAL

      CALL CHKOUT ( 'ZZGFUDLT' )
      RETURN

      END



