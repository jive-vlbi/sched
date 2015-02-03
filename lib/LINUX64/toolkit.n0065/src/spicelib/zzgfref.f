C$Procedure ZZGFREF ( Private --- GF, update REFVAL )

      SUBROUTINE ZZGFREF( REFVAL )

C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Set reference value in the GF sub-system.
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
C     STORE_VALUE
C     GEOMETRY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzholdd.inc'

      DOUBLE PRECISION      REFVAL

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ZZPUT      P   ZZHOLDD stores a DP value.
C     GF_REF     P   ZZHOLDD acts on the reference value
C                    for a GF search.
C     REFVAL     I   The value to set as the reference value.
C
C$ Detailed_Input
C
C     REFVAL     the double precision scalar value to set as the
C                reference value for a geometry finder search.
C
C$ Detailed_Output
C
C    None.
C
C$ Parameters
C
C    None.
C
C$ Exceptions
C
C    None.
C
C$ Files
C
C    None.
C
C$ Particulars
C
C    This routine wraps a put call to ZZHOLDD for the GF_REF
C    value.
C
C$ Examples
C
C    None.
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
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.1.0, 06-AUG-2010 (EDW)
C
C       Change in ZZHOLDD functionality required edit to ZZHOLDD 
C       call and inclusion of "zzholdd.inc" parameters file.
C
C-   SPICELIB Version 1.0.0  28-NOV-2009 (EDW)
C
C-&

C$ Index_Entries
C
C    store a double precision reference value
C
C-&

C
C     Local Variables
C

      LOGICAL               OK

C
C     Store the REFVAL value for use in ZZGFUDLT.
C
      CALL ZZHOLDD ( ZZPUT, GF_REF, OK, REFVAL )

      END

