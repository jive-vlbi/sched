C$ Abstract
C
C     Declare parameters specific to SPK type 2.
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
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-DEC-2013 (NJB)
C
C-&

C     MAXDEG         is the maximum allowed degree of the input
C                    Chebyshev expansions. If the value of MAXDEG is
C                    increased, the SPICELIB routine SPKPVN must be
C                    changed accordingly. In particular, the size of
C                    the record passed to SPKRnn and SPKEnn must be
C                    increased, and comments describing the record size
C                    must be changed.
C
C                    The record size requirement is
C
C                       MAXREC = 3 * ( MAXDEG + 2 )  
C
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 27 )


C
C     TOLSCL         is a tolerance scale factor (also called a
C                    "relative tolerance") used for time coverage
C                    bound checking. TOLSCL is unitless. TOLSCL
C                    produces a tolerance value via the formula
C
C                       TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )
C
C                    where FIRST and LAST are the coverage time bounds
C                    of a type 2 segment, expressed as seconds past
C                    J2000 TDB.
C
C                    The resulting parameter TOL is used as a tolerance
C                    for comparing the input segment descriptor time
C                    bounds to the first and last epoch covered by the
C                    sequence of time intervals defined by the inputs
C                    to SPKW02:
C
C                       BTIME
C                       INTLEN
C                       N

C
C     Tolerance scale for coverage gap at the endpoints
C     of the segment coverage interval:
C
      DOUBLE PRECISION      TOLSCL
      PARAMETER           ( TOLSCL = 1.D-13 )

C
C     End of include file spk02.inc.
C     
