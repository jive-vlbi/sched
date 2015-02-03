C$ Abstract
C
C     This file contains public, global parameter declarations
C     for the SPICELIB occultation routines.
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
C     ELLIPSOID
C     GEOMETRY
C     OCCULTATION
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     S.C. Krening      (JPL)
C     N.J. Bachman      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 24-JAN-2012 (SCK) (NJB)
C
C-&

C
C     The following integer codes indicate the geometric relationship
C     of the three bodies.
C
C     The meaning of the sign of each code is given below.
C
C                    Code sign          Meaning
C                    ---------          ------------------------------
C                       > 0             The second ellipsoid is
C                                       partially or fully occulted
C                                       by the first.
C
C                       < 0             The first ellipsoid is 
C                                       partially of fully
C                                       occulted by the second.
C
C                       = 0             No occultation.
C
C     The meanings of the codes are given below. The variable names
C     indicate the type of occultation and which target is in the back.
C     For example, TOTAL1 represents a total occultation in which the
C     first target is in the back (or occulted by) the second target.
C
C                    Name      Code     Meaning
C                    ------    -----    ------------------------------
C                    TOTAL1     -3      Total occultation of first
C                                       target by second.
C
C                    ANNLR1     -2      Annular occultation of first
C                                       target by second.  The second
C                                       target does not block the limb
C                                       of the first.
C
C                    PARTL1     -1      Partial occultation of first
C                                       target by second target.
C
C                    NOOCC       0      No occultation or transit:  both
C                                       objects are completely visible
C                                       to the observer.
C
C                    PARTL2      1      Partial occultation of second
C                                       target by first target.
C
C                    ANNLR2      2      Annular occultation of second
C                                       target by first.
C
C                    TOTAL2      3      Total occultation of second
C                                       target by first.
C


      INTEGER               TOTAL1
      PARAMETER           ( TOTAL1 = -3 )

      INTEGER               ANNLR1
      PARAMETER           ( ANNLR1 = -2 )

      INTEGER               PARTL1
      PARAMETER           ( PARTL1 = -1 )

      INTEGER               NOOCC
      PARAMETER           ( NOOCC  =  0 )

      INTEGER               PARTL2
      PARAMETER           ( PARTL2 =  1 )

      INTEGER               ANNLR2
      PARAMETER           ( ANNLR2 =  2 )

      INTEGER               TOTAL2
      PARAMETER           ( TOTAL2 =  3 )

C
C     End include file occult.inc
C
