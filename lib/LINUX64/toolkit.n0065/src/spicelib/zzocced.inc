C$ Abstract
C
C     Declare ZZOCCED return code parameters, comparison strings
C     and other parameters.
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
C     ELLIPSOID
C     GEOMETRY
C     GF
C     OCCULTATION
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
C-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB)
C
C-&


C     The function returns an integer code indicating the geometric
C     relationship of the three bodies.   
C
C     Codes and meanings are:
C
C        -3                    Total occultation of first target by 
C                              second.
C
      INTEGER               TOTAL1
      PARAMETER           ( TOTAL1 = -3 )

C
C        -2                    Annular occultation of first target by 
C                              second.  The second target does not
C                              block the limb of the first.
C
      INTEGER               ANNLR1
      PARAMETER           ( ANNLR1 = -2 )

C
C        -1                    Partial occultation of first target by
C                              second target.
C
      INTEGER               PARTL1
      PARAMETER           ( PARTL1 = -1 )

C
C         0                    No occultation or transit:  both objects
C                              are completely visible to the observer.
C
      INTEGER               NOOCC
      PARAMETER           ( NOOCC  =  0 )

C
C         1                    Partial occultation of second target by
C                              first target.
C
      INTEGER               PARTL2
      PARAMETER           ( PARTL2 =  1 )

C
C         2                    Annular occultation of second target by 
C                              first.
C
      INTEGER               ANNLR2
      PARAMETER           ( ANNLR2 =  2 )

C
C         3                    Total occultation of second target by 
C                              first.  
C
      INTEGER               TOTAL2
      PARAMETER           ( TOTAL2 =  3 )

C
C     End include file zzocced.inc
C

