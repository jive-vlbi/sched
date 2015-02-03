C$Procedure      ZZBODBRY ( Return barycenter code for a body )
 
      INTEGER FUNCTION ZZBODBRY ( BODY )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the barycenter code associated with a body belonging to
C     a planetary system.  For other bodies, simply return the
C     input ID code.
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
C     NAIF_IDS
C
C$ Keywords
C
C     PRIVATE
C     UTILITY
C
C$ Declarations
 
      INTEGER               BODY
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   ID code of body.
C
C     The function returns the NAIF integer ID code of the barycenter,
C     if any, associated with BODY.
C
C$ Detailed_Input
C
C      BODY        is the integer ID code of the body for which the
C                  barycenter ID code is requested.
C
C$ Detailed_Output
C
C     The function returns the NAIF integer ID code of the barycenter,
C     if any, associated with BODY.  If BODY is not the NAIF integer
C     ID code of a planet or satellite, the value BODY is returned.
C
C     Planetary barycenter codes are the integers 1, ..., 9.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If BODY is not the NAIF integer ID code of a planet or
C        satellite, the value BODY is returned.  This case is 
C        not considered to be an error.
C
C     2) Codes of the form
C
C           PXNNN, where 
C
C                  P   is    1, ...,  9,
C                  X   is    1, 2, 3, 4, 6, 7, 8, 9
C              and NNN is  001, ... 999
C
C        are mapped to the integer P.  These codes are not
C        considered to be erroneous, though they were not 
C        part of the planned satellite numbering scheme at
C        the date this routine was released.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine enables the caller to determine to which 
C     planetary system, if any, a planet or natural satellite belongs.
C     This capability is used by the SPICELIB PCK subsystem.
C
C     Planets have ID codes of the form P99, where P is 1, ..., 9.
C
C     Natural satellites have ID codes of the form
C
C        PNN, where 
C
C               P  is  1, ..., 9 
C           and NN is 01, ... 98
C
C     or
C
C        PXNNN, where 
C
C               P   is    1, ...,  9,
C               X   is    0  or    5,
C           and NNN is  001, ... 999
C
C     Codes with X = 5 are provisional.
C
C$ Examples
C
C     1) Find the planetary system (indicated by a barycenter ID
C        code) associated with the ID code 65001 (a provisional code
C        for a Saturnian satellite):
C
C            BODY = 65001
C            BARY = ZZBODBRY ( BODY )
C
C        BARY is assigned the value 6.
C
C     2) Find the planetary system associated with the ID code
C        60001 (an "extended" code for a Saturnian satellite):
C
C            BODY = 60001
C            BARY = ZZBODBRY ( BODY )
C
C        BARY is assigned the value 6.
C
C     3) Find the planetary system associated with the ID code
C        606 (Titan):
C
C            BODY = 606
C            BARY = ZZBODBRY ( BODY )
C
C        BARY is assigned the value 6.
C
C     4) Find the planetary system associated with the ID code
C        699 (Saturn):
C
C            BODY = 699
C            BARY = ZZBODBRY ( BODY )
C
C        BARY is assigned the value 6.
C
C     5) Find the planetary system associated with the ID code 6
C        (Saturn system barycenter):
C
C            BODY = 6
C            BARY = ZZBODBRY ( BODY )
C
C        BARY is assigned the value 6.
C
C     6) Find the planetary system associated with the ID code
C        9511010 (asteroid Gaspra):
C
C            BODY = 9511010
C            BARY = ZZBODBRY ( BODY )
C
C        BARY is assigned the value 9511010.
C
C$ Restrictions
C
C     1) This routine should not be called from routines outside
C        of SPICELIB.  The interface and functionality may change
C        without notice.
C
C$ Literature_References
C
C     None.
C 
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-FEB-2004 (NJB)
C
C-&


      IF (  ( BODY .GE. 100 ) .AND. ( BODY .LE. 999 )  ) THEN
C
C        BODY is a "traditional" NAIF planet or natural satellite
C        ID code.
C
         ZZBODBRY = BODY / 100

      ELSE IF (  ( BODY .GE. 10000 ) .AND. ( BODY .LE. 99999 )  ) THEN
C
C        BODY is an "extended" NAIF natural satellite ID code.
C
         ZZBODBRY = BODY / 10000

      ELSE
C
C        BODY is a barycenter code or is not associated with a 
C        planetary system.  In either case, we simply return
C        the input value BODY.
C        
         ZZBODBRY = BODY

      END IF

      RETURN
      END
