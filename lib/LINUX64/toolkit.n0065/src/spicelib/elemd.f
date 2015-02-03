C$Procedure            ELEMD ( Element of a double precision set )
 
      LOGICAL FUNCTION ELEMD ( ITEM, A )
 
C$ Abstract
C
C      Determine whether an item is an element of a double
C      precision set.
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
C      SETS
C
C$ Keywords
C
C      CELLS, SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION ITEM
      DOUBLE PRECISION A      ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ITEM       I   Item to be tested.
C      A          I   Set to be tested.
C
C      The function returns TRUE if ITEM is an element of set A.
C
C$ Detailed_Input
C
C      ITEM        is an item which may or may not be an element of
C                  the input set.
C
C
C      A           is a set.
C
C
C$ Detailed_Output
C
C      The function returns TRUE if ITEM is a member of the set A,
C      and returns FALSE otherwise.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      The LOGICAL functions ELEMC and ELEMI correspond to the
C      set operator IN in the Pascal language.
C
C$ Examples
C
C      Let the character sets PLANETS and ASTEROIDS contain the
C      following elements.
C
C            PLANETS            ASTEROIDS
C            --------           ----------
C            'Earth'            'Apollo'
C            'Mars'             'Ceres'
C            'Pluto'
C            'Venus'
C
C      Then all of the following expressions are true.
C
C            ELEMC ( 'Earth',  PLANETS   )
C            ELEMC ( 'Pluto',  PLANETS   )
C            ELEMC ( 'Ceres',  ASTEROIDS )
C
C      And all of the following expressions are false.
C
C            ELEMC ( 'Saturn', PLANETS   )
C            ELEMC ( 'Pluto',  ASTEROIDS )
C            ELEMC ( 'CERES',  ASTEROIDS )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      None.
C
C$ Files
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      C.A. Curzon     (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C       If the value of the function RETURN is TRUE upon execution of
C       this module, this function is assigned a default value of
C       either 0, 0.0D0, .FALSE., or blank depending on the type of the
C       function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     element of a d.p. set
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
C
C        Now participates in error handling.  References to RETURN,
C        CHKIN, and CHKOUT added.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHD
      INTEGER               CARDD
      LOGICAL               RETURN
 
 
C
C     Standard error handling:
C
      IF ( RETURN() ) THEN
         ELEMD = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'ELEMD' )
      END IF
 
C
C     Just a binary search.
C
      ELEMD = ( BSRCHD ( ITEM, CARDD ( A ), A(1) ) .NE. 0 )
 
      CALL CHKOUT ( 'ELEMD' )
      RETURN
      END
