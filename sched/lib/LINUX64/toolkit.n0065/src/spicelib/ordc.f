C$Procedure   ORDC ( The ordinal position of an element in a set )
 
      INTEGER FUNCTION ORDC ( ITEM, SET )
 
C$ Abstract
C
C     The function returns the ordinal position of any given item in a
C     set.  If the item does not appear in the set, the function returns
C     zero.
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
C     SETS
C
C$ Keywords
C
C     SEARCH
C     SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = - 5 )
 
      CHARACTER*(*)         ITEM
      CHARACTER*(*)         SET ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   An item to locate within a set.
C     SET        I   A set to search for a given item.
C
C     The function returns the ordinal position of ITEM within the SET.
C
C$ Detailed_Input
C
C     ITEM      Is an string to be located within a character set.
C
C     SET       Is a properly validated SPICELIB set that is to be
C               searched for the occurrence of item.
C
C$ Detailed_Output
C
C     The function returns the ordinal position of ITEM within SET.
C     If ITEM is not an element of SET, the function is returned as 0.
C
C$ Parameters
C
C      None.
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
C     A natural ordering can be imposed upon the elements of any
C     SPICELIB set, be it INTEGER, CHARACTER or DOUBLE PRECISION.  For
C     character strings the ASCII collating sequence serves as the
C     ordering relation, for DOUBLE PRECISION and INTEGER variables
C     the arithmetic ordering is used.
C
C     Given any element of a set, its location within this ordered
C     sequence of elements is called its ordinal position within
C     the set.
C
C     For illustrative purposes suppose that SET represents the set
C
C              { 8, 1, 2, 9, 7, 4, 10 }
C
C     The ordinal position of:     8 is 5
C                                  1 is 1
C                                  2 is 2
C                                  9 is 6
C                                  7 is 4
C                                  4 is 3
C                                 10 is 7
C
C     Given an item of the SET, this routine returns its ordinal
C     position.  If the item is not in the set, this function returns
C     a value of 0.
C
C$ Examples
C
C     Suppose that you wished to find the relative position of a value
C     in a large list of values stored within an array.  Say we want
C     to know the relative position of item I of ARRAY withing the
C     set of values represented in ARRAY.
C
C     The following sequence of subroutine calls would allow you
C     determine the relative position of the value ARRAY(I).
C
C     INTEGER               N
C     PARAMETER           ( N = something useful )
C
C     CHARACTER*(*)         ARRAY (         N )
C     CHARACTER*(*)         SET   ( LBCELL: N )
C     INTEGER               I
C
C     INTEGER               NVALID
C     INTEGER               POSITION
C
C
C     set the value of NVALID to be the number of valid elements in the
C     array ARRAY
C
C     CALL MOVEC  ( ARRAY, N,      SET(1) )
C     CALL VALIDC ( N,     NVALID, SET    )
C
C     POSITION = ORDC ( ARRAY(I), SET )
C
C     POSITION now contains the ordinal position of ARRAY(I) within the
C     values represented in the array.
C
C$ Restrictions
C
C     SET must be a validated or empty set.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     C.A. Curzon     (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
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
C     the ordinal position of an element in a set
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
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
 
 
C
C     Standard error handling:
C
      IF ( RETURN() ) THEN
         ORDC = 0
         RETURN
      ELSE
         CALL CHKIN ( 'ORDC' )
      END IF
 
C
C     Given the structure of sets, there's not much to do.
C
      ORDC = BSRCHC ( ITEM, CARDC(SET), SET(1) )
 
 
      CALL CHKOUT ( 'ORDC' )
      RETURN
      END
