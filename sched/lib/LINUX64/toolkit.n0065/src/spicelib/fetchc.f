C$Procedure            FETCHC ( Fetch from a character set )
 
      INTEGER FUNCTION FETCHC ( NTH, SET )
 
C$ Abstract
C
C      Returns the location within the set array of the NTH element
C      within the order imposed by the ASCII collating sequence.
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
C      None.
C
C$ Keywords
C
C      SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               NTH
      CHARACTER*(*)         SET      ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NTH        I   Index of a particular element.
C      SET        I   Input set.
C
C      The function returns the location of the NTH element in the set.
C
C$ Detailed_Input
C
C      NTH         is an index to an element of a set.  If the set is to
C                  be conceived as sorted in increasing order, then the
C                  NTH element of a set is well defined.
C
C      SET         is a set.
C
C
C$ Detailed_Output
C
C      The function returns the location within the set array of the
C      NTH element within the order imposed by the ASCII collating
C      sequence. Thus, a set may be traversed in order:
C
C         SET( FETCHC ( 1 ) )
C         SET( FETCHC ( 2 ) )
C          .
C          .
C         SET( FETCHC ( CARDC ( SET ) ) )
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Within a set, the elements may be stored in arbitrary
C      order. The elements of a set may be retrieved by stepping
C      through the set array:
C
C         SET( 1 )
C         SET( 2 )
C          .
C          .
C         SET( CARDC ( SET ) )
C
C      Likewise, the elements may be retreived in the order imposed by
C      the ASCII collating sequence, by using FETCHC:
C
C         SET( FETCHC ( 1, SET ) )
C         SET( FETCHC ( 2, SET ) )
C          .
C          .
C         SET( FETCHC ( CARDC ( SET ), SET ) )
C
C      In general, FETCHC ( I, SET ) is not equal to I.
C
C$ Examples
C
C      Let SET contain the following elements.
C
C         'Feynman'
C         'Einstein'
C         'Bohr'
C         'Newton'
C
C      Then the code fragment
C
C         DO I = 1, CARDC ( SET )
C            WRITE (*,*) SET(FETCHC(I,SET))
C         END DO
C
C      always produces the following output.
C
C         Bohr
C         Einstein
C         Feynman
C         Newton
C
C      The code fragment
C
C         DO I = 1, CARDC ( SET )
C            WRITE (*,*) SET(I)
C         END DO
C
C      produces the same elements in unspecified order.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      1) If the element does not exist, the error SPICE(INVALIDINDEX)
C         is signalled, and the value of FETCHC is zero.
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
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is TRUE upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     fetch from a character set
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER            CARDC
      LOGICAL            RETURN
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) THEN
         FETCHC = 0
         RETURN
      ELSE
         CALL CHKIN ( 'FETCHC' )
      END IF
 
C
C     Check to see if the N'TH element exists.
C
      IF ( NTH .LT. 1  .OR.  NTH .GT. CARDC ( SET ) ) THEN
         FETCHC = 0
         CALL SETMSG ( 'NTH element does not exist. NTH was *.' )
         CALL ERRINT ( '*', NTH              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
 
C
C     The great secret is that, for now, sets really are maintained
C     in order, for reasons of efficiency.
C
      ELSE
         FETCHC = NTH
      END IF
 
 
      CALL CHKOUT ( 'FETCHC' )
      RETURN
      END
