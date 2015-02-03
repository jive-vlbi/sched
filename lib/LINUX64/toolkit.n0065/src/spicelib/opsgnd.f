C$Procedure            OPSGND ( Opposite Sign Double Precision Numbers )
 
      LOGICAL FUNCTION OPSGND  ( X, Y )
 
C$ Abstract
C
C      Function: true if the input arguments have opposite signs.
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
C      NUMBERS
C
C$ Declarations
 
      DOUBLE PRECISION   X
      DOUBLE PRECISION   Y
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      X          I   A double precision number
C      Y          I   A double precision number
C
C$ Detailed_Input
C
C      X      is any double precision number.
C
C      Y      is any double precision number.
C
C$ Detailed_Output
C
C      OPSGND is returned as .TRUE. if one of the pair X,Y is positive
C             and the other is negative.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      This routine returns the value:
C
C            (      (( X .GT. 0) .AND. (Y .LT. 0))
C              .OR. (( X .LT. 0) .AND. (Y .GT. 0)) )
C
C      This is a more stable value than
C
C            ( X*Y .LT. 0 )
C
C      Note that if either of the two values is zero, OPSGND will be
C      false.
C
C$ Examples
C
C      This routine can be used whenever a decision depends upon two
C      Double Precision values having opposite signs.
C
C      IF ( OPSGND ( F(X1), F(X2) ) ) THEN
C            .
C            .
C         find a root of F lying between X1 and X2
C            .
C            .
C      ELSE
C            .
C            .
C         do something
C            .
C            .
C      END IF
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     opposite sign d.p. numbers
C
C-&
 
 
      OPSGND = (      (( X .GT. 0 ) .AND. ( Y .LT. 0 ))
     .           .OR. (( X .LT. 0 ) .AND. ( Y .GT. 0 ))
     .         )
 
      RETURN
      END
