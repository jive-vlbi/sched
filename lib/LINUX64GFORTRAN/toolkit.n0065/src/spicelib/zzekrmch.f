C$Procedure      ZZEKRMCH ( EK, row match )
 
      LOGICAL FUNCTION ZZEKRMCH ( NCNSTR,
     .                            ACTIVE,
     .                            HANDLE,
     .                            SEGDSC,
     .                            CDSCRS,
     .                            ROW,
     .                            ELTS,
     .                            OPS,
     .                            VTYPES,
     .                            CHRBUF,
     .                            CBEGS,
     .                            CENDS,
     .                            DVALS,
     .                            IVALS  )
 
C$ Abstract
C
C     Determine whether a specified row in an EK file satisfies
C     a specified set of constraints.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
 
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekwild.inc'
      INCLUDE 'ekqlimit.inc'
 
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 10 )
 
      INTEGER               NCNSTR
      LOGICAL               ACTIVE ( * )
      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               CDSCRS ( CDSCSZ, * )
      INTEGER               ROW
      INTEGER               ELTS   ( * )
      INTEGER               OPS    ( * )
      INTEGER               VTYPES ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CBEGS  ( * )
      INTEGER               CENDS  ( * )
      DOUBLE PRECISION      DVALS  ( * )
      INTEGER               IVALS  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NCNSTR     I   Number of constraints present in query.
C     ACTIVE     I   Array of flags indicating applicable constraints.
C     HANDLE     I   Handle of EK file containing row.
C     SEGDSC     I   Segment descriptor.
C     CDSCRS     I   Descriptors of columns referenced in query.
C     ROW        I   Index of row to match.
C     ELTS       I   Indices of column entry elements to match.
C     OPS        I   Operators used in query constraints.
C     VTYPES     I   Data types of values on RHS of constraints.
C     CHRBUF     I   Buffer containting query tokens.
C     CBEGS      I   Begin indices of character query tokens.
C     CENDS      I   End indices of character query tokens.
C     DVALS      I   D.p. values used in query constraints.
C     IVALS      I   Integer values used in query constraints.
C     MAXCOL     P   Maximum number of columns per segment.
C
C     The function returns .TRUE. if and only if the specified
C     EK row satisfies the input constraints.
C
C$ Detailed_Input
C
C     NCNSTR         is the number of input constraints against which
C                    the input row is to be compared.
C
C     ACTIVE         is an array of logical flags indicating which
C                    constraints are currently applicable.  The Nth
C                    element of ACTIVE indicates whether or not to apply
C                    the Nth constraint:  if ACTIVE(N) is .TRUE., the
C                    constraint is applicable, otherwise it isn't.
C
C                    The elements of the other input arguments that
C                    define constraints are defined when the
C                    corresponding element of ACTIVE is .TRUE.  For
C                    example, when the second constraint is not active,
C                    the second column descriptor in LDSCRS may not be
C                    defined.
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     SEGDSC         is the descriptor of the EK segment containing the
C                    column entry to be compared.
C
C     CDSCRS         is an array of column descriptors for the columns
C                    referenced in the input constraints.  The Ith
C                    descriptor corresponds to the Ith constraint.
C
C     ROW            is the index of the row to compare against the
C                    input constraints.
C
C     ELTS           is an array of column entry elements to match.
C
C
C     OPS            are relational operators used in the input
C                    constraints.  The elements of OPS are any of the
C                    integer parameters
C
C                       EQ, GE, GT, LE, LT, NE, LIKE, ISNULL, NOTNUL
C
C                    The Ith element of OPS corresponds to the Ith
C                    constraint.
C
C     VTYPES         is an array of data type codes which indicate the
C                    types of the values on the right hand sides of the
C                    input constraints.  The Ith element of VTYPES
C                    applies to the Ith constraint.
C
C     CHRBUF,
C     CBEGS,
C     CENDS          are, respectively, a string containing character
C                    tokens representing values on the right hand sides
C                    of query constraints, and arrays of begin and end
C                    indices of these tokens within CHRBUF.  If the Nth
C                    constraint has a character value on the right hand
C                    side, that value is CHRBUF( CBEGS(N) : CENDS(N) ).
C                    For constraints whose right hand sides do not
C                    specify character values, the corresponding
C                    elements of CBEGS and CENDS are not used.
C
C     DVALS,
C     IVALS          are, respectively, arrays of double precision and
C                    integer values appearing on the right hand sides of
C                    input constraints.  The contents of DVALS and IVALS
C                    are meaningful only for those constraints whose
C                    right hand sides specify values having these data
C                    types.
C
C                    Constraints involving unary operators can be either
C
C                       COLUMN_ENTRY(I)  ISNULL
C                       COLUMN_ENTRY(I)  NOTNUL
C
C                    For constraints of this form, the corresponding
C                    elements of the value arrays are ignored.
C
C$ Detailed_Output
C
C     The function returns .TRUE. if and only if the specified
C     EK row satisfies the input constraints.
C
C$ Parameters
C
C     Within the EK system, operators used in EK queries are
C     represented by integer codes.  The codes and their meanings are
C     listed below.
C
C     Relational expressions in EK queries have the form
C
C        <column name> <operator> <value>
C
C     For columns containing numeric values, the operators
C
C        EQ,  GE,  GT,  LE,  LT,  NE
C
C     may be used; these operators have the same meanings as their
C     Fortran counterparts.  For columns containing character values,
C     the list of allowed operators includes those in the above list,
C     and in addition includes the operator
C
C        LIKE
C
C     which is used to compare strings to a template.  In the character
C     case, the meanings of the parameters
C
C        GE,  GT,  LE,  LT
C
C     match those of the Fortran lexical functions
C
C        LGE, LGT, LLE, LLT
C
C
C     The additional unary operators
C
C        ISNULL, NOTNUL
C
C     are used to test whether a value of any type is null.
C
C$ Exceptions
C
C     1)  If an error is detected, the function will return the value
C         .FALSE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is a utility intended primarily for use by EKSRCH.
C
C$ Examples
C
C     See EKSRCH.
C
C$ Restrictions
C
C     1)  Constraints must apply to scalar columns only.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
C
C
C-&
 
 
C
C     Non-SPICELIB functions
C
      LOGICAL               ZZEKSCMP
 
C
C     Local variables
C
      INTEGER               I
 
C
C     Use discovery check-in.
C
C     For each active constraint in the list, see whether the specified
C     row satisfies the constraint.  If any constraint is not satisfied,
C     return immediately.
C
      I        =  1
      ZZEKRMCH = .TRUE.
 
      DO WHILE (  ( I .LE. NCNSTR ) .AND. ZZEKRMCH  )
 
 
         IF ( ACTIVE(I) ) THEN
C
C           See whether the row satisfies the Ith constraint.
C
            ZZEKRMCH  =  ZZEKSCMP ( OPS(I),
     .                              HANDLE,
     .                              SEGDSC,
     .                              CDSCRS(1,I),
     .                              ROW,
     .                              ELTS(I),
     .                              VTYPES(I),
     .                              CHRBUF( CBEGS(I) : CENDS(I) ),
     .                              DVALS(I),
     .                              IVALS(I),
     .                              .FALSE.   )
 
         END IF
 
C
C        Take a look at the next constraint.
C
         I  =  I + 1
 
      END DO
 
C
C     It's a match if we got this far.
C
      RETURN
      END
