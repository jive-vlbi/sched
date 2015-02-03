C$Procedure   ZZEKINQN ( Private: EK, insert into query, numeric )
 
      SUBROUTINE ZZEKINQN ( VALUE, TYPE,  LEXBEG, LEXEND,
     .                      EQRYI, EQRYD, DESCR           )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Insert a numeric value into a specified encoded EK query, and
C     obtain a descriptor for the stored value.
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
 
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekbool.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      VALUE
      INTEGER               TYPE
      INTEGER               LEXBEG
      INTEGER               LEXEND
      INTEGER               EQRYI  ( LBCELL : * )
      DOUBLE PRECISION      EQRYD  ( * )
      INTEGER               DESCR  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     VALUE      I   Numeric value.
C     TYPE       I   Data type.  May be INT, DP, or TIME.
C     LEXBEG,
C     LEXEND     I   Begin and end positions of value's lexeme.
C     EQRYI     I-O  Integer portion of encoded query.
C     EQRYD     I-O  Numeric portion of encoded query.
C     DESCR      O   Descriptor for value.
C
C$ Detailed_Input
C
C     VALUE          is a numeric value to be inserted into an
C                    encoded query.
C
C     TYPE           indicates the data type of the numeric value.
C                    TYPE may be INT, DP, or TIME.
C
C     LEXBEG,
C     LEXEND         are the begin and end character positions in the
C                    original query of the lexeme that generated the
C                    input value.  These indices may be used for error
C                    correction.
C
C     EQRYI          is the integer portion of an encoded EK query
C
C     EQRYD          is the numeric portion of an encoded EK query.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query,
C                    updated to reflect the addition of a value to the
C                    encoded query's numeric buffer.
C
C     EQRYD          is the numeric portion of an encoded EK query,
C                    with the input numeric value added.
C
C     DESCR          is a descriptor for the input value.  The
C                    descriptor contains EQVDSZ elements.
C
C$ Parameters
C
C     See the INCLUDE files.
C
C$ Exceptions
C
C     1)  If the input query is uninitialized, the error
C         SPICE(NOTINITIALIZED) will be signalled.
C
C     2)  If there is insufficient space in the encoded query's
C         numeric component, the error SPICE(BUFFERTOOSMALL) is
C         signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should always be used to insert numeric values
C     into an encoded query; the insertion should never be done
C     directly.
C
C$ Examples
C
C     See ZZEKNRML.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
 
C
C     Local variables
C
      INTEGER               FREE
      INTEGER               INIT
      INTEGER               ROOM
      INTEGER               SIZE
 
C
C     Use discovery check-in.
C
      CALL ZZEKREQI ( EQRYI, 'INITIALIZED', INIT )
 
      IF ( INIT .NE. ITRUE ) THEN
 
         CALL CHKIN  ( 'ZZEKINQN'                                    )
         CALL SETMSG ( 'Encoded query must be initialized before it ' //
     .                 'may be written.'                             )
         CALL SIGERR ( 'SPICE(NOTINITIALIZED)'                       )
         CALL CHKOUT ( 'ZZEKINQN'                                    )
         RETURN
 
      END IF
 
C
C     Get the numeric free pointer; make sure there's enough room.
C
      CALL ZZEKREQI ( EQRYI, 'FREE_NUM',     FREE )
      CALL ZZEKREQI ( EQRYI, 'NUM_BUF_SIZE', SIZE )
 
      ROOM  =  SIZE - FREE + 1
 
      IF ( ROOM .LE. 0 ) THEN
 
         CALL CHKIN  ( 'ZZEKINQN'                                    )
         CALL SETMSG ( 'Out of room in numeric portion of encoded ' //
     .                 'query; only # elements were available.'      )
         CALL ERRINT ( '#',  SIZE                                    )
         CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'                       )
         CALL CHKOUT ( 'ZZEKINQN'                                    )
         RETURN
 
      END IF
 
C
C     Insert the value into the double precision portion of the encoded
C     query.
C
      EQRYD(FREE)      =  VALUE
 
C
C     Fill in the descriptor.
C
      CALL CLEARI ( EQVDSZ, DESCR )
 
      DESCR( EQDTYP )  =  TYPE
      DESCR( EQBLEX )  =  LEXBEG
      DESCR( EQELEX )  =  LEXEND
      DESCR( EQVPTR )  =  FREE
 
C
C     Update the numeric free pointer.
C
      CALL ZZEKWEQI ( 'FREE_NUM', FREE+1, EQRYI )
 
 
      RETURN
      END
