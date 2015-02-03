C$Procedure   ZZEKINQC ( Private: EK, insert into query, character )
 
      SUBROUTINE ZZEKINQC ( VALUE, LENGTH, LEXBEG, LEXEND,
     .                      EQRYI, EQRYC,  DESCR           )
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Insert a character value into a specified encoded EK query, and
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
 
      CHARACTER*(*)         VALUE
      INTEGER               LENGTH
      INTEGER               LEXBEG
      INTEGER               LEXEND
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      INTEGER               DESCR  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     VALUE      I   Character value.
C     LENGTH     I   Length of item to insert.
C     LEXBEG,
C     LEXEND     I   Begin and end positions of value's lexeme.
C     EQRYI     I-O  Integer portion of encoded query.
C     EQRYC     I-O  Character portion of encoded query.
C     DESCR      O   Descriptor for value.
C
C$ Detailed_Input
C
C     VALUE          is a character value to be inserted into an
C                    encoded query.
C
C     LENGTH         indicates the length of the input character value.
C                    If LENGTH exceeds LEN(VALUE), the stored value
C                    is padded with trailing blanks.  This allows
C                    faithful representation of literal strings.
C
C     LEXBEG,
C     LEXEND         are the begin and end character positions in the
C                    original query of the lexeme that generated the
C                    input value.  These indices may be used for error
C                    correction.
C
C     EQRYI          is the integer portion of an encoded EK query
C
C     EQRYC          is the character portion of an encoded EK query.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query,
C                    updated to reflect the addition of a value to the
C                    encoded query's character buffer.
C
C     EQRYC          is the character portion of an encoded EK query,
C                    with the input value added.
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
C     2)  If the input character count LENGTH is non-positive, the
C         error SPICE(INVALIDCOUNT) is signalled.
C
C     3)  If there is insufficient space in the encoded query's
C         character component, the error SPICE(BUFFERTOOSMALL) is
C         signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should always be used to insert character values
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
      INTEGER               L
      INTEGER               ROOM
      INTEGER               SIZE
 
C
C     Use discovery check-in.
C
      CALL ZZEKREQI ( EQRYI, 'INITIALIZED', INIT )
 
      IF ( INIT .NE. ITRUE ) THEN
 
         CALL CHKIN  ( 'ZZEKINQC'                                    )
         CALL SETMSG ( 'Encoded query must be initialized before it ' //
     .                 'may be written.'                             )
         CALL SIGERR ( 'SPICE(NOTINITIALIZED)'                       )
         CALL CHKOUT ( 'ZZEKINQC'                                    )
         RETURN
 
      END IF
 
C
C     Check the input length value.
C
      IF ( LENGTH .LT. 1 ) THEN
 
         CALL CHKIN  ( 'ZZEKINQC'                                    )
         CALL SETMSG ( 'Length of string value was #; must be > 0.'  )
         CALL ERRINT ( '#',  LENGTH                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                         )
         CALL CHKOUT ( 'ZZEKINQC'                                    )
         RETURN
 
      END IF
 
C
C     Get the character free pointer; make sure there's enough room.
C
      CALL ZZEKREQI ( EQRYI, 'FREE_CHR',     FREE )
      CALL ZZEKREQI ( EQRYI, 'CHR_BUF_SIZE', SIZE )
 
      ROOM  =  SIZE - FREE + 1
 
      IF ( LENGTH .GT. ROOM ) THEN
 
         CALL CHKIN  ( 'ZZEKINQC'                                    )
         CALL SETMSG ( 'Out of room in character portion of encoded ' //
     .                 'query; only # elements were available; # '    //
     .                 'are needed.'                                 )
         CALL ERRINT ( '#',  ROOM                                    )
         CALL ERRINT ( '#',  LENGTH                                  )
         CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'                       )
         CALL CHKOUT ( 'ZZEKINQC'                                    )
         RETURN
 
      END IF
 
C
C     Insert the value into the character portion of the encoded query.
C
      L                =  MIN ( LENGTH, LEN(VALUE) )
      EQRYC(FREE:)     =  VALUE(:L)
 
C
C     Fill in the descriptor.
C
      CALL CLEARI ( EQVDSZ, DESCR )
 
      DESCR( EQDTYP )  =  CHR
      DESCR( EQBLEX )  =  LEXBEG
      DESCR( EQELEX )  =  LEXEND
      DESCR( EQBSTR )  =  FREE
      DESCR( EQESTR )  =  FREE + LENGTH - 1
 
C
C     Update the character free pointer.
C
      CALL ZZEKWEQI ( 'FREE_CHR', FREE+LENGTH, EQRYI )
 
 
      RETURN
      END
