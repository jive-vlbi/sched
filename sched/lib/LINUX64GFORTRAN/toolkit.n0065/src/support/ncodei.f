 
C$Procedure NCODEI ( Encode integer value into integer item )
 
      SUBROUTINE NCODEI ( VALUE, ITEM )
      IMPLICIT NONE
 
C$ Abstract
C
C     Encode an integer value into an integer item.
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
C     CELLS
C
C$ Keywords
C
C     CELLS
C
C$ Declarations
 
      INTEGER               VALUE
      INTEGER               ITEM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     VALUE      I   Non-negative integer value.
C     ITEM       O   Item into which VALUE has been encoded.
C
C$ Detailed_Input
C
C     VALUE       is an arbitrary non-negative integer value.
C
C$ Detailed_Output
C
C     ITEM        is an integer item, into which the value has been
C                 been encoded. The value can be recovered by calling
C                 subroutine DCODE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the number to be encoded is negative, the error
C        'SPICE(OUTOFRANGE)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     NCODE (and its inverse, DCODE) provide a uniform way to store
C     and retrieve values from the control areas of cells and cell-
C     based data types. This makes it possible to write templates
C     for a generic data type, suitable for instantiation by a
C     pre-compiler.
C
C$ Examples
C
C     The following code fragment illustrates how NCODE and DCODE
C     can be used to create instantiable subroutine templates.
C
C        C
C        C     Check the bolzat counter, to see if the elements
C        C     have been freebished; if not, do it now, and set
C        C     the counter to zero.
C        C
C              CALL DCODE@ ( STRUCT(-4), BCOUNT )
C
C              IF ( BCOUNT .GT. 0 ) THEN
C                 CALL FREEB@ ( CARD@ ( STRUCT ), STRUCT(1)  )
C                 CALL NCODE@ (                0, STRUCT(-4) )
C              END IF
C
C     By replacing all occurrences of `@' with the appropriate
C     type ending (C, D, or I), this single template can give
C     rise to three separate pieces of type-dependent code.
C
C     The alternative to using NCODE and DCODE is to use simple
C     assignments for numeric cells, and calls to ENCHAR and
C     DECHAR for character cells, destroying the symmetry inherent
C     in the rest of the code.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NCODEI' )
      END IF
 
      IF ( VALUE .GE. 0 ) THEN
         ITEM = VALUE
 
      ELSE
         CALL SETMSG ( 'Cannot encode #; must be non-negative.' )
         CALL ERRINT ( '#', VALUE                               )
         CALL SIGERR ( 'SPICE(OUTOFRANGE)'                      )
      END IF
 
      CALL CHKOUT ( 'NCODEI' )
      RETURN
 
 
 
 
C$Procedure DCODEI ( Decode integer value from integer item )
 
      ENTRY DCODEI ( ITEM, VALUE )
 
C$ Abstract
C
C     Decode the integer value stored in an integer item by a
C     previous call to NCODEI.
C
C$ Required_Reading
C
C     CELLS
C
C$ Keywords
C
C     CELLS
C
C$ Declarations
C
C     INTEGER               ITEM
C     INTEGER               VALUE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item into which an integer value has been encoded.
C     VALUE      O   Encoded value.
C
C$ Detailed_Input
C
C     ITEM        is an integer item, into which an integer value
C                 has been encoded by a previous call to NCODEI.
C
C$ Detailed_Output
C
C     VALUE       is the encoded value.
C
C$ Parameters
C
C     None.
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
C     NCODE (and its inverse, DCODE) provide a uniform way to store
C     and retrieve values from the control areas of cells and cell-
C     based data types. This makes it possible to write templates
C     for a generic data type, suitable for instantiation by a
C     pre-compiler.
C
C$ Examples
C
C     The following code fragment illustrates how NCODE and DCODE
C     can be used to create instantiable subroutine templates.
C
C        C
C        C     Check the bolzat counter, to see if the elements
C        C     have been freebished; if not, do it now, and set
C        C     the counter to zero.
C        C
C              CALL DCODE@ ( STRUCT(-4), BCOUNT )
C
C              IF ( BCOUNT .GT. 0 ) THEN
C                 CALL FREEB@ ( CARD@ ( STRUCT ), STRUCT(1)  )
C                 CALL NCODE@ (                0, STRUCT(-4) )
C              END IF
C
C     By replacing all occurrences of `@' with the appropriate
C     type ending (C, D, or I), this single template can give
C     rise to three separate pieces of type-dependent code.
C
C     The alternative to using NCODE and DCODE is to use simple
C     assignments for numeric cells, and calls to ENCHAR and
C     DECHAR for character cells, destroying the symmetry inherent
C     in the rest of the code.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DCODEI' )
      END IF
 
      VALUE = ITEM
 
      CALL CHKOUT ( 'DCODEI' )
      RETURN
      END
