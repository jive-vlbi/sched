C$Procedure      ZZEKILLT ( EK, indirect, last element less than )
 
      INTEGER FUNCTION ZZEKILLT (  HANDLE,  SEGDSC,  COLDSC,  NROWS,
     .                             DTYPE,   CVAL,    DVAL,    IVAL  )
 
C$ Abstract
C
C     Find the ordinal position of the row, in an specified EK segment,
C     whose value in a specified column is the last last element less
C     than a specified value, where the order relation is given by an
C     order vector in a specified DAS file.
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
C     DAS
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               NROWS
      INTEGER               DTYPE
      CHARACTER*(*)         CVAL
      DOUBLE PRECISION      DVAL
      INTEGER               IVAL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   HANDLE of EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Descriptor of column to be searched.
C     NROWS      I   Number of rows in column.
C     DTYPE      I   Data type of input value.
C     CVAL       I   Character string value.
C     DVAL       I   Double precision value.
C     IVAL       I   Integer value.
C
C     The function returns the index of the last order vector element
C     that points to an array element that is less than the input
C     value of the same data type as the column.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of the EK containing the
C                    segment of interest.
C
C     SEGDSC         is the segment descriptor of the EK
C                    segment of interest.
C
C     COLDSC         is a column descriptor for the column whose
C                    entries are to be compared with an input scalar
C                    value.  The column must be indexed.
C
C     NROWS          is the number of rows in the segment of interest.
C
C     DTYPE          is the data type of the input scalar value.
C
C     CVAL,
C     DVAL,
C     IVAL           are a set of scalar variables of character,
C                    double precision, and integer type.  Whichever
C                    of these has the same data type as the column
C                    indicated by COLDSC is used to compare rows
C                    against.  If COLDSC has data type TIME, DVAL
C                    is used in the comparison.
C
C$ Detailed_Output
C
C     The function returns the index of the last order vector element
C     that points to a column entry that is less than whichever of
C     CVAL, DVAL, or IVAL has the same data type as the input column.
C     If the least element of the column is greater than the input
C     value of the matching type, the function returns the value zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the array size NROWS is non-positive, the error
C         SPICE(INVALIDSIZE) will be signalled.
C
C     2)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     3)  If an I/O error occurs during any access to the file
C         specified by HANDLE, the error will be diagnosed by routines
C         called by this routine.
C
C     4)  If any of SEGDSC, COLDSC, or NROWS are invalid, this routine
C         may fail in unpredictable, but possibly spectacular, ways.
C         Except as described in this header section, no attempt is
C         made to handle these errors.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine supports allow rapid look-up of elements in indexed
C     EK columns.
C
C$ Examples
C
C     See ZZEKKEY.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      DNUM
 
      INTEGER               COLTYP
      INTEGER               INUM
      INTEGER               REC
 
C
C     Initialize the function's return value.
C
      ZZEKILLT  =  0
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKILLT' )
      END IF
 
C
C     Validate the number of rows in the column.
C
      IF ( NROWS .LT. 1 ) THEN
C
C        There's nobody home---that is, there is nothing in the array
C        to compare against.  Zero is the only sensible thing to return.
C
         ZZEKILLT = 0
         CALL SETMSG ( 'Number of rows must be positive; was #.' )
         CALL ERRINT ( '#',  NROWS                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                      )
         CALL CHKOUT ( 'ZZEKILLT'                                )
         RETURN
      END IF
 
C
C     Hand off the problem to the LLT routine of the correct type.
C
      COLTYP  =  COLDSC ( TYPIDX )
 
      IF ( COLTYP .EQ. CHR ) THEN
 
         CALL ZZEKLLTC ( HANDLE, SEGDSC, COLDSC, CVAL, ZZEKILLT, REC )
 
 
      ELSE IF ( COLTYP .EQ. DP ) THEN
 
         IF ( DTYPE .EQ. DP ) THEN
            DNUM = DVAL
         ELSE
            DNUM = IVAL
         END IF
 
         CALL ZZEKLLTD ( HANDLE, SEGDSC, COLDSC, DNUM, ZZEKILLT, REC )
 
 
      ELSE IF ( COLTYP .EQ. TIME ) THEN
 
         CALL ZZEKLLTD ( HANDLE, SEGDSC, COLDSC, DVAL, ZZEKILLT, REC )
 
 
      ELSE IF ( COLTYP .EQ. INT ) THEN
 
         IF ( DTYPE .EQ. DP ) THEN
            INUM = NINT ( DVAL )
         ELSE
            INUM = IVAL
         END IF
 
         CALL ZZEKLLTI ( HANDLE, SEGDSC, COLDSC, INUM, ZZEKILLT, REC )
 
      ELSE
 
         CALL SETMSG ( 'The data type # is not supported.' )
         CALL ERRINT ( '#',  COLTYP                        )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                )
         CALL CHKOUT ( 'ZZEKILLT'                          )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZEKILLT' )
      RETURN
      END
