C$Procedure      EKSSUM ( EK, return segment summary )
 
      SUBROUTINE EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS,
     .                     NCOLS,   CNAMES,  DTYPES,  SIZES,
     .                     STRLNS,  INDEXD,  NULLOK          )
      
C$ Abstract
C
C     Return summary information for a specified segment in a
C     specified EK.
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
C     UTILITY
C
C$ Declarations
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      CHARACTER*(*)         TABNAM
      INTEGER               NROWS
      INTEGER               NCOLS
      CHARACTER*(*)         CNAMES ( * )
      CHARACTER*(*)         DTYPES ( * )
      INTEGER               SIZES  ( * )
      INTEGER               STRLNS ( * )
      LOGICAL               INDEXD ( * )
      LOGICAL               NULLOK ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of EK.
C     SEGNO      I   Number of segment to be summarized.
C     TABNAM     O   Name of table containing segment.
C     NROWS      O   Number of rows in segment.
C     NCOLS      O   Number of columns in segment.
C     CNAMES     O   Names of columns in segment.
C     DTYPES     O   Data types of columns in segment.
C     SIZES      O   Entry sizes of columns in segment.
C     STRLNS     O   String lengths of columns in segment.
C     INDEXD     O   Flags indicating whether columns are indexed.
C     NULLOK     O   Flags indicating whether columns allow nulls.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle specifying the EK containing
C                    the segment to be summarized.
C
C     SEGNO          is the number of the segment whose summary is
C                    desired.  Segments are numbered from 1 to NSEG,
C                    where NSEG is the count of segments in the file.
C
C$ Detailed_Output
C
C     TABNAM         is the name of the table to which the segment
C                    belongs.
C
C     NROWS          is the number of rows in the segment.
C
C     NCOLS          is the number of columns in the segment.  The
C                    maximum number of columns in a segment is given
C                    by the parameter MXCLSG, which is defined in the
C                    include file
C
C                       ekglimit.inc.
C
C                    Currently, this limit is set at 100 columns.
C
C     CNAMES         is an array of names of columns in the segment.
C
C     DTYPES         is an array of data types of columns in the
C                    segment.  Each data type is indicated by a short
C                    character string.  The strings and their meanings
C                    are:
C
C                       'CHR'       Character type.
C                       'DP'        Double precision type.
C                       'INT'       Integer type.
C                       'TIME'      Time type.
C
C                    The Ith element of DTYPES corresponds to the
C                    column whose name is the Ith element of CNAMES.
C
C     SIZES          is an array of declared sizes of column entries.
C                    The Ith element of SIZES is the declared size of
C                    the column whose name is the Ith element of CNAMES.
C                    Scalar-valued columns have size 1; fixed-size,
C                    array-valued columns have size greater than 1.
C                    Array valued columns of variable size have a size
C                    value of -1.
C
C     STRLNS         is an array of declared string lengths of
C                    character column entries.  These lengths are
C                    defined only for columns of character type.
C                    The Ith element of SIZES is the declared size of
C                    the column whose name is the Ith element of CNAMES,
C                    if that column has character type; otherwise, the
C                    Ith element of STRLNS is undefined.  For
C                    character columns having variable string length,
C                    the returned value of STRLNS is -1.
C
C     INDEXD         is an array of logical flags indicating whether the
C                    corresponding columns are indexed.  The Ith element
C                    of INDEXD applies to the column whose name is the
C                    Ith element of CNAMES.
C
C     NULLOK         is an array of logical flags indicating whether the
C                    corresponding columns allow null values.  The Ith
C                    element of NULLOK applies to the column whose name
C                    is the Ith element of CNAMES.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.  The output arguments will not be
C         modified.
C
C     2)  If SEGNO is not the index of an existing segment in the
C         specified file, the error SPICE(INDEXOUTOFRANGE) will be
C         signalled.  The output arguments will not be modified.
C
C     3)  If an I/O error occurs while attempting to obtain summary
C         information for the specified segment, the error will be
C         diagnosed by routines called by this routine.  The output
C         arguments may be modified in this case.
C
C$ Files
C
C     See the description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine supports the function of summarizing a binary
C     EK file, allowing NAIF Toolkit users to determine whether it
C     contains data of interest.  The routine also also provides
C     address information necessary to retrieve information from the
C     segment.
C
C$ Examples
C
C     1)  Dump the table and column names of the segments in an EK.
C
C            C
C            C     Open the EK for read access and get the number of
C            C     segments it contains.
C            C
C                  CALL EKOPR ( EKNAME, HANDLE )
C
C                  NSEG = EKNSEG ( HANDLE )
C
C            C
C            C     Loop through the segments, dumping the desired
C            C     summary information for each one.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Segment summary for file ', EKNAME
C                  WRITE (*,*) ' '
C                  WRITE (*,*) ' '
C
C                  DO I = 1, NSEG
C
C                     CALL EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS,
C                 .                  NCOLS,   CNAMES,  DTYPES,  SIZES,
C                 .                  STRLNS,  INDEXD,  NULLOK         )
C
C                     WRITE (*,*)
C                 .   '========================================'      //
C                 .   '========================================'
C
C
C                     WRITE (*,*) 'Table containing segment: ', TABNAM
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Number of rows:     ', NROWS
C                     WRITE (*,*) 'Number of columns:  ', NCOLS
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Column names and attributes: '
C                     WRITE (*,*) ' '
C
C                     DO J = 1, NCOLS
C
C                        WRITE (*,*) 'Column:   '//CNAMES(J)
C                        WRITE (*,*) ' '
C                        WRITE (*,*) 'Data type: ', DTYPES(J)
C                        WRITE (*,*) 'Dimension: ', SIZES(J)
C
C                        IF ( DTYPES(J) .EQ. 'CHR' ) THEN
C                           WRITE (*,*) 'String length: ', STRLNS(J)
C                        END IF
C
C                        IF ( INDEXD(J) ) THEN
C                           WRITE (*,*) 'Indexed'
C                        END IF
C
C                        IF ( NULLOK(J) ) THEN
C                           WRITE (*,*) 'Nulls allowed'
C                        ELSE
C                           WRITE (*,*) 'Nulls not allowed'
C                        END IF
C
C                        WRITE (*,*) ' '
C                     END DO
C
C                     WRITE (*,*)
C                 .   '========================================'      //
C                 .   '========================================'
C
C                  END DO
C
C                  END
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
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Bug fix:  correct parameter is now used to set dimension
C        of local variable SEGDSC.
C
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return EK segment summary
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Bug fix:  correct parameter SDSCSZ is now used to set dimension
C        of local variable SEGDSC.  Previously, the parameter 
C        CDSCSZ had been used.
C
C-&

 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NTYPES
      PARAMETER           ( NTYPES = 4 )
 
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 4 )
 
C
C     Local variables
C
      CHARACTER*(SHORT)     TYPSTR ( NTYPES )
 
      INTEGER               CDSCRS ( CDSCSZ, MXCLSG )
      INTEGER               I
      INTEGER               SEGDSC ( SDSCSZ )
 
C
C     Saved variables
C
      SAVE                  TYPSTR
 
C
C     Initial values
C
      DATA                  TYPSTR / 'CHR', 'DP', 'INT', 'TIME' /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKSSUM' )
      END IF
 
C
C     Get the info from a knowledgeable source.
C
      CALL ZZEKSINF ( HANDLE, SEGNO, TABNAM, SEGDSC, CNAMES, CDSCRS )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKSSUM' )
         RETURN
      END IF
 
      NROWS  =  SEGDSC ( NRIDX )
      NCOLS  =  SEGDSC ( NCIDX )
 
      DO I = 1, NCOLS
 
         DTYPES(I)  =  TYPSTR (  CDSCRS(TYPIDX,I)  )
 
         SIZES (I)  =  CDSCRS(SIZIDX,I)
 
         IF ( CDSCRS(TYPIDX,I) .EQ. CHR ) THEN
            STRLNS(I)  =  CDSCRS(LENIDX,I)
         ELSE
            STRLNS(I)  =  0
         END IF
 
         INDEXD(I)  =  CDSCRS(IXTIDX,I) .NE. IFALSE
         NULLOK(I)  =  CDSCRS(NFLIDX,I) .NE. IFALSE
 
      END DO
 
      CALL CHKOUT ( 'EKSSUM' )
      RETURN
      END
