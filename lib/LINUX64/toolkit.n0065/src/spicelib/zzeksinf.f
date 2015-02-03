C$Procedure      ZZEKSINF ( EK, return segment information )
 
      SUBROUTINE ZZEKSINF ( HANDLE,  SEGNO,   TABNAM,
     .                      SEGDSC,  CNAMES,  CDSCRS )
 
C$ Abstract
C
C     Return general segment information for a specified segment in a
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
 
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektnamsz.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      CHARACTER*(*)         TABNAM
      INTEGER               SEGDSC ( SDSCSZ    )
      CHARACTER*(*)         CNAMES (         * )
      INTEGER               CDSCRS ( CDSCSZ, * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of EK.
C     SEGNO      I   Number of segment to be summarized.
C     TABNAM     O   Name of table containing segment.
C     SEGDSC     O   Segment descriptor.
C     CNAMES     O   Names of columns in segment.
C     CDSCRS     O   Descriptors of columns in segment.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
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
C     SEGDSC         is an EK segment descriptor.  The contents of this
C                    integer array are described in the include file
C
C                       eksegdsc.inc.
C
C                    Two commonly used elements of the
C                    descriptor are the number of rows in the table
C                    and the number of columns in the table.  The
C                    indices of these items are given by the parameters
C                    NRIDX and NCIDX, respectively.
C
C
C     CNAMES         is a list of names of data columns in the segment.
C
C     CDSCRS         is a list of descriptors of columns in the segment.
C                    Elements (1:CDSCSZ,I) of this integer array
C                    comprise the descriptor of the Ith column in the
C                    segment.  The contents of a column descriptor are
C                    listed below.  The parameters shown in the first
C                    subscript of CDSCRS are declared in the include
C                    file
C
C                       ekcoldsc.inc.
C
C                    We recommend using these parameters in any calling
C                    routine.
C
C                       CDSCRS(CLSIDX,I):    Column class
C                       CDSCRS(TYPIDX,I):    Data type
C                       CDSCRS(LENIDX,I):    String length
C                       CDSCRS(SIZIDX,I):    Element size
C                       CDSCRS(NAMIDX,I):    Column name base address
C                       CDSCRS(IXTIDX,I):    Column index's type code
C                       CDSCRS(IXPIDX,I):    Column index's pointer
C                       CDSCRS(NULIDX,I):    Null flag
C                       CDSCRS(ORDIDX,I):    Column's ordinal position
C                                            in parent table
C                       CDSCRS(METIDX,I):    Column's integer metadata
C                                            pointer
C                       CDSCRS(11,I):        Reserved.
C
C                    Notes:
C
C                       1) Element 3 applies only to character columns.
C
C                          Element 3 takes the boolean value IFALSE
C                          if the column contains variable-length
C                          strings.
C
C                          The boolean parameter IFALSE is represented
C                          by the integer -1.
C
C                       2) Element 4 takes the boolean value IFALSE
C                          if the column contains variable-size
C                          arrays.
C
C                       3) Element 6 takes the value IFALSE if the
C                          column is not indexed.
C
C                       4) Element 8 takes the value IFALSE if null
C                          values are not allowed in the column.
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
C            C     segments it
C            C     contains.
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
C                     CALL ZZEKSINF ( HANDLE,  I,      TABNAM,
C                 .                   SEGDSC,  CNAMES, CDSCRS  )
C
C                     WRITE (*,*)
C                 .   '========================================'      //
C                 .   '========================================'
C
C
C                     WRITE (*,*) 'Table containing segment: ', TABNAM
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'Column names: '
C                     WRITE (*,*) ' '
C
C                     DO J = 1, SEGDSC(NCIDX)
C
C                        WRITE (*,*) '   '//CNAMES(J)
C
C                     END DO
C
C                     WRITE (*,*)
C                 .   '========================================'      //
C                 .   '========================================'
C
C                  END DO
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
C-    Beta Version 1.1.0, 03-JUL-1996 (NJB)
C
C        Bug fix:  table and column names are now padded with trailing
C        blanks on output if necessary.
C
C-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 03-JUL-1996 (NJB)
C
C        Bug fix:  table and column names are now padded with trailing
C        blanks on output if necessary.  Previously, if the caller
C        declared these variables with string lengths longer than
C        TNAMSZ and CNAMSZ respectively, the trailing characters
C        at positions past those designated by these parameters were
C        left unassigned on output.
C
C-&

 
C
C     SPICELIB functions
C
      INTEGER               EKNSEG
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               I
      INTEGER               NCOLS
      INTEGER               NSEG
      INTEGER               P
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKSINF' )
      END IF
 
C
C     Verify that the target file is a paged DAS EK open for read
C     access, or we can't summarize the file.
C
      CALL ZZEKPGCH ( HANDLE, 'READ' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKSINF' )
         RETURN
      END IF
 
C
C     Find out how many segments are in the file, so we can check
C     the index for validity.
C
      NSEG = EKNSEG ( HANDLE )
 
      IF (  ( SEGNO .LT. 1 ) .OR. ( SEGNO .GT. NSEG )  ) THEN
 
         CALL SETMSG ( 'Segment index was #; valid range is 1:#' )
         CALL ERRINT ( '#',  SEGNO                               )
         CALL ERRINT ( '#',  NSEG                                )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                  )
         CALL CHKOUT ( 'ZZEKSINF'                                )
         RETURN
 
      END IF
 
C
C     We're ready to proceed.  The first step is to find the
C     segment's metadata location and look up the segment descriptor.
C
      CALL ZZEKMLOC ( HANDLE, SEGNO,  P,           BASE   )
      CALL DASRDI   ( HANDLE, BASE+1, BASE+SDSCSZ, SEGDSC )
 
C
C     Get the table name.  The table's base address is in the segment
C     descriptor.
C
      CALL DASRDC ( HANDLE,
     .              SEGDSC(TNMIDX) + 1,
     .              SEGDSC(TNMIDX) + TNAMSZ,
     .              1,
     .              TNAMSZ,
     .              TABNAM                   )
 
 
      IF (  LEN( TABNAM )  .GT.  TNAMSZ  ) THEN
         TABNAM (TNAMSZ+1:) = ' '
      END IF


C
C     Read the column descriptors.  The first one starts at DAS
C     integer address
C
C        BASE + CDOFF + 1.
C
C
      NCOLS   =  SEGDSC(NCIDX)
 
      CALL DASRDI ( HANDLE,
     .              BASE + CDOFF + 1,
     .              BASE + CDOFF + NCOLS*CDSCSZ,
     .              CDSCRS                        )
 
C
C     Now read the column names into the names array.
C
      CALL DASRDC ( HANDLE,
     .              SEGDSC(NMLIDX) + 1,
     .              SEGDSC(NMLIDX) + NCOLS*CNAMSZ,
     .              1,
     .              CNAMSZ,
     .              CNAMES  )
 
      
      IF (   LEN( CNAMES(1) )  .GT.  CNAMSZ  ) THEN
      
         DO I = 1, NCOLS
            CNAMES(I)(CNAMSZ+1:) = ' '
         END DO
         
      END IF
 
C
C     All output arguments are set, or else FAILED() is .TRUE.
C
 
      CALL CHKOUT ( 'ZZEKSINF' )
      RETURN
      END
