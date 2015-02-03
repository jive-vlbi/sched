C$Procedure ZZEKESIZ ( EK, element entry size )
 
      INTEGER FUNCTION ZZEKESIZ ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
C$ Abstract
C
C     Return the size of a specified column entry.
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
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'eksegdsc.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               RECPTR
      INTEGER               COLDSC ( CDSCSZ )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C
C     The function returns the number of elements in the specified
C     column entry.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     SEGDSC         is the segment descriptor of the segment
C                    containing the column specified by COLDSC.
C
C     COLDSC         is the column descriptor of the column containing
C                    the entry whose size is requested.
C
C     RECPTR         is a pointer to the EK record containing the
C                    column entry of interest.
C
C$ Detailed_Output
C
C     The function returns the number of elements in the specified
C     column entry.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If an I/O error occurs while reading the indicated file,
C         the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This utility centralizes the commonly performed function of
C     determining the element count of a column entry.
C
C$ Examples
C
C     See EKRCEI.
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
C-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
C
C-&
 
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
      INTEGER               ZZEKSZ04
      INTEGER               ZZEKSZ05
      INTEGER               ZZEKSZ06
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
 
      INTEGER               CLASS
      INTEGER               RECNO
      INTEGER               SEGNO
      INTEGER               UNIT
 
C
C     Initialize the function's return value.
C
      ZZEKESIZ  =  0
 
C
C     Use discovery check-in.
C
C     Delegate the problem to the routine of the appropriate class.
C     The first three classes are scalars.
C
C
      CLASS  =  COLDSC ( CLSIDX )
 
      IF ( CLASS .EQ. 1 ) THEN
 
         ZZEKESIZ  =  1
 
 
      ELSE IF ( CLASS .EQ. 2 ) THEN
 
         ZZEKESIZ  =  1
 
 
      ELSE IF ( CLASS .EQ. 3 ) THEN
 
         ZZEKESIZ  =  1
 
 
      ELSE IF ( CLASS .EQ. 4 ) THEN
 
         ZZEKESIZ  =  ZZEKSZ04 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
      ELSE IF ( CLASS .EQ. 5 ) THEN
 
         ZZEKESIZ  =  ZZEKSZ05 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
      ELSE IF ( CLASS .EQ. 6 ) THEN
 
         ZZEKESIZ  =  ZZEKSZ06 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
      ELSE IF ( CLASS .EQ. 7 ) THEN
 
         ZZEKESIZ  =  1
 
 
      ELSE IF ( CLASS .EQ. 8 ) THEN
 
         ZZEKESIZ  =  1
 
 
      ELSE IF ( CLASS .EQ. 9 ) THEN
 
         ZZEKESIZ  =  1
 
 
      ELSE
 
C
C        This is an unsupported column class.
C
         CALL DASHLU   ( HANDLE, UNIT  )
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         SEGNO  =  SEGDSC ( SNOIDX )
 
         CALL CHKIN  ( 'ZZEKESIZ'                                      )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported integer class.  COLUMN = #; '     //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKESIZ'                                      )
         RETURN
 
      END IF
 
 
      RETURN
      END
