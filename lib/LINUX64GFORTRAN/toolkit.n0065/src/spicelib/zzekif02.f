C$Procedure ZZEKIF02 ( EK, initialize type 2 segment for fast load )
 
      SUBROUTINE ZZEKIF02 ( HANDLE, SEGNO )
 
C$ Abstract
C
C     Initialize a new type 2 EK segment to allow fast loading.
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
C
C$ Declarations
 
 
      INCLUDE 'ekclas07.inc'
      INCLUDE 'ekclas08.inc'
      INCLUDE 'ekclas09.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGNO      I   Segment number.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file open for write access.
C                    A new type 2 segment is to be created in this file
C                    via a fast load.  The segment's metadata has
C                    already been set up by EKBSEG.
C
C     SEGNO          is the number of the segment to prepare for a
C                    fast load.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
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
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine carries out the type-2-specific preparation for
C     populating a type 2 EK segment with data via the fast column
C     loader routines.  This routine expects the segment's metadata to
C     already have been written by EKBSEG.
C
C     This routine expects the segment to contain columns having class
C     7, 8, or 9.
C
C$ Examples
C
C     See EKIFLD.
C
C$ Restrictions
C
C     1)  Assumes total number of words required for column metadata
C         is no greater than IPSIZE.  Currently, with a maximum of 100
C         columns and a maximum metadata size of 2 words per column,
C         this condition is met.
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
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               CLASS
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               OFFSET
      INTEGER               P
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               SEGDSC ( SDSCSZ )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKIF02' )
      END IF
 
C
C     Read in the segment descriptor.
C
      CALL ZZEKMLOC ( HANDLE, SEGNO,   PAGE,         MBASE  )
      CALL DASRDI   ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )
 
      NCOLS  =  SEGDSC(NCIDX)
      NROWS  =  SEGDSC(NRIDX)
 
C
C     Allocate space for column metadata.  We assume that one page
C     of IPSIZE integers is enough room.
C
      CALL ZZEKAPS ( HANDLE, SEGDSC, INT, .FALSE., P, BASE  )
 
 
      OFFSET  =  BASE
 
      DO I = 1, NCOLS
C
C        Read the class from the descriptor of the Ith column directly
C        from the file.  We'll need the descriptor's address in order to
C        update the descriptor in the file.
C
         DSCBAS  =  MBASE + CDOFF + (I-1)*CDSCSZ
 
         CALL DASRDI ( HANDLE, DSCBAS+CLSIDX, DSCBAS+CLSIDX, CLASS )
 
C
C        Update the file.  Set the column descriptor's metadata pointer
C        to the base address of the metadata area.
C
         CALL DASUDI ( HANDLE, DSCBAS+METIDX, DSCBAS+METIDX, OFFSET )
 
C
C        Increment the metadata offset by the size of the metadata
C        for the current column.  The classes of interest range from
C        7 to 9.
C
         IF ( CLASS .EQ. 7 ) THEN
 
            OFFSET  =  OFFSET + MDSZ07
 
 
         ELSE IF ( CLASS .EQ. 8 ) THEN
 
            OFFSET  =  OFFSET + MDSZ08
 
 
         ELSE IF ( CLASS .EQ. 9 ) THEN
 
            OFFSET  =  OFFSET + MDSZ09
 
         ELSE
 
            CALL SETMSG ( 'Class # is not supported.' )
            CALL ERRINT ( '#',  CLASS                 )
            CALL SIGERR ( 'SPICE(NOCLASS)'            )
            CALL CHKOUT ( 'ZZEKIF02'                  )
            RETURN
 
         END IF
 
      END DO
 
 
      CALL CHKOUT ( 'ZZEKIF02' )
      RETURN
      END
