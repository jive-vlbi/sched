C$Procedure  ZZEKCDSC ( Private: EK, return column descriptor )
 
      SUBROUTINE ZZEKCDSC ( HANDLE, SEGDSC, COLUMN, COLDSC )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Look up the column descriptor for a column of a given name
C     in a specified segment.
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
      INCLUDE 'eksegdsc.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      CHARACTER*(*)         COLUMN
      INTEGER               COLDSC ( CDSCSZ )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to an EK file.
C     SEGDSC     I   Segment descriptor.
C     COLUMN     I   Name of column.
C     COLDSC     O   Descriptor for specified column.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle for the file containing the
C                    column of interest.  The EK may be open for read
C                    or write access.
C
C     SEGDSC         is the descriptor of the segment containing the
C                    column for which a descriptor is desired.
C
C     COLUMN         is the name of the column whose descriptor is
C                    desired.  Case and white space are not significant.
C
C$ Detailed_Output
C
C     COLDSC         is the descriptor of the column belonging to the
C                    specified file and segment and having name COLUMN.
C                    See the include file ekcoldsc.inc for details
C                    regarding the structure of EK column descriptors.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input column name does not match any column in the
C         designated segment, the error SPICE(BUG) is signalled.  It
C         is the caller's responsibility to call this routine with
C         valid input arguments.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exists for the sole purpose of centralizing code
C     used to perform column descriptor look-ups.
C
C$ Examples
C
C     See the EKACEx routines.
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
C-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    CNAME
 
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               MBASE
      INTEGER               NAMBAS
      INTEGER               NCOLS
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
C
C     Use discovery check-in.
C
C     Get the segment's integer metadata's base address.
C
      MBASE  =  SEGDSC ( IMDIDX )
 
C
C     Get the number of columns.
C
      NCOLS  =  SEGDSC ( NCIDX )
 
C
C     Search linearly through the column descriptors, looking for
C     a column name match.  It's an error if we don't find the input
C     name.
C
      FOUND = .FALSE.
      I     =  1
 
      DO WHILE (  ( I .LE. NCOLS )  .AND.  ( .NOT. FOUND )  )
 
         DSCBAS  =  MBASE + SDSCSZ + (I-1)*CDSCSZ
C
C        Get the character base address of the column name from the
C        current descriptor.
C
         CALL DASRDI ( HANDLE, DSCBAS+1, DSCBAS+CDSCSZ, COLDSC )
 
         NAMBAS = COLDSC( NAMIDX )
 
C
C        Look up the name and compare.
C
         CALL DASRDC ( HANDLE,  NAMBAS+1,  NAMBAS+CNAMSZ,
     .                 1,       CNAMSZ,    CNAME         )
 
         IF (  EQSTR( CNAME, COLUMN )  ) THEN
            FOUND = .TRUE.
         ELSE
            I     =  I + 1
         END IF
 
 
      END DO
 
 
      IF ( .NOT. FOUND ) THEN
 
         CALL DASHLU ( HANDLE,   UNIT                             )
         CALL CHKIN  ( 'ZZEKCDSC'                                 )
         CALL SETMSG ( 'Descriptor for column # was not found. ' //
     .                 'Segment base = #; file = #.'            )
         CALL ERRCH  ( '#',  COLUMN                               )
         CALL ERRINT ( '#',  MBASE                                )
         CALL ERRFNM ( '#',  UNIT                                 )
         CALL SIGERR ( 'SPICE(BUG)'                               )
         CALL CHKOUT ( 'ZZEKCDSC'                                 )
         RETURN
 
      END IF
 
      END
