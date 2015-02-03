C$Procedure      ZZEKSFWD ( EK, set forward pointer for data page )
 
      SUBROUTINE ZZEKSFWD ( HANDLE, TYPE, P, FWARD )
 
C$ Abstract
C
C     Set the forward data pointer for a specified EK data page.
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
 
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               P
      INTEGER               FWARD
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TYPE       I   Data type of page.
C     P          I   Page number.
C     FWARD      I   Forward data pointer.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TYPE           is the data type of the desired page.
C
C     P              is the page number of the allocated page.  This
C                    number is recognized by the EK paged access
C                    routines.
C
C     FWARD          is a forward data pointer.  This is the number
C                    of a data page on which the last column entry
C                    on page P is continued.
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
C     2)  If TYPE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     3)  If an I/O error occurs while reading or writing the indicated
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
C     This routine sets the forward data pointer of the specified EK
C     data page.  The value of the pointer is a page number.
C
C$ Examples
C
C     See ZZEKAPS.
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
C-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      DOUBLE PRECISION      DPPTR
      INTEGER               BASE
 
C
C     Use discovery check-in.
C
C     Look up the base address of the page.
C
      CALL ZZEKPGBS ( TYPE, P, BASE )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
 
      IF ( TYPE .EQ. CHR ) THEN
C
C        Set the encoded count.
C
         CALL ZZEKSEI ( HANDLE, BASE+CFPIDX, FWARD )
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
C
C        Convert the input count to d.p. type.
C
         DPPTR  =  FWARD
 
         CALL DASUDD ( HANDLE, BASE+DFPIDX, BASE+DFPIDX, DPPTR )
 
 
      ELSE
C
C        The remaining possibility is that TYPE is INT.  If we had had
C        an unrecognized type, ZZEKPGBS would have complained.
C
         CALL DASUDI ( HANDLE, BASE+IFPIDX, BASE+IFPIDX, FWARD )
 
      END IF
 
      RETURN
      END
