C$Procedure     EKSHDW ( EK, return shadowing status <STUB> )
 
      SUBROUTINE EKSHDW ( HANDLE, ISSHAD )
 
C$ Abstract
C
C     Return shadowing status of a specified EK file.  THIS IS A
C     STUB ROUTINE.
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
C     FILES
C     UTILITY
C
C$ Declarations
 
      INTEGER               HANDLE
      LOGICAL               ISSHAD
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     ISSHAD     O   Logical flag indicating whether EK is shadowed.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of an EK open for writing.
C
C$ Detailed_Output
C
C     ISSHAD         is a logical flag that is returned .TRUE. if and
C                    only if the EK file designated by HANDLE is
C                    shadowed.
C
C                    In this stub version of the routine, ISSHAD is
C                    always returned .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  It is not an error to supply an input handle that does not
C         belong to an EK that is open for write access.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine is a utility that allows a program to test the
C     shadowing status of a specified EK file.
C
C$ Examples
C
C     See the $Examples section of the umbrella routine EKSHAD.
C
C$ Restrictions
C
C     1) This is a stub routine.
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
C-    Beta Version 1.0.0, 19-DEC-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     return shadowing status of an EK file
C
C-&
 
      INTEGER                I
 
      I      =  HANDLE
      ISSHAD = .FALSE.
      RETURN
      END
