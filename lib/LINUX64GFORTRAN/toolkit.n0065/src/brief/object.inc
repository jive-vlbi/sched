C$ Abstract
C
C    Constants required by the family of "object" routines.
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
C     None.
C
C$ Keywords
C
C     OBJECTS
C
C$ Parameters
C
C     LBCELL   is the lower bound for all cells used throughout
C              the SPICE library..
C
C     NULL     is a constant used to indicate that a particular
C              object in a list is unused.
C
C     RMPOBJ   is the slot in the object list that tells how
C              many values are stored for each object.  I.E.
C              the number of values stored for each object
C              in an object list OBJLIS is OBJLIS(RMPOBJ).
C
C     NACTIV   is the slot in an object list that tells hows
C              many objects in the list are currently active.
C              In otherwords the number of active objects
C              in the object list OBJLIS is OBJLIS(NACTIV)
C
C     LSTID    is the slot in an object list that gives the
C              last object unique ID that was assigned.
C              In otherwords, the value of the last unique
C              object ID code in the object list OBJLIS
C              is OBJLIS(LSTID).
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Not Applicable
C
C$ Particulars
C
C     This include file contains the parameters used by the
C     family of object routines.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 23-FEB-1996 (WLT)
C
C
C-&
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               NULL
      PARAMETER           ( NULL   = 0 )
 
      INTEGER               RMPOBJ
      PARAMETER           ( RMPOBJ = -2 )
 
      INTEGER               NACTIV
      PARAMETER           ( NACTIV = -3 )
 
      INTEGER               LSTID
      PARAMETER           ( LSTID  = -4 )
