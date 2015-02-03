C$ Abstract
C
C     This file contains the number of non-inertial reference
C     frames that are currently built into the SPICE toolkit
C     software.
C
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
C     FRAMES
C
C$ Declarations
 
      INTEGER               NNINRT
      PARAMETER           ( NNINRT = 105 )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NINERT     P   Number of built-in non-inertial reference frames.
C
C$ Parameters
C
C     NINERT     is the number of built-in non-inertial reference
C                frames.  This value is needed by both  ZZFDAT, and
C                FRAMEX.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.5.0, 11-OCT-2011 (BVS)
C
C        Increased the number of non-inertial frames from 100 to 105
C        in order to accomodate the following PCK based frames:
C
C           IAU_CERES
C           IAU_PALLAS
C           IAU_LUTETIA
C           IAU_DAVIDA
C           IAU_STEINS
C
C-    SPICELIB Version 1.4.0, 11-MAY-2010 (BVS)
C
C        Increased the number of non-inertial frames from 96 to 100
C        in order to accomodate the following PCK based frames:
C
C           IAU_BORRELLY
C           IAU_TEMPEL_1
C           IAU_VESTA
C           IAU_ITOKAWA
C
C-    SPICELIB Version 1.3.0, 12-DEC-2002 (BVS)
C
C        Increased the number of non-inertial frames from 85 to 96
C        in order to accomodate the following PCK based frames:
C
C           IAU_CALLIRRHOE
C           IAU_THEMISTO
C           IAU_MAGACLITE
C           IAU_TAYGETE
C           IAU_CHALDENE
C           IAU_HARPALYKE
C           IAU_KALYKE
C           IAU_IOCASTE
C           IAU_ERINOME
C           IAU_ISONOE
C           IAU_PRAXIDIKE
C
C-    SPICELIB Version 1.2.0, 02-AUG-2002 (FST)
C
C        Increased the number of non-inertial frames from 81 to 85
C        in order to accomodate the following PCK based frames:
C
C           IAU_PAN
C           IAU_GASPRA
C           IAU_IDA
C           IAU_EROS
C
C-    SPICELIB Version 1.1.0, 20-FEB-1997 (WLT)
C
C        Increased the number of non-inertial frames from 79 to 81
C        in order to accomodate the following earth rotation
C        models:
C
C           ITRF93
C           EARTH_FIXED
C
C-    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT)
C
C-&
