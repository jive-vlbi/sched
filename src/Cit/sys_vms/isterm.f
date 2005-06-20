	INTEGER FUNCTION ISTERM(TERM)
C-----------------------------------------------------------------------
C	Return a TRUE value (odd) if TERM is a terminal device,
C	a FALSE value otherwise. 
C	TJP 1982 December 7
C       Revised 1990 Jul 12
C-----------------------------------------------------------------------
	implicit integer (a-z)
	PARAMETER (DC$_TERM=66)
	PARAMETER (DVI$_DEVCLASS=4)
	PARAMETER (DVI$_DEVTYP=6)
	PARAMETER (DVI$_DEVNAM=32)
C
	character*(*) TERM
	dimension itmlist(4),iosb(2)
C
C		Get an event flag
C
	ier = lib$get_ef(ef)
	if (.not.ier) call exit(ier)
C
C		Find device class of TERM
C
	do l=len(term),1,-1
	    if (term(l:l).ne.' ') goto 10
	end do
	l = 1
   10	itmlist(1) = dvi$_devclass*2**16 + 4
	itmlist(2) = %loc(devclass)
	itmlist(3) = 0
	itmlist(4) = 0
	ier = sys$getdviw(%val(ef),,term(:l),itmlist,iosb,,,)
	if (.not.ier) then
		isterm = ier
		goto 100
	end if
	if (.not.iosb(1)) then
	    isterm = iosb(1)
	else if (devclass.eq.dc$_term) then
	    isterm = 1
	else
	    isterm = 4828 ! "input device is not a terminal"
	end if
C
C		Release event flag
C
100	ier = lib$free_ef(ef)
	if (.not.ier) call exit(ier)
C
C		Return
C
	return
C
	end
