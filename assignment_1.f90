program name 

	implicit none
	integer :: i,n ,fact =1
	write (*,*) "enter an integer"
	read (*,*)n
	do i=1,n
		fact =fact *i
	end do	 
	print*, "the factorial of ",n, "is =",fact
end 
