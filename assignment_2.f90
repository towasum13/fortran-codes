program name
    implicit none
    real::a,b,c
    real ,dimension(2,2):: m
    m(1,1)=-1
    m(1,2)=0
    m(2,1)=1
    m(2,2)=-1
    a=1
    b=-(m(1,1)+m(2,2))
    c=m(1,1)*m(2,2)-m(1,2)*m(2,1)
    call roots(a,b,c)
    
end

subroutine roots(a,b,c)
	implicit none
	real:: a ,b , c
	real :: disc ,root1 ,root2 ,zero , root1_r ,root1_i ,root2_r ,root2_i
	zero= 0.000001
	a=1.0 
	b=5.0
	c=3.0
	disc= b*b-4*a*c
	!three possibilities 
	! 1. disc < 0
	! 2. -zero < disc < zero
	! 3. disc > 0
	if (abs(disc) <= zero) then
		root1= -b/(2*a)
		root2 = root1
		write(*,*) "real and equal " ,root1 ,root2
	    else if (disc > 0)then
	    	root1 =(-b+ sqrt(disc))/(2*a)
	    	root2 = (-b-sqrt(disc))/(2*a)
	    	write(*,*) "real and unequal" ,root1 ,root2
	    else 
	    	root1_r =-b/(2*a)
	    	root1_i =sqrt(-disc)/(2*a)
	    	root2_r =-b/(2*a)
	    	root2_i =-sqrt(-disc)/(2*a)
	    	write(*,*)"complex roots" , "(" ,root1_r ,root1_i ,")" , "(", root2_r ,root2_i ,")"
	end if
end subroutine
