program name
    implicit none
        real :: r,x,y,c,pi, coun = 0.0
        integer :: i,n=100000
        do i=1, n
            call random_number(r) 
            x=r*100
            call random_number(r)
            y = r * 100
            call distance(x,y, 50.0, 50.0, c)
            coun = coun+c
        end do
        pi= (coun/n)*4
        print*, "the value of pi is =",pi
end program name

subroutine distance(x,y,cx,cy, c)
    implicit none 
    real ,intent(in):: x,y,cx,cy
    real,intent(out):: c
    if (sqrt((cx - x)**2 + (cy - y)**2) <= 50) then
        c=1
    else
        c=0
    end if

end subroutine
