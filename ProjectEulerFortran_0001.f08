! Project Euler No. 1

program sum_multiples
    implicit none
    integer :: i, sum

    sum = 0
    do i = 1, 999
        if (mod(i, 3) == 0 .or. mod(i, 5) == 0) then
            sum = sum + i
        end if
    end do

    print *, "The sum of all multiples of 3 or 5 below 1000 is: ", sum
end program sum_multiples
