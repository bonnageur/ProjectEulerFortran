! Project Euler No. 2

program sum_even_fibonacci
    implicit none
    integer :: a, b, next_term, sum

    ! initialize variables
    a = 1
    b = 2
    sum = 0

    ! loop through Fibonacci sequence until the terms exceed 4 million
    do while (b <= 4000000)
        if (mod(b, 2) == 0) then
            sum = sum + b
        end if
        next_term = a + b
        a = b
        b = next_term
    end do

    print *, "The sum of even-valued terms in the Fibonacci sequence below 4 million is: ", sum
end program sum_even_fibonacci
