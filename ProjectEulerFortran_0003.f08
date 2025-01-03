! Project Euler No. 3

program largest_prime_factor
    implicit none
    integer, parameter :: int64 = selected_int_kind(15)     ! define 64-bit integer kind
    integer(kind=int64) :: number, factor, largest_prime

    ! initialize variables
    number = 600851475143_int64
    factor = 2_int64
    largest_prime = 0_int64

    ! divide out all factors of 2
    do while (mod(number, factor) == 0)
        largest_prime = factor
        number = number / factor
    end do

    ! check odd factors starting from 3
    factor = 3_int64
    do while (factor * factor <= number)
        do while (mod(number, factor) == 0)
            largest_prime = factor
            number = number / factor
        end do
        factor = factor + 2_int64
    end do

    ! if the reamining number is greater than 1, it is a prime factor
    if (number > 1_int64) then
        largest_prime = number
    end if

    print *, "The largest prime factor of 600851475143 is: ", largest_prime
end program largest_prime_factor
