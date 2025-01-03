! Project Euler No.4

program largest_palindrome
    implicit none

    ! variables
    integer :: i, j, product, max_palindrome
    integer :: lower_bound, upper_bound
 
    ! constants
    lower_bound = 100     ! lowest 3-digit number
    upper_bound = 999     ! highest 3-digit number

    ! initialize
    max_palindrome = 0

    ! loop through numbers in descending order for better efficiency
    outer: do i = upper_bound, lower_bound, -1
        ! start j from i to avoid duplicate calculations since a x b = b x a
        do j = i, lower_bound, -1
            product = i * j

            ! early exit if product is smaller than current max
            if (product <= max_palindrome) cycle outer

            ! check if palindrome and update max if needed
            if (is_palindrome(product)) then
                max_palindrome = product
                ! optional: print intermediate results
                print *, "Found palindrome:", product, " = ", i, " x ", j
            end if
        end do
    end do outer

    ! print result
    print '(A,I0,A,/,A,I0,A,I0)', &
        "The largest palindrome made from the product of two 3-digit numbers is: ", &
        max_palindrome, &
        NEW_LINE('A'), &
        "Range checked: ", lower_bound, " to ", upper_bound

contains

    logical function is_palindrome(num)
        integer, intent(in) :: num
        integer :: temp, reversed, digit

        ! more efficient palindrome check using integer arithmetic
        temp = num
        reversed = 0

        ! build reversed number
        do while (temp > 0)
            digit = mod(temp, 10)
            reversed = reversed * 10 + digit
            temp = temp / 10
        end do

        is_palindrome = (num == reversed)
    end function is_palindrome

end program largest_palindrome 
