module interfaces
    interface
        function predicate(x) result(b)
            integer(8), intent(in) :: x
            logical :: b
        end function
    end interface
endmodule

program day1
    use M_strings, only : chomp
    use interfaces, only : predicate

    implicit none

    integer :: io, error, file_size
    integer(kind=8) :: part1, part2, start, end, i
    character(len=:), allocatable :: file, token
    character :: c
    character(10) :: part
    part1 = 0
    part2 = 0

    open(newunit=io, file="inputs/day2.txt", status="old", action="read", access="stream")
    inquire(io, SIZE=file_size)
    allocate(character(len=file_size) :: file)
    read(io) file
    close(io)

    i = 0
    !                             what the fuck is this language man??
    do while ((chomp(file, token, ",-" // NEW_LINE('A'))) > 0)
        if (i == 0) then
            read(token, *) start
        else
            read(token, *) end
            part1 = part1 + sum_invalid_ids(start, end, part1_pred)
            part2 = part2 + sum_invalid_ids(start, end, part2_pred)
        endif 
        i = MODULO(i + 1, 2)
    end do

    write(*,*), "part 1: ", part1
    write(*,*), "part 2: ", part2

    contains

    function sum_invalid_ids(start_id, end_id, pred) result(sum)
        implicit none
        procedure(predicate) :: pred
        integer(kind=8), intent(in) :: start_id, end_id
        integer(kind=8) :: sum, id, m

        sum = 0

        ! this is technically invalid if start and end are different test_modulus brackets
        ! however in my input this doens't occur :)
        do id = start_id, end_id
            if (pred(id)) then
                sum = sum + id
            endif
        end do
    end function 

    function part1_pred(id) result(is_valid)
        implicit none
        integer(kind=8), intent(in) :: id
        integer(kind=8) :: modulus
        logical :: is_valid

        if (id < 1000) then 
            is_valid = check_rep_div(id, 10_8, 2)
        elseif (id < 100000) then 
            is_valid = check_rep_div(id, 100_8, 2)
        elseif (id < 10000000) then 
            is_valid = check_rep_div(id, 1000_8, 2)
        elseif (id < 1000000000) then 
            is_valid = check_rep_div(id, 10000_8, 2)
        else
            is_valid = check_rep_div(id, 100000_8, 2)
        endif
    end function

    function part2_pred(id) result(is_valid)
        implicit none
        integer(kind=8), intent(in) :: id
        logical :: is_valid

        if (id < 100) then
            is_valid = check_rep_div(id, 10_8, 2)
        elseif (id < 1000) then
            is_valid = check_rep_div(id, 10_8, 3)
        elseif (id < 10000) then
            is_valid = check_rep_div(id, 100_8, 2)
        elseif (id < 100000) then
            is_valid = check_rep_div(id, 10_8, 5)
        elseif (id < 1000000) then
            is_valid = check_rep_div(id, 1000_8, 2) .or. check_rep_div(id, 100_8, 3)
        elseif (id < 10000000) then
            is_valid = check_rep_div(id, 10_8, 7)
        elseif (id < 100000000) then
            is_valid = check_rep_div(id, 10000_8, 2)
        elseif (id < 1000000000) then
            is_valid = check_rep_div(id, 1000_8, 3)
        elseif (id < 10000000000_8) then
            is_valid = check_rep_div(id, 100000_8, 2) .or. check_rep_div(id, 100_8, 5)
        else
            is_valid = .false.
        endif
    end function

    function check_rep_div(id, modulus, n) result(is_valid)
        implicit none
        integer(kind=8), intent(in) :: id, modulus
        integer, intent(in) :: n
        integer(kind=8) :: i, r, k
        logical :: is_valid
        is_valid = .true.
        r = MODULO(id, modulus)
        k = id / modulus
        do i = 2, n
            if (i == n) then
                is_valid = is_valid .and. k == r
            else
                is_valid = is_valid .and. MODULO(k, modulus) == r
            endif
            k = k / modulus
        end do
    end function
end program day1
