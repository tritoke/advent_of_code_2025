program day1
    use M_strings, only : chomp

    implicit none

    integer :: io, error, file_size
    integer(kind=8) :: part1, part2
    character(len=:), allocatable :: file, token
    part1 = 0
    part2 = 0

    open(newunit=io, file="inputs/day3.txt", status="old", action="read", access="stream")
    inquire(io, SIZE=file_size)
    allocate(character(len=file_size) :: file)
    read(io) file
    close(io)

    do while ((chomp(file, token, NEW_LINE('A'))) > 0)
        part1 = part1 + max_joltage(token)
        part2 = part2 + max_override_joltage(token)
    end do

    write(*,*), "part 1: ", part1
    write(*,*), "part 2: ", part2

    contains

    function max_joltage(jank) result(max_jolts)
        implicit none
        character(len=:), allocatable, intent(in) :: jank
        integer :: i, j, a, b, joltage, max_jolts
        joltage = 0
        max_jolts = 0

        do i = 1, len(jank)
            a = ichar(jank(i:i)) - ichar("0")
            do j = i + 1, len(jank)
                b = ichar(jank(j:j)) - ichar("0")
                joltage = a * 10 + b
                max_jolts = max(joltage, max_jolts)
            end do
        end do
    end function

    function max_override_joltage(jank) result(joltage)
        implicit none
        character(len=:), allocatable, intent(in) :: jank
        integer :: i, jell, b, jattery(12)
        integer(kind=8) :: joltage
        joltage = 0 ! the joltage of the currently selected jattery jank jells
        jattery = (0)

        do i = 1, len(jank)
            jell = ichar(jank(i:i)) - ichar("0")
            ! decide whether its worth include this joltage in the jattery
            call update_jattery(jattery, jell, joltage)
        end do
    end function

    subroutine update_jattery(jattery, jell, joltage)
        implicit none

        integer, intent(inout) :: jattery(12)
        integer(kind=8), intent(inout) :: joltage
        integer, intent(in) :: jell
        integer(kind=8) :: jj, mjj
        integer :: i, j, mji

        mjj = 0
        do i = 1, 12
            ! jattery joltage
            jj = 0
            do j = 1, 12
                if (j < 12) then
                    if (j < i) then
                        jj = jj * 10 + jattery(j)
                    else
                        jj = jj * 10 + jattery(j + 1)
                    endif
                else
                    jj = jj * 10 + jell
                end if
            end do
            if (jj > mjj) then
                mjj = jj
                mji = i
            end if
        end do

        ! if the new jell increases the joltage then update the jattery to include the new jell
        if (mjj > joltage) then
            jattery(mji:10) = jattery(mji+1:12)
            jattery(12) = jell
            joltage = mjj
        end if
    end subroutine
end program day1
