program day1
    use iso_fortran_env

    implicit none

    integer :: io, error, part1, part2, dial, old_dial, distance
    character :: direction
    character(10) :: line
    dial = 50
    part1 = 0
    part2 = 0

    ! open the file read only
    open(newunit=io, file="inputs/day1.txt", status="old", action="read")
    do
        ! attempt to read two numbers from the file
        read(io, *, iostat=error) line

        ! if we hit EOF break out
        if (is_iostat_end(error)) then
            exit
        end if

        read(line(1:1), *), direction
        read(line(2:LEN(line)), *), distance

        old_dial = dial
        if (direction == 'L') then
            dial = MODULO((dial - distance), 100)
        else
            dial = MODULO((dial + distance), 100)
        end if

        if (dial == 0) then
            part1 = part1 + 1
        endif

        ! first count the number of complete spins we do
        part2 = part2 + distance / 100
        ! now consider whether the relative spin will take us round
        ! only consider this when we didn't start at 0 otherwise we count both ending and starting on zero
        if (old_dial /= 0) then
            if (direction == 'L' .and. MODULO(distance, 100) >= old_dial) then
                part2 = part2 + 1
            elseif (direction == 'R' .and. MODULO(distance, 100) >= (100 - old_dial)) then
                part2 = part2 + 1
            endif
        endif
    end do

    write(*,*), "part 1: ", part1
    write(*,*), "part 2: ", part2

    close(io)
end program day1
