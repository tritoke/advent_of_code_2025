program day1
    use M_strings, only : chomp, ascii_lf

    implicit none

    integer :: io, error, file_size, width, height, i
    integer(kind=8) :: part1, part2
    character, allocatable :: file(:), token(:)
    character, allocatable :: grid(:,:)
    part1 = 0
    part2 = 0

    open(newunit=io, file="inputs/day4.txt", status="old", action="read", access="stream")
    inquire(io, SIZE=file_size)
    allocate(character :: file(file_size))
    read(io) file
    close(io)

    width = 0
    height = 0
    do i = 0, file_size
        if (file(i) == ascii_lf) then
            height = height + 1
            if (width == 0) then
                width = i
            end if
        end if
    end do

    grid = reshape(file, ([width, height]))

    part1 = count_accessible()
    part2 = count_removable()

    write(*,*), "part 1: ", part1
    write(*,*), "part 2: ", part2

    contains

    function count_accessible() result(count)
        implicit none

        integer :: count, x, y

        count = 0
        do y = 1, height
            do x = 1, width - 1
                if (grid(x, y) == '@' .and. count_clear_neighbours(x, y) > 4) then
                    count = count + 1
                end if
            end do
        end do
    end function

    function count_clear_neighbours(x, y) result(count)
        implicit none

        integer, intent(in) :: x, y
        integer :: count

        count = &
            unoccupied_maybe_oob(x - 1, y) &
            + unoccupied_maybe_oob(x - 1, y + 1) &
            + unoccupied_maybe_oob(x, y + 1) &
            + unoccupied_maybe_oob(x + 1, y + 1) &
            + unoccupied_maybe_oob(x + 1, y) &
            + unoccupied_maybe_oob(x + 1, y - 1) &
            + unoccupied_maybe_oob(x, y - 1) &
            + unoccupied_maybe_oob(x - 1, y - 1)
    end function

    function unoccupied_maybe_oob(x, y) result(unoccupied_int)
        integer, intent(in) :: x, y
        integer :: unoccupied_int
        logical :: unoccupied

        ! width is one wider than the true width of the grid due to the trailing newline
        unoccupied = (x < 1 .or. x >= width) &
            .or. (y < 1 .or. y > height) &
            ! now we know its inbound so check the value directly
            .or. grid(x,y) == '.'
        unoccupied_int = unoccupied
    end function

    function count_removable() result(count)
        implicit none

        integer :: count, x, y
        logical :: removed

        count = 0
        do
            removed = .false.
            do y = 1, height
                do x = 1, width - 1
                    if (grid(x, y) == '@' .and. count_clear_neighbours(x, y) > 4) then
                        grid(x, y) = '.'
                        removed = .true.
                        count = count + 1
                    end if
                end do
            end do

            if (.not. removed) then
                exit
            end if
        end do
    end function
end program day1
