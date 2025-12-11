program day1
    use M_strings, only : chomp, split, ascii_lf
    use stdlib_sorting, only: sort_index

    implicit none

    type :: range
        integer(kind=8) :: start, end
    end type

    integer :: io, error, file_size, i, j, dash_count
    integer(kind=8) :: part1, part2, a, b
    character(len=:), allocatable :: file, token
    integer(kind=8), allocatable :: range_starts(:)
    type(range), allocatable :: ranges(:)
    part1 = 0
    part2 = 0

    open(newunit=io, file="inputs/day5.txt", status="old", action="read", access="stream")
    inquire(io, SIZE=file_size)
    allocate(character(len=file_size) :: file)
    read(io) file
    close(io)

    dash_count = 0
    do i = 1, file_size
        if (file(i:i) == '-') then
            dash_count = dash_count + 1
        end if
    end do
    allocate(ranges(dash_count))
    allocate(range_starts(dash_count))

    i = 1
    do while ((chomp(file, token, ascii_lf)) > 0)
        if (i <= dash_count) then
            do j = 1, len(token)
                if (token(j:j) == '-') then
                    exit
                end if
            end do

            read(token(1:j - 1), *), a
            read(token(j+1:), *), b
            ranges(i) = range(start=a, end=b)
            range_starts(i) = a
            i = i + 1
            cycle
        end if
        read(token, *), a
        if (is_fresh(a)) then
            part1 = part1 + 1
        end if
    end do

    call sort_and_merge_ranges()

    do i = 1, j
        part2 = part2 + (ranges(i)%end - ranges(i)%start) + 1
    end do

    write(*,*), "part 1: ", part1
    write(*,*), "part 2: ", part2

    contains

    function is_fresh(id) result (fresh)
        integer(kind=8), intent(in) :: id
        integer :: r
        logical :: fresh
        
        fresh = .false.
        do r = 1, dash_count
            if (ranges(r)%start <= id .and. ranges(r)%end >= id) then
                fresh = .true.
                exit
            end if
        end do
    end function

    subroutine sort_and_merge_ranges()
        integer, allocatable :: index(:)

        allocate(index(size(ranges)))

        call sort_index(range_starts(1:size(ranges)), index(1:size(ranges)))
        ranges(:) = ranges( index(1:size(ranges)) )

        j = 1
        do i = 2, size(ranges)
            if (ranges(i)%start <= ranges(j)%end) then
                ranges(j)%end = max(ranges(j)%end, ranges(i)%end)
            else
                j = j + 1
                ranges(j) = ranges(i)
            end if
        end do
    end subroutine
end program day1
