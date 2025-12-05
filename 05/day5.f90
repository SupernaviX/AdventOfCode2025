program day5
  use iso_fortran_env
  implicit none

  type :: t_range
    integer(int64) :: start
    integer(int64) :: finish
  end type

  type(t_range), allocatable :: ranges(:)
  integer(int64), allocatable :: values(:)
  integer(int64) :: answer

  call read_input(ranges, values)

  call part1(ranges, values, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(ranges, answer)
  print "(A, I0)", "part 2: ", answer
contains
  subroutine read_input(ranges, values)
    implicit none

    type(t_range), allocatable, intent(out) :: ranges(:)
    type(t_range), allocatable :: temp_ranges(:)
    integer(int64), allocatable, intent(out) :: values(:)
    integer(int64), allocatable :: temp_values(:)
    integer(int64) :: fd, ios, n, pos
    character(len=256) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 1
    allocate(ranges(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      if (n > size(ranges)) then
        allocate(temp_ranges(size(ranges) * 2))
        temp_ranges(:size(ranges)) = ranges
        call move_alloc(temp_ranges, ranges)
      end if

      pos = index(line, "-")
      if (pos == 0) exit
      read(line(:pos-1), *) ranges(n)%start
      read(line(pos+1:), *) ranges(n)%finish

      n = n + 1
    end do
    ranges = ranges(:n-1)

    n = 1
    allocate(values(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      if (n > size(values)) then
        allocate(temp_values(size(values) * 2))
        temp_values(:size(values)) = values
        call move_alloc(temp_values, values)
      end if

      read(line, *) values(n)
      n = n + 1
    end do
    values = values(:n-1)
  end subroutine

  subroutine part1(ranges, values, answer)
    implicit none

    type(t_range), allocatable :: ranges(:)
    integer(int64), allocatable :: values(:)
    integer(int64), intent(out) :: answer
    integer :: i, j

    answer = 0
    do i = 1, size(values)
      do j = 1, size(ranges)
        if (contains(ranges(j), values(i))) then
          answer = answer + 1
          exit
        end if
      end do
    end do
  end subroutine

  subroutine part2(ranges, answer)
    implicit none

    type(t_range), allocatable :: ranges(:)
    integer(int64), intent(out) :: answer
    integer(int64), allocatable :: values(:)
    integer :: i, j
    integer(int64) :: range_start
    logical :: contained

    allocate(values(size(ranges) * 2))
    do i = 1, size(ranges)
      values((i * 2) - 1) = ranges(i)%start
      values((i * 2)) = ranges(i)%finish + 1
    end do
    call qsort(values)
    call dedup(values)

    answer = 0
    range_start = -1
    do i = 1, size(values)
      contained = .false.
      do j = 1, size(ranges)
        if (contains(ranges(j), values(i))) then
          contained = .true.
          exit
        end if
      end do
      if (range_start < 0 .and. contained) then
        range_start = values(i)
      else if (range_start >= 0 .and. .not. contained) then
        answer = answer + values(i) - range_start
        range_start = -1
      end if
    end do
  end subroutine

  function contains(range, value) result(contained)
    implicit none

    type(t_range) :: range
    integer(int64) :: value
    logical :: contained

    contained = value >= range%start .and. value <= range%finish
  end function

  recursive subroutine qsort(list)
    implicit none

    integer(int64), intent(inout) :: list(:)
    integer :: i, j
    integer(int64) :: pivot

    if (size(list) < 2) return
    i = 1
    pivot = list(size(list))
    do j = 1, size(list) - 1
      if (list(j) <= pivot) then
        call swap(list, i, j)
        i = i + 1
      end if
    end do
    call swap(list, i, size(list))

    call qsort(list(:i-1))
    call qsort(list(i+1:))
  end subroutine

  subroutine swap(list, i, j)
    implicit none

    integer(int64), intent(inout) :: list(:)
    integer :: i, j
    integer(int64) :: tmp

    tmp = list(i)
    list(i) = list(j)
    list(j) = tmp
  end subroutine

  subroutine dedup(list)
    implicit none

    integer(int64), allocatable, intent(inout) :: list(:)
    integer :: i, n, shift

    i = 1
    n = size(list)
    shift = 0
    do
      if (i == n) exit
      if (list(i) == list(i + shift + 1)) then
        shift = shift + 1
        n = n - 1
      else
        i = i + 1
        if (shift > 0) then
          list(i) = list(i + shift)
        end if
      end if
    end do
    list = list(:n)
  end subroutine
end program