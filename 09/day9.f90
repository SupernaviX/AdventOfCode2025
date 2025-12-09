program day9
  use iso_fortran_env
  implicit none

  type :: t_tile
    integer(int64) :: pos(2)
  end type

  type :: t_ranges
    integer(int64) :: r(8, 2), n
  end type

  type(t_tile), allocatable :: red_tiles(:)
  integer(int64) :: answer

  call read_input(red_tiles)
  call part1(red_tiles, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(red_tiles, answer)
  print "(A, I0)", "part 2: ", answer
contains
  subroutine read_input(red_tiles)
    implicit none

    type(t_tile), allocatable, intent(out) :: red_tiles(:)
    type(t_tile), allocatable :: temp_red_tiles(:)
    integer :: fd, ios, n
    character(len=1024) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 0
    allocate(red_tiles(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      n = n + 1
      if (n > size(red_tiles)) then
        allocate(temp_red_tiles(size(red_tiles) * 2))
        temp_red_tiles(:size(red_tiles)) = red_tiles
        call move_alloc(temp_red_tiles, red_tiles)
      end if

      read(line, *) red_tiles(n)%pos
    end do
    red_tiles = red_tiles(:n)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(red_tiles, answer)
    implicit none

    type(t_tile), allocatable :: red_tiles(:)
    integer(int64), intent(out) :: answer
    integer(int64) :: i, j, area

    answer = 0
    do i = 1, size(red_tiles)
      do j = i + 1, size(red_tiles)
        area = (1 + abs(red_tiles(i)%pos(1) - red_tiles(j)%pos(1))) * (1 + abs(red_tiles(i)%pos(2) - red_tiles(j)%pos(2)))
        answer = max(answer, area)
      end do
    end do
  end subroutine

  subroutine part2(red_tiles, answer)
    implicit none

    type(t_tile), allocatable :: red_tiles(:)
    integer(int64), intent(out) :: answer
    integer(int64) :: width, height, i, j, y, x1, x2, y1, y2, area
    type(t_ranges), allocatable :: green_ranges(:)
    logical :: seen_green

    width = 0
    height = 0
    do i = 1, size(red_tiles)
      width = max(width, red_tiles(i)%pos(1))
      height = max(height, red_tiles(i)%pos(1))
    end do
    allocate(green_ranges(height))
    do i = i, height
      green_ranges(i)%n = 0
    end do

    do i = 1, size(red_tiles)
      if (i == size(red_tiles)) then
        j = 1
      else
        j = i + 1
      end if
      x1 = min(red_tiles(i)%pos(1), red_tiles(j)%pos(1))
      x2 = max(red_tiles(i)%pos(1), red_tiles(j)%pos(1))
      y1 = min(red_tiles(i)%pos(2), red_tiles(j)%pos(2))
      y2 = max(red_tiles(i)%pos(2), red_tiles(j)%pos(2))
      do y = y1, y2
        call add_range(green_ranges(y), x1, x2)
      end do
    end do
    do y = 1, height
      call fill_range(green_ranges(y))
    end do

    answer = 0
    do i = 1, size(red_tiles)
      do j = i + 1, size(red_tiles)
        x1 = min(red_tiles(i)%pos(1), red_tiles(j)%pos(1))
        x2 = max(red_tiles(i)%pos(1), red_tiles(j)%pos(1))
        y1 = min(red_tiles(i)%pos(2), red_tiles(j)%pos(2))
        y2 = max(red_tiles(i)%pos(2), red_tiles(j)%pos(2))
        if (fully_overlaps_range(green_ranges, x1, x2, y1, y2)) then
          area = (1 + x2 - x1) * (1 + y2 - y1)
          answer = max(answer, area)
        end if
      end do
    end do
  end subroutine

  subroutine add_range(ranges, lo, hi)
    implicit none

    type(t_ranges), intent(inout) :: ranges
    integer(int64) :: lo, hi, i, j, state
    integer(int64) :: new_r(8, 2), new_n
    integer :: merged_index

    new_n = 0
    merged_index = 0
    do i = 1, ranges%n
      if (lo > ranges%r(i,2)) then
        ! preserve all ranges which are purely before our new range
        new_n = new_n + 1
        if (new_n > 8) then
          stop "too many ranges"
        end if
        new_r(new_n,:) = ranges%r(i,:)
        cycle
      end if

      if (merged_index == 0) then
        new_n = new_n + 1
        if (new_n > 8) then
          stop "too many ranges"
        end if
        new_r(new_n,1) = lo
        new_r(new_n,2) = hi
        merged_index = new_n
      end if

      if (hi < ranges%r(i,1)) then
        ! preserve all ranges which are purely after our new range
        new_n = new_n + 1
        if (new_n > 8) then
          stop "too many ranges"
        end if
        new_r(new_n,:) = ranges%r(i,:)
        cycle
      end if

      ! this range must overlap with our new range. merge them
      new_r(new_n,1) = min(new_r(merged_index,1), ranges%r(i,1))
      new_r(new_n,2) = max(new_r(merged_index,2), ranges%r(i,2))
    end do
    if (merged_index == 0) then
      new_n = new_n + 1
      if (new_n > 8) then
        stop "too many ranges"
      end if
      new_r(new_n,1) = lo
      new_r(new_n,2) = hi
      merged_index = new_n
    end if

    ranges%n = new_n
    ranges%r = new_r
  end subroutine

  subroutine fill_range(ranges)
    implicit none

    type(t_ranges), intent(inout) :: ranges
    integer(int64) :: new_r(8, 2), new_n, i
    logical :: inside

    new_n = 0
    inside = .false.
    do i = 1, ranges%n
      if (.not. inside) then
        new_n = new_n + 1
        new_r(new_n,:) = ranges%r(i,:)
      else
        new_r(new_n,2) = ranges%r(i,2)
      end if
      inside = .not. inside
    end do

    ranges%r = new_r
    ranges%n = new_n
  end subroutine

  function fully_overlaps_range(all_ranges, x1, x2, y1, y2) result(overlaps)
    implicit none
    type(t_ranges), allocatable :: all_ranges(:)
    integer(int64) :: x1, x2, y1, y2, y
    logical :: overlaps

    do y = y1, y2
      overlaps = overlaps_range(all_ranges(y), x1, x2)
      if (.not. overlaps) then
        return
      end if
    end do
  end function

  function overlaps_range(ranges, x1, x2) result(overlaps)
    implicit none
    type(t_ranges):: ranges
    integer(int64) :: x1, x2, i
    logical :: overlaps

    overlaps = .false.
    do i = 1, ranges%n
      if (x1 < ranges%r(i,1)) cycle
      overlaps = x2 <= ranges%r(i,2)
      return
    end do
  end function
end program day9