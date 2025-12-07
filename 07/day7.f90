program day7
  use iso_fortran_env
  implicit none

  type :: t_room
    integer :: start
    logical, allocatable :: grid(:,:)
  end type

  type(t_room) :: room
  integer(int64) :: answer

  call read_input(room)
  call part1(room, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(room, answer)
  print "(A, I0)", "part 2: ", answer
contains
  subroutine read_input(room)
    implicit none

    type(t_room), intent(out) :: room
    logical, allocatable :: temp_grid(:,:)
    integer :: fd, ios, i, n, width
    character(len=1024) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    read(fd, "(A)", iostat = ios) line
    if (ios /= 0) stop "Expected a line"
    width = len(trim(line))
    allocate(room%grid(1, width))
    room%start = scan(line, "S")

    n = 0
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      n = n + 1
      if (n > size(room%grid, 1)) then
        allocate(temp_grid(size(room%grid, 1) * 2, width))
        temp_grid(:size(room%grid, 1),:) = room%grid
        call move_alloc(temp_grid, room%grid)
      end if

      do i = 1, width
        room%grid(n, i) = (line(i:i) == "^")
      end do
    end do
    room%grid = room%grid(:n,:width)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(room, answer)
    implicit none

    type(t_room) :: room
    integer(int64), intent(out) :: answer
    integer :: i, j
    logical :: current(size(room%grid, 2)), next(size(room%grid, 2))

    answer = 0
    do i = 1, size(room%grid, 2)
      current(i) = i == room%start
    end do
    do i = 1, size(room%grid, 1)
      do j = 1, size(room%grid, 2)
        if (room%grid(i, j)) then
          if (current(j)) then
            answer = answer + 1
            next(j-1) = .true.
            next(j+1) = .true.
          end if
        else if (current(j)) then
          next(j) = .true.
        end if
      end do
      current(:) = next(:)
      next(:) = .false.
    end do
  end subroutine

  subroutine part2(room, answer)
    implicit none

    type(t_room) :: room
    integer(int64), intent(out) :: answer
    integer :: i, j
    integer(int64) :: current(size(room%grid, 2)), next(size(room%grid, 2))

    do i = 1, size(room%grid, 2)
      if (i == room%start) then
        current(i) = 1
      else
        current(i) = 0
      end if
    end do
    next(:) = 0
    do i = 1, size(room%grid, 1)
      do j = 1, size(room%grid, 2)
        if (room%grid(i, j)) then
          next(j-1) = next(j-1) + current(j)
          next(j+1) = next(j+1) + current(j)
        else
          next(j) = next(j) + current(j)
        end if
      end do
      current(:) = next(:)
      next(:) = 0
    end do
    answer = sum(current)
  end subroutine
end program day7