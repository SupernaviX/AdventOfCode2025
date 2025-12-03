program day3
  use iso_fortran_env

  integer, allocatable :: input(:, :)
  integer(int64) :: answer

  call read_input(input)
  call part1(input, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(input, answer)
  print "(A, I0)", "part 2: ", answer

contains
  subroutine read_input(list)
    implicit none

    integer, allocatable, intent(out) :: list(:,:)
    integer, allocatable :: temp(:,:)
    integer :: fd, ios, n, m, i
    character(len=100) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 1
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      if (.not. allocated(list)) then
        m = len(trim(line))
        allocate(list(1, m))
      end if

      if (n > size(list, 1)) then
        allocate(temp(size(list, 1) * 2, m))
        temp(:size(list, 1),:) = list
        call move_alloc(temp, list)
      end if

      do i = 1, m
        read(line(i:i), *) list(n, i)
      end do
      n = n + 1
    end do
    list = list(:n-1,:m)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(input, answer)
    implicit none

    integer, allocatable :: input(:, :)
    integer(int64), intent(out) :: answer
    integer(int64) :: i, j, hi, lo

    answer = 0
    do i = 1, size(input, 1)
      hi = 1
      do j = 1, size(input, 2) - 1
        if (input(i, j) > input(i, hi)) then
          hi = j
        end if
      end do
      lo = hi + 1
      do j = hi + 1, size(input, 2)
        if (input(i, j) > input(i, lo)) then
          lo = j
        end if
      end do
      answer = answer + (input(i, hi) * 10) + input(i, lo)
    end do
  end subroutine

  subroutine part2(input, answer)
    implicit none

    integer, allocatable :: input(:, :)
    integer(int64), intent(out) :: answer
    integer(int64) :: indices(12)
    integer(int64) :: i, j, k

    answer = 0
    do i = 1, size(input, 1)
      do j = 1, size(indices)
        if (j == 1) then
          indices(j) = 1
        else
          indices(j) = indices(j - 1) + 1
        end if
        do k = indices(j), size(input, 2) - size(indices) + j
          if (input(i, k) > input(i, indices(j))) then
            indices(j) = k
          end if
        end do
        answer = answer + (input(i, indices(j)) * (10 ** (size(indices) - j)))
      end do
    end do
  end subroutine
end program day3