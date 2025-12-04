program day4
  implicit none

  integer, allocatable :: input(:,:)
  integer :: answer

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
    integer :: fd, ios, i, n, m
    character(len=256) :: line

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
        if (line(i:i) == "@") then
          list(n, i) = 1
        else
          list(n, i) = 0
        end if
      end do
      n = n + 1
    end do
    list = list(:n-1,:m)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(input, answer)
    implicit none

    integer, allocatable :: input(:,:)
    integer, intent(out) :: answer
    integer :: x, y, x0, y0, x1, y1

    answer = 0
    do y = 1, size(input, 1)
      do x = 1, size(input, 2)
        if (input(y, x) == 0) then
          cycle
        end if
        y0 = max(1, y - 1)
        x0 = max(1, x - 1)
        y1 = min(size(input, 1), y + 1)
        x1 = min(size(input, 2), x + 1)
        if (sum(input(y0:y1,x0:x1)) < 5) then
          answer = answer + 1
        end if
      end do
    end do
  end subroutine

  subroutine part2(input, answer)
    implicit none

    integer, allocatable :: input(:,:)
    integer, intent(out) :: answer
    integer :: copy(size(input, 1), size(input, 2))
    integer :: copy2(size(input, 1), size(input, 2))
    integer :: x, y, x0, y0, x1, y1, last_answer

    answer = 0
    copy = input
    do
      last_answer = answer
      do y = 1, size(input, 1)
        do x = 1, size(input, 2)
          if (copy(y, x) == 0) then
            copy2(y, x) = 0
            cycle
          end if
          y0 = max(1, y - 1)
          x0 = max(1, x - 1)
          y1 = min(size(input, 1), y + 1)
          x1 = min(size(input, 2), x + 1)
          if (sum(copy(y0:y1,x0:x1)) < 5) then
            copy2(y, x) = 0
            answer = answer + 1
          else
            copy2(y, x) = 1
          end if
        end do
      end do
      copy = copy2
      if (last_answer == answer) then
        exit
      end if
    end do
  end subroutine
end program day4