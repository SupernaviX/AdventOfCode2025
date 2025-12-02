program day1
  implicit none

  type :: t_turn
    logical :: right
    integer :: value
  end type

  type(t_turn), allocatable :: input(:)
  integer :: answer

  call read_input(input)

  call part1(input, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(input, answer)
  print "(A, I0)", "part 2: ", answer

contains
  subroutine read_input(list)
    implicit none

    type(t_turn), allocatable, intent(out) :: list(:)
    type(t_turn), allocatable :: temp(:)
    integer :: fd, ios, n, value
    character(len=200) :: line
    logical :: right

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"
    n = 1
    allocate(list(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      if (n > size(list)) then
        allocate(temp(size(list) * 2))
        temp(:size(list)) = list
        call move_alloc(temp, list)
      end if

      right = (line(1:1) == 'R')
      read(line(2:), *) value
      list(n) = t_turn(right = right, value = value)
      n = n + 1
    end do
    list = list(:n-1)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"

  end subroutine

  subroutine part1(list, answer)
    implicit none

    type(t_turn), allocatable :: list(:)
    integer, intent(out) :: answer
    integer :: dial, i, sum

    dial = 50
    sum = 0
    do i = 1, size(list)
      if (list(i)%right) then
        dial = modulo(dial + list(i)%value, 100)
      else
        dial = modulo(dial - list(i)%value, 100)
      end if
      if (dial == 0) then
        sum = sum + 1
      end if
    end do

    answer = sum
  end subroutine

  subroutine part2(list, answer)
    implicit none

    type(t_turn), allocatable :: list(:)
    integer, intent(out) :: answer
    integer :: dial, i, j, sum

    dial = 50
    sum = 0
    do i = 1, size(list)
      if (list(i)%right) then
        do j = 1, list(i)%value
          dial = dial + 1
          if (dial == 100) then
            sum = sum + 1
            dial = 0
          end if
        end do
      else
        do j = 1, list(i)%value
          dial = dial - 1
          if (dial == -1) then
            dial = 99
          end if
          if (dial == 0) then
            sum = sum + 1
          end if
        end do
      end if
    end do
    answer = sum
  end subroutine
end program day1
