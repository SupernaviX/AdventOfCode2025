program day2
  use iso_fortran_env
  implicit none

  type :: t_range
    integer(int64) :: start
    integer(int64) :: finish
  end type

  type(t_range), allocatable :: input(:)
  integer(int64) :: answer

  call read_input(input)

  call part1(input, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(input, answer)
  print "(A, I0)", "part 2: ", answer

contains
  subroutine read_input(list)
    implicit none

    type(t_range), allocatable, intent(out) :: list(:)
    type(t_range), allocatable :: temp(:)
    integer(int64) :: fd, ios, n, pos, start, finish
    character(len=1024) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"
    read(fd, "(A)", iostat = ios) line
    if (ios /= 0) stop "Error reading input"
    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"

    n = 1
    allocate(list(1))
    do
      pos = index(line, "-")
      if (pos == 0) exit
      read(line(:pos-1), *) start
      line = line(pos+1:)
      pos = index(line, ",")
      if (pos == 0) then
        pos = len(line)
      end if
      read(line(:pos+1), *) finish
      line = line(pos+1:)

      if (n > size(list)) then
        allocate(temp(size(list) * 2))
        temp(:size(list)) = list
        call move_alloc(temp, list)
      end if

      list(n) = t_range(start = start, finish = finish)
      n = n + 1
    end do
    list = list(:n-1)
  end subroutine

  subroutine part1(list, answer)
    implicit none

    type(t_range), allocatable :: list(:)
    integer(int64), intent(out) :: answer
    integer(int64) :: i, sus_index, sus_value

    answer = 0
    do i = 1, size(list)
      do sus_index = next_sus(list(i)%start), prev_sus(list(i)%finish)
        answer = answer + sus_from_index(sus_index)
      end do
    end do
  end subroutine

  subroutine part2(list, answer)
    implicit none

    type(t_range), allocatable :: list(:)
    integer(int64), intent(out) :: answer
    integer(int64) :: i, j
    logical :: temp

    answer = 0
    do i = 1, size(list)
      do j = list(i)%start, list(i)%finish
        if (is_ubersus(j)) then
          answer = answer + j
        end if
      end do
    end do
  end subroutine

  function next_sus(num) result(index)
    implicit none

    integer(int64), intent(in) :: num
    integer(int64) :: index, length, lo, hi
    character(len=32) :: num_str

    write(num_str, "(I0)") num
    length = len(trim(num_str))
    if (modulo(length, 2) == 1) then
      index = 10 ** (length / 2)
    else
      hi = num / (10 ** (length/2))
      lo = modulo(num, (10 ** (length/2)))
      if (hi < lo) then
        index = hi + 1
      else
        index = hi
      end if
    end if
  end function

  function prev_sus(num) result(index)
    implicit none

    integer(int64), intent(in) :: num
    integer(int64) :: index, length, lo, hi
    character(len=16) :: num_str

    write(num_str, "(I0)") num
    length = len(trim(num_str))
    if (modulo(length, 2) == 1) then
      index = -1 + 10 ** (length / 2)
    else
      hi = num / (10 ** (length/2))
      lo = modulo(num, (10 ** (length/2)))
      if (hi <= lo) then
        index = hi
      else
        index = hi - 1
      end if
    end if
  end function

  function sus_from_index(index) result(sus)
    implicit none

    integer(int64), intent(in) :: index
    integer(int64) :: sus
    character(len=128) :: sus_str

    write(sus_str, *) index
    write(sus_str(len(trim(sus_str))+1:), "(I0)") index
    read(sus_str, *) sus
  end function

  function is_ubersus(number) result(sus)
    implicit none

    integer(int64), intent(in) :: number
    character(len=128) num_str, num_substr
    integer :: i, j
    logical :: sus

    write(num_str, "(I0)") number

    do i = 1, len(trim(num_str))/2
      num_substr = num_str(:i)
      sus = .true.
      do j = i + 1, len(trim(num_str)), i
        if (num_substr /= num_str(j:j+i-1)) then
          sus = .false.
          exit
        end if
      end do
      if (sus) then
        return
      end if
    end do
  end function
end program day2
