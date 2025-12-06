program day6
  use iso_fortran_env
  implicit none

  integer :: PLUS = 0
  integer :: TIMES = 1

  character(4096), allocatable :: lines(:)
  integer(int64), allocatable :: columns(:,:)
  integer, allocatable :: ops(:)
  integer(int64) :: answer

  call read_lines(lines)
  call part1(lines, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(lines, answer)
  print "(A, I0)", "part 2: ", answer

contains
  subroutine read_lines(lines)
    implicit none

    character(len=4096), allocatable, intent(out) :: lines(:)
    character(len=4096), allocatable :: tmp_lines(:)
    character(len=4096) :: line
    integer :: fd, ios, n

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 0
    allocate(lines(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit
      if (len(trim(line)) == 0) exit

      n = n + 1
      if (n > size(lines)) then
        allocate(tmp_lines(1:size(lines)*2))
        tmp_lines(:size(lines)) = lines(:)
        call move_alloc(tmp_lines, lines)
      end if
      lines(n) = line
    end do
    lines = lines(1:n)
  end subroutine

  subroutine part1(lines, answer)
    character(4096), allocatable :: lines(:)
    integer(int64), intent(out) :: answer

    integer(int64), allocatable :: columns(:,:)
    integer, allocatable :: ops(:)
    integer(int64), allocatable :: tmp_columns(:,:)
    integer :: i, j, start, finish, line_len

    allocate(columns(1, size(lines) - 1))
    do i = 1, (size(lines) - 1)
      line_len = verify(lines(i), " ", .true.)
      j = 0
      finish = 0
      do
        if (finish >= line_len) exit
        start = finish + verify(lines(i)(finish:), " ") - 1
        finish = start + scan(lines(i)(start:), " ") - 1
        if (finish == 0) finish = line_len

        j = j + 1
        if (j > size(columns, 1)) then
          allocate(tmp_columns(size(columns, 1) * 2, size(columns, 2)))
          tmp_columns(:j,:) = columns(:,:)
          call move_alloc(tmp_columns, columns)
        end if

        read(lines(i)(start:finish), *) columns(j,i)
      end do

      if (.not. allocated(ops)) then
        allocate(ops(j))
      end if
    end do
    columns = columns(:j,:size(lines)-1)

    i = size(lines)
    j = 1
    finish = 0
    line_len = verify(lines(i), " ", .true.)
    do
      if (finish >= line_len) exit
      start = finish + verify(lines(i)(finish:), " ") - 1
      finish = start + scan(lines(i)(start:), " ") - 1
      if (finish == 0) finish = line_len

      if (lines(i)(start:finish) == "*") then
        ops(j) = TIMES
      else
        ops(j) = PLUS
      end if
      j = j + 1
    end do

    answer = 0
    do i = 1, size(ops)
      if (ops(i) == TIMES) then
        answer = answer + product(columns(i,:))
      else
        answer = answer + sum(columns(i,:))
      end if
    end do
  end subroutine

  subroutine part2(lines, answer)
    implicit none

    character(4096), allocatable :: lines(:)
    integer(int64), intent(out) :: answer
    integer(int64) :: columns(8)
    integer(int64) :: digit, number
    integer :: line_len, cursor, i, j, op, blanks

    line_len = 0
    do i = 1, size(lines)
      line_len = max(line_len, verify(lines(i), " ", .true.))
    end do

    answer = 0
    do cursor = 1, line_len
      if (lines(size(lines))(cursor:cursor) == "*") then
        j = 0
        op = TIMES
        columns(:) = 1
      else if (lines(size(lines))(cursor:cursor) == "+") then
        j = 0
        op = PLUS
        columns(:) = 0
      end if

      blanks = 0
      number = 0
      do i = 1, size(lines) - 1
        if (lines(i)(cursor:cursor) == " ") then
          blanks = blanks + 1
        else
          read(lines(i)(cursor:cursor), *) digit
          number = (number * 10) + digit
        end if
      end do

      if (blanks < size(lines) - 1) then
        j = j + 1
        columns(j) = number
      else
        if (op == TIMES) then
          answer = answer + product(columns)
        else
          answer = answer + sum(columns)
        end if
      end if
    end do

    if (op == TIMES) then
      answer = answer + product(columns)
    else
      answer = answer + sum(columns)
    end if
  end subroutine
end program day6
