program day12
  implicit none

  type :: t_layout
    logical :: presents(6, 3, 3)
    type(t_tree), allocatable :: trees(:)
  end type

  type :: t_tree
    integer :: width, height
    integer :: presents(6)
  end type

  type(t_layout) :: layout
  integer :: answer

  call read_input(layout)
  call part1(layout, answer)
  print "(A, I0)", "part 1: ", answer
contains
  subroutine read_input(layout)
    implicit none
    
    type(t_layout), intent(out) :: layout
    type(t_tree), allocatable :: temp_trees(:)
    integer :: fd, ios, n, i, j, k
    character(len = 1024) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    do i = 1, 6
      read(fd, "(A)", iostat = ios) line ! layout number
      do j = 1, 3
        read(fd, "(A)", iostat = ios) line
        do k = 1, 3
          layout%presents(i,j,k) = line(k:k) == "#"
        end do
      end do
      read(fd, "(A)", iostat = ios) line ! newline
    end do

    n = 0
    allocate(layout%trees(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      n = n + 1
      if (n > size(layout%trees)) then
        allocate(temp_trees(size(layout%trees) * 2))
        temp_trees(:size(layout%trees)) = layout%trees
        call move_alloc(temp_trees, layout%trees)
      end if

      i = scan(line, "x")
      read(line(1:i-1), *) layout%trees(n)%width
      j = i + scan(line(i+1:), ":")
      read(line(i+1:j-1), *) layout%trees(n)%height
      j = j + 1
      do i = 1, 6
        k = scan(line(j+1:), " ")
        if (k == 0) then
          k = len(trim(line)) + 1
        else
          k = j + k
        end if
        read(line(j+1:k-1), *) layout%trees(n)%presents(i)
        j = k
      end do
    end do
    layout%trees = layout%trees(:n)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(layout, answer)
    implicit none

    type(t_layout) :: layout
    integer, intent(out) :: answer
    integer :: i

    answer = 0
    do i = 1, size(layout%trees)
      if (do_presents_fit(layout%presents, layout%trees(i))) then
        answer = answer + 1
      end if
    end do
  end subroutine

  function do_presents_fit(presents, tree) result(fit)
    implicit none

    logical :: presents(6, 3, 3)
    type(t_tree) :: tree
    logical :: fit
    integer :: present_sizes(6)
    integer :: i, total_area, used_area

    do i = 1, 6
      present_sizes(i) = count(presents(i,:,:))
    end do

    total_area = tree%width * tree%height
    used_area = 0
    do i = 1, 6
      used_area = used_area + (tree%presents(i) * present_sizes(i))
    end do
    if (total_area <= used_area) then
      fit = .false.
      return
    end if
    used_area = 0
    do i = 1, 6
      used_area = used_area + (tree%presents(i) * 9)
    end do
    if (total_area >= used_area) then
      fit = .true.
      return
    end if

    if (tree%width < (2 * sum(tree%presents(:)))) then
      ! ?????
      fit = .false.
      return
    end if
    fit = .true.
  end function
end program