program day8
  implicit none

  type :: t_box
    integer :: pos(3)
  end type

  type :: t_pair
    integer :: ids(2)
    real :: distance
  end type

  type(t_box), allocatable :: boxes(:)
  integer :: answer

  call read_input(boxes)
  call part1(boxes, 1000, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(boxes, answer)
  print "(A, I0)", "part 2: ", answer
contains
  subroutine read_input(boxes)
    implicit none

    type(t_box), allocatable, intent(out) :: boxes(:)
    type(t_box), allocatable :: temp_boxes(:)
    integer :: fd, ios, n
    character(128) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 0
    allocate(boxes(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      n = n + 1
      if (n > size(boxes)) then
        allocate(temp_boxes(size(boxes)*2))
        temp_boxes(:size(boxes)) = boxes
        call move_alloc(temp_boxes, boxes)
      end if
      read(line, *) boxes(n)%pos(:)
    end do
    boxes = boxes(:n)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(boxes, connections, answer)
    implicit none

    type(t_box), allocatable :: boxes(:)
    integer :: connections
    integer, intent(out) :: answer
    integer :: circuits(size(boxes)), counts(size(boxes))
    type(t_pair) :: distances(size(boxes) * size(boxes) / 2)
    integer :: i, j, k

    k = 0
    do i = 1, size(boxes)
      circuits(i) = i
      counts(i) = 0
      do j = i + 1, size(boxes)
        k = k + 1
        distances(k)%ids = [i, j]
        distances(k)%distance = find_distance(boxes(i), boxes(j))
      end do
    end do

    call qsort_pairs(distances(:k))

    do i = 1, connections
      call join_boxes(circuits, distances(i)%ids(1), distances(i)%ids(2))
    end do

    do i = 1, size(circuits)
      counts(circuits(i)) = counts(circuits(i)) + 1
    end do

    call qsort(counts)

    answer = product(counts(size(counts)-2:))
  end subroutine

  subroutine part2(boxes, answer)
    implicit none

    type(t_box), allocatable :: boxes(:)
    integer, intent(out) :: answer
    integer :: circuits(size(boxes))
    type(t_pair) :: distances(size(boxes) * size(boxes) / 2)
    integer :: i, j, k

    k = 0
    do i = 1, size(boxes)
      circuits(i) = i
      do j = i + 1, size(boxes)
        k = k + 1
        distances(k)%ids = [i, j]
        distances(k)%distance = find_distance(boxes(i), boxes(j))
      end do
    end do

    call qsort_pairs(distances(:k))

    hunt: do i = 1, k
      call join_boxes(circuits, distances(i)%ids(1), distances(i)%ids(2))
      do j = 1, size(circuits)
        if (circuits(j) /= 1) then
          cycle hunt
        end if
      end do
      answer = boxes(distances(i)%ids(1))%pos(1) * boxes(distances(i)%ids(2))%pos(1)
      return
    end do hunt
    answer = -1
  end subroutine

  function find_distance(box1, box2) result(distance)
    implicit none
    type(t_box) :: box1, box2
    real :: distance

    distance = abs(real(box1%pos(1)) - real(box2%pos(1))) ** 2
    distance = distance + abs(real(box1%pos(2)) - real(box2%pos(2))) ** 2
    distance = distance + abs(real(box1%pos(3)) - real(box2%pos(3))) ** 2
    distance = sqrt(distance)
  end function

  recursive subroutine qsort_pairs(list)
    implicit none

    type(t_pair), intent(inout) :: list(:)
    integer :: i, j
    real :: pivot

    if (size(list) < 2) return
    i = 1
    pivot = list(size(list))%distance
    do j = 1, size(list) - 1
      if (list(j)%distance <= pivot) then
        call swap_pairs(list, i, j)
        i = i + 1
      end if
    end do
    call swap_pairs(list, i, size(list))

    call qsort_pairs(list(:i-1))
    call qsort_pairs(list(i+1:))
  end subroutine

  subroutine swap_pairs(list, i, j)
    implicit none

    type(t_pair), intent(inout) :: list(:)
    integer :: i, j
    type(t_pair) :: tmp

    tmp = list(i)
    list(i) = list(j)
    list(j) = tmp
  end subroutine

  subroutine join_boxes(circuits, index1, index2)
    implicit none

    integer, intent(inout) :: circuits(:)
    integer :: index1, index2, id1, id2, replace, i

    id1 = circuits(index1)
    id2 = circuits(index2)
    replace = min(id1, id2)
    do i = 1, size(circuits)
      if (circuits(i) == id1 .or. circuits(i) == id2) then
        circuits(i) = replace
      end if
    end do
  end subroutine

  recursive subroutine qsort(list)
    implicit none

    integer, intent(inout) :: list(:)
    integer :: i, j
    integer :: pivot

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

    integer, intent(inout) :: list(:)
    integer :: i, j
    integer :: tmp

    tmp = list(i)
    list(i) = list(j)
    list(j) = tmp
  end subroutine

end program day8