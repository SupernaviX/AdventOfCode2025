program day11
  use iso_fortran_env
  implicit none

  type :: t_graph
    integer(int64) :: goal
    logical, allocatable :: edges(:,:)
    character(len=3), allocatable :: names(:)
  end type

  type(t_graph) :: graph
  integer(int64) :: answer

  call read_input(graph)
  call part1(graph, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(graph, answer)
  print "(A, I0)", "part 2: ", answer

contains
  subroutine read_input(graph)
    implicit none

    type(t_graph), intent(out) :: graph
    integer(int64) :: fd, ios, n, i, id, edge
    character(len=1024) :: line
    character(len=3) name

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 0
    allocate(graph%edges(1,1))
    graph%edges(1,1) = .false.
    allocate(graph%names(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      name = line(1:3)
      call find_id(graph, n, name, id)

      i = 6
      do while (line(i:i) >= "a" .and. line(i:i) <= "z")
        call find_id(graph, n, line(i:i+2), edge)
        if (line(i:i+2) == "out") graph%goal = edge
        graph%edges(id, edge) = .true.
        i = i + 4
      end do
    end do
    graph%edges = graph%edges(:n,:n)
    graph%names = graph%names(:n)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(graph, answer)
    implicit none
    
    type(t_graph) :: graph
    integer(int64), intent(out) :: answer
    integer(int64) :: start, n

    n = size(graph%names)
    call find_id(graph, n, "you", start)

    call navigate_to(graph, start, graph%goal, answer)
  end subroutine

  subroutine part2(graph, answer)
    implicit none
    
    type(t_graph) :: graph
    integer(int64), intent(out) :: answer
    integer(int64) :: start, goal1, goal2, n, dist1, dist2, dist3

    n = size(graph%names)
    call find_id(graph, n, "svr", start)
    call find_id(graph, n, "dac", goal1)
    call find_id(graph, n, "fft", goal2)

    call navigate_to(graph, goal1, goal2, dist2)
    if (dist2 > 0) then
      call navigate_to(graph, start, goal1, dist1)
      call navigate_to(graph, goal2, graph%goal, dist3)
    else
      call navigate_to(graph, start, goal2, dist1)
      call navigate_to(graph, goal2, goal1, dist2)
      call navigate_to(graph, goal1, graph%goal, dist3)
    end if
    answer = dist1 * dist2 * dist3
  end subroutine

  subroutine navigate_to(graph, start, goal, count)
    implicit none

    type(t_graph) :: graph
    integer(int64) :: start, goal
    integer(int64) :: paths(size(graph%names))
    integer(int64), intent(out) :: count

    paths(:) = -1
    paths(goal) = 1
    call navigate(graph, paths, start)
    count = paths(start)
  end subroutine

  recursive subroutine navigate(graph, paths, from)
    implicit none

    type(t_graph) :: graph
    integer(int64), intent(inout) :: paths(size(graph%edges, 1))
    integer(int64) :: from, i, count

    if (paths(from) == -2) stop "Cycle detected"
    if (paths(from) /= -1) return ! already visited

    paths(from) = -2
    count = 0
    do i = 1, size(graph%edges, 2)
      if (graph%edges(from, i)) then
        call navigate(graph, paths, i)
        count = count + paths(i)
      end if
    end do
    paths(from) = count
  end subroutine

  subroutine find_id(graph, n, name, id)
    implicit none

    type(t_graph), intent(inout) :: graph
    logical, allocatable :: temp_edges(:,:)
    character(len=3), allocatable :: temp_names(:)
    integer(int64), intent(inout) :: n
    character(len=3), intent(in) :: name
    integer(int64), intent(out) :: id
    integer(int64) :: i

    do i = 1, n
      if (graph%names(i) == name) then
        id = i
        return
      end if
    end do
    n = n + 1
    if (n > size(graph%names)) then
      allocate(temp_edges(size(graph%edges, 1) * 2, size(graph%edges, 2) * 2))
      temp_edges(:,:) = .false.
      temp_edges(:size(graph%edges, 1), :size(graph%edges, 2)) = graph%edges
      call move_alloc(temp_edges, graph%edges)

      allocate(temp_names(size(graph%names) * 2))
      temp_names(:size(graph%names)) = graph%names
      call move_alloc(temp_names, graph%names)
    end if
    id = n
    graph%names(id) = name
  end subroutine
end program day11