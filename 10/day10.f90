program day10
  implicit none

  type :: t_machine
    logical :: lights(10)
    integer :: n_lights
    logical :: buttons(16, 10)
    integer :: n_buttons
    integer :: joltage(10)
    integer :: n_joltage
  end type

  type :: t_joltage_state
    integer :: index
    integer, allocatable :: joltage(:)
  end type

  type(t_machine), allocatable :: machines(:)
  integer :: answer

  call read_input(machines)
  call part1(machines, answer)
  print "(A, I0)", "part 1: ", answer
  call part2(machines, answer)
  print "(A, I0)", "part 2: ", answer
contains
  subroutine read_input(machines)
    implicit none

    type(t_machine), allocatable, intent(out) :: machines(:)
    type(t_machine), allocatable :: temp_machines(:)
    integer :: fd, ios, n, i, j, button
    character(len=1024) :: line

    open(unit = fd, file = "input", iostat = ios, status = "old", action = "read")
    if (ios /= 0) stop "Error opening input"

    n = 0
    allocate(machines(1))
    do
      read(fd, "(A)", iostat = ios) line
      if (ios /= 0) exit

      n = n + 1
      if (n > size(machines)) then
        allocate(temp_machines(size(machines) * 2))
        temp_machines(:size(machines)) = machines
        call move_alloc(temp_machines, machines)
      end if

      machines(n)%n_lights = 0
      machines(n)%n_buttons = 0
      machines(n)%n_joltage = 0
      i = 2
      do while (line(i:i) /= "]")
        machines(n)%n_lights = machines(n)%n_lights + 1
        machines(n)%lights(machines(n)%n_lights) = line(i:i) == "#"
        i = i + 1
      end do
      i = i + 2
      do while (line(i:i) == "(")
        machines(n)%n_buttons = machines(n)%n_buttons + 1
        machines(n)%buttons(machines(n)%n_buttons,:) = .false.

        i = i + 1
        do
          j = i + scan(line(i:), ",)") - 1
          read(line(i:j-1), *) button
          machines(n)%buttons(machines(n)%n_buttons, button + 1) = .true.
          i = j + 1
          if (line(j:j) == ")") exit
        end do

        i = i + 1
      end do

      do while (line(i:i) /= "}")
        machines(n)%n_joltage = machines(n)%n_joltage + 1

        j = i + scan(line(i+1:), ",}")
        read(line(i+1:j-1), *) machines(n)%joltage(machines(n)%n_joltage)
        i = j
      end do
    end do
    machines = machines(:n)

    close(unit = fd, iostat = ios)
    if (ios /= 0) stop "Error closing input"
  end subroutine

  subroutine part1(machines, answer)
    implicit none

    type(t_machine), allocatable :: machines(:)
    integer, intent(out) :: answer
    integer :: i, presses

    answer = 0
    do i = 1, size(machines)
      call count_button_presses(machines(i), presses)
      answer = answer + presses
    end do
  end subroutine

  subroutine part2(machines, answer)
    implicit none

    type(t_machine), allocatable :: machines(:)
    integer, intent(out) :: answer
    integer :: i, presses

    answer = 0
    do i = 1, size(machines)
      call count_joltage_presses(machines(i), presses)
      print *, "machine", i, "of", size(machines), ":", presses
      answer = answer + presses
    end do
  end subroutine

  subroutine count_button_presses(machine, presses)
    implicit none

    type :: t_state
      logical :: lit(10)
      integer :: cost
    end type

    type(t_machine) :: machine
    integer, intent(out) :: presses
    integer :: seen(0:1024)
    integer :: QUEUE_SIZE = 16384
    type(t_state) :: queue(16384), current
    integer :: qs, qe, hash, i

    seen(:) = -1
    qs = 1
    qe = 1
    queue(qs)%lit(:) = .false.
    queue(qs)%cost = 0

    do
      current = queue(qs)
      hash = light_hash(current%lit)
      if (hash == light_hash(machine%lights)) then
        presses = current%cost
        return
      end if

      qs = qs + 1
      if (qs > QUEUE_SIZE) qs = qs - QUEUE_SIZE
      if (seen(hash) > -1) cycle
      do i = 1, machine%n_buttons
        qe = qe + 1
        if (qe > QUEUE_SIZE) qe = qe - QUEUE_SIZE
        queue(qe)%lit = push_buttons(current%lit, machine%buttons(i,:))
        queue(qe)%cost = current%cost + 1
        seen(hash) = queue(qe)%cost
      end do

    end do
  end subroutine

  function push_buttons(lights, buttons) result(new_lights)
    implicit none

    logical :: lights(10), buttons(10), new_lights(10)
    integer :: i

    do i = 1, 10
      new_lights(i) = lights(i) .neqv. buttons(i)
    end do

  end function

  function light_hash(lights) result(hash)
    implicit none

    logical :: lights(:)
    integer :: hash, i

    hash = 0
    do i = 1, size(lights)
      hash = hash * 2
      if (lights(i)) hash = hash + 1
    end do
  end function

  subroutine count_joltage_presses(machine, presses)
    implicit none

    type(t_machine) :: machine
    integer, intent(out) :: presses
    logical, allocatable :: buttons(:,:)
    integer, allocatable :: target(:), max_presses(:)
    integer :: i, j

    allocate(buttons(machine%n_buttons, machine%n_joltage))
    allocate(target(machine%n_joltage))
    allocate(max_presses(machine%n_buttons))

    max_presses(:) = 1000000
    do i = 1, machine%n_buttons
      buttons(i,:machine%n_joltage) = machine%buttons(i,:machine%n_joltage)
    end do
    do i = 1, machine%n_joltage
      target(i) = machine%joltage(i)
    end do

    call qsort_buttons(buttons, target)

    do i = 1, size(buttons, 1)
      do j = 1, size(target)
        if (buttons(i,j)) max_presses(i) = min(max_presses(i), target(j))
      end do
    end do

    presses = -1
    call do_count_joltage_presses(buttons, target, max_presses, presses)
  end subroutine

  recursive subroutine do_count_joltage_presses(buttons, target, max_presses, presses)
    implicit none

    logical :: buttons(:,:)
    integer, allocatable :: target(:), target2(:)
    integer, allocatable :: max_presses(:), max_presses2(:)
    integer, intent(inout) :: presses
    integer :: i, j, k, highest, best
    logical :: success

    allocate(target2(size(target)))
    allocate(max_presses2(size(max_presses) - 1))

    best = -1
    highest = 1000000
    do i = 1, size(target)
      if (buttons(1,i)) then
        highest = min(highest, target(i))
      end if
    end do

    if (size(buttons, 1) == 1) then
      ! it's the final button
      success = .true.
      do j = 1, size(target)
        if (buttons(1, j)) then
          if (target(j) /= highest) success = .false.
        else
          if (target(j) /= 0) success = .false.
        end if
      end do
      if (success) then
        presses = highest
      else
        presses = -1
      end if
      return
    end if

    do i = 1, size(target)
      if (target(i) > 0 .and. .not. any(buttons(:,i))) then
        presses = -1
        return
      end if
      target2(i) = target(i)
      do j = 1, size(buttons, 1)
        if (buttons(j, i)) target2(i) = target2(i) - max_presses(j)
      end do
      if (target2(i) > 0) then
        presses = -1
        return
      end if
    end do

    do i = highest, 0, -1
      ! press the first button this many times
      max_presses2(:) = max_presses(2:)
      do j = 1, size(target)
        if (buttons(1, j)) then
          target2(j) = target(j) - i
          do k = 2, size(buttons, 1)
            if (buttons(k, j)) then
              max_presses2(k - 1) = min(max_presses2(k - 1), target2(j))
            end if
          end do
        else
          target2(j) = target(j)
        end if
      end do
      ! recurse
      call do_count_joltage_presses(buttons(2:,:), target2, max_presses2, presses)
      if (presses /= -1) then
        if (best == -1) then
          best = presses + i
        else
          best = min(best, presses + i)
        end if
        presses = -1
      end if
    end do
    presses = best
  end subroutine

  recursive subroutine qsort_buttons(list, target)
    implicit none

    logical, intent(inout) :: list(:,:)
    integer, intent(in) :: target(:)
    integer :: i, j
    logical, allocatable :: pivot(:)

    if (size(list) < 2) return
    i = 1
    pivot = list(size(list, 1),:)
    do j = 1, size(list, 1) - 1
      if (compare_buttons(list(j,:), pivot, target)) then
        call swap_buttons(list, i, j)
        i = i + 1
      end if
    end do
    call swap_buttons(list, i, size(list, 1))

    call qsort_buttons(list(:i-1,:), target)
    call qsort_buttons(list(i+1:,:), target)
  end subroutine

  subroutine swap_buttons(list, i, j)
    implicit none

    logical, intent(inout) :: list(:,:)
    integer :: i, j
    logical, allocatable :: tmp(:)

    tmp = list(i,:)
    list(i,:) = list(j,:)
    list(j,:) = tmp
  end subroutine

  function compare_buttons(lhs, rhs, target) result(lt)
    implicit none

    logical :: lhs(:), rhs(:)
    integer, intent(in) :: target(:)
    logical :: lt
    integer :: l, r, i

    l = 0
    r = 0
    lt = .true.
    do i = 1, size(lhs)
      if (lhs(i)) l = l + 1
      if (rhs(i)) r = r + 1
    end do

    if (l > r) then
      lt = .true.
      return
    else if (l < r) then
      lt = .false.
      return
    end if

    l = 1000000
    r = 1000000

    do i = 1, size(lhs)
      if (lhs(i)) l = min(l, target(i))
      if (rhs(i)) r = min(r, target(i))
    end do
    lt = r <= l
  end function
end program day10
