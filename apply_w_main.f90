
program apply_w_main
  implicit none

! To debug, set debug = .true., to check the speed, set debug = .false.
! .
  logical :: debug = .true.

  integer                           :: n, iter, repeat, status
  double precision                  :: t, fsecond
  double precision, dimension(3, 3) :: W

! S will be used to save A, if debug is true.
  double precision, allocatable, dimension(:, :) :: A, S 

  if ( debug ) then
    n = 4 * 2 + 2  ! you can decrease this to check smaller problems
    repeat = 1

    allocate(A(n, n), stat = status)
    if ( status > 0 ) then
      print*, '*** Error: could not allocate A. Terminating...'
      stop
    end if
    allocate(S(n, n), stat = status)
    if ( status > 0 ) then
      print*, '*** Error: could not allocate S. Terminating...'
      stop
    end if
    call random_number(A)
    S = A  ! save A
  else
    n = 4 * 512 + 2  ! so 2050
    repeat = 500
    allocate(A(n, n), stat = status)
    if ( status > 0 ) then
      print*, '*** Error: could not allocate A. Terminating...'
      stop
    end if
    call random_number(A)
  end if
  call random_number(W)

!!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!! You must supply apply_w below
!!!!!!!!!!!!!!! 
  t = fsecond()
    do iter = 1, repeat
      call apply_w(A, W, n) 
    end do
  print'(a, 1pe10.2)', 'time = ', fsecond() - t

  if ( debug ) then
    call check_error(A, S, W, n)
    deallocate(S)
  end if
  deallocate(A)

end program apply_w_main

! ----------------------------------------------------------------------

subroutine check_error(A, S, W, n)
! S contains the original data and A should contain
! the result of your computation in apply_w.
! S will contain the correct result on exit.

  implicit none
  integer                           :: n
  double precision, dimension(n, n) :: A, S
  double precision, dimension(3, 3) :: W

! Local variables
  integer                           :: n1, n2, k

! A terribly slow, but simple, way to compute the result.
  n1 = n - 1
  n2 = n - 2
  S(2:n1, 2:n1) = &
    W(1, 1) * S(1:n2, 1:n2) + W(1, 2) * S(1:n2, 2:n1) + W(1, 3) *&
S(1:n2, 3:n) + &
    W(2, 1) * S(2:n1, 1:n2) + W(2, 2) * S(2:n1, 2:n1) + W(2, 3) *&
S(2:n1, 3:n) + &
    W(3, 1) * S(3:n,  1:n2) + W(3, 2) * S(3:n,  2:n1) + W(3, 3) * S(3:n,3:n)

! Fixing the "borders".
  S(1, :) = A(1, :)
  S(n, :) = A(n, :)
  S(:, 1) = A(:, 1)
  S(:, n) = A(:, n)

  if ( n <= 20 ) then  ! just print small matrices
    print*, 'Your matrix = '
    do k = 1, n
      print'(20f6.3)', A(k, :)
    end do

    print*, 'Correct result = '
    do k = 1, n
      print'(20f6.3)', S(k, :)
    end do
  end if

  print'(a, 1pe12.2)', 'The maximum error = ', maxval(abs(S - A))
  print'(a)', 'a reasonable value for a small problem is <= 1e-14.'
end subroutine check_error

