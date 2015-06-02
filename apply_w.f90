subroutine apply_w(A, W, n)
  
integer                           :: n
  double precision, dimension(n, n) :: A,A_tmp
  double precision, dimension(3, 3) :: W
  integer :: i,j


!$OMP parallel do  private(i,j) shared(A_tmp,A,n) schedule(static)
  do k = 2,n-1
    do j = 2,n-1
       A_tmp(j,k) =  W(1, 1) * A(j-1, k-1) + W(1,2) * A(j-1, k) + W(1, 3) * A(j-1,k+1) + &
         W(2, 1) * A(j,   k-1) + W(2,2) * A(j,   k) + W(2, 3) * A(j,   k+1) +  & 
        W(3, 1) * A(j+1, k-1) + W(3,2) * A(j+1, k) + W(3, 3) * A(j+1, k+1)
    end do
  end do
!$OMP end parallel do 
!$OMP BARRIER

!$OMP parallel do  private(i,j) shared(A_tmp,A,n) schedule(static)
  do j = 2, n-1
        do i = 2, n-1
               A(i,j)= A_tmp(i,j)
        end do
 end do
!$OMP end parallel do
end
