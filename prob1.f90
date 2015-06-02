program prob1

implicit none  ! recommended
        integer ::  n,row,col,j,norm,info,error
        integer :: dimension(1000) :: ipiv
        double precision :: lambda 
        double precision, dimension(1000,1000) :: A        
        double precision, dimension(1000,1) :: v,av
        v = 1
        n = 1000 
        do col = 0, n
                do row = 0, n
                        A(row,col)=SQRT(2*row+col)
                end do
                A(row,row)=1.5d0 !optimera??
        end do

        call dgetrf( n, n, A, 1, ipiv, INFO )       

        do j = 0, 25
                call dgetrs( 'N', n, 1, A, 1, ipiv, v, 1, INFO )
                norm=1.0d0/dnrm2(n,v,1)
                v=v*norm
        end do 
        call dgemv ('N', n, n, 1, A, 1, v, 1, 0,av ,1)!kan jag anvanda A?
        
        lambda = ddot(n, v, 1, av, 1)
        call daxpy(n, -1.0d0*lambda,v,1,av,1) 
        error = dnrm2(n,av,1)
       call    print*, error
end program prob1
