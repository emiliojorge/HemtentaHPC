program prob1

implicit none  ! recommended
        
        integer ::  n,row,col,j,norm,info,error
        integer, dimension(1000,1) :: ipiv
        double precision :: lambda 
        double precision, dimension(1000,1000) :: A        
        double precision, dimension(1000,1) :: v,av
        double precision,  external :: dnrm2
        double precision,  external ::  ddot
        v = 1
        n = 1000
        print*, "pre A" 
        do col = 1, n
                do row = 1, n
                        A(row,col)=SQRT(2.0d0*row+col)
                end do
                A(col,col)=1.5d0 !optimera??i
        end do
        print*,"Init A "

        call dgetrf( n, n, A, 1, ipiv, INFO )       
        print*,"factorized "

        do j = 0, 25
                call dgetrs( 'N', n, 1, A, 1, ipiv, v, 1, INFO )
                norm=1.0d0/dnrm2(n,v,1)
                v=v*norm
        print*, "interations done"
        end do 
        call dgemv ('N', n, n, 1, A, 1, v, 1, 0,av ,1)!kan jag anvanda A?
        print*,"multiply Av "

        lambda = ddot(n, v, 1, av, 1)
        call daxpy(n, -1.0d0*lambda,v,1,av,1) 
        error = dnrm2(n,av,1)
        print*,"Error is ", error
end program prob1
