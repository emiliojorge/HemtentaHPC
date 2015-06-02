program prob1

implicit none  ! recommended
        
        integer ::  n,row,col,j,info
        integer, dimension(1000,1) :: ipiv
        double precision :: lambda,norm,error
        double precision, dimension(1000,1000) :: A,cpA       
        double precision, dimension(1000,1) :: v,av
        double precision, dimension(1,1000) :: vt
        double precision,  external :: dnrm2
        double precision,  external ::  ddot
        v = 1
        n = 1000
        do col = 1, n
                do row = 1, n
                        A(row,col)=SQRT(2.0d0*row+col)
                end do
                A(col,col)=1.5d0 !optimera??i
        end do
        call dlacpy('' ,n,n,A,n,cpA,n)
        call dgetrf( n, n, cpA, n, ipiv, INFO )      
 
        do j = 1, 25
                call dgetrs( 'N', n, 1, cpA, n, ipiv, v, n, INFO )
                
                norm=1.0d0/dnrm2(n,v,1)
                call dscal(n,norm,v,1)
        end do 
        !call dgemv ('N', n, n, 1.0d0, A, n, v, 1, 0,av ,1)!kan jag anvanda A?
        av=matmul(A,v)
        lambda = ddot(n, av, 1, v, 1)
        print*, "lambda is", lambda
        call daxpy(n, -1.0d0*lambda,v,1,av,1) 
        error = dnrm2(n,av,1)
        print*,"Error is ", error
end program prob1
