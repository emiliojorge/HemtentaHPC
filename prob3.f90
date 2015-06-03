program prob3
        implicit    none
        include    "mpif.h"
        
        integer ::status(MPI_STATUS_SIZE)
        integer ::       iter,row,col,n,i,l,n_threads
        integer ::      cont,contNew, root_comm,My_row_comm,My_col_comm 
        integer :: tag,my_rank,root_colrank,root_rowrank,world_group,err
        double precision, dimension(100,1) :: A
        double precision, dimension(100,1) :: x,y,tmp_y
        double precision :: error,lambda,lambda_old,tmp_lambda
        double precision :: squares,tmp_squares
        n=100
        call MPI_Init(err)
        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, err)
        call MPI_Comm_size(MPI_COMM_WORLD,n_threads,err)        
        tag = 1
        row = (my_rank)/4+1
        col = mod(my_rank,4)+1
        
        call MPI_Comm_split(MPI_COMM_WORLD, row, 0, My_row_comm, err)
        call MPI_Comm_split(MPI_COMM_WORLD, col*10, 0, My_col_comm, err)
        if(col==row) then
                call MPI_Comm_split(MPI_COMM_WORLD, 9,0,root_comm,err)
                do i=1,n
                        A(i,1)=(col-1)*n+i
                end do
                if(col==4) then
                        A(n,1)=440.0d0
                end if
        else 
                A=(row-col)
                call MPI_Comm_split(MPI_COMM_WORLD,10,0,root_comm,err)
        end if
        if (my_rank==0) then
                iter=0
                lambda_old = 100000.0d0
        end if
        x=(1.0d0/400.0d0)
        cont=1
        
        do while (cont==1) 
                if (my_rank==0)then
                        iter=iter+1  
                end if
                lambda=0
                do i=1,100 
                        y(i,1)=A(i,1)*x(i,1)
                        lambda=lambda+y(i,1)*x(i,1)
                end do
                !  lambda=dot_product(x,y)
              
                call MPI_Allreduce(lambda,tmp_lambda,1,MPI_DOUBLE,&
                                MPI_SUM, MPI_COMM_WORLD,err)
                call MPI_Reduce(y,tmp_y,100,MPI_DOUBLE,MPI_SUM,row-1,&
                                                My_row_comm,err)
                if(row==col) y=tmp_y
                if (my_rank==0) then
                        error=abs(lambda-lambda_old)
                        lambda_old=lambda
                        print*,iter,lambda, error
                        if (error<=0.001d0) then
                               cont=0
                        end if
                end if
                call MPI_Bcast(cont,1,MPI_INT,0,MPI_COMM_WORLD,err)
                if (row == col) then
                        squares=0
                        !not actually lambda but squares for norm
                        do i=1,n
                                squares=squares+y(i,1)**2
                        end do
                        
                        call MPI_Allreduce(squares,tmp_squares,1,&
                                  MPI_DOUBLE,MPI_SUM,&  root_comm,err)
                        squares=1/SQRT(tmp_squares)
                        x=y*squares
                end if
                call MPI_Bcast(x,100,MPI_DOUBLE,col-1,My_col_comm,err)
                      
       end do 
        
       call MPI_Finalize(err)
end program prob3
