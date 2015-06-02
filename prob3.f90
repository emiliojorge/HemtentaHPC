program prob3  
        implicit    none
        include    "mpif.h"
        
        integer ::status(MPI_STATUS_SIZE),iter,row,col,n,i,l&
                        ,cont,root_comm,My_row_comm,My_col_comm
        integer ::tag,my_rank,root_colrank,root_rowrank,world_group,err
        double precision, dimension(100,1) :: A
        double precision, dimension(100,1) :: x,y
        double precision, dimension(4) ::rowrank1,rowrank2,&
                                            rowrank3,rowrank4   
        
        double precision :: error,lambda,lambda_old
        n=100
        call MPI_Init(err)
        
        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, err)
       
         

        
        tag = 1
        row = (my_rank)/4+1
        col = mod(my_rank,4)+1
        
        call MPI_Comm_split(MPI_COMM_WORLD, row, 0, My_row_comm, err)
        call MPI_Comm_split(MPI_COMM_WORLD, col+4, 0, My_col_comm, err)
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
        end if
        x=(1.0d0/400.0d0)
         
  !      if (row==col) then
  !              call MPI_Comm_rank(My_row_comm,root_rowrank,err)
  !              call MPI_Comm_rank(My_col_comm,root_colrank,err)
  !      print*, my_rank
  !              do i=1,4
  !                      if (i/=row) then
  !                              call MPI_Send(root_rowrank,1,MPI_INTEGER,&
  !                               my_rank-row+i, tag,MPI_COMM_WORLD)       
!                                call MPI_Send(root_colrank,1,MPI_INTEGER,&
 !                                my_rank-col*4+(i*4), tag,MPI_COMM_WORLD)       
  !                      end if
   !             end do
    !    else
    !           print*, my_rank,5*(row-1),5*(col-1)
    !             call MPI_Recv(root_rowrank, 1, MPI_INTEGER,5*(row-1)&
    !                                    ,2,MPI_COMM_WORLD,status)
    !             call MPI_Recv(root_colrank, 1, MPI_INTEGER,5*(col-1)&
    !                                    ,3,MPI_COMM_WORLD,status)
    !    end if
        print*, "found roots"
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
                call MPI_Allreduce(lambda,lambda,1,MPI_INTEGER,MPI_SUM, &
                               MPI_COMM_WORLD)
                call MPI_Reduce(y,y,100,MPI_INTEGER,MPI_SUM,row-1,&
                                                My_row_comm)
                if (my_rank==0) then
                        error=abs(lambda-lambda_old)
                        lambda_old=lambda
                        print*,"error is: ", error
                        if (error<=0.001d0) cont=0
                end if
                call MPI_Bcast(cont,1,MPI_INTEGER,0,MPI_COMM_WORLD)
                
                if (row == col) then
                        lambda=0
                        !not actually lambda but squares for norm
                        do i=1,n
                                lambda=lambda+y(i,1)**2
                        end do
                        call MPI_Allreduce(lambda,lambda,1,MPI_INTEGER,MPI_SUM,&
                                        root_comm)
                        lambda=1/SQRT(lambda)
                        x=y*lambda
                end if
                call MPI_Bcast(x,100,MPI_INTEGER,col-1,My_col_comm)
         
       end do 
        
       call MPI_Finalize(err)
end program prob3


