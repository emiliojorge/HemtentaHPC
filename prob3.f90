program prob3  
        implicit    none
        include    "mpif.h"
        
        integer :: status(MPI_STATUS_SIZE)
        integer ::tag,my_rank,root_colrank,root_rowrank,row,col,n,i,world_group,err,&
                        rowgroup1,rowgroup2,rowgroup3,rowgroup4
        double precision, dimension(100,1) :: A
        double precision, dimension(100,1) :: x
        double precision, dimension(4) ::rowrank1,rowrank2,&
                                            rowrank3,rowrank4
        integer :: status(MPI_STATUS_SIZE)        
        n=100
        call MPI_Init(err)
        
        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, err)
        call MPI_Comm_size(MPI_COMM_WORLD, n_procs, err)
        
        rowrank1=(/0,1,2,3/)
        rowrank2=(/4,5,6,7/)
        rowrank3=(/8,9,10,11/)
        rowrank4=(/12,13,14,15/)

        
        tag = 1
        row = (my_rank+1)/4+1
        col = mod(my_rank,4)+1

        call MPI_Comm_split(MPI_COMM_WORLD, row, 0 My_row_comm, err)
        call MPI_Comm_split(MPI_COMM_WORLD, col+4, 0 My_col_comm, err)
        !måste veta 11,22,33,44
        
        if(col==row) then
                do i=1,n
                        A(i)=(col-1)*n+i
                end do
                if(col=4) then
                        A(n)=440
                end do
        else 
                A=(row-col)
        end if
        
        x=(1.0d0/400.0d0)
        
        if (row==col)&
                call MPI_Comm_rank(My_row_comm,root_rowrank,err)
                call MPI_Comm_rank(My_col_comm,root_colrank,err)
                do i=1,4
                        if i~=row then 
                                call MPI_Send(root_rowrank,1,MPI_INTEGER,&
                                 my_rank-row+i, tag,MPI_COMM_WORLD)       
                                call MPI_Send(root_collrank,1,MPI_INTEGER,&
                                 my_rank-col*4+(i*4), tag,MPI_COMM_WORLD)       
                        end do
                end do
        else
                 call MPI_Recv(root_rowrank, 1, MPI_INTEGER,(row-1)*4+row-1&
                                        ,tag,MPI_COMM_WORLD,status)
                 call MPI_Recv(root_colrank, 1, MPI_INTEGER,(col-1)*4+col-1&
                                        ,tag,MPI_COMM_WORLD,status)


       call MPI_Finalize(err)
end program prob3


