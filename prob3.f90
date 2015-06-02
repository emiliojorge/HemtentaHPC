program prob3  
        implicit    none
        include    "mpif.h"
        
        integer :: status(MPI_STATUS_SIZE)
        integer ::tag,my_rank,my_rowrank,row,col,n,i,world_group,err,&
                        rowgroup1,rowgroup2,rowgroup3,rowgroup4
        double precision, dimension(400,1) :: A
        double precision, dimension(400,1)
        double precision, dimension(4) ::rowrank1,rowrank2,&
                                            rowrank3,rowrank4
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

        if ( row=1 ) &
        call MPI_Comm_split(MPI_COMM_WORLD, 1, 0 My_row_comm, err)


        if ( row=2 ) &
        call MPI_Comm_split(MPI_COMM_WORLD, 2, 0 My_row_comm, err)


        if ( row=3 ) &
        call MPI_Comm_split(MPI_COMM_WORLD, 3, 0 My_row_comm, err)


        if ( row=4 ) &
        call MPI_Comm_split(MPI_COMM_WORLD, 4, 0 My_row_comm, err)

       
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
        
        if (my_rank==0) then
                x=(1.0d0/400.0d0)
        end do

        call MPI_Finalize(err)
end program prob3


