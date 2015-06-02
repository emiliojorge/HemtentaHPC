program prob3  
        implicit    none
        include    "mpif.h"
        
        integer :: status(MPI_STATUS_SIZE)
        integer :: tag,my_rank
        call MPI_Init(err)

        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, err)
        call MPI_Comm_size(MPI_COMM_WORLD, n_procs, err)
        tag = 1
end program prob3
