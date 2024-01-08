! Name: Adam_Fletcher
! Date: 3/28/2022
! Purpose: To sort the data in ascending order from an ASCII file and the also &
!          find the median value of the data points in that file. 


program sort_median
  implicit none

                                  ! Variable Dictionary
  
  character(len=256) :: filename,filename2   !Var to hold name of inpute file
  integer :: ierror,ierror2                  !I/O error status for both files
  integer :: lun, lun2                       !Var for new log unit numbers
  integer :: N=0                             !Initialize data set amount
  integer :: i,j                             !Indices for loops
  integer :: min                             !Var for selection sort min value
  real :: x                                  !Var to read in data from file
  real :: temp                               !Place holder for select sort swap
  real :: med                                !Var for median value of data
  real, allocatable :: array(:)              !Sets aside enough space for array

  write(*,*)"Enter a file name."             !Prompt user for data file input
  read(*,*) filename
  
  open(newunit=lun,file=trim(filename),status='OLD',iostat=ierror)
  if(ierror/=0)then                          !Opens data file
     
     write(*,*) "Error, Enter new file name."
     stop 1
  endif

  do while(ierror==0)
     read(lun,*,iostat=ierror) x             !Reads in values from file
      if(ierror/=0)then                      !Terminates program if no return 0
      
        exit
     else
        N = N + 1                            !Counts number of data points
     endif
  enddo

  
  allocate(array(N))                         !Allocates number of data points   &
  rewind(lun)                                !needed for array 
 
 
  do i=1,N,1                                 !Assigns each element a variable
     read(lun,*,iostat=ierror) array(i)
  enddo
  
     close(unit=lun)                         !Closes file

    
     do i=1,N,1                              !Sorts array in ascending order
        min = minloc(array(i:N),1)
        temp = array(i)
        array(i) = array(i-1+min)
        array(i-1+min) = temp
  
     enddo
  
   
  if((-1)**N==-1)then                        !Calculates median if data set     &
     med=array((N/2)+1)                      !holds an odd number of data points
  else
     med=(array((N/2)+1)+array(N/2))/2       !Calculate median if num of data   &
  endif                                      !points are even
  
  
  write(*,*) "enter file to write info to."  !Prompt user to assign sorted data &
  read(*,*) filename2                        !to a particular file

  
  open(newunit=lun2,file=trim(filename2),status='OLD',iostat=ierror2)
                                             !Open file to store new data
 
  write(lun2,*,iostat=ierror2) "The data array is:"
                                             !Write text to new file
    do i=1,N,1                               
     
        write(lun2,*,iostat=ierror2) array(i)!Write elements in array to new file
        
        

  enddo
  
  write(lun2,*,iostat=ierror2) "The median of the data array is:", med
                                             !Write value of median to new file
  close(unit=lun2)                           !Closes written file
 
  
  stop 0                                     !Termintate program execution

end program sort_median
