!! 直接将JACS论文的SI中分子的坐标复制下来,无法直接作为Gaussian的输入文件
!! 此程序是用于将复制出来的内容转化为类gjf的格式
!! zxli @ NJU 2020.05.24
program Parse_SI_Coordinate

    !! 命令行参数组
    integer                         :: narg
    character(len=256),allocatable  :: arg(:)

    !!
    integer                         :: io, istat
    character(len=256)              :: buffer
    integer                         :: ind
    character(len=3)                :: symbol
    character(len=3),allocatable    :: symbols(:)
    real,allocatable                :: coordinate(:,:) ! 3*n
    integer                         :: d
    real,allocatable                :: copied_coor(:,:) ! 3*n

    !! 计数器
    integer :: i

    !! 保存命令行参数
    narg = iargc()
    if (allocated(arg)) deallocate(arg)
    allocate(arg(narg+1))
    do i=1,narg+1
        call getarg(i-1,arg(i))
    end do
    write(*,"(A)") "-------------------------------------------------"
    write(*,"(A)") "command line"
    write(*,"(1X,*(A,2X))") (trim(arg(i)),i=1,narg+1)
    write(*,"(A)") "-------------------------------------------------"

    !! 读取输入文件,并转化为gjf坐标的格式
    write(*,"(A)") "operate file "//trim(arg(2))

    if (allocated(symbols))     deallocate(symbols)
    if (allocated(coordinate))  deallocate(coordinate)
    if (allocated(copied_coor)) deallocate(copied_coor)

    call open_old_file(io,arg(2))

    !! 读取元素符号和坐标
    do
        read(io,"(A)",iostat=istat) buffer
        if (istat/=0) exit

        ind = index(buffer,".")
        
        if (ind==0) then !! 此行为元素符号

            read(buffer,*) symbol

            if (allocated(symbols)) then
                symbols = [ symbols, symbol]
            else
                allocate(symbols(1))
                symbols(1) = symbol
            end if

            write(*,"(A)") "New Symbol : "//trim(symbol)

        else !! 此行为坐标

            if (allocated(copied_coor)) deallocate(copied_coor)
            allocate(copied_coor(3,size(symbols)))
            if (allocated(coordinate)) then
                d = size(coordinate,dim=2)
                copied_coor(:,1:d) = coordinate(:,1:d)
            end if

            if (allocated(coordinate)) deallocate(coordinate)
            allocate(coordinate(3,size(symbols)))
            coordinate = copied_coor

            !write(*,*) "point 2"

            BACKSPACE(io)
            do i=d+1,size(symbols)
                read(io,*) coordinate(1,i)  !! x
            end do
            do i=d+1,size(symbols)
                read(io,*) coordinate(2,i)  !! y
            end do
            do i=d+1,size(symbols)
                read(io,*) coordinate(3,i)  !! z
            end do

            !write(*,*) "point 3"

        end if

    end do

    write(*,"(A)") "read symbols and coordinate end"

    close(io)

    !! 输出
    call open_new_file(io,trim(arg(2))//".gjf")
    write(io,"(A)") "#"
    write(io,*)
    write(io,"(A)") "title"
    write(io,*)
    write(io,"(I0,2X,I0)") 0,1
    do i=1,size(symbols)
        write(io,"(A2,2X,3(F12.7,2X))") symbols(i), coordinate(:,i)
    end do
    close(io)

    write(*,"(A)") "write symbols and coordinate end"

contains

subroutine open_old_file(io,filename)
    implicit none
    integer,intent(out)         :: io 
    character(len=*),intent(in) :: filename 
    integer                     :: istat
    character(len=80)           :: msg
    open(newunit=io,file=trim(filename),status='old',iostat=istat,iomsg=msg)
    if (istat/=0) then
        write(*,"(3A,I0)") "open_old_file ",trim(filename)," failed: istat = ", istat
        write(*,"(A,A)")   "error message = ", trim(msg)
        stop
    end if 
end subroutine open_old_file

subroutine open_new_file(io,filename)
    implicit none
    integer,intent(out)         :: io 
    character(len=*),intent(in) :: filename 
    integer                     :: istat
    character(len=80)           :: msg
    open(newunit=io,file=trim(filename),status='replace',iostat=istat,iomsg=msg)
    if (istat/=0) then
        write(*,"(A,I0)") "open_new_file "//trim(filename)//" failed: istat = ", istat
        write(*,"(A,A)")  "error message = ", trim(msg)
        stop
    end if
end subroutine open_new_file

end program