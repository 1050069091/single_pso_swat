subroutine calibration
use mod

real,allocatable,dimension(:) :: p_best_obj_fn_val_arr
! type(real_arr_p), allocatable, dimension(:,:) :: p_best_arr3
! type(real_arr_p), allocatable, dimension(:) :: g_best_arr2
real :: g_best_obj_fn_val = -99999999.0
real :: tmp_g_best_obj_fn_val = -99999999.0
real :: fn_obj_val = 0.0

real,dimension(observed_data_rank) :: simu_datas_arr
integer :: best_recur_num=1,best_seed_num=1

integer :: allocate_err1=0,allocate_err2=0,allocate_err3=0,allocate_err4=0

allocate(p_best_obj_fn_val_arr(seed_rank),stat=allocate_err1)
allocate(p_best_arr3(seed_rank,para_rank),stat=allocate_err2)
allocate(g_best_arr2(para_rank),stat=allocate_err3)

if(allocate_err1 + allocate_err2 + allocate_err3 /= 0) then
	write(*,*) 'allocate err in calibration.f90'
	stop
end if

do i=1,seed_rank
	p_best_obj_fn_val_arr(i) = -999999999.0
end do

if(is_use_befor == 1) then
    call system('cp -f ./out/* ./')
    call system('rm -f best_sim_output.txt')
end if

outter:do i=1,recur_rank
    tmp_g_best_obj_fn_val = -99999999.0
    inner:do j=1,seed_rank
        if(.not.(i == 1 .and. j == 1 .and. is_use_befor == 1)) then
		    call modify_para_in_inputfile(j)
        end if
		call system('./swat2012_627 > /dev/null')
		!获得模拟值，并写入文件中
		call extract_swat_output(j,simu_datas_arr)
		if(fn_type == 1) then
			call solve_r2(observed_data_val_arr,simu_datas_arr,observed_data_rank,fn_obj_val)
            write(*,"(' recur num:',I4,'    seed num:',I4,A10,A6,F10.4)") &
                                            i,j,'------->','  R2=',fn_obj_val
		else
			call solve_ns(observed_data_val_arr,simu_datas_arr,observed_data_rank,fn_obj_val)
            write(*,"(' recur num:',I4,'    seed num:',I4,A10,A6,F10.4)") &
                                            i,j,'------->','  NS=',fn_obj_val
		end if
		if(fn_obj_val > p_best_obj_fn_val_arr(j)) then
			p_best_obj_fn_val_arr(j) = fn_obj_val
			do k = 1,para_rank
				p_best_arr3(j,k)%p => para_start_value_3arr(j,k)%p
			end do
			if(fn_obj_val > g_best_obj_fn_val) then
				do k = 1,para_rank
					g_best_arr2(k)%p => para_start_value_3arr(j,k)%p
                    g_best_obj_fn_val = fn_obj_val
                    best_recur_num = i
                    best_seed_num = j
				end do
                do k=1,file_kind_rank
                    call system ('cp -f `ls | grep "[.]'//trim(detail_each_file_include_para_arr(k)%postfile_name(2:))//'$"` ./out')
                end do
                call system('cp -f out/'//trim(obser_val_name)//'.txt out/best_sim_output.txt')
			end if
		end if

        if(fn_obj_val > tmp_g_best_obj_fn_val) then
            tmp_g_best_obj_fn_val = fn_obj_val
        end if

        if(fn_obj_val > obj_fn_threshold) then
            exit outter
        end if
    end do inner

     call date_and_time(b(1), b(2), b(3), date_time2)
     inter_time = date_time2(7)-date_time1(7) + 60*(date_time2(6)-date_time1(6)) &
            + 60*60*(date_time2(5)-date_time1(5)) + 60*60*60*(date_time2(4)-date_time1(4))&
            + 60*60*60*12*(date_time2(3)-date_time1(3))
     write(*,"(' ***************************粒子群算法进化代数:',I4,&
        ' 最大目标函数值:',F8.4,'******',' 耗时:',I6,'s*****************************************')") &
        i,tmp_g_best_obj_fn_val,inter_time

	call update_value_speed()

    end do outter

    call system('rm -f out/'//trim(obser_val_name)//'.txt')

    call date_and_time(b(1), b(2), b(3), date_time2)

    inter_time = date_time2(7)-date_time1(7) + 60*(date_time2(6)-date_time1(6)) &
            + 60*60*(date_time2(5)-date_time1(5)) + 60*60*60*(date_time2(4)-date_time1(4))&
            + 60*60*60*12*(date_time2(3)-date_time1(3))
    write(*,'(/)') 
    write(*,*) '*************************************************************'
    write(*,*) '*************串行粒子群算法率定swat参数**********************'
    write(*,'(/)') 
    write(*,'(" 开始时间: ",A21,"   结束时间: ",A21,"    历时:",I5,A)') &
            (a(1)(1:8)//' '//a(2)(1:2)//':'//a(2)(3:4)//':'//a(2)(5:6)//':'//a(2)(8:10)),&
            (b(1)(1:8)//' '//b(2)(1:2)//':'//b(2)(3:4)//':'//b(2)(5:6)//':'//b(2)(8:10)),inter_time,'s'
    write(*,'(/)') 
    write(*,'(" 率定的参数个数:",I3," 迭代次数:",I5,"  种子个数:",I5)') &
            para_rank,recur_rank,seed_rank
    write(*,'(/)') 
    write(*,'(" the best simulate: the recur_num =",I3,"    the seed_num =",I3)') &
            best_recur_num,best_seed_num
    write(*,'(/)') 
    if(g_best_obj_fn_val < obj_fn_threshold) then
        write(*,'(" best object function value(",F8.4,") not arrive the threshold(",F4.2,")")') &
                g_best_obj_fn_val, obj_fn_threshold
    else    
        write(*,'(" best object function value(",F8.4,") arrive the threshold(",F4.2,")")') &
                g_best_obj_fn_val, obj_fn_threshold
    end if
    write(*,'(/)') 
    write(*,*) '*************************************************************'
    write(*,*) '*************************************************************'

end subroutine calibration
