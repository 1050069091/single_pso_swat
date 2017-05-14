program main
use mod
implicit none

call date_and_time(a(1), a(2), a(3), date_time1)

!获取要率定的参数信息
call read_para_info()

!读取需要抽取的模拟结果信息
call read_extract_info()

!初始化需要率定参数的值：在各参数范围内随机生成
call initialize_para_val()

!获得每个sub中含有的hru信息
call read_sub_hru()

!获得观测数据
call read_observed()

!开始率定
call calibration()

end program main
