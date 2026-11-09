! sum_to_n.f90
! 計算 1 + 2 + ... + N，並用高斯公式 N(N+1)/2 交叉驗證
program sum_to_n
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer(int64) :: n, i, total, formula
    integer :: ios

    write(*, '(A)', advance='no') '請輸入正整數 N: '
    read(*, *, iostat=ios) n

    if (ios /= 0) then
        print *, '錯誤：請輸入整數。'
        stop 1
    end if
    if (n < 1) then
        print *, '錯誤：N 必須 >= 1。'
        stop 1
    end if

    ! 方法一：迴圈逐項相加
    total = 0_int64
    do i = 1_int64, n
        total = total + i
    end do

    ! 方法二：高斯公式
    formula = n * (n + 1_int64) / 2_int64

    print '(A,I0,A,I0)', '1 + 2 + ... + ', n, ' = ', total
    print '(A,I0)',      '公式 N(N+1)/2 驗證 = ', formula

    if (total == formula) then
        print *, '兩種方法結果一致 ✓'
    else
        print *, '警告：結果不一致！'
    end if
end program sum_to_n
