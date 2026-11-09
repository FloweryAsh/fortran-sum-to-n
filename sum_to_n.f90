! sum_to_n.f90
! 計算 1 + 2 + ... + N，並用高斯公式 N(N+1)/2 交叉驗證
program sum_to_n                                    ! 主程式開始，程式名稱為 sum_to_n
    use, intrinsic :: iso_fortran_env, only: int64  ! 從標準內建模組匯入 int64（64 位元整數的 kind 參數）
    implicit none                                   ! 關閉隱式型別：所有變數都必須明確宣告，避免打錯字變成新變數

    integer(int64) :: n, i, total, formula          ! 宣告 64 位元整數：n=上限、i=迴圈計數、total=迴圈總和、formula=公式結果
    integer :: ios                                  ! 宣告一般整數 ios，用來接收讀取輸入時的狀態碼（0 代表成功）

    write(*, '(A)', advance='no') '請輸入正整數 N: ' ! 輸出提示字串；advance='no' 讓游標停在同一行，不換行
    read(*, *, iostat=ios) n                        ! 從標準輸入讀取 n；iostat 讓讀取失敗時不當機，而是把錯誤碼存進 ios

    if (ios /= 0) then                              ! 若 ios 不等於 0，代表讀取失敗（例如輸入的不是整數）
        print *, '錯誤：請輸入整數。'                ! 輸出錯誤訊息
        stop 1                                      ! 結束程式並回傳狀態碼 1（表示異常結束）
    end if                                          ! 結束此 if 區塊
    if (n < 1) then                                 ! 若 n 小於 1，不符合「正整數」的要求
        print *, '錯誤：N 必須 >= 1。'               ! 輸出錯誤訊息
        stop 1                                      ! 結束程式並回傳狀態碼 1
    end if                                          ! 結束此 if 區塊

    ! 方法一：迴圈逐項相加
    total = 0_int64                                 ! 總和初始化為 0；後綴 _int64 讓這個常數也是 64 位元
    do i = 1_int64, n                               ! 迴圈：i 從 1 跑到 n（含 n），每次加 1
        total = total + i                           ! 把目前的 i 累加到 total
    end do                                          ! 迴圈結束

    ! 方法二：高斯公式
    formula = n * (n + 1_int64) / 2_int64           ! 以 N(N+1)/2 直接計算；先乘後除，因 N(N+1) 必為偶數所以整除無誤差

    print '(A,I0,A,I0)', '1 + 2 + ... + ', n, ' = ', total  ! 依格式輸出：A=字串，I0=以最短寬度輸出整數
    print '(A,I0)',      '公式 N(N+1)/2 驗證 = ', formula    ! 輸出公式計算的結果

    if (total == formula) then                      ! 比較兩種方法的結果是否相同
        print *, '兩種方法結果一致 ✓'                ! 相同：輸出確認訊息
    else                                            ! 否則（結果不同）
        print *, '警告：結果不一致！'                ! 輸出警告訊息
    end if                                          ! 結束此 if 區塊
end program sum_to_n                                ! 主程式結束
