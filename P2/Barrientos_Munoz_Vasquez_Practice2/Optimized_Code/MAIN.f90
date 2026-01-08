PROGRAM P1_MC
        USE table_mod
        USE spin
        USE VARIABLES
        USE MEASUREMENT_OBSERVABLE
        USE lattice

IMPLICIT NONE

  DOUBLE PRECISION :: E
  INTEGER :: M, N, i, x, S_i, Delta_M
  INTEGER, ALLOCATABLE :: nbr_array(:,:), s_array(:), s_possible_array(:)
  DOUBLE PRECISION, ALLOCATABLE :: table(:)
  INTEGER :: idx, meas_step
!  REAL :: r1279

  N = L**2
  E = 0
  M = 0

  call setr1279(num_mes)
  
  allocate(nbr_array(4, N), s_array(N), s_possible_array(N), table(9))


  CALL neighbors(L, nbr_array)
  
  CALL spin_initialization(s_array, N)
  
  CALL write_table(table)

  CALL CALC_E_M(s_array, nbr_array, L, E, M)

  open (unit=10, file="OBS_L100_T2_6_MCS10_8.dat", status="replace")

  ! call cpu_time(t_start)
  do x = 1, num_MCS
      do i = 1, N
          CALL Monte_Carlo_update(nbr_array, s_array, N, table, E, M)
      end do
          
      if (mod(x, num_mes) == 0) then
          CALL Output(E, M, x)
      end if
  end do

  ! call cpu_time(t_end)
  ! t_elapsed = t_end - t_start
  ! fps = dble(num_MCS) * dble(N) / t_elapsed

  print *, "MC Simulation completed"

  ! print *, "CPU time (s) =", t_elapsed
  ! print *, "Attempted flips =", total_flips
  ! print *, "MC speed =", fps, " flips/s"

END PROGRAM P1_MC

  SUBROUTINE Output(E,M,x)
  IMPLICIT NONE

    DOUBLE PRECISION, INTENT(in) :: E
    INTEGER, INTENT(in) :: M,x

    write(10, *) E, M
          print *, x
 
 END SUBROUTINE Output
