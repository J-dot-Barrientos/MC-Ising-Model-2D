PROGRAM P2_Binning
  IMPLICIT NONE

  INTEGER :: length_bin
  REAL    :: Avg, Var
  INTEGER :: unit_out

  unit_out = 20
  OPEN(unit_out, file="binning_output.dat", status="replace")

  ! Cabecera del fichero
  WRITE(unit_out,*) "Resultados de varianza vs tamaño de bin"
  WRITE(unit_out,*) "length_bin        Avg                Var"

  length_bin = 1
  DO WHILE (length_bin <= 10000)

     CALL Binning(length_bin, Avg, Var)

     ! Escribir: tamaño del bin, media y varianza
     WRITE(unit_out,'(I8,3X,ES15.6,3X,ES15.6)') length_bin, Avg, Var

     length_bin = length_bin * 2   ! 1,2,4,8,...
  END DO

  CLOSE(unit_out)

  PRINT *, "Resultados escritos en binning_output.dat"

CONTAINS

  SUBROUTINE Binning(length_bin, Avg, Var)
    IMPLICIT NONE
    INTEGER, INTENT(in)  :: length_bin
    REAL,    INTENT(out) :: Avg, Var

    CHARACTER(len=50) :: fname
    INTEGER :: i, num_dat, ios
    REAL, ALLOCATABLE :: array_value(:)

    num_dat = 100000
    ALLOCATE(array_value(num_dat))

    fname = "test3.dat"

    OPEN(unit=1, file=fname, status="old", action='read', iostat=ios)
    IF (ios /= 0) THEN
      PRINT *, "Error abriendo el fichero ", fname
      STOP
    END IF

    ! Leer los datos
    DO i = 1, num_dat
      READ(1, *, iostat=ios) array_value(i)
      IF (ios /= 0) THEN
        num_dat = i - 1
        EXIT
      END IF
    END DO
    CLOSE(1)

    ! Media global
    Avg = SUM(array_value(1:num_dat)) / REAL(num_dat)

    ! Varianza por binning
    Var = Calc_Var(array_value(1:num_dat), length_bin, num_dat, Avg)

    DEALLOCATE(array_value)
  END SUBROUTINE Binning


  REAL FUNCTION Calc_Var(array_value, length_bin, num_dat, Avg)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length_bin, num_dat
    REAL,    INTENT(in) :: array_value(:), Avg

    INTEGER :: i, k, h, num_bins
    REAL, ALLOCATABLE :: avg_bins(:)
    REAL :: val_acum, sumsq

    num_bins = num_dat / length_bin

    IF (num_bins <= 1) THEN
      Calc_Var = 0.0
      RETURN
    END IF

    ALLOCATE(avg_bins(num_bins))

    ! Calcular media en cada bin
    DO k = 1, num_bins
      val_acum = 0.0
      DO i = 1, length_bin
        val_acum = val_acum + array_value((k-1)*length_bin + i)
      END DO
      avg_bins(k) = val_acum / REAL(length_bin)
    END DO

    ! Varianza de los bins respecto a la media global
    sumsq = 0.0
    DO h = 1, num_bins
      sumsq = sumsq + (avg_bins(h) - Avg)**2
    END DO

    Calc_Var = sumsq / REAL(num_bins * (num_bins - 1))

    DEALLOCATE(avg_bins)
  END FUNCTION Calc_Var

END PROGRAM P2_Binning

