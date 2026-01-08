PROGRAM P2_Binning

! Codigo que aplica Binning a un archivo que contiene datos a analizar (un dato por linea y sin cabecera) y
! produce un fichero con : Tamano del bloque - Media de los datos - Varianza de los datos agrupados en bloques
! Hay que especificar el nombre del archivo en SUBROUTINE Binning
! Hay que especificar el limite superior de los bloques del binning en PROGRAM P2_Binning
! Se considera Var = s

  IMPLICIT NONE

  INTEGER :: length_bin
  REAL*8    :: Avg, Var


  ! Cuerpo del programa

  OPEN(1, file="OBS_L100_T2_6_MCS10_8_output.dat", status="replace")          ! Output

    WRITE(1,*) "Resultados de varianza vs tamaño de bin"
    WRITE(1,*) "length_bin        Avg                Er. est.        Er.est./Num_spin       Var"

    length_bin = 1
    
    DO WHILE (length_bin <= 100000)         ! Especificar limite superior de los bloques

      CALL Binning(length_bin, Avg, Var)  ! Funcion de Binning : Obtienes Media y Varianza para un tamano de bloque especificado

      WRITE(1, '(I8,3X,ES15.6,3X,ES15.6,3X,ES15.6,3X,ES15.6)') length_bin, Avg, Var, Var/10000, Var**2  ! Escribir los datos

      length_bin = length_bin * 2         ! Tamanos del bloque : 1, 2, ..., 512
    END DO

  CLOSE(1)

  ! Print de final del analisis

  PRINT *, "Resultados escritos en binning_output.dat"


CONTAINS

! -----------------------------------------------------

  SUBROUTINE Binning(length_bin, Avg, Var)
 
  ! Subrutina que:
    ! 1 - Lee los datos
    ! 2 - Calcula la media global de los datos
    ! 3 - Llama a la funcion Calc_Var

    IMPLICIT NONE
    INTEGER, INTENT(in)  :: length_bin
    REAL*8, INTENT(out) :: Avg, Var

    CHARACTER(len=50) :: fname
    INTEGER :: j,i, num_dat, ios, num_dat_max
    REAL*8, ALLOCATABLE :: array_value(:)
    REAL :: basura

    ALLOCATE(array_value(num_dat))

    fname = "OBS_L100_T2_6_MCS10_8.dat"                 ! Especificar nombre del fichero que contiene los datos
 

    ! Print si el archivo no se puede leer

    OPEN(unit=2, file=fname, status="old", action='read', iostat=ios)
    IF (ios /= 0) THEN
      PRINT *, "Error abriendo el fichero ", fname
      STOP
    END IF

    ! No leer datos

    DO j = 1, 100
       READ(2, *, iostat=ios)
    END DO

    ! Leer los datos
   
    DO i = 1, num_dat
      READ(2, *, iostat=ios) basura, array_value(i)
      IF (ios /= 0) THEN
        num_dat = i - 1  
        EXIT
      END IF
    END DO
    CLOSE(2)

    ! Media global

    Avg = SUM(array_value(1:num_dat)) / REAL(num_dat)

    ! Varianza para un tamano de bloque especificado

    Var = Calc_Var(array_value(1:num_dat), length_bin, num_dat, Avg)


  END SUBROUTINE Binning

! ------------------------------------------------------------------

  REAL*8 FUNCTION Calc_Var(array_value, length_bin, num_dat, Avg)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: length_bin, num_dat
    REAL*8,    INTENT(in) :: array_value(:), Avg

    INTEGER :: i, k, h, num_bins
    REAL*8, ALLOCATABLE :: avg_bins(:)
    REAL*8 :: val_acum, sumsq, Var_2

    num_bins = num_dat / length_bin

    ALLOCATE(avg_bins(num_bins))


    ! Calcular media de cada bloque

    DO k = 1, num_bins
      val_acum = 0.0
      DO i = 1, length_bin
        val_acum = val_acum + array_value((k-1)*length_bin + i)
      END DO
      avg_bins(k) = val_acum / REAL(length_bin)
    END DO

    ! Suma de cuadrados de la diferencia entre la media de cada bloque y la media global

    sumsq = 0.0
    DO h = 1, num_bins
      sumsq = sumsq + (avg_bins(h) - Avg)**2
    END DO

    ! Calculo de la Varianza de la muestra con datos en un tamano de bloque especificado

    Var_2 = sumsq / (REAL(num_bins)) * 1/ (num_bins - 1)
    Calc_Var = SQRT(Var_2)


    DEALLOCATE(avg_bins)


  END FUNCTION Calc_Var

END PROGRAM P2_Binning

