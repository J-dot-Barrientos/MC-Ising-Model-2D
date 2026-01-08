module spin
        contains
        subroutine spin_initialization(s_array, Num_part)
                integer, intent(in) :: Num_part
                integer, intent(out) :: s_array(:)
                integer :: i

                do i = 1, Num_part
                        s_array(i) = 2 * mod(int(2*r1279()),2)-1    ! Llenar la array de spines con spin aleatorio
                end do

        end subroutine spin_initialization

        subroutine Monte_Carlo_update(nbr_array, s_array, Num_part, table, E, M)
                integer, intent(in) :: nbr_array(:,:), Num_part
                integer, intent(inout) :: s_array(:), M
                double precision, intent(inout) :: E
                double precision, intent(in) :: table(:)
                double precision :: Delta_E
                integer :: Delta_M, S_i, idx

                Delta_M = 2                      ! Diferencia de Magnetizacion entre dos sistema con solo un spin diferente 

                S_i = mod(int(Num_part*r1279()),Num_part)+1         ! Escoger un spin aleatorio de la lista para aplicar Metropolis
            
                Delta_E = 2 * s_array(S_i) * sum(s_array(nbr_array(:, S_i)))   ! Calcular la diferencia de E al cambiar un spin
                
                ! Actualizacion de Metropolis
                
               if (Delta_E < 0) then
                  s_array(S_i) = - s_array(S_i)
                  E = E + Delta_E
                  M = M + Delta_M * s_array(S_i)
                else
                  idx = (Delta_E + 2*4)/2 + 1
                  if (r1279() < table(idx)) then
                    s_array(S_i) = - s_array(S_i)
                    E = E + Delta_E
                    M = M + Delta_M * s_array(S_i)
                  end if
                end if

        end subroutine Monte_Carlo_update
end module spin

