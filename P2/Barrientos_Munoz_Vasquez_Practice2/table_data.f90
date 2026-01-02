module table_mod
        use Measurement_Observable
        use Variables

        contains
        subroutine write_table(table) 
                integer, intent(out) :: table(:)
                integer :: n, dmn, z
                real*8 :: beta

                beta = 1 / temp                
                dmn = 2
                z = dmn*2
                do n = -2*z, 2*z, 2
                        table(n) = exp(-beta * n)
                end do
        end subroutine write_table
end module table_mod
