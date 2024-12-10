! File: tests/test_monte_carlo.F90

#include "pfunit.mod"

module test_monte_carlo
    use pfunit
    use monte_carlo
    implicit none

    @test
    subroutine test_simple_monte_carlo()
        real(8) :: result

        result = monte_carlo_simulate(1000, 10)  ! Example simulation
        @assertTrue(result > 0.0)
    end subroutine test_simple_monte_carlo
end module test_monte_carlo

