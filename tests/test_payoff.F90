! File: tests/test_payoff.F90

#include "pfunit.mod"

module test_payoff
    use pfunit
    use payoff
    implicit none

    @test
    subroutine test_positive_payoff()
        real(8), dimension(3) :: path
        real(8) :: payoff_result

        path = [100.0, 110.0, 120.0]  ! Example path
        payoff_result = calculate_payoff(path)

        @assertTrue(payoff_result > 0.0)
    end subroutine test_positive_payoff

    @test
    subroutine test_zero_payoff()
        real(8), dimension(3) :: path
        real(8) :: payoff_result

        path = [100.0, 90.0, 80.0]  ! Example path (below strike price)
        payoff_result = calculate_payoff(path)

        @assertEqual(payoff_result, 0.0)
    end subroutine test_zero_payoff
end module test_payoff

