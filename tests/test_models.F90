! File: tests/test_models.F90

#include "pfunit.mod"

module test_models
    use pfunit
    use models
    implicit none

    @test
    subroutine test_black_scholes()
        real(8) :: S, K, T, r, sigma, price

        ! Example parameters
        S = 100.0
        K = 100.0
        T = 1.0
        r = 0.05
        sigma = 0.2

        price = black_scholes(S, K, T, r, sigma)
        @assertTrue(price > 0.0)
    end subroutine test_black_scholes
end module test_models

