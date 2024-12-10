! File: src/models.f90

module models
    use iso_fortran_env, only: real64, int32
    implicit none
    private

    ! Public interfaces
    public :: black_scholes_model

contains
    function black_scholes_model(spot, strike, r, vol, t) result(price)
        real(real64), intent(in) :: spot, strike, r, vol, t
        real(real64) :: price, d1, d2
        
        d1 = (log(spot/strike) + (r + vol**2/2.0_real64)*t)/(vol*sqrt(t))
        d2 = d1 - vol*sqrt(t)
        
        price = spot*normal_cdf(d1) - strike*exp(-r*t)*normal_cdf(d2)
    end function black_scholes_model

    ! Helper function for Black-Scholes
    function normal_cdf(x) result(cdf)
        real(real64), intent(in) :: x
        real(real64) :: cdf
        real(real64) :: t, y
        real(real64), parameter :: a1 = 0.254829592_real64
        real(real64), parameter :: a2 = -0.284496736_real64
        real(real64), parameter :: a3 = 1.421413741_real64
        real(real64), parameter :: a4 = -1.453152027_real64
        real(real64), parameter :: a5 = 1.061405429_real64
        real(real64), parameter :: p = 0.3275911_real64

        t = 1.0_real64/(1.0_real64 + p*abs(x))
        y = 1.0_real64 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x/2.0_real64)
        
        if (x < 0.0_real64) then
            cdf = y/2.0_real64
        else
            cdf = 1.0_real64 - y/2.0_real64
        end if
    end function normal_cdf
end module models
