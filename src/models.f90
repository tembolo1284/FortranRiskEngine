! File: src/models.f90

module models
    implicit none
    private
    public :: black_scholes

contains

    function black_scholes(S, K, T, r, sigma) result(price)
        real(8), intent(in) :: S, K, T, r, sigma
        real(8) :: price
        real(8) :: d1, d2

        d1 = (log(S / K) + (r + 0.5 * sigma**2) * T) / (sigma * sqrt(T))
        d2 = d1 - sigma * sqrt(T)

        price = S * cdf_normal(d1) - K * exp(-r * T) * cdf_normal(d2)
    end function black_scholes

    function cdf_normal(x) result(cdf)
        real(8), intent(in) :: x
        real(8) :: cdf

        cdf = 0.5 * (1.0 + erf(x / sqrt(2.0)))
    end function cdf_normal

end module models

