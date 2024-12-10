! File: src/payoff.f90

module payoff
    implicit none
    private
    public :: calculate_payoff

contains

    function calculate_payoff(path) result(payoff)
        real(8), dimension(:), intent(in) :: path
        real(8) :: payoff

        payoff = max(path(size(path)) - 100.0_8, 0.0_8)  ! European call option payoff
    end function calculate_payoff

end module payoff

