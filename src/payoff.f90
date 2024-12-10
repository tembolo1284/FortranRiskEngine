! File: src/payoff.f90

module payoff
    use iso_fortran_env, only: real64
    implicit none
    private

    ! Public interfaces
    public :: option_payoff
    public :: immediate_exercise_value
    public :: asian_payoff

contains
    function option_payoff(option_type, spot, strike) result(payoff)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: spot, strike
        real(real64) :: payoff
        
        select case(trim(option_type))
            case('call', 'american_call', 'bermudan_call')
                payoff = max(spot - strike, 0.0_real64)
            case('put', 'american_put', 'bermudan_put')
                payoff = max(strike - spot, 0.0_real64)
            case default
                payoff = 0.0_real64
                print *, 'Warning: Unknown option type ', trim(option_type)
        end select
    end function option_payoff

    ! Function for Asian option payoffs
    function asian_payoff(option_type, average_price, strike) result(payoff)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: average_price, strike
        real(real64) :: payoff
        
        select case(trim(option_type))
            case('asian_call')
                payoff = max(average_price - strike, 0.0_real64)
            case('asian_put')
                payoff = max(strike - average_price, 0.0_real64)
            case('geometric_asian_call')
                payoff = max(average_price - strike, 0.0_real64)
            case('geometric_asian_put')
                payoff = max(strike - average_price, 0.0_real64)
            case default
                payoff = 0.0_real64
                print *, 'Warning: Unknown Asian option type ', trim(option_type)
        end select
    end function asian_payoff

    function immediate_exercise_value(option_type, spot, strike) result(value)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: spot, strike
        real(real64) :: value
        
        select case(trim(option_type))
            case('american_call', 'bermudan_call')
                value = max(spot - strike, 0.0_real64)
            case('american_put', 'bermudan_put')
                value = max(strike - spot, 0.0_real64)
            case default
                value = 0.0_real64
        end select
    end function immediate_exercise_value
end module payoff
