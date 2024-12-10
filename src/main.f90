! File: src/main.f90

program main
    use iso_fortran_env, only: real64, int32
    use monte_carlo, only: monte_carlo_simulation, price_option, price_american_option, &
                          price_option_antithetic, price_asian_option, price_bermudan_option
    use random, only: set_seed
    implicit none

    ! Simulation parameters
    integer(int32), parameter :: n_paths = 10000
    integer(int32), parameter :: n_steps = 252  ! Daily steps for a year
    real(real64), parameter :: T = 1.0_real64   ! Time to maturity (1 year)
    real(real64), parameter :: dt = T/n_steps   ! Time step
    real(real64), parameter :: spot = 100.0_real64  ! Initial stock price
    real(real64), parameter :: strike = 100.0_real64  ! Strike price
    real(real64), parameter :: r = 0.05_real64  ! Risk-free rate
    real(real64), parameter :: vol = 0.2_real64 ! Volatility

    ! Arrays for paths and results
    real(real64), dimension(:,:), allocatable :: paths
    real(real64) :: eu_call_price, eu_put_price
    real(real64) :: am_call_price, am_put_price
    real(real64) :: eu_call_price_anti, eu_put_price_anti
    real(real64) :: asian_call_price, asian_put_price
    real(real64) :: geom_asian_call_price, geom_asian_put_price
    real(real64) :: berm_call_price, berm_put_price
    
    ! Bermudan exercise dates (monthly)
    integer(int32), dimension(12) :: exercise_dates
    integer :: i

    ! Initialize random seed
    call set_seed(123456)

    ! Set up monthly exercise dates for Bermudan options
    do i = 1, 12
        exercise_dates(i) = i * (n_steps / 12)
    end do

    ! Allocate memory for paths
    allocate(paths(n_paths, n_steps))

    ! Generate paths
    paths = monte_carlo_simulation(n_paths, n_steps, dt, spot, vol, r)

    ! Price European options (standard Monte Carlo)
    print *, "Standard Monte Carlo Results"
    print *, "=========================="
    eu_call_price = price_option("call", strike, paths, r, dt, n_steps)
    eu_put_price = price_option("put", strike, paths, r, dt, n_steps)

    ! Price American options
    print *, ""
    print *, "American Option Results"
    print *, "======================"
    am_call_price = price_american_option("american_call", strike, paths, r, dt, n_steps)
    am_put_price = price_american_option("american_put", strike, paths, r, dt, n_steps)

    ! Price European options with antithetic variates
    print *, ""
    print *, "Antithetic Variates Results"
    print *, "=========================="
    eu_call_price_anti = price_option_antithetic("call", strike, r, dt, n_steps, n_paths, spot, vol)
    eu_put_price_anti = price_option_antithetic("put", strike, r, dt, n_steps, n_paths, spot, vol)

    ! Price Asian options
    print *, ""
    print *, "Asian Option Results"
    print *, "==================="
    asian_call_price = price_asian_option("asian_call", strike, paths, r, dt, n_steps)
    asian_put_price = price_asian_option("asian_put", strike, paths, r, dt, n_steps)
    geom_asian_call_price = price_asian_option("geometric_asian_call", strike, paths, r, dt, n_steps)
    geom_asian_put_price = price_asian_option("geometric_asian_put", strike, paths, r, dt, n_steps)

    ! Price Bermudan options
    print *, ""
    print *, "Bermudan Option Results"
    print *, "======================"
    berm_call_price = price_bermudan_option("bermudan_call", strike, paths, r, dt, n_steps, exercise_dates)
    berm_put_price = price_bermudan_option("bermudan_put", strike, paths, r, dt, n_steps, exercise_dates)

    ! Output all results with formatted printing
    print *, ""
    print *, "Simulation Parameters:"
    print *, "===================="
    print '(A,I10)', " Number of paths:", n_paths
    print '(A,I10)', " Number of steps:", n_steps
    print '(A,F10.5)', " Time to maturity:", T
    print '(A,F10.5)', " Initial stock price:", spot
    print '(A,F10.5)', " Strike price:", strike
    print '(A,F10.5)', " Risk-free rate:", r
    print '(A,F10.5)', " Volatility:", vol
    
    print *, ""
    print *, "Option Prices:"
    print *, "============="
    print '(A,F10.5)', " European Call (std MC):", eu_call_price
    print '(A,F10.5)', " European Put (std MC):", eu_put_price
    print '(A,F10.5)', " European Call (antithetic):", eu_call_price_anti
    print '(A,F10.5)', " European Put (antithetic):", eu_put_price_anti
