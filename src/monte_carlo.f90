! File: src/monte_carlo.f90

module monte_carlo
    use iso_fortran_env, only: real64, int32
    use models, only: black_scholes_model
    use random, only: random_normal
    use payoff, only: option_payoff, immediate_exercise_value, asian_payoff
    implicit none
    private

    ! Public interfaces
    public :: generate_path
    public :: monte_carlo_simulation
    public :: price_option
    public :: price_american_option
    public :: price_option_antithetic
    public :: price_asian_option
    public :: price_bermudan_option

    ! Constants for statistical calculations
    real(real64), parameter :: CONFIDENCE_95 = 1.96_real64

contains
    subroutine generate_path(n_steps, dt, spot, vol, r, path)
        integer(int32), intent(in) :: n_steps
        real(real64), intent(in) :: dt, spot, vol, r
        real(real64), dimension(n_steps), intent(out) :: path
        
        real(real64), dimension(n_steps) :: dW
        integer :: i
        
        ! Generate Wiener process increments
        call random_normal(dW)
        dW = dW * sqrt(dt)
        
        ! Initialize path with spot price
        path(1) = spot
        
        ! Generate price path using Black-Scholes model
        do i = 2, n_steps
            path(i) = path(i-1) * exp((r - 0.5_real64 * vol**2) * dt + vol * dW(i))
        end do
    end subroutine generate_path

    subroutine generate_antithetic_paths(n_steps, dt, spot, vol, r, path, path_anti)
        integer(int32), intent(in) :: n_steps
        real(real64), intent(in) :: dt, spot, vol, r
        real(real64), dimension(n_steps), intent(out) :: path, path_anti
        
        real(real64), dimension(n_steps) :: dW
        integer :: i
        
        ! Generate Wiener process increments
        call random_normal(dW)
        dW = dW * sqrt(dt)
        
        ! Initialize paths with spot price
        path(1) = spot
        path_anti(1) = spot
        
        ! Generate price paths using Black-Scholes model
        do i = 2, n_steps
            ! Regular path
            path(i) = path(i-1) * exp((r - 0.5_real64 * vol**2) * dt + vol * dW(i))
            ! Antithetic path (using negative of the random increment)
            path_anti(i) = path_anti(i-1) * exp((r - 0.5_real64 * vol**2) * dt - vol * dW(i))
        end do
    end subroutine generate_antithetic_paths

    function monte_carlo_simulation(n_paths, n_steps, dt, spot, vol, r) result(paths)
        integer(int32), intent(in) :: n_paths, n_steps
        real(real64), intent(in) :: dt, spot, vol, r
        real(real64), dimension(n_paths, n_steps) :: paths
        
        integer :: i
        !$OMP PARALLEL DO PRIVATE(i)
        do i = 1, n_paths
            call generate_path(n_steps, dt, spot, vol, r, paths(i,:))
        end do
        !$OMP END PARALLEL DO
    end function monte_carlo_simulation

    function monte_carlo_simulation_antithetic(n_paths, n_steps, dt, spot, vol, r) result(paths)
        integer(int32), intent(in) :: n_paths, n_steps  ! n_paths should be even
        real(real64), intent(in) :: dt, spot, vol, r
        real(real64), dimension(n_paths, n_steps) :: paths
        
        integer :: i
        !$OMP PARALLEL DO PRIVATE(i)
        do i = 1, n_paths/2
            call generate_antithetic_paths(n_steps, dt, spot, vol, r, &
                                         paths(2*i-1,:), paths(2*i,:))
        end do
        !$OMP END PARALLEL DO
    end function monte_carlo_simulation_antithetic

    function price_option(option_type, strike, paths, r, dt, n_steps) result(price)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: strike, r, dt
        integer(int32), intent(in) :: n_steps
        real(real64), dimension(:,:), intent(in) :: paths
        real(real64) :: price
        
        integer :: n_paths, i
        real(real64), dimension(size(paths, 1)) :: payoffs
        real(real64) :: mean_payoff, variance, std_error
        
        n_paths = size(paths, 1)
        
        !$OMP PARALLEL DO PRIVATE(i)
        do i = 1, n_paths
            payoffs(i) = option_payoff(option_type, paths(i,n_steps), strike)
        end do
        !$OMP END PARALLEL DO
        
        ! Apply discount factor
        payoffs = payoffs * exp(-r * dt * n_steps)
        
        ! Calculate price and error estimates
        mean_payoff = sum(payoffs) / n_paths
        variance = sum((payoffs - mean_payoff)**2) / (n_paths - 1)
        std_error = sqrt(variance / n_paths)
        
        price = mean_payoff
        
        ! Print error estimates
        print '(A,F10.5)', " Standard Error:", std_error
        print '(A,F10.5)', " 95% Confidence Interval: ±", CONFIDENCE_95 * std_error
    end function price_option

    function price_option_antithetic(option_type, strike, r, dt, n_steps, n_paths, spot, vol) result(price)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: strike, r, dt, spot, vol
        integer(int32), intent(in) :: n_steps, n_paths
        real(real64) :: price
        
        real(real64), dimension(:,:), allocatable :: paths
        real(real64), dimension(:), allocatable :: payoffs
        real(real64) :: mean_payoff, variance, std_error
        integer :: effective_paths, i
        
        ! Ensure n_paths is even for antithetic pairs
        effective_paths = (n_paths + 1) / 2 * 2
        
        ! Allocate arrays
        allocate(paths(effective_paths, n_steps))
        allocate(payoffs(effective_paths))
        
        ! Generate paths using antithetic variates
        paths = monte_carlo_simulation_antithetic(effective_paths, n_steps, dt, spot, vol, r)
        
        ! Calculate payoffs
        !$OMP PARALLEL DO PRIVATE(i)
        do i = 1, effective_paths
            payoffs(i) = option_payoff(option_type, paths(i,n_steps), strike)
        end do
        !$OMP END PARALLEL DO
        
        ! Apply discount factor
        payoffs = payoffs * exp(-r * dt * n_steps)
        
        ! Calculate price and error estimates
        mean_payoff = sum(payoffs) / effective_paths
        variance = sum((payoffs - mean_payoff)**2) / (effective_paths - 1)
        std_error = sqrt(variance / effective_paths)
        
        price = mean_payoff
        
        ! Print error estimates
        print '(A,F10.5)', " Standard Error:", std_error
        print '(A,F10.5)', " 95% Confidence Interval: ±", CONFIDENCE_95 * std_error
        
        deallocate(paths, payoffs)
    end function price_option_antithetic

    function price_asian_option(option_type, strike, paths, r, dt, n_steps) result(price)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: strike, r, dt
        integer(int32), intent(in) :: n_steps
        real(real64), dimension(:,:), intent(in) :: paths
        real(real64) :: price
        
        integer :: n_paths, i
        real(real64), dimension(size(paths, 1)) :: payoffs, arithmetic_means, geometric_means
        real(real64) :: mean_payoff, variance, std_error
        real(real64), dimension(n_steps) :: temp_path
        
        n_paths = size(paths, 1)
        
        !$OMP PARALLEL DO PRIVATE(i, temp_path)
        do i = 1, n_paths
            temp_path = paths(i,:)
            
            ! Calculate arithmetic and geometric means
            select case(trim(option_type))
                case('asian_call', 'asian_put')
                    arithmetic_means(i) = sum(temp_path) / n_steps
                    payoffs(i) = asian_payoff(option_type, arithmetic_means(i), strike)
                
                case('geometric_asian_call', 'geometric_asian_put')
                    geometric_means(i) = exp(sum(log(temp_path)) / n_steps)
                    payoffs(i) = asian_payoff(option_type, geometric_means(i), strike)
            end select
        end do
        !$OMP END PARALLEL DO
        
        ! Apply discount factor
        payoffs = payoffs * exp(-r * dt * n_steps)
        
        ! Calculate price and error estimates
        mean_payoff = sum(payoffs) / n_paths
        variance = sum((payoffs - mean_payoff)**2) / (n_paths - 1)
        std_error = sqrt(variance / n_paths)
        
        price = mean_payoff
        
        ! Print error estimates
        print '(A,F10.5)', " Standard Error:", std_error
        print '(A,F10.5)', " 95% Confidence Interval: ±", CONFIDENCE_95 * std_error
    end function price_asian_option

    function price_bermudan_option(option_type, strike, paths, r, dt, n_steps, exercise_dates) result(price)
        character(len=*), intent(in) :: option_type
        real(real64), intent(in) :: strike, r, dt
        integer(int32), intent(in) :: n_steps
        real(real64), dimension(:,:), intent(in) :: paths
        integer(int32), dimension(:), intent(in) :: exercise_dates
        real(real64) :: price
        
        integer :: n_paths, i, j
        real(real64), dimension(size(paths, 1)) :: exercise_values, continuation_values
        logical, dimension(size(paths, 1)) :: exercise_flags
        real(real64), dimension(size(paths, 1)) :: discounted_payoffs
        real(real64) :: std_error, variance, mean_payoff
        integer :: date_idx
        
        n_paths = size(paths, 1)
        
        ! Initialize arrays
        exercise_flags = .false.
        discounted_payoffs = 0.0_real64
        
        ! Backward induction through the exercise dates
        do date_idx = size(exercise_dates), 1, -1
            j = exercise_dates(date_idx)
            
            ! Calculate immediate exercise values at current exercise date
            do i = 1, n_paths
                exercise_values(i) = immediate_exercise_value(option_type, paths(i,j), strike)
            end do
            
            ! Only consider paths that are in-the-money
            where (exercise_values > 0.0_real64)
                ! Calculate continuation value using least squares regression
                continuation_values = estimate_continuation_value(paths(:,j), &
                                   discounted_payoffs, exercise_values > 0.0_real64)
                
                ! Exercise when immediate exercise value exceeds continuation value
                exercise_flags = exercise_values > continuation_values
            end where
            
            ! Update payoffs
            where (exercise_flags)
                discounted_payoffs = exercise_values * exp(-r * dt * (j-1))
            end where
        end do
        
        ! Calculate price and error estimates
        mean_payoff = sum(discounted_payoffs) / n_paths
        variance = sum((discounted_payoffs - mean_payoff)**2) / (n_paths - 1)
        std_error = sqrt(variance / n_paths)
        
        price = mean_payoff
        
        ! Print error estimates
        print '(A,F10.5)', " Standard Error:", std_error
        print '(A,F10.5)', " 95% Confidence Interval: ±", CONFIDENCE_95 * std_error
    end function price_bermudan_option

    function estimate_continuation_value(stock_prices, future_payoffs, itm_mask) result(continuation_values)
        real(real64), dimension(:), intent(in) :: stock_prices
        real(real64), dimension(:), intent(in) :: future_payoffs
        logical, dimension(:), intent(in) :: itm_mask
        real(real64), dimension(size(stock_prices)) :: continuation_values
        
        real(real64) :: x_mean, y_mean, xy_mean, x2_mean
        real(real64) :: beta0, beta1
        integer :: n_itm, i, itm_idx
        real(real64), dimension(:), allocatable :: x_itm, y_itm
        
        ! Count in-the-money paths
        n_itm = count(itm_mask)
        
        if (n_itm > 1) then
            ! Extract in-the-money values
            allocate(x_itm(n_itm), y_itm(n_itm))
            itm_idx = 1
            do i = 1, size(stock_prices)
                if (itm_mask(i)) then
                    x_itm(itm_idx) = stock_prices(i)
                    y_itm(itm_idx) = future_payoffs(i)
                    itm_idx = itm_idx + 1
                end if
            end do
            
            ! Calculate means for regression
            x_mean = sum(x_itm) / n_itm
            y_mean = sum(y_itm) / n_itm
            xy_mean = sum(x_itm * y_itm) / n_itm
            x2_mean = sum(x_itm * x_itm) / n_itm
            
            ! Calculate regression coefficients
            beta1 = (xy_mean - x_mean * y_mean) / (x2_mean - x_mean * x_mean)
            beta0 = y_mean - beta1 * x_mean
            
            ! Calculate continuation values for all paths
            continuation_values = beta0 + beta1 * stock_prices
            
            deallocate(x_itm, y_itm)
        else
            continuation_values = 0.0_real64
        end if
    end function estimate_continuation_value

end module monte_carlo
