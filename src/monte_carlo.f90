! File: src/monte_carlo.f90

module monte_carlo
    use random
    use payoff
    implicit none
    private
    public :: monte_carlo_simulate

contains

    function monte_carlo_simulate(n_paths, n_steps) result(final_result)
        integer, intent(in) :: n_paths, n_steps
        real(8) :: final_result
        real(8), dimension(n_steps), intent(out) :: path
        real(8) :: payoff_accumulator
        integer :: i

        payoff_accumulator = 0.0

        ! Run Monte Carlo paths
        do i = 1, n_paths
            call generate_path(path, n_steps)
            payoff_accumulator = payoff_accumulator + calculate_payoff(path)
        end do

        ! Average the payoff
        final_result = payoff_accumulator / real(n_paths, 8)
    end function monte_carlo_simulate

    subroutine generate_path(path, n_steps)
        real(8), dimension(n_steps), intent(out) :: path
        integer, intent(in) :: n_steps
        real(8) :: dt, dW
        integer :: i

        dt = 1.0 / n_steps
        path(1) = 100.0  ! Initial value
        do i = 2, n_steps
            dW = sqrt(dt) * gaussian_random()
            path(i) = path(i - 1) * exp(0.05 * dt + 0.2 * dW)
        end do
    end subroutine generate_path

end module monte_carlo

