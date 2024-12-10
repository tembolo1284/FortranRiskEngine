# 1 "/root/fortran-workspace/FortranRiskEngine/src/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/root/fortran-workspace/FortranRiskEngine/src/main.f90"
! File: src/main.f90

program monte_carlo_main
    use monte_carlo
    implicit none

    ! Simulation parameters
    integer, parameter :: n_paths = 1000000, n_steps = 365
    real(8) :: result

    print *, "Running Monte Carlo simulation with ", n_paths, " paths and ", n_steps, " steps."

    ! Run simulation and display result
    result = monte_carlo_simulate(n_paths, n_steps)
    print *, "Simulation result: ", result
end program monte_carlo_main

