! File: src/random.f90

module random
    use iso_fortran_env, only: real64, int32
    implicit none
    private

    ! Public interfaces
    public :: random_normal
    public :: set_seed

contains
    subroutine set_seed(seed)
        integer(int32), intent(in) :: seed
        integer(int32), allocatable :: seed_array(:)
        integer(int32) :: n
        
        call random_seed(size=n)
        allocate(seed_array(n))
        seed_array = seed
        call random_seed(put=seed_array)
    end subroutine set_seed

    subroutine random_normal(x)
        real(real64), dimension(:), intent(out) :: x
        real(real64), dimension(size(x)) :: u1, u2
        real(real64), parameter :: PI = 3.14159265358979323846_real64
        
        call random_number(u1)
        call random_number(u2)
        
        ! Box-Muller transform
        x = sqrt(-2.0_real64 * log(u1)) * cos(2.0_real64 * PI * u2)
    end subroutine random_normal
end module random
