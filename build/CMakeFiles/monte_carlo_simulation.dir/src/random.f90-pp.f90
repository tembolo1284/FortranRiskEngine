# 1 "/root/fortran-workspace/FortranRiskEngine/src/random.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/root/fortran-workspace/FortranRiskEngine/src/random.f90"
! File: src/random.f90

module random
    implicit none
    private
    public :: gaussian_random

contains

    function gaussian_random() result(rand)
        real(8) :: rand
        real(8) :: u1, u2

        call random_number(u1)
        call random_number(u2)

        rand = sqrt(-2.0 * log(u1)) * cos(2.0 * 3.141592653589793 * u2)
    end function gaussian_random

end module random

