! File: tests/test_random.F90

#include "pfunit.mod"

module test_random
    use pfunit
    use random
    implicit none

    @test
    subroutine test_random_in_range()
        real(8) :: rand

        rand = gaussian_random()
        @assertTrue(rand >= -5.0 .and. rand <= 5.0)
    end subroutine test_random_in_range
end module test_random

