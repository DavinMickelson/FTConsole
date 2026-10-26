module RandomUtilities

    implicit none

contains

    subroutine GenerateRandomArray(arr)

        real, intent(out) :: arr(:)
        integer :: i

        call random_seed()

        do i = 1, size(arr)
            call random_number(arr(i))
        end do

    end subroutine GenerateRandomArray

end module RandomUtilities
