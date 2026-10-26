module StatisticsUtilities

    implicit none

	contains
	
	!--------------------------------------------------------
    real function Average(arr)

        real, intent(in) :: arr(:)

        Average = sum(arr) / size(arr)

		end function Average
		
end module StatisticsUtilities