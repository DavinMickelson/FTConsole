
	!****************************************************************************
	! FTConsole.f90
	!
	! Demonstrates:
	!   - Procedures
	!   - Arrays
	!   - Random numbers
	!   - User input
	!   - String handling
	!   - Statistics
	!****************************************************************************

	program FTConsole

	use Constants
	use RandomUtilities
	use StatisticsUtilities
	use Banking

	implicit none

	integer :: i
	integer :: score
	integer :: valueCount

	! Banking
	type(BankAccount) :: BankAcct
	type(SavingsAccount) :: Savings
  type(CheckingAccount) :: Checking
	
	! real, dimension(10) :: values
	real, allocatable :: values(:)

	character(len=50) :: playerName

	print *, "========================================="
	print *, "      Intel Fortran Demo Program"
	print *, "========================================="
	print *, "      Program Version:", ProgramVersion
	print *, "========================================="
	print *

! ===========================================================
! Ask user's name
	print *, "What is your name?"
	read(*,'(A)') playerName

	print *
	print *, "Welcome ", trim(playerName), "!"
	print *

	! integer :: i
  print *, "The largest integer is: "
	print *, huge(i)

! ===========================
! Generate random numbers based on user input
	print *, "How many random numbers should I generate?"
	read(*,*) valueCount

	allocate(values(valueCount))

! Call GenerateRandomArray()
	call GenerateRandomArray(values)

	print *, valueCount, " random numbers were generated:"

	do i = 1, size(values)
		write(*,'(I7,A,F8.4)') i, ": ", values(i)
	end do

	! Write to file
	open(unit=10, file='RandomNumbers.txt')

	do i = 1, size(values)
		write(10,'(F10.6)') values(i)
	end do

	close(10)

! ===========================
! Calculate and display statistics
	print *
	print *, "Statistics"
	print *, "----------"

	write(*,'(A,F8.4)') "Average = ", Average(values)
	write(*,'(A,F8.4)') "Maximum = ", maxval(values)
	write(*,'(A,F8.4)') "Minimum = ", minval(values)

! ===========================
! Guessing game
	print *
	print *, "Let's play a guessing game!"
	call GuessingGame(score)
 
	print *
	print *, "Final Score = ", score

	if (score > 75) then
		print *, "Excellent!"
	else if (score > 50) then
		print *, "Pretty good!"
	else
		print *, "You can do better!"
	end if

	print *
	print *, "Program complete."

	!------------------------------ Banking
	print *
	print *, "Banking"
	
	BankAcct%Owner = "Bob"
	BankAcct%Balance = 1000.0
	BankAcct%InterestRate = 0.03

	print *, BankAcct%Balance
	
	Savings%Owner = "Bryan"
	Savings%Balance = 3000.0
	Savings%MinimumBalance = 100
	
	print *, Savings%Balance
	print *, Savings%InterestRate
	print *, Savings%MinimumBalance
	
	Checking%Owner = "Ellen"
	Checking%Balance = 5000.0
	Checking%MonthlyFee = 10
	
	print *, Checking%Balance
	print *, Checking%InterestRate
	print *, Checking%MonthlyFee
	
	contains

	!--------------------------------------------------------
	subroutine GuessingGame(score)

	integer, intent(out) :: score

	integer :: answer
	integer :: guess
	real :: r

	call random_number(r)

	answer = int(r * 100.0) + 1

	print *, "I am thinking of a number between 1 and 100."
	print *, "Can you guess it?"

	do

		write(*,'(A)',advance='no') "Your guess: "
		read(*,*) guess

		if (guess == answer) then
			print *, "Correct!"
			exit
		else if (guess < answer) then
			print *, "Too low."
		else
			print *, "Too high."
		end if

	end do

	score = max(0, 100 - abs(answer - guess))

	end subroutine GuessingGame

	end program FTConsole