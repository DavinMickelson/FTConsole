
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

	integer :: i = 0
	integer :: score = 0
	integer :: valueCount = 0

	! Banking
	type(BankAccount) :: BankAcct
	type(SavingsAccount) :: Savings
	type(CheckingAccount) :: Checking

	real, allocatable :: values(:)

	character(len=50) :: playerName

	print *, "========================================="
	print *, "      Intel Fortran Fun"
	print *, "========================================="
	print *, "      Program Version:", ProgramVersion
	print *, "========================================="
	print *, ""

	! ===============================================
	! Ask user's name

	write(*,'(A)', advance='no') "What is your name? :"
	read(*,'(A)') playerName

	print *, ""
	print *, "Welcome ", trim(playerName), "!"
	print *, ""

	! ===============================================
	! Display the largest numbers for integer, real, and double precision

	print *, "The largest integer is: "
	print *, huge(1)
	print *, ""
	print *, "The largest real is: "
	print *, huge(1.0)
	print *, ""
	print *, "The largest double precision real is: "
	print *, huge(1.0d0)
	print *

	! ===============================================
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

	! Calculate and display statistics
	print *, ""
	print *, "Statistics"
	print *, "----------"

	write(*,'(A,F8.4)') "Minimum = ", minval(values)
	write(*,'(A,F8.4)') "Maximum = ", maxval(values)
	write(*,'(A,F8.4)') "Average = ", Average(values)
	
	! ===============================================
	! Guessing game

	print *, ""
	print *, "Guessing game"
	print *, "-------------"
	call GuessingGame(score)

	print *, ""
	print *, "Final Score = ", score

	if (score > 75) then
		print *, "Excellent!"
	else if (score > 50) then
		print *, "Pretty good!"
	else
		print *, "You can do better!"
	end if

	! ===============================================
	! Banking fun

	print *, ""
	print *, "Banking"
	print *, "-------"

	! Create three types of accounts
	BankAcct%Owner = "Bob"
	BankAcct%Balance = 1000.0
	BankAcct%InterestRate = 0.03

	Savings%Owner = "Bryan"
	Savings%Balance = 3000.0
	Savings%MinimumBalance = 100

	Checking%Owner = "Ellen"
	Checking%Balance = 5000.0
	Checking%MonthlyFee = 10
	
	! Demonstrate account functionality

	call BankAcct%Display()
	write(*,'(A)') "Depositing $500 into Bob's Bank account..."
	call BankAcct%Deposit(500.0)
	call BankAcct%Display()

	call Savings%Display()
	write(*,'(A)') "Depositing $200 into Bryan's Savings account..."
	call Savings%Deposit(200.0)
	write(*,'(A)') "After deposit:"
	call Savings%Display()
	write(*,'(A)') "Withdrawing $3500 from Bryan's Savings account..."
	call Savings%Withdraw(3500.0)
	write(*,'(A)') "After withdrawal:"
	call Savings%Display()
	write(*,'(A)') "Withdrawing $2000 from Bryan's Savings account..."
	call Savings%Withdraw(2000.0)
	write(*,'(A)') "After withdrawal:"
	call Savings%Display()

	call Checking%Display()
	write(*,'(A)') "Applying monthly fee..."
	call Checking%ApplyMonthlyFee()
	write(*,'(A)') "After applying monthly fee:"
	call Checking%Display()

	! ===============================================
	! Program completion message

	print *, ""
	print *, "Program complete."

	contains

	! ===============================================
	! Guessing Game Subroutine
	subroutine GuessingGame(score)

	integer, intent(out) :: score
	
	integer :: guessCount = 0
	integer :: answer
	integer :: guess
	real :: r

	call random_number(r)

	answer = int(r * 100.0) + 1

	print *, "I am thinking of a number between 1 and 100."
	print *, "Can you guess it?"
	print *, "Use -1 to skip."

	do
		guessCount = guessCount + 1

		write(*,'(A)',advance='no') "Your guess: "
		read(*,*) guess

		if (guess == -1) then
			print *, "Skipping..."
			exit
		end if

		if (guess < 1 .or. guess > 100) then
			print *, "Please enter a number between 1 and 100."
			cycle
		end if

		if (guess == answer) then
			print *, "Correct!"
			exit
		else if (guess < answer) then
			print *, "Too low."
		else
			print *, "Too high."
		end if

	end do

	score = max(0, 100 - (guessCount - 1) * 10)

	end subroutine GuessingGame

	end program FTConsole