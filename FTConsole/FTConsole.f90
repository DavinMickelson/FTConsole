
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
	character(len=1) :: pause

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

	write(*,'(A)') ""
	write(*,'(3A)') "Welcome, ", trim(playerName), "!"
	write(*,'(A)') "Welcome, " // trim(playerName)  // "!"
	write(*,'(A)') ""

	! ===============================================
	! Display the largest numbers for integer, real, and double precision

	write(*,'(A)') "The largest integer is: "
	write(*,*) huge(1)
	write(*,'(A)') ""
	write(*,'(A)') "The largest real is: "
	write(*,*) huge(1.0)
	write(*,'(A)') ""
	write(*,'(A)') "The largest double precision real is: "
	write(*,*) huge(1.0d0)
	write(*,'(A)') ""

	! ===============================================
	! Generate random numbers based on user input

	write(*,'(A)') "How many random numbers should I generate?"
	read(*,*) valueCount

	allocate(values(valueCount))

	! Call GenerateRandomArray()
	call GenerateRandomArray(values)

	write(*,'(A,I0,A)') "Generated ", valueCount, " random numbers:"

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
	write(*,'(A)') ""
	write(*,'(A)') "Statistics"
	write(*,'(A)') "----------"

	write(*,'(A,F8.4)') "Minimum = ", minval(values)
	write(*,'(A,F8.4)') "Maximum = ", maxval(values)
	write(*,'(A,F8.4)') "Average = ", Average(values)

	! ===============================================
	! Guessing game

	write(*,'(A)') ""
	write(*,'(A)') "Guessing game"
	write(*,'(A)') "-------------"
	call GuessingGame(score)

	write(*,'(A)') ""
	write(*,'(A,I0)') "Final Score = ", score

	if (score >= 70) then
		write(*,'(A)') "Excellent!"
	else if (score >= 50) then
		write(*,'(A)') "Pretty good!"
	else
		write(*,'(A)') "You can do better!"
	end if

	write(*,'(A)') ""
	write(*,'(A)',advance='no') &
		"Press <Enter> to continue..."
	read(*,'(A)') pause

	! ===============================================
	! Banking fun

	write(*,'(A)') ""
	write(*,'(A)') "Banking"
	write(*,'(A)') "-------"

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

	write(*,'(A)') "==============================="

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

	write(*,'(A)') "==============================="

	call Checking%Display()
	write(*,'(A)') "Applying monthly fee..."
	call Checking%ApplyMonthlyFee()
	write(*,'(A)') "After applying monthly fee:"
	call Checking%Display()

	! ===============================================
	! Program completion message

	write(*,'(A)') ""
	write(*,'(A)') "Program complete."

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

	write(*,'(A)') "I am thinking of a number between 1 and 100."
	write(*,'(A)') "Can you guess it?"
	write(*,'(A)') "Use -1 to skip."

	do
		guessCount = guessCount + 1

		write(*,'(A)',advance='no') "Your guess: "
		read(*,*) guess

		if (guess == -1) then
			write(*,'(A)') "Skipping..."
			exit
		end if

		if (guess < 1 .or. guess > 100) then
			write(*,'(A)') "Please enter a number between 1 and 100."
			cycle
		end if

		if (guess == answer) then
			write(*,'(A)') "Correct!"
			exit
		else if (guess < answer) then
			write(*,'(A)') "Too low."
		else
			write(*,'(A)') "Too high."
		end if

	end do

	score = max(0, 100 - (guessCount - 1) * 10)

	end subroutine GuessingGame

	end program FTConsole