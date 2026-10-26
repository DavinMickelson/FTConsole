	module Banking

	implicit none

	type :: BankAccount
		character(len=50) :: Owner
		real :: Balance
		real :: InterestRate
	contains
	procedure :: Display
	procedure :: Deposit
	procedure :: Withdraw
	end type BankAccount

	type, extends(BankAccount) :: SavingsAccount
		real :: MinimumBalance
	contains
	procedure :: Withdraw => SavingsWithdraw
	procedure :: Display => SavingsDisplay
	end type SavingsAccount

	type, extends(BankAccount) :: CheckingAccount
		real :: MonthlyFee
	contains
	procedure :: ApplyMonthlyFee
	end type CheckingAccount

	contains

	! ===============================================
	! BankAccount methods

	subroutine Display(this)
	class(BankAccount), intent(in) :: this

	write(*,'(A)') ""
	write(*,'(A)') "Account Information"
	write(*,'(A)') "-------------------"

	write(*,'(A,A)')    "Owner: ", trim(this%Owner)
	write(*,'(A,F10.2)') "Balance: $", this%Balance
	write(*,'(A,F6.2,A)') "Interest Rate: ", this%InterestRate * 100.0, "%"

	select type (this)

	type is (SavingsAccount)
		write(*,'(A,F10.2)') &
			"Minimum Balance: $", this%MinimumBalance

	type is (CheckingAccount)
		write(*,'(A,F10.2)') &
			"Monthly Fee: $", this%MonthlyFee

	class default
		continue

	end select

	print *, "-------------------"
	print *, ""

	end subroutine Display

	subroutine Deposit(this, amount)
	class(BankAccount), intent(inout) :: this
	real, intent(in) :: amount

	this%Balance = this%Balance + amount
	end subroutine Deposit

	subroutine Withdraw(this, amount)
	class(BankAccount), intent(inout) :: this
	real, intent(in) :: amount

	this%Balance = this%Balance - amount
	end subroutine Withdraw

	! ===============================================
	! SavingsAccount methods

	subroutine SavingsWithdraw(this, amount)
	class(SavingsAccount), intent(inout) :: this
	real, intent(in) :: amount

	if (this%Balance - amount >= this%MinimumBalance) then
		this%Balance = this%Balance - amount
	else
		write(*,'(A)') "Withdrawal denied."
	end if
	end subroutine SavingsWithdraw

	subroutine SavingsDisplay(this)
	class(SavingsAccount), intent(in) :: this

	call Display(this)
	!write(*,'(A,F10.2)') "Minimum Balance: $", this%MinimumBalance
	end subroutine SavingsDisplay

	! ===============================================
	! CheckingAccount methods

	subroutine ApplyMonthlyFee(this)
	class(CheckingAccount), intent(inout) :: this

	this%Balance = this%Balance - this%MonthlyFee
	end subroutine ApplyMonthlyFee

	end module Banking