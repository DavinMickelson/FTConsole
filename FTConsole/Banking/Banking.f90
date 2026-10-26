	module Banking

	implicit none

	type :: BankAccount
		character(len=50) :: Owner
		real :: Balance
		real :: InterestRate
	contains
		procedure :: Deposit
		procedure :: Withdraw
	end type BankAccount

	type, extends(BankAccount) :: SavingsAccount
		real :: MinimumBalance
	contains
		procedure :: Withdraw => SavingsWithdraw
	end type SavingsAccount

	type, extends(BankAccount) :: CheckingAccount
		real :: MonthlyFee
	contains
		procedure :: ApplyMonthlyFee
	end type CheckingAccount

	contains

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

    subroutine SavingsWithdraw(this, amount)
        class(SavingsAccount), intent(inout) :: this
        real, intent(in) :: amount

        if (this%Balance - amount >= this%MinimumBalance) then
            this%Balance = this%Balance - amount
        else
            print *, "Withdrawal denied."
        end if
    end subroutine SavingsWithdraw

    subroutine ApplyMonthlyFee(this)
        class(CheckingAccount), intent(inout) :: this

        this%Balance = this%Balance - this%MonthlyFee
    end subroutine ApplyMonthlyFee

	end module Banking