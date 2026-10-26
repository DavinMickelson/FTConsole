	module Banking

	implicit none

	type :: BankAccount

		character(len=50) :: Owner
		real :: Balance
		real :: InterestRate

	end type BankAccount

	type, extends(BankAccount) :: SavingsAccount

		real :: MinimumBalance

	end type SavingsAccount


	type, extends(BankAccount) :: CheckingAccount

		real :: MonthlyFee

	end type CheckingAccount

	end module Banking