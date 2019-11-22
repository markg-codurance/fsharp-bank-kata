module Tests

open System
open Bank
open Expecto

type PrinterMock() =
  let mutable stmt = ""
  interface Bank.IPrinter with
    member __.Print message = __.Statement <- __.Statement + message
  member this.Statement
    with get() = stmt
    and private set(value) = stmt <- value
  
type DateMock() =
  let mutable date = DateTime.Now
  interface IDateService with
    member __.GetDate() = __.CurrentDate.ToString("dd/MM/yyyy")
  member private __.CurrentDate
    with get() = date
    and set(value) = date <- value
  member __.SetDate date = __.CurrentDate <- date
  
let testWithAccount description testFn =
  let printerMock = PrinterMock()
  let dateMock = DateMock()
  let account = Bank.Account(printerMock, dateMock) :> IAccountService
  testCase description (fun () -> testFn account printerMock (dateMock :> IDateService))
  
[<Tests>]
let tests =
  testList "Bank account must" [
        
    testWithAccount "print a header when a statement is requested"
      (fun account printerMock _ ->
         account.PrintStatement()
         Expect.equal printerMock.Statement "Date || Amount || Balance\n" "Header is not as expected"
      )

    testWithAccount "print the deposit of 100 when a statement is requested"
      (fun account printerMock dateMock ->
         let currentDate = dateMock.GetDate()
         account.Deposit(100)
         account.PrintStatement()
         let expectedStatement = sprintf "Date || Amount || Balance\n%s || 100 || 100\n" currentDate
         Expect.equal printerMock.Statement expectedStatement "Header is not as expected"
      )

    testWithAccount "print two transactions in LIFO order"
      (fun account printerMock dateMock ->
         let currentDate = dateMock.GetDate()
         let expectedStatement = sprintf "Date || Amount || Balance\n%s || 125 || 200\n%s || 75 || 75\n" currentDate currentDate
         account.Deposit(75)
         account.Deposit(125)
         account.PrintStatement()
         Expect.equal printerMock.Statement expectedStatement "Transactions are not displayed in the correct order"
      )

    testWithAccount "print the statement with the correct balance"
      (fun account printerMock dateMock ->
         let currentDate = dateMock.GetDate()
         let expectedStatement = sprintf "Date || Amount || Balance\n%s || 25 || 300\n%s || 275 || 275\n" currentDate currentDate
         account.Deposit(275)
         account.Deposit(25)
         account.PrintStatement()
         Expect.equal printerMock.Statement expectedStatement "Transactions are not displayed with accurate balances"
      )
      
    testWithAccount "print the statement with withdrawal"
      (fun account printerMock dateMock ->
         let currentDate = dateMock.GetDate()
         let expectedStatement = sprintf "Date || Amount || Balance\n%s || -25 || 75\n%s || 100 || 100\n" currentDate currentDate
         account.Deposit(100)
         account.Withdraw(25)
         account.PrintStatement()
         Expect.equal printerMock.Statement expectedStatement "Withdrawals are not displayed correctly"
      )
      
    testWithAccount "print the statement with transaction in date order"
      (fun account printerMock dateMock ->
         let dateMock = (dateMock :?> DateMock)
         let expected = "Date || Amount || Balance\n14/01/2012 || -500 || 2500\n13/01/2012 || 2000 || 3000\n10/01/2012 || 1000 || 1000\n"
         dateMock.SetDate(DateTime(2012, 01, 10))
         account.Deposit(1000)
         dateMock.SetDate(DateTime(2012, 01, 13))
         account.Deposit(2000)
         dateMock.SetDate(DateTime(2012, 01, 14))
         account.Withdraw(500)
         account.PrintStatement()
         Expect.equal printerMock.Statement expected "Transactions are not displayed with accurate balances or in the proper date order with correct dates"
      )
  ]
