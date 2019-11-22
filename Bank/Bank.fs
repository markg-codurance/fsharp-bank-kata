module Bank
open System.Collections.Generic

type IPrinter = abstract member Print : string -> unit
type IDateService = abstract member GetDate : unit -> string
type IAccountService =
  abstract member PrintStatement : unit -> unit
  abstract member Deposit : int -> unit
  abstract member Withdraw : int -> unit
type Transaction = string*int*int
type TransactionType = Deposit | Withdrawal

let getBalance transactions = Seq.sumBy (fun (_,amount,_) -> amount) transactions
let negate amount = -1 * amount

type Account(printerService:IPrinter, dateService:IDateService) =
  let print = printerService.Print
  let transactions = List<Transaction>()
  interface IAccountService with
    member __.PrintStatement() =
      print("Date || Amount || Balance\n")
      let printTransaction(date, amount, balance) = print(sprintf "%s || %i || %i\n" date amount balance)
      transactions |> Seq.iter printTransaction
    member __.Deposit amount = __.Transact Deposit amount
    member __.Withdraw amount = __.Transact Withdrawal amount

  member private __.Transact transactionType amount =
    let currentBalance = getBalance transactions
    let transactionAmount =
      match transactionType with
      | Deposit -> amount
      | Withdrawal -> negate amount
    transactions.Insert(0, (dateService.GetDate(), transactionAmount, currentBalance+transactionAmount))
