(* Strict implimentation of the FA1.2 specification in PascaLIGO *)

type amt is nat;

type transferParams is michelson_pair(address, "from", michelson_pair(address, "to", amt, "value"), "")
type approveParams is michelson_pair(address, "spender", amt, "value")
type balanceParams is michelson_pair(address, "owner", contract(amt), "")
type allowanceParams is michelson_pair(michelson_pair(address, "owner", address, "spender"), "", contract(amt), "")
type totalSupplyParams is (unit * contract(amt))

type action is
  | Transfer of transferParams
  | Approve of approveParams
  | GetBalance of balanceParams
  | GetAllowance of allowanceParams
  | GetTotalSupply of totalSupplyParams

type allowances is map (address, amt);

type account is record [
  [@annot:b] balance    : amt;
  [@annot:a] allowances : allowances;
]

(* contract storage *)
type storage is record [
    totalSupply     : amt;
    ledger          : big_map (address, account);
]

type return is list (operation) * storage

const noop : list (operation) = nil;

function getAccount (const a : address; const s : storage) : account is
  case s.ledger[a] of
    | None -> record [balance = 0n; allowances = (map [] : allowances)]
    | Some(acct) -> acct
  end;

function getAllowance (const ownerAccount : account; const spender : address; const s : storage) : amt is
  case ownerAccount.allowances[spender] of
  | None -> 0n
  | Some (amt) -> amt
  end;

(* Transfer token to another account *)
function transfer (const from_ : address; const to_ : address; const value : amt; var s : storage) : return is
  begin

    (* Retrieve sender account from storage *)
    const senderAccount : account = getAccount(from_, s);

    (* Balance check *)
    if senderAccount.balance < value then failwith("NotEnoughBalance") else skip;

    (* Check this address can spend the tokens *)
    if from_ =/= Tezos.sender then begin
      const spenderAllowance : amt = getAllowance(senderAccount, Tezos.sender, s);

      if spenderAllowance < value then failwith("NotEnoughAllowance")
      (* Decrease any allowances *)
      else senderAccount.allowances[Tezos.sender] := abs(spenderAllowance - value);
    end else skip;

    senderAccount.balance := abs(senderAccount.balance - value);

    s.ledger[from_] := senderAccount;

    var receiverAccount : account := getAccount(to_, s);

    receiverAccount.balance := receiverAccount.balance + value;

    s.ledger[to_] := receiverAccount;

  end with (noop, s)

(* Approve an amt to be spent by another address in the name of the sender *)
function approve (const spender : address; const value : amt; var s : storage) : return is
  begin

    var senderAccount : account := getAccount(Tezos.sender, s);

    const spenderAllowance : amt = getAllowance(senderAccount, spender, s);

    (* Prevent a corresponding attack vector *)
    if spenderAllowance > 0n and value > 0n then failwith("UnsafeAllowanceChange")
    else senderAccount.allowances[spender] := value;

    (* Update storage *)
    s.ledger[Tezos.sender] := senderAccount;

  end with (noop, s)

(* View function that forwards the balance of source to a contract *)
function getBalance (const owner : address; const contr : contract(amt); var s : storage) : return is
  begin
    const ownerAccount : account = getAccount(owner, s);
  end with (list [transaction(ownerAccount.balance, 0tz, contr)], s)

(* View function that forwards the allowance amt of spender in the name of tokenOwner to a contract *)
function getAllowance (const owner : address; const spender : address; const contr : contract(amt); var s : storage) : return is
  begin
    const spenderAllowance : amt = getAllowance(getAccount(owner, s), spender, s);
  end with (list [transaction(spenderAllowance, 0tz, contr)], s)

(* View function that forwards the totalSupply to a contract *)
function getTotalSupply (const contr : contract(amt); var s : storage) : return is (list [transaction(s.totalSupply, 0tz, contr)], s)

function main (const action : action; var s : storage) : return is
  case action of
    | Transfer(params) -> transfer(params.0, params.1.0, params.1.1, s)
    | Approve(params) -> approve(params.0, params.1, s)
    | GetBalance(params) -> getBalance(params.0, params.1, s)
    | GetAllowance(params) -> getAllowance(params.0.0, params.0.1, params.1, s)
    | GetTotalSupply(params) -> getTotalSupply(params.1, s)
  end;
