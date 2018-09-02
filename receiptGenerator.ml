open Stdio
open Core
open Tag
   
module ReceiptInfo = struct
  type t = {
    receiptNum : int;
    landLord : string;
    tenant : string;
    amount : int; (* in cents / multiplied by 100 *)
    receiptDate : Date.t;
    address : string;
    startRent : Date.t;
    endRent : Date.t;
  }[@@deriving sexp]
end


let generateHtmlReceipt ri =
  
  let strStyle = "table, th, td {   border: 1px solid black;  border-collapse: collapse; }" in
  let style = { Tag.tagType = "style"; Tag.tagData=strStyle; Tag.tagOptions=[]; Tag.internals=[];  } in
  let head = { Tag.tagType = "head"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[style];  } in

  let rn = Printf.sprintf "%d" ri.ReceiptInfo.receiptNum in
  let td0 = { Tag.tagType = "td"; Tag.tagData="Receipt Num"; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let td00 = { Tag.tagType = "td"; Tag.tagData=rn; Tag.tagOptions=[]; Tag.internals=[];  } in
  let row0 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td0; td00];  } in

  let td111 = { Tag.tagType = "td"; Tag.tagData="Property Address"; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let td112 = { Tag.tagType = "td"; Tag.tagData=ri.ReceiptInfo.address; Tag.tagOptions=[]; Tag.internals=[];  } in
  let row10 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td111; td112];  } in
  
  let td1 = { Tag.tagType = "td"; Tag.tagData="Landlord"; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let td2 = { Tag.tagType = "td"; Tag.tagData=ri.ReceiptInfo.landLord; Tag.tagOptions=[]; Tag.internals=[];  } in
  let row1 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td1; td2];  } in
  
  let td3 = { Tag.tagType = "td"; Tag.tagData="Tenant"; Tag.tagOptions=[]; Tag.internals=[];  } in
  let td4 = { Tag.tagType = "td"; Tag.tagData=ri.ReceiptInfo.tenant; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let row2 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td3; td4];  } in

  let amt = Printf.sprintf "$%d.00" ri.ReceiptInfo.amount in
  let td5 = { Tag.tagType = "td"; Tag.tagData="Amount"; Tag.tagOptions=[]; Tag.internals=[];  } in
  let td6 = { Tag.tagType = "td"; Tag.tagData=amt; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let row3 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td5; td6];  } in

  let dt = Date.to_string_american ri.ReceiptInfo.receiptDate in
  let td7 = { Tag.tagType = "td"; Tag.tagData="Date"; Tag.tagOptions=[]; Tag.internals=[];  } in
  let td8 = { Tag.tagType = "td"; Tag.tagData=dt; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let row4 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td7; td8];  } in

  let rentalPeriod = sprintf "%s - %s\n" (Date.to_string_american ri.ReceiptInfo.startRent) (Date.to_string_american ri.ReceiptInfo.endRent) in 
  let td11 = { Tag.tagType = "td"; Tag.tagData="Rental Period: "; Tag.tagOptions=[]; Tag.internals=[];  } in
  let td12 = { Tag.tagType = "td"; Tag.tagData=rentalPeriod; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let row5 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td11; td12];  } in
  
  let td9 = { Tag.tagType = "td"; Tag.tagData="Signature"; Tag.tagOptions=[]; Tag.internals=[];  } in
  let td10 = { Tag.tagType = "td"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[];  } in  
  let row6 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td9; td10];  } in

  let heading = { Tag.tagType = "h2"; Tag.tagData="Rent Receipt"; Tag.tagOptions=[]; Tag.internals=[];  } in
  let rows = [row0;row1;row10;row2;row3;row4;row5;row6] in
  let table = { Tag.tagType = "table"; Tag.tagData=""; Tag.tagOptions=["style=\"width:100%\""]; Tag.internals=rows;  } in
  let body = { Tag.tagType = "body"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[heading;table];  } in
  let html = { Tag.tagType = "html"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[head; body];  } in (toString html)
  

let saveReceipt receipt receiptDate sexpReceipt =
  let fileName = receiptDate ^ ".html" in   
  let file = Out_channel.create fileName in
  Exn.protect ~f:(fun () -> Out_channel.fprintf file "%s" receipt)
    ~finally:(fun () -> Out_channel.close file);
  let file = Out_channel.create "receipt.dat" in
  Exn.protect ~f:(fun () -> Out_channel.fprintf file "%s" sexpReceipt)
     ~finally:(fun () -> Out_channel.close file)


(* let readReceipt =
 *   let file = In_channel.create "receipt.dat" in
 *   Exn.protect ~f:(fun () ->
 *     let strReceipt = In_channel.input_all file in 
 *     let receiptInfo = Sexp.of_string strReceipt |> ReceiptInfo.t_of_sexp in receiptInfo)  
 *     ~finally:(fun () -> In_channel.close file) *)

let readReceipt =
  try
    let file = In_channel.create "receipt.dat" in
    let strReceipt = In_channel.input_all file in
    let receiptInfo = Sexp.of_string strReceipt |> ReceiptInfo.t_of_sexp in
    In_channel.close file; Some receiptInfo   
  with
  |_ -> None
      
let prompt pr fn errMsg defVal =
  Out_channel.output_string stdout pr;
  Out_channel.flush stdout;
  try
    match In_channel.input_line In_channel.stdin with
    | None -> (match defVal with
              | None -> failwith (Printf.sprintf "%s defval " errMsg)
              | Some x -> x)        
  
    | Some str ->
       match str with
       | "" -> (match defVal with
              | None -> failwith (Printf.sprintf "%s defval " errMsg)
              | Some x -> x)            
       | _ -> (fn str)
    
 with
 | _ -> (match defVal with
        | None -> failwith errMsg
        | Some x -> x) 

let () =
  let open ReceiptInfo in
  let ris = readReceipt in
  let open Printf in
  match ris with
  | Some ri ->
    
    let recNum = prompt (sprintf "Enter receiptNo: [ %d ] " (ri.receiptNum + 1))  int_of_string "Bad receipt no" (Some (ri.receiptNum + 1)) in
    let propAddress = prompt (sprintf "Enter address: [ %s ] " ri.address) (fun x -> x) "Bad address"  (Some ri.address) in
    (*printf "****** %s ******* \n" propAddress ;*)
    let landLord = prompt (sprintf "Enter the name of the landlord: [ %s ] " ri.landLord) (fun x -> x) "Bad landlord name"  (Some ri.landLord) in
    let tenant = prompt (sprintf "Enter the name of the tenant: [ %s ] " ri.tenant) (fun s -> s) "Bad tenant name" (Some ri.tenant) in
    let rentAmt = prompt (sprintf "Enter rent amount: [ %d ] " ri.amount) int_of_string "Bad rent amount" (Some ri.amount) in
    let dt = Date.today (Time.Zone.of_string "EST") in
    let startRent = prompt "Enter start rent date (DD/MM/YYYY): " Date.of_string "Bad start rent date" None in
    let endRent = prompt "Enter end rent date (DD/MM/YYYY): " Date.of_string "Bad end rent date" None in                

    let open ReceiptInfo in
    let nri = { receiptNum = recNum; landLord = landLord;
             tenant = tenant; amount = rentAmt; receiptDate = dt;
             startRent = startRent; endRent = endRent;
             address = propAddress; } in
  
    let htmlReceipt = generateHtmlReceipt ri in
    saveReceipt htmlReceipt (Date.to_string dt) (ReceiptInfo.sexp_of_t nri |> Sexp.to_string)
    
  | None ->

    let recNum = prompt (sprintf "Enter receiptNo: [ %d ] " 0)  int_of_string "Bad receipt no" None in
    let propAddress = prompt (sprintf "Enter address: [ %s ] " "") (fun x -> x) "Bad address"  None in 
    let landLord = prompt (sprintf "Enter the name of the landlord: [ %s ] " "") (fun x -> x) "Bad landlord name"  None in
    let tenant = prompt (sprintf "Enter the name of the tenant: [ %s ] " "") (fun s -> s) "Bad tenant name" None in
    let rentAmt = prompt (sprintf "Enter rent amount: [ %d ] " 0) int_of_string "Bad rent amount" None in
    let dt = Date.today (Time.Zone.of_string "EST") in
    let startRent = prompt "Enter start rent date (DD/MM/YYYY): " Date.of_string "Bad start rent date" None in
    let endRent = prompt "Enter end rent date (DD/MM/YYYY): " Date.of_string "Bad end rent date" None in                

    let open ReceiptInfo in
    let ri = { receiptNum = recNum; landLord = landLord;
             tenant = tenant; amount = rentAmt; receiptDate = dt;
             startRent = startRent; endRent = endRent;
             address = propAddress; } in
  
    let htmlReceipt = generateHtmlReceipt ri in
    saveReceipt htmlReceipt (Date.to_string dt) (ReceiptInfo.sexp_of_t ri |> Sexp.to_string)
    
        
    
    
