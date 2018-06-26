open Base
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
  } [@@ deriving_show]
end

let prompt pr fn errMsg =
  Out_channel.output_string stdout pr;
  Out_channel.flush stdout;
  try
    match In_channel.input_line In_channel.stdin with
    | None -> failwith errMsg
    | Some str -> (fn str)
  with
  | _ -> failwith errMsg


let generateHtmlReceipt ri =
  
  let strStyle = "table, th, td {   border: 1px solid black;  border-collapse: collapse; }" in
  let style = { Tag.tagType = "style"; Tag.tagData=strStyle; Tag.tagOptions=[]; Tag.internals=[];  } in
  let head = { Tag.tagType = "head"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[style];  } in
  let td1 = { Tag.tagType = "td"; Tag.tagData="Landlord"; Tag.tagOptions=[]; Tag.internals=[];  } in
  let td2 = { Tag.tagType = "td"; Tag.tagData=ri.ReceiptInfo.landLord; Tag.tagOptions=[]; Tag.internals=[];  } in
  let row1 = { Tag.tagType = "tr"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[td1; td2];  } in
  let table = { Tag.tagType = "table"; Tag.tagData=""; Tag.tagOptions=["style=\"width:50%\""]; Tag.internals=[row1; ];  } in
  let body = { Tag.tagType = "body"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[table];  } in
  let html = { Tag.tagType = "html"; Tag.tagData=""; Tag.tagOptions=[]; Tag.internals=[head; body];  } in
  printf "%s\n" (toString html)
       
let () =
  let recNum = prompt "Enter receiptNo: " int_of_string "Bad receipt no" in
  let landLord = prompt "Enter the name of the landlord: " (fun x -> x) "Bad landlord name" in
  let tenant = prompt "Enter the name of the tenant: " (fun s -> s) "Bad tenant name" in
  let rentAmt = prompt "Enter rent amount: " int_of_string "Bad rent amount" in
  let dt = Date.today (Time.Zone.of_string "EST") in
  let ri = { ReceiptInfo.receiptNum = recNum; ReceiptInfo.landLord = landLord;
             ReceiptInfo.tenant = tenant; ReceiptInfo.amount = rentAmt; ReceiptInfo.receiptDate = dt } in
  
  generateHtmlReceipt ri
  
  
        
    
    
