open Base
open Printf
open Core
   
module Tag = struct 
type t =
  {
    tagType : string;
    tagData : string;
    tagOptions : string list;
    internals : t list;
  }
end

let optsToString opts = 
  List.fold ~f:(fun a b -> a ^ " " ^ b) ~init:"" opts

  
let rec toString tg =
  sprintf "<%s %s> %s %s </%s>" tg.Tag.tagType (optsToString tg.Tag.tagOptions)
    tg.Tag.tagData
    (List.map tg.Tag.internals ~f:(toString)  |> List.fold ~f:(^) ~init:"" )
    tg.Tag.tagType    
