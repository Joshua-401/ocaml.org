open Ast
open Minicgen
open Parser
open Parsetree
open Typecheck

let deepsea_version = "DeepSEA/EVM version 0.1.0"
       
type mode = ABI | BYTECODE | COMBINED_JSON | ASSEMBLY | MINIC | COQ

let string_of_token = function
  | ARRAY -> "ARRAY"
  | MAPPING -> "MAPPING"
  | CONST -> "CONST"
  | EOF -> "EOF"
  | EXTERNAL -> "EXTERNAL"
  | GHOST -> "GHOST"
  | IDENT s -> "IDENT \"" ^ s ^ "\""
  | INT i -> "INT \"" ^ string_of_int i ^ "\""
  | UINT i -> "UINT \"" ^ string_of_int i ^ "\""
  | LOGICAL -> "LOGICAL"
  | LAYER -> "LAYER"
  | OBJECT -> "OBJECT"
  (* | OF -> "OF" *)
  | SIGNATURE -> "SIGNATURE"
  | STRING s -> "STRING(\"" ^ String.escaped s ^ "\")"
  | TRUSTED -> "TRUSTED"
  | TYPE -> "TYPE"
  | ASSERT       -> "ASSERT"
  | BEGIN        -> "BEGIN"
  | DENY         -> "DENY"
  | DO           -> "DO"
  | ELSE         -> "ELSE"
  | END          -> "END"
  | FAIL         -> "FAIL"
  | FIRST        -> "FIRST"
  | FOLD         -> "FOLD"
  | FOR          -> "FOR"
  | IF           -> "IF"
  | IN           -> "IN"
  | LET          -> "LET"
  | MATCH        -> "MATCH"
  | MOD          -> "MOD"
  (* | SKIP         -> "SKIP" *)
  | THEN         -> "THEN"
  | TO           -> "TO"
  | WITH         -> "WITH"
  | AT           -> "AT"
  | ARROW        -> "ARROW"
  | ASSIGN       -> "ASSIGN"
  | BAR          -> "BAR"
  | BARBAR       -> "BARBAR"
  | BANG         -> "BANG"
  | COLON        -> "COLON"
  | COLONCOLON   -> "COLONCOLON"
  | COLONGREATER -> "COLONGREATER"
  | COMMA        -> "COMMA"
  | CONJUNCTION  -> "CONJUNCTION"
  | DISJUNCTION  -> "DISJUNCTION"
  | DOT          -> "DOT"
  | DOUBLEARROW  -> "DOUBLEARROW"
  | EQUAL        -> "EQUAL"
  | GREATER      -> "GREATER"
  | GREATEREQ    -> "GREATEREQ"
  | LBRACE       -> "LBRACE"
  | LBRACKET     -> "LBRACKET"
  | LESS         -> "LESS"
  | LESSEQ       -> "LESSEQ"
  | LPAREN       -> "LPAREN"
  | MINUS        -> "MINUS"
  | PLUS         -> "PLUS"
  | RBRACE       -> "RBRACE"
  | RBRACKET     -> "RBRACKET"
  | RPAREN       -> "RPAREN"
  | SEMICOLON    -> "SEMICOLON"
  | STAR         -> "STAR"
  | SLASH        -> "SLASH"
  | UNEQUAL      -> "UNEQUAL"
  | BITNOT       -> "BITNOT"
  | BITAND       -> "BITAND"
  | XOR          -> "XOR"
  | SHL          -> "SHL"
  | SHR          -> "SHR"
  | LIST         -> "LIST"

let rec read_tokens buf =
  let token = Lexer.token buf in
  print_endline (string_of_token token);
  if token <> EOF then read_tokens buf

let print_parse_file_structure structure =
  List.iter
    (function
     | i, PDtype t ->
       print_endline ("type " ^ i ^ " ::= " ^ string_of_p_type_FO t)
     | i, PDsignature s ->
       print_endline ("object signature " ^ i ^ " ::= " ^ string_of_p_signature s)
     | i, PDlayer_sig f ->
       print_endline ("layer signature " ^ i ^ " ::= " ^ string_of_p_layer_signature f)
     | i, PDobject o ->
       print_endline ("object " ^ i ^ " ::= " ^ string_of_p_object_definition o)
     | i, PDlayer c ->
       print_endline ("layer " ^ i ^ " ::= " ^ string_of_p_layer_definition c)
     | i, PDexternal_with (s, ann) ->
       print_endline ("external" ^ string_of_p_annotations ann ^
         " with \"" ^ String.escaped s ^ "\"")
     | i, PDexternal_type (s, lli_opt, ann) ->
       print_endline ("external type " ^ i ^ string_of_p_annotations ann ^
         " = \"" ^ String.escaped s ^ "\"" ^
         match lli_opt with None -> "" | Some lli -> " \"" ^ lli ^ "\"")
     | i, PDexternal_const (s, t, ann) ->
       print_endline ("external const " ^ i ^ string_of_p_annotations ann ^
         " : " ^ string_of_p_type_FO t ^
         " = \"" ^ String.escaped s ^ "\"")
     | i, PDexternal_function (s, arg, ret, ann) ->
       print_endline ("external const " ^ i ^ string_of_p_annotations ann ^
         " : " ^ string_of_p_type_FO arg ^
         " -> " ^ string_of_p_type_FO ret ^ " = \"" ^ String.escaped s ^ "\"")
     | i, PDexternal_prop (s, t) ->
       print_endline ("external assert " ^ i ^
         " : " ^ string_of_p_type_FO t ^
         " = \"" ^ String.escaped s ^ "\"")
    )
    structure

let print_ast_file_structure ast =
  List.iter
    (function
    | i, ADtype t ->
      print_endline ("AType " ^ i ^ " ::= " ^ string_of_a_type true t)
    (*
    | i, ADobject o ->
      print_endline (string_of_a_object o)
    *)
    | i, ADlayer c ->
      print_endline ("ALayer " ^ i ^ " ::= " ^ string_of_a_layer c)
    (*
    | i, ADexternal_with (s, except) ->
      print_endline ("AExternal with \"" ^ String.escaped s ^ "\"" ^
        if except = []
          then ""
          else " except: " ^ String.concat ", " except)
    *)
    )
    ast.aFileDeclations;
  List.iter (fun (s, except) ->
    print_endline ("AExternal with \"" ^ String.escaped s ^ "\"" ^
      if except = []
        then ""
        else " except: " ^ String.concat ", " except)
    ) ast.aFileExternalVerbatim


let usage () = 
  prerr_endline ( "usage: dsc program.ds (bytecode | abi | combined-json | assembly | minic | coq)\n"
		  ^"or     dsc --version\n");
  exit 1

let combined_json filename abi bytecode =
  Printf.sprintf "{\"contracts\":{\"%s\":{\"abi\":\"%s\", \"bin\":\"%s\"}}, \"version\":\"%s\"}"
  		 filename
		 (Str.global_replace (Str.regexp "\"") "\\\"" abi)
		 bytecode
		 deepsea_version

let main argv =
  (if (Array.length argv = 2 && argv.(1) = "--version")
     then begin print_endline deepsea_version; exit 0 end);
  (if (Array.length argv <> 3) then usage());
  let filename = argv.(1) in
  let mode_flag = match Array.get argv 2 with
    | "bytecode" -> BYTECODE
    | "abi"      -> ABI
    | "combined-json" -> COMBINED_JSON
    | "assembly" -> ASSEMBLY
    | "minic"    -> MINIC
    | "coq"      -> COQ
    | _          -> usage () in  
  (* print_endline ("reading from \"" ^ filename ^ "\""); *)
  let ch = open_in filename in
  let buf = Lexing.from_channel ch in
  (*let _ = Location.init buf filename in*)
  let parse_structure = try
    file
      Lexer.token
      (*fun buf -> let t = Lexer.token buf
                  in print_endline ("TOK: " ^ string_of_token t); t*)
      buf 
     with Failure _
        | Parsing.Parse_error ->
	  let curr = buf.Lexing.lex_curr_p in
	  let line = curr.Lexing.pos_lnum in
	  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
	  let tok = Lexing.lexeme buf in
	  print_endline (filename ^":"^ string_of_int line 
			 ^":"^ string_of_int cnum 
			 ^": Syntax error at token \"" ^ tok ^ "\".");
	  exit 1
   in
  (* let _ = print_parse_file_structure parse_structure in *)
  let has_error, ast_structure = typecheck parse_structure
  in (* print_ast_file_structure ast_structure; *)
  if has_error then
    begin print_endline "typecheck failed"; exit 1 end
  else
    let abi = abigen ast_structure in
    match mode_flag with
    | COQ -> Coqgen.coqgen filename ast_structure
    | ABI -> print_endline abi 			   
    | MINIC -> let ge = minicgen filename ast_structure in print_endline (Backend.LanguageExt.show_genv ge)			     
    | _     ->
       let ge = minicgen filename ast_structure in
       match Backend.Glue.full_compile_genv ge with
       | None -> print_endline "Compilation failed"; exit 1
       | Some (Backend.Datatypes.Coq_pair (program, entrypoint)) ->
          let asm =
            Backend.ASM.transform
              (List.rev (Backend.DatatypesExt.caml_list program))
              entrypoint in
          match mode_flag with
	   | BYTECODE -> print_endline (Backend.ASM.assemble asm)
	   | ASSEMBLY -> print_endline (Backend.ASM.mnemonics asm)
	   | COMBINED_JSON -> print_endline (combined_json filename abi (Backend.ASM.assemble asm))
	   | _        -> (print_endline "unreachable"; exit 1)

let _ = main Sys.argv
