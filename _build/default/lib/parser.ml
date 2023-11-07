
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WORD of (
# 1 "lib/parser.mly"
       (string)
# 15 "lib/parser.ml"
  )
    | EOF
  
end

include MenhirBasics

type ('s, 'r) _menhir_state = 
  | MenhirState0 : ('s, _menhir_box_main) _menhir_state
    (** State 0.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState1 : (('s, _menhir_box_main) _menhir_cell1_WORD, _menhir_box_main) _menhir_state
    (** State 1.
        Stack shape : WORD.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_WORD = 
  | MenhirCell1_WORD of 's * ('s, 'r) _menhir_state * (
# 1 "lib/parser.mly"
       (string)
# 39 "lib/parser.ml"
)

and _menhir_box_main = 
  | MenhirBox_main of (string list) [@@unboxed]

let _menhir_action_1 =
  fun () ->
    (
# 9 "lib/parser.mly"
      ( [] )
# 50 "lib/parser.ml"
     : (string list))

let _menhir_action_2 =
  fun _1 _2 ->
    (
# 10 "lib/parser.mly"
                ( _1 :: _2 )
# 58 "lib/parser.ml"
     : (string list))

let _menhir_action_3 =
  fun _1 ->
    (
# 7 "lib/parser.mly"
               ( _1 )
# 66 "lib/parser.ml"
     : (string list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | EOF ->
        "EOF"
    | WORD _ ->
        "WORD"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_4 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let _1 = _v in
      let _v = _menhir_action_3 _1 in
      MenhirBox_main _v
  
  let rec _menhir_run_2 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_WORD -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let MenhirCell1_WORD (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_2 _1 _2 in
      _menhir_goto_line _menhir_stack _v _menhir_s
  
  and _menhir_goto_line : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState0 ->
          _menhir_run_4 _menhir_stack _v
      | MenhirState1 ->
          _menhir_run_2 _menhir_stack _v
  
  let rec _menhir_run_1 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_WORD (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WORD _v_0 ->
          _menhir_run_1 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState1
      | EOF ->
          let _v_1 = _menhir_action_1 () in
          _menhir_run_2 _menhir_stack _v_1
  
  let _menhir_run_0 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WORD _v ->
          _menhir_run_1 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState0
      | EOF ->
          let _v = _menhir_action_1 () in
          _menhir_run_4 _menhir_stack _v
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_0 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
