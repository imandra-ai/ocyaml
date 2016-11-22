(* OCaml bindings for libyaml                                          *)
(* (c)Copyright Aesthetic Integration, Ltd., 2016                      *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* Released under Apache 2.0 license as described in the file LICENSE. *)
(*                                                                     *)
(* Contributors:                                                       *)
(* Konstantin kanishchev (kostya@aestheticintegration.com)             *)
(*                                                                     *)

type yaml =
    | Scalar of string
    | Collection of yaml list
    | Structure of (yaml * yaml) list

(** Many bad things could happen with the parser and emitter. *)
type yaml_error_type =
    | YAML_NO_ERROR       (** No error is produced. *)
    | YAML_MEMORY_ERROR   (** Cannot allocate or reallocate a block of memory. *)
    | YAML_READER_ERROR   (** Cannot read or decode the input stream. *)
    | YAML_SCANNER_ERROR  (** Cannot scan the input stream. *)
    | YAML_PARSER_ERROR   (** Cannot parse the input stream. *)
    | YAML_COMPOSER_ERROR (** Cannot compose a YAML document. *)
    | YAML_WRITER_ERROR   (** Cannot write to the output stream. *)
    | YAML_EMITTER_ERROR  (** Cannot emit a YAML stream. *)


(** The stream encoding. *)
type encoding =
    | YAML_ANY_ENCODING     (* Let the parser choose the encoding. *)
    | YAML_UTF8_ENCODING    (** The default UTF-8 encoding. *)
    | YAML_UTF16LE_ENCODING (** The UTF-16-LE encoding with BOM. *)
    | YAML_UTF16BE_ENCODING (** The UTF-16-BE encoding with BOM. *)

(** Scalar styles. *)
type scalar_style =
    | YAML_ANY_SCALAR_STYLE (** Let the emitter choose the style. *)
    | YAML_PLAIN_SCALAR_STYLE (** The plain scalar style. *)
    | YAML_SINGLE_QUOTED_SCALAR_STYLE (** The single-quoted scalar style. *)
    | YAML_DOUBLE_QUOTED_SCALAR_STYLE (** The double-quoted scalar style. *)
    | YAML_LITERAL_SCALAR_STYLE (** The literal scalar style. *)
    | YAML_FOLDED_SCALAR_STYLE  (** The folded scalar style. *)


(** Token types. *)
type token =
    | YAML_NO_TOKEN
    | YAML_STREAM_START_TOKEN of encoding
    | YAML_STREAM_END_TOKEN
    | YAML_VERSION_DIRECTIVE_TOKEN of int * int
    | YAML_TAG_DIRECTIVE_TOKEN  of string * string
    | YAML_DOCUMENT_START_TOKEN
    | YAML_DOCUMENT_END_TOKEN
    | YAML_BLOCK_SEQUENCE_START_TOKEN
    | YAML_BLOCK_MAPPING_START_TOKEN
    | YAML_BLOCK_END_TOKEN
    | YAML_FLOW_SEQUENCE_START_TOKEN
    | YAML_FLOW_SEQUENCE_END_TOKEN
    | YAML_FLOW_MAPPING_START_TOKEN
    | YAML_FLOW_MAPPING_END_TOKEN
    | YAML_BLOCK_ENTRY_TOKEN
    | YAML_FLOW_ENTRY_TOKEN
    | YAML_KEY_TOKEN
    | YAML_VALUE_TOKEN
    | YAML_ALIAS_TOKEN of string
    | YAML_ANCHOR_TOKEN of string
    | YAML_TAG_TOKEN of string * string
    | YAML_SCALAR_TOKEN of scalar_style * string

let string_of_token = function
  | YAML_NO_TOKEN -> "YAML_NO_TOKEN"
  | YAML_STREAM_START_TOKEN _ -> "YAML_STREAM_START_TOKEN _"
  | YAML_STREAM_END_TOKEN -> "YAML_STREAM_END_TOKEN"
  | YAML_VERSION_DIRECTIVE_TOKEN (_,_) -> "YAML_VERSION_DIRECTIVE_TOKEN (_,_)"
  | YAML_TAG_DIRECTIVE_TOKEN (_,_) -> "YAML_TAG_DIRECTIVE_TOKEN (_,_)"
  | YAML_DOCUMENT_START_TOKEN -> "YAML_DOCUMENT_START_TOKEN"
  | YAML_DOCUMENT_END_TOKEN -> "YAML_DOCUMENT_END_TOKEN"
  | YAML_BLOCK_SEQUENCE_START_TOKEN -> "YAML_BLOCK_SEQUENCE_START_TOKEN"
  | YAML_BLOCK_MAPPING_START_TOKEN -> "YAML_BLOCK_MAPPING_START_TOKEN"
  | YAML_BLOCK_END_TOKEN -> "YAML_BLOCK_END_TOKEN"
  | YAML_FLOW_SEQUENCE_START_TOKEN -> "YAML_FLOW_SEQUENCE_START_TOKEN"
  | YAML_FLOW_SEQUENCE_END_TOKEN -> "YAML_FLOW_SEQUENCE_END_TOKEN"
  | YAML_FLOW_MAPPING_START_TOKEN -> "YAML_FLOW_MAPPING_START_TOKEN"
  | YAML_FLOW_MAPPING_END_TOKEN -> "YAML_FLOW_MAPPING_END_TOKEN"
  | YAML_BLOCK_ENTRY_TOKEN -> "YAML_BLOCK_ENTRY_TOKEN"
  | YAML_FLOW_ENTRY_TOKEN -> "YAML_FLOW_ENTRY_TOKEN"
  | YAML_KEY_TOKEN -> "YAML_KEY_TOKEN"
  | YAML_VALUE_TOKEN -> "YAML_VALUE_TOKEN"
  | YAML_ALIAS_TOKEN _ -> "YAML_ALIAS_TOKEN _"
  | YAML_ANCHOR_TOKEN _ -> "YAML_ANCHOR_TOKEN _"
  | YAML_TAG_TOKEN (_,_) -> "YAML_TAG_TOKEN (_,_)"
  | YAML_SCALAR_TOKEN (_,_) -> "YAML_SCALAR_TOKEN (_,_)"

type yaml_parser
external open_parser  : string -> yaml_parser = "open_parser"
external close_parser : yaml_parser -> unit = "close_parser"
external next_token   : yaml_parser -> token = "next_token"
external get_error    : yaml_parser -> (yaml_error_type * string * string) = "get_error"

let rec parse_sequence p =
    let rec scan () =
        match next_token p  with
        | YAML_BLOCK_END_TOKEN -> []
        | YAML_BLOCK_ENTRY_TOKEN ->
            let entry = parse_yaml (next_token p) p in
            entry :: scan ()
        | token  -> failwith ("Unexpected YAML token in sequence: " ^ string_of_token token)
        in
    Collection ( scan () |> List.rev )

and parse_sequence_flow p =
    (* In a flow sequence, we expect one entry before the first entry token. *)
    match next_token p with
    | YAML_FLOW_SEQUENCE_END_TOKEN -> Collection []
    | token ->
      let first_entry = parse_yaml token p in
      let rec scan () =
          match next_token p  with
          | YAML_FLOW_SEQUENCE_END_TOKEN -> []
          | YAML_FLOW_ENTRY_TOKEN ->
              let entry = parse_yaml (next_token p) p in
              entry :: scan ()
          | token  -> failwith ("Unexpected YAML token in flow sequence: " ^ string_of_token token)
          in
      Collection ( first_entry :: (scan () |> List.rev) )

and parse_mapping p =
    let rec scan key () = match next_token p, key with
        | YAML_BLOCK_END_TOKEN, None -> []
        | YAML_KEY_TOKEN,       None ->
            let key   = parse_yaml (next_token p) p in
            scan (Some key) ()
        | YAML_VALUE_TOKEN,     Some key ->
            let value = parse_yaml (next_token p) p in
            ( key, value)  :: scan None ()
        | YAML_BLOCK_END_TOKEN, Some _ -> failwith "Unmatched key token."
        | YAML_KEY_TOKEN,       Some _ -> failwith "Two key tokens in a row."
        | YAML_VALUE_TOKEN,     None   -> failwith "Value token without a key"
        | token , _ -> failwith ("Unexpected YAML token in mapping: " ^ string_of_token token)
        in
    Structure ( scan None () |> List.rev )

and parse_yaml token p =
    match token  with
    | YAML_STREAM_START_TOKEN _ -> parse_yaml (next_token p) p
    | YAML_BLOCK_SEQUENCE_START_TOKEN -> parse_sequence p
    | YAML_FLOW_SEQUENCE_START_TOKEN -> parse_sequence_flow p
    | YAML_BLOCK_MAPPING_START_TOKEN  -> parse_mapping p
    | YAML_SCALAR_TOKEN ( _ , v ) -> Scalar v
    | YAML_NO_TOKEN ->  begin
        match get_error p with
            | YAML_NO_ERROR, _ , _ -> failwith "Unexpected end of YAML stream"
            | _ , errmsg, ctx -> failwith ("YAML parsing error: " ^ errmsg ^ ". Context: " ^ ctx)
        end
    | _ -> failwith ("Unexpected YAML token: " ^ string_of_token token)

let load filename =
    let p = open_parser filename in
    let yaml = parse_yaml (next_token p) p in
    let () = close_parser p in
    yaml
