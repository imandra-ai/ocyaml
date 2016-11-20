/* OCaml bindings for libyaml                                          */
/* (c)Copyright Aesthetic Integration, Ltd., 2016                      */
/* All rights reserved.                                                */
/*                                                                     */
/* Released under Apache 2.0 license as described in the file LICENSE. */
/*                                                                     */
/* Contributors:                                                       */
/* Konstantin kanishchev (kostya@aestheticintegration.com)             */
/*                                                                     */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/misc.h>
#include <caml/signals.h>

#include <stdio.h>
#include <yaml.h>

static struct custom_operations yaml_custom_ops = {
    "com.aestheticintegration.ocyaml.yaml_parser_state",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

typedef struct parser_state_s {
    FILE * fh;
    yaml_parser_t * parser;
} parser_state_t;

#define Pstate_val(v) (*((parser_state_t **) Data_custom_val(v)))

CAMLprim 
value open_parser(value v1){
    CAMLparam1(v1);
    CAMLlocal1(res);

    parser_state_t * pstate = (parser_state_t *) malloc(sizeof(parser_state_t));
    pstate->parser = (yaml_parser_t *) malloc(sizeof(yaml_parser_t));

    pstate->fh = fopen( String_val(v1), "r");
    // TODO: OCAML exceptions here
    if(pstate->fh == NULL)
        fputs("Failed to open file!\n", stderr);
    if(!yaml_parser_initialize(pstate->parser))
        fputs("Failed to initialize parser!\n", stderr);

    yaml_parser_set_input_file(pstate->parser, pstate->fh);

    res = caml_alloc_custom( &yaml_custom_ops, sizeof(pstate), 0, 1 );
    Pstate_val(res) = pstate;

    CAMLreturn(res);
}

CAMLprim 
value close_parser(value v1){
    CAMLparam1(v1);
    parser_state_t * pstate = Pstate_val(v1);
    
    yaml_parser_delete(pstate->parser);
    fclose(pstate->fh);

    free((void *)pstate->parser);
    free((void *)pstate);

    CAMLreturn(Val_unit);
}

int getEncodingCaseNumber(yaml_encoding_t e){
    switch(e) {
        case YAML_ANY_ENCODING:     return 0; 
        case YAML_UTF8_ENCODING:    return 1; 
        case YAML_UTF16LE_ENCODING: return 2; 
        case YAML_UTF16BE_ENCODING: return 3; 
        default: return 0;
    }
}

int getScalarStyleCaseNumber(yaml_scalar_style_t s){
    switch(s) {
        case YAML_ANY_SCALAR_STYLE:           return 0;
        case YAML_PLAIN_SCALAR_STYLE:         return 1;
        case YAML_SINGLE_QUOTED_SCALAR_STYLE: return 2;
        case YAML_DOUBLE_QUOTED_SCALAR_STYLE: return 3;
        case YAML_LITERAL_SCALAR_STYLE:       return 4;
        case YAML_FOLDED_SCALAR_STYLE:        return 5;
        default: return 0;
    }
}


int getTokenCaseNumber(yaml_token_type_t type){
    switch(type) {
        case YAML_NO_TOKEN:                   return  0;
        case YAML_STREAM_END_TOKEN:           return  1;
        case YAML_DOCUMENT_START_TOKEN:       return  2;
        case YAML_DOCUMENT_END_TOKEN:         return  3;
        case YAML_BLOCK_SEQUENCE_START_TOKEN: return  4;
        case YAML_BLOCK_MAPPING_START_TOKEN:  return  5;
        case YAML_BLOCK_END_TOKEN:            return  6;
        case YAML_FLOW_SEQUENCE_START_TOKEN:  return  7;
        case YAML_FLOW_SEQUENCE_END_TOKEN:    return  8;
        case YAML_FLOW_MAPPING_START_TOKEN:   return  9;
        case YAML_FLOW_MAPPING_END_TOKEN:     return 10;
        case YAML_BLOCK_ENTRY_TOKEN:          return 11;
        case YAML_FLOW_ENTRY_TOKEN:           return 12;
        case YAML_KEY_TOKEN:                  return 13;
        case YAML_VALUE_TOKEN:                return 14;
        case YAML_STREAM_START_TOKEN:         return  0; // of encoding
        case YAML_VERSION_DIRECTIVE_TOKEN:    return  1; // of int * int
        case YAML_TAG_DIRECTIVE_TOKEN:        return  2; // of string * string
        case YAML_ALIAS_TOKEN:                return  3; // of string
        case YAML_ANCHOR_TOKEN:               return  4; // of string
        case YAML_TAG_TOKEN:                  return  5; // of string * string
        case YAML_SCALAR_TOKEN:               return  6; // of scalar_type * string
        default: return 0; // TODO: exception?
    }
}


CAMLprim 
value next_token(value v1){
    CAMLparam1(v1);
    CAMLlocal2(res,local);

    parser_state_t * pstate = Pstate_val(v1);

    yaml_token_t  token;   
    yaml_parser_scan(pstate->parser, &token);

    switch(token.type) {
        case YAML_STREAM_START_TOKEN:
            res = caml_alloc(1, getTokenCaseNumber(token.type));
            Store_field(res, 0, getEncodingCaseNumber(token.data.stream_start.encoding));
            break;
        case YAML_VERSION_DIRECTIVE_TOKEN: // of int * int
            res = caml_alloc(1, getTokenCaseNumber(token.type));
            Store_field(res, 0, Val_int(token.data.version_directive.major));
            Store_field(res, 1, Val_int(token.data.version_directive.minor));
            break;
        case YAML_TAG_DIRECTIVE_TOKEN: //  of string * string   
            res = caml_alloc(1, getTokenCaseNumber(token.type));
            Store_field(res, 0, caml_copy_string(token.data.tag_directive.handle));
            Store_field(res, 1, caml_copy_string(token.data.tag_directive.prefix));
            break;
        case YAML_ALIAS_TOKEN:  // of string
            res = caml_alloc(1, getTokenCaseNumber(token.type));
            Store_field(res, 0, caml_copy_string(token.data.alias.value));
            break;
        case YAML_ANCHOR_TOKEN: // of string
            res = caml_alloc(1, getTokenCaseNumber(token.type));
            Store_field(res, 0, caml_copy_string(token.data.anchor.value));
            break;
        case YAML_TAG_TOKEN: // of string * string
            res = caml_alloc(2, getTokenCaseNumber(token.type));
            Store_field(res, 0, caml_copy_string(token.data.tag_directive.handle));
            Store_field(res, 1, caml_copy_string(token.data.tag_directive.prefix));
            break;
        case YAML_SCALAR_TOKEN:// of scalar_style * string
            res = caml_alloc(2, getTokenCaseNumber(token.type));
            Store_field(res, 0, getScalarStyleCaseNumber(token.data.scalar.style));
            Store_field(res, 1, caml_copy_string(token.data.scalar.value));
            break;
        default:
            res = Val_int(getTokenCaseNumber(token.type));
    }
    yaml_token_delete(&token);

    CAMLreturn(res);
}

int getErrorCase( yaml_error_type_t err) {
    switch(err) {
        case YAML_NO_ERROR       : return 0;
        case YAML_MEMORY_ERROR   : return 1;
        case YAML_READER_ERROR   : return 2;
        case YAML_SCANNER_ERROR  : return 3;
        case YAML_PARSER_ERROR   : return 4;
        case YAML_COMPOSER_ERROR : return 5;
        case YAML_WRITER_ERROR   : return 6;
        case YAML_EMITTER_ERROR  : return 7;
    }
}


CAMLprim 
value get_error(value v1){
    CAMLparam1(v1);
    CAMLlocal1(res);

    parser_state_t * pstate = Pstate_val(v1);
    res = caml_alloc(3, 0);
    Store_field(res, 0, Val_int(getErrorCase(pstate->parser->error)));
    Store_field(res, 1, caml_copy_string(pstate->parser->problem));
    Store_field(res, 2, caml_copy_string(pstate->parser->context));
    
    CAMLreturn(res);
}
