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
#include <caml/misc.h>
#include <caml/signals.h>

const int Scalar = 0; 
const int Collection = 1;
const int Structure = 2;

#include <stdio.h>
#include <yaml.h>


CAMLprim value read_yaml(yaml_parser_t * p_parser);

CAMLprim value 
read_structure(yaml_parser_t * p_parser)
{
    CAMLparam0();
    CAMLlocal4(res, hd, tl, pair);
    tl = Val_emptylist;
    yaml_token_t token;   
    do {
        yaml_parser_scan(p_parser, &token);
        switch(token.type){
            case YAML_KEY_TOKEN:   
//                printf("Key \n");
                yaml_token_delete(&token);
                pair = caml_alloc(2,0);
                Store_field( pair, 0, read_yaml(p_parser) );  
                break;
            case YAML_VALUE_TOKEN: 
//                printf("Value \n");
                yaml_token_delete(&token);
                Store_field( pair, 1, read_yaml(p_parser) );  
                hd = caml_alloc(2,0);
                Store_field( hd, 0, pair);  
                Store_field( hd, 1, tl );              
                tl = hd;
                break;
            case YAML_BLOCK_END_TOKEN: 
//                printf("Mapping end \n");
                break;
            default:
                // TODO: Exception here
//                printf("Got unexpected token of type %d while reading a structure\n", token.type); fflush(stdout);
                yaml_token_delete(&token);
        }
    } while(token.type != YAML_BLOCK_END_TOKEN);
    yaml_token_delete(&token);
    res = caml_alloc(1, Structure);
    Store_field(res, 0, tl);
    CAMLreturn (res);
}

CAMLprim value 
read_sequence(yaml_parser_t * p_parser)
{
    CAMLparam0();
    CAMLlocal3(res, hd, tl);
    tl = Val_emptylist;
    yaml_token_t token;   
    do {
        yaml_parser_scan(p_parser, &token);
        switch(token.type){
            case YAML_BLOCK_ENTRY_TOKEN:   
//                printf("Block entry\n");
                yaml_token_delete(&token);
                hd = caml_alloc(2,0);
                Store_field( hd, 0, read_yaml(p_parser));  
                Store_field( hd, 1, tl );              
                tl = hd;
                break;
            case YAML_BLOCK_END_TOKEN: 
//                printf("Sequence end\n");
                break;
            default:
                // TODO: Exception here
//                printf("Got unexpected token of type %d while reading a sequence\n", token.type); fflush(stdout);
                yaml_token_delete(&token);
        }
    } while(token.type != YAML_BLOCK_END_TOKEN);
    yaml_token_delete(&token);
    res = caml_alloc(1, Collection);
    Store_field(res, 0, tl);
    CAMLreturn (res);
}

CAMLprim value 
read_yaml(yaml_parser_t * p_parser)
{
    CAMLparam0();
    CAMLlocal1(res);
    yaml_token_t token;   
    yaml_parser_scan(p_parser, &token);
    switch(token.type){
        case YAML_BLOCK_MAPPING_START_TOKEN:  
//            printf("Mapping start\n");
            yaml_token_delete(&token);
            res = read_structure(p_parser); 
            break;
        case YAML_BLOCK_SEQUENCE_START_TOKEN: 
//            printf("Sequence start\n");
            yaml_token_delete(&token);
            res = read_sequence (p_parser); 
            break;
        case YAML_SCALAR_TOKEN:     
//            printf("Scalar \n");
            res = caml_alloc(1, Scalar);
            Store_field(res, 0, caml_copy_string(token.data.scalar.value));
            yaml_token_delete(&token);
            break;
        default:
            // TODO: Exception here
//            printf("Got unexpected yaml token of type %d\n", token.type); fflush(stdout);
            yaml_token_delete(&token);
    }
    CAMLreturn (res);
}

CAMLprim value test (value v1)
{
    CAMLparam1(v1);
    CAMLlocal1(res);

    FILE *fh = fopen( String_val(v1), "r");
    yaml_parser_t parser;
    yaml_token_t  token;   

    // TODO: OCAML exceptions here
    if(!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if(fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);
    yaml_parser_scan(&parser, &token);

    do {
        switch(token.type)
        {
            case YAML_STREAM_START_TOKEN: 
//                printf("Stream start\n");
                yaml_token_delete(&token);
                res = read_yaml(&parser); 
                CAMLreturn (res);
                break;
            case YAML_STREAM_END_TOKEN:
//                printf("Stream end\n");
                break;
            default:
            //    printf("Got unexpected toplevel token of type %d\n", token.type);
                yaml_token_delete(&token);
        }
    } while(token.type != YAML_STREAM_END_TOKEN);
    yaml_token_delete(&token);
    CAMLreturn (res);
}
