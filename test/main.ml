open Kaputt
open Ocyaml

let add_test name ~input ~expected =
  Test.add_simple_test
    ~title:name
    (fun () ->
       let filename, out_channel =
         Filename.open_temp_file "ocyaml_test" name in
       output_string out_channel input;
       close_out out_channel;
       let actual =
         Ocyaml.load filename in
       Assertion.equal ~eq:Ocyaml.equal expected actual)

let () =
  add_test "structure_collection_no_indent.yaml"
    ~input:(String.concat "\n"
              [ "some_things:"
              ; "- first"
              ; "- second"
              ])
    ~expected:Ocyaml.(
        Structure
          [ ( Scalar "some_things"
            , Collection
                [ Scalar "first"; Scalar "second" ]
            )
          ]
      );

  add_test "structure_collection_no_indent_followed_by_key.yaml"
    ~input:(String.concat "\n"
              [ "some_things:"
              ; "- first"
              ; "- second"
              ; "some_indented_things:"
              ; "  - first"
              ; "  - second"
              ])
    ~expected:Ocyaml.(
        Structure
          [ ( Scalar "some_things"
            , Collection
                [ Scalar "first"; Scalar "second" ]
            )
          ; ( Scalar "some_indented_things"
            , Collection
                [ Scalar "first"; Scalar "second" ]
            )
          ]
      );

  Test.launch_tests ()
