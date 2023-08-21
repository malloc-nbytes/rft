let usage () =
  let _ = print_endline "usage: ./rft <.filetype>" in
  failwith "usage"

let delete_files files =
  List.iter (fun file ->
      try
        Sys.remove file
      with
      | Sys_error msg -> Printf.eprintf "ERR: could not remove file %s: [%s]\n" file msg
    ) files

let find_files filetype =
  let rec traverse_dirs dir =
    let entries = Sys.readdir dir in
    let matching_files =
      Array.fold_left (fun acc entry ->
          let full_path = Filename.concat dir entry in
          if Sys.is_directory full_path then
            traverse_dirs full_path @ acc
          else if Filename.check_suffix entry filetype then
            full_path :: acc
          else
            acc
        ) [] entries in
    matching_files in
  traverse_dirs (Sys.getcwd ())

let () =
  let argv = Sys.argv and argc = Array.length Sys.argv in

  let _ = if argc != 2 then
            usage ()
          else () in

  let filetype = argv.(1) in

  let files_to_remove = find_files filetype in

  match files_to_remove with
  | [] ->
     let _ = print_endline "No files found" in
     print_endline "Aborting"
  | _ ->
     let _ = Printf.printf "Found %d files:\n" (List.length files_to_remove) in
     let _ = List.iter (fun f -> Printf.printf "  %s\n" f) files_to_remove in
     let _ = print_endline "Delete these files? [Y/n]" in
     let ans = read_line () in
     (match String.lowercase_ascii ans with
     | "y" -> delete_files files_to_remove
     | _ -> print_endline "Aborting")
