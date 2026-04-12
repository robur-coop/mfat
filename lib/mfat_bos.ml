(* Copyright (c) 2016 The bos programmers
 * SPDX-License-Identifier: ISC
 * Copyright (c) 2026 Romain Calascibetta <romain.calascibetta@gmail.com>
 *)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let src = Logs.Src.create "mfat.fancy"
let ( let* ) = Result.bind

module Log = (val Logs.src_log src : Logs.LOG)
module S = Set.Make (String)
module M = Map.Make (String)

let of_fpath ~where path =
  let segs = Fpath.segs path in
  let trim = List.drop_while (( = ) "") in
  let segs = trim segs in
  let segs = List.rev (trim (List.rev segs)) in
  match segs with
  | [] -> Fmt.failwith "%s: empty path: %a" where Fpath.pp path
  | segs when List.exists (( = ) "") segs ->
      Fmt.failwith "%s: empty segment: %a" where Fpath.pp path
  | segs -> String.concat "/" segs

module Pat = struct
  let err_malformed_pat = Fmt.str "malformed named string pattern: %S"

  (* Patterns *)

  type lexeme = Lit of string | Var of string
  type t = lexeme list
  type parse_state = S_lit | S_dollar | S_var

  let of_string s =
    let b = Buffer.create 255 in
    let flush b =
      let s = Buffer.contents b in
      Buffer.clear b; s
    in
    let err () = Error (`Msg (err_malformed_pat s)) in
    let push_lit b acc =
      if Buffer.length b <> 0 then Lit (flush b) :: acc else acc
    in
    let max_i = String.length s - 1 in
    let rec loop acc state i =
      if i > max_i then
        if state <> S_lit then err () else Ok (List.rev (push_lit b acc))
      else
        match state with
        | S_lit ->
            begin match s.[i] with
            | '$' -> loop acc S_dollar (i + 1)
            | c ->
                Buffer.add_char b c;
                loop acc S_lit (i + 1)
            end
        | S_dollar ->
            begin match s.[i] with
            | '$' ->
                Buffer.add_char b '$';
                loop acc S_lit (i + 1)
            | '(' -> loop (push_lit b acc) S_var (i + 1)
            | _ -> err ()
            end
        | S_var ->
            begin match s.[i] with
            | ')' -> loop (Var (flush b) :: acc) S_lit (i + 1)
            | ',' -> err ()
            | c ->
                Buffer.add_char b c;
                loop acc S_var (i + 1)
            end
    in
    loop [] S_lit 0

  let v s = match of_string s with Ok v -> v | Error (`Msg e) -> invalid_arg e

  let to_string p =
    let b = Buffer.create 255 in
    let add = function
      | Lit l ->
          let max_i = String.length l - 1 in
          let rec loop start i =
            if i > max_i then Buffer.add_substring b l start (i - start)
            else if l.[i] <> '$' then loop start (i + 1)
            else begin
              Buffer.add_substring b l start (i - start + 1);
              Buffer.add_char b '$';
              let next = i + 1 in
              loop next next
            end
          in
          loop 0 0
      | Var v -> Buffer.(add_string b "$("; add_string b v; add_char b ')')
    in
    List.iter add p; Buffer.contents b

  (* Substitution *)

  type defs = string M.t

  let _subst ?(undef = fun _ -> None) defs p =
    let subst acc = function
      | Lit _ as l -> l :: acc
      | Var v as var -> (
          match M.find v defs with
          | Some lit -> Lit lit :: acc
          | None -> (
              match undef v with
              | Some lit -> Lit lit :: acc
              | None -> var :: acc))
    in
    List.(rev (fold_left subst [] p))

  let _format ?(undef = fun _ -> "") defs p =
    let b = Buffer.create 255 in
    let add = function
      | Lit l -> Buffer.add_string b l
      | Var v ->
          begin match M.find v defs with
          | Some s -> Buffer.add_string b s
          | None -> Buffer.add_string b (undef v)
          end
    in
    List.iter add p; Buffer.contents b

  (* Matching
   N.B. matching is not t.r. but stack is bounded by number of variables. *)

  let match_literal pos s lit =
    (* matches [lit] at [pos] in [s]. *)
    let l_len = String.length lit in
    let s_len = String.length s - pos in
    if l_len > s_len then None
    else
      try
        for i = 0 to l_len - 1 do
          if lit.[i] <> s.[pos + i] then raise Exit
        done;
        Some (pos + l_len)
      with Exit -> None

  let with_index_range ?(first = 0) ?last str =
    let str_len = String.length str in
    let max_idx = str_len - 1 in
    let last =
      match last with
      | None -> max_idx
      | Some last -> if last > max_idx then max_idx else last
    in
    let first = if first < 0 then 0 else first in
    if first > max_idx || last < 0 || first > last then ""
    else if first = 0 && last = max_idx then str
    else String.sub str first (last + 1 - first)

  let match_pat ~env pos s pat =
    let init, no_env =
      match env with
      | None -> (Some M.empty, true)
      | Some _ as init -> (init, false)
    in
    let rec loop pos = function
      | [] -> if pos = String.length s then init else None
      | Lit lit :: p ->
          begin match match_literal pos s lit with
          | None -> None
          | Some pos -> loop pos p
          end
      | Var n :: p ->
          let rec try_match next_pos =
            if next_pos < pos then None
            else
              match loop next_pos p with
              | None -> try_match (next_pos - 1)
              | Some m as r ->
                  if no_env then r
                  else
                    let first = pos and last = next_pos - 1 in
                    let str = with_index_range s ~first ~last in
                    Some (M.add n str m)
          in
          try_match (String.length s)
      (* Longest match first. *)
    in
    loop pos pat

  let matches p s = match_pat ~env:None 0 s p <> None
  let query ?(init = M.empty) p s = match_pat ~env:(Some init) 0 s p
end

module Make (Blk : Mfat.BLOCK) = struct
  include Mfat.Make (Blk)

  let file_exists t filepath =
    let filepath = of_fpath ~where:"file_exists" filepath in
    match stat t filepath with
    | Ok { Mfat.is_dir= false; _ } -> Ok true
    | Ok _ -> Ok false
    | Error _ -> Ok false

  let dir_exists t dirpath =
    let dirpath = of_fpath ~where:"dir_exists" dirpath in
    match stat t dirpath with
    | Ok { Mfat.is_dir= true; _ } -> Ok true
    | Ok _ -> Ok false
    | Error _ -> Ok false

  let exists t path =
    let path = of_fpath ~where:"exists" path in
    match stat t path with Ok _ -> Ok true | Error _ -> Ok false

  let file_must_exist t filepath =
    let filepath' = of_fpath ~where:"file_must_exist" filepath in
    match stat t filepath' with
    | Ok { Mfat.is_dir= false; _ } -> Ok filepath
    | Ok _ -> error_msgf "%a: Not a file" Fpath.pp filepath
    | Error (`Msg msg) ->
        error_msgf "file %a must exist: %s" Fpath.pp filepath msg

  let dir_must_exist t dirpath =
    let dirpath' = of_fpath ~where:"dir_must_exist" dirpath in
    match stat t dirpath' with
    | Ok { Mfat.is_dir= true; _ } -> Ok dirpath
    | Ok _ -> error_msgf "%a: Not a directory" Fpath.pp dirpath
    | Error (`Msg msg) ->
        error_msgf "directory %a must exist: %s" Fpath.pp dirpath msg

  let must_exist t path =
    let path' = of_fpath ~where:"must_exist" path in
    match stat t path' with
    | Ok { Mfat.is_dir= true; _ } -> Ok (Fpath.to_dir_path path)
    | Ok _ -> Ok path
    | Error (`Msg msg) -> error_msgf "%a must exist: %s" Fpath.pp path msg

  let delete_file t ?(must_exist = false) filepath =
    let filepath' = of_fpath ~where:"delete_file" filepath in
    match stat t filepath' with
    | Ok { Mfat.is_dir= false; _ } ->
        let on_error (`Msg msg) =
          Log.err (fun m ->
              m "Unexpected error while removing %a: %s" Fpath.pp filepath msg)
        in
        let result = remove t filepath' in
        Result.iter_error on_error result;
        Ok ()
    | Ok { Mfat.is_dir= true; _ } ->
        error_msgf "%a is not a file" Fpath.pp filepath
    | Error _ when must_exist ->
        error_msgf "%a does not exist" Fpath.pp filepath
    | Error _ -> Ok ()

  let delete_dir t ?must_exist:(must = false) ?(recurse = false) dirpath =
    let rec delete_files to_rmdir dirs =
      match dirs with
      | [] -> Ok to_rmdir
      | dir :: todo ->
          let delete_dir_files dirs entry =
            match (dirs, entry) with
            | (Error _ as err), _ -> err
            | Ok dirs, { Mfat.is_dir= true; name; _ } ->
                Ok (Fpath.(to_dir_path (dir / name)) :: dirs)
            | Ok dirs, { name; _ } ->
                let filepath = Fpath.(dir / name) in
                let filepath' = of_fpath ~where:"delete_dir" filepath in
                let* () =
                  remove t filepath'
                  |> Result.map_error @@ fun _ ->
                     msgf "Impossible to remove %a" Fpath.pp filepath
                in
                Ok dirs
          in
          let* entries =
            ls t (of_fpath ~where:"delete_dir" dir)
            |> Result.map_error @@ fun _ ->
               msgf "Impossible to get entries from %a" Fpath.pp
                 (Fpath.to_dir_path dir)
          in
          let* dirs = List.fold_left delete_dir_files (Ok []) entries in
          delete_files (dir :: to_rmdir) (List.rev_append dirs todo)
    in
    let rec delete_dirs = function
      | [] -> Ok ()
      | dir :: dirs ->
          let* () =
            remove t (of_fpath ~where:"delete_dir" dir)
            |> Result.map_error @@ fun _ ->
               msgf "Impossible to remote %a" Fpath.pp (Fpath.to_dir_path dir)
          in
          delete_dirs dirs
    in
    let delete recurse dir =
      if not recurse then remove t (of_fpath ~where:"delete_dir" dir)
      else
        let* rmdirs = delete_files [] [ dir ] in
        delete_dirs rmdirs
    in
    let* dirpath = if must then dir_must_exist t dirpath else Ok dirpath in
    delete recurse dirpath

  let delete t ?(must_exist = false) ?(recurse = false) path =
    let path' = of_fpath ~where:"delete" path in
    match stat t path' with
    | Ok { Mfat.is_dir= true; _ } -> delete_dir t ~must_exist ~recurse path
    | Ok _ -> delete_file t ~must_exist path
    | Error _ when not must_exist -> Ok ()
    | Error _ -> error_msgf "%a does not exist" Fpath.pp path

  let stat t path =
    match stat t (of_fpath ~where:"stat" path) with
    | Ok _ as value -> value
    | Error _ -> error_msgf "%a does not exist" Fpath.pp path

  type 'a error =
       Fpath.t
    -> ('a, [ `Msg of string ]) result
    -> (unit, [ `Msg of string ]) result

  type elements =
    [ `Any
    | `Files
    | `Dirs
    | `Sat of blk Mfat.t -> Fpath.t -> (bool, [ `Msg of string ]) result ]

  type traverse =
    [ `Any
    | `None
    | `Sat of blk Mfat.t -> Fpath.t -> (bool, [ `Msg of string ]) result ]

  exception Fold_error of [ `Msg of string ]

  let errfn err fn ~backup =
   fun t path ->
    match fn t path with
    | Ok v -> v
    | Error _ as value -> (
        match err path value with
        | Ok () -> backup
        | Error err -> raise (Fold_error err))

  let err_predicate_fn err fn t = errfn err fn t ~backup:false

  let do_traverse_fn err = function
    | `Any -> fun _ _ -> true
    | `None -> fun _ _ -> false
    | `Sat sat -> err_predicate_fn err sat

  let is_element_fn err = function
    | `Any -> err_predicate_fn err exists
    | `Files -> err_predicate_fn err file_exists
    | `Dirs -> err_predicate_fn err dir_exists
    | `Sat sat -> err_predicate_fn err sat

  let is_dir_fn err =
    let is_dir t path =
      match stat t path with
      | Ok { Mfat.is_dir= true; _ } -> Ok true
      | Ok _ -> Ok false
      | Error _ -> error_msgf "Impossible to describe %a" Fpath.pp path
    in
    err_predicate_fn err is_dir

  let readdir_fn err =
    let readdir t d =
      match ls t (of_fpath ~where:"readdir" d) with
      | Ok entries ->
          let fn { Mfat.name; _ } =
            if name = "." || name = ".." then None else Some name
          in
          let names = List.filter_map fn entries in
          Ok names
      | Error _ as err -> err
    in
    errfn err readdir ~backup:[]

  let is_dotfile bname = String.length bname > 0 && bname.[0] = '.'

  let log path = function
    | Ok _ -> assert false
    | Error (`Msg msg) ->
        Log.err (fun m -> m "Mfat.fold (%a): %s" Fpath.pp path msg);
        Ok ()

  let fold ?(err = log) ?(dotfiles = false) ?(elements = `Any)
      ?(traverse = `Any) t fn acc paths =
    try
      let do_traverse = do_traverse_fn err traverse in
      let is_element = is_element_fn err elements in
      let is_dir = is_dir_fn err in
      let readdir = readdir_fn err in
      let process_path path (acc, to_traverse) =
        let acc = if is_element t path then fn path acc else acc in
        let to_traverse =
          if is_dir t path && do_traverse t path then path :: to_traverse
          else to_traverse
        in
        (acc, to_traverse)
      in
      let dir_child d acc bname =
        if (not dotfiles) && is_dotfile bname then acc
        else process_path Fpath.(d / bname) acc
      in
      let rec go acc = function
        | (d :: ds) :: up ->
            let childs = readdir t d in
            let acc, to_traverse =
              List.fold_left (dir_child d) (acc, []) childs
            in
            go acc (to_traverse :: ds :: up)
        | [] :: [] -> acc
        | [] :: up -> go acc up
        | _ -> assert false
      in
      let init acc path =
        let base = Fpath.(basename (normalize path)) in
        if (not dotfiles) && is_dotfile base then acc else process_path path acc
      in
      let acc, to_traverse = List.fold_left init (acc, []) paths in
      Ok (go acc (to_traverse :: []))
    with Fold_error err -> Error err

  module File = struct
    let exists t path = file_exists t path
    let must_exist t filepath = file_must_exist t filepath
    let delete t ?must_exist path = delete_file t ?must_exist path

    let read t path =
      let path' = of_fpath ~where:"File.read" path in
      let* chunks = to_seq t path' in
      let buf = Buffer.create 4096 in
      Seq.iter (Buffer.add_string buf) chunks;
      Ok (Buffer.contents buf)

    let read_lines t path =
      let path' = of_fpath ~where:"File.read_lines" path in
      let* chunks = to_seq t path' in
      let buf = Buffer.create 4096 in
      Seq.iter (Buffer.add_string buf) chunks;
      Ok (String.split_on_char '\n' (Buffer.contents buf))

    let fold_lines f acc t path =
      let path' = of_fpath ~where:"File.fold_lines" path in
      let* chunks = to_seq t path' in
      let buf = Buffer.create 4096 in
      Seq.iter (Buffer.add_string buf) chunks;
      let lines = String.split_on_char '\n' (Buffer.contents buf) in
      Ok (List.fold_left (fun a l -> f l a) acc lines)

    let write t path content =
      let path' = of_fpath ~where:"File.write" path in
      write t path' content

    let write_lines t path lines =
      let content = String.concat "\n" lines in
      write t path content
  end

  module Dir = struct
    let exists t path = dir_exists t path
    let must_exist t path = dir_must_exist t path

    let create t ?(path = true) dir =
      let segs = Fpath.segs dir in
      let segs = List.filter (fun str -> str <> "") segs in
      if path then begin
        let rec go current created = function
          | [] -> Ok created
          | seg :: rest ->
              let next =
                match current with
                | None -> Fpath.v seg
                | Some p -> Fpath.(p / seg)
              in
              let* is_dir = dir_exists t next in
              if is_dir then go (Some next) created rest
              else
                let* is_file = file_exists t next in
                if is_file then error_msgf "%a: not a directory" Fpath.pp next
                else
                  let* () = mkdir t (of_fpath ~where:"Dir.create" next) in
                  go (Some next) true rest
        in
        go None false segs
      end
      else begin
        let* is_dir = dir_exists t dir in
        if is_dir then Ok false
        else
          let* is_file = file_exists t dir in
          if is_file then error_msgf "%a: not a directory" Fpath.pp dir
          else
            let* () = mkdir t (of_fpath ~where:"Dir.create" dir) in
            Ok true
      end

    let delete t ?must_exist ?recurse path =
      delete_dir t ?must_exist ?recurse path

    let contents t ?(rel = false) dir =
      let dir' = of_fpath ~where:"Dir.contents" dir in
      let* entries = ls t dir' in
      let to_fpath entry =
        let name = entry.Mfat.name in
        let child = Fpath.v name in
        if entry.Mfat.is_dir then
          if rel then Fpath.to_dir_path child
          else Fpath.(to_dir_path (dir // child))
        else if rel then child
        else Fpath.(dir // child)
      in
      Ok (List.map to_fpath entries)

    let fold_contents ?err ?dotfiles ?elements ?traverse t f acc dir =
      let* paths = contents t ~rel:false dir in
      fold ?err ?dotfiles ?elements ?traverse t f acc paths
  end

  let matches t ?(dotfiles = false) pat =
    let parent = Fpath.parent pat in
    let base = Fpath.basename pat in
    match Pat.of_string base with
    | Error _ as e -> e
    | Ok pat ->
        let* entries = Dir.contents t ~rel:false parent in
        let entries =
          if not dotfiles then
            List.filter
              (fun p ->
                let b = Fpath.basename p in
                String.length b = 0 || b.[0] <> '.')
              entries
          else entries
        in
        let matching =
          List.filter
            (fun p ->
              let b = Fpath.basename (Fpath.rem_empty_seg p) in
              Pat.matches pat b)
            entries
        in
        Ok matching

  let query t ?(dotfiles = false) ?(init = M.empty) pat =
    let parent = Fpath.parent pat in
    let base = Fpath.basename pat in
    match Pat.of_string base with
    | Error _ as e -> e
    | Ok pat ->
        let* entries = Dir.contents t ~rel:false parent in
        let entries =
          if not dotfiles then
            let fn path =
              let bname = Fpath.basename path in
              String.length bname = 0 || bname.[0] <> '.'
            in
            List.filter fn entries
          else entries
        in
        let results =
          let fn path =
            let bname = Fpath.basename (Fpath.rem_empty_seg path) in
            match Pat.query ~init pat bname with
            | Some defs -> Some (path, defs)
            | None -> None
          in
          List.filter_map fn entries
        in
        Ok results
end
