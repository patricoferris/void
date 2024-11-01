module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let defs, mount_flags =
        C.C_define.import c ~c_flags:[ "-D_GNU_SOURCE" ]
          ~includes:[ "linux/sched.h"; "sys/mount.h" ]
          C.C_define.Type.
            [
              (* Clone3 Flags *)
              ("CLONE_PIDFD", Int);
              ("CLONE_NEWPID", Int);
              ("CLONE_NEWCGROUP", Int);
              ("CLONE_NEWNS", Int);
              ("CLONE_NEWIPC", Int);
              ("CLONE_NEWNET", Int);
              ("CLONE_NEWTIME", Int);
              ("CLONE_NEWUSER", Int);
              ("CLONE_NEWUTS", Int);
              (* Mount Flags *)
              ("MS_REMOUNT", Int);
              ("MS_BIND", Int);
              ("MS_SHARED", Int);
            ]
        |> List.fold_left
             (fun (cls, mnts) -> function
               | name, C.C_define.Value.Int v ->
                   let t = (String.lowercase_ascii name, v) in
                   if String.starts_with ~prefix:"CLONE" name then
                     (t :: cls, mnts)
                   else (cls, t :: mnts)
               | _ -> assert false)
             ([], [])
      in
      let sigs vs =
        List.map (fun (name, _) -> Printf.sprintf "  val %s : t" name) vs
      in
      let structs vs =
        List.map (fun (name, v) -> Printf.sprintf "  let %s = 0x%x" name v) vs
      in
      let flags_nspace =
        List.filter (String.starts_with ~prefix:"clone_new") (List.map fst defs)
      in
      let mount =
        [ "module Mount_flags : sig"; "  type t = int" ]
        @ sigs mount_flags
        @ [ "end = struct"; "  type t = int" ]
        @ structs mount_flags @ [ "end" ]
      in
      C.Flags.write_lines "config.ml"
        ([ "module Clone_flags : sig"; "  type t = int" ]
        @ sigs defs @ [ "val all : t list" ]
        @ [ "end = struct"; "  type t = int" ]
        @ structs defs
        @ [ "let all = [" ^ String.concat ";" flags_nspace ^ "]" ]
        @ [ "end" ] @ mount))
