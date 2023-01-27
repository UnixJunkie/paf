
module L = BatList
module A = BatArray

(* fbr: - fork with pipes writing in sequence
        - fork with pipes but poll
        - threads with Domainslib.Chan
        - also vary csize
*)

let to_chan chan x =
  Marshal.(to_channel chan x [No_sharing; Closures])
    
let from_chan chan =
  Marshal.from_channel chan

(* send integers *)
let demux max_count _csize out_chans =
  let pipe_index = ref 0 in
  let n_pipes = A.length out_chans in
  for i = 0 to max_count - 1 do
    let pipe = out_chans.(!pipe_index) in
    to_chan pipe i;
    pipe_index := (!pipe_index + 1) mod n_pipes
  done;
  A.iter close_out out_chans

(* increment received int; send it out *)
let work in_pipe out_pipe =
  try to_chan out_pipe (1 + (from_chan in_pipe))
  with End_of_file -> begin
      close_in in_pipe;
      close_out out_pipe
    end

let mux in_pipes =
  let total = ref 0 in
  let num_pipes = A.length in_pipes in
  let n = ref num_pipes in
  let opened = A.make num_pipes true in
  let i = ref 0 in
  while !n > 0 do
    (if opened.(!i) then
       let in_pipe = in_pipes.(!i) in       
       try
         let x = from_chan in_pipe in
         total := !total + x
       with End_of_file ->
         begin
           close_in in_pipe;
           opened.(!i) <- false
         end
    );
    i := (!i + 1) mod num_pipes
  done;
  Printf.printf "total: %d\n" !total

let main () =
  let nprocs = int_of_string Sys.argv.(1) in
  let to_worker_pipes = A.init nprocs (fun _i -> Unix.pipe ()) in
  let from_worker_pipes = A.init nprocs (fun _i -> Unix.pipe ()) in
  let to_workers =   A.map (fun (_pipe_exit, pipe_entry) -> Unix.out_channel_of_descr pipe_entry) to_worker_pipes in
  let from_demuxer = A.map (fun (pipe_exit, _pipe_entry) -> Unix.in_channel_of_descr  pipe_exit)  to_worker_pipes in
  let from_workers = A.map (fun (pipe_exit, _pipe_entry) -> Unix.in_channel_of_descr  pipe_exit)  from_worker_pipes in
  let to_muxer =     A.map (fun (_pipe_exit, pipe_entry) -> Unix.out_channel_of_descr pipe_entry) from_worker_pipes in
  (* start muxer *)
  (match Unix.fork () with
   | 0 -> (* forked out *) mux from_workers
   | _pid -> ()
  );
  (* start workers *)
  for i = 0 to nprocs - 1 do
    match Unix.fork () with
    | 0 -> (* forked out *) work from_demuxer.(i) to_muxer.(i)
    | _pid -> ()
  done;
  (* start demuxer *)
  demux 1_000_000 0 to_workers

let () = main ()
