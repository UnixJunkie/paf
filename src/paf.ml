
module A = BatArray

(* fbr: - fork with pipes writing in sequence
        - fork with pipes but poll
        - threads with Domainslib.Chan
        - also vary csize

There is probably a bug in Marshal.from_channel: it fails instead of blocking read
if the channel doesn't have a complete string representation of the marshalled value

*)

let to_out_fd fd x =
  let str = Marshal.(to_string x [No_sharing; Closures]) in
  let _written: int = Unix.write fd (Bytes.unsafe_of_string str) 0 (String.length str) in
  ()

let buff = Bytes.create (1024 * 1024)

let from_in_fd fd =
  (* read the fixed size marshalled value header *)
  let _read1 = Unix.read fd buff 0 Marshal.header_size in
  (* determine remaining size to read *)
  let to_read_next = Marshal.data_size buff 0 in
  assert(Marshal.header_size + to_read_next < Bytes.length buff);
  let _read2 = Unix.read fd buff Marshal.header_size to_read_next in
  Marshal.from_bytes buff 0

(* send integers *)
let demux max_count _csize out_fds =
  let pipe_index = ref 0 in
  let n_pipes = A.length out_fds in
  for i = 0 to max_count - 1 do
    let pipe = out_fds.(!pipe_index) in
    Printf.printf "d: sending %d\n%!" i;
    to_out_fd pipe i;
    pipe_index := (!pipe_index + 1) mod n_pipes
  done;
  A.iter Unix.close out_fds

(* increment received int; send it out *)
let work rank in_pipe out_pipe =
  try
    let received = from_in_fd in_pipe in
    Printf.printf "w %d: received: %d\n%!" rank received;
    to_out_fd out_pipe (received + 1)
  with End_of_file -> (Unix.close in_pipe;
                       Unix.close out_pipe)
     (* | Failure f -> (Printf.printf "w %d: failure %s\n%!" rank f; *)
     (*                 Unix.close in_pipe; *)
     (*                 Unix.close out_pipe) *)

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
         let x = from_in_fd in_pipe in
         Printf.printf "m received: %d\n%!" x;
         total := !total + x
       with End_of_file ->
         begin
           Unix.close in_pipe;
           opened.(!i) <- false
         end
    );
    i := (!i + 1) mod num_pipes
  done;
  Printf.printf "total: %d\n%!" !total

let main () =
  let nprocs = int_of_string Sys.argv.(1) in
  let to_worker_pipes = A.init nprocs (fun _i -> Unix.pipe ()) in
  let from_worker_pipes = A.init nprocs (fun _i -> Unix.pipe ()) in
  let from_demuxer, to_workers = A.split to_worker_pipes in
  let from_workers, to_muxer = A.split from_worker_pipes in
  (* cleaning before forking *)
  flush_all ();
  Gc.compact ();
  (* start muxer *)
  (match Unix.fork () with
   | 0 -> (* forked out *) (mux from_workers; exit 0)
   | _pid -> Printf.printf "forked muxer\n%!"
  );
  (* start workers *)
  for i = 0 to nprocs - 1 do
    flush_all ();
    match Unix.fork () with
    | 0 -> (* forked out *) (work i from_demuxer.(i) to_muxer.(i); exit 0)
    | _pid -> Printf.printf "forked worker %d\n%!" i
  done;
  (* start demuxer *)
  demux 1_000_000 0 to_workers;
  Printf.printf "finished\n"

let () = main ()
