
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
  let _nprocs = int_of_string Sys.argv.(1) in
  failwith "not implemented yet"

let () = main ()
