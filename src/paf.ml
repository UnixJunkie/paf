
module L = BatList
module A = BatArray

(* fbr: - fork with pipes writing in sequence
        - fork with pipes but poll
        - threads with Domainslib.Chan
        - also vary csize
*)

let to_chan chan x =
  Marshal.(to_channel chan x [No_sharing, Closures])

let from_chan chan =
  Marshal.from_channel chan

(* send integers *)
let demux max_count csize out_pipes () =
  let output_channels = A.map Unix.out_channel_of_descr out_pipes in
  let pipe_index = ref 0 in
  let n_pipes = A.length output_channels in
  for i = 0 to max_count - 1 do
    let pipe = output_channels.(!pipe_index) in
    to_chan pipe i;
    pipe_index := (!pipe_index + 1) mod n_pipes
  done;
  A.iter Unix.close out_pipes

let main () =
  let nprocs = int_of_string Sys.argv.(1) in
  failwith "not implemented yet"

let () = main ()
