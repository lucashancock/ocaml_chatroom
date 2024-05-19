(* let connect_to_server host port =
  let inet_addr = Unix.inet_addr_of_string host in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  Lwt.return socket
*)

let connect_to_server host port =
  let inet_addr = Unix.inet_addr_of_string host in (* internet address from hostname supplied *)
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in (* gets socket address. combines inet_addr with port to fully specify the endpoint *)
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in (* creates the socket for IPv4 and TCP *)
  let%lwt () = Lwt_unix.connect socket sockaddr in (* promises connection with socket and socket address *)
  Lwt.return socket (* returns when the promise is fulfilled? *)

(* let rec receive_messages socket =
  Lwt_io.read_line_opt (Lwt_io.of_fd ~mode:Lwt_io.input socket) >>= function
  | Some msg ->
    Lwt_io.printf "%s\n" msg >>= fun () ->
    receive_messages socket
  | None ->
    Lwt_io.printf "Connection closed by server\n"
*)

let rec receive_messages socket =
  let%lwt opt = Lwt_io.read_line_opt (Lwt_io.of_fd ~mode:Lwt_io.input socket) in (* promise to read line sent from server socket (changed to ic)*)
  match opt with (* matching output from the server *)
  | Some msg -> let%lwt () = Lwt_io.printf "%s\n" msg in receive_messages socket (* server sent back some bytes, promise an output to stdio, then recursively call this function to receive more messages. *)
  | None -> Lwt_io.printf "Connection closed by server\n" (* server sent back nothing, connection must be closed. *)


(* let rec send_messages socket =
  Lwt_io.read_line Lwt_io.stdin >>= fun msg ->
  Lwt_io.write (Lwt_io.of_fd ~mode:Lwt_io.output socket) (msg ^ "\n") >>= fun _ ->
  send_messages socket
*)

let rec send_messages socket =
  let%lwt msg = Lwt_io.read_line Lwt_io.stdin in (* read the message the client wants to send from stdin as a promise *)
  match msg with (* matching the message entered *)
  | "/exit" -> let%lwt () = Lwt_unix.close socket in
    Lwt_io.printf "Client exiting\n" (* if message is /exit then just say client exited and do nothing else *)
  | _ -> let%lwt () = Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output socket) (msg ^ "\n") in (* otherwise, write the line to the socket as an oc, add new line *)
  send_messages socket (* recursively call the function to handle sending more messages *)

let main () =
  print_endline "Enter the hostname:";
  let host = read_line () in
  print_endline "Enter the port:";
  let port = read_int () in
  let%lwt socket = connect_to_server host port in
  let message_receiver = receive_messages socket in
  let message_sender = send_messages socket in
  Lwt.join [message_receiver; message_sender]

let () =
  Lwt_main.run (main ())
  
  