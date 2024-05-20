(* Simple Ocaml client for a chatroom *)

let connect_to_server host port =
  let inet_addr = Unix.inet_addr_of_string host in (* internet address from hostname supplied *)
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in (* gets socket address. combines inet_addr with port to fully specify the endpoint *)
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in (* creates the socket for IPv4 and TCP *)
  let%lwt () = Lwt_unix.connect socket sockaddr in (* promises connection with socket and socket address *)
  Lwt.return socket (* returns when the promise is fulfilled? *)

let rec receive_messages socket =
  let%lwt opt = Lwt_io.read_line_opt (Lwt_io.of_fd ~mode:Lwt_io.input socket) in (* promise to read line sent from server socket (changed to ic)*)
  match opt with (* matching output from the server *)
  | Some msg -> let%lwt () = Lwt_io.printf "%s\n" msg in receive_messages socket (* server sent back some bytes, promise an output to stdio, then recursively call this function to receive more messages. *)
  | None -> Lwt_io.printf "Connection closed by server\n" (* server sent back nothing, connection must be closed. *)

let send_messages socket =
  let rec loop () =
    let%lwt msg = Lwt_io.read_line Lwt_io.stdin in (* read the client input from stdin as a promise to msg var *)
    match msg with (* match the message sent *)
    | "/exit" ->
      let%lwt () = Lwt_io.printf "Exiting\n" in (* print to standard output that client exited as a promise *)
      Lwt.return_unit (* Stop the function without sending any further messages *)
    | _ ->
      let%lwt () = Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output socket) (msg) in (* otherwise, write the client input as a oc to the socket *)
      loop () (* Continue sending messages *)
  in
  Lwt.catch loop (fun _ -> Lwt.return_unit) (* Catch cancellation and return immediately *)

let main () =
  print_endline "Enter the hostname:"; (* ask client for hostname *)
  let host = read_line () in
  print_endline "Enter the port:"; (* ask client for port # *)
  let port = read_int () in
  print_endline "Enter nickname:"; (* ask client for nickname for identifiability. having exact same names might cause some bugs. TODO *)
  let nickname = read_line () in 
  print_endline "\nWelcome to the Chatroom!";
  print_endline "------------------------------------------";
  print_endline "";
  print_endline "Instructions:";
  print_endline "/exit to exit";
  print_endline "";
  let%lwt socket = connect_to_server host port in (* connect to server and return the file descriptor of the socket as a promise *)
  let%lwt () = Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output socket) (nickname) in (* send nickname to server so it can parse it and add to list *)
  let message_receiver = receive_messages socket in (* boot up the message receiver 'thread' *)
  let message_sender = send_messages socket in (* boot up the message sender 'thread' *)
  (* Wait for one of sender or receiver tasks to complete or cancel *)
  Lwt.pick [
    (let%lwt () = message_receiver in Lwt.return_unit); (* wait for message receiver thread to end or finish the promise *)
    (let%lwt () = message_sender in Lwt.return_unit); (* wait for message sender thread to end or finish the promise *)
    ]
  
let () =
  Lwt_main.run (main ()) (* run the main thread *)