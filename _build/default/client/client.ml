open Lwt.Infix

let connect_to_server host port =
  let inet_addr = Unix.inet_addr_of_string host in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  Lwt.return socket

let rec receive_messages socket =
  Lwt_io.read_line_opt (Lwt_io.of_fd ~mode:Lwt_io.input socket) >>= function
  | Some msg ->
    Lwt_io.printf "%s\n" msg >>= fun () ->
    receive_messages socket
  | None ->
    Lwt_io.printf "Connection closed by server\n"

let rec send_messages socket =
  Lwt_io.read_line Lwt_io.stdin >>= fun msg ->
  Lwt_io.write (Lwt_io.of_fd ~mode:Lwt_io.output socket) (msg ^ "\n") >>= fun _ ->
  send_messages socket

let main () =
  print_endline "Enter the hostname:";
  let host = read_line () in
  print_endline "Enter the port:";
  let port = read_int () in
  connect_to_server host port >>= fun socket ->
  let message_receiver = receive_messages socket in
  let message_sender = send_messages socket in
  Lwt.join [message_receiver; message_sender]

let () =
  Lwt_main.run (main ())
  
  