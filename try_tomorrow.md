(_ server.ml _)
open Lwt.Infix
open Lwt_unix

let port = 12345

let handle_client ic oc =
Lwt_io.read_lines ic
|> Lwt_stream.iter_s (fun line ->
Lwt_io.printlf "Received: %s" line >>= fun () ->
Lwt_io.write_line oc line)
|> Lwt.ignore_result

let () =
let addr = Unix.ADDR*INET (Unix.inet_addr_loopback, port) in
let server_sock = socket PF_INET SOCK_STREAM 0 in
bind server_sock addr;
listen server_sock 10;
Printf.printf "Server listening on port %d\n" port;
let rec accept_loop () =
accept server_sock >>= fun (client_sock, *) ->
let ic = Lwt_io.of_fd ~mode:Lwt_io.input client_sock in
let oc = Lwt_io.of_fd ~mode:Lwt_io.output client_sock in
Printf.printf "New connection\n";
Lwt.async (fun () -> handle_client ic oc);
accept_loop ()
in
Lwt_main.run (accept_loop ())

---

(_ client.ml _)
open Lwt.Infix
open Lwt_unix

let server_address = "127.0.0.1"
let port = 12345

let handle_input ic oc =
Lwt_io.read_lines ic
|> Lwt_stream.iter_s (fun line ->
Lwt_io.printlf "Received: %s" line)
|> Lwt.ignore_result

let () =
Lwt_main.run (
let%lwt server_sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
let server_addr = Unix.ADDR_INET (Unix.inet_addr_of_string server_address, port) in
Lwt_unix.connect server_sock server_addr >>= fun () ->
let ic = Lwt_io.of_fd ~mode:Lwt_io.input server_sock in
let oc = Lwt_io.of_fd ~mode:Lwt_io.output server_sock in
Printf.printf "Connected to server\n";
Lwt.async (fun () -> handle_input ic oc);
let rec send_loop () =
let%lwt line = Lwt_io.read_line_opt Lwt_io.stdin in
match line with
| Some line ->
Lwt_io.write_line oc line >>= fun () ->
send_loop ()
| None -> Lwt.return_unit
in
send_loop ()
)
