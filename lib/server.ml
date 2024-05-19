open Lwt

let handle_client (client_socket, client_address) =
  let client_input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let client_output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  let rec broadcast_message () =
    Lwt.catch (fun () ->
      Lwt_io.read_line_opt client_input >>= function
      | Some msg ->
        let formatted_msg = (Unix.string_of_inet_addr client_address) ^ ": " ^ msg in
        Lwt_io.printf "Received message from client: %s\n" formatted_msg >>= fun () ->
          Lwt_io.write client_output (formatted_msg ^ "\n") >>= fun () ->
            broadcast_message ()
      | None ->
        Lwt_io.printf "Client disconnected: %s\n" (Unix.string_of_inet_addr client_address)
    ) (function
        | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
          Lwt_io.printf "Client disconnected abruptly: %s\n" (Unix.string_of_inet_addr client_address)
        | ex -> Lwt.fail ex
    )
  in
  broadcast_message ()

let create_server port =
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind socket addr >>= fun () ->
  Lwt_unix.listen socket 10;
  Lwt_io.printf "Server listening on port %d\n" port >>= fun () ->
  let rec accept_loop () =
    Lwt_unix.accept socket >>= fun (client_socket, client_address) ->
    match client_address with
    | Unix.ADDR_INET (inet_addr, _) ->
      let client_ip = Unix.string_of_inet_addr inet_addr in
      Lwt_io.printf "Client connected from %s\n" client_ip >>= fun () ->
      Lwt.async (fun () -> handle_client (client_socket, inet_addr));
      accept_loop ()
    | _ ->
      Lwt_io.printf "Unknown client address\n" >>= fun () ->
      accept_loop ()
  in
  accept_loop ()
  

