open Lwt

let connected_clients : (string * Lwt_unix.file_descr * Lwt_io.input_channel * Lwt_io.output_channel * Unix.inet_addr) list ref = ref [] (* create a mutable list to handle all of the currently connected clients *)

let handle_client (client_socket, client_address) =
  let client_input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let client_output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  let%lwt nickname = Lwt_io.read_line (Lwt_io.of_fd ~mode:Lwt_io.input client_socket) in
  (* Add client to the list of connected clients *)
  connected_clients := (nickname, client_socket, client_input, client_output, client_address) :: !connected_clients;
  
  let joinmsg = "\t\t\t** " ^ nickname ^ " joined the chat **\n" in
  let send_join_to_client (_, _, _, output, _) =
    Lwt_io.write_line output joinmsg
  in
  let%lwt () = Lwt_list.iter_s send_join_to_client !connected_clients in

  let rec broadcast_message () =
    Lwt.catch (fun () ->
      Lwt_io.read_line_opt client_input >>= function
      | Some msg ->
        let formatted_msg = "\t\t\t[ " ^ nickname ^ " ]" ^ ": " ^ msg in
        let%lwt () = Lwt_io.printf "%s: %s\n" (Unix.string_of_inet_addr client_address) msg in
        (* Broadcast message to all other clients in the ref list *)
        let broadcast_to_clients (other_nick, _, _, other_output, _) =
          if not (nickname = other_nick) then
            Lwt_io.write other_output (formatted_msg ^ "\n")
          else 
            Lwt.return_unit
        in
        let%lwt () = Lwt_list.iter_s broadcast_to_clients !connected_clients in
        broadcast_message ()
      | None ->
        connected_clients :=
          List.filter
            (fun (other_nick, _, _, _, _) -> not(nickname = other_nick)) !connected_clients;
            let exitmsg = "\t\t\t** " ^ nickname ^ " left the chat **\n" in
            let send_exit_to_client (_, _, _, output, _) =
              Lwt_io.write_line output exitmsg
            in
            let%lwt () = Lwt_list.iter_s send_exit_to_client !connected_clients in
        Lwt_io.printf "%s disconnected: %s\n" nickname (Unix.string_of_inet_addr client_address);
    ) (function
        | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
          Lwt_io.printf "Client disconnected abruptly: %s\n" (Unix.string_of_inet_addr client_address)
        | ex -> Lwt.fail ex
    ) in
broadcast_message ()
  

let create_server port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, port) in (* gets socket address for any inet and the specified port *)
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in (* creates the socket for IPv4 and TCP connections *)
  let%lwt () = Lwt_unix.bind socket sockaddr in (* binds the socket and socket address *)
  Lwt_unix.listen socket 10; (* listens on that socket with a max queue of 10 *)
  let%lwt () = Lwt_io.printf "Server listening on port %d\n" port in
  let rec accept_loop () = (* the accept loop for new clients *)
    let%lwt (client_socket, client_address) = Lwt_unix.accept socket in (* promise to get the client socket and address by using accept *)
    match client_address with
    | Unix.ADDR_INET (inet_addr, _) -> (* make sure its an inet address with port we get from accept *)
      let client_ip = Unix.string_of_inet_addr inet_addr in
        let%lwt () = Lwt_io.printf "Client connected from %s\n" client_ip in (* print diagnostic message about client connection *)
        Lwt.async (fun () -> handle_client (client_socket, inet_addr)); (* not too sure why this is needed for multiple client connections instead of let%lwt *)
        accept_loop () (* recursive call to accept more clients *)
    | _ ->
      Lwt_io.printf "Unknown client address\n" >>= fun () -> (* error handling with unknown client address *)
      accept_loop ()
  in
  accept_loop () (* entry point into accept loop! *)