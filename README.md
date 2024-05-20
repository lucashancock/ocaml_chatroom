# OCaml Chat Server

This project is an implementation of a simple chat server in OCaml using the Lwt library for asynchronous programming.

## Features

- Allows multiple clients to connect simultaneously.
- Clients can send messages to the server, and the server broadcasts those messages to all connected clients.
- When a new client joins, a join message is sent to all currently connected clients.

## Video Demonstration

https://github.com/lucashancock/ocaml_chatroom/assets/111306378/0814a3fb-08ea-4905-b856-f0186c9bdb05

## Purpose

The purpose of this project was to deepen my understanding of Ocaml and functional programming languages after taking a course on it during Spring 2024. I have always wanted to implement my own simple chatroom, and doing it in a functional language seemed like a challenge!

## Prerequisites

- OCaml
- Lwt

## Installation/Usage

1. Clone the repository

2. Navigate to the project directory

3. Run `dune build` to build

4. Run `dune exec server` to spin up the server on port 9000

5. Run `dune exec client` to start up a client. You can start up more than one instance of the client. Just open a new terminal window, or use another machine on the same network!

6. To exit a client instance, type `\exit` and hit enter. You can also press Control-C.

7. To stop the server, press Control-C with the server terminal window in focus.
