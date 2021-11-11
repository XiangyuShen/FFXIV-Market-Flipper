# FFXIV-Market-Flipper

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)

## General info
Application to find the best goods to flip for profit in FFXIV.
	
## Technologies
Project is created with:
* Ocaml version: 4.12.0
* Dune version: 2.9
* Cohttp-async and Cohttp-lwt-unix libraries version: 4.0.0
* Yojson library version: 1.7.0
* Core library version: v0.14.1
	
## Setup
To run this project, clone or this repositiory locally, then run:

### Dune
```
$ cd ../FFXIV-Market-Flipper
$ dune build
$ dune exec ./testing.exe {SERVER-NAME}
```

### Ocaml Native  
```
$ cd ../FFXIV-Market-Flipper
$ ocamlbuild -use-ocamlfind -tag thread -pkg cohttp-lwt-unix testing.native
$ ./testing.native {SERVER-NAME}
```
Where {SERVER-NAME} is the name of the FFXIV server you want to sell on.
