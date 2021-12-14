# FFXIV-Market-Flipper

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)
* [Usage](#usage)

## General info
Application to find the best goods to flip for profit on the FFXIV marketboard.
	
## Technologies
Project is created with and required to run:
* Ocaml: v4.12.0
* Dune: v2.9
* Cohttp-async: v4.0.0
* Cohttp-lwt-unix: v4.0.0
* Yojson: v1.7.0
* Core: v0.14.1
* Lwt_ssl v1.1.3 OR tls v0.15.1

For more information, see .opam file.
	
## Setup
To run this project, clone or download this repositiory locally, then run:

### Dune
```
$ cd ../FFXIV-Market-Flipper
$ dune build @install
$ dune exec -- ./src/flipper.exe init {SERVER-NAME}
$ dune exec -- ./src/flipper.exe update
```
Where {SERVER-NAME} is the name of the FFXIV server you want to sell on.

## Usage

### Single-item Lookup
```
$ dune exec -- ./src/flipper.exe {ITEM-NAME/ITEM-ID}
```

### Multi-Lookup
```
$ dune exec -- ./src/flipper.exe listings
$ dune exec -- ./src/flipper.exe listings --margin
$ dune exec -- ./src/flipper.exe listings --stacks
```
