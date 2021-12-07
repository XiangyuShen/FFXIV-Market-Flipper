# FFXIV-Market-Flipper

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)

## General info
Application to find the best goods to flip for profit on the FFXIV marketboard.
	
## Technologies
Project is created with:
* Ocaml version: 4.12.0
* Dune version: 2.9
* Cohttp-async and Cohttp-lwt-unix libraries version: 4.0.0
* Yojson library version: 1.7.0
* Core library version: v0.14.1
* Lwt_ssl library version 1.1.3 OR tls library version 0.15.1
	
## Setup
To run this project, clone or download this repositiory locally, then run:

### Dune
```
$ cd ../FFXIV-Market-Flipper
$ dune build
$ dune exec ./flipper.exe init {SERVER-NAME}
$ dune exec ./flipper.exe update
```
Where {SERVER-NAME} is the name of the FFXIV server you want to sell on.

## Usage

### Single-item Lookup
```
$ dune exec ./flipper.exe {ITEM-NAME/ITEM-ID}
```

### Multi-Lookup
```
$ dune exec ./flipper.exe listings
$ dune exec ./flipper.exe listings --margin
$ dune exec ./flipper.exe listings --stacks
```
