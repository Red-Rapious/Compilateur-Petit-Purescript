type ident = string

type binop =
| Beq | Bneq 
| Blt | Ble | Bgt | Bge
| Badd | Bsub | Bmul | Bdiv
| Band | Bor | Bconcat 

type constant = 
| Cbool of bool
| Cstring of string
| Cint of int

type expr =
| Econst of constant
| Elident of ident
| Euident of ident

type program = {
  main : expr list
}