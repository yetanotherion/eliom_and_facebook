(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
type eq_op = [ `Eq | `Neq ]
type diff_op = [ `Gte | `Lte | `Gt | `Lt ]
type op = [`Eqop of eq_op | `Diffop of diff_op ]

type single_expr = [
  `Attending of (op * int)
| `Declined of (op * int)
| `Invited of (op * int)
| `Location of (eq_op * string)
| `Owner of (eq_op * string)
| `Name of (eq_op * string)
]

type expr = [
| `Or of (expr * expr)
| `And of (expr * expr)
| `Single of single_expr
]

let eq_op_to_string op =
  match op with
    | `Eq -> "="
    | `Neq -> "!="

let diff_op_to_string diffop =
  match diffop with
    | `Gte -> ">="
    | `Lte -> "<="
    | `Gt -> ">"
    | `Lt -> "<"

let op_to_string op =
  match op with
    | `Eqop eqop -> eq_op_to_string eqop
    | `Diffop diffop -> diff_op_to_string diffop

let single_expr_to_string s_expr =
  let open Printf in
  match s_expr with
    | `Attending (o, i) -> sprintf "attending %s %d" (op_to_string o) i
    | `Declined (o, i) -> sprintf "declined %s %d" (op_to_string o) i
    | `Invited (o, i) -> sprintf "invited %s %d" (op_to_string o) i
    | `Location (o, l) -> sprintf "location %s %s" (eq_op_to_string o) l
    | `Owner (o, own) -> sprintf "owner %s %s" (eq_op_to_string o) own
    | `Name (o, n) -> sprintf "name %s %s" (eq_op_to_string o) n


let rec expr_to_string expr =
  let open Printf in
  match expr with
    | `Or (a, b) -> sprintf "OR (%s, %s)" (expr_to_string a) (expr_to_string b)
    | `And (a, b) -> sprintf "AND (%s, %s)" (expr_to_string a) (expr_to_string b)
    | `Single a -> single_expr_to_string a
