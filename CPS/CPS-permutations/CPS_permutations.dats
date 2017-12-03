(* ****** ****** *)
(*
##myatsccdef=\
patsopt -d $1 | atscc2js -o $fname($1)_dats.js -i -
*)
(* ****** ****** *)
//
#define
ATS_MAINATSFLAG 1
//
#define
ATS_DYNLOADNAME
"permute__dynload"
(* ****** ****** *)
#define
LIBATSCC2JS_targetloc
"$PATSHOME/contrib\
/libatscc2js/ATS2-0.3.2" // latest stable release
//
#include
"{$LIBATSCC2JS}/staloadall.hats" // for prelude stuff
#staload
"{$LIBATSCC2JS}/SATS/print.sats" // for print into a store
//
(* ****** ****** *)
//
#staload
UN = "prelude/SATS/unsafe.sats"
//

typedef
cont0() = cfun(void)
typedef
cont1(res:t@ype) = cfun(res, void)

extern
fun
size_get(): int = "mac#"
//
%{^
function
size_get()
{
  return parseInt(document.getElementById("intmax").value);
}
%}

extern
fun
nextPerm(xs: list0(int)): list0(int)

extern
fun
swap(xs: list0(int), i: int, j:int): list0(int)

extern
fun
permutate(xs: list0(int), p1:int, p2:int): list0(int)

extern
fun
pivot(xs: list0(int)): int

extern
fun
take(xs:list0(int), i:int): list0(int)

implement
take(xs, i) = let
fun aux(xs:list0(int), i:int, res:list0(int)): list0(int) = let
val () = assertloc(i <= list0_length(xs))
in
case+ xs of
| list0_nil() => list0_nil
| list0_cons(x, xs) => if i = 0 then list0_reverse(res) else aux(xs, i-1, list0_cons(x, res))
end
in
aux(xs, i, list0_nil)
end


extern
fun
drop(xs:list0(int), i:int): list0(int)


implement
drop
(xs, i) = let
val () = assertloc(i <= list0_length(xs))
in
case+ xs of
| list0_nil() => list0_nil
| list0_cons(x, xs1) => if i = 0 then xs else drop(xs1, i-1)
end


extern
fun
get_at(xs: list0(int), i:int): int


implement
get_at
(xs, i) = let
val () = assertloc(i < list0_length(xs))
in
case- xs of
| list0_cons(x, xs) => if i = 0 then x else get_at(xs, i-1)
end


implement
swap(xs, i, j) =
list0_append
(
list0_append
(
take(xs, i)
,
list0_cons( get_at(xs, j), take( drop(xs, i+1), j- i -1 ))
)
,
list0_cons( get_at(xs, i), drop(xs, j+1) )
)


implement
permutate
(xs, p1, p2) =
if (p1 < p2 andalso get_at(xs, p1 -1) > get_at(xs, p2)) then permutate(xs, p1, p2 - 1)
else
let val sxs = swap(xs, p1 - 1, p2)
in
list0_append(take(sxs, p1), list0_reverse(drop(sxs, p1)))
end


implement
pivot(xs) = let
val n = list0_length(xs) -1
in
let
fun aux(xs: list0(int), i:int): int =
if n = i then i
else
(
if get_at(xs, n-i) > get_at(xs, n-i-1) then n-i
else aux(xs, i+1)
)
in
aux(xs, 0)
end
end


implement
nextPerm(xs) = permutate(xs, pivot(xs), list0_length(xs) -1)


extern
fun
make_list0
(size:int): list0(int)


implement
make_list0
(size) =
if size = 0 then list0_nil
else list0_cons(size, make_list0(size-1))


extern
fun
thePerms(): stream(list0(int))


implement
thePerms() = let
fun aux(input_list:list0(int)): stream(list0(int)) =
$delay(
stream_cons(input_list, aux(nextPerm(input_list)))
)
in
let
val N = size_get()
val input_list = list0_reverse(make_list0(N))
in
aux(input_list)
end
end


extern
fun
stream_kforeach
( xs: stream(list0(int))
, f0: cfun(list0(int), cont1(bool), void), k0: cont0()): void


implement
stream_kforeach(xs, f0, k0) =
(
case+ !xs of
| stream_nil() => k0()
| stream_cons(x, xs) =>
  f0(x, lam(y) => if y then stream_kforeach(xs, f0, k0) else k0())
)


extern
fun
is_reversed(xs:list0(int)):bool


implement
is_reversed(xs) =
case+ xs of
| list0_nil() => true
| list0_cons(x1, xs1) => (case+ xs1 of
  		      	 | list0_nil() => true
			 | list0_cons(x2, xs2) => if x1 < x2 then false else is_reversed(xs1)
			 )


extern
fun
the_perms_start(): void = "mac#"


implement
the_perms_start() =
stream_kforeach(the_perms_stream, fopr, k0) where
{
val the_perms_stream = thePerms()
val fopr =
lam (xs: list0(int), k:cont1(bool)):void
=<cloref1>
k( if is_reversed(xs) then confirm("DONE !") else confirm("Next Permutation= [" + String(xs) +"]. Continue?") )
//
val k0 = lam() =<cloref1> alert("Stopped")
}


%{$
//
function
permute__initize()
{
//
permute__dynload(); return;
//
} // end of [permute__initize]
%}


(* ****** ****** *)


%{$
//
jQuery(document).ready(function(){permute__initize();});
//
%} // end of [%{$]