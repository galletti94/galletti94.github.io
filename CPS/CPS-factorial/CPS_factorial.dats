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
"fact__dynload"
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
kfact(n:int, k:cont1(int)): void

implement
kfact(n, k) =
if (n > 0) then kfact(n-1, lam (res) => k(res*n))
else k(1)

extern
fun
fact(n:int):int

implement
fact(n) = let
fun aux(n:int, res: int): int =
if (n > 0) then aux(n-1, res*n)
else res
in
aux(n, 1)
end


extern
fun
the_facts(): stream(int)


implement
the_facts() = let
fun the_facts_helper(n:int): stream(int) = $delay(
stream_cons(fact(n), the_facts_helper(n+1)))
in
the_facts_helper(1)
end

val the_facts_stream = the_facts()

extern
fun
stream_kforeach
( xs: stream(int)
, f0: cfun(int, cont1(bool), void), k0: cont0()): void


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
the_facts_start(): void = "mac#"

implement
the_facts_start() =
stream_kforeach(the_facts_stream, fopr, k0) where
{
val fopr =
lam (x: int, k:cont1(bool)):void
=<cloref1>
k( confirm("factorial= "+String(x)+". Continue?") )
//
val k0 = lam() =<cloref1> alert("Stopped")
}

%{$
//
function
fact__initize()
{
//
fact__dynload(); return;
//
} // end of [fact__initize]
%}

(* ****** ****** *)

%{$
//
jQuery(document).ready(function(){fact__initize();});
//
%} // end of [%{$]