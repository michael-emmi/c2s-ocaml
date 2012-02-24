// begin [ fact.fifo.cp ]
// @expect verified
// @unroll 10
// @bfs-depth 4

const n: int
var acc: int
var cnt: int

proc fact_seq (var i: int) : int begin
   var f: int
   if i > 1 then
      call f := fact_seq (i-1);
	  return f * i
   else
      	return 1
   fi
end

proc fact_reply (var i: int) : void begin
   var f: int
   yield;
   call f := fact_seq(n);
   assert cnt = n => f = i;
   return
end

proc fact_request (var i: int) : void begin

   // Note, here "acc := 1" should & will work before or after the post, since
   // the post is deferred.  Also, any other initial value of acc should &
   // will break the invariant.

   post fact_task (i);
   acc := 1;
   return
end

proc fact_task (var i: int) : void begin
   var a: int
   if i > 1 then
      post fact_task (i-1);
	  a := acc;
	  yield;
	  acc := a * i;
   else
      post fact_reply (acc)
   fi;
   cnt := cnt + 1;
   return
end

proc main () : void begin
   cnt := 0;
   post fact_request (n);
   acc := 1;
   return
end