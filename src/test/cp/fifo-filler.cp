// begin [ fifo-filler.cp ]
// @expect verified
// @unroll 10
// @bfs-depth 4

var n: int

proc main () : void begin
   n := 0;
   while * do
      post p ();
      post q ()
   done
end

proc p () : void begin

   assert 0 <= n and n <= 1;

   n := n + 1;
	post p ();
	post q ();
   return
end

proc q () : void begin

   assert 0 <= n and n <= 1;

   n := n - 1;
   return
end

// end [ fifo-filler.cp ]
