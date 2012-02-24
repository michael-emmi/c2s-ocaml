var n: int

proc main () : void begin
   n := 0;
   post h1 ();
   post h2 ()
end

proc h1 () : void begin
   assert n = 0;
   n := n + 1;
   post h3 ();
   post h4 ()
end

proc h2 () : void begin
   assert n = 1;
   n := n + 1;
   post h5 ();
   post h6 ()
end

proc h3 () : void begin
   assert n = 2;
   n := n + 1;
   post h7 ()
end

proc h4 () : void begin
   assert n = 3;
   n := n + 1;
   skip
end

proc h5 () : void begin
   assert n = 4;
   n := n + 1;
   skip
end

proc h6 () : void begin
   assert n = 5;
   n := n + 1;
   skip
end

proc h7 () : void begin
   assert n = 6
end