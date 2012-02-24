var b,c,d: bool

proc main () : void begin
   b,c,d := false, false, false;
   post h1 ();
   post h2 ();
   post h3 ();
   post h4 ();
   return
end

proc h1 () : void begin
   yield;
   assert !b;
   return
end

proc h2 () : void begin
   yield;
   if c then b := true else skip fi;
   return
end

proc h3 () : void begin
   yield;
   if d then c := true else skip fi;
   return
end

proc h4 () : void begin
   yield;
   d := true;
   return
end
