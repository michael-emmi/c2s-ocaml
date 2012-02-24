var b: bool
type pid

proc main () : void begin
   var x, y: pid
   new x;
   new y;

   b := false;
   post h1 (y) x;
   return
end

proc h1 (var other: pid) : void begin
   yield;
   if (b = false) then
      post h1 (other) self;
      post h2 () other
   else
      skip
   fi;
   return
end

proc h2 () : void begin
   	yield;
   	b := true;
   	return
end
