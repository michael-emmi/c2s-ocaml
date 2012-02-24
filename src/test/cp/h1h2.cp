var b: bool
type pid

proc main () : void begin
/*   var x: pid
   new x;*/

   b := false;
   post h1 ();
   return
end

proc h1 () : void begin
   yield;
   if (b = false) then
      post h1 ();
      post h2 ()
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
