// begin [ quicksort.cp ]
// @notest

var me, parent: int
var nresults: int
var xs1, middle: [int] int
var len1, lenm: int

/* Spawn for multiprocessors. */

// var pids: [int] bool // special shared variable
// proc spawn () begin
//    var p: int
//    p := *;
//    assume !pids[p];
//    pids[p] := true;
//    return p
// end

/* Spawn for a single processor. */
proc spawn () : int begin
   return 0
end

proc main () : void begin
   var xs: [int] int
   var len: int
   
   len := 3;
   xs[0] := 3;
   xs[1] := 2;
   xs[2] := 1;
   
   call sort(xs,len,0,0);
   return
end


proc sort (var xs: [int] int, var len: int, 
           var parent_id, my_id: int) : void begin
   parent := parent_id;
   me := my_id;
   nresults := 0;
   call split(xs,len);
   return
end

proc result (var xs: [int] int, var len: int) : void begin

   // assert len > 1 => xs[0] <= xs[1];

   if nresults = 0 then
      xs1 := xs;
      len1 := len;
      nresults := nresults + 1;
      return
   else 

      // if we received the results in the opposite order,
      // then swap them so that the first is the left

      if len1 > 0 and xs1[0] > middle[0] then
         xs1, len1, xs, len := xs, len, xs1, len1
      else
         skip
      fi;

      call xs, len := merge( xs1, len1, xs, len );
      post result (xs,len) parent;
      return
   fi
end

proc split (var xs: [int] int, var len: int) : void begin
   var pivot: int
   var c1, c2: int
   var left, right: [int] int
   var lenl, lenr: int
   var i: int

   if len <= 1 then
      post result (xs,len) parent;
      return

   else
      pivot := xs[0];
      lenl, lenm, lenr := 0, 0, 0;
   
      i := 0;
      while (i < len) do
         if xs[i] < pivot then
            left[lenl] := xs[i];
            lenl := lenl + 1
         else if xs[i] = pivot then
            middle[lenm] := xs[i];
            lenm := lenm + 1
         else
            right[lenr] := xs[i];
            lenr := lenr + 1
         fi fi;
         i := i + 1
      done;
      
      call c1 := spawn ();
      call c2 := spawn ();
      post sort (left, lenl, me, c1) c1;
      post sort (right, lenr, me, c2) c2;
      return
   fi
end      

proc merge ( var left: [int] int, var lenl: int, 
             var right: [int] int, var lenr: int ) : ([int] int, int)
begin
   // construct `xs' as the concatenation of `left', `middle',
   // and `right'.

   var xs: [int] int
   var len: int
   var i: int
   
   len := lenl + lenm + lenr;

   i := 0;
   while i < lenl do
      xs[i] := left[i];
      i := i + 1
   done;
   
   while i < lenl + lenm do
      xs[i] := middle[i - lenl];
      i := i + 1
   done;
   
   while i < len do
      xs[i] := right[i - (lenl + lenm)];
      i := i + 1
   done;
   
   return xs, len
end

// end [ quicksort.cp ]