/* First come identifier and constant declarations.. */

var b1, b2: bool
var b3: bool
var i, j, k: int
var t1, t2: (int, bool)
var m: [ int, int ] bool
var t3: (int, (int, [int] bool), bool)
const I, J, K: int

/* etc.  Valid types are "bool", "int", the tuple type "(t1,..,tn)",
 * where t1,..,tn are types, and the map type "[t1,..,tn] t", where
 * t1,..,tn and t are types. */

/* Then come procedure declarations.. */

/* Every program must have a procedure named `main'. */

proc main ( ) : void begin
   return
end

proc p1 ( var x, y: int, const c: bool, var z: int ) : void begin
   var d: [int] bool
   const c1, c2: int
   skip
end

proc p2 ( var x: int ) : ( int, int, bool ) begin
   return (1,3,false)
end

/* etc.  Each procedure declares a sequence of parameters, then a return
 * type, then a sequence of local variable declarations, and finally a
 * sequence of statements. */

/* Finally, the statements.. */

proc p () : (int,bool) begin

   /* Any statement can be labeled, by any number of labels.. */

   l1: l2: l3: skip;

   /* .. or not. */

   skip;

   /* Assertions and assumptions take a single expression. */
 
   assert true;
   assume true;

   /* (Parallel) assignments take an equal number of lvalues
    * and exprssions. */

   i, j, k := j, k, 0;
   m[i,j] := false;

   /* The usual boolean and integer expressions are avaialable, along
    * with the nondeterministic choice expression "*". */

   b1, b2 := (b1 & b2) | (b1 <=> b2) | (b1 => false) | ! b3, *;
   b3 := (i = j) | (i != j) | (i < j) | (i <= j) | (j > i) | (j >= i);
   k := (i + 5) * (10 - j % k) / -10;

   /* The boolean operators, and a few integer operators, can also be
    * referred to by their English names. */
   b1 := (b1 and b2) or (b1 iff b2) or (b1 implies false) or (not b3);
   k := (j mod k) + (j div k);

   /* Then there are tuple expressions, and map select and update. */
   t1, t2 := (10,true), (5,false);
   b3 := m[i,j];
   m := m[ i,j := false ];

   /* Goto statements take any number of targets -- their semantics 
    * nondeterministically chooses one of the targets. */

   l4: goto l2, l3, l4;

   /* Synchronous calls return values, while asynchornous posts return
    * immediately without values. */

   call p1 (i,j,b1,k);
   call i, j, b1 := p2 (k);
   call p2 (k);

   post p1 (i,j,b1,k);
   post p2 (k);

   /* The "yield" statement expresses a possible point of preemption. */
   yield;

   /* And the "dead" statement marks values stored in the subsequent
    * identifiers as no longer relevant. (This statement comes from
	* Boolean programs, and I'm not sure we'll keep it.) */   
   dead b1, b2, b3;

   /* Statements are nested inside "if" and "while" statements. */

   if x > 3 then
      skip; skip
   else
      skip
   fi;

   while *
   	  invariant true
	  invariant x > 3 or i > j
   do
      if * then
         skip; skip; skip
      else if i > j then
            skip
	     else
		    skip; skip
		 fi
      fi;

      skip
   done;
   
   /* And finally the return statement takes any expression -- the
    * type of expression should match the return type. */

   return (k,b3)
end
