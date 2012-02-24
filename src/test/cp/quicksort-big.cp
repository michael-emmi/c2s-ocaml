// begin [ quicksort-big.cp ]

type pid

func sorted_seg ( xs: [int] int, A: int, B: int ): bool
   = ( forall i:int, j:int ::
   	   		  i >= A & i < B & j >= A & j < i => xs[j] <= xs[i] )

func sorted ( xs: [int] int, len: int ): bool = (
	 forall i:int, j:int :: 0 <= i & i < j & j < len => xs[i] <= xs[j]
)

func smaller ( xs: [int] int, ax: int, bx: int,
	 		  ys: [int] int, ay: int, by: int ): bool = (
	 forall i:int, j:int :: ax <= i & i < bx & ay <= j & j < by
	 => xs[i] <= ys[j]
)

/*const null: pid*/
/*const r, a, b, c, d, e: pid*/

const CHUNK_SIZE: int

// phases
// const START: int
// const DOWN: int
// const UP: int

// local storage
// var phase: int
var pivot: int
var buffer: [int] int
var blen: int
var bidx: int
var last_from_left, last_from_right: bool
var leaf: bool

// invariant leaf <=> left = NULL & right = NULL
// invariant leaf => sorted(buffer,blen)

// process identifiers
var parent: pid
var left: pid
var right: pid

proc main () : void begin

	call init_node (3,null);
	blen := *;
	call send_buffer (self,true,true);
	return
end

/*proc {: leavealone :} boogie_si_record_bool (var v: bool) : void 
begin
end

proc {: leavealone :} boogie_si_record_pid (var v: pid) : void 
begin
end

proc {: leavealone :} boogie_si_record_int (var v: int) : void 
begin
end
*/

proc init_node (const level: int, const p: pid) : void 
begin
	if level > 0 then
		new left;
		new right;
		post init_node (level-1, self) left;
		post init_node (level-1, self) right
	else
		left := null;
		right := null;
	fi;

   blen := 0;
   bidx := 0;
   last_from_left := false;
   last_from_right := false;
   leaf := level = 0;
   parent := p;

   // call boogie_si_record_int(blen);
   return
end

// send the contents of the buffer (buffer,blen) to `recipient'..
// if `term' then send a terminating signal (_,-1) when finished.
proc send_buffer (var recipient: pid, var term: bool, var down: bool) : void
begin
   var xs: [int] int
   var i: int
/*
   call boogie_si_record_int(__pe__lvl);
   call boogie_si_record_pid(self);
   call boogie_si_record_int(blen);
*/
   // assert bidx >= 0;
   i := 0;

   while i < CHUNK_SIZE and bidx+i < blen
   do
      xs[i] := buffer[bidx+i];
      i := i + 1
   done;
   
   bidx := bidx + i;
   
   // assert leaf => sorted(xs,i);
   
   // send this chunk
   post chunk (xs, i, self, down) recipient;
   
   // did we reach the end?
   if bidx+i >= blen then
      if term then
         post chunk (xs,0,self,down) recipient
      else
         skip
      fi;

      bidx := 0
      
   // otherwise schedule the next chunk
   else
      post send_buffer (recipient, term, down) self
   fi;
   return
end


proc chunk (var xs: [int] int, var len: int, var sender: pid, const down: bool) : void
begin
/*   call boogie_si_record_int(__pe__lvl);
   call boogie_si_record_pid(self);
   call boogie_si_record_int(blen);*/

   // assert self = c => sorted(buffer,blen);
   // assert (self = a) => !leaf;
   // assert a != e;
   
   // assert (self = a => !leaf) and (self = b => leaf);
   
   if down then
      call chunk_down (xs,len,sender)
   else
      call chunk_up (xs,len,sender)
   fi;
	return
end

proc chunk_down (var xs: [int] int, var len: int, var sender: pid) : void
begin
   var ls, rs: [int] int
   var lenl, lenr: int
   
   // √ assert self = b => leaf;
   // √ assert self = a => !leaf;
   // √ assert self = c => leaf;
   // assert self = c => sorted(buffer,blen);
/*
   call boogie_si_record_int(__pe__lvl);
   call boogie_si_record_pid(self);
   call boogie_si_record_int(blen);
   call boogie_si_record_bool(__pe__err);*/

   if len <= 0 then
      if leaf then
         post send_buffer (parent, true, false) self;
         /* XX */ assume sorted(buffer,blen) /* XX */
      else
         post chunk_down (xs,0,self) left;
         post chunk_down (xs,0,self) right
      fi
   else
      if leaf then
         call merge_chunk_into_buffer (xs,len);
         /* XX */ assume sorted(buffer,blen) /* XX */
      else
         call ls, lenl, rs, lenr := split (xs,len);
         post chunk_down (ls,lenl,self) left;
         post chunk_down (rs,lenr,self) right
      fi
   fi;
	return
end

proc chunk_up (var xs: [int] int, var len: int, var sender: pid) : void
begin

   if len <= 0 then

      if sender = left then
         last_from_left := true;
         
         // start eating the buffer
         if blen > 0 then
            post send_buffer (parent, last_from_right, false) self
         else skip fi
          
      else 
         last_from_right := true;
         
         // if this is the last chunk, and the buffer is empty, 
         // send terminating signal to parent
         if last_from_left and blen <= 0 then
            post chunk_up (buffer, 0, self) parent
             
         // otherwise we have to remember to send it later..
         // ToDo: figure that out
         else
            skip
         fi
      fi

   else
   
      // assert sorted(xs,len);
      
      // directly forward chunks from the left child
      if sender = left then
         post chunk_up (xs,len,self) parent
          
      // buffer the right-child's chunks
      else if !last_from_left then
         call append_to_buffer (xs,len)
         // -- here (buffer,blen) is still sorted
          
      // already sending the buffer..
      else if blen > 0 then
         call append_to_buffer (xs,len)
         
      // just forward it otherwise
      else
         post chunk_up (xs,len,self) parent
            
      fi fi fi
   
   fi;
	return
end

// chunk message handler
/* proc chunk (var xs: [int] int, var len: int, var sender: pid) : void
   // requires leaf <=> left = NULL & right = NULL
   // requires leaf => sorted(buffer,blen)
   // ensures leaf <=> left = NULL & right = NULL
   // ensures leaf => sorted(buffer,blen)
begin

   var ls, rs: [int] int
   var lenl, lenr: int

   // choose the pivot in the first step
   if phase = START then
      pivot := xs[0];
      phase := DOWN;
      last_from_left, last_from_right := false, false
   else skip fi;
   
   if phase = DOWN then

      // last downstream chunk?
      if len < 0 then
      
         phase := UP;
      
         // send terminating signal to both children
         if left != NULL then
            post chunk (xs,-1,self) left;
            post chunk (xs,-1,self) right;

         // send buffer contents to parent.
         // NOTE: this is only OK with the one-shot merge at the leaves
         else
            post send_buffer (parent, true) self
            
         fi

      else // len >= 0
      
         // split the list and send to children
         if left != NULL then
            call ls, lenl, rs, lenr := split (xs,len);
            post chunk (ls, lenl, self) left;
            post chunk (rs, lenr, self) right

         // merge the list and sorted buffer
         else
            call merge_chunk_into_buffer (xs,len);
         fi
      fi

   else if phase = UP then
      if len < 0 then
         
         if sender = left then
            last_from_left := true;
            
            // start eating the buffer
            if blen > 0 then
               post send_buffer (parent, last_from_right) self
            else skip fi
            
         else 
            last_from_right := true;
            
            // if this is the last chunk, and the buffer is empty, 
            // send terminating signal to parent
            if last_from_left and blen <= 0 then
               post chunk (buffer, -1, self) parent
               
            // otherwise we have to remember to send it later..
            // ToDo: figure that out
            else
               skip
            fi
         fi
      
      else
         // assert sorted(xs,len);
      
         // directly forward chunks from the left child
         if sender = left then
            post chunk (xs,len,self) parent
            
         // buffer the right-child's chunks
         else if !last_from_left then
            call append_to_buffer (xs,len)
            // -- here (buffer,blen) is still sorted
            
         // already sending the buffer..
         else if blen > 0 then
            call append_to_buffer (xs,len)
            
         // just forward it otherwise
         else
            post chunk (xs,len,self) parent
            
         fi fi fi
      fi
   else skip
   fi fi;

   return
end */

// merge (xs,len) into the thus-far sorted buffer (buffer,blen)..
// split (xs,len) into two lists (ys,leny) and (zs,lenz)
// such that all elements of (ys,leny) are less than (pivot)
// and all elements of (zs,lenz) are greater or equal to (pivot)

proc // {:modular:} 
split (var xs: [int] int, var len: int)
      : (ys: [int] int, leny: int, zs: [int] int, lenz: int)

   /* requires len >= 0
   ensures smaller (ys, 0, leny, zs, 0, lenz)
   ensures (forall j:int :: 0 <= j & j < leny => ys[j] < pivot)
   ensures (forall j:int :: 0 <= j & j < lenz => zs[j] >= pivot)
   ensures leny + lenz = old(len) */
	  
begin
   // var ys, zs: [int] int
   // var leny, lenz: int
   var i: int

   leny, lenz, i := 0, 0, 0;

   while i < len
   /* invariant i = leny + lenz
   invariant i <= len
   invariant (forall j:int :: 0 <= j & j < leny => ys[j] < pivot)
   invariant (forall j:int :: 0 <= j & j < lenz => zs[j] >= pivot)
   invariant smaller (ys, 0, leny, zs, 0, lenz) */
   do
      if xs[i] < pivot then
         ys[leny] := xs[i];
         leny := leny + 1
      else
         zs[lenz] := xs[i];
         lenz := lenz + 1
      fi;
      i := i + 1
   done;

   return ys, leny, zs, lenz
end


// merge (xs,len) into the thus-far sorted buffer (buffer,blen)..
proc // {:modular:}
merge_chunk_into_buffer (var xs: [int] int, var len: int) : void
   /* requires len >= 0
   requires blen >= 0
   requires sorted(buffer,blen)
   ensures sorted(buffer,blen) */
   
begin
   call xs, len := sort_chunk (xs,len);
   call merge_sorted_chunk_into_buffer_one_shot (xs,len);
   return
end

// merge sorted list (xs,len) into thus-far sorted buffer (buffer,blen),
// all in one step.
proc // {:modular:}
merge_sorted_chunk_into_buffer_one_shot 
      (var xs: [int] int, var len: int) : void 

   /* requires len >= 0
   requires blen >= 0
   requires sorted(xs,len)
   requires sorted(buffer,blen)
   ensures sorted(buffer,blen) */
      
begin
   var new_buffer: [int] int
   var i, ix, iy: int
   
   i, ix, iy := 0, 0, 0;
   
   while i < len + blen
   /* invariant i = ix + iy
   invariant sorted (new_buffer,i)
   invariant sorted_seg (xs,ix,len)
   invariant sorted_seg (buffer,iy,blen)
   invariant ix < len => smaller (new_buffer, 0, i, xs, ix, len)
   invariant iy < blen => smaller (new_buffer, 0, i, buffer, iy, blen) */
   do
      if ix < len & ( iy < blen => (xs[ix] <= buffer[iy]) ) then
         new_buffer[i] := xs[ix];
         ix := ix + 1
      else
         new_buffer[i] := buffer[iy];
         iy := iy + 1
      fi;
      i := i + 1;
   done;

   buffer, blen := new_buffer, i;
   return
end

proc // {:modular:}
sort_chunk (var xs: [int] int, var len: int) 
         : (zs: [int] int, zlen: int) 

   /* ensures sorted (zs,zlen)
   ensures len = zlen */

begin

   var ys: [int] int
   
   // .. make sure (ys,len) is a sorted copy of (xs,len)
   assume sorted(ys,len); // XXX
   
   return ys, len
end

// append the list (xs,len) to the buffer (buffer,blen)
proc // {:modular:} 
append_to_buffer (var xs: [int] int, var len: int) : void
   /* requires len >= 0
   requires blen >= 0
   ensures blen = old(blen) + len */

   // requires sorted(buffer,blen)
   // requires sorted(xs,len)
   // ensures sorted(buffer,blen)

begin
   var i: int
   i := 0;
   while i < len
   do
      buffer[blen+i] := xs[i];
      i := i + 1
   done;
   blen := blen + len;
   return
end

// end [ quicksort-big.cp ]