require 'colorize'

class String
  def unindent()
    idx = self.index(/[^ ]/)
    self[idx..-1].gsub(/\n#{" " * idx}/,"\n")
  end
  def indent(n)
    self.gsub(/\n/,"\n" + (" " * n))
  end
  def reindent(n)
    self.unindent.indent(n)
  end
end

$uniqwrites = true

INIT_PROC = "TMInit"
BEGIN_PROC = "TXBegin"
READ_PROC = "TXRead"
WRITE_PROC = "TXWrite"
COMMIT_PROC = "TXCommit"
ABORT_PROC = "TXRollback"

def beginOp( )
  <<-xxx
  call #{BEGIN_PROC}(t);
  xxx
end

def readOp( cycle )
  <<-xxx
  havoc i, l;
  assume 0 < l && l < NUM_LOCS;
  assume i != CS.ignore ==> #{(1..cycle).to_a.map{|i| "t == T#{i}"}.join(" || ")};
  assume i != CS.ignore ==> CS.op(i) == CS.READ && CS.loc(i) == l;
  if (i != CS.ignore) { call CS.BeginOp(t,i); }
    call v := #{READ_PROC}(t,l);
    assume i != CS.ignore ==> CS.val(i) == v;
    if (i != CS.ignore) { call CS.EndOp(t,i); 
  }  
  xxx
end

def writeOp( cycle ) 
  <<-xxx
  havoc i, l, v;
  #{if $uniqwrites then "v := uniq_val; uniq_val := uniq_val + 1;" end}
  assume 0 < l && l < NUM_LOCS;
  assume i != CS.ignore ==> #{(1..cycle).to_a.map{|i| "t == T#{i}"}.join(" || ")};
#{if $deferred then
  "if (i != CS.ignore) {
    assume CS.thread(i) == t && CS.op(i) == CS.WRITE && CS.loc(i) == l && CS.val(i) == v;
    CS.deferred[i] := true;
    i := CS.ignore;
  }"
else
  "assume i != CS.ignore ==> CS.op(i) == CS.WRITE && CS.loc(i) == l && CS.val(i) == v;"
end}
  if (i != CS.ignore) { call CS.BeginOp(t,i); }
  call #{WRITE_PROC}(t,l,v);
  if (i != CS.ignore) { call CS.EndOp(t,i); }
  xxx
end

def commitOp( cycle )
  <<-xxx
  havoc i;
  assume i != CS.ignore ==> #{(1..cycle).to_a.map{|i| "t == T#{i}"}.join(" || ")};
#{if $deferred then
  "assume i != CS.ignore ==> CS.thread(i) == t && CS.op(i) == CS.WRITE;"
else
  "assume i == CS.ignore;"
end}
  if (i != CS.ignore) { call CS.BeginOp(t,i); }
  call b := #{COMMIT_PROC}(t);
  assume b;
  if (i != CS.ignore) { call CS.EndOp(t,i); }
  xxx
end

def abortOp( cycle )
  <<-xxx
  // ToDo: implement call to #{ABORT_PROC}
  xxx
end

def harnessXXX( options )
  cycle = options.cycle
  recbound = options.recursion
  deferred = options.update == :deferred
  printing = false

  size = 2*cycle
  idxs = (0...size).to_a
  evens = idxs.select{|i| i%2 == 0}
  odds = idxs.select{|i| i%2 == 1}
  
  more_thds = false
  more_txs = false
  more_ops = false
  recbound = recbound || 0
  
  $deferred = deferred  
  $shortcut_inlining = false
  
  puts "Generating #{cycle}-cycle detection harness with:".underline if options.verbose
  puts "* #{if more_thds then "from #{cycle} to #{cycle+recbound}" else "exactly #{cycle}" end} threads." if options.verbose
  puts "* #{if more_txs then "from 1 to #{1+recbound}" else "exactly 1" end} transaction(s) per thread." if options.verbose
  puts "* #{if more_ops then "any R-(R+W)^i-W-C (for i=0..#{recbound})" else "only R-W-C" end} transactions." if options.verbose
    
  def pred( i, n )
    return (if i == 0 then n-1 else i-1 end);
  end

  def succ( i, n )
    return (i+1) % n
  end
  
  <<-xxx
    
// The #{cycle}-length conflict cycle discovery encoding begins here.
type tid = int;
type loc = int;
type val = int;
type op;

#{if printing then
"type sep;
const s: sep;
procedure {:leavealone} boogie_si_record_tid(t: tid);
procedure {:leavealone} boogie_si_record_loc(x: loc);
procedure {:leavealone} boogie_si_record_val(v: val);
procedure {:leavealone} boogie_si_record_op(o: op);
procedure {:leavealone} boogie_si_record_bool(b: bool);
procedure {:leavealone} boogie_si_record_sep(s: sep);"
else
  "// boogie_si_record_XXX procedure declarations suppressed."
end}

const #{(1..cycle).to_a.map{|i| "T#{i}"}.join(", ")}: tid;
#{(1..cycle).to_a.map{|i| "axiom T#{i} == #{i-1};"}.join("\n")}

const NUM_LOCS: int;
axiom NUM_LOCS == 100;

#{if $uniqwrites then "var uniq_val: int;" end}

const unique CS.READ, CS.WRITE, CS.COMMIT, CS.ABORT: op;
    
/**
 * Our encoding of a conflict cycle a1->b1, a2->b2, .., aK->bK, between K
 * threads, each thread J executing operations bJ and a(J mod K + 1), uses
 * the following table, indexed beginning with 0.
 * 
 * conflicts / (a1->b1)  (a2->b2)  ..   (aK->bK)
 *           ____________________________________
 *           | a1 | b1 | a2 | b2 | .. | aK | bK |
 *           ------------------------------------
 * threads / t1--) (--t2---) (--t3-  -tK--) (--t1
 *
 * Thus, valid indices are 0..(K-1), even indices are "a" operations -- i.e. the
 * source of a conflict edge -- and odd indices are "b" operations -- i.e. the
 * target of a conflict edge.  We store the following for each index:
 *
 * started[..], finished[..] / whether the given operation has begun, or ended
 * action[..] / the type of operation, e.g. Read(x,1), Write(y,2)
 * thread[..] / the operation's thread identifier
 * sametx[..] / when two same-thread operations occur in the same transaction
 * concop[..] / when two conflicting operations execute concurrently
 *
 * Additionally, when both conflicting operations of a given thread t occur in
 * the same transaction, the t's "a" operation is allowed to precede its "b"
 * operation; the predicate "waiting[t]" indicates that the "a" operation has
 * been encountered without having yet seen the corresponding "b" operation.
 *
 */
 
// the conflict table
function CS.thread(int) returns (tid);
function CS.loc(int) returns (loc);
function CS.op(int) returns (op);
function CS.val(int) returns (val);
function CS.sametx(int) returns (bool);
function CS.sameop(int) returns (bool);
function CS.concop(int) returns (bool);
const CS.ignore: int;
axiom CS.ignore == #{size};

// auxiliary monitoring
var CS.started, CS.finished: [int] bool;
#{if $deferred then "var CS.deferred: [int] bool;" end}
var CS.waiting: [tid] bool;

function CS.pred(i: int) returns (int);
#{idxs.map{|i| "axiom CS.pred(#{i}) == #{pred(i,size)};"}.join("\n")}

function CS.cbuddy(i: int) returns (int);
function CS.tbuddy(i: int) returns (int);
#{evens.map{|i| 
"axiom CS.cbuddy(#{i}) == #{i+1} && CS.cbuddy(#{i+1}) == #{i};
axiom CS.tbuddy(#{i}) == #{pred(i,size)} && CS.tbuddy(#{pred(i,size)}) == #{i};"
}.join("\n")}

// conflicting operations actually do conflict
#{evens.map{|i| [
  "axiom CS.loc(#{i}) == CS.loc(#{i+1});",
  "axiom CS.op(#{i}) == CS.WRITE || CS.op(#{i+1}) == CS.WRITE;",
  "axiom CS.op(#{i}) == CS.READ || CS.op(#{i}) == CS.WRITE;",
  "axiom CS.op(#{i+1}) == CS.READ || CS.op(#{i+1}) == CS.WRITE;"
].join("\n") }.join("\n")}

// only bother with the explicitly named threads,
// which are adjancent in the conflict table
#{(1..cycle).to_a.map{|i|
  "axiom CS.thread(#{pred(2*(i-1),size)}) == T#{i} && CS.thread(#{2*(i-1)}) == T#{i};"
}.join("\n")}

// operations of the same thread agree on whether they are in the
// same transaction
#{evens.map{|i| 
  "axiom CS.sametx(#{i}) == CS.sametx(#{pred(i,size)});"
}.join("\n")}

// conflict points of the same operation agree on whether they are in the
// same operation
#{evens.map{|i| [
  "axiom CS.sameop(#{i}) == CS.sameop(#{pred(i,size)});",
  "axiom CS.sameop(#{i}) ==> CS.op(#{i}) == CS.op(#{pred(i,size)});",
  "axiom CS.sameop(#{i}) ==> CS.loc(#{i}) == CS.loc(#{pred(i,size)});",
  "axiom CS.sameop(#{i}) ==> CS.val(#{i}) == CS.val(#{pred(i,size)});"
].join("\n") }.join("\n")}

// conflicting operations agree on whether they are concurrent
#{evens.map{|i| 
  "axiom CS.concop(#{i}) == CS.concop(#{succ(i,size)});"
}.join("\n")}

procedure CS.BeginOp(t: tid, i: int)
{
  var j: int;    
      
  if (i != CS.ignore) {
    assume i >= 0 && i < #{size};
    assume CS.thread(i) == t;
    assume !CS.started[i];
    #{if $deferred then "assume CS.op(i) == CS.WRITE ==> CS.deferred[i];" end}

    j := CS.pred(i);        
    
    if ((#{evens.map{|i| "i==#{i}"}.join(" || ")}) && CS.sametx(i) && !CS.sameop(i) && !CS.finished[j]) {
      CS.waiting[t] := true;
    
    } else {
      assume (#{evens.map{|i| "i==#{i}"}.join(" || ")}) ==> CS.finished[j] || (CS.sameop(j) && (CS.finished[CS.cbuddy(j)] || CS.concop(CS.cbuddy(j))));
      assume (#{odds.map{|i| "i==#{i}"}.join(" || ")}) ==> CS.finished[j] || CS.concop(j);
      CS.waiting[t] := false;
    }        

    CS.started[i] := true;
    if (CS.sameop(i)) {
        CS.started[ CS.tbuddy(i) ] := true;
    }    
#{if printing then "
    call {:leavealone} boogie_si_record_int(i);
    call {:leavealone} boogie_si_record_tid(t);
    call {:leavealone} boogie_si_record_op(CS.op(i));
    call {:leavealone} boogie_si_record_loc(CS.loc(i));
    call {:leavealone} boogie_si_record_val(CS.val(i));"
end}      
  } 
  return;
}

procedure CS.EndOp(t: tid, i: int)
{    
  if (i != CS.ignore) {
    assume i >= 0 && i < #{size};
    assume CS.thread(i) == t;
    assume CS.started[i] && !CS.finished[i];
    assume CS.concop(i) ==> CS.started[ CS.cbuddy(i) ];
    assume CS.sameop(i) && CS.concop(CS.tbuddy(i)) ==> CS.started[CS.cbuddy(CS.tbuddy(i))];

    CS.finished[i] := true;
    if (CS.sameop(i)) {
        CS.finished[ CS.tbuddy(i) ] := true;
    }    
#{if printing then "
    call {:leavealone} boogie_si_record_int(i);
    call {:leavealone} boogie_si_record_tid(t);
    call {:leavealone} boogie_si_record_op(CS.op(i));
    call {:leavealone} boogie_si_record_loc(CS.loc(i));
    call {:leavealone} boogie_si_record_val(CS.val(i));
    
    if (CS.concop(i)) {
      call {:leavealone} boogie_si_record_sep(s);
      call {:leavealone} boogie_si_record_sep(s);
      call {:leavealone} boogie_si_record_int(i);
      call {:leavealone} boogie_si_record_bool(CS.started[i-1]);
      call {:leavealone} boogie_si_record_bool(CS.started[i+1]);
    }
    
    if (CS.sameop(i)) {
      call {:leavealone} boogie_si_record_sep(s);
      call {:leavealone} boogie_si_record_sep(s);
      call {:leavealone} boogie_si_record_sep(s);
      call {:leavealone} boogie_si_record_int( i );
      call {:leavealone} boogie_si_record_int( CS.tbuddy(i) );
    }
    
    if (CS.waiting[t]) {
        call {:leavealone} boogie_si_record_sep(s);
        call {:leavealone} boogie_si_record_bool(true);
    }"        
end}
    assume CS.started[i] ==> CS.finished[i];
  }
  return;
}

// Each transaction reads, writes, and commits...
// and might also read and write inbetween.
procedure CS.Transaction(t: tid)
{
  var i: int;
  var l: loc;
  var v: val;
  var b: bool;
    
  #{beginOp().reindent(2)}
    
  // An initial read operation.
  assume {:yield} true;
  #{readOp(cycle).reindent(2)}
    
  while (#{if more_ops then "*" else "false" end}) {
    if (*) {
      // Another read operation..            
      #{readOp(cycle).reindent(6)}
    } else {    
      // Another write operation..
      #{writeOp(cycle).reindent(6)}
    }
  }
    
  // A final write operation.
  assume {:yield} true;
  #{writeOp(cycle).reindent(2)}

  // The commit operation.
  assume {:yield} true;
  #{commitOp(cycle).reindent(2)}
    
  assume !CS.waiting[t];
  return;
}

// Each thread performs a sequence of transactions.
procedure CS.Thread(t: tid)
{
  // a first transaction...
  call CS.Transaction(t);
    
  while (#{if more_txs then "*" else "false" end}) {
    // ... and perhaps more transactions
    call CS.Transaction(t);
  }
  return;
}

procedure {:entrypoint} Main()
{
  var t: tid where #{(1..cycle).to_a.map{|i| "t != T#{i}"}.join(" && ")};
  
  // no operation has yet started nor finished
  #{idxs.map{|i| "assume !CS.started[#{i}];"}.join("\n  ")}
  #{idxs.map{|i| "assume !CS.finished[#{i}];"}.join("\n  ")}
  #{if $deferred then idxs.map{|i| "assume !CS.deferred[#{i}];"}.join("\n  ") end}

  // no operations are yet waiting for their matching operation
  assume #{(1..cycle).to_a.map{|i| "!CS.waiting[T#{i}]"}.join(" && ")};
  
  // Initialize the TM
  call #{INIT_PROC}();
  
  #{if $uniqwrites then "uniq_val := 1;" end}

  // Execute #{cycle} or more threads.
  #{(1..cycle).to_a.map{|i| "call {:async} CS.Thread(T#{i});"}.join("\n  ")}
  while (#{if more_thds then "*" else "false" end}) {
    havoc t;
    // Note: Corral seems to ignore the "where" clause above,
    // so we add this redundant assume.
    assume #{(1..cycle).to_a.map{|i| "t != T#{i}"}.join(" && ")};
    #{if printing then "call {:leavealone} boogie_si_record_tid(t);" end}
    call {:async} CS.Thread(t);
  }
  call {:async} CS.Check();
  return;
}

// check that all stages of the cycle have been encountered
procedure CS.Check()
{
  // allow this check to occur after the delays of other operations.
  assume {:yield} true;    
  
  // all operations has started and finished
  #{idxs.map{|i| "// assume CS.started[#{i}];"}.join("\n  ")}
  #{idxs.map{|i| "assume CS.finished[#{i}];"}.join("\n  ")}
  
  // no operations are still waiting
  // ToDo: I think this is redundant.
  // assume #{(1..cycle).to_a.map{|i| "!CS.waiting[T#{i}]"}.join(" && ")};    
#{if printing then "
  // print out lots of information
  call {:leavealone} boogie_si_record_tid(T1);
  call {:leavealone} boogie_si_record_tid(T2);
  call {:leavealone} boogie_si_record_bool(false);
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_tid(CS.thread(#{i}));"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);    
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_bool(CS.started[#{i}]);"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_bool(CS.finished[#{i}]);"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_bool(CS.sametx(#{i}));"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_bool(CS.concop(#{i}));"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);
  
  #{(1..cycle).to_a.map{|i|
    "call {:leavealone} boogie_si_record_bool(CS.waiting[T#{i}]);"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);
  
  call {:leavealone} boogie_si_record_op(CS.READ);
  call {:leavealone} boogie_si_record_op(CS.WRITE);
  call {:leavealone} boogie_si_record_op(CS.COMMIT);
  call {:leavealone} boogie_si_record_op(CS.ABORT);
  
  call {:leavealone} boogie_si_record_sep(s);
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_loc(CS.loc(#{i}));"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_op(CS.op(#{i}));"
  }.join("\n  ")}
  
  call {:leavealone} boogie_si_record_sep(s);    
  
  #{idxs.map{|i|
    "call {:leavealone} boogie_si_record_val(CS.val(#{i}));"
  }.join("\n  ")}"
end}
  // if this point is reachable, the we've found a cycle
  assert false;    
  return;
}
  xxx
end