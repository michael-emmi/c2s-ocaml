/**
 * begin [ping-pong.cp]
 * @expect verified
 * @phase-bound $unroll
 * @fifo-bound 1 5
 * @unroll 1 50
 */

var b: bool

proc main () : void begin
	b := false;
	post p ();
	return
end

proc p () : void begin
	assert !b;
	b := true;
	post q ();
	return
end

proc q () : void begin
	b := false;
	post p ();
	return
end