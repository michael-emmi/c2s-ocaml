/**
 * begin [fifo-flipper-1.cp]
 * @expect verified
 * @bfs-depth 4
 * @unroll 50
 */


var b: bool

proc main () : void begin
	b := false;
	while * do
		post p1 ();
		post q1 ()
	done
end

proc p1 () : void begin
	assert !b;
	b := true;
	return
end

proc q1 () : void begin
	b := false;
	return
end
