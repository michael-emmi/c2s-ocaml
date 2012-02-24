/**
 * begin [fifo-flipper-2.cp]
 * @expect verified
 * @bfs-depth 4
 * @unroll 50
 */


var b: bool

proc main () : void begin
	b := false;
	while * do
		if * then post p1 ()
		else post p2 ()
		fi;
		
		if * then post q1 ()
		else post q2 ()
		fi
	done
end

proc p1 () : void begin
	assert !b;
	b := true;
	return
end

proc p2 () : void begin
    b := true;
	return
end

proc q1 () : void begin
	b := false;
	return
end

proc q2 () : void begin
	b := false;
	return
end
