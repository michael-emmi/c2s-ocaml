/**
 * begin [fifo-flipper-3.cp]
 * @expect verified
 * @bfs-depth 4
 * @unroll 50
 */


var b: bool

proc main () : void begin
	b := false;
	while * do
		if * then post p1 ()
		else if * then post p2 ()
		else post p3 ()
		fi fi ;
		
		if * then post q1 ()
		else if * then post q2 ()
		else post q3 ()
		fi fi 
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

proc p3 () : void begin
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

proc q3 () : void begin
	b := false;
	return
end
