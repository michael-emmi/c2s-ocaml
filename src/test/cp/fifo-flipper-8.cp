/**
 * begin [fifo-flipper-8.cp]
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
		else if * then post p3 ()
		else if * then post p4 ()
		else if * then post p5 ()
		else if * then post p6 ()
		else if * then post p7 ()
		else post p8 ()
		fi fi fi fi fi fi fi;
		
		if * then post q1 ()
		else if * then post q2 ()
		else if * then post q3 ()
		else if * then post q4 ()
		else if * then post q5 ()
		else if * then post q6 ()
		else if * then post q7 ()
		else post q8 ()
		fi fi fi fi fi fi fi
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

proc p4 () : void begin
	b := true;
	return
end

proc p5 () : void begin
	b := true;
	return
end

proc p6 () : void begin
    b := true;
	return
end

proc p7 () : void begin
	b := true;
	return
end

proc p8 () : void begin
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

proc q4 () : void begin
	b := false;
	return
end

proc q5 () : void begin
	b := false;
	return
end

proc q6 () : void begin
	b := false;
	return
end

proc q7 () : void begin
	b := false;
	return
end

proc q8 () : void begin
	b := false;
	return
end
