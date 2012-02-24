// begin [producer-consumer.cp]

func size(first: int, last: int): int = (last - first) + 1

var buffer: [int] int
var window_size: int
var first: int
var last: int

proc main () : void begin
	first, last := 0, -1;
	
	post producer ();
	post consumer ();
	return
end

proc producer () : void begin
	if size(first,last) < window_size then
		last := last + 1;
		buffer[last] := last
	else
		skip
	fi;
	post producer();
	return
end

proc consumer () : void begin
	if size(first,last) > 0 then
		assert buffer[first] = first;
		first := first + 1
	else
		skip
	fi;
	post consumer();
	return
end

// end [producer-consumer.cp]