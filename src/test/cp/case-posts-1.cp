/* begin [ case-posts-1.cp ]
 * @expect verified
 * @unroll 50
 * @bfs-depth 4 
 */

var x: int

proc main () : void 
begin
	var y: int

	x, y := 0, 0;

	while * do
		assume y < 100;
		if * then post p1 ()
		else skip
		fi;
		
		y := y + 1
	done;
end

proc p1 () : void
begin
	assert x < 100;
	x := x + 1;
	return
end