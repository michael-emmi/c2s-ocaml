// begin [ basic-test-fail.cp ]
// @expect error
// @unroll 10
// @bfs-depth 4

var x: int

proc main () : void
begin
	post p ()
end

proc p () : void
begin
	assert false
end
