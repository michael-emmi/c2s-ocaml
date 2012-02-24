/**
 * begin [leader-election.cp]
 * -- based on leader3.erl code by Thomas Arts
 */

type pid

// roles
const CANDIDATE: int
const CAPTURED: int
const SURRENDERED: int
const ELECTED: int

var role: int

proc main () : void 
begin
	
	return
end

proc init (const leader: int, const candidates: [int] int) : void
begin
	return
end

proc leader () : int
begin
	return
end

proc capture (const sender: pid) : void
begin
	if role = CANDIDATE then
		// ToDo: see who is the stronger candidate
		skip
		
	else if role = CAPTURED then
		// ToDo: do nothing?
		skip
		
	else if role = SURRENDERED then
		// ToDo: do nothing?
		skip
		
	else if role = ELECTED then
		// ToDo: tell him I'm elected
		skip
	else
		// ToDo: ignore unexpected messages
		skip
		
	fi fi fi fi
	return
end

proc accept (const sender: pid) : void
begin
	return
end

proc elect () : void
begin
	return
end

// end [leader-election.cp]