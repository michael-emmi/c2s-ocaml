/**
 * begin [ping-pong.cp]
 * @expect verified
 * @phase-bound $unroll
 * @fifo-bound 1 5
 * @unroll 1 50
 */

var b: bool;
var c: bool;

procedure Main ()
{
	b := false;
	call {:async} Ping ();
	return;
}

procedure Ping () 
{
	assert !b;
	b := true;
	call {:async} Pong ();
    return;
}


procedure Pong () 
{
	b := false;
	call {:async} Ping ();
	return;
}
