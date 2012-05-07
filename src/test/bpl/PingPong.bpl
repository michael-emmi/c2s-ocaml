/**
 * [PingPong.bpl] 
 * 
 * Two asynchronous procedures posting eachother back and forth.
 */

var x: bool;

axiom {:dispatch} pending(Ping) <= 1;
axiom {:dispatch} pending(Pong) <= 1;
axiom {:dispatch} pending(Ping) > 0 ==> !x;
axiom {:dispatch} pending(Pong) > 0 ==> x;

procedure Ping ()
requires !x;
modifies x;
ensures x;
posts Pong;
ensures pending(Pong) <= old(pending(Pong)) + 1;
{
	assert !x;
    call {:async} Pong ();
    x := true;
    return;
}

procedure Pong ()
requires x;
modifies x;
ensures !x;
posts Ping;
ensures pending(Ping) <= old(pending(Ping)) + 1;
{
	assert x;
    call {:async} Ping ();
    x := false;
    return;
}

procedure Main ()
posts Ping;
{
    x := false;
    call {:async} Ping ();
    return;
}
