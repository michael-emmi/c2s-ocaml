
var b: bool;
var c: bool;

procedure {:node p1} Main ()
{
	b := false;
	call {:async} {:node p1} Ping ();
	return;
}

procedure Ping () 
{
	assert !b;
	b := true;
	call {:async} {:node p2} Pong ();
    return;
}


procedure Pong () 
{
	b := false;
	call {:async} {:node p1} Ping ();
	return;
}
