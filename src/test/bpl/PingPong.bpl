
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
