<?php 

$input = $_POST['data'];
$flags = $_POST['options'];
$boogie = "Boogie";
$ifile = "boogie-input.bpl";
$ofile = "boogie-output.log";

if($input == "")
	echo "no input given";

else {
	$f = fopen($ifile, 'w') or die("can't open temp file for writing");
	fwrite($f, $input);
	fclose($f);
	$statement = $boogie . " " . $flags . " " . $ifile . " 2>&1";

	$start = microtime(true);
	system($statement);
	$end = microtime(true);
	echo "Time: " . ($end - $start) . "\n";
}

?>