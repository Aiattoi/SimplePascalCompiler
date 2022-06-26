program basicExit;

var n : integer;

procedure printNtimes(n : integer);
var I : integer;
begin
	I := 1;
	while 1 do begin
		if I = n then begin
			writeln(n);
			exit;
		end;
		write(n);
		inc(I);	
	end;
end;

begin
	readln(n);
	printNtimes(n);
	if n = 5 then
		exit;
	writeln(5);
end.
