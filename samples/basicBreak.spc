program basicBreak;

var i, n : integer;

begin
	i := 1;
	readln(n);
	
	while(1) do begin
		writeln(i);
		i := i + 1;
		if i = n then
			break;
	end;
	writeln(n);
end.
