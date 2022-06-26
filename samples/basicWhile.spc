program basicWhile;

var
    n: integer;
    i: integer;
begin
    	i := 1;
    	readln(n);
	
	while (i < n) do begin
		writeln(i);
		i := i + 1;
	end;

	writeln(n)
end.
