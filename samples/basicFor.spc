program basicFor;

var n : integer;

begin
	readln(n);
	for I := 1 to 5 do
		writeln(n);

	for I := n to n*n do begin
		if I > n*n div 2 then
			writeln(I+1000)
		else
			writeln(I);
	end;

	for J := 10 downto 5 do
		writeln(J);
	writeln(n);
end.
