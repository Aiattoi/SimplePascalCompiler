program basicIf;

var i, j : integer;

begin
	readln(i);
	if i < 0 then
		writeln(-1)
	else if i > 0 then 
		writeln(1)
	else 
		writeln(0);
	writeln(i);

	if i < 0 then begin
		writeln(-1);
		j := 1;
	end
	else if i > 0 then begin
		writeln(1);
		j := 2;
	end
	else begin
		writeln(0);
		j := 3;
	end;
	writeln(i);
	write(j);
end.
