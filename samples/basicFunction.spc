program basicFunction;

var n : integer;

function add(a : integer; b : integer) : integer;
begin
	add := a+b
end;    

function foo(n : integer) : integer;
const fC = 5;
var fI : integer;
begin
	fI := n div 2;
	if n < fC then 
		writeln(fI)	
	else
		for I := fI to n do
			writeln(I);
	foo := fI;
end;    

begin
	readln(n);
	writeln(10 * add(n, 2));
	foo(n);
	writeln(inc(n));
end.
