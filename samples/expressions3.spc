program expressions3;

var a, b, c, d : integer;
var e, f : integer;

const n_val = 5;

begin
	{ test all operations }
	readln(a);
	readln(b);

	writeln(a = b);
	writeln(a <> b);
	writeln(a < b);
	writeln(a <= b);
	writeln(a > b);
	writeln(a >= b);
	writeln(a + b);
	writeln(a - b);
	writeln(a or b);
	writeln(a | b);
	writeln(a * b);
	{writeln(a / b);}
	writeln(a div b);
	writeln(a mod b);
	writeln(a and b);
	writeln(a & b);
	writeln(~a);
	writeln(not a);
	{ test priority }
	writeln(n_val + a * b);
	
	readln ( c );

	d := c < n_val * n_val;
	writeln ( d );

	{ random tests }
	readln(e);
	readln(f);

	writeln(2*(e+f));
	writeln(e*f);
	writeln(e*f*n_val);

	write(n_val)
end.
