program expressions2;

var
    x: integer;
    y: integer;

    a: integer;
    b: integer;

    c: integer;

    d: integer;
begin
    readln(x);
    readln(y);

    a := x + y;
    b := y - x;

    writeln(x);
    writeln(y);
    writeln(a);
    writeln(b);

    c := (x + a) * (y - b);

    writeln(c);

    d := a mod b;

    writeln(d);
end.