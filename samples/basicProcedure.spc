program proc;

procedure proc(n: integer);
var max, i: integer;
begin
    writeln(n);

    while ((n mod 2) = 0) do
    begin
        writeln(2);
        n := n div 2;
    end;

    while ((n mod 3) = 0) do
    begin
        writeln(3);
        n := n div 3;
    end;

    max := n;
    i := 5;
    while i <= max do
    begin
        while ((n mod i) = 0) do
        begin
            writeln(i);
            n := n div i;
        end;
        i := i + 2;
        while ((n mod i) = 0) do
        begin
            writeln(i);
            n := n div i;
        end;
        i := i + 4;
    end;
    if n <> 1 then writeln(n);
end;

begin
    proc(4);
    proc(5);
    proc(6);
    proc(7);
    proc(8);
    proc(9);
    proc(10);
    proc(11);
    proc(12);
    proc(13);
    proc(14);
    proc(15);
    proc(16);
    proc(17);
    proc(100);
    proc(131);
    proc(133);
end.
