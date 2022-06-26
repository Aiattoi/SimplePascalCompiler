program basicArray;

const ARRMAX = 10;

var threeD : array[1..10] of array[1..10] of array[1..10] of integer;
var twoD : array[1..10] of array[1..10] of integer;
var oneD : array[1..10] of integer;

begin
	{ init 1D array}
	for I := 1 to ARRMAX do
		oneD[I] := I - 1;

	{ init 2D array }
	for I := 1 to ARRMAX do
		for J := 1 to ARRMAX do
			if I = J then
				twoD[I][J] := 1	
			else
				twoD[I][J] := 0;

	{ write 2D array }
	for I := 1 to ARRMAX do begin
		for J := 1 to ARRMAX - 1 do
			write(twoD[I][J]);
		writeln(twoD[I][ARRMAX]);
	end;

	
	twoD[1] := oneD;
	{ write changed 2D array }
	for I := 1 to ARRMAX do begin
		for J := 1 to ARRMAX - 1 do
			write(twoD[I][J]);
		writeln(twoD[I][ARRMAX]);
	end;

	{ init 3D array }
	for I := 1 to ARRMAX do
		for J := 1 to ARRMAX do
			for K := 1 to ARRMAX do
				if (I mod 2 = 0) then
					threeD[I][J][K] := 1
				else
					threeD[I][J][K] := 0;

	writeln('-----------------------');
	writeln('3D array:');
	for I := 1 to ARRMAX do begin
		write(I);
		writeln('th 2D array');
		for J := 1 to ARRMAX do begin
			for K := 1 to ARRMAX - 1 do
				write(threeD[I][J][K]);
			writeln(threeD[I][J][ARRMAX]);
		end;
		writeln('---');
	end;
end.
