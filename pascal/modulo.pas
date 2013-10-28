program Hello;

var i: integer;
var j: integer;

begin
  for i := -5 to 5 do
    for j := -5 to 5 do
      begin
        if (i >= 0) then write(' ');
        write(i, ' mod ');
        if (j >= 0) then write(' ');
        write(j, ' = ');
        if (j = 0) then
          writeln('undefined')
        else
          writeln(i mod j)
      end
end.