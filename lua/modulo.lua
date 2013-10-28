
for i = -5,5,1 do
  for j = -5,5,1 do
    line = string.format("%2d mod %2d = ", i, j)
    if j == 0 then
      line = line .. "undefined"
    else
      line = line .. string.format("%d", i % j)
    end
    print(line)
  end
end
