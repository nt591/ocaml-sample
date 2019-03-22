let rec power x y =
  if y < 0 then 0
    else if y = 0 then 1
      else x * power x (y - 1)