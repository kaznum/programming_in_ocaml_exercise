let capitalize ch =
  if 'a' <= ch && ch <= 'z' then char_of_int(int_of_char(ch) - 32) else ch;;
