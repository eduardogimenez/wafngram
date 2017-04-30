let trace str =
  begin
    Printf.fprintf stdout "%s\n" str;
    flush stdout;
  end
