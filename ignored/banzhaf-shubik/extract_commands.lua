WriteCommands = function(text, exclude)
  local file = io.open("commands.tex", "w")
  for line in text:gmatch("[^\r\n]+") do
    local append = true
    for i = 1, #exclude do
      if line:match("\\newcommand{\\" .. exclude[i]) then
        append = false
        break
      end
    end
    if append then
      file:write(line .. "\n")
    end
  end
  
  file:close(file)
end
Div = function(div)
  if string.sub(FORMAT, 1, 4) ~= "html" and div.classes:includes("hidden") then
      WriteCommands(
        div.content[1].content[1].text,
        {"bm"}
      )
      return { }
  end
end
