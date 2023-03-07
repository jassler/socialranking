Div = function(div)
  if string.sub(FORMAT, 1, 4) ~= "html" and div.classes:includes("hidden") then
      print(div.content[1].content[1].text)
      return {}
  end
end
