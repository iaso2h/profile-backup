--[[
Source:https://github.com/hchunhui/librime-lua/issues/35
通过特定命令启动外部程序。

※将"- lua_processor@exe_processor" 放在 engine/processors 里，并位于默认 selector 之前
※rime.lua中 增加"exe_processor = require("exe")"


--]] local function generic_open(dest)
  if os.execute('start "" ' .. dest) then
    return true
  elseif os.execute('open ' .. dest) then
    return true
  elseif os.execute('xdg-open ' .. dest) then
    return true
  end
end

local function exe(key, env)
  local engine = env.engine
  local context = engine.context
  local kNoop = 2
  if (context.input == "/zdic" or context.input == "/hand") then
    generic_open("https://www.zdic.net")
    context:clear()
  elseif context.input == "/baidu" then
    generic_open("https://www.baidu.com")
  elseif context.input == "/tiger" then
    generic_open("https://tiger-code.com/search")
  elseif (context.input == "/ggl" or context.input == "/google") then
    generic_open("https://www.google.com")
    context:clear()
  elseif context.input == "/muyi" then
    generic_open("https://typer.owenyang.top")
    context:clear()
  elseif context.input == "/zitong" then
    generic_open("https://zi.tools")
    context:clear()
  elseif context.input == "/yedian" then
    generic_open("http://www.yedict.com")
    context:clear()
  elseif context.input == "/ds" then
    generic_open("http://www.deepseek.com")
    context:clear()
  elseif context.input == "/sz" then
    generic_open("http://suzhi.fun")
    context:clear()
  end
  return kNoop
end

return exe
-- return { func = exe }    --与"return exe"等效。
