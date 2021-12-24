
local shared = require('shared')

local whitelistexists = false

local skippedCodeBlockText = "SKIPPED"

function fileExists(name)
    local f=io.open(name,"r")
    if f ~= nil then io.close(f) return true end
    return false
end

local function readGHCiOutput(block)
    local interpreter = shared.codeBlockInterpreter(block)
    local blockhash = shared.codeBlockSha1(block)
    local fileptr = io.input(string.format("%s-output-%s.txt", interpreter, blockhash))
    local contents = io.read("*all")
    io.close(fileptr)
    return contents
end

local function stripEscapeSequences(text)
    text = string.gsub(text, "[\27]%[%?1h", "")
    text = string.gsub(text, "[\27]%[%?1l", "")
    text = string.gsub(text, "[\27]%[;1m", "")
    text = string.gsub(text, "[\27]%[31m", "")
    text = string.gsub(text, "[\27]%[0m", "")
    text = string.gsub(text, "[\27]>", "")
    text = string.gsub(text, "[\27]=", "")
    return text
end

local function stripBackspaces(text)
    -- strip up to two backspaces in a row, consuming the preceding characters
    text = string.gsub(text, "..[\8][\8]", "")
    text = string.gsub(text, ".[\8]", "")
    return text
end

local function convertCarriageReturnsToNewlines(text)
    return string.gsub(text, "\r+\n", "\n")
end

local function getTrimStart(text)
    -- find an explicit cut start
    local _, trimStart = string.find(text, '> --cut>>\n', nil, true)
    if trimStart ~= nil then return trimStart+1 end

    -- cut after intro if we're running in a tidal repl
    _, trimStart = string.find(text, 'Loaded GHCi configuration from BootTidal.hs\n', nil, true)
    if trimStart ~= nil then return trimStart+1 end

    -- try starting with the GHCi starting verbiage next
    trimStart, _ = string.find(text, 'GHCi, version 8', nil, true)
    if trimStart ~= nil then return trimStart end

    -- if that's not present, fall back to outputting all the characters
    return 0
end

local function getTrimEnd(text)
    local trimEnd, _ = string.find(text, '\nLeaving GHCi.', nil, true)
    -- Walk backwards and find the beginning of the previous line
    -- It's always a blank line like "Prelude Sound.Tidal.Context> "
    trimEnd = trimEnd - 1
    while string.sub(text,trimEnd,trimEnd) ~= "\n" do
        trimEnd = trimEnd - 1
    end
    return trimEnd
end

local function trimExample(text)
    return string.sub(text,getTrimStart(text),getTrimEnd(text))
end

local function scrubText(text)
    return string.gsub(text,"/Users/%a+/","/.../")
end

function CodeBlock(block)

    if whitelistexists and not shared.codeBlockClassesContain(block, "whitelist") then
        return pandoc.CodeBlock(skippedCodeBlockText)
    end

    local thetext = block.text

    if shared.codeBlockClassesContain(block, "ghcisession") or shared.codeBlockClassesContain(block, "tidalsession") then
        local ghcioutput = readGHCiOutput(block)
        thetext = trimExample(convertCarriageReturnsToNewlines(stripBackspaces(stripEscapeSequences(ghcioutput))))
    end

    thetext = scrubText(thetext)

    local returnvalues = {}

    if string.len(thetext) > 0 then
        table.insert(returnvalues, pandoc.CodeBlock(thetext))
    end

    if shared.codeBlockClassesContain(block,"insertdiagram") then
        table.insert(returnvalues, pandoc.Para(pandoc.Image({}, block.identifier..".svg")))
    end

    return returnvalues
end

function Pandoc(pandoc)
    if fileExists("whitelistexists") then
        whitelistexists = true
    end
end

return {
    { Pandoc = Pandoc },
    { CodeBlock = CodeBlock }
}

-- vim:sw=4:ts=4:et:ai:
