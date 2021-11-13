
local ghciinputfilename="ghci.input"
local ghcioutputfilename="ghci.output"

local whitelistexists = false

local skippedCodeBlockText = "SKIPPED"

local function codeBlockClassesContain(block, classname)
    for _, value in ipairs(block.classes) do
        if value == classname then
            return true
        end
    end

    return false
end

function fileExists(name)
    local f=io.open(name,"r")
    if f ~= nil then io.close(f) return true end
    return false
end

local function writetoFile(contents)
    local fileptr = io.output(ghciinputfilename)
    io.write(contents)
    io.close(fileptr)
end

local function runGHCI()
    os.execute("TERM=xterm script -q ghci.output ghci < ghci.input > /dev/null")
end

local function readGHCiOutput()
    local fileptr = io.input(ghcioutputfilename)
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

    -- if there's no cut present, start with the GHCi starting verbiage
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

function CodeBlock(block)

    if whitelistexists and not codeBlockClassesContain(block, "whitelist") then
        return pandoc.CodeBlock(skippedCodeBlockText)
    end

    local thetext = block.text

    local tidalexpression = block.attributes["tidalexpression"]

    if tidalexpression ~= nil then
        tidalexpression = string.gsub(tidalexpression, "%%", "%%%%", nil, true)
        expressionwithoutSmoney = string.gsub(tidalexpression, "s $ ", "", nil, true)

        thetext = string.gsub(thetext, "{{tidalexpression}}", tidalexpression, nil, true)
        thetext = string.gsub(thetext, "{{tidalexpressionnoSmoney}}", expressionwithoutSmoney, nil, true)
    end

    if codeBlockClassesContain(block, "ghcisession") then
        writetoFile(thetext .. "\n")
        runGHCI()
        local ghcioutput = readGHCiOutput()
        thetext = trimExample(convertCarriageReturnsToNewlines(stripBackspaces(stripEscapeSequences(ghcioutput))))
    end

    if codeBlockClassesContain(block,"insertdiagram") then
        return {pandoc.CodeBlock(thetext), pandoc.Para(pandoc.Image({}, block.identifier..".svg"))}
    else
        return pandoc.CodeBlock(thetext)
    end

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
