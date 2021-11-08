
local ghciinputfilename="ghci.input"
local ghcioutputfilename="ghci.output"

local function writetoFile(contents)
    local fileptr = io.output(ghciinputfilename)
    io.write(contents)
    io.close(fileptr)
end

local function runGHCI()
    os.execute("TERM=dumb script -q ghci.output ghci < ghci.input > /dev/null")
end

local function readGHCiOutput()
    local fileptr = io.input(ghcioutputfilename)
    local contents = io.read("*all")
    io.close(fileptr)
    return contents
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
    if block.classes[1] == "ghcisession" then

        ghciscript = block.text

        if block.attributes["tidalexpression"] ~= nil then
            ghciscript = string.gsub(ghciscript, "{{tidalexpression}}", block.attributes["tidalexpression"], nil, true)
        end

        writetoFile(ghciscript .. "\n")
        runGHCI()
        local ghcioutput = readGHCiOutput()
        local codeblocktext = trimExample(convertCarriageReturnsToNewlines(ghcioutput))

        return pandoc.CodeBlock(codeblocktext)
    end
end

-- vim:sw=4:ts=4:et:ai:
