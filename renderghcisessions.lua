
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

local function trimExample(text)
    local startposition, trimfrom = string.find(text, "Prelude> import Sound.Tidal.Context\n")
    local trimto, endposition = string.find(text, "Prelude Sound.Tidal.Context> \nLeaving GHCi.")
    return string.sub(text,trimfrom+1,trimto-1)
end

function CodeBlock(block)
    if block.classes[1] == "ghcisession" then

        writetoFile(block.text .. "\n")
        runGHCI()
        local ghcioutput = readGHCiOutput()
        local codeblocktext = trimExample(convertCarriageReturnsToNewlines(ghcioutput))

        return pandoc.CodeBlock(codeblocktext)
    end
end

-- vim:sw=4:ts=4:et:ai:
