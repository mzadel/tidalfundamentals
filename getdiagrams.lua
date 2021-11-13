
local diagrams = {}
local diagrampatterns = {}
local whitelist = {}

local function arrayContains(arr, item)
    for _, value in ipairs(arr) do
        if value == item then
            return true
        end
    end

    return false
end

local function shouldRender(name)
    return next(whitelist) == nil or arrayContains(whitelist,name)
end

function writeWhitelistExistsFile()
    if next(whitelist) ~= nil then
        os.execute("touch whitelistexists")
    end
end

function writeHaskellDiagramPatterns()
    local fileptr = io.output("PatternExpressions.hs")

    io.write("module PatternExpressions where\n\n")
    io.write("import Sound.Tidal.Context\n\n")

    for _, diagramname in ipairs(diagrams) do
        local expressiontext = "undefined";

        if shouldRender(diagramname) then
            expressiontext = diagrampatterns[diagramname]
        end

        io.write(string.format("%sExpr = (%s)\n", diagramname, expressiontext))
    end

    io.close(fileptr)
end

function writeDiagramMakefile()
    local fileptr = io.output("Makefile.diagrams")
    io.write("diagramsX=\\\n")
    for _, diagramname in ipairs(diagrams) do
        if shouldRender(diagramname) then
            io.write(string.format("%s\\\n", diagramname))
        end
    end
    io.close(fileptr)
end

function CodeBlock(block)
    if block.attributes["tidalexpression"] ~= nil and arrayContains(block.classes,"diagram") then
        table.insert(diagrams,block.identifier)
        diagrampatterns[block.identifier] = block.attributes["tidalexpression"]
    end

    if arrayContains(block.classes,"whitelist") then
        local blockIdentifier = "UNIDENTIFIED"
        if string.len(block.identifier) > 0 then
            blockIdentifier = block.identifier
        end
        table.insert(whitelist,blockIdentifier)
    end
end

function Pandoc(pdoc)
    writeWhitelistExistsFile()
    writeHaskellDiagramPatterns()
    writeDiagramMakefile()
end

-- vim:sw=4:ts=4:et:ai:
