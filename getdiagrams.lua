
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

function handleDiagramBlock(block)
    table.insert(diagrams,block.identifier)
    diagrampatterns[block.identifier] = {}

    local tidalexpression = block.attributes["tidalexpression"]

    if arrayContains(block.classes,"patternalgebraexample") then

        patterntypesignature = ""
        operatortypesignature = ""
        if block.attributes["type"] ~= nil then
            patterntypesignature = string.format(" :: Pattern %s", block.attributes["type"])
            operatortypesignature =  string.format(" :: Pattern %s -> Pattern %s -> Pattern %s", block.attributes["type"], block.attributes["type"], block.attributes["type"])
        end

        diagrampatterns[block.identifier][block.identifier .. "Operator"] = '(' .. block.attributes["operator"] .. ')' .. operatortypesignature
        diagrampatterns[block.identifier][block.identifier .. "OperatorString"] = '"' .. block.attributes["operator"] .. '"'
        diagrampatterns[block.identifier][block.identifier .. "Left"] = block.attributes["leftexpression"] .. patterntypesignature
        diagrampatterns[block.identifier][block.identifier .. "Right"] = block.attributes["rightexpression"] .. patterntypesignature

        tidalexpression = string.format("%s %s %s", block.attributes["leftexpression"], block.attributes["operator"], block.attributes["rightexpression"]) .. patterntypesignature
    end

    if tidalexpression ~= nil then
        diagrampatterns[block.identifier][block.identifier] = tidalexpression
    end
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
        for variablename, expressiontext in pairs(diagrampatterns[diagramname]) do
            if not shouldRender(diagramname) then
                expressiontext = "undefined"
            end

            io.write(string.format("%sExpr = %s\n", variablename, expressiontext))
        end
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
    if arrayContains(block.classes,"diagram") then
        handleDiagramBlock(block)
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
