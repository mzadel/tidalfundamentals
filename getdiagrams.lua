
local shared = require('shared')

local diagrams = {}
local diagrampatterns = {}
local whitelist = {}
local replsessionhashes = {}

local function shouldRender(name)
    return next(whitelist) == nil or shared.arrayContains(whitelist,name)
end

function handleREPLBlock(block)

    local interpreter = shared.codeBlockInterpreter(block)
    local blockhash = shared.codeBlockSha1(block)

    replsessionhashes[blockhash] = interpreter

    local filename = string.format("%s-input-%s.txt", interpreter, blockhash)
    local fileptr = io.output(filename)
    local thetext = getBlockTextWithReplacements(block)
    io.write(thetext .. "\n")
    io.close(fileptr)
end

function handleDiagramBlock(block)
    exp = {}

    local tidalexpression = block.attributes["tidalexpression"]

    if shared.arrayContains(block.classes,"spatternexample") then
        tidalexpression = "s $ " .. tidalexpression
    end

    if shared.arrayContains(block.classes,"patternalgebraexample") then
        local left, operator, right, patterntypesignature = string.match(tidalexpression, "(.+) +([%|%+]+) +(.+) +:: +(.+)")

        exp[block.identifier .. "OperatorString"] = '"' .. operator .. '"'
        exp[block.identifier .. "Left"] = left .. ' :: ' .. patterntypesignature
        exp[block.identifier .. "Right"] = right .. ' :: ' .. patterntypesignature
    end

    if shared.arrayContains(block.classes,"signalsamplingexample") then
        local parameter, func, arc = string.match(tidalexpression, "queryArc %((%a+) (%a+)%) +%((Arc [%d%.]+ [%d%.]+)%)")
        exp[block.identifier .. "Parameter"] = parameter
        exp[block.identifier .. "Function"] = func .. " :: Pattern Double"
        exp[block.identifier .. "Arc"] = arc .. " :: Arc"
    end

    if shared.arrayContains(block.classes,"signalsetsparameter") then
        local leftexpression, operator, rightexpression = string.match(tidalexpression, "%((.+)%) ([%|%+]+) %((.+)%)")
        local key, segmentedcurve, func = string.match(leftexpression, '(%a+) %$ (segment %d+ %$ (%a+))')

        rightexpression = string.gsub(rightexpression, "s $ ", "s $ parseBP_E ", nil, true)

        exp[block.identifier .. "Key"] = key
        exp[block.identifier .. "SegmentedCurve"] = segmentedcurve .. " :: Pattern Double"
        exp[block.identifier .. "SegmentedCurveString"] = '"' .. segmentedcurve .. '"'
        exp[block.identifier .. "Function"] = func .. " :: Pattern Double"
        exp[block.identifier .. "OperatorString"] = '"' .. operator .. '"'
        exp[block.identifier .. "Right"] = rightexpression

        exp[block.identifier .. "String"] = '"' .. string.gsub(tidalexpression, '"', '\\"', nil, true) .. '"'

        tidalexpression = string.gsub(tidalexpression, "s $ ", "s $ parseBP_E ", nil, true)
    end

    if shared.arrayContains(block.classes,"queryexample") then
        local pat, arc = string.match(tidalexpression, "queryArc %((.+)%) %((Arc [%d%.]+ [%d%.]+)%)")
        exp[block.identifier .. "Pattern"] = pat
        exp[block.identifier .. "Arc"] = arc .. " :: Arc"
    end

    if tidalexpression ~= nil then
        exp[block.identifier] = tidalexpression
    end

    table.insert(diagrams,block.identifier)
    diagrampatterns[block.identifier] = exp
end

function getBlockTextWithReplacements(block)

    local thetext = block.text

    local tidalexpression = block.attributes["tidalexpression"]

    if tidalexpression ~= nil then
        tidalexpression = string.gsub(tidalexpression, "%%", "%%%%", nil, true)
        thetext = string.gsub(thetext, "{{tidalexpression}}", tidalexpression, nil, true)
    end

    return thetext
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
    io.write("diagrams=\\\n")
    for _, diagramname in ipairs(diagrams) do
        if shouldRender(diagramname) then
            io.write(string.format("%s\\\n", diagramname))
        end
    end
    io.close(fileptr)
end

function writeGhciMakefile()
    local fileptr = io.output("Makefile.replsessions")
    io.write("replsessions=\\\n")
    for hash, interpreter in pairs(replsessionhashes) do
        if shouldRender(hash) then
            io.write(string.format("%s-output-%s.txt\\\n", interpreter, hash))
        end
    end
    io.close(fileptr)
end

function CodeBlock(block)

    if shared.arrayContains(block.classes, "ghcisession") or shared.arrayContains(block.classes, "tidalsession") then
        handleREPLBlock(block)
    end

    if shared.arrayContains(block.classes,"diagram") then
        handleDiagramBlock(block)
    end

    if shared.arrayContains(block.classes,"whitelist") then

        -- add a dummy entry for the case where a block is neither a REPL
        -- session nor a diagram, but we still want to whitelist the block
        if next(whitelist) == nil then
            table.insert(whitelist,"WHITELISTEXISTS")
        end

        if shared.arrayContains(block.classes, "ghcisession") or shared.arrayContains(block.classes, "tidalsession") then
            table.insert(whitelist,shared.codeBlockSha1(block))
        end

        if shared.arrayContains(block.classes,"diagram") and string.len(block.identifier) > 0 then
            table.insert(whitelist,block.identifier)
        end

    end

end

function Pandoc(pdoc)
    writeWhitelistExistsFile()
    writeHaskellDiagramPatterns()
    writeDiagramMakefile()
    writeGhciMakefile()
end

-- vim:sw=4:ts=4:et:ai:
