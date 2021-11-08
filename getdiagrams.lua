
local diagrams = {}
local diagrampatterns = {}

local function codeBlockClassesContain(block, classname)
    for index, value in ipairs(block.classes) do
        if value == classname then
            return true
        end
    end

    return false
end

function writeHaskellDiagramPatterns()
    local fileptr = io.output("PatternExpressions.hs")

    io.write("module PatternExpressions where\n\n")
    io.write("import Sound.Tidal.Context\n\n")

    for _, diagramname in ipairs(diagrams) do
        io.write(string.format("%sExpr = (%s)\n", diagramname, diagrampatterns[diagramname]))
    end

    io.close(fileptr)
end

function writeDiagramMakefile()
    local fileptr = io.output("Makefile.diagrams")
    io.write("diagramsX=\\\n")
    for _, diagramname in ipairs(diagrams) do
        io.write(string.format("%s\\\n", diagramname, diagrampatterns[diagramname]))
    end
    io.close(fileptr)
end

function CodeBlock(block)
    if codeBlockClassesContain(block,"ghcisession") and codeBlockClassesContain(block,"diagram") then
        table.insert(diagrams,block.identifier)
        diagrampatterns[block.identifier] = block.attributes["tidalexpression"]
    end
end

function Pandoc(pdoc)
    writeHaskellDiagramPatterns()
    writeDiagramMakefile()
end

-- vim:sw=4:ts=4:et:ai:
