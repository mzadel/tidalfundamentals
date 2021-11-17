
local shared = {}
local pandocutils = require('pandoc.utils')

function shared.arrayContains(arr, item)
    for _, value in ipairs(arr) do
        if value == item then
            return true
        end
    end

    return false
end

function shared.codeBlockClassesContain(block, classname)
    return shared.arrayContains(block.classes, classname)
end

function shared.tableToString(t)
    result = ""
    for k,v in pairs(t) do
        result = result .. tostring(k) .. tostring(v)
    end
    return result
end

function shared.stringifyAttr(a)
    return a.identifier .. shared.tableToString(a.classes) .. shared.tableToString(a.attributes)
end

function shared.stringifyCodeBlock(block)
    return shared.stringifyAttr(block.attr) .. block.text
end

function shared.codeBlockSha1(block)
    return pandocutils.sha1(shared.stringifyCodeBlock(block))
end

function shared.codeBlockInterpreter(block)
    local interpreter = "INTERPRETERNOTFOUND"
    if shared.arrayContains(block.classes,"ghcisession") then
        interpreter = "ghci"
    elseif shared.arrayContains(block.classes,"tidalsession") then
        interpreter = "tidal"
    end
    return interpreter
end

return shared
