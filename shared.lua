
local shared = {}

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

return shared
