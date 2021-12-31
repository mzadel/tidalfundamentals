
function Str(elem)
    local findresult = string.find(elem.text, "{{describegitref_", 1, true)

    if findresult ~= nil then
        local gitref = string.match(elem.text, "_(.*)}}")

        local handle

        handle = io.popen("git name-rev --name-only "..gitref)
        local gitrefname = handle:read("*a")
        gitrefname = string.gsub(gitrefname,"\n$","")
        gitrefname = string.gsub(gitrefname,"^tags/","")
        handle:close()

        handle = io.popen("git rev-parse "..gitref)
        local gitcommit = handle:read("*a")
        gitcommit = string.gsub(gitcommit,"\n$","")
        handle:close()

        local handle = io.popen("git log -1 --pretty=format:'%cs' "..gitref)
        local gitdate = handle:read("*a")
        gitdate = string.gsub(gitdate,"\n$","")
        handle:close()

        return {
            pandoc.Str(gitrefname),
            pandoc.Space(),
            pandoc.Str("("..gitcommit.."),"),
            pandoc.Space(),
            pandoc.Str(gitdate)
        }
    else
        return elem
    end
end

-- vim:sw=4:ts=4:et:ai:
