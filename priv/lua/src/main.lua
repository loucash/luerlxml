require "xml"
require "handler"

-- ------------------ API --------------------

function simple_tree_parser(xml)
    local xml = xml or gxml
    local h = simpleTreeHandler()
    local p = xmlParser1(h)
    p:parse(xml)
    return response_ok(h.root)
end

function dom_parser(xml)
    local xml = xml or gxml
    local h = domHandler()
    local p = xmlParser1(h)
    p:parse(xml)
    return response_ok(h.root)
end

-- ------------------ internal --------------------
function response_ok(response)
    return_val = {["status"] = "ok", ["response"] = response}
    return return_val
end
