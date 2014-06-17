require "xml"
require "handler"

--- domHandler
function domHandler()
    local obj = {}
    obj.options = {commentNode=1,piNode=1,dtdNode=1,declNode=1}
    obj.root = { _children = {}, _type = "ROOT" }
    obj.stack = {obj.root}
    obj.current = obj.root
    obj.starttag = function(self,t,a)
            local node = { _type = 'ELEMENT',
                           _name = t,
                           _attr = a,
                           _children = {} }
            local current = self.stack[#self.stack]
            table.insert(current._children,node)
            table.insert(self.stack, node)
    end
    obj.endtag = function(self,t,s)
            local current = self.stack[#self.stack]
            if t ~= current._name then
                error("XML Error - Unmatched Tag ["..s..":"..t.."]\n")
            end
            table.remove(self.stack)
    end
    obj.text = function(self,t)
            local current = self.stack[#self.stack]
            local node = { _type = "TEXT",
                           _text = t }
            table.insert(current._children,node)
    end
    obj.comment = function(self,t)
            local current = self.stack[#self.stack]
            if self.options.commentNode then
                local node = { _type = "COMMENT",
                               _text = t }
                table.insert(current._children,node)
            end
    end
    obj.pi = function(self,t,a)
            local current = self.stack[#self.stack]
            if self.options.piNode then
                local node = { _type = "PI",
                               _name = t,
                               _attr = a }
                table.insert(current._children,node)
            end
    end
    obj.decl = function(self,t,a)
            local current = self.stack[#self.stack]
            if self.options.declNode then
                local node = { _type = "DECL",
                               _name = t,
                               _attr = a }
                table.insert(current._children,node)
            end
    end
    obj.dtd = function(self,t,a)
            local current = self.stack[#self.stack]
            if self.options.dtdNode then
                local node = { _type = "DTD",
                               _name = t,
                               _attr = a }
                table.insert(current._children,node)
            end
    end
    obj.cdata = obj.text
    return obj
end

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
