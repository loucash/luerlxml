package.path = "../src/?.lua;" ..
               "src/?.lua;" ..
               "lua/src/?.lua;" ..
               "priv/lua/src/?.lua;" ..
               "../test/?.lua;" ..
               "test/?.lua;" ..
               "lua/test/?.lua;" ..
               "priv/lua/test/?.lua;" ..
               package.path

local tsc       = require "telescope"
local main      = require "main"

local XML = "<a>1</a>"

-- ------------------------------------------------------------------------- --

context("Test parsers", function()
    test("simple tree parser", function()
        local r = simple_tree_parser(XML)
        assert_type(r, 'table')
        local status = r.status
        assert_equal(status, "ok")
        assert_not_nil(r.response)
    end)
    test("dom parser", function()
        if (true) then return end
        local r = dom_parser(XML)
        assert_type(r, 'table')
        local status = r.status
        assert_equal(status, "ok")
        assert_not_nil(r.response)
    end)
end)
