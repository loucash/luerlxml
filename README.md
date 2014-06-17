luerlxml
========

Simple XML Parser in Erlang based on
[LuaXML](http://manoelcampos.com/files/LuaXML-0.0.1-(lua5).tar.gz), from Paul Chakravarti and
[luerl](https://github.com/rvirding/luerl), lua interpreter from Robert Virding.

It is a POC how you can use `luerl` interpreter in your project, by embedding
lua code.

I don't know how efficient this parser is. Some benchmarking needs to
be done.

## Usage

Build:

```
make deps compile
```

You need to start an application:

```erlang
1> luerlxml:start().
ok
```

And then you have two parsers available:

`dom_parser`:

```erlang
2> luerlxml:dom_parser(<<"<a>123</a>">>).
{ok,[{<<"_children">>,
      [[{<<"_children">>,
         [[{<<"_text">>,<<"123">>},{<<"_type">>,<<"TEXT">>}]]},
        {<<"_name">>,<<"a">>},
        {<<"_type">>,<<"ELEMENT">>}]]},
     {<<"_type">>,<<"ROOT">>}]}
```

`simple_tree_parser`:

```erlang
3> luerlxml:simple_tree_parser(<<"<a>123</a>">>).
{ok,[{<<"a">>,<<"123">>}]}
```

## Tests

There is a really minimum of tests added, just to show that it is working ;-)

```
$  make test
==> luerlxml (ct)
DONE.
Testing Github.luerlxml: TEST COMPLETE, 2 ok, 0 failed of 2 test cases

tsc --luacov -f priv/lua/test/test_*.lua
------------------------------------------------------------------------
Test parsers:
simple tree parser                                                   [P]
dom parser                                                           [P]
------------------------------------------------------------------------
2 tests 2 passed 6 assertions 0 failed 0 errors 0 unassertive 0 pending
luacov
```


## Contributing
If you see something missing or incorrect, do not hesitate to create an issue
or pull request. Thank you!
