local ls = require("luasnip") local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local d = ls.dynamic_node
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local rep = require("luasnip.extras").rep


return {
-- Latex snippets
s({trig=";a", snippetType="autosnippet"},
  {
    t("\\alpha"),
  }
),
s({trig="pg",},
  fmt( -- the snippet code actually looks like the equation environment it produces.
    [[
    \textcolor{PineGreen}{<>}
    ]],
    -- the insert node is placed in the <> angle brackets
    { i(1) },
    -- this is where i specify that angle brackets are used as node positions.
    { delimiters = "<>" }
  )
),
-- section and subsection
s({trig="s",},
  fmt( -- the snippet code actually looks like the equation environment it produces.
    [[
    \section{<>}
    ]],
    -- the insert node is placed in the <> angle brackets
    { i(1) },
    -- this is where i specify that angle brackets are used as node positions.
    { delimiters = "<>" }
  )
),
s({trig="ss",},
  fmt( -- the snippet code actually looks like the equation environment it produces.
    [[
    \subsection*{<>}
    ]],
    -- the insert node is placed in the <> angle brackets
    { i(1) },
    -- this is where i specify that angle brackets are used as node positions.
    { delimiters = "<>" }
  )
),
s({trig="eq", dscr="A LaTeX equation environment"},
  fmt( -- The snippet code actually looks like the equation environment it produces.
    [[
      \begin{equation}
          <>
      \end{equation}
    ]],
    -- The insert node is placed in the <> angle brackets
    { i(1) },
    -- This is where I specify that angle brackets are used as node positions.
    { delimiters = "<>" }
  )
),
s({trig="i", dscr="A LaTeX equation environment"},
  fmt( -- The snippet code actually looks like the equation environment it produces.
    [[
      \emph{<>}
    ]],
    -- The insert node is placed in the <> angle brackets
    { i(1) },
    -- This is where I specify that angle brackets are used as node positions.
    { delimiters = "<>" }
  )
),
s({trig="b", dscr="A LaTeX equation environment"},
  fmt( -- The snippet code actually looks like the equation environment it produces.
    [[
      \textbf{<>}
    ]],
    -- The insert node is placed in the <> angle brackets
    { i(1) },
    -- This is where I specify that angle brackets are used as node positions.
    { delimiters = "<>" }
  )
),
s({trig="env",},
  fmta(
    [[
      \begin{<>}
          <>
      \end{<>}
    ]],
    {
      i(1),
      i(2),
      rep(1),  -- this node repeats insert node i(1)
    }
  )
),
}
