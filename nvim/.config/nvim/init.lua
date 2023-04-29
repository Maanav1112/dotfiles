require("opts")
require("bootstrap")
require("plugins")
require("plugins.lualine")
require("plugins.treesitter")
require("maps")
require("Luasnip")
vim.cmd [[colorscheme gruvbox-material]]
require("luasnip.loaders.from_lua").load({paths = "~/.config/nvim/LuaSnip/"})
vim.g.vimtex_compiler_method = 'tectonic'

