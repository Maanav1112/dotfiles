require("lazy").setup({
  "folke/which-key.nvim",
  "catppuccin/nvim",
  "lervag/vimtex",
  {
    "iamcco/markdown-preview.nvim",
    ft = "markdown",
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },
  {"nvim-orgmode/orgmode"},
  {'romgrk/barbar.nvim',
    dependencies = 'nvim-tree/nvim-web-devicons',
    init = function() vim.g.barbar_auto_setup = false end,
    opts = {
       animation = true,
       insert_at_start = true,
    },
    version = '^1.0.0', -- optional: only update when a new 1.x version is released
  },
   'nvim-lualine/lualine.nvim',
   'nvim-treesitter/nvim-treesitter',
   'sainnhe/gruvbox-material',
  {'akinsho/toggleterm.nvim', version = "*", config = true},
  {
	"L3MON4D3/LuaSnip",
	-- follow latest release.
	version = "<CurrentMajor>.*",
	build = "make install_jsregexp"
},
})
