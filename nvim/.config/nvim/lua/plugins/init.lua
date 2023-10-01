require("lazy").setup({
  "folke/which-key.nvim",
  { "folke/neoconf.nvim", cmd = "Neoconf" },
  "ms-jpq/chadtree",
  "norcalli/nvim-colorizer.lua",
  "folke/neodev.nvim",
  "sainnhe/gruvbox-material",
  "akinsho/org-bullets.nvim",
  {
  "willothy/nvim-cokeline",
  config = true
},
  "terrortylor/nvim-comment",
  {'iamcco/markdown-preview.nvim'},
   {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
  },
  {'lervag/vimtex'},
  {"nvim-treesitter/nvim-treesitter"},
  {"nvim-orgmode/orgmode"},
  {
  'glepnir/dashboard-nvim',
  event = 'VimEnter',
  config = function()
    require('dashboard').setup {
    theme = 'hyper',
    config = {
      week_header = {
       enable = true,
      },
      shortcut = {
        { desc = '󰊳 Update', group = '@property', action = 'Lazy update', key = 'u' },
        {
          icon = ' ',
          icon_hl = '@variable',
          desc = 'Files',
          group = 'Label',
          action = 'Telescope find_files',
          key = 'f',
        },
        {
          desc = ' Apps',
          group = 'DiagnosticHint',
          action = 'Telescope app',
          key = 'a',
        },
        {
          desc = ' dotfiles',
          group = 'Number',
          action = 'Telescope dotfiles',
          key = 'd',
        },
      },
    },
  }
  end,
  dependencies = { {'nvim-tree/nvim-web-devicons'}}
},
{
    'nvim-telescope/telescope.nvim', tag = '0.1.2',
-- or                              , branch = '0.1.x',
      dependencies = { 'nvim-lua/plenary.nvim' }
},
'xuhdev/vim-latex-live-preview'
})

