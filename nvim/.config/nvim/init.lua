--	 ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ 
--	 ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║
--	 ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ 
--	 ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ 
--	 ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ 
--	 ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ 

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("options")
require("plugins")
require("plugins.telescope")
require("plugins.lualine")
require("plugins.org")

--colorscheme
vim.cmd.colorscheme "gruvbox-material"

vim.g.livepreview_previewer = 'zathura'
vim.g.vimtex_view_method = 'zathura'

