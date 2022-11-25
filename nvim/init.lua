local options = {
  undofile = true, -- persitent undo
  autoread = true, -- auto reload file

  expandtab = true,
  smarttab = true,
  shiftwidth = 2,
  tabstop = 2,

  hlsearch = false,
  incsearch = true,
  ignorecase = true,
  smartcase = true,

  splitbelow = true,
  splitright = true,

  wrap = true,
  scrolloff = 5,
  sidescrolloff = 5,
  cursorline = true,
  laststatus = 3,
  showmode = false,
  termguicolors = true,
  signcolumn = "yes",
  -- winbar = "%t %m",

  listchars = "tab:>·,trail:~,lead:·",

}

for k, v in pairs(options) do
  vim.opt[k] = v
end

-- Basic mappings

vim.g.mapleader = " "

local map = vim.keymap.set

map("n", "x", '"_x') -- disable yanking for x

map("i", "<C-c>", "<Esc>")

local nmap = {
  ["<A-j>"] = ":bnext<CR>",
  ["<A-k>"] = ":bprevious<CR>",
}

for k, v in pairs(nmap) do
  map("n", k, v, { noremap = true, silent = true })
end

-- Loading plugins
require("rl.plugins")

vim.cmd("colorscheme onedarkpro")

require'nvim-treesitter.configs'.setup {
  indent = {
    enable = true
  }
}
