vim.cmd([[
    call plug#begin('~/.config/nvim/plugged')
    Plug 'overcache/NeoSolarized'
    Plug 'BurntSushi/ripgrep'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-lualine/lualine.nvim'
    Plug 'github/copilot.vim'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    " If you want to have icons in your statusline choose one of these
    call plug#end()
]])

vim.cmd("color NeoSolarized")

vim.opt.completeopt= "menuone,longest"
vim.opt.dir= "~/.vimswap/"
vim.opt.display:append("lastline")
vim.opt.expandtab = true
vim.opt.exrc = true
vim.opt.formatoptions:append("tcrq")
vim.opt.guifont = "Menlo Regular:h14"
vim.opt.hidden = true
vim.opt.hls = true
vim.opt.linebreak = true
vim.opt.ic = false
vim.opt.wrap = false
vim.opt.wrapscan = false
vim.opt.number = true
vim.opt.path:append("**")
vim.opt.ruler = true
vim.opt.scrolloff = 2
vim.opt.secure = true
vim.opt.shiftwidth= 4
vim.opt.softtabstop= 4
vim.opt.synmaxcol= 300 -- syntax on long lines is slow.. limit syntax to 300 columns"
vim.opt.tags= "tags;/"
vim.opt.textwidth= 80
vim.opt.wildignore= ".c.basis,.c.edited,.c.link,.c.orig,.class,.p"
vim.opt.wildmenu = true
vim.opt.wildmode= "list:longest"

vim.cmd("let mapleader = '<Space>'")

vim.g.syntax = "on"

vim.cmd([[
" help autocmd-events to see list of events
augroup Makefiles
    au!
    au BufRead,BufNewFile Makefile* setlocal noet ts=8 sts=8 sw=8 nowrap
    au BufEnter,BufNewFile *.mak setlocal noet ts=8 sts=8 sw=8 nowrap
augroup END

"au BufNewFile,BufRead *.arxml set filetype=xml
"au BufNewFile,BufRead *.tlc set filetype=tlc
]])

vim.keymap.set('n', ':2html', ':source $VIMRUNTIME/syntax/2html.vim')
vim.keymap.set('n', '<C-h>', '5zh')
vim.keymap.set('n', '<C-l>', '5zl')
vim.keymap.set('n', '<C-wq>', '<nop>')
vim.keymap.set('n', '<f11>', ':prev<cr>')
vim.keymap.set('n', '<f12>', ':next<cr>')
vim.keymap.set('n', '<leader>W', ':match none<cr>')
vim.keymap.set('n', '<leader>cd', ':cd %:p:h<cr>:pwd<cr>')
vim.keymap.set('n', '<leader>cs', ':w<cr>:!seqdiag %; eog %:r.png<cr>')
vim.keymap.set('n', '<leader>ev', ':vsp $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>gf', ':let @"=expand("%:p")<cr>')
vim.keymap.set('n', '<leader>hex', ':%!xxd<cr>')
vim.keymap.set('n', '<leader>nohex', ':%!xxd -r')
vim.keymap.set('n', '<leader>sv', ':source $MYVIMRC<cr>')
vim.keymap.set('n', '<M-h>', ':wincmd h<cr>')
vim.keymap.set('n', '<M-l>', ':wincmd l<cr>')
vim.keymap.set('n', '<M-j>', ':wincmd j<cr>')
vim.keymap.set('n', '<M-k>', ':wincmd k<cr>')

-- Using Lua functions
vim.keymap.set('n', '<leader>ff', require("telescope.builtin").find_files)
vim.keymap.set('n', '<leader>fg', require("telescope.builtin").live_grep)
vim.keymap.set('n', '<leader>fb', require("telescope.builtin").buffers)
vim.keymap.set('n', '<leader>fh', require("telescope.builtin").help_tags)

