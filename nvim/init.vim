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

color default

set completeopt=menuone,longest
set dir=~/.vimswap/
set display+=lastline
set expandtab
set exrc
set formatoptions+=tcrq
set guifont=Menlo\ Regular:h14
set guioptions+=m "no menu
set guioptions-=T "no toolbar
set guioptions-=l "no left scrollbar
set guioptions-=r "no right scrollbar
set hidden
set hls
set linebreak
set mousemodel="popup"
set noic
set nowrap
set nowrapscan
set number
set path+=**
set ruler
set scrolloff=2 "scroll at 2 lines to bottom
set secure
set shiftwidth=4
set softtabstop=4
set synmaxcol=300 "syntax on long lines is slow.. limit syntax to 300 columns
set tags=tags;/
set textwidth=80
set wildignore=.c.basis,.c.edited,.c.link,.c.orig,.class,.p
set wildmenu
set wildmode=list:longest

let mapleader="\<SPACE>"

syntax on

" help autocmd-events to see list of events
augroup Makefiles
    au!
    au BufRead,BufNewFile Makefile* setlocal noet ts=8 sts=8 sw=8 nowrap
    au BufEnter,BufNewFile *.mak setlocal noet ts=8 sts=8 sw=8 nowrap
augroup END

"au BufNewFile,BufRead *.arxml set filetype=xml
"au BufNewFile,BufRead *.tlc set filetype=tlc

nnoremap :2html :source $VIMRUNTIME/syntax/2html.vim
nnoremap <C-h> 5zh
nnoremap <C-l> 5zl
nnoremap <C-wq> <nop>
nnoremap <f11> :prev<cr>
nnoremap <f12> :next<cr>
nnoremap <leader>W :match none<cr>
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>cs :w<cr>:!seqdiag %; eog %:r.png<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>gf :let @"=expand("%:p")<cr>
nnoremap <leader>hex :%!xxd<cr>
nnoremap <leader>nohex :%!xxd -r
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <M-h> :wincmd h<cr>
nnoremap <M-l> :wincmd l<cr>
nnoremap <M-j> :wincmd j<cr>
nnoremap <M-k> :wincmd k<cr>

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Using Lua functions
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>ss <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>bb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>hh <cmd>lua require('telescope.builtin').help_tags()<cr>


