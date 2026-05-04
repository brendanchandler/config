# Neovim Config

## Setup (per machine)

1. Clone the config repo (if not already): `git clone ... ~/src/config`
2. Create `~/.config/nvim/init.lua` with:

   ```lua
   local repo = vim.fn.expand("~/src/config/nvim")
   vim.opt.rtp:prepend(repo)
   dofile(repo .. "/init.lua")
   ```

3. Launch `nvim` — lazy.nvim installs all plugins on first run.
4. LSP servers (clangd, pyright, rust-analyzer, bash-language-server) are
   installed automatically by mason-lspconfig on first file open.

## Key Mappings

| Key | Action |
|-----|--------|
| `<Space>` | Leader |
| `<C-h/j/k/l>` | Move between splits |
| `<leader>ff` | Find files (Telescope) |
| `<leader>fg` | Live grep |
| `<leader>fb` | Buffers |
| `<leader>fh` | Help tags |
| `gd` | Go to definition |
| `gr` | References |
| `K` | Hover docs |
| `<leader>rn` | Rename symbol |
| `<leader>ca` | Code action |
| `[d` / `]d` | Prev/next diagnostic |
| `<leader>e` | Show diagnostic float |
