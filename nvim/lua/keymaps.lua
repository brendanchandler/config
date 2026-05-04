vim.g.mapleader      = " "
vim.g.maplocalleader = " "

local map = vim.keymap.set

-- Clear search highlight
map("n", "<Esc>", "<cmd>nohlsearch<cr>")

-- Window navigation (standard across the community)
map("n", "<C-h>", "<C-w>h", { desc = "Move to left split" })
map("n", "<C-j>", "<C-w>j", { desc = "Move to lower split" })
map("n", "<C-k>", "<C-w>k", { desc = "Move to upper split" })
map("n", "<C-l>", "<C-w>l", { desc = "Move to right split" })

-- Telescope
map("n", "<leader>ff", function() require("telescope.builtin").find_files() end,  { desc = "Find files" })
map("n", "<leader>fg", function() require("telescope.builtin").live_grep() end,   { desc = "Live grep" })
map("n", "<leader>fb", function() require("telescope.builtin").buffers() end,     { desc = "Buffers" })
map("n", "<leader>fh", function() require("telescope.builtin").help_tags() end,   { desc = "Help tags" })

-- Hex view toggle
map("n", "<leader>hex",   ":%!xxd<cr>",    { desc = "Hex view" })
map("n", "<leader>nohex", ":%!xxd -r<cr>", { desc = "Hex restore" })

-- Diagnostic navigation (LSP buffer-local maps are in lsp.lua on_attach)
map("n", "[d", vim.diagnostic.goto_prev,        { desc = "Prev diagnostic" })
map("n", "]d", vim.diagnostic.goto_next,        { desc = "Next diagnostic" })
map("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic" })
map("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Diagnostics list" })
