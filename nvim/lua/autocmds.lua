local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Makefiles require real tabs
augroup("Makefiles", { clear = true })
autocmd("FileType", {
    group    = "Makefiles",
    pattern  = "make",
    callback = function()
        vim.opt_local.expandtab   = false
        vim.opt_local.tabstop     = 8
        vim.opt_local.softtabstop = 8
        vim.opt_local.shiftwidth  = 8
    end,
})

-- Rust: 100-column textwidth (common in the ecosystem)
augroup("Rust", { clear = true })
autocmd("FileType", {
    group    = "Rust",
    pattern  = "rust",
    callback = function() vim.opt_local.textwidth = 100 end,
})

-- Python: PEP 8
augroup("Python", { clear = true })
autocmd("FileType", {
    group    = "Python",
    pattern  = "python",
    callback = function() vim.opt_local.textwidth = 79 end,
})

-- Markdown: enable wrap and spell
augroup("Markdown", { clear = true })
autocmd("FileType", {
    group    = "Markdown",
    pattern  = "markdown",
    callback = function()
        vim.opt_local.wrap      = true
        vim.opt_local.spell     = true
        vim.opt_local.textwidth = 0
    end,
})

-- Highlight trailing whitespace (non-insert modes)
augroup("TrailingWS", { clear = true })
autocmd({ "BufWinEnter", "InsertLeave" }, {
    group    = "TrailingWS",
    callback = function() vim.fn.matchadd("ErrorMsg", [[\s\+$]]) end,
})
autocmd("InsertEnter", {
    group    = "TrailingWS",
    callback = function() vim.fn.clearmatches() end,
})
