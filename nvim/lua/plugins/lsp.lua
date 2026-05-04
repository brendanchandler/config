return {
    { "williamboman/mason.nvim", opts = {} },
    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = { "williamboman/mason.nvim" },
        opts = {
            ensure_installed = {
                "clangd",        -- C/C++
                "pyright",       -- Python
                "rust_analyzer", -- Rust
                "bashls",        -- Bash
                "cmake",         -- CMake
            },
            automatic_installation = true,
        },
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = { "williamboman/mason-lspconfig.nvim", "hrsh7th/cmp-nvim-lsp" },
        config = function()
            local lspconfig    = require("lspconfig")
            local capabilities = require("cmp_nvim_lsp").default_capabilities()

            local on_attach = function(_, bufnr)
                local map = function(keys, func, desc)
                    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
                end
                local b = require("telescope.builtin")
                map("gd",         b.lsp_definitions,       "Go to definition")
                map("gr",         b.lsp_references,        "References")
                map("gI",         b.lsp_implementations,   "Go to implementation")
                map("<leader>D",  b.lsp_type_definitions,  "Type definition")
                map("<leader>ds", b.lsp_document_symbols,  "Document symbols")
                map("<leader>ws", b.lsp_workspace_symbols, "Workspace symbols")
                map("K",          vim.lsp.buf.hover,       "Hover docs")
                map("<leader>rn", vim.lsp.buf.rename,      "Rename symbol")
                map("<leader>ca", vim.lsp.buf.code_action, "Code action")
            end

            local servers = {
                clangd        = {},
                pyright       = {},
                rust_analyzer = {},
                bashls        = {},
                cmake         = {},
            }
            for name, cfg in pairs(servers) do
                cfg.on_attach    = on_attach
                cfg.capabilities = capabilities
                lspconfig[name].setup(cfg)
            end
        end,
    },
}
