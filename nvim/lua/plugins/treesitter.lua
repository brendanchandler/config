return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = {
                    "c", "cpp", "python", "rust", "bash",
                    "lua", "vim", "vimdoc",
                    "cmake", "make", "toml", "json", "yaml",
                    "markdown", "markdown_inline",
                },
                auto_install = true,
                highlight    = { enable = true },
                indent       = { enable = true },
            })
        end,
    },
}
