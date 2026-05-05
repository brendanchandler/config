return {
    {
        "nvim-treesitter/nvim-treesitter",
        lazy  = false,
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter").install({
                "c", "cpp", "python", "rust", "bash",
                "lua", "vim", "vimdoc",
                "cmake", "make", "toml", "json", "yaml",
                "markdown", "markdown_inline",
            })
        end,
    },
}
