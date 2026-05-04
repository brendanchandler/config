return {
    {
        "folke/tokyonight.nvim",
        lazy     = false,
        priority = 1000,
        opts     = { style = "night" },
        config   = function(_, opts)
            require("tokyonight").setup(opts)
            vim.cmd.colorscheme("tokyonight-night")
        end,
    },
    {
        "nvim-lualine/lualine.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        opts = {
            options = {
                theme                = "tokyonight",
                section_separators   = "",
                component_separators = "|",
            },
            sections = {
                lualine_c = { { "filename", path = 1 } },
            },
        },
    },
    { "nvim-tree/nvim-web-devicons", lazy = true },
}
