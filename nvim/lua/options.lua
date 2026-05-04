local opt = vim.opt

opt.completeopt    = { "menuone", "longest", "preview" }
opt.expandtab      = true
opt.formatoptions:append("tcrq")
opt.hidden         = true
opt.hlsearch       = true
opt.ignorecase     = false
opt.linebreak      = true
opt.number         = true
opt.relativenumber = true
opt.ruler          = true
opt.scrolloff      = 4
opt.shiftwidth     = 4
opt.softtabstop    = 4
opt.tabstop        = 4
opt.signcolumn     = "yes"
opt.synmaxcol      = 300
opt.termguicolors  = true
opt.textwidth      = 80
opt.updatetime     = 250
opt.wildmenu       = true
opt.wildmode       = { "list", "longest" }
opt.wrap           = false
opt.wrapscan       = false
