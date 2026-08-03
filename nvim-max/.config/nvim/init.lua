vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.env.NVIM = vim.v.servername

local border = "rounded"

vim.g.netrw_browsex_viewer = "xdg-open"
vim.g.netrw_banner = 1
vim.g.netrw_liststyle = 1
vim.g.netrw_preview = 1
vim.g.netrw_keepdir = 0
vim.g.netrw_localcopydircmd = 'cp -r'

vim.opt.clipboard:append("unnamedplus")
vim.g.clipboard = 'osc52'
if vim.env.TMUX then
  vim.g.clipboard = 'tmux'
end

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.hlsearch = true
vim.opt.scrolloff = 0
vim.opt.number = true
vim.opt.mouse = "a"
vim.opt.breakindent = true
vim.opt.undofile = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.infercase = true
vim.opt.smartcase = true
vim.opt.smarttab = true
vim.opt.smartindent = true
vim.opt.signcolumn = "auto:2"
vim.opt.updatetime = 250
vim.opt.timeout = true
vim.opt.timeoutlen = 300
vim.opt.completeopt = "menu,menuone,fuzzy,noinsert,noselect"
vim.opt.cursorline = false
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true
vim.opt.listchars = { tab = "▸ ", extends = "…", precedes = "…", nbsp = "␣", trail = "·" }
vim.opt.list = true
vim.opt.laststatus = 3
vim.opt.confirm = false
vim.opt.showmode = false
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.shortmess = vim.opt.shortmess:append("WcC")
vim.opt.pumheight = 10
vim.opt.pumblend = 0
vim.opt.pumborder = border
vim.opt.winblend = 0
vim.opt.virtualedit = "block"
vim.opt.fillchars = { eob = " " }
vim.opt.splitkeep = "screen"
vim.opt.grepprg = 'rg --vimgrep -. --no-messages --smart-case'
vim.opt.background = 'dark'
vim.opt.winborder = border
vim.opt.path:append("**")
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.wrap = false

local tree_sitters = {
  "asm", "awk", "bash", "c", "c3", "c_sharp", "caddy", "cmake", "comment",
  "commonlisp", "css", "csv", "dart", "diff", "disassembly", "dockerfile",
  "eex", "elixir", "embedded_template", "git_config", "git_rebase",
  "gitattributes", "gitcommit", "gitignore", "go", "gomod", "gosum",
  "gotmpl", "gowork", "graphql", "hcl", "heex", "html", "htmldjango",
  "http", "hurl", "hjson", "java", "javadoc", "javascript", "jinja",
  "jinja_inline", "jjdescription", "jq", "json", "json5", "jsonnet",
  "kotlin", "latex", "lua", "luadoc", "make", "markdown", "markdown_inline",
  "mermaid", "nasm", "nginx", "ninja", "nix", "perl", "php", "php_only",
  "proto", "python", "query", "ruby", "rust", "scheme", "scss", "sql",
  "svelte", "templ", "terraform", "toml", "tsv", "tsx", "twig",
  "typescript", "vim", "vimdoc", "xml", "yaml", "zig",
}

local language_servers = {
  bashls = {},
  clangd = {},
  c3_lsp = {},
  omnisharp = {},
  cssls = {},
  tailwindcss = {},
  dartls = {},
  docker_compose_language_service = {},
  dockerls = {},
  expert = {
    settings = {
      workspaceSymbols = {
        minQueryLength = 0
      }
    }
  },
  emmet_language_server = {
    filetypes = { "css", "eruby", "gohtml", "heex", "html", "javascript", "javascriptreact", "less", "php", "pug", "sass", "scss", "templ", "typescriptreact" },
  },
  gopls = {},
  goimports = {},
  golangci_lint = {},
  golangci_lint_ls = {},
  graphql = {},
  html = {},
  htmx = {},
  jdtls = {},
  jsonls = {},
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        diagnostics = { globals = { "vim" } },
        workspace = {
          library = { vim.env.VIMRUNTIME },
          checkThirdParty = false,
        },
      },
    },
  },
  marksman = {},
  perlpls = {},
  phpactor = {},
  buf_ls = {},
  ruff = {},
  ty = {},
  rubocop = {},
  ruby_lsp = {},
  rust_analyzer = {},
  sqls = {},
  svelte = {},
  templ = {},
  terraformls = {},
  tombi = {},
  ts_ls = {},
  yamlls = {},
  ansiblels = {},
  zls = {},
  org = {},
}

vim.pack.add({
  'https://github.com/romus204/tree-sitter-manager.nvim',
  'https://github.com/neovim/nvim-lspconfig',
  'https://github.com/nvim-mini/mini.nvim',
  'https://github.com/lewis6991/gitsigns.nvim',
  'https://github.com/christoomey/vim-tmux-navigator',
  'https://github.com/nvim-lua/plenary.nvim',
  'https://github.com/folke/todo-comments.nvim',
  'https://github.com/folke/snacks.nvim',
  'https://github.com/folke/which-key.nvim',
  'https://github.com/MeanderingProgrammer/render-markdown.nvim',
  'https://github.com/3rd/image.nvim',
  'https://github.com/github/copilot.vim',
})

local on_attach = function(_, bufnr)
  vim.keymap.set("n", "<leader>ln", vim.lsp.buf.rename, { buffer = bufnr, desc = "Rename symbol" })
  vim.keymap.set({ "n", "v" }, "<leader>lf", vim.lsp.buf.format, { buffer = bufnr, desc = "Format code" })
  vim.keymap.set({ "n", "v" }, "<leader>la", vim.lsp.buf.code_action, { buffer = bufnr, desc = "Code actions" })
  vim.keymap.set("n", "<leader>lh", vim.lsp.buf.signature_help, { buffer = bufnr, desc = "Signature help" })

  vim.keymap.set("n", "<leader>ld", function() Snacks.picker.diagnostics({ bufnr = bufnr }) end,
    { buffer = bufnr, desc = "Document diagnostics" })
  vim.keymap.set("n", "<leader>lD", function() Snacks.picker.diagnostics() end,
    { buffer = bufnr, desc = "Workspace diagnostics" })
  vim.keymap.set("n", "<leader>ls", function() Snacks.picker.lsp_symbols({ bufnr = bufnr }) end,
    { buffer = bufnr, desc = "Document symbols" })
  vim.keymap.set("n", "<leader>lS", function() Snacks.picker.lsp_workspace_symbols() end,
    { buffer = bufnr, desc = "Workspace symbols" })
  vim.keymap.set("n", "<leader>lr", function() Snacks.picker.lsp_references() end,
    { buffer = bufnr, desc = "References" })
end

local MiniIcons = require("mini.icons")
MiniIcons.setup()
MiniIcons.tweak_lsp_kind()
MiniIcons.mock_nvim_web_devicons()

local MiniNotify = require("mini.notify")
MiniNotify.setup({
  window = {
    config = { border = border },
    winblend = 0,
    max_width_share = 0.5,
  },
})
vim.notify = MiniNotify.make_notify()

require("mini.statusline").setup({ use_icons = true })
require("mini.tabline").setup({ show_icons = true, tabpage_section = "right" })

require("mini.files").setup({
  mappings = {
    close       = '<Esc>',
    go_in       = 'l',
    go_in_plus  = '<cr>',
    go_out      = 'h',
    go_out_plus = '-',
    mark_goto   = "'",
    mark_set    = 'm',
    reset       = '<BS>',
    reveal_cwd  = '@',
    show_help   = 'g?',
    synchronize = '=',
    trim_left   = '<',
    trim_right  = '>',
  },
})
vim.keymap.set('n', '-', '<cmd>lua MiniFiles.open()<CR>')
vim.api.nvim_create_autocmd("User", {
  pattern = { "MiniFilesWindowOpen", "MiniFilesWindowUpdate" },
  callback = function(args)
    local win_id = args.data.win_id
    vim.api.nvim_win_set_config(win_id, { border = border })
  end,
})

require("mini.sessions").setup()
vim.keymap.set("n", "<leader>sw", ':lua MiniSessions.write("")<Left><Left>', { desc = "Write" })
vim.keymap.set("n", "<leader>sd", '<cmd>lua MiniSessions.select("delete", {force = true})<CR>', { desc = "Delete" })
vim.keymap.set("n", "<leader>sr", '<cmd>lua MiniSessions.select("read")<CR>', { desc = "Read" })

local Snacks = require('snacks')

Snacks.setup({
  animate = {
    enabled = true,
    animate = { enabled = true },
    filter = function(buf)
      return vim.g.snacks_dim ~= false and vim.b[buf].snacks_dim ~= false and vim.bo[buf].buftype == ""
    end,
  },
  picker = {
    enabled = true,
    ui_select = true,
    win = {
      input = {
        keys = {
          ["<esc>"] = { "close", mode = { "i", "n" } },
        },
      },
    },
  },
  explorer = { enabled = true, replace_netrw = true },
  indent = { enabled = true },
  scope = { enabled = true },
  gitbrowse = { enabled = true },
  lazygit = { enabled = true },
  dim = { enabled = true },
  image = { enabled = true },
  input = { enabled = true },
  scroll = { enabled = true },
  statuscolumn = { enabled = true },
})

vim.keymap.set('n', '-', '<cmd>lua Snacks.explorer.open()<CR>', { desc = 'Explorer' })
vim.keymap.set("n", "<leader>f", function() Snacks.picker.files() end, { desc = "Find files" })
vim.keymap.set("n", "<leader>/", function() Snacks.picker.grep() end, { desc = "Grep" })
vim.keymap.set("n", "<leader>*", function() Snacks.picker.grep_word() end, { desc = "Grep cword" })
vim.keymap.set("n", "<leader>b", function() Snacks.picker.buffers() end, { desc = "Buffers" })
vim.keymap.set("n", "<leader>?", function() Snacks.picker.help() end, { desc = "Help" })
vim.keymap.set("n", "<leader>j", function() Snacks.picker.jumps() end, { desc = "Jumps" })
vim.keymap.set("n", "<leader>'", function() Snacks.picker.marks() end, { desc = "Marks" })
vim.keymap.set("n", '<leader>"', function() Snacks.picker.registers() end, { desc = "Registers" })

local gitsigns = require('gitsigns')
gitsigns.setup({
  preview_config = {
    border = border,
  },
})

vim.keymap.set("n", "<leader>gg", function() Snacks.lazygit.open() end, { desc = "Lazygit" })
vim.keymap.set("n", "<leader>gd", function() Snacks.picker.git_diff() end, { desc = "Diff" })
vim.keymap.set("n", "<leader>gl", function() Snacks.picker.git_log() end, { desc = "Log" })
vim.keymap.set("n", "<leader>gL", function() Snacks.picker.git_log_file() end, { desc = "Log buffer" })
vim.keymap.set("n", "<leader>gs", function() Snacks.picker.git_status() end, { desc = "Status" })
vim.keymap.set("n", "<leader>gb", function() Snacks.gitbrowse.open() end, { desc = "Browse" })
vim.keymap.set("n", "<leader>gB", gitsigns.toggle_current_line_blame, { desc = "Blame" })
vim.keymap.set("n", "<leader>gh", gitsigns.preview_hunk, { desc = "Preview Hunk" })
vim.keymap.set("n", "]h", function() gitsigns.nav_hunk('next') end, { desc = "Next Hunk" })
vim.keymap.set("n", "[h", function() gitsigns.nav_hunk('prev') end, { desc = "Previous Hunk" })


local render_markdown = require('render-markdown')
render_markdown.setup({})
render_markdown.disable()
require('image').setup({ backend = 'sixel' })

require("tree-sitter-manager").setup({
  ensure_installed = tree_sitters,
  border = border,
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
for server, config in pairs(language_servers) do
  config.on_attach = on_attach
  if config.capabilities then
    config.capabilities = vim.tbl_deep_extend("force", capabilities, config.capabilities)
  else
    config.capabilities = capabilities
  end
  vim.lsp.config(server, config)
  vim.lsp.enable(server)
end

require("mini.ai").setup()
require("mini.bracketed").setup()
require("mini.pairs").setup()
require("mini.surround").setup()

local gen_loader = require("mini.snippets").gen_loader
local MiniSnippets = require("mini.snippets")
MiniSnippets.setup({
  snippets = {
    gen_loader.from_lang(),
  }
})
MiniSnippets.start_lsp_server({ match = false })

require("mini.move").setup({
  mappings = {
    left = "<M-,>",
    right = "<M-.>",
    down = "<M-d>",
    up = "<M-u>",
    line_left = "<M-,>",
    line_right = "<M-.>",
    line_down = "<M-d>",
    line_up = "<M-u>",
  },
})

require('todo-comments').setup()
vim.keymap.set("n", "<leader>t", function() Snacks.picker.todo_comments({ keywords = { "TODO", "FIX", "FIXME" } }) end,
  { desc = "Todo comments" })

require("mini.completion").setup({
  window = {
    info = { border = border },
    signature = { border = border },
  },
})
local wk = require('which-key')
wk.setup({
  delay = 0,
  win = {
    border = border,
  },
  plugins = {
    marks = true,
    registers = true,
    spelling = { enabled = false },
    presets = {
      operators = true,
      motions = true,
      text_objects = true,
      windows = true,
      nav = true,
      z = true,
      g = true,
    },
  },
})

wk.add({
  { '<leader>g',  group = 'Git',      mode = { 'n', 'x' } },
  { '<leader>n',  group = 'Neovim',   mode = { 'n', 'x' } },
  { '<leader>l',  group = 'Lsp',      mode = { 'n', 'x' } },
  { '<leader>s',  group = 'Session',  mode = { 'n', 'x' } },
  { '<leader>gh', group = 'Hunks',    mode = { 'n', 'x' } },
  { '<leader>gd', group = 'Diff',     mode = { 'n', 'x' } },
  { 's',          group = 'Surround', mode = { 'n', 'x' } },
})

vim.keymap.set('n', "<Esc>", "<cmd>nohlsearch<CR>", { silent = true })
vim.keymap.set('n', "<Tab>", "<cmd>bnext<CR>", { desc = "Next buffer" })
vim.keymap.set('n', "<S-Tab>", "<cmd>bprevious<CR>", { desc = "Previous buffer" })
vim.keymap.set('n', "<C-c>", "<cmd>bdelete<CR>", { desc = "Close buffer" })
vim.keymap.set('n', "<C-s>", "<cmd>write<CR>", { desc = "Save buffer" })
vim.keymap.set({ 'n', 'x' }, "P", '"+P', { desc = "Paste before" })
vim.keymap.set({ 'n', 'x' }, "p", '"+p', { desc = "Paste after" })
vim.keymap.set('n', "<leader>r", ":%s/<C-r><C-w>//gc<Left><Left><Left>", { desc = "Replace" })
vim.keymap.set('v', "<leader>r", ":s/<C-r><C-w>//gc<Left><Left><Left>", { desc = "Replace" })
vim.keymap.set('i', "<C-h>", "<Left>", { noremap = true })
vim.keymap.set('i', "<C-j>", "<Down>", { noremap = true })
vim.keymap.set('i', "<C-k>", "<Up>", { noremap = true })
vim.keymap.set('i', "<C-l>", "<Right>", { noremap = true })
vim.keymap.set('n', '<M-z>', '<cmd>suspend<CR>', { noremap = true })
vim.keymap.set('t', '<S-Esc>', '<C-\\><C-n>', { noremap = true })

vim.keymap.set('n', "<leader>nc", "<cmd>e " .. vim.fn.resolve(vim.fn.expand("~/.config/nvim/init.lua")) .. "<CR>",
  { desc = "Config" })
vim.keymap.set('n', "<leader>nu", "<cmd>lua vim.pack.update()<CR>", { desc = "Update plugins" })
vim.keymap.set('n', "<leader>nr", "<cmd>source " .. vim.fn.resolve(vim.fn.expand('~/.config/nvim/init.lua')) .. "<CR>",
  { desc = "Reload" })

vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  group = "YankHighlight",
  callback = function() vim.highlight.on_yank({ timeout = 500 }) end,
})

-- convert from vimscript to lua https://neovim.io/doc/user/autocmd.html#ModeChanged
vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "[vV\x16]*:*",
  callback = function() vim.opt_local.relativenumber = vim.fn.mode():match("^[vV\x16]") ~= nil end,
})

vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "*:[vV\x16]*",
  callback = function() vim.opt_local.relativenumber = vim.fn.mode():match("^[vV\x16]") ~= nil end,
})

vim.api.nvim_create_autocmd({ "WinEnter", "WinLeave" }, {
  callback = function() vim.opt_local.relativenumber = vim.fn.mode():match("^[vV\x16]") ~= nil end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "help", "man" },
  callback = function() vim.cmd("wincmd L") end,
})

vim.diagnostic.config({
  float = false,
  severity_sort = true,
  virtual_text = { current_line = true },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "󰅙",
      [vim.diagnostic.severity.WARN] = "",
      [vim.diagnostic.severity.INFO] = "󰋼",
      [vim.diagnostic.severity.HINT] = "",
    },
  },
})

vim.api.nvim_create_augroup("AutoQF", { clear = true })
vim.api.nvim_create_autocmd("WinLeave", {
  group = "AutoQF",
  pattern = "*",
  callback = function()
    if vim.bo.filetype == 'qf' and vim.fn.pumvisible() == 0 then
      vim.cmd('lclose')
      vim.cmd('cclose')
    end
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = "AutoQF",
  pattern = "qf",
  callback = function()
    vim.keymap.set('n', '<leader>r', ':cdo s/<C-r><C-w>//gc<Left><Left><Left>',
      { desc = 'Replace', noremap = true, buffer = true })
    vim.keymap.set('n', 'q', '<cmd>cclose<CR><cmd>lclose<CR>', { desc = 'close qf', noremap = true, buffer = true })
    vim.keymap.set('n', '<Esc>', '<cmd>cclose<CR><cmd>lclose<CR>', { desc = 'close qf', noremap = true, buffer = true })
  end,
})

vim.api.nvim_create_autocmd("TermOpen", {
  pattern = "*",
  command = "startinsert",
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "gitcommit", "gitrebase" },
  callback = function()
    vim.opt_local.bufhidden = "wipe"
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "snacks_picker_input",
  callback = function()
    vim.b.minicompletion_disable = true
  end,
})

vim.cmd [[
   colorscheme catppuccin "default catppuccin lunaperche habamax miniautumn miniwinter retrobox sorbet unokai wildcharm zaibatsu
   hi Normal guibg=NONE
   hi NormalFloat guibg=NONE
   hi FloatBorder guibg=NONE
   hi PMenu guibg=NONE
   autocmd! nvim.terminal TermClose
]]
