vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.env.NVIM = vim.v.servername

local path_package = vim.fn.stdpath("data") .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/echasnovski/mini.nvim",
    mini_path,
  }
  vim.fn.system(clone_cmd)
  vim.cmd("packadd mini.nvim | helptags ALL")
  vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

local MiniDeps = require("mini.deps")
MiniDeps.setup({ path = { package = path_package } })
local add = MiniDeps.add
local border = "rounded"

add({ source = "catppuccin/nvim", name = "catppuccin" })

require("catppuccin").setup({
  auto_integrations = true,
  flavour = "mocha",
  transparent_background = true,
  float = {
    transparent = true,
    solid = true,
  },
  integrations = {
    blink_cmp = {
      style = 'bordered',
    },
    gitsigns = true,
    mini = { enabled = true, indentscope_color = "" },
    which_key = true,
    notify = true,
    todo_comments = true,
    snacks = {
      enabled = true,
      indent_scope_color = "",
    },
    render_markdown = true,
  },
})

vim.cmd.colorscheme("catppuccin")

vim.g.netrw_browsex_viewer = "xdg-open"
vim.g.netrw_banner = 1
vim.g.netrw_liststyle = 1
vim.g.netrw_preview = 1
vim.g.netrw_keepdir = 0
vim.g.netrw_localcopydircmd = 'cp -r'

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
vim.opt.completeopt = "menuone,noinsert,noselect"
vim.opt.cursorline = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true
vim.opt.listchars = { tab = "▸ ", extends = "…", precedes = "…", nbsp = "␣", trail = "·" }
vim.opt.list = true
vim.opt.laststatus = 3
vim.opt.expandtab = true
vim.opt.confirm = false
vim.opt.showmode = false
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.shortmess = vim.opt.shortmess:append("WcC")
vim.opt.pumheight = 10
vim.opt.pumblend = 0
vim.opt.winblend = 0
vim.opt.virtualedit = "block"
vim.opt.fillchars = { eob = " " }
vim.opt.splitkeep = "screen"
vim.opt.grepprg = 'rg --vimgrep -.'
vim.opt.background = 'dark'
vim.opt.winborder = border
vim.opt.path:append("**")

local languages = {
  bash = { lsp = { bashls = { config = {}, bin = "bash-language-server" } }, ts = { "bash" } },
  c = { lsp = { clangd = { config = {}, bin = "clangd" } }, ts = { "c" } },
  c3 = { lsp = { c3_lsp = { config = {} } }, ts = { "c3" } },
  c_sharp = { lsp = { omnisharp = { config = {}, bin = "omnisharp" } }, ts = { "c_sharp" } },
  css = {
    lsp = {
      cssls = { config = {}, bin = "css-lsp" },
      tailwindcss = { config = {}, bin = "tailwindcss-language-server" },
    },
    ts = { "css" },
  },
  dart = { ts = { "dart" } },
  dockerfile = {
    lsp = {
      docker_compose_language_service = { config = {}, bin = "docker-compose-language-service" },
      dockerls = { config = {}, bin = "dockerfile-language-server" },
    },
    ts = { "dockerfile" },
  },
  elixir = {
    lsp = {
      expert = {
        config = {
          settings = {
            workspaceSymbols = {
              minQueryLength = 0
            }
          }
        },
      },
    },
    ts = { "elixir", "eex", "heex" },
  },
  emmet = {
    lsp = {
      emmet_language_server = {
        config = {
          filetypes = { "css", "eruby", "gohtml", "heex", "html", "javascript", "javascriptreact", "less", "php", "pug", "sass", "scss", "templ", "typescriptreact" },
        },
        bin = "emmet-language-server",
      },
    },
  },
  go = {
    lsp = {
      gopls = { config = {}, bin = "gopls" },
      goimports = { config = {}, bin = "goimports" },
      golangci_lint = { config = {}, bin = "golangci-lint" },
      golangci_lint_ls = { config = {}, bin = "golangci-lint-langserver" },
    },
    ts = { "go", "gomod", "gosum", "gotmpl", "gowork" },
  },
  graphql = { lsp = { graphql = { config = {}, bin = "graphql-language-service-cli" } }, ts = { "graphql" } },
  html = {
    lsp = { html = { config = {}, bin = "html-lsp" }, htmx = { config = {}, bin = "htmx-lsp" } },
    ts = { "html", "htmldjango" },
  },
  java = { lsp = { jdtls = { config = {}, bin = "jdtls" } }, ts = { "java" } },
  javascript = { lsp = { ts_ls = { config = {}, bin = "typescript-language-server" } }, ts = { "javascript" } },
  json = {
    lsp = { jsonls = { config = {}, bin = "json-lsp" } },
    ts = { "json", "json5", "jsonnet" },
  },
  lua = {
    lsp = {
      lua_ls = {
        config = {
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
        bin = "lua-language-server",
      },
    },
    ts = { "lua", "luadoc" },
  },
  markdown = {
    lsp = {
      markdown_oxide = {
        config = {
          capabilities = {
            workspace = {
              didChangeWatchedFiles = {
                dynamicRegistration = true,
              },
            },
          },
        },
        bin = "markdown-oxide",
      },
    },
    ts = { "markdown", "markdown_inline" },
  },
  nix = { ts = { "nix" } },
  perl = { lsp = { perlpls = { config = {}, bin = "pls" } }, ts = { "perl" } },
  php = { lsp = { phpactor = { config = {}, bin = "phpactor" } }, ts = { "php", "php_only" } },
  proto = { lsp = { buf_ls = { config = {}, bin = "buf" } }, ts = { "proto" } },
  python = {
    lsp = {
      ruff =
      {
        config = {
        },
        bin = "ruff"
      },
      basedpyright = {
        config = {
          settings = {
            basedpyright = {
              analysis = {
                typeCheckingMode = "off",
              },
            },
          },
        },
        bin = "basedpyright"
      }
    },
    ts = { "python" }
  },
  ruby = {
    lsp = { rubocop = { config = {}, bin = "rubocop" }, ruby_lsp = { config = {}, bin = "ruby-lsp" } },
    ts = { "ruby" },
  },
  rust = { lsp = { rust_analyzer = { config = {}, bin = "rust-analyzer" } }, ts = { "rust" } },
  scss = { ts = { "scss" } },
  sql = { lsp = { sqls = { config = {}, bin = "sqls" } }, ts = { "sql" } },
  svelte = { lsp = { svelte = { config = {}, bin = "svelte-language-server" } }, ts = { "svelte" } },
  templ = { lsp = { templ = { config = {}, bin = "templ" } }, ts = { "templ" } },
  terraform = { lsp = { terraformls = { config = {}, bin = "terraform-ls" } }, ts = { "terraform", "hcl" } },
  toml = { lsp = { tombi = { config = {}, bin = "tombi" } }, ts = { "toml" } },
  typescript = { lsp = { ts_ls = { config = {}, bin = "typescript-language-server" } }, ts = { "typescript", "tsx" } },
  twig = { ts = { "twig" } },
  vim = { ts = { "vim", "vimdoc", "query" } },
  yaml = {
    lsp = { yamlls = { config = {}, bin = "yaml-language-server" }, ansiblels = { config = {}, bin = "ansible-language-server" } },
    ts = { "yaml" },
  },
  zig = { lsp = { zls = { config = {}, bin = "zls" } }, ts = { "zig" } },
  diff = { ts = { "diff" } },
  embedded_template = { ts = { "embedded_template" } },
  git = { ts = { "git_config", "git_rebase", "gitattributes", "gitcommit", "gitignore" } },
  http = { ts = { "http" } },
}

local tree_sitters = {}
local language_servers = {}
local mason_binaries = {}

for _, lang_config in pairs(languages) do
  -- Extract tree-sitter parsers
  if lang_config.ts then
    for _, parser in ipairs(lang_config.ts) do
      table.insert(tree_sitters, parser)
    end
  end

  -- Extract LSP servers and their binaries
  if lang_config.lsp then
    for server_name, server_data in pairs(lang_config.lsp) do
      -- Extract LSP configurations
      if server_data.config then
        language_servers[server_name] = server_data.config
      end
      -- Extract Mason binaries
      if server_data.bin then
        table.insert(mason_binaries, server_data.bin)
      end
    end
  end
end

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

add({
  source = 'folke/snacks.nvim',
  depends = { 'folke/todo-comments.nvim', 'nvim-lua/plenary.nvim' },
})

require('todo-comments').setup()
local Snacks = require('snacks')

Snacks.setup({
  animate = {
    enabled = true,
    animate = { enabled = true },
    filter = function(buf)
      return vim.g.snacks_dim ~= false and vim.b[buf].snacks_dim ~= false and vim.bo[buf].buftype == ""
    end,
  },
  picker = { enabled = true, ui_select = true },
  explorer = { enabled = true, replace_netrw = true },
  notifier = { enabled = true },
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

add({ source = 'nvim-lualine/lualine.nvim' })

require('lualine').setup({
  options = {
    theme = 'catppuccin-nvim',
    globalstatus = true,
  },
  tabline = {
    lualine_a = { 'buffers' },
  },
})

vim.keymap.set('n', '-', '<cmd>lua Snacks.explorer.open()<CR>', { desc = 'Explorer' })

require("mini.sessions").setup({
  directory = "",
  file = ".session.vim",
})
vim.keymap.set("n", "<leader>ss", '<cmd>lua MiniSessions.write(".session.vim")<CR>', { desc = "Save" })
vim.keymap.set("n", "<leader>sl", '<cmd>lua MiniSessions.read(".session.vim")<CR>', { desc = "Load" })
vim.keymap.set("n", "<leader>sd", '<cmd>lua MiniSessions.delete(".session.vim")<CR>', { desc = "Delete" })

vim.keymap.set("n", "<leader>f", function() Snacks.picker.files() end, { desc = "Find files" })
vim.keymap.set("n", "<leader>/", function() Snacks.picker.grep() end, { desc = "Grep" })
vim.keymap.set("n", "<leader>*", function() Snacks.picker.grep_word() end, { desc = "Grep cword" })
vim.keymap.set("n", "<leader>b", function() Snacks.picker.buffers() end, { desc = "Buffers" })
vim.keymap.set("n", "<leader>?", function() Snacks.picker.help() end, { desc = "Help" })
vim.keymap.set("n", "<leader>j", function() Snacks.picker.jumps() end, { desc = "Jumps" })
vim.keymap.set("n", "<leader>'", function() Snacks.picker.marks() end, { desc = "Marks" })
vim.keymap.set("n", '<leader>"', function() Snacks.picker.registers() end, { desc = "Registers" })

add('lewis6991/gitsigns.nvim')
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

local function build_blink(params)
  vim.notify('Building blink.cmp', vim.log.levels.INFO)
  local obj = vim.system({ 'cargo', 'build', '--release' }, { cwd = params.path }):wait()
  if obj.code == 0 then
    vim.notify('Building blink.cmp done', vim.log.levels.INFO)
  else
    vim.notify('Building blink.cmp failed', vim.log.levels.ERROR)
  end
end

add({
  source = 'saghen/blink.cmp',
  depends = { "rafamadriz/friendly-snippets", "zbirenbaum/copilot.lua", "giuxtaposition/blink-cmp-copilot" },
  hooks = {
    post_install = build_blink,
    post_checkout = build_blink,
  },
})

require("copilot").setup({
  suggestion = { enabled = false },
  panel = { enabled = false },
})

require('blink.cmp').setup({
  keymap = { preset = 'super-tab' },
  completion = {
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 500,
      window = { border = border }
    },
    menu = { border = border }
  },
  signature = {
    enabled = true,
    window = { border = border }
  },
  sources = {
    default = { 'lsp', 'path', 'snippets', 'buffer', 'copilot' },
    providers = {
      copilot = {
        name = "copilot",
        module = "blink-cmp-copilot",
        score_offset = 100,
        async = true,
      },
    },
  },
  fuzzy = {
    implementation = "prefer_rust_with_warning"
  }
})

add({
  source = "nvim-treesitter/nvim-treesitter",
  checkout = "main",
  monitor = "main",
  hooks = {
    post_checkout = function()
      vim.cmd("TSUpdate")
    end,
  }
})

require 'nvim-treesitter'.setup {
  install_dir = vim.fn.stdpath('data') .. '/site'
}

require 'nvim-treesitter'.install(tree_sitters)

vim.api.nvim_create_autocmd('FileType', {
  pattern = tree_sitters,
  callback = function()
    vim.treesitter.start()
  end
})

add({ source = "neovim/nvim-lspconfig", depends = { 'saghen/blink.cmp' } })

local capabilities = require('blink.cmp').get_lsp_capabilities(vim.lsp.protocol.make_client_capabilities())

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

vim.keymap.set("n", "<leader>t", function() Snacks.picker.todo_comments({ keywords = { "TODO", "FIX", "FIXME" } }) end,
  { desc = "Todo comments" })

add({ source = 'folke/which-key.nvim' })

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

add('MeanderingProgrammer/render-markdown.nvim')
require('render-markdown').setup()
add({ source = 'obsidian-nvim/obsidian.nvim', version = '*' })
require('obsidian').setup({
  legacy_commands = false,
  workspaces = {
    { name = 'notes', path = '~/notes' },
  },
  completion = { blink = true },
})


-- nvim
vim.keymap.set('n', "<Esc>", "<cmd>nohlsearch<CR>", { silent = true })
vim.keymap.set('n', "<Tab>", "<cmd>bnext<CR>", { desc = "Next buffer" })
vim.keymap.set('n', "<S-Tab>", "<cmd>bprevious<CR>", { desc = "Previous buffer" })
vim.keymap.set('n', "<C-c>", "<cmd>bdelete<CR>", { desc = "Close buffer" })
vim.keymap.set('n', "<C-s>", "<cmd>write<CR>", { desc = "Save buffer" })
vim.keymap.set('v', "p", "P", { desc = "Paste before" })
vim.keymap.set('n', "<leader>r", ":%s/<C-r><C-w>//gc<Left><Left><Left>", { desc = "Replace" })
vim.keymap.set('v', "<leader>r", ":s/<C-r><C-w>//gc<Left><Left><Left>", { desc = "Replace" })
vim.keymap.set('i', "<C-h>", "<Left>", { noremap = true })
vim.keymap.set('i', "<C-j>", "<Down>", { noremap = true })
vim.keymap.set('i', "<C-k>", "<Up>", { noremap = true })
vim.keymap.set('i', "<C-l>", "<Right>", { noremap = true })
vim.keymap.set('n', '<M-z>', ':suspend<CR>', { noremap = true })

-- nvim config
vim.keymap.set('n', "<leader>nc", "<cmd>e " .. vim.fn.resolve(vim.fn.expand("~/.config/nvim/init.lua")) .. "<CR>",
  { desc = "Config" })
vim.keymap.set('n', "<leader>nu", "<cmd>DepsUpdate!<CR>", { desc = "Update plugins" })
vim.keymap.set('n', "<leader>np", "<cmd>DepsClean!<CR>", { desc = "Prune plugins" })
vim.keymap.set('n', "<leader>ns", "<cmd>DepsShowLog<CR>", { desc = "Show plugins log" })
vim.keymap.set('n', "<leader>nr", "<cmd>source " .. vim.fn.resolve(vim.fn.expand('~/.config/nvim/init.lua')) .. "<CR>",
  { desc = "Reload" })

-- nvim improvements
vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  group = "YankHighlight",
  callback = function()
    vim.highlight.on_yank({ timeout = 500 })
  end,
})

-- convert from vimscript to lua https://neovim.io/doc/user/autocmd.html#ModeChanged
vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "[vV\x16]*:*",
  callback = function()
    vim.opt_local.relativenumber = vim.fn.mode():match("^[vV\x16]") ~= nil
  end,
})

vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "*:[vV\x16]*",
  callback = function()
    vim.opt_local.relativenumber = vim.fn.mode():match("^[vV\x16]") ~= nil
  end,
})

vim.api.nvim_create_autocmd({ "WinEnter", "WinLeave" }, {
  callback = function()
    vim.opt_local.relativenumber = vim.fn.mode():match("^[vV\x16]") ~= nil
  end,
})

-- man and help split to the right
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "help", "man" },
  callback = function()
    vim.cmd("wincmd L")
  end,
})

-- diagnostics
vim.diagnostic.config({
  float = false,
  severity_sort = true,
  virtual_text = {
    current_line = true,
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "󰅙",
      [vim.diagnostic.severity.WARN] = "",
      [vim.diagnostic.severity.INFO] = "󰋼",
      [vim.diagnostic.severity.HINT] = "",
    },
  },
})

-- Auto close quickfix or location list
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

-- Keymap only for quickfix or location list
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

-- Clipboard with OSC 52
vim.g.clipboard = {
  name = 'OSC 52',
  copy = {
    ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
    ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
  },
  paste = {
    ['+'] = require('vim.ui.clipboard.osc52').paste('+'),
    ['*'] = require('vim.ui.clipboard.osc52').paste('*'),
  },
}
