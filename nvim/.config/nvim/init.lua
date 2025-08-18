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
vim.opt.grepprg = 'rg'
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
      elixirls = {
        config = {
          cmd = { "elixir-ls" },
        },
        bin = "elixir-ls",
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
    ts = { "json", "json5", "jsonc", "jsonnet" },
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
  toml = { lsp = { taplo = { config = {}, bin = "taplo" } }, ts = { "toml" } },
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
  -- lsp actions
  vim.keymap.set("n", "<leader>ln", vim.lsp.buf.rename, { buffer = bufnr, desc = "Rename symbol" })
  vim.keymap.set({ "n", "v" }, "<leader>lf", vim.lsp.buf.format, { buffer = bufnr, desc = "Format code" })
  vim.keymap.set({ "n", "v" }, "<leader>la", vim.lsp.buf.code_action, { buffer = bufnr, desc = "Code actions" })
  vim.keymap.set("n", "<leader>lh", vim.lsp.buf.signature_help, { buffer = bufnr, desc = "Signature help" })

  -- lsp pickers
  vim.keymap.set("n", "<leader>ld", '<cmd>Pick diagnostic scope="current"<CR>',
    { buffer = bufnr, desc = "Document diagnostics" })
  vim.keymap.set("n", "<leader>lD", '<cmd>Pick diagnostic scope="all"<CR>',
    { buffer = bufnr, desc = "Workspace diagnostics" })
  vim.keymap.set("n", "<leader>ls", '<cmd>Pick lsp scope="document_symbol"<CR>',
    { buffer = bufnr, desc = "Document symbols" })
  vim.keymap.set("n", "<leader>lS", '<cmd>Pick lsp scope="workspace_symbol"<CR>',
    { buffer = bufnr, desc = "Workspace symbols" })
  vim.keymap.set("n", "<leader>lr", '<cmd>Pick lsp scope="references"<CR>', { buffer = bufnr, desc = "References" })
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

require("mini.statusline").setup({
  use_icons = true,
})

local MiniTabline = require("mini.tabline")
MiniTabline.setup({
  show_icons = true,
  format = function(buf_id, label)
    local suffix = vim.bo[buf_id].modified and "+ " or ""
    return MiniTabline.default_format(buf_id, label) .. suffix
  end,
})

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

require("mini.sessions").setup({
  directory = "",
  file = ".session.vim",
})
vim.keymap.set("n", "<leader>ss", '<cmd>lua MiniSessions.write(".session.vim")<CR>', { desc = "Save" })
vim.keymap.set("n", "<leader>sl", '<cmd>lua MiniSessions.read(".session.vim")<CR>', { desc = "Load" })
vim.keymap.set("n", "<leader>sd", '<cmd>lua MiniSessions.delete(".session.vim")<CR>', { desc = "Delete" })

local MiniPick = require("mini.pick")
MiniPick.setup({
  window = {
    config = { border = border },
  },
})
vim.ui.select = MiniPick.ui_select
vim.keymap.set("n", "<leader>f", "<cmd>Pick files<CR>", { desc = "Find files" })
vim.keymap.set("n", "<leader>/", "<cmd>Pick grep_live<CR>", { desc = "Grep" })
vim.keymap.set("n", "<leader>*", '<cmd>Pick grep pattern="<cword>"<CR>', { desc = "Grep cword" })
vim.keymap.set("n", "<leader>b", "<cmd>Pick buffers<CR>", { desc = "Buffers" })
vim.keymap.set("n", "<leader>?", "<cmd>Pick help<CR>", { desc = "Help" })
vim.keymap.set("n", "<leader>j", '<cmd>Pick list scope="jump"<CR>', { desc = "Jumps" })
vim.keymap.set("n", "<leader>'", "<cmd>Pick marks<CR>", { desc = "Marks" })
vim.keymap.set("n", '<leader>"', "<cmd>Pick registers<CR>", { desc = "Registers" })

add("github/copilot.vim")

add("christoomey/vim-tmux-navigator")

require("mini.git").setup()
require('mini.diff').setup({
  view = { style = "sign" }
})

vim.keymap.set("n", '<leader>gd', '<cmd>Git diff HEAD<CR>', { desc = 'Diff from HEAD' })
vim.keymap.set("n", '<leader>gu', '<cmd>Git diff<CR>', { desc = 'Unstaged lines' })
vim.keymap.set("n", '<leader>gs', '<cmd>Git diff --cached<CR>', { desc = 'Staged lines' })
vim.keymap.set('n', '<leader>gl', '<cmd>Git log --decorate --graph --all --oneline<CR>', { desc = 'Log' })
vim.keymap.set('n', '<leader>gf', '<cmd>Git log --decorate --graph --all --oneline %<CR>', { desc = 'File history' })
vim.keymap.set("n", '<leader>go', '<cmd>lua MiniDiff.toggle_overlay()<CR>', { desc = 'Overlay Hunks' })
vim.keymap.set("n", '<leader>gi', '<cmd>lua MiniGit.show_at_cursor()<CR>', { desc = 'Inspect' })

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "git", "diff" },
  callback = function()
    vim.opt_local.foldmethod = "expr"
    vim.opt_local.foldexpr = "v:lua.MiniGit.diff_foldexpr()"
    vim.opt_local.foldlevel = 2
  end,
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

add({ source = "neovim/nvim-lspconfig" })

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
require("mini.extra").setup()

require("mini.jump2d").setup()
vim.keymap.set('n', '<CR>', '<Cmd>lua MiniJump2d.start(MiniJump2d.builtin_opts.single_character)<CR>')

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

require("mini.hipatterns").setup({
  highlighters = {
    FIXME = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
    HACK = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
    TODO = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
    NOTE = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
  },
})

vim.keymap.set("n", "<leader>t", "<cmd>Pick hipatterns<CR>", { desc = "Todo comments" })

require("mini.completion").setup({
  window = {
    info = { border = border },
    signature = { border = border },
  },
})

local MiniIndentScope = require("mini.indentscope")
MiniIndentScope.setup({
  symbol = '│',
  options = {
    indent_at_cursor = false,
    draw = {
      animation = MiniIndentScope.gen_animation.none,
      delay = 100,
    },
  },
})

local miniclue = require('mini.clue')
miniclue.setup({
  triggers = {
    -- Leader triggers
    { mode = 'n', keys = '<Leader>' },
    { mode = 'x', keys = '<Leader>' },
    -- Built-in completion
    { mode = 'i', keys = '<C-x>' },
    -- `g` key
    { mode = 'n', keys = 'g' },
    { mode = 'x', keys = 'g' },
    -- Marks
    { mode = 'n', keys = "'" },
    { mode = 'n', keys = '`' },
    { mode = 'x', keys = "'" },
    { mode = 'x', keys = '`' },
    -- Registers
    { mode = 'n', keys = '"' },
    { mode = 'x', keys = '"' },
    { mode = 'i', keys = '<C-r>' },
    { mode = 'c', keys = '<C-r>' },
    -- Window commands
    { mode = 'n', keys = '<C-w>' },
    -- `z` key
    { mode = 'n', keys = 'z' },
    { mode = 'x', keys = 'z' },
    -- mini.surround
    { mode = 'n', keys = 's' },
    { mode = 'x', keys = 's' },
    -- mini.bracketed
    { mode = 'n', keys = '[' },
    { mode = 'x', keys = '[' },
    { mode = 'n', keys = ']' },
    { mode = 'x', keys = ']' },
  },
  clues = {
    miniclue.gen_clues.builtin_completion(),
    miniclue.gen_clues.g(),
    miniclue.gen_clues.marks(),
    miniclue.gen_clues.registers(),
    miniclue.gen_clues.windows(),
    miniclue.gen_clues.z(),

    { mode = 'n', keys = '<leader>g', desc = '+Git' },
    { mode = 'x', keys = '<leader>g', desc = '+Git' },
    { mode = 'n', keys = '<leader>n', desc = '+Neovim' },
    { mode = 'x', keys = '<leader>n', desc = '+Neovim' },
    { mode = 'n', keys = '<leader>l', desc = '+Lsp' },
    { mode = 'x', keys = '<leader>l', desc = '+Lsp' },
    { mode = 'n', keys = '<leader>s', desc = '+Session' },
    { mode = 'x', keys = '<leader>s', desc = '+Session' },
  },
  window = {
    config = {
      border = border,
    },
    delay = 0,
  },
})

-- nvim
vim.keymap.set('n', "<Esc>", "<cmd>nohlsearch<CR>", { silent = true })
vim.keymap.set('n', "<Tab>", "<cmd>bnext<CR>", { desc = "Next buffer" })
vim.keymap.set('n', "<S-Tab>", "<cmd>bprevious<CR>", { desc = "Previous buffer" })
vim.keymap.set('n', "<C-c>", "<cmd>bdelete<CR>", { desc = "Close buffer" })
vim.keymap.set('n', "<C-s>", "<cmd>write<CR>", { desc = "Save buffer" })
vim.keymap.set('v', "p", "P", { desc = "Paste before" })
vim.keymap.set('n', "<leader>r", ":%s///gc", { desc = "Replace" })
vim.keymap.set('v', "<leader>r", ":s///gc", { desc = "Replace" })
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
  virtual_lines = {
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

-- Find command
---@diagnostic disable-next-line: duplicate-set-field
_G.Find = function(filename)
  local cmd = string.format(
    'fd -t f -t l -L -H -i -E "{.git,node_modules,.venv,.elixir_ls}" -g "*%s*" | sed "s/$/:1: /"', filename
  )
  local result = vim.fn.systemlist(cmd)
  vim.fn.setqflist({}, 'r', { title = 'Find Results', lines = result })
  vim.cmd('copen')
end

vim.cmd [[command! -nargs=1 Find lua Find(<q-args>)]]

-- Grep command
---@diagnostic disable-next-line: duplicate-set-field
_G.Grep = function(filename)
  local cmd = string.format('rg %s', filename)
  local result = vim.fn.systemlist(cmd)
  vim.fn.setqflist({}, 'r', { title = 'Grep Results', lines = result })
  vim.cmd('copen')
end

vim.cmd [[command! -nargs=1 Grep lua Grep(<q-args>)]]

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
    vim.keymap.set('n', '<leader>r', ':cdo s///gc', { desc = 'Replace', noremap = true, buffer = true })
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

vim.cmd("colorscheme catppuccin-mocha")
