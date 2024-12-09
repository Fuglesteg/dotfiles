return {
  "epwalsh/obsidian.nvim",
  version = "*",  -- Use latest release instead of latest commit
  lazy = true,
  ft = "markdown",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    disable_frontmatter = true,
    note_path_func = function(spec)
      local path = spec.dir / spec.title
      return path:with_suffix(".md")
    end,
    note_id_func = function(title)
      return title
    end,
    workspaces = {
      {
        name = "Drive",
        path = "~/Drive",
      }
    },
    follow_url_func = function(url)
      vim.fn.jobstart({"firefox", url})
    end,
  },
}
