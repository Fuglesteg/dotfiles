local hydra = require("hydra")

hydra({
    name = "Buffer",
    hint = 
    [[
    _n_ -> Next buffer
    _N_ -> Previous buffer
    ]],
    mode = "n",
    body = "<leader>b",
    heads = {
        {"n", ":bnext<cr>", {silent = true}},
        {"N", ":bprev<cr>", {silent = true}}
    },
    config = {silent = true}
})
