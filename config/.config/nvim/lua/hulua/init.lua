local function ZLua()
    vim.api.nvim_command("call fzf#run({'source': 'fasd -dlR', 'sink': 'e', 'down': '40%'})")
end

local function make_call()
    local args = {source='fasd -dlR'}
    local ret = vim.api.nvim_call_dict_function(args, "fzf#run", {args})
    return ret
end

local function ZFLua()
    local lol = make_call()
    print(lol)
    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_attach(buf, {}, {})
    local mode = vim.api.nvim_get_mode()
    print(mode["mode"])
    print(mode["blocking"])
    print("Setting cwd to: ")
    print(buf)
    print(res)
    -- vim.api.nvim_command(string.format("cd %s", ret))
    -- vim.api.nvim_command(":Files .")
end

local function DockerBuildL(tag)
    local cmd = string.format(':vsp | :te docker build -t %s .', tag)
    vim.api.nvim_command(cmd)
    print("hello")
end

local function DockerCp(file, container)
    -- would be good to do this
end

return {
    ZLua =  ZLua,
    ZFLua = ZFLua,
    DockerBuildL = DockerBuildL
}
