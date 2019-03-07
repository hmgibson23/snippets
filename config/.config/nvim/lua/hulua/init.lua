local function ZLua()
    vim.api.nvim_command("call fzf#run({'source': 'fasd -dlR', 'sink': 'e', 'right': '40%'})")
end

local function DockerBuildL(tag)
    local cmd = string.format('docker build -t %s', tag)
    print("Running: ", cmd)
    local file = assert(io.popen(cmd, 'r'))
    local output = file:read('*all')
    file:close()
    vim.api.nvim_out_write(output)
end

local function DockerCp(file, container)
    -- would be good to do this
end

return {
    ZLua =  ZLua,
    DockerBuildL = DockerBuildL
}
