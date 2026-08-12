local tools = require("plugins.lsp.tools")

local function names(specs)
  local result = {}
  for _, spec in ipairs(specs) do
    table.insert(result, spec.mason)
  end
  return result
end

describe("Mason filetype dependencies", function()
  it("maps Python tools to Python buffers", function()
    assert.are.same({ "black", "isort", "pylint" }, names(tools.for_filetype("python")))
  end)

  it("deduplicates tools shared by Terraform formatter and validator", function()
    assert.are.same({ "terraform", "tfsec" }, names(tools.for_filetype("terraform")))
  end)

  it("maps Go buffers to the Delve debug adapter", function()
    assert.are.same({ "delve" }, names(tools.dap_for_filetype("go")))
  end)
end)
