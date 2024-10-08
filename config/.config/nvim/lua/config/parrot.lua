local M = {}
local parrot = require("parrot")

function M.setup()
	parrot.setup({
		providers = {
			groq = {
				api_key = os.getenv("GROQ_API_KEY"),
			},
			gemini = {
				api_key = os.getenv("GEMINI_API_KEY"),
			},
		},
		chat_dir = vim.loop.fs_realpath(vim.fn.stdpath("data"):gsub("/$", "") .. "/parrot/chats"),
		hooks = {
			Complete = function(prt, params)
				local template = [[
        I have the following code from {{filename}}:

        ```{{filetype}}
        {{selection}}
        ```

        Please finish the code above carefully and logically.
        Respond just with the snippet of code that should be inserted."
        ]]
				local model_obj = prt.get_model("command")
				prt.Prompt(params, prt.ui.Target.append, model_obj, nil, template)
			end,
			Ask = function(parrot, params)
				local template = [[
          In light of your existing knowledge base, please generate a response that
          is succinct and directly addresses the question posed. Prioritize accuracy
          and relevance in your answer, drawing upon the most recent information
          available to you. Aim to deliver your response in a concise manner,
          focusing on the essence of the inquiry.
          Question: {{command}}
        ]]
				local model_obj = parrot.get_model("command")
				parrot.logger.info("Asking model: " .. model_obj.name)
				parrot.Prompt(params, parrot.ui.Target.popup, model_obj, "🤖 Ask ~ ", template)
			end,
			CompleteFullContext = function(prt, params)
				local template = [[
        I have the following code from {{filename}}:

        ```{{filetype}}
        {filecontent}}
        ```

        Please look at the following section specifically:
        ```{{filetype}}
        {{selection}}
        ```

        Please finish the code above carefully and logically.
        Respond just with the snippet of code that should be inserted.
        ]]
				local model_obj = prt.get_model("command")
				prt.Prompt(params, prt.ui.Target.append, model_obj, nil, template)
			end,
			CompleteMultiContext = function(prt, params)
				local template = [[
        I have the following code from {{filename}} and other realted files:

        ```{{filetype}}
        {{multifilecontent}}
        ```

        Please look at the following section specifically:
        ```{{filetype}}
        {{selection}}
        ```

        Please finish the code above carefully and logically.
        Respond just with the snippet of code that should be inserted.
        ]]
				local model_obj = prt.get_model("command")
				prt.Prompt(params, prt.ui.Target.append, model_obj, nil, template)
			end,
			FixBugs = function(prt, params)
				local template = [[
        You are an expert in {{filetype}}.
        Fix bugs in the below code from {{filename}} carefully and logically:
        Your task is to analyze the provided {{filetype}} code snippet, identify
        any bugs or errors present, and provide a corrected version of the code
        that resolves these issues. Explain the problems you found in the
        original code and how your fixes address them. The corrected code should
        be functional, efficient, and adhere to best practices in
        {{filetype}} programming.

        ```{{filetype}}
        {{selection}}
        ```

        Fixed code:
        ]]
				local model_obj = prt.get_model("command")
				prt.logger.info("Fixing bugs in selection with model: " .. model_obj.name)
				prt.Prompt(params, prt.ui.Target.new, model_obj, nil, template)
			end,
			UnitTests = function(prt, params)
				local template = [[
        I have the following code from {{filename}}:

        ```{{filetype}}
        {{selection}}
        ```

        Please respond by writing table driven unit tests for the code above.
        ]]
				local model_obj = prt.get_model("command")
				prt.logger.info("Creating unit tests for selection with model: " .. model_obj.name)
				prt.Prompt(params, prt.ui.Target.enew, model_obj, nil, template)
			end,
			Debug = function(prt, params)
				local template = [[
        I want you to act as {{filetype}} expert.
        Review the following code, carefully examine it, and report potential
        bugs and edge cases alongside solutions to resolve them.
        Keep your explanation short and to the point:

        ```{{filetype}}
        {{selection}}
        ```
        ]]
				local model_obj = prt.get_model("command")
				prt.logger.info("Debugging selection with model: " .. model_obj.name)
				prt.Prompt(params, prt.ui.Target.enew, model_obj, nil, template)
			end,
		},
	})
	local function parrot_status()
		local status_info = require("parrot.config").get_status_info()
		local status = ""
		if status_info.is_chat then
			status = status_info.prov.chat.name
		else
			status = status_info.prov.command.name
		end
		return string.format("%s(%s)", status, status_info.model)
	end
end

return M
