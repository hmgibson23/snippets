local M = {}

function M.setup()
	local status_ok, hints = pcall(require, "lsp-inlayhints")
	if not status_ok then
		return
	end

	vim.api.nvim_create_augroup("LspAttach_inlayhints", {})
	vim.api.nvim_create_autocmd("LspAttach", {
		group = "LspAttach_inlayhints",
		callback = function(args)
			if not (args.data and args.data.client_id) then
				return
			end

			local client = vim.lsp.get_client_by_id(args.data.client_id)

			if client.server_capabilities.inlayHintProvider then
				hints.on_attach(args.buf, client)
			end
		end,
	})

	hints.setup({
		inlay_hints = {
			parameter_hints = {
				show = false,
				-- prefix = "<- ",
				separator = ", ",
			},
			type_hints = {
				-- type and other hints
				show = true,
				prefix = "",
				separator = ", ",
				remove_colon_end = false,
				remove_colon_start = false,
			},
			labels_separator = "  ",
			max_len_align = false,
			max_len_align_padding = 1,
			right_align = false,
			right_align_padding = 7,
			highlight = "Comment",
		},
		debug_mode = false,
	})
end

return M
