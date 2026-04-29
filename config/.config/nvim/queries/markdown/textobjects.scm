
;; located in: nvim/after/queries/markdown/textobjects.scm

;extends

(fenced_code_block (code_fence_content) @code_cell.inner) @code_cell.outer
