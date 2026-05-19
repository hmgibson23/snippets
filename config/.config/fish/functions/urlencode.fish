function urlencode --description 'URL encode a string (supports stdin)'
    # Check if no arguments are passed and stdin is a terminal (no pipe)
    if test (count $argv) -eq 0; and isatty stdin
        echo "Usage: urlencode <string>" >&2
        echo "   or: echo <string> | urlencode" >&2
        return 1
    end

    if test (count $argv) -gt 0
        # Handle argument input
        python3 -c "import urllib.parse, sys; print(urllib.parse.quote(sys.argv[1]))" $argv[1]
    else
        # Handle piped stdin
        python3 -c "import urllib.parse, sys; print(urllib.parse.quote(sys.stdin.read().rstrip('\n')))"
    end
end
