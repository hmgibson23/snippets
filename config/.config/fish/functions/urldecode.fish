function urldecode --description 'URL decode a string (supports stdin)'
    # Check if no arguments are passed and stdin is a terminal (no pipe)
    if test (count $argv) -eq 0; and isatty stdin
        echo "Usage: urldecode <string>" >&2
        echo "   or: echo <string> | urldecode" >&2
        return 1
    end

    if test (count $argv) -gt 0
        # Handle argument input
        python3 -c "import urllib.parse, sys; print(urllib.parse.unquote(sys.argv[1]))" $argv[1]
    else
        # Handle piped stdin
        python3 -c "import urllib.parse, sys; print(urllib.parse.unquote(sys.stdin.read().rstrip('\n')))"
    end
end
