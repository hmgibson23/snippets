function saml-assume
    # Step 1: Login with SAML2AWS (will prompt for password)
    echo "🔐 Logging into AWS with saml2aws..."
    saml2aws login --session-duration 3600 --force 
 
    # Step 2: Export AWS creds into fish
    echo "🌍 Loading AWS credentials..."
    saml2aws script --shell=fish | source

    echo "✅ AWS Env ready!"
end
