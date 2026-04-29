function claude-setup -a model_choice
    # Step 1: Login with SAML2AWS (will prompt for password)
    echo "🔐 Logging into AWS with saml2aws..."
    saml2aws login --session-duration 3600 --force --profile nuk-digital-dev-architecture

    # Step 2: Export AWS creds into fish
    echo "🌍 Loading AWS credentials..."
    saml2aws script --profile nuk-digital-dev-architecture --shell=fish | source

    # Step 3: Set Base Claude / Bedrock env vars
    set -gx CLAUDE_CODE_MAX_OUTPUT_TOKENS 64000
    set -gx MAX_THINKING_TOKENS 1024
    set -gx CLAUDE_CODE_USE_BEDROCK 1
    set -gx ANTHROPIC_SMALL_FAST_MODEL eu.anthropic.claude-3-5-haiku-20241022-v1:0

    # Default to 'sonnet' if no argument is provided
    if test -z "$model_choice"
        set model_choice "sonnet"
    end

    # Step 4: Set the Model and the matching AWS Region
    switch $model_choice
        case sonnet
            set -gx ANTHROPIC_MODEL eu.anthropic.claude-sonnet-4-5-20251101-v1:0
            set -gx AWS_REGION eu-west-1
            echo "🧠 Selected Model: Claude 4.5 Sonnet (EU)"
            
        case opus
            set -gx ANTHROPIC_MODEL arn:aws:bedrock:eu-west-1:986741882442:inference-profile/eu.anthropic.claude-opus-4-5-20251101-v1:0
            set -gx AWS_REGION eu-west-1
            echo "🧠 Selected Model: Claude 4.5 Opus (EU)"

        case opus46
            set -gx ANTHROPIC_MODEL eu.anthropic.claude-opus-4-6-v1
            set -gx AWS_REGION eu-west-1
            echo "🧠 Selected Model: Claude 4.6 Opus (EU Inference Profile)"

        case opus46-us
            set -gx ANTHROPIC_MODEL us.anthropic.claude-opus-4-6-v1
            set -gx AWS_REGION us-east-1
            echo "🧠 Selected Model: Claude 4.6 Opus (US Inference Profile)"
            
        case global
            set -gx ANTHROPIC_MODEL global.anthropic.claude-sonnet-4-20250514-v1:0
            set -gx AWS_REGION us-east-1
            echo "🧠 Selected Model: Claude 4 Sonnet (Global/US)"
            
        case instant
            set -gx ANTHROPIC_MODEL us.anthropic.claude-instant-v1:0
            set -gx AWS_REGION us-east-1
            echo "🧠 Selected Model: Claude Instant (US)"
            
        case '*'
            echo "⚠️ Unknown model '$model_choice'. Defaulting to Sonnet..."
            set -gx ANTHROPIC_MODEL eu.anthropic.claude-sonnet-4-5-20251101-v1:0
            set -gx AWS_REGION eu-west-1
    end

    echo "✅ Claude code environment ready!"
end
