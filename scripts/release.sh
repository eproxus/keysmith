#!/bin/sh
# Automated release script for Rebar projects.
#
# - Updates the change log and bumps version
# - Commits and tags the changes,
# - Publishes to Hex.pm and GitHub
#
# Use --dry-run to test the release without pushing and publishing
#
set -e -u

# FIXME: Commit opens editor
# FIXME: Changelog gets "by @eproxus" added to all commits after 'git switch main'
# FIXME: Release still has "Version 0.6.1" as description.
# FIXME: GitHub release has "Release v0.6.1" instead of just "v0.6.1"

# Configuration
DRY_RUN=true

# Colors and logging
readonly RED='\033[0;31m' GREEN='\033[0;32m' YELLOW='\033[1;33m' NC='\033[0m'
log() { printf "${2}[${1}]${NC} %s\n" "$3"; }
info() { log "INFO" "${GREEN}" "$1"; }
warn() { log "WARN" "${YELLOW}" "$1"; }
error() {
    log "ERROR" "${RED}" "$1"
    exit 1
}
dry_run() { log "DRY-RUN" "${YELLOW}" "$1"; }
clear_previous_line() { printf "\e[F\e[K"; }

# Unified execute function with dry-run support
execute() {
    mode="$1"
    description="$2"
    command="$3"
    if [ "${mode}" = "check" ]; then
        info "🔄 ${description}..."
        if eval "${command}"; then
            clear_previous_line
            info "✅ ${description}"
        else
            clear_previous_line
            if [ "${DRY_RUN}" = "true" ]; then
                warn "⚠️ ${description} failed (would block release)"
            else
                error "⚠️ ${description} failed"
            fi
        fi
    else
        if [ "${DRY_RUN}" = "true" ]; then
            dry_run "⏸️ ${description}"
        else
            info "🔄 ${description}..."
            if eval "${command}"; then
                clear_previous_line
                info "✅ ${description}"
            else
                clear_previous_line
                error "⚠️ ${description}"
            fi
        fi
    fi
}

# Convenience helpers for better readability
check() {
    execute "check" "$1" "$2"
}

action() {
    execute "action" "$1" "$2"
}

# Parse arguments
while [ $# -gt 0 ]; do
    case $1 in
        --publish) DRY_RUN=false ;;
        -h | --help)
            cat << EOF
Usage: $0 [--publish] [--help]
  --publish  Actually commit, tag, and publish (default is dry-run)
  --help     Show this help
EOF
            exit 0
            ;;
        *) error "Unknown option: $1" ;;
    esac
    shift
done

# Check dependencies
for tool in git mise; do
    command -v "${tool}" > /dev/null 2>&1 || error "Missing tool: ${tool}"
done
mise install --quiet

# Check authentication
rebar3 hex user whoami > /dev/null 2>&1 || error "Not authenticated with Hex.pm"
gh auth status > /dev/null 2>&1 || error "Not authenticated with GitHub"

# Pre-flight checks
main_commit=$(git rev-parse main)
current_commit=$(git rev-parse HEAD)
check "Commit is on main branch" "[ '${current_commit}' = '${main_commit}' ]"
check "Repository is clean" "[ -z \"\$(git status --porcelain)\" ]"
action "Switch to main branch pointer" "git switch main"

action "Fetching from remote" "git fetch --quiet"
remote_main=$(git rev-parse origin/main)
check "main up-to-date with origin" "[ '${main_commit}' = '${remote_main}' ]"

# Check GitHub Actions status
WORKFLOW_NAMES=$(gh workflow list --json name --jq '[.[].name] | tostring')
error_message=$(gh run list --branch main --commit "${main_commit}" \
    --json conclusion,workflowName \
    --jq "
        ([.[] | select(.conclusion==\"success\") | .workflowName] | sort) as \$successful |
        if \$successful == (${WORKFLOW_NAMES} | sort)
        then \"\"
        else \"Error: Mismatch in successful workflows. Successful: \\(\$successful)\"
        end
    ")
check "build status of main" "[ -z '${error_message}' ]"

# Main release process
info "Starting release..."

check "Running CI checks" "mise run ci"
action "Updating changelog" "git-cliff --bump"

version=$(git-cliff --bumped-version --output=-)
[ -n "${version}" ] || error "Failed to get version"
info "Release version: ${version}"

# Check for existing tag
git tag -l "${version}" | grep -q "^${version}$" && error "Tag ${version} already exists"

# Commit if changes exist
if ! git diff-index --quiet HEAD --; then
    action "Committing version ${version}" "git commit --all --message 'chore(version): Release ${version}'"
else
    warn "No changes to commit"
fi

action "Tagging release ${version}" "git tag --sign '${version}'"
action "Pushing to remote" "git push origin main --tags"
action "Publishing to Hex.pm" "rebar3 hex publish"

# GitHub release
# FIXME: Make this run git-cliff instead
git_cliff() { mise run --quiet changelog --strip=all --output=- "$@"; }
if [ "${DRY_RUN}" = "true" ]; then
    release_notes=$(git_cliff --unreleased --bump)
else
    release_notes=$(git_cliff --latest)
fi
release_notes=$(echo "${release_notes}" | tail -n +2)
[ -n "${release_notes}" ] || error "Failed to get release notes"

info "Release notes:"
printf '%s' "${release_notes}" | glow
action "Creating GitHub release" "gh release create '${version}' --title '${version}' --notes '${release_notes}'"

# Completion
if [ "${DRY_RUN}" = "true" ]; then
    info "Dry-run release ${version} completed! 🎯"
    info "Review and run with --publish to release"
else
    info "Release ${version} completed! 🎉"
fi
