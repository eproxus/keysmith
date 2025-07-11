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

# Execute with dry-run support
run() {
    if [ "${DRY_RUN}" = "true" ]; then
        dry_run "$1: $2"
    else
        info "$1..."
        if eval "$2"; then
            info "$1 ✓"
        else
            error "Failed: $1"
        fi
    fi
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
mise install

# Check authentication
rebar3 hex user whoami > /dev/null 2>&1 || error "Not authenticated with Hex.pm"
gh auth status > /dev/null 2>&1 || error "Not authenticated with GitHub"

# Pre-flight checks
if [ "${DRY_RUN}" = "false" ]; then
    main_commit=$(git rev-parse main)
    current_commit=$(git rev-parse HEAD)
    [ "${current_commit}" = "${main_commit}" ] || error "Current commit is not on main branch"
    git_status=$(git status --porcelain)
    if ! git diff-index --quiet HEAD -- || [ -n "${git_status}" ]; then
        error "Repository not clean"
    fi
else
    dry_run "Skipping main branch and clean repo checks"
fi

# Main release process
if [ "${DRY_RUN}" = "true" ]; then
    info "Starting release (DRY-RUN)..."
else
    info "Starting release..."
fi

run "Running CI checks" "mise run ci"
run "Updating changelog" "git-cliff --bump"

version=$(git-cliff --bumped-version --output=-)
[ -n "${version}" ] || error "Failed to get version"
info "Release version: ${version}"

# Check for existing tag
git tag -l "${version}" | grep -q "^${version}$" && error "Tag ${version} already exists"

# Commit if changes exist
if ! git diff-index --quiet HEAD --; then
    run "Committing version ${version}" "git commit -m 'chore(version): Release ${version}'"
else
    warn "No changes to commit"
fi

run "Tagging release ${version}" "git tag '${version}'"
run "Pushing to remote" "git push origin main --tags"
run "Publishing to Hex.pm" "rebar3 hex publish"

# GitHub release
cliff_args="--strip=all --output=-"
if [ "${DRY_RUN}" = "true" ]; then
    cliff_args="--unreleased --bump ${cliff_args}"
else
    cliff_args="--latest ${cliff_args}"
fi
release_notes=$(git-cliff "${cliff_args}" | tail -n +2)
[ -n "${release_notes}" ] || error "Failed to get release notes"

if [ "${DRY_RUN}" = "true" ]; then
    dry_run "Would create GitHub release: gh release create '${version}'"
    dry_run "Release notes preview:"
    printf '%s' "${release_notes}" | glow
else
    run "Creating GitHub release" "gh release create '${version}' --title 'Release ${version}' --notes '${release_notes}'"
fi

# Completion
if [ "${DRY_RUN}" = "true" ]; then
    info "Dry-run release ${version} completed! 🎯"
    info "Review and run with --publish to release"
else
    info "Release ${version} completed! 🎉"
fi
