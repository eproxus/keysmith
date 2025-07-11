#!/bin/sh
set -e -u

SUBTREE_PREFIX="test/fixtures/type_id"
REMOTE_REPO="git@github.com:jetify-com/typeid.git"
REMOTE_REF="9129b051eb14c5553dbebc99119c7b4741e5219b"
KEEP_DIR="spec"

GIT_ROOT="$(git rev-parse --show-toplevel)"

# Ensure we're in the root of the git repository
cd "${GIT_ROOT}"

# Check if there are any uncommitted changes
STASH_NEEDED=false
if ! git diff-index --quiet HEAD --; then
    echo "Working directory has uncommitted changes. Stashing..."
    TIMESTAMP=$(date)
    git stash push -m "Temporary stash for subtree update - ${TIMESTAMP}"
    STASH_NEEDED=true
fi

# Function to restore stash on exit
cleanup() {
    if [ "${STASH_NEEDED}" = true ]; then
        echo "Restoring stashed changes..."
        git stash pop
    fi
}

# Set trap to ensure stash is restored even if script fails
trap cleanup EXIT

# Update or add the subtree
if [ -d "${SUBTREE_PREFIX}" ]; then
    echo "Updating existing subtree..."
    git subtree pull --prefix "${SUBTREE_PREFIX}" "${REMOTE_REPO}" "${REMOTE_REF}" --squash
else
    echo "Adding new subtree..."
    git subtree add --prefix "${SUBTREE_PREFIX}" "${REMOTE_REPO}" "${REMOTE_REF}" --squash
fi

# Navigate to the subtree directory
cd "${SUBTREE_PREFIX}"

# Move the directory we want to keep to a temporary location
mv "${KEEP_DIR}" "../${KEEP_DIR}_temp"

# Remove all other content
git rm -rf .

# Move the kept directory back
mv "../${KEEP_DIR}_temp" "${KEEP_DIR}"

# # Add the changes
git add .

# # Commit the changes
git commit -m "Update ${SUBTREE_PREFIX}/${KEEP_DIR} folder from ${REMOTE_REPO}"

echo "Subtree updated and cleaned successfully."
