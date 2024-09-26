#!/bin/sh
set -e -x

SUBTREE_PREFIX="test/fixtures/type_id"
REMOTE_REPO="git@github.com:jetify-com/typeid.git"
REMOTE_REF="9129b051eb14c5553dbebc99119c7b4741e5219b"
KEEP_DIR="spec"

GIT_ROOT="$(git rev-parse --show-toplevel)"

# Ensure we're in the root of the git repository
cd "${GIT_ROOT}"

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
