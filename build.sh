#!/usr/bin/env bash
# build.sh – one-shot build script for org-publish site
# Usage:
#   ./build.sh        # production build  (my-site-root = SITE_DOMAIN)
#   ./build.sh dev    # local preview     (my-site-root = "$(pwd)/")

set -euo pipefail

### === 1. Configure your production domain ================================
SITE_DOMAIN="https://zhixiao-zhang.github.io/"   # make sure to keep the trailing slash

### === 2. Parse CLI arg & decide my-site-root =============================
if [[ "${1:-}" == "dev" ]]; then
  MY_ROOT="$(pwd)/public/"
  echo "🛠  Dev mode: my-site-root = $MY_ROOT"
else
  MY_ROOT="$SITE_DOMAIN"
  echo "🚀  Production mode: my-site-root = $MY_ROOT"
fi

### === 3. Run Emacs in batch to publish ===================================
EMACS_CMD=(
  emacs
  --batch
  --eval "(setq my-site-root \"${MY_ROOT}\")"
  -l publish.el
)

echo "🔧  Running Emacs publish…"
"${EMACS_CMD[@]}"

### === 4. Replace {{site-root}} placeholders ==============================
echo "🔄  Replacing {{site-root}} → ${MY_ROOT}"
# GNU sed doesn’t need a backup suffix; BSD/macOS sed does.
if sed --version >/dev/null 2>&1; then
  # GNU sed
  find public -type f -name '*.html' -exec sed -i "s|{{site-root}}|${MY_ROOT}|g" {} +
else
  # BSD sed (macOS)
  find public -type f -name '*.html' -exec sed -i '' "s|{{site-root}}|${MY_ROOT}|g" {} +
fi

### === 5. Delete org-publish-cache ========================================
rm -rf ~/.org-timestamps

echo "✅  Build finished. Output available in ./public"
