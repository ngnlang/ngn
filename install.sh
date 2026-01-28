#!/usr/bin/env bash
set -euo pipefail

BASE_URL="https://github.com/ngnlang/ngn/releases"
VERSION="${NGN_VERSION:-latest}"
INSTALL_DIR="${NGN_INSTALL_DIR:-$HOME/.local/bin}"

OS_NAME="$(uname -s)"
ARCH_NAME="$(uname -m)"

case "${OS_NAME}" in
  Linux) OS="linux" ;;
  *)
    echo "Unsupported OS: ${OS_NAME}" >&2
    exit 1
    ;;
esac

case "${ARCH_NAME}" in
  x86_64|amd64) ARCH="x86_64" ;;
  arm64|aarch64) ARCH="arm64" ;;
  *)
    echo "Unsupported architecture: ${ARCH_NAME}" >&2
    exit 1
    ;;
esac

TARBALL="ngn-${VERSION}-${OS}-${ARCH}.tar.gz"
URL="${BASE_URL}/${TARBALL}"

TMP_DIR="$(mktemp -d)"
cleanup() { rm -rf "${TMP_DIR}"; }
trap cleanup EXIT

echo "Downloading ${URL}"
if command -v curl >/dev/null 2>&1; then
  curl -fsSL "${URL}" -o "${TMP_DIR}/ngn.tar.gz"
elif command -v wget >/dev/null 2>&1; then
  wget -qO "${TMP_DIR}/ngn.tar.gz" "${URL}"
else
  echo "curl or wget is required" >&2
  exit 1
fi

tar -xzf "${TMP_DIR}/ngn.tar.gz" -C "${TMP_DIR}"

mkdir -p "${INSTALL_DIR}"

if [ ! -f "${TMP_DIR}/ngn" ] || [ ! -f "${TMP_DIR}/ngnr" ]; then
  echo "Archive missing ngn or ngnr" >&2
  exit 1
fi

install -m 0755 "${TMP_DIR}/ngn" "${INSTALL_DIR}/ngn"
install -m 0755 "${TMP_DIR}/ngnr" "${INSTALL_DIR}/ngnr"

PATH_UPDATED=0
case "${SHELL:-}" in
  *bash)
    for profile in "$HOME/.bashrc" "$HOME/.bash_profile"; do
      if [ -f "$profile" ] && grep -q "${INSTALL_DIR}" "$profile"; then
        PATH_UPDATED=1
      fi
    done
    if [ "$PATH_UPDATED" -eq 0 ]; then
      echo "export PATH=\"${INSTALL_DIR}:\$PATH\"" >> "$HOME/.bashrc"
      PATH_UPDATED=1
    fi
    ;;
  *zsh)
    if [ -f "$HOME/.zshrc" ] && grep -q "${INSTALL_DIR}" "$HOME/.zshrc"; then
      PATH_UPDATED=1
    else
      echo "export PATH=\"${INSTALL_DIR}:\$PATH\"" >> "$HOME/.zshrc"
      PATH_UPDATED=1
    fi
    ;;
esac

if ! command -v ngn >/dev/null 2>&1; then
  echo "ngn installed to ${INSTALL_DIR}"
  if [ "$PATH_UPDATED" -eq 1 ]; then
    echo "Added ${INSTALL_DIR} to your PATH (open a new shell)"
  fi
  echo "Run: export PATH=\"${INSTALL_DIR}:\$PATH\""
else
  echo "ngn $(ngn --version) installed"
fi
