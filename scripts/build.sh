#! /bin/bash

if [ -z ${GITHUB_REF+x} ]; then
    export GITHUB_REF=`git rev-parse --symbolic-full-name HEAD`
fi

export PATH=$PATH:$HOME/.stack/bin

if hash stack 2>/dev/null; then
    echo "Have stack, skipping installation..."
else
    echo "Installing stack..."
    curl -sSL https://get.haskellstack.org/ | sh
    echo "  done installing stack."
fi

echo "Building milkshake-cli..."
stack build --ghc-options="-O2"
stack install
cp `stack path --local-bin`/milkshake-cli ./
sleep 1
tar -czvf milkshake-cli.tar.gz milkshake-cli
sleep 1
rm milkshake-cli
ls -lah milkshake-cli.tar.gz
ls -lah
pwd
echo "Done building on ${GITHUB_REF}"
