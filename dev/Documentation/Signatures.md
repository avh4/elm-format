Cryptographic signatures are used to give some minimal validation that files have not changed unexpectedly.

# Binaries from CI builds

All CI runners have access to a private key
that is used to sign all Release Build workflow artifacts.
The set of runners currently includes GitHub hosted runners,
and private runners provisioned by Lamdera.
The public key is stored in `keys/gpg-actions.pub`
and is used by `dev/build.sh publish-*`
to verify the downloaded binaries before publishing.

## How to generate a new key

1. Remove the old key:
    - `rm ./keys/github-actions.pub`
1. Create the new key:
    - `minisign -G -s ./XXX_NEW_PRIVATE_KEY -p ./keys/github-actions.pub`
    - Leave the password blank
1. Get the private key:
    - `cat ./XXX_NEW_PRIVATE_KEY`
    - Copy the result as the value of `MINISIGN_PRIVATE_KEY` at <https://github.com/avh4/elm-format/settings/secrets/actions>
1. Securely delete the private key:
    - `shred -vz XXX_NEW_PRIVATE_KEY`
1. Check in the changes to `./keys/github-actions.pub`
1. Push to a branch whose name starts with "release/" to trigger the Build Release workflows, and make sure they succeed.
