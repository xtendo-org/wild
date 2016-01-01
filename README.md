Release your Haskell project into the wild with **wild**.

- Current version: **0.1.0**
- Portability: POSIX (tested on GNU/Linux. Should work on other POSIX platforms too)

## Build: `wild build`

This command simply runs the following commands:

- `stack install`
- `strip --strip-all --remove-section=.comment --remove-section=.note` for the executables that has been put under `~/.local/bin` by `stack install`
- `upx -9` for those executables

## Version bump: `wild bump`

This command reads `CHANGELOG.md`, updates the version string in `<project>.cabal` and `README.md`, then lets you commit those changes.

It assumes you have just added a new version description in `CHANGELOG.md`, where the lines that start with `##` (level 2 heading) will be fetched to get the version strings, and the latest two of them will be used to update the version string in `<project>.cabal` and `README.md`. Then the Git commit message editor will be run, with the `-v` option to show the diff.

## GitHub release: `wild github`

This command creates a new GitHub release and uploads the executables as its assets.

The project repository must have `wild.json` file to use this command. If it doesn't exist, `wild github` creates one with the defaults.

Edit `wild.json`:

- `"owner"`: The GitHub project owner.
- `"repo"`: The GitHub repo name.
- `"tokenPath"`: The path to the file which contains the GitHub token.

If you don't have a GitHub token, go to GitHub's **Personal settings** → **Personal access tokens** → **Generate new token** to get one. Then

```sh
echo -n "1234abcd..." > ~/.github.token
```

to save it locally, and set `"tokenPath"` in `wild.json` to `~/.github.token`. (Preferably, set `chmod 400 ~/.github.token` as well.)
