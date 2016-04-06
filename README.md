Release your Haskell project into the wild. **wild** is a simple Haskell script to help release Haskell projects.

- Current version: **0.1.0**
- Portability: POSIX (tested on GNU/Linux. Should work on other POSIX platforms too)

## `wild build`: Build

This command simply runs the following commands:

- `stack install`
- `strip --strip-all --remove-section=.comment --remove-section=.note` for the executables that has been put under `~/.local/bin` by `stack install`
- `upx -9` for those executables

In other words, `strip` and `upx` need to be in your `$PATH`.

## `wild bump`: Version bump

This command reads `CHANGELOG.md`, updates the version string in `<project>.cabal` and `README.md`, then lets you commit those changes.

It assumes you have just added a new version description in `CHANGELOG.md`, where the lines that start with `##` (level 2 heading) will be fetched to get the version strings, and the latest two of them will be used to update the version string in `<project>.cabal` and `README.md`. Then the Git commit message editor will be run, with the `-v` option to show the diff. To cancel committing, you should abnormally exit the editor (In Vim's case, it's `:cq`).

## `wild github`: GitHub release

This command creates a new GitHub release draft and uploads the executables as its assets. The name of the release is `v` prepended to the latest version string in `CHANGELOG.md`.

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
