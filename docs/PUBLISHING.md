# Publishing this repository

## Suggested GitHub details

- Repository name: `haskell-expression-interpreter`
- Description: `A Haskell arithmetic expression interpreter with local bindings, explicit errors, algebraic simplification, and 57 automated checks.`
- Topics: `haskell`, `functional-programming`, `interpreter`, `algebraic-data-types`, `recursion`, `education`

## Before the first push

1. Confirm that your course permits publishing the solution, and choose the appropriate repository visibility.
2. Keep the report, student index number, assignment brief, backups, and temporary files outside this repository.
3. Decide whether to add a license. None has been chosen automatically.
4. Run the compilation and test commands in the README.
5. Review the files to be committed. The existing assistance credit in the source has been preserved.

The original coursework files remain outside this folder. Only this folder is intended to become the GitHub repository.

## Create the remote repository

On GitHub, create an empty repository named `haskell-expression-interpreter`. Do not initialize the remote with a README, license, or gitignore because the local folder already contains its own files.

Local Git has been initialized on branch `main`. No commit, remote, or push was created during preparation.

From this folder, stage only the intended files:

```sh
git status --short
git add Expr.hs README.md examples docs .github .gitignore .gitattributes .editorconfig
git diff --cached --stat
git diff --cached
git commit -m "Initial commit: Haskell expression interpreter"
```

If Git asks for a name and email, configure your preferred commit identity locally for this repository before retrying the commit. Do not place passwords or tokens in repository files or remote URLs.

Replace `YOUR_REPOSITORY_URL` below with the HTTPS or SSH URL of your new GitHub repository:

```sh
git remote add origin YOUR_REPOSITORY_URL
git push -u origin main
```

After pushing, open the **Actions** tab and confirm that both operating-system jobs pass. The workflow configuration has been prepared locally; a successful hosted run is not claimed before GitHub executes it.

## Later changes

The files here are an independent copy. Future edits made to the original coursework folder do not automatically update this repository. Make repository changes here, rerun the tests, and review the diff before committing.
