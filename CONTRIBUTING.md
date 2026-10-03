# Contributing to camtrapR

Thank you for your interest in contributing to **camtrapR**. Contributions of many kinds are welcome, including bug reports, documentation improvements, reproducible examples, tests, and code changes.

camtrapR is a long-running R package. We aim to make changes carefully, preserve existing behavior where possible, and keep the code understandable and maintainable. A focused contribution that fits the package’s existing design is generally more helpful than a large rewrite.

## Before you start

Please search the open and closed issues to see whether the problem or idea has already been discussed.

- **Bug fix:** If there is already an issue, please comment there before starting substantial work. If not, open an issue describing the bug and, where possible, provide a small reproducible example.
- **Enhancement or new feature:** Please open an issue or comment on an existing one before coding. This gives us a chance to discuss the intended behavior and scope.
- **Small typo or documentation correction:** A pull request is welcome without a separate issue.

Issues labelled **good first issue** are intended to be approachable starting points. Please feel free to ask questions in the issue before taking one on.

## Development branches

The project uses the following branches:

- **`main`**: stable code associated with CRAN releases. Ordinary development should not target this branch.
- **`dev`**: the integration branch for ongoing development. Pull requests should normally target `dev`.
- **Feature branches**: larger changes, especially changes that may affect existing behavior or compatibility, should be developed on a branch based on `dev` and merged into `dev` when ready.

Contributors who do not have write access should work in a fork and submit a pull request to `dev`. Please don’t submit routine development pull requests directly to `main`.

## Making a contribution

1.  **Agree on the scope.** For anything beyond a small, self-contained fix, discuss the proposed change in an issue first.
2.  **Work on a focused branch.** Keep the change as small as reasonably possible and avoid bundling unrelated cleanup or formatting changes with it.
3.  **Follow the existing conventions.** Match the package’s current code, documentation, and testing patterns. Please avoid unrelated refactoring.
4.  **Add or update tests where appropriate.** Bug fixes should generally include a test that demonstrates the problem and verifies the fix, if the package’s existing test setup allows it.
5.  **Update documentation where needed.** User-facing changes may need updates to function documentation, vignettes, examples, or other relevant materials.
6.  **Run relevant checks.** Before opening a pull request, please run `devtools::test()` and `devtools::check(args = c("--as-cran"))` if you can. GitHub Actions also runs the package checks on pull requests to `dev` and `main`. A passing CI check is required before a pull request is merged. If you cannot run a check locally, or it fails for a reason you believe is unrelated to your change, mention that in the pull request.
7.  **Open a pull request to `dev`.** Describe the problem, the change, any behavior that users should know about, and the checks you ran. Link the relevant issue.

Please make sure your changes do not unintentionally alter unrelated files. If you are unsure whether a change is in scope, ask in the issue or pull request.

## AI-assisted coding

AI coding tools are permitted, just as other development tools are. They do not change the expectations for a contribution:

- You are responsible for the submitted code, including code drafted or modified with AI assistance.
- You should understand and be able to explain every substantive change in your contribution.
- Review and test AI-generated suggestions rather than assuming they are correct. In particular, check for changes in behavior, compatibility, dependencies, documentation, and tests.
- Keep AI-assisted changes focused. Please do not submit large generated rewrites, broad refactors, or code that you cannot confidently review and maintain.
- If AI substantially generated or directed the implementation, mention that in the pull request description and briefly say how you reviewed or tested it. Routine autocomplete or minor wording assistance does not need to be reported.

## Pull requests and review

A maintainer will review contributions for correctness, scope, compatibility, clarity, and fit with the package. We may ask for changes, propose a smaller alternative, or decide not to merge a contribution. Opening a pull request does not guarantee acceptance.

Please be constructive and patient during review. Review is part of maintaining the package over time, not just checking whether the code works in one example.

## Questions

If you are unsure where to start, or how a proposed change fits into the project, please ask in the relevant issue before doing substantial work.
