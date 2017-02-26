# How to contribute

No code is perfect and we always welcome third-party patches. This document describes guidelines that we need contributors to follow so that we can accept their pull requests.

## Getting started

- Make sure you have an account on this site,
- Submit a ticket for your issue, assuming one does not already exists.
    - Write in English,
    - Use the imperative mood in the subject/title field,
    - Clearly describe the issue including steps to reproduce when it is a bug,
    - Make sure you fill in the earliest version that you know has the issue.
- Fork the repository on this site and `git-clone(1)` it locally.

## Making changes

- Create a topic branch from where you want to base your work,
- Make your changes taking [style guide](CODINGSTYLE.md) into account,
- Check for whitespace errors issuing `git diff --check`, `git diff --cached --check` or `git diff HEAD --check`,
- Make commits writing good [commit messages](http://chris.beams.io/posts/git-commit/),

## Submitting changes

- Push your changes to a topic branch in your fork of the repository,
- Submit a pull request to the project's repository,
- Update your ticket to mark you have submitted code and are ready for it to be reviewed. Include a link to the pull request in the ticket.

# Additional resources

- [GEDKeeper coding style guide](CODINGSTYLE.md)
- [GEDKeeper chat on gitter](https://gitter.im/Serg-Norseman/GEDKeeper)
