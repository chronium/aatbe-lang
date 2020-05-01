# Contribution Guidelines

Everyone is welcome to contribute in any form they like. Feel free to join on issues, make pull requests, and be nice to everyone.

## Organization

The following list of people involved in the project is defined like so:

* _Author_: Original developer of this project
* _Maintainers_: Lead developers and people responsible of reviews and maintainance.
* _Developers_: People who have contributed or are still contributing to this project.
* _Contributors_: People who have occasionally contributed to this project.
* _Supporters_: People who provided financial support.

The lists must be stored in [CREDITS.MD](CREDITS.md), containing three pieces of information:
- _Name_: Legal given name(s) and/or legal family name(s).
- _Alias_: Nickname chosen by the person.
- _Email_: Valid email address owned by the person.

These will be formatted as follows `$name (@$alias) <$email>`. If *Name* is not provided, *Alias* will not be wrapped in parentheses. If neither *Name* nor *Alias* are provided, *Email* shall not be wrapped in angle brackets. Members of the lists *Maintainers* and *Developers* must provide a valid email address, to allow contact if it is required.

For *Supporters* please contact *Author* or a *Maintainer* if you want your data to be included under [CREDITS.MD](CREDITS.md).

These lists will be managed following these rules:
1. Only *Author* and *Maintainers* will be able to modify all lists except for *Author*.
2. Any listed person may remove or modify their own data.
3. Lists will be sorted by "join date", with the older entries being kept first.

### Exception
If a developer wants to remain anonymous, but still contribute, they are not required to have any information put in [CREDITS.MD](CREDITS.md), other than the `Signed-off-by` footer in the commit message.

# Code submission policy

Nobody is perfect, so you will be judged only by code quality (humor is second). The following are some dos & dont's which we'd love for you to try and stick to.

## Do:
* Write idiomatic Rust.
* Conform to the general conding style. Please use `rust-fmt` before submitting a pull request.
* Make sure commits are rebased to the latest master branch.
* Wrap commit messages as 72 characters long.
* The first commit message line should follow this [guide](https://chris.beams.io/posts/git-commit/)
* Write your commit messages in proper English, punctuation is important.

## Don't:
* Submit code that is incompatible with the project license (MIT License).
* Touch anything outside of the scope of the PR.
* Make excessive commits with minor changes.
* Use meaningless words like "refactor" and "fix" to avoid explaining changes.
* Include commented-out code.
* Forget to use `rust-fmt` before submitting the PR.

# Licensing

All project files are licensed under the _MIT_ license, described at root-level file: [LICENSE](LICENSE). The license can be overwritten in the following ways:
- File header contains a different license.
- The parent folder contains a different *LICENSE* file.

All contributors are required to follow the [Developer Certificate of Origin](https://developercertificate.org/). To comply with that, there is a requirement for a `Signed-off-by` statement at the end of the commit message, which implies agreement to the DCO.

Below is the original document linked above listing the DCO:
```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

The format of the sign-off message is the following, required to be included on each commit of the pull request:
```
Signed-off-by: $name <$email>
```

That can simply be added by specifying the `-s` or `--signoff` flag to the `git commit` command. Optionally, a GPG signature could also be added.
