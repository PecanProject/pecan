# How to contribute

Third-party contributions are highly encouraged for PEcAn and will grow the code as well as the understanding of PEcAn and its applications.  The core development team can not add all models that exist to PEcAn or all possible scenarios and analysis that people want to conduct.  Our goal is to keep it as easy as possible for you contribute changes that get things working in your environment.
There are a few guidelines that we need contributors to follow so that we can have a chance of keeping on top of things.

## PEcAn CORE vs Models vs Modules

New functionality is typically directed toward modules to provide a slimmer PEcAn Core, reducing the requirements to get PEcAn running on different platforms, especially HPC machines, and to allow greater freedom for modules and models.

Generally, new model should be added to the models folder and new modules should be added to the modules folder.
Exceptions include code that is reused in many models or modules and wrapper functions that call specific implementations in models; these can be placed in the core packages.

If you are unsure of whether your contribution should be implemented as a model, module or part of PEcAn Core, you may join our [Slack Channel](https://join.slack.com/t/pecanproject/shared_invite/enQtMzkyODUyMjQyNTgzLWEzOTM1ZjhmYWUxNzYwYzkxMWVlODAyZWQwYjliYzA0MDA0MjE4YmMyOTFhMjYyMjYzN2FjODE4N2Y4YWFhZmQ).

## Creating Issues

- Make sure you have a GitHub account.
- Search GitHub and Google to see if your issue has already been reported
  - Create an issue in GitHub, assuming one does not already exist.
  - Clearly describe the issue including steps to reproduce when it is a bug.
  - Make sure you fill in the earliest version that you know has the issue.
- Ask @dlebauer, @mdietze or @robkooper to add you to the PEcAn project if you plan on fixing the issue.

## Getting Started

We highly recommend starting with running PEcAn in Docker. This will give you a chance to familiarize yourself with PEcAn as well as getting all the pieces setup. Please check out [DEV-INTRO.md](DEV-INTRO.md) for more information.

At this point you will have a copy of PEcAn and you are almost ready to work on the code. The first step however is to switch the code  to your forked version of PEcAn. To get this started you will need to first fork PEcAn on GitHub using the following two steps.

1. Goto the [PEcAn project](https://github.com/PecanProject/pecan) repo.
2. Click on the Fork button in the upper right corner.

At this point you will have a copy of the pecan repo in your personal space. Next steps are to setup your local copy to work with the forked version.

Introduce your self to GIT (if you have not done this yet), make sure you use an email associated with your GitHub account.

```bash
git config --global user.name "John Doe"
git config --global user.email johndoe@example.com
```

Switch pecan to your fork

```bash
git remote set-url origin https://github.com/<your username>/pecan.git
```

Setup pecan to be able to fetch from the master/develop

```bash
git remote add upstream https://github.com/PecanProject/pecan.git
```

## PEcAn Branches

PEcAn uses two protected branches, the master branch and the develop branch. The master branch will match the official releases, but all work will be done on the develop branch. Make sure that you create branches from the develop branch. This should be the default branch in your git repository.

## Adding Features

When you add a new feature always create an issue first, this allows others to comment and give you tips. It will also help us keep track of what people are adding and with new releases helps us to write new release notes and give you credit for your work.

Secondly always work in a  branch, never work on the master or develop branch. Keep your master and develop branch in sync with the master and develop of the official PEcAn repository. This makes the pull requests (you do want your work to be in the main branch right?) easier for us.

Finally try to keep your branches focused on fixing/adding only one feature and try not fall in the trap of doing a lot of things in a  single branch. This will not only make it harder for us to process your pull request but makes it take longer before you can submit your pull request. Small pull requests are more likely to be looked at faster and pulled into the develop branch faster.

Here is a simplified workflow on how add a new feature:

### Get latest version

Update your develop (both locally and on GitHub)

```bash
git fetch upstream
git checkout develop
git merge upstream/develop
git push
```

### Create a branch to do your work

A good practice is to call the branch in the form of `GH-<issue-number>` followed by the title of the issue. This makes it easier to find out the issue you are trying to solve and helps us to understand what is done in the branch. Calling a branch my-work is confusing. Names of branch can not have a space, and should be replaced with a hyphen.

```bash
git checkout -b GH-issuenumber-title-of-issue
```

### Work and commit

Do you work, and commit as you see fit. Make your commit messages helpful.

### Update other files (CITATION, NEWS, CHANGELOG)

Your PR should include:

- CITATION.cff: if you are making or have made a non-trivial contribution (please ask if unsure; our approach is inclusive), add your name to the author section.
- NEWS.md: for each package
- CHANGELOG.md: add changes to [Unreleased] section


### Push your changes up to GitHub

If this is the first time pushing to GitHub you will need to extended command, other wise you can simply do a `git push`.

```bash
git push -u origin GH-issuenumber-title-of-issue
```


### Pull Request

 When finished create a pull request from your branch to the main pecan repository.

## Additional Resources

- [Adding models to PEcAn](https://pecanproject.github.io/pecan-documentation/master/adding-an-ecosystem-model.html)
- [PEcAn configuration files](https://pecanproject.github.io/pecan-documentation/master/pecan-xml-configuration.html)
- [Development help](https://pecanproject.github.io/pecan-documentation/master/developer-guide.html)
- [PEcAn Code of Conduct](https://pecanproject.github.io/pecan-documentation/master/contributor-covenant-code-of-conduct.html)
