# How to contribute

Third-party contributions are highly encouraged for PEcAn and will grow the code as well as the understanding of PEcAn and its applications. 
The core development team can not add all models that exist to PEcAn or all possible scenarios and analysis that people want to conduct. 
Our goal is to keep it as easy as possible for you contribute changes that get things working in your environment. 
There are a few guidelines that we need contributors to follow so that we can have a chance of keeping on top of things.

## PEcAn CORE vs Models vs Modules

New functionality is typically directed toward modules to provide a slimmer PEcAn Core, reducing the requirements to get PEcAn running on different platforms, especially HPC machines, and to allow greater freedom for modules and models.

Generally, new model should be added to the models folder and new modules should be added to the modules folder. 
Exceptions include code that is reused in many models or modules and wrapper functions that call specific implementations in models; these can be placed in the core packages.

If you are unsure of whether your contribution should be implemented as a model, module or part of PEcAn Core, you may visit [Gitter](https://gitter.im/PecanProject/pecan) or ask on the pecan-develop mailing list for advice.

## Creating Issues

- Make sure you have a GitHub account.
- Search GitHub and Google to see if your issue has already been reported
        - Create an issue in GitHub, assuming one does not already exist.
	- Clearly describe the issue including steps to reproduce when it is a bug.
	- Make sure you fill in the earliest version that you know has the issue.
- Ask @dlebauer, @mdietze or @robkooper to add you to the PEcAn project if you plan on fixing the issue.

## Getting Started

We highly recommend starting with the [downloaded VM image](http://opensource.ncsa.illinois.edu/projects/artifacts.php?key=PECAN). This image comes with a copy of PEcAn preinstalled with a fully populated copy of the and BETYdb database. 
It also has the following models preinstalled: ED, SIPNET, Linkages and DALEC. 
Another option is to install PEcAn on your own machine, using the instructions in the [documentation](https://pecanproject.github.io/pecan-documentation/installing-pecan.html). However, be prepared for this to take a half day or so.

When you login into the VM you will have already a cloned copy of PEcAn however for development this needs to be switch to your forked version of PEcAn. To get this started you will need to first fork PEcAn on GitHub using the following two steps.

1. Goto the [PEcAn project](https://github.com/PecanProject/pecan) repo.
2. Click on the Fork button in the upper right corner.

At this point you will have a copy of the pecan repo in your personal space. Next steps are to setup the VM for you to work with git.

Introduce your self to GIT, make sure you use an email associated with your GitHub account.
```
git config --global user.name "John Doe"
git config --global user.email johndoe@example.com
```

Switch pecan to your fork
```
git remote set-url origin https://github.com/<your username>/pecan.git
```

Setup pecan to be able to fetch from the master
```
git remote add upstream https://github.com/PecanProject/pecan.git
```

## Adding Features

When you add a new feature always create an issue first, this allows others to comment and give you tips. It will also help us keep track of what people are adding and with new releases helps us to write new release notes and give you credit for your work.

Secondly always work in a  branch, never work on the master branch. Keep your master branch in sync with the master of the official PEcAn repository. This makes the pull requests (you do want your work to be in the main branch right?) easier for us.

Finally try to keep your branches focused on fixing/adding only one feature and try not fall in the trap of doing a lot of things in a  single branch. This will not only make it harder for us to process your pull request but makes it take longer before you can submit your pull request. Small pull requests are more likely to be looked at faster and pulled into the main branch faster.

Here is a simplified workflow on how add a new feature:

### Get latest version

Update your master (both locally and on GitHub)

```
git fetch upstream
git checkout master
git merge upstream/master
git push
```

### Create a branch to do your work.

A good practice is to call the branch in the form of GH-<issue-number> followed by the title of the issue. This makes it easier to find out the issue you are trying to solve and helps us to understand what is done in the branch. Calling a branch my-work is confusing. Names of branch can not have a space, and should be replaced with a hyphen.

```
git checkout -b GH-issuenumber-title-of-issue
```

### Work and commit

Do you work, and commit as you see fit.Make your commit messages helpful. 

### Push your changes up to GitHub.

If this is the first time pushing to GitHub you will need to extended command, other wise you can simply do a `git push`.

```
git push -u origin GH-issuenumber-title-of-issue
```

### Pull Request

 When finished create a pull request from your branch to the main pecan repository.

## Additional Resources

- [Adding models to PEcAn](https://pecanproject.github.io/pecan-documentation/master/adding-an-ecosystem-model.html)
- [PEcAn configuration files](https://pecanproject.github.io/pecan-documentation/master/pecan-xml-configuration.html)
- [Development help](https://pecanproject.github.io/pecan-documentation/master/overview.html)
- [PEcAn Code of Conduct](https://pecanproject.github.io/pecan-documentation/master/contributor-covenant-code-of-conduct.html)
