---
title: Deploy to GitHub Pages
author: Michael Barrowman
date: '2020-07-22'
slug: deploy-to-github-pages
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2020-07-22T11:04:02+01:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

Within `git`, the default branch is usually named `master`, however in recent times, the negative connotations of that word are coming to the forefront of a lot of people's minds, and so they are wishing to diverge away from that kind of terminology. The simplest change that we can make is to default to the `HEAD` branch of a repo, which will point towards whatever the actual default branch is for a the repo, whether that is `master`, `main` or `Captain`.

Unfortunately, this change can be slow, and although resources like GitHub have expressed interest in switching away from the default `master`, some things are still hardcoded. One of which is the limitations of GitHub Pages deployment. Users can currently choose from one of three options:

* Build website from the `master` branch
* Build website from the `docs` folder in the `master` branch
* Build website from the `gh-pages` branch

The use of `master` here is hardcoded, and many users currently choose to use the `docs` folder in the `master` branch as the location to store their website. Depending on workflows, the other two options might not be possible, or would require huge restructuring of your workflow if you wish to switch from `master`. For User pages (repos that are `<username>.github.io`), they can only be built from the `master` branch; hopefully this will change soon (see [here](#User-repos)).

For example, this blog is written using `blogdown`, a package for `R`, which helps to create blogs. In doing so, it creates a static site in a subdirectory called `public` (by default). You do your work in the parent folder, it generates content in this subfolder. If you wish to use GitHub Pages to publish your site, the [recommendations](https://bookdown.org/yihui/blogdown/github-pages.html) by [yihui](https://yihui.org/), the package autho, are to set up your `git` directory inside this subdirectory. This has the limitation of meaning the content of your parent folder is not backed up to GitHub, and is only stored locally.

However, thanks to GitHub user [`s0`](https://github.com/s0) and their GitHub Action, it's possible to keep your work inside the `doc` folder on your `default` branch (whatever it's name may be) and have that folder automatically pushed to the `gh-pages` branch.

For those who don't know, GitHub Actions allows automation when certain events (triggers) occur within your repo. You can try to write your own complicated commands, or use those created by other users within a relatively simple skeleton. We're going to use one of these simple skeletons to utilise `s0`'s work, thus allowing us to use the `main` branch, rather than the `master` branch on our repo. In order to use GitHub Actions, we need to add a [`.yaml`](https://en.wikipedia.org/wiki/YAML) file (stands for YAML Ain't Markup Language) within our repo.

## Rename the branch

The first step in this process would be to actually change the name of our `master` branch to `main` (or whatever you choose). 

### Directly on GitHub

GitHub doesn't directly support renaming of branches (as far as I know). So, what we need to do is to create a new branch for our repository by clicking on the branches button at the top left of our repo Code page (probably says `master` right now).

And then type in the name of the new branch (e.g. `main`). If this branch doesn't exist, you'll be given the option to `Create branch: main from 'master'`. Click on Settings then Branches and you can change your default branch to the new `main` branch.

You can then delete your `master` branch, although it might be worth holding on to it for a little while, in case anybody downstream from you is using it, or as a back-up in case something goes wrong!

### Git Bash

If you have a local copy of your repo, you can run the following in command line to rename it:
```
git branch -m master main
```
If you want to rename the current branch, you can simplify this to be:
```
git branch -m main
```
Note you will have to use `-M` instead of `-m` if you are renaming a branch and only changing captalisation, e.g. from `main` to `Main`.

A common error when running this command is the following (or something to this effect):
```
error: refname refs/heads/HEAD not found
fatal: Branch rename failed
```

This means you don't have a branch checked out, and so you'll have to create a new branch, but when doing so, you can name it whatever you want
```
git checkout -b main
```

After you've done this locally, you'll have to `git push` your repo up to GitHub again. However, you'll probably get an error telling you to run the following instead
```
git push --set-upstream origin main
```
This will just ensure your new `main` branch is upstream of the previous `master` branch.

Then you'll have to change GitHub's default branch to your new one in the Settings as above.

## GitHub Workflows

GitHub Action files are stored in a special directory in your repo, the `.github/workflow` directory. All we have to do is create a file in this directory, name it something useful and give it the `.yaml` extension. Sounds simple, and for most people it is. The only limitation is that sometimes, we can't create folders with the `.` at the start (particularly on Windows). Or at least, we can't create them in the usual Right Click > New > Folder method in Windows Explorer. The simple way is to use Command Line to do it.
```
mkdir .github
```
The `mkdir` command makes directories. It's as simple as that.

Alternatively, you can create this directly on GitHub in the usual manner. Just remember that you will have to `git pull` any changes you make this way.


## The YAML

The YAML file that we create will look like this:

```
name: 'Deploy to gh-pages'
on:
  push:	
    branches:
      - main
    paths:	
      - 'docs/**'
      
jobs:
  deploy:
    name: Push docs to gh-pages
    runs-on: ubuntu-latest    
    steps:
    - uses: actions/checkout@main

    - name: Deploy
      uses: s0/git-publish-subdir-action@develop
      env:
        REPO: self
        BRANCH: gh-pages
        FOLDER: docs
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```
Feel free to copy the above directly. If you're using RStudio to manage your repo, you can create a New Text File, save it with the extension `.yaml` and RStudio will conveniently colour code the file. Otherwise, you can do it in notepad (just make sure the extension sticks).

What does this file do? I don't think it's too important to go into great details here, there is plenty of [reading to be done](https://docs.github.com/en/actions) on GitHub Actions. So, just a quick look at the important bits, in case you want to change something. Apart from the `name:` line, it's split into two parts, the `on:` and the `jobs:` parts. Note that in YAML files, whitespace is important, and gives structure: `push:` is a child of `on:` and `jobs:` is the parent of `deploy:`, but `on:` and `jobs:` are siblings.

### The Triggers

The `on:` part contains information on what events will trigger the event. Here, we're telling GitHub that we want this Action to run when we push our repo to the `main` branch, only if something has changed in the `docs` folder (or path). This means that any pushes that happen to other branches will be ignored, and any pushes that don't change our `docs` folder will also be ignored. The syntax for `paths:` actually allows you to check for changes to anything that matches this string, so by using `'docs/**'`, we match anything that starts with `'docs/'`, i.e. anything within the `docs` folder. This is useful because we're building our `gh-pages` branch based solely on what's in `docs`. If something changes elsewhere in the repo, it doesn't matter (even if you are accessing data in your repo, but outside of your site because those changes will still be pushed to your repo, just don't need to trigger a site rebuild). It also doesn't matter what happens on other branches (such as a `development` branch) because we're not wanting to build our GitHub pages from them.

### The Actions

The `jobs:` part contains the actual actions that occurs. You can have many Actions and jobs within he same file, but here we only have one job, which consists of two tasks. `deploy:` is just the formal name for the job that we're running. If we want more jobs to run, we can give them different names and place them at the same hierarchical starting point as `deploy:` (i.e. with two spaces in front). Different jobs will run in parallel, each individual job will run in order.

We then give a bit of information about the job, first it's name `Push docs to gh-pages` (more like a title), followed by what operating system we want GitHub Actions to use to implement it. Finally, we have the `steps:`, which is where we put the list of tasks that need to be run (in order).

This Action only has two steps and they are both `uses:` steps, which basically means we're going to be using Actions that are properly defined elsewhere. We could write an action directly here in quotes and supply the name of what application we want it to be run in (at a Command Line level), but we don't have to since these Actions are defined for us. Each task starts with a `-` followed by the information for that task.

#### Checkout

The first task is simple `- uses: actions/checkout@master`. You may recognise the format of this as it comes up a lot within GitHub, it is `<user>/<repo>@<branch>`. This is because all Actions created by other users, are actually repos. So what we're doing here is saying we want to `use` the Action defined within the `main` branch of the `checkout` repo made by the user `actions` and we can actually view that repo [here](https://github.com/actions/checkout), or since it is an Action, we can view it on the Actions Marketplace [here](https://github.com/marketplace/actions/checkout).

This Action essentially runs a `git checkout` command on your repo so that it's files can be accessed by your workflow. Actions that change your repo in some way will typically start with this. They will usually also end with something that `commits` and `pushes` the results back onto your repo. We don't need to do this part because it is covered by the second task

#### Deploy

Task number two is where the magic happens. There's a lot more here than in the first task
```
- name: Deploy
  uses: s0/git-publish-subdir-action@develop
  env:
    REPO: self
    BRANCH: gh-pages
    FOLDER: docs
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```
We have three children within this task, `name:`, `uses:` and `env:` and `env:` even has some children of it's own. Firstly, we're giving this task a name, `Deploy`; this isn't necessary, but it looks a little neater and makes it clearer what this task is doing (useful if you're running a lot of tasks in a single job).

The next child is the same as previously, `uses: s0/git-publish-subdir-action@develop`. We're going to run the action on the `develop` branch of the `git-publish-subdir-action` repo by the user `s0`. Once again, you can view this repo [here](https://github.com/s0/git-publish-subdir-action) or as an Action on the Marketplace [here](https://github.com/marketplace/actions/push-git-subdirectory-as-branch). This is main part of what we're doing. This Action does the actual copying of the subdirectory and pushes it to a new branch.

The last child is `env:` and this is where you might have to change things depending on your use-case. This has four children, which are actually variables. Just like in most programming, we work within an environment that contains variables, well here we're going to define some for the `git-publish-subdir-action` to use.

You don't need to worry about `REPO` and `GITHUB_TOKEN`, these just mean that the action is going to run on the current repo (`REPO: self`) and provides authentication that it's really us doing the changes (by generating a `GITHUB_TOKEN` to use as an auth token). The other two variables are important, it's telling the Action what directory we want to copy, which by default (if you're been running your GitHub Pages using the `master/docs` form) is currently set to be `docs`, but this can be any other folder (or sub-folder) in your repo, e.g `public/home`, `my-gh-pages-site` or `"My Homepage"` (don't forget the quotes). Then finally, the name of the branch we want to put it on. If you're looking here with the intention of using a GitHub Page, then this will have to be `gh-pages` (unless using a [User account](#User-repos)), but can be any name you want your new branch to be.


## Check it works

Finally, once we've done all this, we can `git push` to the `main` branch of our repo on GitHub and it should build our website (provided we have the GitHub pages set to use `gh-pages`). To check whether this has worked, simply load up your GitHub Page. You can also have a look at the run through of the Action in the Actions tab in your repo home. This gives you the output of the Command Line of every step of your Action (although remember there are only two).

## User repos

User repos are named like `<username>.github.io` and can only be built from the `master` branch. This is because User repos are expected to be self-contained and their GitHub Pages site can only be built from the `master` branch. This means that you are expected to have an `index.html` file in your home directory. However, this can limit the ways in which your site can be built. To fix this, we can use the workflow as above, but instead of pushing the subdirectory to the `gh-pages` branch, we push it to the `master` branch, whilst still using our `main` branch as our default. The only change we need to do, is to replace the `BRANCH: gh-pages` line in the YAML file with `BRANCH: master` (make sure you keep the whitespace before it).

If you are using a Custom Domain, there is one last thing that you'll need to consider. When you add a Custom Domain, GitHub stores this as the `CNAME` file in your home directory. The Action above destroys the `master` branch before rebuilding it from scratch. This includes deleting that `CNAME` file and, since it isn't in the subdirectory of your `main` branch, it doesn't get put back in. The solution? Just put it in there yourself. This simply means adding a file to your `docs` directory called `CNAME` (no file extension) and have it's only contents be your url. Since new files without extensions can, again, be tricky. At Command Line:
```
echo -n [YOUR_URL_HERE] > docs/CNAME
```
(The use of `-n` here means that a newline isn't added to the end of the line)



