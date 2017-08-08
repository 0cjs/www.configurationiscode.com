---
layout: blog.html
title: Why You Must Use etckeeper
---

Why You Must Use etckeeper
==========================

[TL;DR][tldr]: You configure systems, and [Configuration Is Code][CIC].
So, just like coders, you should use Git or similar. Etckeeper lets
you do this for your `/etc/` directories while (mostly) handling the
permissions problems. (There are a few other sysadmin-specific issues:
please see the "Security Notes" section below before you clone an
etckeeper repo.)

All major distributions and packaging systems now include Git and
etckeeper; given that there haven't been huge changes or major bugs for
a long time, installing that is fine. You can also get it from (and read
the source code at) the [etckeeper] project on GitHub.


Introduction
------------

Developers all over the world have for decades used [version control
systems][VCS] (VCSs) for what we also call "revision control" or
"configuration control." The general idea here is to track changes: who
changed what, when, and why.

There's an important difference between this and mere automatic backup
and preservation of previous versions: in version control systems a
change is a specific action that you want to be preserved, rather than
just a record of "what happened," as automatic backup of files gives you.

For example, tweaking `/etc/resolv.conf` to add a new name server in
order to test whether something in your system is broken or your current
name servers are broken is a short-term change to help debug a problem.
Making the same change to say 'I want always to use this new name server
rather than old one' is a completely different type of change, and only
the latter is a change you would 'commit' to the version control system,
to be kept forever.

(Developers do this all the time; they'll change a bit of code &ndash;
perhaps put in a print statement &ndash; to debug something, but that
doesn't mean that they want to make that change permanent and distribute
it out to everybody else using the same code.)

[Git] is one of the more popular VCSs available today, and probably
the most commonly used. (It's used for the Linux kernel.) If you're
reasonably familiar with any other version control systems, you've at
least heard of Git. If you haven't, it's probably best you go through a
short tutorial before you go further.

XXX suggest tutorials here


Why Use Version Control?
------------------------

Version control has some obvious benefits, such as being able to revert
files to their previous versions. (This is a form of backup.) But
there's far more you can get from version control than this. In this
section we go through the various good things, beyond just backup, that
version control can give you.

### Backup and Undo

A VCS can do a "backup" up and let you bring back previous versions of
files, and even entire file trees. That is a common need, and people
have found many other ways of doing this, from simply copying `file`
to `file.orig`, to full backup systems (copying files to other storage,
e.g., tape), to Apple's Time Machine, and many other methods. Most
programs that let you edit files also let you undo changes (at least
during the current session) even if you've saved those changes to disk.

I mention "file trees" as well as files above because sometimes the
changes in indiviual files need to be synchronized to make sense.
Take, for example, changing the port on which a server listens and the
firewall rule (possibly on another host) that allows access to that
port. If you change the port without changing the firewall rule, or vice
versa, the clients will no longer have access to that server. So you
want a way to record that either both changes or neither change should
happen.

### Picking Preservation Points

One way in which VCSs are similar to manual backup methods (e.g., `cp
file file.orig`) and different from standard backup systems is that
you get to pick the point at which you want to preserve a copy of what
you're doing.

But when you get down to the level of changes you make across minutes,
rather than days, consistent changes are almost never done in single
operations. You my edit a configuration file to change the letter 'y'
to 'n', but you'll no doubt quite soon be changing the rest of the word
so that it says not 'nen' but has been changed from 'yes' to 'no'.
Yet, it's not often you find yourself wanting to undo the last three
characters changed, rather than undoing to the point where 'it used to
work.'[1]

[1]: This is clearly a contrived example, and you can go a long way with
an editor that lets you change multiple files in multiple buffers before
you save them all in one (nearly) atomic operation, but you get the
idea.

XXX why not save every change

XXX pull together changes to several files into a single "backup point"

### Add info (who/what/why/when)

### Communication

XXX git log as a twitter feed

### Test Copies

### Cryptographic signing 

XXX know about change not consistent with with you've seen before
XXX sign changes, if that's important





What VCSs share with copying a file to a .orig file and d





** particular time point 




The /etc Difference
-------------------

`/etc/`, while being configuration for the system (and quite often
actual code: `/etc/init.d/`) is also effectively a production deployment
of that same configuration, and thus has special issues that developers
don't usually deal with in their VCS repos. The main one is file
permissions and owners.

VCSs don't generally worry about file permissions or owners, assuming
that the developer owns everything and has set read/write/etc.
permissions to whatever is appropriate for his personal development
system. Production deployments obviously can't work this way.


What etckeeper Adds to Git
--------------------------

* Can use regular `git commit` etc.
* `etckeeper init` - also to fix perms


To-do
-----


* put `passwd-` etc. in `.gitignore`
* cryptographiclly signed record of changes
* stuff that git gives you (backup, copies, etc.)
* warnings about security issues (ssh priv keys in repo)
* communication with others, and communication with yourself!
* note that some package installs will not change etc
* how to write git commit messages






[CIC]: https://www.configurationiscode.com/
[etckeeper]: https://github.com/joeyh/etckeeper
[Git]: https://en.wikipedia.org/wiki/Git
[tldr]: https://en.wikipedia.org/wiki/TL;DR
[VCS]: https://en.wikipedia.org/wiki/Version_control
