# Bitbucket.el #

An Emacs wrapper for the Bitbucket API.

## Installation ##

Currently, you have to manually install the package.

```lisp
(add-to-list 'load-path "my-package-directory")
```

I am working on getting the package into MELPA. You can track the progress through [this pull request](https://github.com/melpa/melpa/pull/4682)

## Authentication ##

Basic Authentication is supported. 

The access token is created by base64 encoding your username and password. In emacs you can do this with the following code.

```lisp
(base64-encode-string
   (concat "username" ":" "password"))
```

(If you are using 2-factor-authentication, set an app password, and use that instead of the user password.)

You can apply the token programmatically:

```lisp
(setq bitbucket-basic-auth "my-basic-auth-token"))
```

or through an environment variable:

```shell
export BITBUCKET_BASIC_AUTH=my-basic-auth-token
```

## TODO ##

This project is a work in progress. I have only ported the Issues endpoints, which allows Bitbucket integration for [git-commit-insert-issue](https://gitlab.com/emacs-stuff/git-commit-insert-issue)

If you need more endpoints added, create an issue, or even better send a pull request.

## Inspiration ##

This project has been inspired by the great [emacs-gitlab](https://github.com/nlamirault/emacs-gitlab)
