<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
# My Emacs Config #
This is my emacs config; posted here for others to use as well as for me to
track how it works. It requires Emacs24 or later, as it uses the built in
package-manager.

## Init.el ##
**`init.el`** contains the startup information and is used in place of the the usual `.emacs` in
  `~`. It will download all packages from MELPA/Marmalade when loading, and
  perform some very basic environment configuration. It will also load the
  `settings` and `modes` folders.

## Modes ##
**`modes`** contains mode-specific settings, defined as mode-hooks. Each mode
  shall be contained in its own file with the same name as the mode *including*
  "mode".

## Settings ##
**`settings`** contains overall configuration based on general type. *Most*
settings that are not mode-specific should be in one of these files to make sure
it is loaded properly. For easy trouble-shooting, these files are loaded
manually in `init.el`.

## Packages ##
The **`packages`** folder contains all non-package manager packages that I use.

## Other files ##
The **`custom.el`** and **`mc-lists.el`** are automatically generated files from
`M-x customize` and `multiple-cursors.el`. 






