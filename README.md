# makefile+.el

*Various extensions to makefile-mode*

![Showing calculated targets](img/screenshot.png)

Screenshot showing the Makefile target selection
for [drunkenfall](https://github.com/drunkenfall/drunkenfall). Notice how the `$(BINARY)` part is calculated to
it's value in the minibuffer selection.

### Installation

Install the `makefile+` package from MELPA

### Features

- Interactively selecting a make target and running it.  Bound to `C-c
  C-e` in `makefile-mode`.
- Calculation of variables et.c.; `$(BINARY)` will show up as what it
  evaluates to.
- Execution from any buffer in a project.  If more than one is found,
  an interactive prompt for one is shown.  This is added to the
  `projectile-commander` on the `m` key.

### License

This project is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.
