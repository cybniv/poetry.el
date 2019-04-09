[![Build Status](https://travis-ci.org/galaunay/poetry.el.svg?branch=master)](https://travis-ci.org/galaunay/poetry.el)
[![Coverage Status](https://coveralls.io/repos/github/galaunay/poetry.el/badge.svg?branch=master)](https://coveralls.io/github/galaunay/poetry.el?branch=master)
## Poetry.el

 [Poetry](https://poetry.eustace.io/) in Emacs.

 This is a work in progress, advices are welcome.


## Usage

Poetry.el uses [transient](https://github.com/magit/transient) to provide a magit-like interface.
The entry point is simply:
```
M-x poetry
```
The interface should then be pretty much self-explanatory:

<img src="doc/screenshot1.png" alt="screenshot" width="600"/>


## Installation

  1. Install dependencies

Poetry.el needs `transient` and `toml` to be installed.
They are both available on MELPA.

  2. Clone the `poetry.el` repository:

```bash
    $ git clone https://github.com/galaunay/poetry.el /path/to/poetry.el/directory
```

  3. Add the following lines to `.emacs.el` (or equivalent):

```elisp
    (add-to-list 'load-path "/path/to/poetry.el/directory")
    (require 'poetry)
```
