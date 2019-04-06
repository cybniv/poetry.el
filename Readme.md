## Poetry.el

 [Poetry](https://poetry.eustace.io/) in Emacs.
 
 This is a work in progress, any advices is welcome.
 

## Usage

Poetry.el uses [transient](https://github.com/magit/transient) to provide a magit-like interface.
The entry point is simply:
``` 
M-x poetry
```
The interface should then be pretty much self-explanatory:

<img src="doc/screenshot1.png" alt="screenshot" width="600"/>


## Installation

  1. Clone the `poetry.el` repository:

```bash
    $ git clone https://github.com/galaunay/poetry.el /path/to/poetry.el/directory
```

  2. Add the following lines to `.emacs.el` (or equivalent):

```elisp
    (add-to-list 'load-path "/path/to/poetry.el/directory")
    (require 'poetry)
```
