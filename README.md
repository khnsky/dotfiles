## dotfiles
so called `dotfiles` are plain text configuration files on UNIX-like systems
named so because they are typically named with a leading `.` making them hidden.  

## version control
there are several methods of tracking `dotfiles` with version control:  
- initializing git repo in your `$HOME`.  this is nice because all the
files are already in their expected locations, however this is quite messy
because of all the files that are located in `~` but are not to be committed.
this leads to complicated and extensive `.gitignore`'s and accidentally pushing
files that should not have been pushed.  
- creating normal repo and symlinking all the files manually / using a homegrown
script.  this avoids the clutter of the previous solution, however is still
extremely error prone, and the organization of directory structure is not
obvious.  
- using already available solution, like GNU `stow`, from manpage:
`stow - manage farms of symbolic links`, originally created to manage versions
of software like `perl`.  this allows to separate configuration flies into
logically distinct packages and install only the ones you want, and let `stow`
worry about fixing broken symlinks and creating directories.  

for those reasons GNU `stow` is used.  

## installation
most of the packages are simply installable using GNU `stow`, however some like
firefox that do not follow regular location of configuration flies need manual
installation / special script.  for ones that can be installed using `stow` run:  

```sh
stow [ -d <stow-dir> ] [ -t <target-dir> ] <package>
```

`package` is the name of the directory containing the configurations, like `i3` or
`nvim`.  the `stow-dir` is the directory containing packages (by default current
directory), and `target-dir` is the directory for the packages to be installed in,
in this case the home directory (by default the directory one above current
directory).

otherwise one can also use the `install.sh` script that checks if a package
contains its install script and runs it or otherwise uses stow.  

```sh
./install.sh [packages...]
```

if no package was supplied it installs all packages.  

## directory structure
because `stow` is used a specified directory structure of  
```<package-name>/<path-relative-to-home>```  
must be used.
