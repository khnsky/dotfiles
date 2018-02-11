# dotfiles
---  

easily install config files with gnu stow:  

> stow \[ -d _stow-dir_ ] [ -t _target-dir_ ] _package_  

where:
*package* is the name of folder with configs you would like to install for e.g. i3, vim.
*stow dir* is the directory in which packages are.
*target dir* is home.  

so to install i3 config you would use:  

> stow i3  

you need to specify stow dir if you are not in it currently, you need to specify target dir if it's not one above the stow dir.  

vim config installation requires installation of nvim files.  
installing firefox doesn't work, for now you need to symlink files manually.  
