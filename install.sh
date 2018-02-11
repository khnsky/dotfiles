#!bin/sh

for file in . ; do
    if [ -d "$file" ]; then
        stow "$file"
    fi
done

git update-index --assume-unchanged mpd/.config/mpd/log
git update-index --assume-unchanged mpd/.config/mpd/pid
git update-index --assume-unchanged ranger/.config/ranger/bookmarks
git update-index --assume-unchanged ranger/.config/ranger/history
