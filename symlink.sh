#!/bin/sh

# this script makes symbolic links to files in current directory
# in destination directory

# settings

# destination directory
DESTINATION=$HOME
# files not intended to be linked in normal fashion,
# should be linked to different directory or not at all
SPECIAL=

get_destination() {
	echo -n "input destination: "
	read -r $DESTINATION
}

get_special() {
	echo -n "input special: "
	read -r $SPECIAL
}

interactive() {
	while true; do
		echo -n "input (d)estination, input (s)pecial, (r)eturn or (e)xit"
		read -r $arg
		case "$arg" in
			d) get_destination ;;
			s) get_special ;;
			r) return ;;
			e) exit 1 ;;
			*) echo "bad key"
		esac
	done
}
# if destination string is empty echo error and exit
if [ -z "$DESTINATION" ]; then
	echo "destination variable is empty"
	exit 1
fi

# loop trough all files in current directory
for file in ./* ; do

	# do checks if its not some git specific file
	# e.g. .git .gitignore or README.md

	# if special is nonzero
	if [ -n "$SPECIAL" ]; then
		# loop trough special string
		for arg in $SPECIAL; do
			# if current file is in special string empty file strig
			if [ "$file" = "$arg" ]; then
				file=
			fi
		done
	fi

	# if file string is nonzero
	if [ -n "$file" ]; then
		# and destination/file does not already exist
		if [ ! -e "$DESTINATION/$file" ]; then
			# link current file to destination/file
			ln -s "$file" "$DESTINATION/$file"
			echo "linking $file to $DESTINATION/$file"
		fi
		# resolve file already existing here
	fi

done

# do additional linking
# e.g. .vimrc for neovim or config.h for st

exit 0
