alias sudo='sudo '                      # expand aliases when using with sudo

alias ping1='ping -c1'

alias cp='cp -ir'
alias rm='rm -I --preserve-root'
alias mv='mv -i'
alias ln='ln -i'

alias df='df -h'

disass() {
    objdump -d -M intel "$@" | highlight -S asm -O truecolor
}

alias webcam='mpv av://v4l2:/dev/video0 --profile=low-latency --untimed --vf=hflip'
