#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi
export LC_ALL=en_US.UTF-8
# exec gnome-session
# exec startkde
# exec startxfce4
xrdb -merge ~/.Xresources
/usr/bin/mouse.sh
# emacs --daemon
urxvtd -q -o -f
exec awesome
# ...or the Window Manager of your choice
