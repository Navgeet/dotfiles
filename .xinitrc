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

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

if [ -s ~/.Xresources ]; then
    xrdb -merge ~/.Xresources
fi

xbindkeys
nm-applet &
exec /usr/bin/awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr

# ...or the Window Manager of your choice
