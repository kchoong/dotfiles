if xrandr | grep -q "HDMI1 connected";
then
    ( exec "~/.screenlayout/extended.sh" )
else
    ( exec "~/.screenlayout/primary.sh" )     
fi
