#!/bin/sh
# sway 的 logout：swaynag 确认后退出 sway。
# 由 waybar 的 power 菜单经 dispatcher 调用，见 waybar/.config/waybar/scripts/logout.sh

exec swaynag -t warning \
	-m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' \
	-B 'Yes, exit sway' 'swaymsg exit'