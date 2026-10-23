#!/bin/sh
# niri 的 logout：niri 的 quit 动作自带 "Press Enter to confirm" 确认提示，
# 所以这里不额外套确认框，直接触发。
# 由 waybar 的 power 菜单经 dispatcher 调用，见 waybar/.config/waybar/scripts/logout.sh

exec niri msg action quit