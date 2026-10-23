#!/bin/sh
# waybar power 菜单的 Logout 入口：按当前桌面环境分发到对应 WM 的 logout 脚本。
# 各 WM 的实现在自己的包里：
#   sway/.config/sway/scripts/logout.sh
#   niri/.config/niri/scripts/logout.sh

case "${XDG_CURRENT_DESKTOP:-}${DESKTOP_SESSION:-}" in
*niri*)
	exec "$HOME/.config/niri/scripts/logout.sh"
	;;
*sway*)
	exec "$HOME/.config/sway/scripts/logout.sh"
	;;
esac

printf 'logout: 未识别的桌面环境（XDG_CURRENT_DESKTOP=%s）\n' "${XDG_CURRENT_DESKTOP:-}" >&2
exit 1