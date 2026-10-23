# 用 stow 安装配置：make <应用> == stow <应用>
# 顶层每个目录就是一个应用（stow 的包）
# 依赖写成普通的 make 前置依赖：sway niri: waybar
#
#   make sway              先 stow waybar，再 stow sway
#   make uninstall-sway    撤销 sway

PKGS := $(shell find . -mindepth 1 -maxdepth 1 -type d -not -name '.*' -printf '%f\n' | sort)

.DEFAULT_GOAL := help
.PHONY: help list $(PKGS)

# sway、niri 依赖 waybar：waybar 走它自己的规则先装
sway niri: waybar

$(PKGS):
	stow $@

uninstall-%:
	stow -D $*

help:
	@echo "make <应用>             安装（sway、niri 会先装 waybar）"
	@echo "make uninstall-<应用>   撤销"
	@echo "make list               列出所有应用"

list:
	@echo $(PKGS) | tr ' ' '\n' | sed 's/^/  /'