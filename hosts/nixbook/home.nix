# ==============================================================================
# Home Manager 用户配置文件
# ==============================================================================
# Home Manager 用于管理用户级别的配置，与系统级配置（configuration.nix）分离。
# 本文件配置用户 gai_yk 的个人环境，包括：
# - 用户级软件包
# - Shell 配置（Zsh）
# - Git 配置
# - Vim 配置
# - 输入法和 Xwayland 服务
# - 环境变量
# ==============================================================================

{ config, pkgs, lib, self,... }:

{
  # ==============================================================================
  # 1. Home Manager 基础配置
  # ==============================================================================

  # Home Manager 版本号（要与 flake.nix 中的版本对应）
  home.stateVersion = "25.11";

  # 启用 Home Manager
  programs.home-manager.enable = true;


  # 输入法配置（fcitx5 + 拼音）
  i18n.inputMethod = {
    type = "fcitx5";  # 使用 fcitx5 输入法框架
    enable = true;
    fcitx5.addons = with pkgs; [
      fcitx5-gtk                       # GTK 应用输入法支持
      pkgs.qt6Packages.fcitx5-qt       # Qt6 应用输入法支持
      pkgs.qt6Packages.fcitx5-chinese-addons  # Qt6 应用中文输入法支持（包含拼音）
    ];
  };
  

  # ==============================================================================
  # 2. Shell 配置（Zsh）
  # ==============================================================================

  programs.zsh = {
    enable = true;                    # 启用 Zsh
    autosuggestion.enable = true;     # 启用命令自动建议
    syntaxHighlighting.enable = true; # 启用语法高亮
    initContent = ''                  # 初始化脚本
      eval "$(zoxide init zsh)"       # 初始化 zoxide
    '';
  };

  # ==============================================================================
  # 4. Git 配置
  # ==============================================================================

  programs.git = {
    enable = true;                    # 启用 Git
    settings = {
      user = {
        name = "gai_yk";              # 用户名
        email = "1214241948@qq.com";  # 邮箱
      };
      init.defaultBranch = "main";    # 默认分支名
      core.editor = "vim";            # 默认编辑器
    };
  };

  # ==============================================================================
    # 5. Vim 配置
    # ==============================================================================
    
    programs.vim = {
      enable = true;                    # 启用 Vim
      settings = {
        number = true;                  # 显示行号
        mouse  = "a";                   # 启用鼠标支持
      };
    };  # ==============================================================================
  # 6. Niri 窗口管理器配置
  # ==============================================================================

  # 配置文件通过软链接管理（直接链接整个文件夹到 dotfiles）
  home.activation = {
    linkNiriConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # 链接 Niri 配置文件夹
      rm -rf ~/.config/niri
      ln -sf ~/dotfiles/config/niri ~/.config/niri

      # 链接 Kitty 配置文件夹
      rm -rf ~/.config/kitty
      ln -sf ~/dotfiles/config/kitty ~/.config/kitty

      # 链接 Emacs 配置文件夹
      rm -rf ~/.config/emacs
      ln -sf ~/dotfiles/config/emacs ~/.config/emacs

      # 链接 fcitx5 配置文件夹
      rm -rf ~/.config/fcitx5
      ln -sf ~/dotfiles/config/fcitx5 ~/.config/fcitx5
    '';
  };
}
