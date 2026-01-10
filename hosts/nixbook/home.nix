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

  # ==============================================================================
  # 2. 用户级软件包
  # ==============================================================================
  # 这些包只安装在用户环境中，不影响其他用户

  home.packages = with pkgs; [
    # --- 输入法相关 ---
    fcitx5-rime                      # Rime 中州韵输入法引擎
    fcitx5-gtk                       # GTK 应用输入法支持
    pkgs.qt6Packages.fcitx5-chinese-addons  # Qt6 应用中文输入法支持

    # --- X11 兼容层 ---
    xwayland                         # Xwayland（在 Wayland 中运行 X11 应用）

    # --- 目录导航工具 ---
    zoxide                           # 智能目录跳转工具（比 cd 更智能）

    # --- C++ 开发工具 ---
    clang-tools                      # C++ 语言服务器 (LSP) 和开发工具
  ];

  # ==============================================================================
  # 3. Shell 配置（Zsh）
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
  # 6. 系统服务配置（用户级）
  # ==============================================================================

  # --- fcitx5 输入法服务 ---
  systemd.user.services.fcitx5 = {
    Unit = {
      Description = "Fcitx5 Input Method";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
      Wants = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.fcitx5}/bin/fcitx5 -d --replace";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  # --- Xwayland 服务（Niri 兼容层）---
  systemd.user.services.xwayland = {
    Unit = {
      Description = "Xwayland for niri";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.xwayland}/bin/Xwayland :0 -rootless -terminate -listen 28 -listen 29 -wm fd:30";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  # ==============================================================================
  # 7. 环境变量配置
  # ==============================================================================

  home.sessionVariables = {
    # --- 显示相关 ---
    DISPLAY = ":0";                   # X11 显示变量
    WAYLAND_DISPLAY = "wayland-1";    # Wayland 显示变量

    # --- fcitx5 输入法环境变量（Wayland）---
    INPUT_METHOD = "fcitx5";          # 输入法类型
    GTK_IM_MODULE = "fcitx5";         # GTK 应用输入法模块
    QT_IM_MODULE = "fcitx5";          # Qt 应用输入法模块
    XMODIFIERS = "@im=fcitx5";        # X11 输入法修饰符
    SDL_IM_MODULE = "fcitx5";         # SDL 应用输入法模块
    GLFW_IM_MODULE = "ibus";          # GLFW 应用输入法模块（使用 ibus）

    # --- Electron 应用输入法支持（VSCode、Zed）---
    ELECTRON_OZONE_PLATFORM_HINT = "auto";  # Electron 自动选择平台
    ELECTRON_ENABLE_LOGGING = "1";          # 启用 Electron 日志
    ELECTRON_ENABLE_STACK_DUMPING = "1";    # 启用堆栈转储
    NIXOS_OZONE_WL = "1";                   # NixOS Ozone Wayland 支持
  };

  # ==============================================================================
  # 8. Niri 窗口管理器配置
  # ==============================================================================

  # Niri 配置通过软链接管理（直接链接到 dotfiles）
  home.activation = {
    linkNiriConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      mkdir -p ~/.config/niri ~/.config/kitty ~/.config/emacs
      ln -sf ~/dotfiles/config/niri/config.kdl ~/.config/niri/config.kdl
      ln -sf ~/dotfiles/config/kitty/kitty.conf ~/.config/kitty/kitty.conf
      ln -sf ~/dotfiles/config/emacs/init.el ~/.config/emacs/init.el
    '';
  };
}
