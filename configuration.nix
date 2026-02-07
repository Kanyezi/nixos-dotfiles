# ==============================================================================
# NixOS 系统配置文件
# ==============================================================================
# 本文件定义了 NixOS 系统的所有配置，包括：
# - 系统引导和硬件配置
# - 网络和国际化设置
# - 桌面环境（Niri Wayland 窗口管理器）
# - 系统软件包和服务
# - 用户账户管理
# - Nix 包管理器设置
#
# 帮助文档：
# - man configuration.nix(5)
# - https://search.nixos.org/options
# - nixos-help 命令
# ==============================================================================

{ config, lib, pkgs, pkgs-unstable, inputs, ... }:

{
  # ==============================================================================
  # 1. 基础系统配置
  # ==============================================================================

  # 导入硬件配置文件（自动生成，包含磁盘、内核模块等硬件信息）
  imports = [./hardware-configuration.nix];

  # NixOS 系统版本号（不要修改，用于追踪配置变更）
  system.stateVersion = "25.11";

  boot.kernelPackages = pkgs.linuxPackages_latest;   # 最新稳定，含 ntsync 内核支持
  boot.kernelModules = [ "ntsync" ];

  # 系统引导配置
  boot.loader.systemd-boot.enable = true;    # 启用 systemd-boot 引导加载器
  boot.loader.efi.canTouchEfiVariables = true;  # 允许修改 EFI 变量

  # ==============================================================================
  # 2. 网络和国际化配置
  # ==============================================================================

  # 主机名
  networking.hostName = "Kanyezi";

  # 时区设置
  time.timeZone = "Asia/Shanghai";

  # 默认语言环境
  i18n.defaultLocale = "zh_CN.UTF-8";

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

  # 启用 NetworkManager 网络管理器（支持 WiFi、以太网、VPN 等）
  networking.networkmanager.enable = true;

  # ==============================================================================
  # 3. 硬件配置
  # ==============================================================================

  # OpenGL/Vulkan 图形支持（Zed 编辑器和 Steam 等应用依赖）
  hardware.graphics = {
    enable = true;           # 启用 OpenGL 支持
    enable32Bit = true;      # 启用 32 位 DRI 支持（运行 32 位游戏需要）
  };
  hardware.bluetooth.enable = true;

  # 网络代理配置（如需要可取消注释）
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # ==============================================================================
  # 4. 桌面环境配置（Niri Wayland 窗口管理器）
  # ==============================================================================

  # 启用 Niri 窗口管理器（基于 Wayland 的动态平铺窗口管理器）
  programs.niri.enable = true;

  # X11 服务器配置（用于运行 Xwayland 兼容 X11 应用）
  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";  # 键盘布局

  # PipeWire 音频服务（现代音频系统，替代 PulseAudio 和 JACK）
  services.pipewire = {
     enable = true;        # 启用 PipeWire
     pulse.enable = true;  # 启用 PulseAudio 兼容层
  };

  # XDG Desktop Portal 配置（桌面应用与系统交互的桥梁，Zed 依赖）
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk    # GTK 桌面门户（文件选择器等）
      xdg-desktop-portal-wlr    # Wayland 桌面门户
    ];
    config = {
      niri = {
        default = "gtk";
        "org.freedesktop.impl.portal.FileChooser" = "gtk";
        "org.freedesktop.impl.portal.OpenURI" = "gtk";
      };
      common = {
        default = "gtk";
      };
    };
  };

  # ==============================================================================
  # 5. 用户账户配置
  # ==============================================================================

  # 定义用户账户
  users.users.gai_yk = {
     isNormalUser = true;              # 普通用户
     extraGroups = [ "wheel" ];        # wheel 组：允许使用 sudo
     packages = with pkgs; [
      tree  # 目录树显示工具
    ];
  };

  # ==============================================================================
  # 6. 系统服务配置
  # ==============================================================================

  # SSH 服务（远程登录）
  services.openssh = {
    enable = true;
  };

  # Docker 容器服务
  virtualisation.docker.enable = true;

  # Zsh Shell（增强的交互式 Shell）
  programs.zsh.enable = true;

  # 电源管理服务（修复 Noctalia Shell UPower 警告）
  services.upower.enable = true;              # 电源管理守护进程
  services.power-profiles-daemon.enable = true;  # 电源配置文件守护进程
  services.flatpak.enable = true;

  # v2rayA 代理服务（Web 界面管理 V2Ray）
  services.v2raya = {
    enable = true;
  };

  # Noctalia Shell 服务（自定义桌面 Shell）
  services.noctalia-shell = {
    enable = true;
  };

  # ==============================================================================
  # 7. 系统软件包
  # ==============================================================================

  # 启用 nix-ld（允许运行未链接的预编译二进制文件）
  programs.nix-ld.enable = true;

  # 系统级软件包列表
  environment.systemPackages = with pkgs; [
    # --- 基础工具 ---
    vim                     # Vim 文本编辑器
    wget                    # 命令行下载工具
    git                     # 分布式版本控制系统
    tree                    # 目录树显示工具
    unzip                   # ZIP 压缩文件解压工具

    pkgs.wpsoffice-cn
    pkgs.localsend

    # --- Shell 和终端 ---
    zsh                     # Zsh 交互式 Shell
    kitty                   # GPU 加速的终端模拟器
    alacritty               # 现代 GPU 终端模拟器

    # --- 浏览器 ---
    firefox                 # Firefox 网页浏览器
    google-chrome            # Google Chrome
    pkgs.chromium            # Chromium 开源浏览器（Google Chrome 的基础）

    # --- 代码编辑器 ---
    vscode                  # Visual Studio Code 代码编辑器
    pkgs-unstable.zed-editor  # Zed 高性能代码编辑器（来自 unstable 通道）
    nixd                    # Nix 语言服务器（提供 Nix 代码补全和诊断）
    emacs
    neovim

    # --- 应用启动器 ---
    fuzzel                  # Wayland 应用启动器

    # --- 系统控制工具 ---
    brightnessctl           # 屏幕亮度控制工具

    # --- 网络代理 ---
    v2raya                  # V2Ray Web 管理界面

    # --- VSCode Wayland 依赖库 ---
    libdrm                  # Direct Rendering Manager 库
    mesa                    # OpenGL/Vulkan 驱动
    xdg-utils               # XDG 桌面工具集
    wayland                 # Wayland 显示协议库
    wayland-protocols       # Wayland 协议扩展
    gtk3                    # GTK3 图形工具库
    at-spi2-atk             # 辅助功能支持库
    libxkbcommon            # XKB 键盘布局库

    # --- Noctalia Shell 相关 ---
    inputs.noctalia.packages.${config.nixpkgs.system}.default  # Noctalia Shell 桌面环境
    adwaita-icon-theme      # Adwaita 图标主题（GNOME 默认）
    papirus-icon-theme      # Papirus 图标主题
    hicolor-icon-theme      # 基础图标主题

    # --- 输入法相关 ---
    fcitx5-gtk                       # GTK 应用输入法支持
    pkgs.qt6Packages.fcitx5-qt       # Qt6 应用输入法支持
    pkgs.qt6Packages.fcitx5-chinese-addons  # Qt6 应用中文输入法支持（包含拼音）

    # --- X11 兼容层 ---
    xwayland                         # Xwayland（在 Wayland 中运行 X11 应用）

    # --- 目录导航工具 ---
    zoxide                           # 智能目录跳转工具（比 cd 更智能）

    # --- C++ 开发工具 ---
    clang-tools                      # C++ 语言服务器 (LSP) 和开发工具
    gcc
    gnumake
    cmake
    pkg-config
    zenity
    jq
    nss.tools

    # --- Rust 开发工具 ---
    rustc
    cargo
    rustfmt
    clippy

    # --- Zed 编辑器依赖 ---
    vulkan-tools            # Vulkan 工具集
    xdg-desktop-portal-wlr  # Wayland 桌面门户
    libsecret               # 密钥存储库

    # --- Niri 窗口管理器周边工具 ---
    xwayland-satellite      # Xwayland 桥接工具（运行 X11 应用）
    mako                    # Wayland 通知守护进程
    swaybg                  # Wayland 背景设置工具
    swaylock                # Wayland 屏幕锁定工具
    swayidle                # Wayland 空闲管理工具

    # --- 游戏和娱乐 ---
    steam                   # Steam 游戏平台
    wechat                  # 微信客户端
    qq                      # QQ 客户端
    pkgs.lutris
    pkgs.wineWowPackages.wayland
    steam-run

    # --- 自定义脚本 ---
    (pkgs.writeShellScriptBin "iflow" ''
      #!/run/current-system/sw/bin/bash
      export PATH="/home/gai_yk/.nvm/versions/node/v22.21.1/bin:$PATH"
      exec /home/gai_yk/.nvm/versions/node/v22.21.1/bin/iflow "$@"
    '')  # iFlow CLI 包装脚本
  ];

  # ==============================================================================
  # 8. 环境变量配置
  # ==============================================================================

  # 环境变量
  environment.variables = {
    # --- 桌面门户 ---
    GTK_USE_PORTAL = "1";   # 使用 Portal 进行文件选择等操作

    # --- Electron/Wayland 支持 ---
    NIXOS_OZONE_WL = "0";   # 禁用 Electron Wayland 支持，强制使用 X11（通过 Xwayland）
    ELECTRON_OZONE_PLATFORM_HINT = "x11";  # 强制 Electron 使用 X11 平台（通过 Xwayland，支持 fcitx5）

    # --- 显示相关 ---
    DISPLAY = ":0";                   # X11 显示变量
    WAYLAND_DISPLAY = "wayland-1";    # Wayland 显示变量

    # --- Electron 调试（可选）---
    ELECTRON_ENABLE_LOGGING = "1";   # 启用 Electron 日志
    ELECTRON_ENABLE_STACK_DUMPING = "1";  # 启用堆栈转储
  };

  # ==============================================================================
  # 9. Nix 包管理器配置
  # ==============================================================================

  # 允许安装非自由软件（如 Steam、VSCode 等）
  nixpkgs.config.allowUnfree = true;

  # Nix 设置
  nix.settings = {
     # 使用清华大学镜像源加速包下载
     substituters = [
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
     ];
     # 启用实验性功能（ flakes 和 nix 命令）
     experimental-features = ["nix-command" "flakes" ];
  };

  # ==============================================================================
  # 10. 防火墙配置
  # ==============================================================================

  networking.firewall.enable = true;  # 启用防火墙
  # 开放 TCP 端口：22（SSH）、2017（自定义服务）
  networking.firewall.allowedTCPPorts = [ 22 2017 ];

  # ==============================================================================
  # 11. 字体配置
  # ==============================================================================

  # 安装系统字体（修复 VSCode 等应用启动问题）
  fonts.packages = with pkgs; [
    noto-fonts              # Noto 基础字体
    noto-fonts-cjk-sans     # Noto CJK 无衬线字体（中日韩）
    noto-fonts-cjk-serif    # Noto CJK 衬线字体
    noto-fonts-color-emoji  # Noto 彩色表情字体
    liberation_ttf          # Liberation 字体（替代 Arial/Times）
    fira-code               # Fira Code 等宽字体（带连字符）
    fira-code-symbols       # Fira Code 符号字体
    dejavu_fonts            # DejaVu 字体（Unicode 覆盖广泛）
  ];
}
