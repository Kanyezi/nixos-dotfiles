# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, pkgs-unstable, inputs, ... }:

{
  imports = [./hardware-configuration.nix];
  system.stateVersion = "25.11";
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  networking.hostName = "Kanyezi";
  time.timeZone = "Asia/Shanghai";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.inputMethod = {
    type = "fcitx5";
    enable = true;
    fcitx5.addons = with pkgs; [
      qt6Packages.fcitx5-chinese-addons
      fcitx5-rime
    ];
  };

  networking.networkmanager.enable = true;


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  

  programs.niri.enable = true;

  # Kitty 终端配置
  environment.etc."xdg/kitty/kitty.conf".text = builtins.readFile ./config/kitty.conf;

  # Fuzzel 启动器配置
  environment.etc."xdg/fuzzel/fuzzel.ini".text = builtins.readFile ./config/fuzzel.ini;

  # Niri 窗口管理器配置
  environment.etc."xdg/niri/config.kdl".text = builtins.readFile ./config/niri.kdl;
  


  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";

  # 启用触摸板自然滚动
  services.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;
    };
  };

  services.pipewire = {
     enable = true;
     pulse.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.gai_yk = {
     isNormalUser = true;
     extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
     packages = with pkgs; [
      tree
    ];
  };
  
  
  services.openssh = {
    enable = true;
  };

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  programs.nix-ld.enable = true;
  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    niri
    fuzzel
    zsh
    kitty
    vscode
    alacritty
    firefox
    pkgs-unstable.zed-editor
    v2raya
    unzip
    xfce.thunar
    # XDG Portal (修复 VSCode 文件对话框)
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-wlr
    # 社交通信应用
    wechat
    qq
    steam
    steam-run
    brightnessctl
    # VSCode Wayland 依赖
    libdrm
    mesa
    xdg-utils
    # Wayland 相关库
    wayland
    wayland-protocols
    # VSCode 额外依赖
    gtk3
    at-spi2-atk
    libxkbcommon
    # Noctalia Shell 包
    inputs.noctalia.packages.${config.nixpkgs.system}.default
    # 图标主题 (修复 Noctalia Shell 图标加载问题)
    adwaita-icon-theme
    papirus-icon-theme
    hicolor-icon-theme
    # WeChat 依赖
    libnotify
    dbus
    libappindicator
    libdbusmenu
  ];

  # 6. 服务
  virtualisation.docker.enable = true;
  programs.zsh.enable = true;

  # 电源管理服务 (修复 Noctalia Shell UPower 警告)
  services.upower.enable = true;
  services.power-profiles-daemon.enable = true;

  # v2rayA 服务配置
  services.v2raya = {
    enable = true;
  };

  # Noctalia 服务配置
  services.noctalia-shell = {
    enable = true;
  };

  # XDG Portal 服务 (修复文件对话框)
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # fcitx5 环境变量 (Wayland)
  environment.sessionVariables = {
    GTK_USE_PORTAL = "1";
    # VSCode Wayland 支持
    NIXOS_OZONE_WL = "1";
    ELECTRON_OZONE_PLATFORM_HINT = "auto";
    GDK_BACKEND = "wayland";
  };
  environment.variables = {
    # 告诉 fcitx5 启动时一定扫描系统级插件目录
    FCITX_ADDON_DIRS = "/run/current-system/sw/lib/fcitx5";
  };
  
  # 覆盖 fcitx5 的默认 XMODIFIERS 设置
  environment.variables = {
    XMODIFIERS = lib.mkForce "@im=fcitx5";
  };

  # 7. Nix 自身
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
     substituters = [
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
     ];
     experimental-features = ["nix-command" "flakes" ];
  };

  # 8. 防火墙
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 2017 ];

  # 字体支持 (修复 VSCode 启动问题)
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    dejavu_fonts
  ];
}

