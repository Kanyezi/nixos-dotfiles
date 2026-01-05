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
      fcitx5-rime
      fcitx5-gtk
      pkgs.qt6Packages.fcitx5-chinese-addons
    ];
  };

  networking.networkmanager.enable = true;


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  

  programs.niri.enable = true;
  environment.etc."niri/config.kdl".text = builtins.readFile ./config/niri.kdl;

  # Kitty 终端配置
  environment.etc."xdg/kitty/kitty.conf".text = builtins.readFile ./config/kitty.conf;


  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";
  
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

  # fcitx5 环境变量 (Wayland)
  environment.sessionVariables = {
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    XMODIFIERS = "@im=fcitx";
    INPUT_METHOD = "fcitx";
    SDL_IM_MODULE = "fcitx";
    GLFW_IM_MODULE = "ibus";
    # VSCode Wayland 支持
    NIXOS_OZONE_WL = "1";
    ELECTRON_OZONE_PLATFORM_HINT = "wayland";
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

