{ config, pkgs, ... }:

{
  # Home-Manager 版本号要与 flake 对应
  home.stateVersion = "25.11";

  programs.home-manager.enable = true;

  # --- 用户级包 --------------------------------------------------------------
  home.packages = with pkgs; [
    fcitx5-rime
    fcitx5-gtk
    pkgs.qt6Packages.fcitx5-chinese-addons
    xwayland
  ];

  # --- Shell -----------------------------------------------------------------
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    initContent = ''
      eval "$(zoxide init zsh)"
    '';
  };

  # --- Git -------------------------------------------------------------------
  programs.git = {
    enable = true;
    # 旧：userName / userEmail / extraConfig
    # 新：
    settings = {
      user = {
        name = "gai_yk";    # 旧 userName
        email = "1214241948@qq.com";  # 旧 userEmail
      };
      init.defaultBranch = "main";
      core.editor = "vim";
    };
  };

  # --- Vim -------------------------------------------------------------------
  programs.vim = {
    enable = true;
    settings = {
      number = true;
      mouse  = "a";
    };
  };

   # fcitx5 服务
  systemd.user.services.fcitx5 = {
    Unit = {
      Description = "Fcitx5 Input Method";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
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

  # 设置 DISPLAY 变量
  # home.sessionVariables.DISPLAY = ":0";
  home.sessionVariables = {
    DISPLAY = ":0";
  };
}