# ==============================================================================
# NixOS Flake 配置文件
# ==============================================================================
# Flake 是 Nix 的现代配置管理方式，提供：
# - 声明式依赖管理（inputs）
# - 可复现的构建环境
# - 模块化配置结构
#
# 使用方式：
# - nixos-rebuild switch --flake .#Kanyezi  # 应用配置
# - nix flake update                       # 更新依赖
# ==============================================================================

{
  description = "NixOS 25.11";  # Flake 描述

  # ==============================================================================
  # 输入源（Inputs）
  # ==============================================================================
  # 定义所有外部依赖，包括 NixOS 包仓库、社区模块和第三方项目
  
  inputs = {
    # NixOS 25.11 稳定版包仓库
    # 包含所有经过测试的稳定软件包
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    
    # NixOS unstable 包仓库
    # 包含最新版本的软件包，用于获取不稳定但新版本的应用（如 Zed）
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";  # ← 必须保留
    
    # Niri 窗口管理器 Flake
    # 基于 Wayland 的动态平铺窗口管理器，提供现代化的窗口管理体验
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";  # 跟随主 nixpkgs 版本
    };
    
    # Home Manager 用户配置管理工具
    # 用于管理用户级别的配置（Shell、Git、Vim 等）
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";  # 跟随主 nixpkgs 版本
    };
    
    # Quickshell 快速 Shell 框架
    # 用于创建自定义 Shell 和桌面小部件
    quickshell = {
      url = "github:outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # Noctalia Shell 自定义桌面环境
    # 提供现代化的桌面 Shell 体验
    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # ==============================================================================
  # 输出（Outputs）
  # ==============================================================================
  # 定义 Flake 构建的所有输出，包括 NixOS 系统配置
  
  outputs = { self, nixpkgs, nixpkgs-unstable, niri, home-manager, noctalia, quickshell }@inputs:
    let
      # 系统架构
      system = "x86_64-linux";
      
      # unstable 通道的包集合（用于获取最新软件）
      pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
    in {
      # NixOS 系统配置
      nixosConfigurations.Kanyezi = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          # 将 pkgs-unstable 和 inputs 传递给所有模块
          ({ config, ... }: {
            _module.args = {
              inherit pkgs-unstable inputs self;
            };
          })
          
          # 导入 Noctalia Shell NixOS 模块
          inputs.noctalia.nixosModules.default
          
          # 主系统配置文件
          ./configuration.nix
          
          # Home Manager NixOS 模块
          home-manager.nixosModules.home-manager
          
          # Home Manager 配置
          {
            home-manager.useGlobalPkgs = true;      # 使用系统级包集合
            home-manager.useUserPackages = true;    # 为用户安装包
            home-manager.users.gai_yk = { ... }: {  # 用户 gai_yk 的配置
              imports = [ ./hosts/nixbook/home.nix ];  # 导入用户配置文件
            };
            home-manager.extraSpecialArgs = { inherit self; };  # 传递 self 参数
          }
        ];
      };
    };
}