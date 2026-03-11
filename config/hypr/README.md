# Hyprland 配置

这是一个经过规范化整理的 Hyprland 配置，采用模块化结构，便于管理和维护。

## 📁 目录结构

```
hypr/
├── hyprland.conf          # 主配置文件（入口点）
├── variables.conf         # 全局变量定义
└── config/                # 配置模块目录
    ├── core/              # 核心配置
    │   ├── env.conf       # 环境变量
    │   ├── general.conf   # 通用设置
    │   ├── input.conf     # 输入设备配置
    │   └── misc.conf      # 杂项设置
    ├── appearance/        # 外观配置
    │   ├── decoration.conf # 窗口装饰
    │   ├── animations.conf # 动画效果
    │   └── group.conf      # 窗口组设置
    ├── behavior/          # 行为配置
    │   ├── gestures.conf   # 手势设置
    │   ├── rules.conf     # 窗口规则
    │   └── keybinds.conf  # 快捷键绑定
    └── startup/           # 启动配置
        └── execs.conf     # 自动启动程序
```

## 🎨 主要特性

### 布局与窗口管理
- **布局模式**: Dwindle 平铺布局
- **窗口组**: 支持窗口分组，带渐变组栏
- **窗口圆角**: 10px 圆角半径
- **窗口间隙**: 20px 工作区间隙，5-10px 窗口间隙

### 视觉效果
- **窗口透明度**: 0.95 (全屏时)
- **模糊效果**: 启用弹出菜单和输入法模糊
- **阴影效果**: 20px 扩散半径，柔和阴影
- **动画**: 自定义贝塞尔曲线，流畅的窗口和工作区动画

### 快捷键系统

#### 工作区导航
- `Super + 1-0`: 跳转到工作区 1-10
- `Super + Shift + 1-0`: 移动窗口到工作区 1-10
- `Ctrl + Super + 1-0`: 跳转到工作区组 1-10
- `Ctrl + Super + Shift + 1-0`: 移动窗口到工作区组 1-10
- `Ctrl + Super + 左/右`: 上一个/下一个工作区
- `Super + 鼠标滚轮`: 快速切换工作区

#### 窗口操作
- `Super + 方向键`: 移动焦点
- `Super + Shift + 方向键`: 移动窗口
- `Super + Z`: 移动窗口模式
- `Super + X`: 调整窗口大小
- `Super + F`: 全屏切换
- `Super + Alt + Space`: 切换浮动模式
- `Super + Q`: 关闭窗口
- `Super + P`: 窗口置顶

#### 窗口组
- `Alt + Tab`: 组内窗口正向循环
- `Shift + Alt + Tab`: 组内窗口反向循环
- `Super + 逗号`: 加入/脱离组
- `Super + U`: 移出当前组

#### 应用启动
- `Super + T`: 终端 (Kitty)
- `Super + W`: 浏览器 (Firefox)
- `Super + C`: 编辑器 (VS Code)
- `Super + E`: 文件管理器 (Thunar)
- `Super + G`: GitHub Desktop
- `Super + M`: 音乐播放器
- `Super + D`: 通讯应用

#### 特殊工作区
- `Super + S`: 切换特殊工作区
- `Ctrl + Shift + Escape`: 系统监视器
- `Super + R`: 待办事项

#### 系统操作
- `Ctrl + Alt + Delete`: 会话菜单
- `Super + L`: 锁屏
- `Super + Alt + L`: 恢复锁屏界面
- `Super + Shift + L`: 系统休眠

#### 屏幕截图与录制
- `Print`: 全屏截图到剪贴板
- `Super + Shift + S`: 区域截图（冻结模式）
- `Super + Shift + Alt + S`: 区域截图
- `Super + Alt + R`: 录屏（带声音）
- `Ctrl + Alt + R`: 录屏（无声音）
- `Super + Shift + C`: 颜色选择器

#### 音量与媒体
- `XF86AudioRaiseVolume`: 音量+
- `XF86AudioLowerVolume`: 音量-
- `XF86AudioMute`: 静音
- `XF86AudioPlay/Pause`: 播放/暂停
- `XF86AudioNext/Prev`: 下一首/上一首

#### 剪贴板与工具
- `Super + V`: 剪贴板历史
- `Super + Alt + V`: 剪贴板历史（仅文本）
- `Super + .`: 表情符号选择器
- `Ctrl + Alt + Escape`: 进程管理器
- `Ctrl + Alt + V`: 音量控制

### 触控板手势
- **四指滑动**: 工作区切换
- **三指上滑**: 切换特殊工作区
- **三指下滑**: 切换特殊工作区（Caelestia）
- **四指下滑**: 系统休眠

### 窗口规则

#### 浮动窗口
系统工具和对话框自动浮动：
- 文件管理器对话框
- 蓝牙管理器
- 音频控制
- 网络管理器
- 外观设置工具
- 图片查看器

#### 特殊工作区
- **系统监视器** (special:sysmon): btop
- **音乐播放器** (special:music): Feishin, Spotify, Supersonic
- **通讯应用** (special:communication): Discord, Vesktop, WhatsApp
- **待办事项** (special:todo): Todoist

#### 特殊应用规则
- **Steam**: 游戏允许撕裂，空闲抑制
- **画中画**: 自动定位到右下角，保持宽高比，置顶
- **XWayland 弹窗**: 优化阴影、圆角和透明度

## 🔧 配置说明

### 环境变量 (env.conf)
- **输入法**: fcitx5 (中文输入)
- **工具包后端**: 强制使用 Wayland（回退 X11）
- **Qt 配置**: Papirus-Dark 图标主题，高 DPI 支持
- **XDG 规范**: Hyprland 桌面环境

### 输入设备 (input.conf)
- **键盘布局**: US 布局
- **触控板**: 自然滚动，打字时禁用，滚动速度 0.3
- **鼠标指针**: sweet-cursors 主题，24px 大小
- **键盘重复**: 250ms 延迟，35 次/秒

### 通用设置 (general.conf)
- **布局**: Dwindle 平铺布局
- **撕裂**: 禁用（Steam 游戏除外）
- **边框**: 2px 大小
- **间隙**: 可配置的工作区和窗口间隙

### 杂项设置 (misc.conf)
- **VFR/VRR**: 可变刷新率支持
- **拖动动画**: 禁用手动调整和拖动时的动画
- **会话锁**: 支持恢复，X 射线模式
- **焦点**: 激活窗口时自动获取焦点

### 窗口装饰 (decoration.conf)
- **圆角**: 10px
- **模糊**: 启用弹出菜单和输入法模糊（8px 核大小，2 次迭代）
- **阴影**: 启用柔和阴影（20px 范围，3 次幂采样）

### 动画 (animations.conf)
- **贝塞尔曲线**: 4 种自定义曲线
- **窗口动画**: 淡入/淡出，移动动画
- **工作区动画**: 标准滑动
- **特殊工作区**: 垂直滑动淡入淡出

### 窗口组 (group.conf)
- **字体**: JetBrains Mono NF, 15px
- **组栏高度**: 25px
- **渐变**: 启用渐变背景
- **圆角**: 5px 圆角
- **颜色**: 基于主题色的活动/非活动状态

### 自动启动 (execs.conf)
- **系统服务**: GNOME Keyring, Polkit 认证代理
- **剪贴板**: wl-paste + cliphist 历史记录
- **清理**: 自动清理 30 天前的垃圾文件
- **位置服务**: GeoClue 位置代理
- **夜间模式**: gammastep 自动调节色温
- **网络代理**: clash
- **输入法**: fcitx5
- **媒体控制**: mpris-proxy 蓝牙媒体控制
- **窗口管理**: QuickShell 动态调整

## 📝 自定义变量

所有可自定义的变量都在 `variables.conf` 中定义，包括：

### 应用程序
- `$app_terminal`: 默认终端
- `$app_browser`: 默认浏览器
- `$app_editor`: 默认编辑器
- `$app_file_manager`: 默认文件管理器

### 界面设置
- `$ui_window_opacity`: 窗口透明度
- `$ui_window_rounding`: 窗口圆角
- `$ui_window_border_size`: 边框大小
- `$ui_workspace_gaps`: 工作区间隙
- `$ui_window_gaps_in/out`: 窗口间隙

### 模糊效果
- `$ui_enable_blur`: 全局模糊开关
- `$ui_enable_blur_popups`: 弹出菜单模糊
- `$ui_blur_size`: 模糊核大小
- `$ui_blur_passes`: 模糊迭代次数

### 快捷键别名
所有快捷键组合都使用变量别名，便于批量修改：
- `$kb_go_to_workspace`: Super
- `$kb_launch_terminal`: Super + T
- `$kb_lock_screen`: Super + L
- 等等...

## 🚀 使用说明

### 修改配置
1. 修改 `variables.conf` 中的变量值
2. 或直接修改对应的配置文件
3. 重启 Hyprland 或运行 `hyprctl reload` 生效

### 添加新快捷键
在 `config/behavior/keybinds.conf` 中添加绑定：
```conf
bind = Modifier, Key, Dispatcher, Command
```

### 添加新窗口规则
在 `config/behavior/rules.conf` 中添加规则：
```conf
windowrule = rule, condition
```

### 添加自动启动程序
在 `config/startup/execs.conf` 中添加：
```conf
exec-once = command
```

