# X11 Window Manager
![Screenshot](https://user-images.githubusercontent.com/7703091/142298919-f0127c69-339f-429c-97b4-95490934e21a.png)

Floating window manager with multiple desktops, parsed human readable config file, and keyboard
orientated design for the X Window System
## Config file example
```
S-Return | Launch "alacritty"
S-d | Launch "rofi -show run"

S-h | MoveLeft
S-j | MoveDown
S-k | MoveUp
S-l | MoveRight

Ss-h | DecreaseWidth
Ss-j | IncreaseHeight
Ss-k | DecreaseHeight
Ss-l | IncreaseWidth

S-r | Raise
S-c | Close
Ss-q | Quit
S-m | Maximize

S-1 | Workspace 1
S-2 | Workspace 2
S-3 | Workspace 3
S-4 | Workspace 4
S-5 | Workspace 5
S-6 | Workspace 6
S-7 | Workspace 7
S-8 | Workspace 8
S-9 | Workspace 9

step = 15
borderWidth = 2

borderFocused = #faf9f6
borderUnfocused = #2f2c25
```

### Modifiers
```S``` - Super

```C``` - Control

```A``` - Alt

```s``` - Shift

### All actions
- MoveUp
- MoveDown
- MoveRight
- IncreaseWidth
- DecreaseWidth
- IncreaseHeight
- DecreaseHeight
- Raise
- Launch \<Provide a string>
- Quit
- CloseWindow
- Maximize
- SwitchToWorkspace \<Provide an Integer>
