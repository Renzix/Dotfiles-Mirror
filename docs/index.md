
# Table of Contents

-   [Startup](#orge49ca21)
    -   [Configure package sources](#org5e7da82)
    -   [Bootstrap use-package](#org0227769)
    -   [quelpa](#org8fe5855)
    -   [Increase garbage collector threshold](#org9905c53)
    -   [Set custom settings to load in own file](#orgf35bc43)
-   [Preferences](#org38b24ae)
    -   [Buffers](#org1e5a8ef)
    -   [Display](#orgaa9dc88)
    -   [Other](#org33e5633)
    -   [Useful Functions](#orgdd727db)
        -   [Edit Text](#org1456ceb)
        -   [Format Text](#org62254d8)
        -   [File Handling](#orgf12cde8)
        -   [Projects](#org6a21603)
        -   [Open Buffer](#org71a7f7b)
        -   [Eval](#org03713fb)
        -   [Redefined Functions](#org310f4c5)
-   [Core](#orgd1d0202)
    -   [Key Packages](#org1e3582d)
        -   [evil](#orgfcf8e53)
        -   [god mode](#org372665b)
        -   [general](#org001394c)
        -   [key-chord](#orgfc1216b)
        -   [avy](#org313f932)
    -   [Fuzzy Find](#org085e25c)
        -   [helm](#org4e52d74)
        -   [ido](#org2a432b3)
        -   [ivy](#org2269b2a)
    -   [Version Control](#org24a3175)
        -   [Git](#org30bd4d9)
        -   [vcmode](#org6cc0375)
    -   [Autocompletion](#org33b2a06)
        -   [Company](#orga820ced)
    -   [Projects](#org7bd9ae0)
        -   [projectile](#org981a26f)
        -   [treemacs](#org9bdc7d8)
    -   [Plain Text Modes](#orgae53036)
        -   [Org](#org41bf6d4)
        -   [LaTeX](#org20c02b4)
        -   [Markdown](#org43839e4)
    -   [Terminals](#orge4a389a)
        -   [vterm](#org9be92b2)
        -   [eshell](#org713df00)
    -   [Templates/Snippets](#orgf07f74d)
        -   [Gentoo Snippets](#orgf55f865)
    -   [Chat programs](#orgd2372ca)
        -   [Matrix](#org57e910e)
        -   [Discord](#orgc95d9bb)
        -   [IRC](#org94b6d35)
    -   [Visual Helpers](#org8c301b0)
        -   [beacon](#org8c2cd2a)
        -   [rainbow-delimiters](#orga33dd68)
        -   [wilfred](#org3d87fc4)
        -   [which-key](#org1a73897)
    -   [Should Be in Emacs](#org105060c)
        -   [expand-region](#org2e86a1e)
    -   [Programming](#org2210b91)
        -   [Autopair](#orgfd79395)
        -   [LSP](#org3bbc847)
        -   [JVM](#org86317a7)
        -   [Scripting langs](#org4f986ef)
        -   [Microsoft/Dotnet](#org6113ed2)
        -   [rust](#org7c4f8c0)
        -   [c and cpp](#org1c0d37b)
        -   [haskell](#org0293e24)
        -   [Google](#org218db81)
        -   [Flycheck](#orgf10184c)
        -   [imenu](#org499060b)
-   [Keybindings](#org4fe3b05)
    -   [Rant on keybindings](#org71d6102)
    -   [Emacs State](#orga4d96cf)
    -   [Normal/Visual State](#org0a3a816)
    -   [Insert State](#orgf987453)
    -   [Ex commands](#orgff6e495)
    -   [Major Modes](#orgca8f38c)
        -   [Plain Text](#org1cc4206)
        -   [Programming](#orgc6c8d31)
        -   [Messaging](#org9c8674f)
        -   [Other](#orgd84e63c)
    -   [Other](#org516494f)
        -   [Helm](#org8b8b58d)

\#-**- after-save-hook: (lambda () (save-excursion (org-babel-goto-named-src-block "export-to-docs") (org-babel-execute-src-block))); -**-

Hello This is my second real attempt to make a "Perfect" config. Idk
if it will ever be good enough but here it is. Note this is available
on [gitlab](https://gitlab.com/Renzix/Dotfiles) and it should be mirrored on [github](https://github.com/Renzix/Dotfiles-Mirror). There are multiple
versions of this file including [readtheorg](index.html), [plain text](index.txt) and [markdown](index.md)
for now.

Some decisions are inspired by

[jamiecollinson](https://jamiecollinson.com/blog/my-emacs-config/): General layout   
[Buffet](https://github.com/buffet/rice/blob/master/emacs/.emacs.d/config.org): Random stuff and Actual help   
[Doom](https://github.com/hlissner/doom-emacs): Heavily Inspired keybindings and other stuff   
[Sacha Chua](https://pages.sachachua.com/.emacs.d/Sacha.html): Random things and ideas   
[Crux](https://github.com/bbatsov/crux): Alot of my redefined functions are based off of it   

<div class="attention">
Made to work with emacs 26 and 27 on windows, linux, and macos

</div>


<a id="orge49ca21"></a>

# Startup

Starts up emacs. This should be somewhat small and only for
proccesses that are required to run first. Also there is a small [bug](https://stackoverflow.com/questions/57153556/why-does-emacs-fail-to-install-gnu-elpa-keyring-update-and-some-other-package)
which is annoying.


<a id="org5e7da82"></a>

## Configure package sources

Melpa is nice because its more bleeding edge.

    (setq package-enable-at-startup nil)
    (setq package-archives
          '(("elpa"     . "https://elpa.gnu.org/packages/")
            ("melpa"        . "https://melpa.org/packages/"))
          package-archive-priorities
          '(("elpa"     . 5) ("melpa"        . 10)))
    ;; bug in macos
    (when (string= system-type "darwin")
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


<a id="org0227769"></a>

## Bootstrap use-package

install `use-package` if not already installed.

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (require 'use-package-ensure)

Makes sure that `use-package` always downloads the package if not available

    (setq use-package-always-ensure t)


<a id="org8fe5855"></a>

## quelpa

Quelpa is another way to get packages and it basically wraps around
git repositories. Its really nice if you want bleeding edge stuff
and also it has a [2](#org22d6f61) with the keyword :quelpa.

    (use-package quelpa
      :init (setq quelpa-upgrade-p t))
    (use-package quelpa-use-package) 


<a id="org9905c53"></a>

## Increase garbage collector threshold

The default garbage collection threshold is 800kB, increasing this
to 10MB for startup increases speed.

    (setq gc-cons-threshold 10000000)
    
    ;; Restore after startup
    (add-hook 'after-init-hook
              (lambda ()
                (setq gc-cons-threshold 1000000)
                (message "gc-cons-threshold restored to %S"
                         gc-cons-threshold)))


<a id="orgf35bc43"></a>

## Set custom settings to load in own file

This stops emacs adding customised settings to `init.el`. I try to
avoid using customize anyway, preferring programmatic control of
variables. Creating it as a temporary file effectively disables it
(i.e. any changes are session local).

    (setq custom-file (make-temp-file "emacs-custom"))


<a id="org38b24ae"></a>

# Preferences

Some preferences I like/dont like about emacs. Basically trying to
make the defaults better.


<a id="org1e5a8ef"></a>

## SOMEDAY Buffers

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-08-18 Sun 21:53] </span></span>   
    Make it so this only triggers on laptop? Also add a section for my variables.

Dont display startscreen on startup. I used to use eshell but now
that I have ss as a keybinding I don't think its nessisary.

    (setq inhibit-startup-screen t
          initial-buffer-choice nil)

Buffers dont ask for confirmation when killed while doing a
job. Makes things much easier/simpler and when I close one i know
im closing it.

    (setq confirm-kill-processes nil)

Nice little display for my battery.

    (display-battery-mode)


<a id="orgaa9dc88"></a>

## Display

Some things that nobody ever should enable. Ever. menu-bar,
tool-bar and scroll-bar all require a mouse so they suck.

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)

Disables more then 2 windows spawning as this makes magit alot more
annoying and in general only 2 windows are nessisary.

    (setq split-width-threshold (- (window-width) 10))
    (setq split-height-threshold nil)
    (defun count-visible-buffers (&optional frame)
      "Count how many buffers are currently being shown.  Defaults to selected FRAME."
      (length (mapcar #'window-buffer (window-list frame))))
    (defun do-not-split-more-than-two-windows (window &optional horizontal)
      "WINDOW HORIZONTAL."
      (if (and horizontal (> (count-visible-buffers) 1))
          nil
        t))
    (advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)

In evil relative line numbers are really nice. This also allows the
current line number to not be 0 but whatever the actual line number
it is. Having 0 is kinda useless in relative line numbers. This is
disabled for [69](#org6e1ada2) because it doesnt deal with it well.

    (when (>= emacs-major-version 26)
      (global-display-line-numbers-mode)
      (setq-default display-line-numbers-type 'relative
                    display-line-numbers-current-absolute t
                    display-line-numbers-width 3
                    display-line-numbers-widen t))

There are alot of good themes out there and having a single theme
is boring so I make it switch depending on the day of the
week. Also the theme stays default if emacs is ran in the terminal
because the themes that are good suck in the terminal and black is
good enough. There is a bug with monokai on macos and windows where
it loads without me telling it to. Because of this i have to
enable/disable it&#x2026;

    (when (display-graphic-p)
      (defvar renzix-weekday (format-time-string "%w"))
      (use-package doom-themes)
      (use-package apropospriate-theme)
      (use-package monokai-theme
        :config 
        (load-theme 'monokai t)
        (disable-theme 'monokai))
      (cond ((string= "0" renzix-weekday) ;; Sunday
             (load-theme 'doom-dracula t))
            ((string= "1" renzix-weekday) ;; Monday
             (load-theme 'doom-opera t))
            ((string= "2" renzix-weekday) ;; Tuesday
             (load-theme 'apropospriate-dark t))
            ((string= "3" renzix-weekday) ;; Wednesday
             (load-theme 'doom-molokai t))
            ((string= "4" renzix-weekday) ;; Thursday
             (load-theme 'doom-nord t))
            ((string= "5" renzix-weekday) ;; Friday
             (load-theme 'monokai t))
            ((string= "6" renzix-weekday) ;; Saturday
             (load-theme 'doom-one t))))

Evil is cool and i want to be able to tell what state I am in
without having to look at the bottom left. One easy way to do this
is change the color and type of the cursor. It changes the type but
not the color by default and normal is the same as emacs
state. However because i have god-mode It does not come with a easy
way of doing this so i have a function to update it.

    (setq evil-insert-state-cursor '(bar "#ff00ff")
          evil-normal-state-cursor '(box "#6666f6")
          evil-motion-state-cursor '(hollow "#87d7ff")
          evil-replace-state-cursor '(hollow "#bf2222")
          evil-operator-state-cursor '(box "#F5F5DC")
          evil-visual-state-cursor '(hollow "#ffffff")
          evil-emacs-state-cursor '(hbar "#43DE43"))
    (setq-default cursor-type 'hbar)
    (set-cursor-color "#43DE43")
    (defun my/update-god-cursor (&rest args)
      "Update the cursor from god-mode."
      (cond 
       ((eq god-local-mode t) ;; if i just do god-local-mode it thinks its a function
        (set-cursor-color "#FFFF00")
        (setq cursor-type 'box))
       ((eq evil-state 'emacs)
        (set-cursor-color "#43DE43")
        (setq cursor-type 'hbar))))


<a id="org33e5633"></a>

## Other

By default symlinks should always be followed. Makes it much easier
to deal with dynamic stuff like my config. Instead of asking me if
I want to follow it.

    (setq vc-follow-symlinks t)

Typing yes or no is hard and y or n is easier so lets do that
instead.

    (defalias 'yes-or-no-p 'y-or-n-p)

Use spaces instead of tabs by default. Either is fine to me however
I have to choose one and only one.

    (setq-default indent-tabs-mode nil)

Emacs backups are kinda weird by default as they apear in the same
directory as the file. Also the tilda is kinda weird syntax so I
make it a single folder and have lots of backups as I dont need the
space.

    (setq backup-directory-alist `(("." . "~/.saves"))
          backup-by-copying t
          delete-old-versions t
          kept-new-versions 10
          kept-old-versions 10
          version-control t)

I probably shouldnt do this but warnings are annoying and i dont
like them so I disable them. Its mainly because a few plugins (im
talking to you ox-pandoc) will warn you for older versions of
software and other stupid stuff like that. Disabled because this is
awful.

    (setq warning-minimum-level :error)

If you set the variable kill-whole-line then the kill-line will
delete the entire line including newline if at the beginning of the
line.

    (setq kill-whole-line t)


<a id="orgdd727db"></a>

## Useful Functions


<a id="org1456ceb"></a>

### Edit Text

Made specifically for insert state but works in other
states. Deletes backwards until a space. Not a true vim word or
WORD.

    (defun my/evil-insert-delete-back-word ()
      "Made specifically for insert state but works in other
    states. Deletes backwards until a space. Not a true vim word or
    WORD."
      (interactive)
      (delete-region
       (point)
       (save-excursion (skip-syntax-backward "^ ") (point))))

This is in vim but not in evil. Reverts any changes that were made
on the current line in insert mode.

    (defun my/evil-insert-undo-line ()
      "Undo a line in insert mode."
      (interactive)
      (if (looking-back "^" 0)
          (backward-delete-char 1)
        (if (looking-back "^\s*" 0)
            (delete-region (point) (line-beginning-position))
          (evil-delete
           (+ (line-beginning-position) (current-indentation)) (point)))))


<a id="org62254d8"></a>

### Format Text

    (defun my/smart-indent ()
      "If a region is selected indent that.
    If given ARG indent the current line.
    Else indent the entire buffer."
      (interactive)
      (save-excursion
        (if (region-active-p) (progn
                                (indent-region (region-beginning) (region-end))
                                (message "Indenting region")))
        (if current-prefix-arg
            (progn
              (indent-region (line-beginning-position) (line-end-position))
              (message "Indenting line"))
          (progn
            (indent-region (point-min) (point-max))
            (message "Identing buffer")))))

Smart indentation that i found [here](https://www.emacswiki.org/emacs/NoTabs). Infers indentation based on
the amount of tabs/spaces in the current buffer. If its a new
buffer then use the [default value](#orgb7f3518).

    (defun my/infer-indentation-style ()
      (let ((space-count (how-many "^  " (point-min) (point-max)))
            (tab-count (how-many "^\t" (point-min) (point-max))))
        (if (> space-count tab-count) (setq indent-tabs-mode nil))
        (if (> tab-count space-count) (setq indent-tabs-mode t))))


<a id="orgf12cde8"></a>

### File Handling

Emacs is actually stupid and if you try to rename a open file it
wont effect the buffer. You then end up with 2 files and you have
to either close or rename the buffer. This should be in emacs by
default idk why its not.

    (defun my/rename-file-and-buffer ()
      "Renames current buffer and file it is visiting."
      (interactive)
      (let* ((name (buffer-name))
             (filename (buffer-file-name))
             (basename (file-name-nondirectory filename)))
        (if (not (and filename (file-exists-p filename)))
            (error "Buffer '%s' is not visiting a file!" name)
          (let ((new-name (read-file-name "New name: "
                                          (file-name-directory filename) basename nil basename)))
            (if (get-buffer new-name)
                (error "A buffer named '%s' already exists!" new-name)
              (rename-file filename new-name 1)
              (rename-buffer new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil)
              (message "File '%s' successfully renamed to '%s'"
                       name (file-name-nondirectory new-name)))))))

This is the same problem as the function above. Emacs does not
close the buffer you have open if you delete the file so you might
accidently save it. Better to just call this function if the buffer
is open.

    (defun delete-file-and-buffer ()
      "Kill the current buffer and deletes the file it is visiting."
      (interactive)
      (let ((filename (buffer-file-name)))
        (when filename
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename)
              (message "Deleted file %s" filename)
              (kill-buffer))))))

Reads a string directly from a file then returns it as a string

    (defun my/get-string-from-file (filePath)
      "Return filePath's file content."
      (with-temp-buffer
        (insert-file-contents filePath)
        (buffer-string)))


<a id="org6a21603"></a>

### SOMEDAY Projects

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-08-18 Sun 21:53] </span></span>   
    Make a `projectile-create-tags` that also works on windows

This first sees if it is in a projectile project. If it isnt then
it will ask for one then run `projectile-find-file`. If it is then
it will just run `projectile-find-file`. Just a better default.

    (defun my/helm-projectile-find-file-or-project ()
      "Does switch project if not in a project and 'find-file' if in one."
      (interactive)
      (if (projectile-project-p)
          (helm-projectile-find-file)
        (helm-projectile-switch-project)))

This one runs `helm-projectile-find-file` if in a project but
normal `helm-find-file` if not inside a project.

    (defun my/helm-projectile-find-file-or-find-file ()
      "Does switch project if not in a project and 'find-file' if in one."
      (interactive)
      (if (projectile-project-p)
          (helm-projectile-find-file)
        (helm-find-files nil)))

This first sees if it is in a projectile project. If it isnt then
it will ask for one then both of them run `helm-projectile-ag` or
`helm-projectile-rg` depending on if you are in windows or
something else. I made this fix because helm-projectile-rg didnt
work on windows but maybe i should try again later (rg does work
on windows just not the emacs plugin).

    (defun my/helm-projectile-search-or-project ()
      "Does switch project if not in a project and search all files in said project."
      (interactive)
      (if (projectile-project-p)
          (if (string-equal system-type "windows-nt")
              (helm-projectile-ag)
            (helm-projectile-rg))
        (helm-projectile-switch-project)))

Creates tags for all the files. I need to get something like this
that works properly on windows.

    (defun my/create-tags (dir-name)
      "Create tags file in DIR-NAME."
      (interactive "DDirectory: ")
      (eshell-command
       (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))


<a id="org71a7f7b"></a>

### SOMEDAY Open Buffer

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-08-18 Sun 21:54] </span></span>   
    Add doas-edit or make [37](#org2f071e9) check for bsd/doas

I like using eshell and vterm but dealing with emacs buffers is
actually insane. I made a coupld simple functions to switch to a
vterm/eshell window and then be able to switch back. This makes
them fullscreen which is hella nice. This is the variable that
stores the perspective.

    (defvar my/window-conf nil)

Here is the eshell toggle function which uses said variable to
switch if not already in a eshell buffer fullscreen.

    (defun my/eshell-toggle (buf-name)
      "Switch to eshell and save persp.  BUF-NAME is the current buffer name."
      (interactive (list (buffer-name)))
      (if (string-equal buf-name "*eshell*")
          (set-window-configuration my/window-conf)
        (progn
          (setq my/window-conf (current-window-configuration))
          (delete-other-windows)
          (eshell))))

This is for the next funciton. vterm doesnt automatically switch
if called and open so i need a helper function.

    (defun my/switch-to-vterm ()
      "Switch to vterm."
      (if (get-buffer "vterm")
          (switch-to-buffer "vterm")
        (vterm)))

Function to switch to a fullscreen terminal and back again without
losing your current layout.

    (defun my/vterm-toggle (buf-name)
      "Switch to vterm and save persp.  BUF-NAME is the current buffer name."
      (interactive (list (buffer-name)))
      (if (string-equal buf-name "vterm")
          (set-window-configuration my:window-conf)
        (progn
          (setq my:window-conf (current-window-configuration))
          (delete-other-windows)
          (switch-to-vterm))))

Opens magit status in a single buffer because its so much easier to
work with a do git things. I dont really need to see the file I was
working on as I can just see the changes in `magit-status`

    (defun my/magit-status-only ()
      "Opens magit-status in a single buffer."
      (magit-status)
      (delete-other-windows))

Opens the current buffer with sudo. Again this probably should be
default or at least some form of it as this doesnt work if you dont
have sudo. Maybe there is a cross platform su thing for tramp? idk

    (defun my/sudo-edit (&optional arg)
      "Edits a file with sudo priv.  Optionally take a ARG for the filename."
      (interactive "P")
      (if (or arg (not buffer-file-name))
          (find-file
           (concat "/sudo:root@localhost:"
                   (ido-read-file-name "Find file(as root): ")))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

Opens my emacs configuration for editing.

    (defun my/open-emacs-config ()
      "Opens my Emacs config uwu."
      (interactive)
      (find-file "~/Dotfiles/.emacs.d/config.org"))


<a id="org03713fb"></a>

### SOMEDAY Eval

Helper function for smart-eval. Says if its valid lisp or not.
Functions that help you do evalution of functions and deal with
stuff.

    (defun my/valid-elisp-p (s)
      "S is a string."
      (and (listp (read s))
           (not (booleanp (read s)))))

@TODO(renzix): Finish this

    (defun my/smart-eval ()
      "This function interactively evaluates elisp.  First it checks
                to see if there is anything in the kill-ring that is valid elisp."
      (interactive)
      (let ((kr (if (current-kill 0 t) (current-kill 0 t) nil))
            (values nil))
        (cond 
         ;; ((use-region-p) (setq values (eval (buffer-substring start ion-end)))))
         ((valid-elisp-p kr) (setq values `(,(eval (read kr)))))
         ((t) (call-interactively 'eval-expression)))
        (message "%s" (car values))
        (kill-new (prin1-to-string (car values)))))


<a id="org310f4c5"></a>

### Redefined Functions

This is C-a redefined to go to first nonwhitespace then if pressed
again go to actual start of line. Stolen from [here](https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/) and from [crux](https://github.com/bbatsov/crux).

    (defun my/move-beginning-of-line (arg)
      "Move point back to indentation of beginning of line.
    
    Move point to the first non-whitespace character on this line.
    If point is already there, move to the beginning of the line.
    Effectively toggle between the first non-whitespace character and
    the beginning of the line.
    
    If ARG is not nil or 1, move forward ARG - 1 lines first.  If
    point reaches the beginning or end of the buffer, stop there."
      (interactive "^p")
      (setq arg (or arg 1))
    
      ;; Move lines first
      (when (/= arg 1)
        (let ((line-move-visual nil))
          (forward-line (1- arg))))
    
      (let ((orig-point (point)))
        (move-beginning-of-line 1)
        (when (= orig-point (point))
          (back-to-indentation))))

This is a function close to evil o and O which is pretty useful in
alot of scenerios. I have 0 idea why this isnt in emacs already. This
was also mainly stolen from [this](https://emacsredux.com/blog/2013/03/26/smarter-open-line/) and [this](https://emacsredux.com/blog/2013/06/15/open-line-above/) which was based off of [crux](https://github.com/bbatsov/crux) emacs
extension.

    (defun my/smart-open-line (arg)
      "Insert an empty line after the current line.
    Position the cursor at its beginning, according to the current mode."
      (interactive "P")
      (if arg
          (progn
            (move-beginning-of-line nil)
            (newline-and-indent)
            (forward-line -1)
            (indent-according-to-mode))
        (progn
          (move-end-of-line nil)
          (newline-and-indent))))


<a id="orgd1d0202"></a>

# Core


<a id="org1e3582d"></a>

## Key Packages


<a id="orgfcf8e53"></a>

### evil

Evil is vi emulation in emacs. It is by far the best vi emulation
outside of vi itself and very extendable/fast.

    (use-package evil
      :config 
      (setq evil-default-state 'emacs) ;; For now this will always be emacs state so i get used to it
      (evil-mode 1))

`evil-collection` is a project which provides evil keybindings for
almost every popular plugin in emacs outside of a few. Its really
nice if you want to use evil in buffers where its very emacsy. A
list of all keybindings and supported packages can be found
[here](https://github.com/emacs-evil/evil-collection). One of the nonsupported packages is magit so here is
[evil-magit](config.md) config. Also [55](#org9c1f150) has to load before evil so it
needs to set evil-want-keybinding to nil.

    (use-package evil-collection
      :after '(evil evil-magit)
      :config (evil-collection-init))

`evil-goggles` makes it so that every edit you do is highlighted
for a brief period of time. This makes it much easier to know
exactly what you are doing and also looks cool af.

    (use-package evil-goggles
      :after evil
      :config (progn
                (evil-goggles-mode)
                (evil-goggles-use-diff-faces)))

`evil-matchit` makes % work for alot of different things. All of
them are listed [here](https://github.com/redguardtoo/evil-matchit).

    (use-package evil-matchit
      :after evil
      :config (global-evil-matchit-mode 1))


<a id="org372665b"></a>

### god mode

God mode is a way to bring a prefix like style of keybinds to
emacs. It allows you to press a prefix key then all your keybinds
are like holding Control. In order to "let go" of the control you
press spacebar first. In order to use alt instead do a g prefix. A
G prefix its the same as Control and Alt. Color wise I have it use
a yellow box and a green underscore for normal emacs mode. There
is no good way of telling when you change buffer focus so I just
have it run after every command.

    (defun god-mode-revert-if-buffer-changed (orig &rest args)
      (when (not (eq (current-buffer) god-mode-current-buffer))
        (if god-local-mode
            (progn
              (god-local-mode)
              (my/update-god-cursor))
          (apply orig args))))
    
    (use-package god-mode
      :config
      (defvar god-mode-current-buffer nil)
      (add-hook 'post-command-hook 'my/update-god-cursor) ;; There is no good way to do this
      (advice-add 'other-window :after 'my/update-god-cursor) 
      (advice-add 'god-mode-maybe-activate :around 'god-mode-revert-if-buffer-changed))


<a id="org001394c"></a>

### general

General keybindings most of the ones i use are going to be defined here
as a general rule of thumb i am using , instead of C-c and those are going
to be defined in other packages.

    (use-package general)


<a id="orgfc1216b"></a>

### key-chord

key-chord allows you to make key strokes that only trigger if you
press them fast enough. This makes for some pretty interesting
ideas and allows you to bind a nonprefix key to a prefix. This is
a very underused package imo because pressing the same character 2
times in a row is very easy.

    (use-package key-chord
      :config (key-chord-mode 1))


<a id="org313f932"></a>

### avy

Avy is nice to use. Its hard to explain just look at the [github](https://github.com/abo-abo/avy) gifs.

    (use-package avy)


<a id="org085e25c"></a>

## Fuzzy Find


<a id="org4e52d74"></a>

### helm

Helm is a fuzzy finder search for ANYTHING you want in emacs. It
also has alot of plugins that work with other plugins. The two
alternatives is ivy and ido. Helm is the heaviest however it also
has the most features. Ivy is the simpliest and has the smallest
code base. Ivy is also very extendable and easier to work with
then helm or ido. Ido comes default with emacs and is said to be
the fastest but has a more complex code base then ivy.

Helm has actions based on whatever you want to do at that
point. You can think of it like a litteral search based tui.

    (defun my/helm-projectile-from-find-files ()
      (interactive)
      (helm-run-after-exit
       (lambda ()
         (let ((default-directory helm-ff-default-directory)
               (projectile-require-project-root nil))
           (helm-projectile-find-file-dwim)))))
    ;; This is mainly for [[helm-projectile]]
    (defun my/helm-goto-find-files ()
      (interactive)
      (helm-run-after-exit
       (lambda ()
         (let ((default-directory (helm-basedir (helm-get-selection))))
           (helm-find-files nil)))))
    
    (defun my/helm-goto-helm-occur ()
      (interactive)
      (helm-run-after-exit
       (lambda ()
         (helm-occur))))
    
    (defun my/helm-goto-helm-rg ()
      (interactive)
      (helm-run-after-exit
       (lambda ()
         (my/helm-projectile-search-or-project))))
    
    (use-package helm
      :init
      (setq helm-follow-mode-persistent t
            helm-autoresize-max-height 40
            helm-display-header-line nil)
      :config
      (require 'helm-config)
      (helm-autoresize-mode t)
      (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
      (helm-mode t))

Helm has a plugin you can use to use ripgrep as the search
tool. This is required if I want to use helm-projectile-rg.

    (use-package helm-rg
      :after helm)


<a id="org2a432b3"></a>

### SOMEDAY ido


<a id="org2269b2a"></a>

### SOMEDAY ivy


<a id="org24a3175"></a>

## Version Control


<a id="org30bd4d9"></a>

### Git

-   Magit

    Magit is one of the greatest emacs packages to exist. It allows
    the power of git in a tui/gui/cli form depending on what is
    needed. Note this is disabled because it is not [43](#org958e5a6) enough
    
        (use-package magit)

-   Forge

    This is in beta but forge allows [53](#orgcc9bcc8) to talk to github and
    gitlab in order to deal with Pull Requests and Issues.
    
        (use-package forge
          :after magit)

-   evil-magit

    [Magit](#org9c555f3) isnt [43](#org958e5a6) enough. It doesnt have standard [43](#org958e5a6) keybindings
    and rebinds stuff like j and k. evil-magit fixes this by
    rebinding them and this is one of the only packages that isnt
    supported by [44](#org02b192d). For some fucking reason this has to
    load before evil so it also needs evil-want-keybinding for
    [44](#org02b192d).
    
        (use-package evil-magit
          :init (setq evil-want-keybinding nil))

-   Git Timemachine

    This package allows you to go back and forth between a files git
    history. 
    
        (use-package git-timemachine
          :bind ("C-c g t" . 'git-timemachine-toggle))

-   Git Gutter

    Shows changes, deletions or additions from master. Really useful
    to see what you did and what will or wont be committed without
    having to open up [53](#orgcc9bcc8).
    
        (use-package git-gutter 
          :config (global-git-gutter-mode)) 


<a id="org6cc0375"></a>

### SOMEDAY vcmode


<a id="org33b2a06"></a>

## Autocompletion


<a id="orga820ced"></a>

### Company

Company is the newest and greatest auto completion engine for
emacs. Technically these have binds but I am not really counting
those as real keybindings because its only in effect during a
completion.

    (use-package company
      :init
      (add-hook 'after-init-hook 'global-company-mode)
      (setq company-require-match 'never
            company-minimum-prefix-length 2
            company-tooltip-align-annotations t
            company-idle-delay 1
            company-tooltip-limit 20
            global-company-mode t)
      :bind (:map company-active-map
                  ("S-TAB" . company-select-previous)
                  ("<backtab>" . company-select-previous)
                  ("<return>" . nil)
                  ("RET" . nil)
                  ("C-SPC" . company-complete-selection)
                  ("TAB" . company-complete-common-or-cycle)))

This is also intergrated with [71](#org0010f6c) for a whole bunhc of
functions.

    (defun company-mode-with-yas (backend)
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    
    (with-eval-after-load "company"
      (with-eval-after-load "yasnippet"
        '(setq company-backends (mapcar #'company-mode-with-yas company-backends))))


<a id="org7bd9ae0"></a>

## Projects


<a id="org981a26f"></a>

### projectile

Projectile is a way to use specific commands for a specific
project. A project is any folder with a source control or a
.projectile file/folder. This is the definition of helm-projectile
however it also installs projectile. This is also intergrated into
[51](#org0ac7289).

    (use-package helm-projectile
      :init
      (setq projectile-enable-caching t
            projectile-file-exists-local-cache-expire (* 5 60)
            projectile-file-exists-remote-cache-expire (* 10 60)
            projectile-switch-project-action 'helm-projectile-find-file
            projectile-sort-order 'recently-active)
      :config
      (projectile-mode t)
      (helm-projectile-on))


<a id="org9bdc7d8"></a>

### treemacs

<a id="org0797ef4"></a><a id="org680ff21"></a><a id="org2fa75ea"></a>

Treemacs is a tree layout file explorer. Its useful for projects
and has TONS of plugins to work with other plugins. It works with
[43](#org958e5a6),[projectile](#org981a26f), and [53](#orgcc9bcc8). It also should have `all-the-icons` to
look pretty :p.

    (use-package treemacs)
    (use-package treemacs-evil
      :after '(treemacs evil))
    (use-package treemacs-projectile
      :after '(treemacs projectile))
    (use-package treemacs-magit
      :after '(treemacs magit))
    ;; Icons for treemacs
    (use-package all-the-icons)


<a id="orgae53036"></a>

## Plain Text Modes


<a id="org41bf6d4"></a>

### Org

Org mode is the best thing since sliced bread. It allows you to do
Outlines, Planning, Capturing, Spreadsheets, Markup, Exporting,
Literite Programming and much [more](https://orgmode.org/).

    (use-package org
      :ghook #'org-indent-mode
      :init
      (setq-default initial-major-mode 'org-mode
                    initial-scratch-message ""
                    org-src-tab-acts-natively t
                    org-confirm-babel-evaluate nil
                    org-return-follows-link t)
      (setq org-log-done 'time
            org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "NEXT(n)" "|")
                                (sequence "WORKING(w!)" "BLOCKED(B@)" "|")
                                (sequence "REPORT(r)" "BUG(b)" "KNOWN(k)" "|" "FIXED(f!)")
                                (sequence "|" "DONE(d)" "CANCEL(c@)")
                                (sequence "|" "STUDY(y!)")))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((org . t)
         (C . t)
         (latex . t)
         (emacs-lisp . t)
         (sql . t)
         (shell . t)
         (python . t))))

<a id="org21b1f75"></a>

Org rifle is a thing that helps me search a org mode multiple org
mode buffers with [51](#org0ac7289) I am mainly gonna use it to search for
locations. Note this requires [51](#org0ac7289)

    (use-package helm-org-rifle
      :after '(org helm))

-   Org Exports

    <a id="orgdfe1990"></a><a id="org87f7724"></a><a id="orge2ca5d5"></a><a id="orgb9634f0"></a>
    There are many plugins you can install to get more exports. Here
    are the 3 that I use frequently. Pandoc is nice for docx, htmlize
    is for html and ox-twbs is for better html docs with
    twitter-bootstrap. ox-hugo because the markdown specs are awful
    and very vague so this one works on the static site generator
    [hugo](https://gohugo.io). See [here](https://ox-hugo.scripter.co) for doucmentation on it.
    
        (use-package ox-pandoc
          :after org)
        (use-package htmlize
          :after org)
        (use-package ox-twbs
          :after org)
        (use-package ox-hugo
          :after org)

-   Evil org mode

    [Org](#org41bf6d4) mode is nice but [43](#org958e5a6) is also very nice. Here is the only
    other one then [53](#orgcc9bcc8) that doesnt have [44](#org02b192d)
    keybindings.
    
        (use-package evil-org
          :after '(org evil)
          :ghook ('org-mode-hook #'evil-org-mode)
          :config
          (evil-org-set-key '(navigation insert textobjects additional calendar))
          (evil-org-agenda-set-keys))


<a id="org20c02b4"></a>

### LaTeX

-   TODO Auctex

    -   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-08-18 Sun 21:52] </span></span>   
        Add MLA style LaTeX template
    
    <a id="orgae2ee5b"></a>
    
    Auctex is supposed to be really good at showing and displaying
    LaTeX. I should use latex but I normally just use org-mode.
    
        (use-package tex
          :ensure auctex
          :config
          (setq TeX-auto-save t
                TeX-parse-self t
                TeX-view-program-selection '(((output-dvi has-no-display-manager)
                                              "dvi2tty")
                                             ((output-dvi style-pstricks)
                                              "dvips and gv")
                                             (output-dvi "xdvi")
                                             (output-pdf "mupdf")
                                             (output-html "xdg-open")))
          (add-to-list 'TeX-view-program-list '("mupdf" "mupdf %o")))
    
    It also has a [58](#orgbff7167) backend
    
        (use-package company-auctex
          :after '(company tex)
          :config (company-auctex-init))


<a id="org43839e4"></a>

### Markdown

<a id="orgc9de7d0"></a>

Markdown is dope and even though I would love to use org-mode for
everything sometimes I have to edit/view markdown.

    (use-package markdown-mode)


<a id="orge4a389a"></a>

## Terminals


<a id="org9be92b2"></a>

### vterm

Very powerful terminal emulator as the project was started by
neovim to create a actual terminal emulator in neovim. This should
in theory be just as good. Unfortunately the [melpa](https://melpa.org/) package doesnt
install properly so you have to [manually](https://github.com/akermu/emacs-libvterm) install it. These keybinds
also don't count. [Line numbers](#orgd1815e1) also dont work properly so we
disable them. Hopefully it will get [fixed soon](https://github.com/akermu/emacs-libvterm/pull/129).

    ;;(use-package vterm)
    (eval-after-load "general"
      '(when (file-directory-p "~/Projects/NotMine/emacs-libvterm")
         (add-to-list 'load-path "~/Projects/NotMine/emacs-libvterm")
         (require 'vterm)
         (general-define-key
          :states '(normal)
          :keymaps 'vterm-mode-map
          "o" #'evil-insert-resume
          "a" #'evil-insert-resume
          "i" #'evil-insert-resume
          "<return>" #'evil-insert-resume)
         (add-hook 'doc-view-mode-hook (lambda ()
                                         (global-linum-relative-mode -1)))))


<a id="org713df00"></a>

### eshell

Eshell is a nice interactive prompt to use with emacs. Imo this is
a really useful package as you can run shell and elisp commands
pretty easily.

    (defun eshell/e (file)
      (find-file file))
    (defun eshell-maybe-bol ()
      (interactive)
      (let ((p (point)))
        (eshell-bol)
        (if (= p (point))
            (beginning-of-line))))
    (add-hook 'eshell-mode-hook
              '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))


<a id="orgf07f74d"></a>

## Templates/Snippets

Yasnippets is possibly cool? It's supposed to intergrate with
[58](#orgbff7167) mode if I add some code which seems cool. All this does is
add the abillity to add predefined definitions in a whole bunch of
languages.

    (use-package yasnippet
      :config (yas-global-mode))

Yasnippets requires another package for some predefined snippets so
I can actually use it without defining it myself.

    (use-package yasnippet-snippets
      :config (yasnippet-snippets-initialize))


<a id="orgf55f865"></a>

### Gentoo Snippets

Gentoo comes with a skeleton for ebuilds which is nice. I would
like to include it by default.

    (add-hook 'ebuild-mode-hook 'ebuild-mode-insert-skeleton)


<a id="orgd2372ca"></a>

## Chat programs


<a id="org57e910e"></a>

### Matrix

<a id="org6bdb0b0"></a>

Matrix is nice but I know nobody on it. Too bad the emacs cilient
is actually amazing&#x2026;

    (when (not (string-equal system-type "windows-nt"))
      (use-package matrix-client
        :quelpa ((matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
                                :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))))


<a id="orgc95d9bb"></a>

### Discord

-   Elcord

    elcord is rich presence in discord.
    
        (use-package elcord
          :config
          (setq elcord-use-major-mode-as-main-icon t)
          (elcord-mode))

-   Discord-api

    A project i have been working on uwu
    
        (when (file-directory-p "~/Projects/Mine/rencord")
          (add-to-list 'load-path "~/Projects/Mine/rencord")
          (require 'rencord))


<a id="org94b6d35"></a>

### IRC

-   rcirc

    This is the lightest irc client i think and comes
    preinstalled. Also works pretty nice and barely requires a
    config.
    
        (setq rcirc-default-nick "Renzix"
              rcirc-server-alist
              '(("irc.freenode.net" :channels ("#emacs"))
                ("localhost" :channels ("#home")))
              rcirc-authinfo
              '(("irc.freenode.net" nickserv "Renzix" "Akeyla10!")
                ("localhost" bitlbee "Renzix" ("Akeyla10!"))))
        ;; Include date in time stamp.
        (setq rcirc-time-format "%Y-%m-%d %H:%M ")
        ;; Keep input line at bottom.                                                                               
        (add-hook 'rcirc-mode-hook
                  (lambda ()
                    (set (make-local-variable 'scroll-conservatively)
                         8192)))
        ;; Turn on spell checking.
        (add-hook 'rcirc-mode-hook (lambda ()
                                     (flyspell-mode 1)))


<a id="org8c301b0"></a>

## Visual Helpers


<a id="org8c2cd2a"></a>

### beacon

Beacon just shos a light to the location where the cursor moved
to. Simple as that.

    (use-package beacon
      :config (beacon-mode 1))


<a id="orga33dd68"></a>

### rainbow-delimiters

closing things get different highlighting so you can tell if it is
closed or not.

    (use-package rainbow-delimiters
      :ghook 'prog-mode-hook)


<a id="org3d87fc4"></a>

### TODO wilfred


<a id="org1a73897"></a>

### which-key

which-key shows keybindings as you press them making it much
easier. Helps ALOT when learning keybinds

    (use-package which-key
      :config (which-key-mode))


<a id="org105060c"></a>

## Should Be in Emacs


<a id="org2e86a1e"></a>

### expand-region

This is a single function package which expands the current region

    (use-package expand-region
      :config (delete-selection-mode 1))


<a id="org2210b91"></a>

## Programming


<a id="orgfd79395"></a>

### Autopair

Autopair just adds a closing ) to your (. It also supports other
types such as []{}<> and many more.

    ;; Misc programming stuff
    (use-package autopair
      :config (autopair-global-mode t))


<a id="org3bbc847"></a>

### LSP

<a id="org60ccb7e"></a>

lsp is basically a server that does syntax checking and stuff. The
best part about it is its editor independant so that all the
editors can improve it making it alot better.

    (use-package lsp-mode
      :hook
      ((scala-mode . lsp)
       (java-mode . lsp)
       (python-mode . lsp)
       (c-mode . lsp))
      :config (setq lsp-prefer-flymake nil))

`lsp-ui` adds a inline ui element so you can see it.

    (use-package lsp-ui
      :after lsp-mode
      :hook (lsp-mode-hook . lsp-ui-mode))

lsp also has [58](#orgbff7167) support

    (use-package company-lsp
      :after '(company lsp-mode))

also has `dap-mode` which is in [alpha](https://github.com/emacs-lsp/dap-mode) and can be used to
debug. Hopefully it gets really good eventually.

    (use-package dap-mode
      :config
      (dap-mode 1)
      (dap-ui-mode 1)
      (require 'dap-python)
      (require 'dap-java)
      (require 'dap-lldb))


<a id="org86317a7"></a>

### JVM

-   java

    All i have for java is a simple . Maybe eventually I will hook
    up the entire eclipse server thing too.
    
        (use-package lsp-java)

-   scala

    Scala lsp is part of [83](#org44c692f) so you can enable/disable it from
    there. Here is just syntax highlighting for scala.
    
        (use-package scala-mode
          :mode "\\.s\\(cala\\|bt\\)$")
    
    <a id="org62bc172"></a><a id="org2869d93"></a>
    
    this is a mode for [scala](#orgab16357) package manager sbt.
    
        (use-package sbt-mode
          :commands sbt-start sbt-command
          :config
          ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
          ;; allows using SPACE when in the minibuffer
          (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

-   SOMEDAY kotlin

-   SOMEDAY clojure


<a id="org4f986ef"></a>

### Scripting langs

-   python

    <a id="orgdc007f7"></a>
    
    We are using [microsofts lsp](https://github.com/microsoft/language-server-protocol) because its supposed to be good. Thats
    about it probably should add more. Also this is partially configured
    in [83](#org44c692f)
    
        (use-package lsp-python-ms)

-   rakudo

    <a id="org25e8e6d"></a><a id="orgdde7b21"></a>
    
    perl6 is such a cool language but its SOO slow. Feels bad. Maybe
    eventually it becomes fast and good enough to be used in
    industry. Note this doesnt have that good syntax highlighting and
    no lsp.
    
        (use-package perl6-mode)
        (use-package flycheck-perl6
          :after flycheck)

-   SOMEDAY perl

-   SOMEDAY common-lisp

-   SOMEDAY shell


<a id="org6113ed2"></a>

### Microsoft/Dotnet

-   csharp

    <a id="org58a0bfd"></a><a id="orgf24df2d"></a>
    
    csharp is still growing a emacs presence. For right now omnisharp
    is what we got and its still in beta. We also have standard syntax
    highlighting for it.
    
        (use-package csharp-mode)
        (use-package omnisharp
          :hook (csharp-mode-hook . omnisharp-mode)
          :config
          (add-to-list 'company-backends 'company-omnisharp)
          (add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode)))

-   powershell

    Just a major mode and simple repl for powershell. Nothing too
    major.
    
        (use-package powershell)


<a id="org7c4f8c0"></a>

### rust

Rust support is alright. Most things should work ootb with rustic
and [83](#org44c692f).

    (use-package rustic)


<a id="org1c0d37b"></a>

### c and cpp

<a id="orgbbcacc6"></a><a id="org78841b8"></a>

This one uses irony server which needs to be installed. Note that
it can be installed inside emacs. Uses to do stuff.

    (use-package irony
      :hook (c++-mode-hook . irony-mode)
      :hook (objc-mode-hook . irony-mode)
      :hook (c-mode-hook . irony-mode))

Irony also has support for [58](#orgbff7167).

    (use-package company-irony
      :after '(company irony))

This allows us to read docs while irony is working.

    (use-package irony-eldoc
      :after '(irony))


<a id="org0293e24"></a>

### haskell

<a id="orga768cd2"></a><a id="org99f8576"></a>

Haskell is a cool language. I should probably actually learn it one
day.

    (use-package haskell-mode)
    (use-package lsp-haskell
      :after lsp-mode)
    (use-package flycheck-haskell
      :after flycheck)


<a id="org218db81"></a>

### Google

-   dart

    Dart is googles new language. I doubt im ever going to use it but
    it seems cool enough to try out.
    
        (use-package dart-mode)

-   golang

    Go is fast/easy. First we can start off with the major mode.
    
        (use-package go-mode)
    
    Then we can give it documentation popups.
    
        (use-package go-eldoc)
    
    flymake-go is good i guess.
    
        (use-package flymake-go)
    
    and finally we can give it [58](#orgbff7167) completion. It should
    recognize it however I havent tested it yet.
    
        (use-package company-go)


<a id="orgf10184c"></a>

### Flycheck

<a id="orgd5cd08e"></a>

These are my flycheck settings although most packages have their
flycheck set to start in their own packages.

    (use-package flycheck
      :init (global-flycheck-mode))
    (use-package flycheck-pos-tip
      :after flycheck
      :config (flycheck-pos-tip-mode))


<a id="org499060b"></a>

### imenu

Imenu is nice to have because you can intelligently view and move
to parts of your program. This one in perticular is able to do it
anywhere and have helm support.

    (use-package imenu-anywhere)


<a id="org4fe3b05"></a>

# Keybindings

This is all of my defined keybinds. I use [49](#org5ff3062) alot because its
a good package for [43](#org958e5a6). People rarely do things like ;; (M-x ;) and
other stuff. Btw the )) is on the end of the line because i may want
to comment out the last <<>> and that would actually comment ))
causing a syntax error.


<a id="org71d6102"></a>

## Rant on keybindings

There are a bunch of different ways on how to bind a key. I think
each way has its merit and should be thought of before you assign a
keybinding. states and prefixes are achived through
keymaps. Arguments are achieved through C-u or evil motions/
1-9/registers. chords are by key-chord or its in vim by
default. commands are by command mode in vim or M-x in
emacs. Modifiers are just control alt and shift etc&#x2026; The big
thing that emacs has over vim is provide context to a
function. Working differently depending on where you are is
extremely powerful. Emacs also allows making new mode maps which is
the other big thing that emacs does better. Note keymaps are a
combination of states and prefixes. Vim arguments work better
however C-u is a VERY interesting idea. vim also loses to
major/minor modes as this makes plugins ALOT more modular. Allowing
multiple ways to look at a single file depending on context. I dont
absolutely hate the idea of using a mouse like in acme however I
will mention that the more you use the mouse the more likely you
will keep trying to use it. In that case you end up abandonning the
keyboard for long periods of times making it worse. Pressing escape
to do 60+ keybindings is MUCH more efficent then reaching over to
your mouse for a max of 20 keybinds which are context dependant. It
may however be easier to understand at first.

-   states (insert or normal)
-   prefix (emacs C-x or vim C-w)
-   modes (major/minor modes or file types)
-   argument (2dd or C-u)
-   chords (keychord)
-   commands (M-x or ex-commands)
-   modifers (C-y or C-S-<backspace>)
-   context (org mode C-c C-c or [this](https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/))
-   mouse (plan 9's acme?)


<a id="orga4d96cf"></a>

## Emacs State

Yes i want to be able to use emacs because i think its important to
understand different ways of doing things. Because of this I am
going to make it a 66% on startup of whether evil mode is enabled
or not. [This is nice if I forget keybinds](http://www.elmindreda.org/emacs.html).

    (general-define-key
     :states '(emacs)
     "M-x" 'helm-M-x
     "C-x C-f" 'helm-find-files
     "C-x f" 'helm-locate
     "C-x C-b" 'helm-buffers-list
     "C-x b" 'helm-multi-files
     "C-x g" 'magit-status
     "C-x C-g" 'magit-dispatch
     "C-x C-o" 'other-window
     "C-x C-k" 'kill-this-buffer
     "M-j" 'delete-blank-lines ;; @TODO(renzix): Make this also join the previous line if no blank lines?
     "C-o" 'my/smart-open-line
     "C-\\" 'er/expand-region
     "C-." 'repeat
     "<escape>" 'god-local-mode
     "C-=" 'my/smart-indent
     "C-j" 'avy-goto-char-2
     "C-'" 'my/helm-projectile-find-file-or-find-file
     "C-\"" 'my/helm-projectile-find-file-or-project
     "M-'" 'my/helm-projectile-search-or-project
     "M-\"" 'helm-occur
     "C-|" 'helm-mini
     "C-:" 'helm-bookmarks
     "M-p" 'projectile-command-map
     "M-m" 'evilmi-jump-items
     "C-c h" 'helm-command-prefix
     "C-a" 'my/move-beginning-of-line)


<a id="org0a3a816"></a>

## Normal/Visual State

General evil overided global keybinds. 

    (general-define-key
     :states '(normal visual)
     "|" 'helm-mini
     "SPC" 'helm-imenu
     ;; "_" 'my/evil-jump-backward
     "s" 'my/eshell-toggle
     "S" 'my/helm-projectile-find-file-or-find-file
     ";" 'helm-M-x
     "g c c" 'comment-line
     "g c r" 'comment-or-uncomment-region
     "g =" 'my/smart-indent
     "g p" 'projectile-command-map
     "\\" 'my/helm-projectile-search-or-project
     "U" 'undo-tree-visualize
     "Q" 'save-buffers-kill-terminal
     ", , c" 'org-capture
     ", , l" 'org-store-link
     (general-chord ";;") 'eval-expression
     (general-chord "SS") 'my/helm-projectile-find-file-or-project
     (general-chord "ss") 'my/vterm-toggle
     (general-chord "``") 'magit-status)


<a id="orgf987453"></a>

## Insert State

These are my keys for insert mode. They should be specifically
about entering or deleting text.

    (general-define-key
     :states '(insert)
     (general-chord "uu") 'my/evil-insert-delete-back-word)


<a id="orgff6e495"></a>

## Ex commands

The rest is my ex commands. These are things that are pretty
useful but do not require much context.

    (evil-ex-define-cmd "cfg" 'my/open-emacs-config)
    (evil-ex-define-cmd "a[genda]" 'org-agenda)
    (evil-ex-define-cmd "q[uit]" 'delete-window)
    (evil-ex-define-cmd "bd" 'kill-this-buffer)


<a id="orgca8f38c"></a>

## Major Modes


<a id="org1cc4206"></a>

### Plain Text

-   Org mode

    The keybinds for org-mode.
    
        (general-define-key
         :keymaps 'org-mode-map
         :states '(normal visual)
         "SPC" 'helm-org-rifle
         "RET" 'org-ctrl-c-ctrl-c
         ", <" 'outline-demote
         ", >" 'outline-promote
         ", p" 'org-up-element
         ", n" 'org-down-element
         ", t" 'org-todo
         ", l" 'org-insert-link
         ", ." 'org-time-stamp
         ", s" 'org-schedule
         ", d" 'org-deadline
         ", e" 'org-export-dispatch
         ", [" 'org-agenda-file-to-front
         ", ]" 'org-remove-file
         ", '" 'org-edit-special
         ", a" 'org-add-note)
        (general-define-key
         :keymaps 'org-mode-map
         :states 'emacs
         "C-c r" 'helm-org-rifle)

-   TODO Org src mode

-   SOMEDAY latex mode

    TeX-command-master

-   SOMEDAY markdown mode


<a id="orgc6c8d31"></a>

### Programming

-   NEXT csharp mode

-   TODO rustic mode

-   SOMEDAY c/cpp mode

-   SOMEDAY haskell mode

-   SOMEDAY java mode

-   SOMEDAY scala mode

-   SOMEDAY kotlin mode

-   SOMEDAY clojure mode

-   SOMEDAY powershell mode

-   SOMEDAY shell mode

-   SOMEDAY golang mode

-   SOMEDAY dart mode

-   SOMEDAY emacs lisp mode

-   SOMEDAY common lisp mode

-   SOMEDAY perl6 mode


<a id="org9c8674f"></a>

### Messaging

-   SOMEDAY matrix mode

-   SOMEDAY irc mode


<a id="orgd84e63c"></a>

### Other

-   SOMEDAY magit


<a id="org516494f"></a>

## Other


<a id="org8b8b58d"></a>

### Helm

I have been doing helm wrong my whole life. Here is my attempt at
making some of the keybinds a bit better with doing stuff like
ag and switching between helm modes.

    (general-define-key
     :keymaps 'helm-find-files-map
     "C-'" 'my/helm-projectile-from-find-files
     "C-s" 'helm-ff-run-grep-ag)
    (general-define-key
     :keymaps 'helm-projectile-find-file-map
     "C-'" 'my/helm-goto-find-files)
    (general-define-key
     :keymaps 'helm-rg-map
     "M-'" 'my/helm-goto-helm-occur)
    (general-define-key
     :keymaps 'helm-occur-map
     "M-'" 'my/helm-goto-helm-rg)

