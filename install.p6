#!/usr/bin/env perl6

sub MAIN() {
    my IO::Path $*HOMEDIR;
    given $*KERNEL {
        when 'linux' {
            say "You are running linux";
            $*HOMEDIR = IO::Path.new(%*ENV<HOME>) // die "Cannot find home";
        }
        default { die "Unimplemented Kernel/OS: $_" }
    }
    log "installing to $*HOMEDIR";
    install(".emacs.d/init.el");
    install(".emacs.d/bookmarks");
    install(".shellrc", ".bashrc", ".zshrc");
    log "Finished installing";
}

multi sub install(Str $file, *@aliases) {
    my $fname = $file.IO;
    mkdir $_ when !"$*HOMEDIR/$fname".IO.parent.e;
    unlink "$*HOMEDIR/$fname".IO when "$*HOMEDIR/$fname".IO.e;
    symlink("$*HOMEDIR/Dotfiles/$fname", "$*HOMEDIR/$fname");
    log "Installed $*HOMEDIR/Dotfiles/$fname to $*HOMEDIR/$fname";
    for @aliases -> $alias {
        unlink "$*HOMEDIR/$alias".IO when "$*HOMEDIR/$alias".IO.e;
        symlink("$*HOMEDIR/$fname", "$*HOMEDIR/$alias" );
        log "symlinked $*HOMEDIR/$fname to $*HOMEDIR/$alias";
    }
}

sub log($str) {
    say $str;
}
