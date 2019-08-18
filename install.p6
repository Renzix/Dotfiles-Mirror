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
    say "installing to $*HOMEDIR";
    install(".emacs.d/init.el");
    install(".bashrc");
}

multi sub install(IO::Path $fname, IO::Path $HOMEDIR) {
    when "$HOMEDIR/$fname".IO ~~ :e {
        unlink "$HOMEDIR/$fname".IO;
    }
    symlink("$HOMEDIR/Dotfiles/$fname", "$HOMEDIR/$fname");
}

multi sub install(Str $fname, IO::Path $HOMEDIR) {
    install(IO::Path.new($fname),$HOMEDIR);
}

multi sub install(Str $fname) {
    install(IO::Path.new($fname),$*HOMEDIR);
}
