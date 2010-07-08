package Emacs::TapMode;
# ABSTRACT: Emacs major mode for editing .tap-files

1;

=head1 DESRIPTION

TAP is the Test Anything Protocol, a communication protocol
between unit tests and a test harness.

This mode mainly defines a grammar for syntax highlighting 
of TAP files.

=head1 USAGE

Put the file F<tap-mode.el> into your load-path and the following into
your F<~/.emacs>:

    (require 'tap-mode)

To associate tap-mode with .tap files add the following to your
F<~/.emacs>:

    (add-to-list 'auto-mode-alist '("\\.tap$" . tap-mode))

To automatically turn on font-lock-mode add the following to your
F<~/.emacs>:

    (add-hook 'tap-mode-hook 'font-lock-mode)

=head1 SEE ALSO

For the actual mode please refer to F<tap-mode.el>.

