
# Eto.Forms

Eto.Forms v2.7.4 compatible with netfw462

1. Eto.Forms 2.7.3 [Gtk] - solved 2.7.4
  - OpenFileDialog not working (ShowDialog returns Ignore instead Ok);

2. Eto.Forms 2.7.1-3 [Gtk]
  - ButtonToolItem does not display text and images set from code (text updates after 2.7.2, images not)

3. Eto.Forms 2.7.1-3 [Gtk]
  - GridView on cells formatting don't process selected row (selected row changing don't call cells formatting)

4. Eto.Forms 2.7.3 [Gtk]
  - GridView not selecting last (or only) row (where AllowEmptySelection=true)

5. Eto.Forms 2.7.4 [Gtk]
  - SaveFileDialog.CurrentFilter is null!

6. Eto.Forms 2.7.4 [Gtk]
  - A window can be active but not have focus, which causes the active window detection methods to work incorrectly.

# Other

LibVLCSharp.GTK
  - all versions only for net47 and above
