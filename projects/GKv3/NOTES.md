
# Fundamentals

.NET 6,8-9 - only supports Windows >= 10 (7 SP1 and 8.1 also ended!)
  https://github.com/dotnet/core/blob/main/release-notes/9.0/supported-os.md
  https://github.com/dotnet/core/blob/main/release-notes/8.0/supported-os.md
  https://github.com/dotnet/core/blob/main/release-notes/6.0/supported-os.md

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

7. Eto.Forms 2.7.4 [WPF]
  - Text randomly disappears in high resolution images.

8. Eto.Forms 2.7.4 [WPF]
  - FontDialog don't return selected font.

9. Eto.Forms 2.7.4 [WPF+, Gtk?]
  - Pen.Color setter don't works, also Pen.Brush.Color.

# Other

LibVLCSharp.GTK
  - all versions only for net47 and above
