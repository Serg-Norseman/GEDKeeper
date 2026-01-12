/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#pragma warning disable CA1416

using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Themes;

namespace GKUI.Platform
{
    internal class EtoAppStyles
    {
        public static void InitCommonStyles()
        {
            Eto.Style.Add<TableLayout>("paddedTable", table => {
                table.Padding = new Padding(8);
                table.Spacing = new Size(4, 4);
            });

            Eto.Style.Add<TableLayout>("paddedTable8", table => {
                table.Padding = new Padding(8);
                table.Spacing = new Size(8, 8);
            });

            Eto.Style.Add<StackLayout>("vertListStack", stack => {
                stack.Orientation = Orientation.Vertical;
                stack.Padding = new Padding(8);
                stack.Spacing = 4;
            });

            Eto.Style.Add<StackLayout>("horzListStack", stack => {
                stack.Orientation = Orientation.Horizontal;
                stack.Padding = new Padding(8);
                stack.Spacing = 4;
            });

            Eto.Style.Add<StackLayout>("dlgFooter", stack => {
                stack.Orientation = Orientation.Horizontal;
                stack.Padding = new Padding(0);
                stack.Spacing = EtoAppConsts.DialogButtonSpacing;
            });

            Eto.Style.Add<StackLayout>("labtexStack", stack => {
                stack.Orientation = Orientation.Vertical;
                stack.Padding = new Padding(0);
                stack.Spacing = 2;
            });

            Eto.Style.Add<Button>("funcBtn", button => {
                button.ImagePosition = ButtonImagePosition.Left;
                button.Size = new Size(160, 26);
            });

            Eto.Style.Add<Button>("dlgBtn", button => {
                button.ImagePosition = ButtonImagePosition.Left;
                button.Size = new Size(120, 26);
            });

            Eto.Style.Add<Button>("iconBtn", button => {
                button.ImagePosition = ButtonImagePosition.Overlay;
                button.Size = new Size(26, 26);
            });
        }

        public static void InitPlatformHandlers(Application application)
        {
            application.Platform.Add<GKToolBar.IHandler>(() => new GKToolBarHandler());
            application.Platform.Add<GKContextMenu.IHandler>(() => new GKContextMenuHandler());
            application.Platform.Add<GKMenuBar.IHandler>(() => new GKMenuBarHandler());
            application.Platform.Add<GKTabControl.IHandler>(() => new GKTabControlHandler());
            application.Platform.Add<GKButtonToolItem.IHandler>(() => new GKButtonToolItemHandler());
            application.Platform.Add<GKDropDownToolItem.IHandler>(() => new GKDropDownToolItemHandler());

#if OS_MACOS
            application.Platform.Add<Font.IHandler>(() => new GKFontHandler());
#endif

#if !DIS_VLC
            application.Platform.Add<NativeHostControl.IHandler>(() => new NativeHostControlHandler());
#endif
        }

        public static void InitPlatformStyles()
        {
#if OS_LINUX
            // FIXME: don't work
            Eto.Style.Add<Eto.GtkSharp.Forms.ToolBar.ToolBarHandler>("tbsi", h => {
                // executed but no result
                h.Control.ToolbarStyle = Gtk.ToolbarStyle.BothHoriz;
                //h.Control.ToolbarStyle = Gtk.ToolbarStyle.Icons;
                h.Control.IconSize = Gtk.IconSize.SmallToolbar;
            });

            Eto.Style.Add<Eto.GtkSharp.Forms.Controls.GridColumnHandler>(null, h => {
                Pango.FontDescription tpf = new Pango.FontDescription();
                tpf.Weight = Pango.Weight.Normal;
                h.Control.Button.ModifyFont(tpf);
                h.Control.Button.ModifyFg(Gtk.StateType.Normal, new Gdk.Color(0, 0, 0));
            });

            Eto.Style.Add<Eto.GtkSharp.Forms.Controls.GridViewHandler>(null, h => {
                //h.ScrolledWindow.KineticScrolling = false;
                //h.ScrolledWindow.OverlayScrolling = false;
            });

            Eto.Style.Add<Eto.GtkSharp.Forms.DialogHandler>(null, h => {
                // don't work, why?
                //h.Control.Window.SetShadowWidth(0, 0, 0, 0);
            });
#endif

#if OS_MSWIN
            Eto.Wpf.Forms.ToolBar.ToolItemHandler.DefaultImageSize = new Size(20, 20);
            Eto.Wpf.Forms.Menu.MenuItemHandler.DefaultImageSize = new Size(20, 20);
#endif
        }

        public static void FixToolBar()
        {
            int toolSize = BaseThemeManager.GetThemeInt(ThemeElement.ToolItemsImageSize);

#if OS_MSWIN
            if (toolSize != 0) {
                Eto.Wpf.Forms.ToolBar.ToolItemHandler.DefaultImageSize = new Size(toolSize, toolSize);
                Eto.Wpf.Forms.Menu.MenuItemHandler.DefaultImageSize = new Size(toolSize, toolSize);
            }
#endif
        }
    }
}
