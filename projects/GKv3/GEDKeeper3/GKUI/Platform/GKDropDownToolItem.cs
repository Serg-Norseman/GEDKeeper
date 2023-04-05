using System;
using Eto;
using Eto.Forms;
using Eto.Drawing;

#if OS_MSWIN
using Eto.Wpf;
using Eto.Wpf.Forms.ToolBar;
using swc = System.Windows.Controls;
using sw = System.Windows;
using swm = System.Windows.Media;
#endif

#if OS_LINUX || OS_FREEBSD
using Eto.GtkSharp.Forms.ToolBar;
#endif

#if OS_MACOS
using Eto.Mac.Forms.ToolBar;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;
#endif

namespace GKUI.Platform
{
#if OS_MSWIN

    public class GKDropDownToolItemHandler : ToolItemHandler<swc.Button, GKDropDownToolItem>, GKDropDownToolItem.IHandler
    {
        Image image;
        readonly swc.Image swcImage;
        readonly swc.TextBlock label;
        ContextMenu contextMenu;
        readonly sw.Shapes.Path arrow;

        public GKDropDownToolItemHandler()
        {
            Control = new swc.Button();
            swcImage = new swc.Image { MaxHeight = 16, MaxWidth = 16 };
            label = new swc.TextBlock();
            // Text="▼/▾"
            arrow = new sw.Shapes.Path { Data = swm.Geometry.Parse("M 0 0 L 3 4 L 6 0 Z"), VerticalAlignment = sw.VerticalAlignment.Center, Margin = new sw.Thickness(8, 2, 0, 0), Fill = swm.Brushes.Black };
            var panel = new swc.StackPanel { Orientation = swc.Orientation.Horizontal, Children = { swcImage, label, arrow } };
            Control.Content = panel;
            Control.Click += Control_Click;
            sw.Automation.AutomationProperties.SetLabeledBy(Control, label);
        }

        private void Control_Click(object sender, sw.RoutedEventArgs e)
        {
            Widget.OnClick(EventArgs.Empty);

            var ctxMenu = contextMenu.ControlObject as swc.ContextMenu;
            if (ctxMenu != null) {
                ctxMenu.PlacementTarget = Control;
                ctxMenu.Placement = swc.Primitives.PlacementMode.Bottom;
                ctxMenu.IsOpen = true;
            }
        }

        public override string Text
        {
            get { return label.Text.ToEtoMnemonic(); }
            set { label.Text = value.ToPlatformMnemonic(); }
        }

        public override string ToolTip
        {
            get { return Control.ToolTip as string; }
            set { Control.ToolTip = value; }
        }

        public override Image Image
        {
            get { return image; }
            set {
                image = value;
                swcImage.Source = image.ToWpf(Screen.PrimaryScreen.LogicalPixelSize, swcImage.GetMaxSize().ToEtoSize());
            }
        }

        public override bool Enabled
        {
            get { return Control.IsEnabled; }
            set {
                Control.IsEnabled = value;
                swcImage.IsEnabled = value;
            }
        }

        public ContextMenu ContextMenu
        {
            get { return contextMenu; }
            set { contextMenu = value; }
        }
    }

#endif

#if OS_LINUX || OS_FREEBSD

    public class GKDropDownToolItemHandler : ToolItemHandler<Gtk.ToolButton, GKDropDownToolItem>, GKDropDownToolItem.IHandler
    {
        ContextMenu contextMenu;

        public override void CreateControl(ToolBarHandler handler, int index)
        {
            Gtk.Toolbar tb = handler.Control;

            Text = "▾";
            Control = new Gtk.ToolButton(GtkImage, Text);
            Control.IsImportant = true;
            Control.Sensitive = Enabled;
            Control.TooltipText = this.ToolTip;
            Control.ShowAll();
            Control.NoShowAll = true;
            Control.Visible = Visible;
            //control.CanFocus = false;			// why is this disabled and not true???
            tb.Insert(Control, index);
            Control.Clicked += Connector.HandleClicked;
        }

        protected new GKDropDownToolItemConnector Connector { get { return (GKDropDownToolItemConnector)base.Connector; } }

        protected override WeakConnector CreateConnector()
        {
            return new GKDropDownToolItemConnector();
        }

        protected class GKDropDownToolItemConnector : WeakConnector
        {
            public new GKDropDownToolItemHandler Handler { get { return (GKDropDownToolItemHandler)base.Handler; } }

            PointF showLocation;

            public void HandleClicked(object sender, EventArgs e)
            {
                Handler?.Widget.OnClick(e);

                var ctxMenu = Handler.ContextMenu.ControlObject as Gtk.Menu;
                if (ctxMenu != null) {
                    var buttonRect = Handler.Control.Clip;
                    var pt = new PointF(buttonRect.Left, buttonRect.Bottom);

                    var parentWindow = (Handler.Widget.Parent as ToolBar).Parent as Window;
                    showLocation = parentWindow.PointToScreen(pt);

                    ctxMenu.Popup(null, null, PopupMenuPosition, 3u, Gtk.Global.CurrentEventTime);
                }
            }

            void PopupMenuPosition(Gtk.Menu menu, out int x, out int y, out bool push_in)
            {
                x = (int)showLocation.X;
                y = (int)showLocation.Y;
                push_in = false;
            }
        }

        public ContextMenu ContextMenu
        {
            get { return contextMenu; }
            set { contextMenu = value; }
        }
    }

#endif

#if OS_MACOS

    public class GKDropDownToolItemHandler : ToolItemHandler<NSToolbarItem, GKDropDownToolItem>, GKDropDownToolItem.IHandler
    {
        ContextMenu contextMenu;

        public new string Text
        {
            get { return Button.Title; }
            set { Button.Title = value; }
        }

        public override void InvokeButton()
        {
            Widget.OnClick(EventArgs.Empty);

            var ctxMenu = contextMenu.ControlObject as NSMenu;
            if (ctxMenu != null) {
                ctxMenu.PopUpMenu(null, Button.Frame.Location, Button.Superview);
            }
        }

        public ContextMenu ContextMenu
        {
            get { return contextMenu; }
            set { contextMenu = value; }
        }
    }

#endif

}
