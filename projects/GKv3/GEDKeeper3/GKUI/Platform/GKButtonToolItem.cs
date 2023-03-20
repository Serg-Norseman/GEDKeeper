using System;

#if OS_MSWIN
using Eto.Wpf.Forms.ToolBar;
#endif

#if OS_LINUX
using Eto.GtkSharp;
using Eto.GtkSharp.Forms.ToolBar;
using Gtk;
#endif

#if OS_MACOS
using Eto.Mac.Forms.ToolBar;
#endif

namespace GKUI.Platform
{
#if OS_MSWIN

    public class GKButtonToolItemHandler : ButtonToolItemHandler
    {
    }

#endif

#if OS_LINUX

    public class GKButtonToolItemHandler : ToolItemHandler<Gtk.ToolButton, GKButtonToolItem>, GKButtonToolItem.IHandler
    {
        Eto.Drawing.Image image;

        protected virtual void SetImage()
        {
            if (Control is Gtk.ToolButton button) {
                Console.WriteLine("SetImage()");
                if (image != null) {
                    GtkImage = image.ToGtk(Gtk.IconSize.Button);
                    GtkImage.Show();
                } else {
                    GtkImage = null;
                }
                button.IconWidget = GtkImage;
            }
        }

        public new Eto.Drawing.Image Image
        {
            get { return image; }
            set {
                if (image != value) {
                    image = value;
                    SetImage();
                }
            }
        }


        protected class GKButtonToolItemHandlerConnector : WeakConnector
        {
            public new GKButtonToolItemHandler Handler => (GKButtonToolItemHandler)base.Handler;

            public void HandleClicked(object sender, EventArgs e)
            {
                Handler?.Widget.OnClick(e);
            }
        }

        protected new GKButtonToolItemHandlerConnector Connector => (GKButtonToolItemHandlerConnector)base.Connector;

        public override void CreateControl(ToolBarHandler handler, int index)
        {
            Toolbar toolbar = handler.Control;
            base.Control = new ToolButton(base.GtkImage, base.Text);
            base.Control.IsImportant = true;
            base.Control.Sensitive = base.Enabled;
            base.Control.TooltipText = ToolTip;
            base.Control.ShowAll();
            base.Control.NoShowAll = true;
            base.Control.Visible = base.Visible;
            toolbar.Insert(base.Control, index);
            base.Control.Clicked += Connector.HandleClicked;
        }

        protected override WeakConnector CreateConnector()
        {
            return new GKButtonToolItemHandlerConnector();
        }
    }

#endif

#if OS_MACOS

    public class GKButtonToolItemHandler : ButtonToolItemHandler
    {
    }

#endif
}
