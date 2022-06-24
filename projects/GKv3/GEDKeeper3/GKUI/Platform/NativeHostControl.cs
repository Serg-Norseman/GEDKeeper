﻿using System;

#if OS_MSWIN
using Eto.Wpf.Forms;
using System.Windows.Forms;
#endif

#if OS_LINUX
using Eto.GtkSharp.Forms;
using Gtk;
#endif

#if OS_MACOS
using Eto.Mac.Forms;
using MonoMac.AppKit;
#endif

namespace GKUI.Platform
{
#if OS_MSWIN

    public class WinFormsUserControl : UserControl
    {
        public bool IsInitialized { get; private set; }

        public WinFormsUserControl()
        {
        }

        protected override void OnHandleCreated(EventArgs e)
        {
            if (!this.IsInitialized) {
                this.IsInitialized = true;
            }

            base.OnHandleCreated(e);
        }
    }

    public class NativeHostControlHandler : WindowsFormsHostHandler<WinFormsUserControl, NativeHostControl, NativeHostControl.ICallback>, NativeHostControl.IHandler
    {
        public new IntPtr NativeHandle => WinFormsControl.Handle;

        public bool IsInitialized => WinFormsControl.IsInitialized;

        public NativeHostControlHandler()
        {
            WinFormsControl = new WinFormsUserControl();
            Control.Focusable = true;
            Control.Background = System.Windows.SystemColors.ControlBrush;
        }
    }

#endif

#if OS_LINUX

    public class GtkUserControl : DrawingArea
    {
        public bool IsInitialized { get; private set; }

        public GtkUserControl()
        {
            this.IsInitialized = true;
        }
    }

    /// <summary>
    /// https://github.com/picoe/Eto.OpenTK/tree/master/Eto.OpenTK.Gtk
    /// </summary>
    public class NativeHostControlHandler : GtkControl<GtkUserControl, NativeHostControl, NativeHostControl.ICallback>, NativeHostControl.IHandler
    {
        public new IntPtr NativeHandle => Control.Handle;

        public bool IsInitialized => Control.IsInitialized;

        public NativeHostControlHandler()
        {
            Control = new GtkUserControl();
        }
    }

#endif

#if OS_MACOS

    public class MacControl : NSView
    {
        public MacControl()
        {
        }

        public bool IsInitialized => true;

        public bool CanFocus { get; set; }
    }

    /// <summary>
    /// https://github.com/picoe/Eto.OpenTK/tree/master/Eto.OpenTK.Mac
    /// </summary>
    public class NativeHostControlHandler : MacView<MacControl, NativeHostControl, NativeHostControl.ICallback>, NativeHostControl.IHandler
    {
        public override NSView ContainerControl
        {
            get { return Control; }
        }

        public new IntPtr NativeHandle => Control.Handle;

        public bool IsInitialized => Control.IsInitialized;

        public NativeHostControlHandler()
        {
            Control = new MacControl();
        }
    }

#endif

}
