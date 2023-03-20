using System;
using Eto;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Platform
{
#if OS_MSWIN

    public class GKButtonToolItem : ButtonToolItem
    {
    }

#endif

#if OS_LINUX

    [Handler(typeof(GKButtonToolItem.IHandler))]
    public class GKButtonToolItem : ButtonToolItem
    {
        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public GKButtonToolItem()
        {
        }

        public GKButtonToolItem(EventHandler<EventArgs> click)
        {
            Click += click;
        }

        public GKButtonToolItem(Command command)
            : base(command)
        {
            Handler.CreateFromCommand(command);
        }

        public new interface IHandler : ButtonToolItem.IHandler
        {
        }
    }

#endif

#if OS_MACOS

    public class GKButtonToolItem : ButtonToolItem
    {
    }

#endif
}
