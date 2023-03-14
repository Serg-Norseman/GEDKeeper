using Eto;
using Eto.Forms;

namespace GKUI.Platform
{
    [Handler(typeof(GKDropDownToolItem.IHandler))]
    public class GKDropDownToolItem : ToolItem
    {
        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public ContextMenu ContextMenu
        {
            get { return Handler.ContextMenu; }
            set { Handler.ContextMenu = value; }
        }

        public GKDropDownToolItem() : base()
        {
        }

        public new interface IHandler : ToolItem.IHandler, IContextMenuHost
        {
        }
    }
}
