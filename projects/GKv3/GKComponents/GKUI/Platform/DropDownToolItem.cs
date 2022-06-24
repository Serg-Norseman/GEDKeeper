using Eto;
using Eto.Forms;

namespace GKUI.Platform
{
    [Handler(typeof(DropDownToolItem.IHandler))]
    public class DropDownToolItem : ToolItem
    {
        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public ContextMenu ContextMenu
        {
            get { return Handler.ContextMenu; }
            set { Handler.ContextMenu = value; }
        }

        public DropDownToolItem() : base()
        {
        }

        public new interface IHandler : ToolItem.IHandler, IContextMenuHost
        {
        }
    }
}
