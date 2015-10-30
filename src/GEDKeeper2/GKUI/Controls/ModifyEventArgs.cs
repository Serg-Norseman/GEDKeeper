using System;
using GKCore.Types;

namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
    public class ModifyEventArgs : EventArgs
    {
        public RecordAction Action { get; private set; }
        public object ItemData { get; set; }

        public ModifyEventArgs(RecordAction action, object itemData)
        {
            this.Action = action;
            this.ItemData = itemData;
        }
    }
}