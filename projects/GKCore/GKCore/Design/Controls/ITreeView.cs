/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design.Controls
{
    public interface ITVNode
    {
        object Tag { get; set; }
    }


    public interface ITreeView : IBaseControl
    {
        ITVNode AddNode(ITVNode parent, string name, object tag);
        void BeginUpdate();
        void Clear();
        void EndUpdate();
        void Expand(ITVNode node);
        object GetSelectedData();
    }
}
