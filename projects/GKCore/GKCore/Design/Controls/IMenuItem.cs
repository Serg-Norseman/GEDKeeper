/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Graphics;

namespace GKCore.Design.Controls
{
    public delegate void ItemAction(IMenuItem sender);


    public interface IMenuItems : IControlItems<IMenuItem>
    {
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IMenuItem : IToolItem
    {
        bool Checked { get; set; }
        IMenuItems SubItems { get; }
        object Tag { get; set; }

        IMenuItem AddItem(string text, object tag, IImage image, ItemAction action);
        void ClearItems();
    }
}
