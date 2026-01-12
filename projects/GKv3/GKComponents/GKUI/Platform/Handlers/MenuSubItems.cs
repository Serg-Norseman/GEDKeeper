/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    internal class MenuSubItems : IMenuItems
    {
        private MenuItem fItem;

        public IMenuItem this[int index]
        {
            get {
                var btnItem = fItem as ButtonMenuItem;
                if (btnItem == null)
                    throw new Exception("Type mismatch");

                if (index < 0 || index >= btnItem.Items.Count)
                    throw new ArgumentOutOfRangeException("index");

                return new MenuItemHandler(btnItem.Items[index]);
            }
        }

        public int Count
        {
            get {
                return fItem is ButtonMenuItem ? ((ButtonMenuItem)fItem).Items.Count : 0;
            }
        }

        public MenuSubItems(MenuItem control)
        {
            fItem = control;
        }
    }
}
