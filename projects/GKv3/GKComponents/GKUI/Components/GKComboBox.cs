/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Linq;
using BSLib;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKComboBox : ComboBox
    {
        public bool AutoSelect { get; set; }


        public GKComboBox()
        {
            // Eto.Forms supports rendering an item image via IImageListItem
        }

        /*protected override void OnDrawItem(DrawItemEventArgs e)
        {
            // Eto.Forms supports rendering an item image via IImageListItem
        }*/

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            if (AutoSelect && !char.IsControl(e.KeyChar) && !e.Control && !e.Alt) {
                var pressedChar = char.ToLower(e.KeyChar);

                var itemsList = Items.Select((itm) => itm.Text).ToArray();

                var newIndex = IndexOf(itemsList, pressedChar, SelectedIndex + 1);
                if (newIndex < 0) {
                    newIndex = IndexOf(itemsList, pressedChar, 0);
                }

                if (newIndex != -1) {
                    SelectedIndex = newIndex;
                }
            }
        }

        private static int IndexOf(string[] array, char chr, int startIndex)
        {
            startIndex = Algorithms.CheckBounds(startIndex, 0, array.Length - 1);

            for (int i = startIndex; i < array.Length; i++) {
                string item = array[i];
                if (item != null && item.Length > 0 && char.ToLower(item[0]) == chr) {
                    return i;
                }
            }
            return -1;
        }
    }
}
