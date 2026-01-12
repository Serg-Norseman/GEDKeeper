/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
