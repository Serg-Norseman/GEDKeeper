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
    /// <summary>
    /// 
    /// </summary>
    public class ComboItem<T> : IComboItem
    {
        public string Text { get; private set; }

        public T Tag { get; private set; }

        public IImage Image { get; private set; }

        public ComboItem(string text)
        {
            Text = text;
        }

        public ComboItem(string text, T tag)
        {
            Text = text;
            Tag = tag;
        }

        public ComboItem(string text, T tag, IImage image)
        {
            Text = text;
            Tag = tag;
            Image = image;
        }

        public override string ToString()
        {
            return Text;
        }
    }
}
