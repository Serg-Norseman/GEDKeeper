/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKUI.Components
{
    public class AccordionItem
    {
        public string Text;
        public object Data;
        public readonly bool IsGroup;
        public bool IsExpanded = false;
        public bool IsVisible = true;

        public AccordionItem()
        {
        }

        public AccordionItem(string text)
        {
            Text = text;
            Data = null;
            IsGroup = false;
        }

        public AccordionItem(string text, object data)
        {
            Text = text;
            Data = data;
            IsGroup = false;
        }

        public AccordionItem(string text, bool isGroup, object data)
        {
            Text = text;
            IsGroup = isGroup;
            Data = data;
        }

        public override string ToString()
        {
            return Text;
        }
    }
}
