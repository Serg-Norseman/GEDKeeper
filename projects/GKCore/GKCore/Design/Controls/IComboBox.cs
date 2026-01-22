/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Design.Controls
{
    public interface IComboBox : IBaseControl
    {
        bool ReadOnly { get; set; }
        int SelectedIndex { get; set; }
        object SelectedItem { get; set; }
        string Text { get; set; }

        void Add(object item);
        void AddItem<T>(string text, T tag, IImage image = null);
        void AddRange(IEnumerable<object> items, bool sorted = false);
        void AddStrings(StringList strings);
        void BeginUpdate();
        void Clear();
        void EndUpdate();
        void Sort();

        T GetSelectedTag<T>();
        void SetSelectedTag<T>(T tagValue, bool allowDefault = true);
    }
}
