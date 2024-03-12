/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using System.Collections;
using System.Collections.Generic;
using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Design.Controls
{
    public interface IComboBox : IBaseControl
    {
        IList Items { get; }
        bool ReadOnly { get; set; }
        int SelectedIndex { get; set; }
        object SelectedItem { get; set; }
        bool Sorted { get; set; }
        string Text { get; set; }
        bool Visible { get; set; }

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
