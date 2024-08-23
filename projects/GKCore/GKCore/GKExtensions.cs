/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GKCore
{
    public static class GKExtensions
    {
        public static string SafeTrim(this string input)
        {
            return !string.IsNullOrEmpty(input) ? input.Trim() : string.Empty;
        }

        public static string GetEventKey(this GDMCustomEvent customEvent)
        {
            return (customEvent == null) ? string.Empty : customEvent.GetTagName() + ":" + customEvent.Classification;
        }

        public static bool Exchange<T>(this GDMList<T> list, T value, RecordAction recordAction) where T : GDMTag
        {
            bool result = false;

            int idx = list.IndexOf(value);
            if (idx < 0 || idx >= list.Count) return false;

            switch (recordAction) {
                case RecordAction.raMoveUp:
                    list.Exchange(idx - 1, idx);
                    result = true;
                    break;

                case RecordAction.raMoveDown:
                    list.Exchange(idx, idx + 1);
                    result = true;
                    break;
            }

            return result;
        }
    }
}
