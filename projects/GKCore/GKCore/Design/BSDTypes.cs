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

using System;

namespace GKCore.Design
{
    /// <summary>
    /// This class is designed to isolate common type names from platform-specific implementations.
    /// Not the best name for the class, it may be renamed in the future.
    /// </summary>
    public static class BSDTypes
    {
        [Flags]
        public enum FontStyle
        {
            None = 0,
            Bold = 1,
            Italic = 2,
            Underline = 4,
            Strikeout = 8
        }


        public enum HorizontalAlignment
        {
            Left,
            Right,
            Center
        }


        public enum SortOrder
        {
            None,
            Ascending,
            Descending
        }
    }
}
