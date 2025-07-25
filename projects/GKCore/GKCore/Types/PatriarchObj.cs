﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using GDModel;

namespace GKCore.Types
{
    public sealed class PatriarchObj
    {
        public bool Mark;

        public GDMIndividualRecord IRec;
        public int BirthYear;
        public int DescendantsCount;
        public int DescGenerations;
        public readonly List<PatriarchObj> Links;
        public bool HasLinks;

        public PatriarchObj()
        {
            Links = new List<PatriarchObj>();
        }
    }
}
