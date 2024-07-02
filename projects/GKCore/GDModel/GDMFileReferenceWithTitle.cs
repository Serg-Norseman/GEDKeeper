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

using System;

namespace GDModel
{
    public sealed class GDMFileReferenceWithTitle : GDMFileReference
    {
        private string fTitle;

        public string Title
        {
            get { return fTitle; }
            set { fTitle = value; }
        }


        public override void Assign(GDMTag source)
        {
            GDMFileReferenceWithTitle srcObj = (source as GDMFileReferenceWithTitle);
            if (srcObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fTitle = srcObj.fTitle;
        }

        public override void Clear()
        {
            base.Clear();
            fTitle = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fTitle);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fTitle);
        }
    }
}
