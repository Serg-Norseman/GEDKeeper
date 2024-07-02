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
using BSLib;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMCutoutPosition : GDMTag
    {
        private int fX1;
        private int fY1;
        private int fX2;
        private int fY2;

        public int X1
        {
            get { return fX1; }
            set { fX1 = value; }
        }

        public int Y1
        {
            get { return fY1; }
            set { fY1 = value; }
        }

        public int X2
        {
            get { return fX2; }
            set { fX2 = value; }
        }

        public int Y2
        {
            get { return fY2; }
            set { fY2 = value; }
        }

        public ExtRect Value
        {
            get {
                return ExtRect.Create(fX1, fY1, fX2, fY2);
            }
            set {
                fX1 = value.Left;
                fY1 = value.Top;
                fX2 = value.Right;
                fY2 = value.Bottom;
            }
        }


        public GDMCutoutPosition()
        {
            SetName(GEDCOMTagType._POSITION);
        }

        public override void Assign(GDMTag source)
        {
            GDMCutoutPosition sourceObj = (source as GDMCutoutPosition);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            fX1 = sourceObj.fX1;
            fY1 = sourceObj.fY1;
            fX2 = sourceObj.fX2;
            fY2 = sourceObj.fY2;
        }

        protected override string GetStringValue()
        {
            string result;

            if (fX1 == 0 && fY1 == 0 && fX2 == 0 && fY2 == 0) {
                result = string.Empty;
            } else {
                result = string.Format("{0} {1} {2} {3}", fX1, fY1, fX2, fY2);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();

            fX1 = 0;
            fY1 = 0;
            fX2 = 0;
            fY2 = 0;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fX1 == 0 && fY1 == 0 && fX2 == 0 && fY2 == 0;
        }

        public override string ParseString(string strValue)
        {
            return GEDCOMUtils.ParseCutoutPosition(strValue, this);
        }

        internal void SetRawData(int x1, int y1, int x2, int y2)
        {
            fX1 = x1;
            fY1 = y1;
            fX2 = x2;
            fY2 = y2;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fX1);
            hashCode.Add(fY1);
            hashCode.Add(fX2);
            hashCode.Add(fY2);
        }
    }
}
