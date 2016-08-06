/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMCutoutPosition : GEDCOMTag
    {
        private ushort fX1;
        private ushort fY1;
        private ushort fX2;
        private ushort fY2;

        public ushort X1
        {
            get { return this.fX1; }
            set { this.fX1 = value; }
        }

        public ushort Y1
        {
            get { return this.fY1; }
            set { this.fY1 = value; }
        }

        public ushort X2
        {
            get { return this.fX2; }
            set { this.fX2 = value; }
        }

        public ushort Y2
        {
            get { return this.fY2; }
            set { this.fY2 = value; }
        }


        public ExtRect Value
        {
            get {
                return ExtRect.Create(this.fX1, this.fY1, this.fX2, this.fY2);
            }
            set {
                this.fX1 = (ushort)value.Left;
                this.fY1 = (ushort)value.Top;
                this.fX2 = (ushort)value.Right;
                this.fY2 = (ushort)value.Bottom;
            }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetName("_POSITION");
        }

        protected override string GetStringValue()
        {
            string result;

            if (this.fX1 == 0 && this.fY1 == 0 && this.fX2 == 0 && this.fY2 == 0) {
                result = "";
            } else {
                result = string.Format("{0} {1} {2} {3}", new object[] { this.fX1, this.fY1, this.fX2, this.fY2 });
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();

            this.fX1 = 0;
            this.fY1 = 0;
            this.fX2 = 0;
            this.fY2 = 0;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fX1 == 0 && this.fY1 == 0 && this.fX2 == 0 && this.fY2 == 0;
        }

        public override string ParseString(string strValue)
        {
            this.fX1 = 0;
            this.fY1 = 0;
            this.fX2 = 0;
            this.fY2 = 0;

            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                StringTokenizer strTok = new StringTokenizer(result);
                strTok.IgnoreWhiteSpace = true;
                strTok.RecognizeDecimals = false;

                Token token = strTok.Next();
                this.fX1 = (ushort)((token.Kind != TokenKind.Number) ? 0 : ushort.Parse(token.Value));
                token = strTok.Next();
                this.fY1 = (ushort)((token.Kind != TokenKind.Number) ? 0 : ushort.Parse(token.Value));
                token = strTok.Next();
                this.fX2 = (ushort)((token.Kind != TokenKind.Number) ? 0 : ushort.Parse(token.Value));
                token = strTok.Next();
                this.fY2 = (ushort)((token.Kind != TokenKind.Number) ? 0 : ushort.Parse(token.Value));
            }

            return result;
        }

        public GEDCOMCutoutPosition(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMCutoutPosition(owner, parent, tagName, tagValue);
        }
    }
}
