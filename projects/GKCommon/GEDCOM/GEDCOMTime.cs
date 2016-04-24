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
    public sealed class GEDCOMTime : GEDCOMTag
    {
        private ushort fHour;
        private ushort fMinutes;
        private ushort fSeconds;
        private ushort fFraction;

        public ushort Fraction
        {
            get { return this.fFraction; }
            set { this.fFraction = value; }
        }

        public ushort Hour
        {
            get { return this.fHour; }
            set { this.fHour = value; }
        }

        public ushort Minutes
        {
            get { return this.fMinutes; }
            set { this.fMinutes = value; }
        }

        public ushort Seconds
        {
            get { return this.fSeconds; }
            set { this.fSeconds = value; }
        }

        public TimeSpan Value
        {
            get {
                return new TimeSpan(0, this.fHour, this.fMinutes, this.fSeconds, (int)(100u * this.fFraction));
            }
            set {
                this.fHour = (ushort)value.Hours;
                this.fMinutes = (ushort)value.Minutes;
                this.fSeconds = (ushort)value.Seconds;
                ushort mSec = (ushort)value.Milliseconds;
                this.fFraction = (ushort)Math.Truncate(mSec / 100.0);
            }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetName("TIME");
        }

        protected override string GetStringValue()
        {
            string result;
            if (this.fHour == 0 && this.fMinutes == 0 && this.fSeconds == 0)
            {
                result = "";
            }
            else
            {
                result = string.Format("{0:00}:{1:00}:{2:00}", new object[] { this.fHour, this.fMinutes, this.fSeconds });

                if (this.fFraction > 0)
                {
                    result = result + "." + this.fFraction.ToString();
                }
            }
            return result;
        }

        public override void Clear()
        {
            base.Clear();
            this.fHour = 0;
            this.fMinutes = 0;
            this.fSeconds = 0;
            this.fFraction = 0;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fHour == 0 && this.fMinutes == 0 && this.fSeconds == 0;
        }

        public override string ParseString(string strValue)
        {
            this.fHour = 0;
            this.fMinutes = 0;
            this.fSeconds = 0;
            this.fFraction = 0;

            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                result = GEDCOMUtils.ExtractDelimiter(result, 0);

                int tmp;
                result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
                this.fHour = (ushort)tmp;
                if (result != "" && result[0] == ':')
                {
                    result = result.Remove(0, 1);
                }

                result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
                this.fMinutes = (ushort)tmp;
                if (result != "" && result[0] == ':')
                {
                    result = result.Remove(0, 1);

                    result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
                    this.fSeconds = (ushort)tmp;
                    if (result != "" && result[0] == '.')
                    {
                        result = result.Remove(0, 1);

                        result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
                        this.fFraction = (ushort)tmp;
                    }
                }
            }
            return result;
        }

        public GEDCOMTime(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMTime(owner, parent, tagName, tagValue);
        }
    }
}
