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
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMTime : GDMTag
    {
        private byte fHour;
        private byte fMinutes;
        private byte fSeconds;
        private short fFraction;


        public byte Hour
        {
            get { return fHour; }
            set { fHour = value; }
        }

        public byte Minutes
        {
            get { return fMinutes; }
            set { fMinutes = value; }
        }

        public byte Seconds
        {
            get { return fSeconds; }
            set { fSeconds = value; }
        }

        public short Fraction
        {
            get { return fFraction; }
            set { fFraction = value; }
        }

        public TimeSpan Value
        {
            get {
                return new TimeSpan(0, fHour, fMinutes, fSeconds, (int)(100u * fFraction));
            }
            set {
                fHour = (byte)value.Hours;
                fMinutes = (byte)value.Minutes;
                fSeconds = (byte)value.Seconds;
                fFraction = (short)Math.Truncate(value.Milliseconds / 100.0);
            }
        }


        public GDMTime()
        {
            SetName(GEDCOMTagType.TIME);
        }

        protected override string GetStringValue()
        {
            string result;
            if (fHour == 0 && fMinutes == 0 && fSeconds == 0) {
                result = "";
            } else {
                result = string.Format("{0:00}:{1:00}:{2:00}", new object[] { fHour, fMinutes, fSeconds });

                if (fFraction > 0) {
                    result = result + "." + fFraction.ToString();
                }
            }
            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fHour = 0;
            fMinutes = 0;
            fSeconds = 0;
            fFraction = 0;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fHour == 0 && fMinutes == 0 && fSeconds == 0;
        }

        public override string ParseString(string strValue)
        {
            return GEDCOMUtils.ParseTime(strValue, this);
        }

        internal void SetRawData(byte hour, byte minutes, byte seconds, short fraction)
        {
            fHour = hour;
            fMinutes = minutes;
            fSeconds = seconds;
            fFraction = fraction;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fHour);
            hashCode.Add(fMinutes);
            hashCode.Add(fSeconds);
            hashCode.Add(fFraction);
        }
    }
}
