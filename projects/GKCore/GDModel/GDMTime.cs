/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
            return GEDCOMUtils.ParseTime(this, strValue);
        }

        public override string ParseString(StringSpan strValue)
        {
            return GEDCOMUtils.ParseTime(this, strValue);
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
