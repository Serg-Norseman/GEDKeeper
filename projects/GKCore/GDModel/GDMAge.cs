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
    /// <summary>
    /// Used to store a person's age for a given event,
    /// allowing for undefined and relative values.
    /// </summary>
    public class GDMAge : GDMTag
    {
        private int fRelative = 0;
        private int fYears = -1;
        private int fMonths = -1;
        private int fDays = -1;


        /// <summary>
        /// -1 <, 0 =, +1 >
        /// </summary>
        public int Relative
        {
            get { return fRelative; }
            set {
                if (fRelative != value) {
                    fRelative = value;
                }
            }
        }

        public int Years
        {
            get { return fYears; }
            set {
                if (fYears != value) {
                    fYears = value;
                }
            }
        }

        public int Months
        {
            get { return fMonths; }
            set {
                if (fMonths != value) {
                    fMonths = value;
                }
            }
        }

        public int Days
        {
            get { return fDays; }
            set {
                if (fDays != value) {
                    fDays = value;
                }
            }
        }

        public bool Child
        {
            get { return (fRelative == 0 && fYears < 8); }
        }

        public bool Infant
        {
            get { return (fRelative == 0 && fYears < 1); }
        }

        public bool StillBorn
        {
            get { return (fRelative == 0 && fYears == 0 && fMonths == 0 && fDays == 0); }
        }


        public GDMAge()
        {
            SetName(GEDCOMTagType.AGE);
        }

        public override void Clear()
        {
            base.Clear();
            fRelative = 0;
            fYears = -1;
            fMonths = -1;
            fDays = -1;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fRelative == 0 && fYears == -1 && fMonths == -1 && fDays == -1);
        }

        public override void Assign(GDMTag source)
        {
            GDMAge sourceObj = (source as GDMAge);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fRelative = sourceObj.fRelative;
            fYears = sourceObj.fYears;
            fMonths = sourceObj.fMonths;
            fDays = sourceObj.fDays;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fRelative);
            hashCode.Add(fYears);
            hashCode.Add(fMonths);
            hashCode.Add(fDays);
        }

        public override string ParseString(string strValue)
        {
            string result;
            if (string.IsNullOrEmpty(strValue)) {
                Clear();
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseAge(this, strValue);
            }
            return result;
        }

        public override string ParseString(StringSpan strValue)
        {
            string result;
            if (string.IsNullOrEmpty(strValue)) {
                Clear();
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseAge(this, strValue);
            }
            return result;
        }

        protected override string GetStringValue()
        {
            var parts = new string[4];
            int pIdx = 0;

            // never write INFANT/CHILD, this can lead to loss of information,
            // always write <1 or <8 and include months and days if set

            if (StillBorn) {
                parts[pIdx++] = "STILLBORN";
            } else {
                parts[pIdx++] = GEDCOMConsts.AgeRelatives[fRelative + 1];

                if (fYears != -1) {
                    parts[pIdx++] = string.Format("{0:000}y", fYears);
                }
                if (fMonths != -1) {
                    parts[pIdx++] = string.Format("{0:00}m", fMonths);
                }
                if (fDays != -1) {
                    parts[pIdx++] = string.Format("{0:000}d", fDays);
                }
            }

            return string.Join(" ", parts, 0, pIdx);
        }
    }
}
