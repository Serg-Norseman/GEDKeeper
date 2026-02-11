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
    public sealed class GDMTextTag : GDMTag, IGDMTextObject
    {
        private readonly GDMLines fLines;

        public GDMLines Lines
        {
            get { return fLines; }
        }


        public GDMTextTag()
        {
            fLines = new GDMLines();
        }

        public GDMTextTag(int tagId) : this()
        {
            SetName(tagId);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fLines.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMTextTag sourceObj = (source as GDMTextTag);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(sourceObj);

            fLines.Assign(sourceObj.fLines);
        }

        public override void Clear()
        {
            base.Clear();
            fLines.Clear();
        }

        public void SetLines(string[] value)
        {
            fLines.Clear();
            fLines.AddRange(value);
        }

        public override bool IsEmpty()
        {
            return fLines.IsEmpty();
        }

        protected override string GetStringValue()
        {
            return fLines.Text;
        }

        public override string ParseString(string strValue)
        {
            fLines.Clear();
            fLines.Add(strValue);
            return string.Empty;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fLines);
        }
    }


    public sealed class GDMNotes : GDMPointer, IGDMTextObject
    {
        private readonly GDMLines fLines;

        public GDMLines Lines
        {
            get {
                if (IsPointer) {
                    throw new InvalidOperationException("GDMNotes is a pointer, please dereference");
                }

                return fLines;
            }
        }


        public GDMNotes()
        {
            SetName(GEDCOMTagType.NOTE);

            fLines = new GDMLines();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fLines.TrimExcess();
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (fLines.IsEmpty() && SubTags.Count == 0);
            }
            return result;
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : ((fLines.Count > 0) ? fLines[0] : string.Empty);
            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fLines.Clear();
                if (!string.IsNullOrEmpty(result)) {
                    fLines.Add(result);
                }
                result = string.Empty;
            }
            return result;
        }

        public override string ParseString(StringSpan strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fLines.Clear();
                if (!string.IsNullOrEmpty(result)) {
                    fLines.Add(result);
                }
                result = string.Empty;
            }
            return result;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fLines);
        }
    }
}
