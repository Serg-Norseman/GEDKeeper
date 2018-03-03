/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using GKCore;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMMultimediaLink : GEDCOMPointer
    {
        private GEDCOMList<GEDCOMFileReference> fFileReferences;

        public GEDCOMList<GEDCOMFileReference> FileReferences
        {
            get { return fFileReferences; }
        }

        public bool IsPointer
        {
            get { return (!string.IsNullOrEmpty(XRef)); }
        }

        public string Title
        {
            get { return GetTagStringValue("TITL"); }
            set { SetTagStringValue("TITL", value); }
        }

        /// <summary>
        /// The _PRIM tag is often added by genealogy programs to signify that this picture is the PRIMary picture,
        /// or the picture that should be used for charts.
        /// See the following programs: PhpGedView, AQ3, PAF5, FO7.
        /// </summary>
        public bool IsPrimary
        {
            get { return GetTagYNValue("_PRIM"); }
            set { SetTagYNValue("_PRIM", value); }
        }

        /// <summary>
        /// It is acceptable to export information to sections of the referenced image files.
        /// To export this information, the use of user-defined tags _PRIM_CUTOUT and _POSITION is agreed.
        /// See the following programs: FTB.
        /// </summary>
        public bool IsPrimaryCutout
        {
            get { return GetTagYNValue("_PRIM_CUTOUT"); }
            set { SetTagYNValue("_PRIM_CUTOUT", value); }
        }

        public GEDCOMCutoutPosition CutoutPosition
        {
            get { return TagClass("_POSITION", GEDCOMCutoutPosition.Create) as GEDCOMCutoutPosition; }
        }


        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("OBJE");
            fFileReferences = new GEDCOMList<GEDCOMFileReference>(this);
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : fStringValue;
            return result;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fFileReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "FILE") {
                result = fFileReferences.Add(new GEDCOMFileReference(Owner, this, tagName, tagValue));
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fFileReferences.Clear();
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (Count == 0 && (fFileReferences.Count == 0));
            }
            return result;
        }

        public override string ParseString(string strValue)
        {
            fStringValue = "";
            return base.ParseString(strValue);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            fFileReferences.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            fFileReferences.SaveToStream(stream);
        }

        public GEDCOMMultimediaLink(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMMultimediaLink(owner, parent, tagName, tagValue);
        }

        #region Utilities

        public string GetUID()
        {
            string result = null;
            try
            {
                if (Value != null)
                {
                    ExtRect cutoutArea;
                    if (IsPrimaryCutout) {
                        cutoutArea = CutoutPosition.Value;
                    } else {
                        cutoutArea = ExtRect.CreateEmpty();
                    }

                    GEDCOMMultimediaRecord mmRec = (GEDCOMMultimediaRecord)Value;
                    result = mmRec.UID + "-" + GKUtils.GetRectUID(cutoutArea.Left, cutoutArea.Top, cutoutArea.Right, cutoutArea.Bottom);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GEDCOMMultimediaLink.GetUID(): " + ex.Message);
                result = null;
            }
            return result;
        }

        #endregion
    }
}
