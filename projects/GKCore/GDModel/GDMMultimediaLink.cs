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
using GKCore;

namespace GDModel
{
    public sealed class GDMMultimediaLink : GDMPointer
    {
        private GDMCutoutPosition fCutoutPosition;
        private readonly GDMList<GDMFileReference> fFileReferences;
        private bool fIsPrimary;
        private bool fIsPrimaryCutout;
        private string fTitle;


        public GDMCutoutPosition CutoutPosition
        {
            get { return fCutoutPosition; }
        }

        public GDMList<GDMFileReference> FileReferences
        {
            get { return fFileReferences; }
        }

        /// <summary>
        /// The _PRIM tag is often added by genealogy programs to signify that this picture is the PRIMary picture,
        /// or the picture that should be used for charts.
        /// See the following programs: PhpGedView, AQ3, PAF5, FO7.
        /// </summary>
        public bool IsPrimary
        {
            get { return fIsPrimary; }
            set { fIsPrimary = value; }
        }

        /// <summary>
        /// It is acceptable to export information to sections of the referenced image files.
        /// To export this information, the use of user-defined tags _PRIM_CUTOUT and _POSITION is agreed.
        /// See the following programs: FTB.
        /// </summary>
        public bool IsPrimaryCutout
        {
            get { return fIsPrimaryCutout; }
            set { fIsPrimaryCutout = value; }
        }

        public string Title
        {
            get { return fTitle; }
            set { fTitle = value; }
        }


        public GDMMultimediaLink()
        {
            SetName(GEDCOMTagType.OBJE);

            fCutoutPosition = new GDMCutoutPosition();
            fFileReferences = new GDMList<GDMFileReference>();
            fIsPrimary = false;
            fIsPrimaryCutout = false;
            fTitle = string.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fFileReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fCutoutPosition.TrimExcess();
            fFileReferences.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMMultimediaLink sourceObj = (source as GDMMultimediaLink);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fCutoutPosition = sourceObj.fCutoutPosition;
            AssignList(sourceObj.fFileReferences, fFileReferences);
            fIsPrimary = sourceObj.fIsPrimary;
            fIsPrimaryCutout = sourceObj.fIsPrimaryCutout;
            fTitle = sourceObj.fTitle;
        }

        public override void Clear()
        {
            base.Clear();

            fCutoutPosition.Clear();
            fFileReferences.Clear();
            fIsPrimary = false;
            fIsPrimaryCutout = false;
            fTitle = string.Empty;
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (SubTags.Count == 0 && (fFileReferences.Count == 0) && string.IsNullOrEmpty(fTitle) && fCutoutPosition.IsEmpty());
            }
            return result;
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : string.Empty;
            return result;
        }

        public string GetUID(GDMTree tree)
        {
            string result = null;
            try {
                var mmRec = tree.GetPtrValue<GDMMultimediaRecord>(this);
                if (mmRec != null) {
                    ExtRect cutoutArea = IsPrimaryCutout ? CutoutPosition.Value : ExtRect.CreateEmpty();
                    result = mmRec.UID + "-" + GKUtils.GetRectUID(cutoutArea.Left, cutoutArea.Top, cutoutArea.Right, cutoutArea.Bottom);
                }
            } catch (Exception ex) {
                Logger.WriteError("GDMMultimediaLink.GetUID()", ex);
                result = null;
            }
            return result;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fFileReferences.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fCutoutPosition);
            ProcessHashes(ref hashCode, fFileReferences);
            hashCode.Add(fIsPrimary);
            hashCode.Add(fIsPrimaryCutout);
            hashCode.Add(fTitle);
        }
    }
}
