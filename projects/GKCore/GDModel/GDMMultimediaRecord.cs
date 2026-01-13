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
    public sealed class GDMMultimediaRecord : GDMRecord
    {
        private readonly GDMList<GDMFileReferenceWithTitle> fFileReferences;

        public GDMList<GDMFileReferenceWithTitle> FileReferences
        {
            get { return fFileReferences; }
        }


        public GDMMultimediaRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.OBJE);

            fFileReferences = new GDMList<GDMFileReferenceWithTitle>();
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

            fFileReferences.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMMultimediaRecord sourceObj = source as GDMMultimediaRecord;
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            AssignList(sourceObj.fFileReferences, fFileReferences);
        }

        /// <summary>
        /// The MoveTo() merges records and their references, but does not change the text in the target.
        /// </summary>
        /// <param name="targetRecord"></param>
        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMMultimediaRecord targetMedia = (targetRecord as GDMMultimediaRecord);
            if (targetMedia == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord);

            while (fFileReferences.Count > 0) {
                var obj = fFileReferences.Extract(0);
                targetMedia.FileReferences.Add(obj);
            }
        }

        public override void Clear()
        {
            base.Clear();
            fFileReferences.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fFileReferences.Count == 0;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fFileReferences.ReplaceXRefs(map);
        }

        public string GetFileTitle()
        {
            if (fFileReferences.Count <= 0) return string.Empty;

            GDMFileReferenceWithTitle fileRef = fFileReferences[0];
            return fileRef.Title;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            ProcessHashes(ref hashCode, fFileReferences);
        }
    }
}
