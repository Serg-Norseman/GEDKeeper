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
    public sealed class GDMSourceData : GDMTag, IGDMStructWithNotes
    {
        private string fAgency;
        private GDMList<GDMSourceEvent> fEvents;
        private GDMList<GDMNotes> fNotes;


        public string Agency
        {
            get { return fAgency; }
            set { fAgency = value; }
        }

        public GDMList<GDMSourceEvent> Events
        {
            get { return fEvents; }
        }

        public bool HasNotes
        {
            get { return fNotes != null && fNotes.Count != 0; }
        }

        public GDMList<GDMNotes> Notes
        {
            get {
                if (fNotes == null) {
                    fNotes = new GDMList<GDMNotes>();
                }

                return fNotes;
            }
        }


        public GDMSourceData()
        {
            SetName(GEDCOMTagType.DATA);

            fAgency = string.Empty;
            fEvents = new GDMList<GDMSourceEvent>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fEvents.Dispose();
                if (fNotes != null) fNotes.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fEvents.TrimExcess();
            if (fNotes != null) fNotes.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();
            fAgency = string.Empty;
            fEvents.Clear();
            if (fNotes != null) fNotes.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fEvents.Count == 0) && string.IsNullOrEmpty(fAgency) && fNotes.IsEmpty();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fEvents.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAgency);
            ProcessHashes(ref hashCode, fEvents);
            ProcessHashes(ref hashCode, fNotes);
        }
    }
}
