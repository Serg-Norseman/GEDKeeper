/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMSourceRecord : GDMRecord
    {
        private GDMList<GDMRepositoryCitation> fRepositoryCitations;


        public GDMSourceData Data
        {
            get { return GetTag<GDMSourceData>(GEDCOMTagType.DATA, GDMSourceData.Create); }
        }

        public StringList Originator
        {
            get { return GetTagStrings(GetTag<GDMTag>(GEDCOMTagType.AUTH, GDMTag.Create)); }
            set { SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.AUTH, GDMTag.Create), value); }
        }

        public StringList Title
        {
            get { return GetTagStrings(GetTag<GDMTag>(GEDCOMTagType.TITL, GDMTag.Create)); }
            set { SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.TITL, GDMTag.Create), value); }
        }

        public string ShortTitle
        {
            get { return GetTagStringValue(GEDCOMTagType.ABBR); }
            set { SetTagStringValue(GEDCOMTagType.ABBR, value); }
        }

        public StringList Publication
        {
            get { return GetTagStrings(GetTag<GDMTag>(GEDCOMTagType.PUBL, GDMTag.Create)); }
            set { SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.PUBL, GDMTag.Create), value); }
        }

        public GDMList<GDMRepositoryCitation> RepositoryCitations
        {
            get { return fRepositoryCitations; }
        }

        public StringList Text
        {
            get { return GetTagStrings(GetTag<GDMTag>(GEDCOMTagType.TEXT, GDMTag.Create)); }
            set { SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.TEXT, GDMTag.Create), value); }
        }


        public GDMSourceRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtSource);
            SetName(GEDCOMTagType.SOUR);

            fRepositoryCitations = new GDMList<GDMRepositoryCitation>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fRepositoryCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            base.Clear();
            fRepositoryCitations.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fRepositoryCitations.Count == 0;
        }

        public override void MoveTo(GDMRecord targetRecord, bool clearDest)
        {
            GDMSourceRecord targetSource = targetRecord as GDMSourceRecord;
            if (targetSource == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            StringList titl = new StringList();
            StringList orig = new StringList();
            StringList publ = new StringList();
            StringList text = new StringList();
            try {
                titl.Text = (targetSource.Title.Text + "\n" + Title.Text).Trim();
                orig.Text = (targetSource.Originator.Text + "\n" + Originator.Text).Trim();
                publ.Text = (targetSource.Publication.Text + "\n" + Publication.Text).Trim();
                text.Text = (targetSource.Text.Text + "\n" + Text.Text).Trim();

                DeleteTag(GEDCOMTagType.TITL);
                DeleteTag(GEDCOMTagType.TEXT);
                DeleteTag(GEDCOMTagType.ABBR);
                DeleteTag(GEDCOMTagType.PUBL);
                DeleteTag(GEDCOMTagType.AUTH);

                base.MoveTo(targetRecord, clearDest);

                targetSource.Title = titl;
                targetSource.Originator = orig;
                targetSource.Publication = publ;
                targetSource.Text = text;

                while (fRepositoryCitations.Count > 0) {
                    GDMRepositoryCitation obj = fRepositoryCitations.Extract(0);
                    obj.ResetOwner(targetSource);
                    targetSource.RepositoryCitations.Add(obj);
                }
            } finally {
                titl.Dispose();
                orig.Dispose();
                publ.Dispose();
                text.Dispose();
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fRepositoryCitations.ReplaceXRefs(map);
        }


        public void SetOriginatorArray(params string[] value)
        {
            SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.AUTH, GDMTag.Create), value);
        }

        public void SetTitleArray(params string[] value)
        {
            SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.TITL, GDMTag.Create), value);
        }

        public void SetPublicationArray(params string[] value)
        {
            SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.PUBL, GDMTag.Create), value);
        }

        public void SetTextArray(params string[] value)
        {
            SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.TEXT, GDMTag.Create), value);
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMSourceRecord otherSource = tag as GDMSourceRecord;
            if (otherSource == null) return 0.0f;

            float match = GetStrMatch(ShortTitle, otherSource.ShortTitle, matchParams);
            return match;
        }

        public GDMRepositoryCitation AddRepository(GDMRepositoryRecord repRec)
        {
            GDMRepositoryCitation cit = null;
            
            if (repRec != null) {
                cit = new GDMRepositoryCitation(this);
                cit.Value = repRec;
                fRepositoryCitations.Add(cit);
            }
            
            return cit;
        }

        public void RemoveRepository(GDMRepositoryRecord repRec)
        {
            if (repRec == null)
                throw new ArgumentNullException("repRec");

            foreach (GDMRepositoryCitation repCit in fRepositoryCitations) {
                GDMRepositoryRecord rep = repCit.Value as GDMRepositoryRecord;

                if (rep == repRec) {
                    fRepositoryCitations.Delete(repCit);
                    break;
                }
            }
        }
    }
}
