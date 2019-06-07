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
        private GDMSourceData fData;
        private GDMTextTag fOriginator;
        private GDMTextTag fPublication;
        private GDMList<GDMRepositoryCitation> fRepositoryCitations;
        private string fShortTitle;
        private GDMTextTag fText;
        private GDMTextTag fTitle;


        public GDMSourceData Data
        {
            get { return fData; }
        }

        public GDMTextTag Originator
        {
            get { return fOriginator; }
        }

        public GDMTextTag Publication
        {
            get { return fPublication; }
        }

        public GDMList<GDMRepositoryCitation> RepositoryCitations
        {
            get { return fRepositoryCitations; }
        }

        public string ShortTitle
        {
            get { return fShortTitle; }
            set { fShortTitle = value; }
        }

        public GDMTextTag Text
        {
            get { return fText; }
        }

        public GDMTextTag Title
        {
            get { return fTitle; }
        }


        public GDMSourceRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtSource);
            SetName(GEDCOMTagType.SOUR);

            fData = new GDMSourceData(this);
            fOriginator = new GDMTextTag(this, GEDCOMTagType.AUTH, string.Empty);
            fPublication = new GDMTextTag(this, GEDCOMTagType.PUBL, string.Empty);
            fRepositoryCitations = new GDMList<GDMRepositoryCitation>(this);
            fShortTitle = string.Empty;
            fText = new GDMTextTag(this, GEDCOMTagType.TEXT, string.Empty);
            fTitle = new GDMTextTag(this, GEDCOMTagType.TITL, string.Empty);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fRepositoryCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Assign(GDMTag source)
        {
            GDMSourceRecord otherSource = (source as GDMSourceRecord);
            if (otherSource == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherSource);

            fData.Assign(otherSource.fData);
            fOriginator.Assign(otherSource.fOriginator);
            fPublication.Assign(otherSource.fPublication);
            fShortTitle = otherSource.fShortTitle;
            fText.Assign(otherSource.fText);
            fTitle.Assign(otherSource.fTitle);

            // TODO: validate this logic!
            foreach (GDMRepositoryCitation srcRepCit in otherSource.fRepositoryCitations) {
                GDMRepositoryCitation copyRepCit = new GDMRepositoryCitation(this);
                copyRepCit.Assign(srcRepCit);
                fRepositoryCitations.Add(copyRepCit);
            }
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
                titl.Text = (targetSource.Title.Lines.Text + "\n" + Title.Lines.Text).Trim();
                orig.Text = (targetSource.Originator.Lines.Text + "\n" + Originator.Lines.Text).Trim();
                publ.Text = (targetSource.Publication.Lines.Text + "\n" + Publication.Lines.Text).Trim();
                text.Text = (targetSource.Text.Lines.Text + "\n" + Text.Lines.Text).Trim();

                DeleteTag(GEDCOMTagType.TITL);
                DeleteTag(GEDCOMTagType.TEXT);
                DeleteTag(GEDCOMTagType.ABBR);
                DeleteTag(GEDCOMTagType.PUBL);
                DeleteTag(GEDCOMTagType.AUTH);

                base.MoveTo(targetRecord, clearDest);

                targetSource.Title.Lines.Assign(titl);
                targetSource.Originator.Lines.Assign(orig);
                targetSource.Publication.Lines.Assign(publ);
                targetSource.Text.Lines.Assign(text);

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
            fOriginator.Lines.Clear();
            fOriginator.Lines.AddStrings(value);
        }

        public void SetTitleArray(params string[] value)
        {
            fTitle.Lines.Clear();
            fTitle.Lines.AddStrings(value);
        }

        public void SetPublicationArray(params string[] value)
        {
            fPublication.Lines.Clear();
            fPublication.Lines.AddStrings(value);
        }

        public void SetTextArray(params string[] value)
        {
            fText.Lines.Clear();
            fText.Lines.AddStrings(value);
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
