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
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMSourceRecord : GDMRecord
    {
        private GDMSourceData fData;
        private readonly GDMDateValue fDate;
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

        public GDMDateValue Date
        {
            get { return fDate; }
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


        public GDMSourceRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.SOUR);

            fData = new GDMSourceData();
            fOriginator = new GDMTextTag((int)GEDCOMTagType.AUTH);
            fPublication = new GDMTextTag((int)GEDCOMTagType.PUBL);
            fRepositoryCitations = new GDMList<GDMRepositoryCitation>();
            fShortTitle = string.Empty;
            fText = new GDMTextTag((int)GEDCOMTagType.TEXT);
            fTitle = new GDMTextTag((int)GEDCOMTagType.TITL);

            fDate = new GDMDateValue();
            fDate.SetName(GEDCOMTagType._DATE);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fData.Dispose();
                fOriginator.Dispose();
                fPublication.Dispose();
                fRepositoryCitations.Dispose();
                fText.Dispose();
                fTitle.Dispose();

                fDate.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fData.TrimExcess();
            fOriginator.TrimExcess();
            fPublication.TrimExcess();
            fRepositoryCitations.TrimExcess();
            fText.TrimExcess();
            fTitle.TrimExcess();

            fDate.TrimExcess();
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
            AssignList(otherSource.fRepositoryCitations, fRepositoryCitations);

            fDate.Assign(otherSource.fDate);
        }

        public override void Clear()
        {
            base.Clear();

            fData.Clear();
            fOriginator.Clear();
            fPublication.Clear();
            fShortTitle = string.Empty;
            fRepositoryCitations.Clear();
            fText.Clear();
            fTitle.Clear();

            fDate.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fData.IsEmpty() && fOriginator.IsEmpty() && fPublication.IsEmpty()
                && string.IsNullOrEmpty(fShortTitle) && fText.IsEmpty() && fTitle.IsEmpty()
                && (fRepositoryCitations.Count == 0)
                && fDate.IsEmpty();
        }

        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMSourceRecord targetSource = targetRecord as GDMSourceRecord;
            if (targetSource == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            GDMLines titl = new GDMLines();
            GDMLines orig = new GDMLines();
            GDMLines publ = new GDMLines();
            GDMLines text = new GDMLines();

            titl.Text = (targetSource.Title.Lines.Text + "\n" + Title.Lines.Text).Trim();
            orig.Text = (targetSource.Originator.Lines.Text + "\n" + Originator.Lines.Text).Trim();
            publ.Text = (targetSource.Publication.Lines.Text + "\n" + Publication.Lines.Text).Trim();
            text.Text = (targetSource.Text.Lines.Text + "\n" + Text.Lines.Text).Trim();

            base.MoveTo(targetRecord);

            targetSource.Title.Lines.Assign(titl);
            targetSource.Originator.Lines.Assign(orig);
            targetSource.Publication.Lines.Assign(publ);
            targetSource.Text.Lines.Assign(text);

            while (fRepositoryCitations.Count > 0) {
                GDMRepositoryCitation obj = fRepositoryCitations.Extract(0);
                targetSource.RepositoryCitations.Add(obj);
            }

            if (targetSource.Date.IsEmpty() && !fDate.IsEmpty()) {
                targetSource.Date.Assign(fDate);
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fRepositoryCitations.ReplaceXRefs(map);
            fDate.ReplaceXRefs(map);
        }


        public void SetOriginatorArray(params string[] value)
        {
            fOriginator.Lines.Clear();
            fOriginator.Lines.AddRange(value);
        }

        public void SetTitleArray(params string[] value)
        {
            fTitle.Lines.Clear();
            fTitle.Lines.AddRange(value);
        }

        public void SetPublicationArray(params string[] value)
        {
            fPublication.Lines.Clear();
            fPublication.Lines.AddRange(value);
        }

        public void SetTextArray(params string[] value)
        {
            fText.Lines.Clear();
            fText.Lines.AddRange(value);
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
                cit = new GDMRepositoryCitation();
                cit.XRef = repRec.XRef;
                fRepositoryCitations.Add(cit);
            }
            
            return cit;
        }

        public void RemoveRepository(GDMRepositoryRecord repRec)
        {
            if (repRec == null)
                throw new ArgumentNullException("repRec");

            foreach (GDMRepositoryCitation repCit in fRepositoryCitations) {
                if (repCit.XRef == repRec.XRef) {
                    fRepositoryCitations.Remove(repCit);
                    break;
                }
            }
        }

        public GDMRepositoryCitation FindRepository(GDMRepositoryRecord repoRec)
        {
            if (repoRec != null) {
                int num = fRepositoryCitations.Count;
                for (int i = 0; i < num; i++) {
                    var repoCit = fRepositoryCitations[i];
                    if (repoCit.XRef == repoRec.XRef) {
                        return repoCit;
                    }
                }
            }

            return null;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fData);
            hashCode.Add(fDate);
            hashCode.Add(fOriginator);
            hashCode.Add(fPublication);
            ProcessHashes(ref hashCode, fRepositoryCitations);
            hashCode.Add(fShortTitle);
            hashCode.Add(fText);
            hashCode.Add(fTitle);
        }
    }
}
