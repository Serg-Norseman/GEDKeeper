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
using System.IO;
using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMSourceRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMRepositoryCitation> fRepositoryCitations;

        public GEDCOMData Data
        {
            get { return GetTag(GEDCOMTagType.DATA, GEDCOMData.Create) as GEDCOMData; }
        }

        public StringList Originator
        {
            get { return GetTagStrings(GetTag(GEDCOMTagType.AUTH, GEDCOMTag.Create)); }
            set { SetTagStrings(GetTag(GEDCOMTagType.AUTH, GEDCOMTag.Create), value); }
        }

        public StringList Title
        {
            get { return GetTagStrings(GetTag(GEDCOMTagType.TITL, GEDCOMTag.Create)); }
            set { SetTagStrings(GetTag(GEDCOMTagType.TITL, GEDCOMTag.Create), value); }
        }

        public string ShortTitle
        {
            get { return GetTagStringValue(GEDCOMTagType.ABBR); }
            set { SetTagStringValue(GEDCOMTagType.ABBR, value); }
        }

        public StringList Publication
        {
            get { return GetTagStrings(GetTag(GEDCOMTagType.PUBL, GEDCOMTag.Create)); }
            set { SetTagStrings(GetTag(GEDCOMTagType.PUBL, GEDCOMTag.Create), value); }
        }

        public GEDCOMList<GEDCOMRepositoryCitation> RepositoryCitations
        {
            get { return fRepositoryCitations; }
        }

        public StringList Text
        {
            get { return GetTagStrings(GetTag(GEDCOMTagType.TEXT, GEDCOMTag.Create)); }
            set { SetTagStrings(GetTag(GEDCOMTagType.TEXT, GEDCOMTag.Create), value); }
        }


        public GEDCOMSourceRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtSource);
            SetName(GEDCOMTagType.SOUR);

            fRepositoryCitations = new GEDCOMList<GEDCOMRepositoryCitation>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fRepositoryCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.REPO) {
                result = fRepositoryCitations.Add(new GEDCOMRepositoryCitation(this, tagName, tagValue));
            } else if (tagName == GEDCOMTagType.DATA) {
                result = base.AddTag(tagName, tagValue, GEDCOMData.Create);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
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

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMSourceRecord targetSource = targetRecord as GEDCOMSourceRecord;
            if (targetSource == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            StringList titl = new StringList();
            StringList orig = new StringList();
            StringList publ = new StringList();
            StringList text = new StringList();
            try
            {
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

                while (fRepositoryCitations.Count > 0)
                {
                    GEDCOMRepositoryCitation obj = fRepositoryCitations.Extract(0);
                    obj.ResetOwner(targetSource);
                    targetSource.RepositoryCitations.Add(obj);
                }
            }
            finally
            {
                titl.Dispose();
                orig.Dispose();
                publ.Dispose();
                text.Dispose();
            }
        }

        public override void Pack()
        {
            base.Pack();
            fRepositoryCitations.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fRepositoryCitations.ReplaceXRefs(map);
        }

        public override void SaveToStream(StreamWriter stream, int level)
        {
            base.SaveToStream(stream, level);
            fRepositoryCitations.SaveToStream(stream, ++level);
        }


        public void SetOriginatorArray(params string[] value)
        {
            SetTagStrings(GetTag(GEDCOMTagType.AUTH, GEDCOMTag.Create), value);
        }

        public void SetTitleArray(params string[] value)
        {
            SetTagStrings(GetTag(GEDCOMTagType.TITL, GEDCOMTag.Create), value);
        }

        public void SetPublicationArray(params string[] value)
        {
            SetTagStrings(GetTag(GEDCOMTagType.PUBL, GEDCOMTag.Create), value);
        }

        public void SetTextArray(params string[] value)
        {
            SetTagStrings(GetTag(GEDCOMTagType.TEXT, GEDCOMTag.Create), value);
        }

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMSourceRecord otherSource = tag as GEDCOMSourceRecord;
            if (otherSource == null) return 0.0f;

            float match = GetStrMatch(ShortTitle, otherSource.ShortTitle, matchParams);
            return match;
        }

        #region Auxiliary

        public GEDCOMRepositoryCitation AddRepository(GEDCOMRepositoryRecord repRec)
        {
            GEDCOMRepositoryCitation cit = null;
            
            if (repRec != null) {
                cit = new GEDCOMRepositoryCitation(this);
                cit.Value = repRec;
                fRepositoryCitations.Add(cit);
            }
            
            return cit;
        }

        public void RemoveRepository(GEDCOMRepositoryRecord repRec)
        {
            if (repRec == null)
                throw new ArgumentNullException("repRec");

            foreach (GEDCOMRepositoryCitation repCit in fRepositoryCitations) {
                GEDCOMRepositoryRecord rep = repCit.Value as GEDCOMRepositoryRecord;

                if (rep == repRec) {
                    fRepositoryCitations.Delete(repCit);
                    break;
                }
            }
        }

        #endregion
    }
}
